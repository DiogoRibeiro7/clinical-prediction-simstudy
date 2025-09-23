# =======================================================================
# run_chunk.R
# Run a chunk of scenarios in parallel and save results to disk
# FIXED VERSION - improved error handling and robustness
# =======================================================================

suppressPackageStartupMessages({
  library(optparse)
  library(future.apply)
  library(dplyr)
  library(readr)
  library(glue)
})

source("replicate_framework.R")

opt <- OptionParser()
opt <- add_option(opt, c("--chunks"), type = "integer", default = 50,
                  help = "Total number of chunks to split the 1050 scenarios [default %default]")
opt <- add_option(opt, c("--chunk_id"), type = "integer", default = 1,
                  help = "Which chunk to run (1-based) [default %default]")
opt <- add_option(opt, c("--iters"), type = "integer", default = 20,
                  help = "Iterations per scenario [default %default]")
opt <- add_option(opt, c("--outdir"), type = "character", default = "results",
                  help = "Output directory for .rds files [default %default]")
opt <- add_option(opt, c("--workers"), type = "integer", default = parallel::detectCores()-1,
                  help = "Parallel workers [default %default]")

args <- parse_args(opt)

# Validate arguments
if (args$chunk_id < 1 || args$chunks < 1) {
  stop("chunk_id and chunks must be positive integers")
}
if (args$chunk_id > args$chunks) {
  stop("chunk_id cannot be greater than chunks")
}
if (args$iters < 1) {
  stop("iters must be a positive integer")
}
if (args$workers < 1) {
  args$workers <- 1
  warning("workers must be positive, setting to 1")
}

dir.create(args$outdir, showWarnings = FALSE, recursive = TRUE)

grid <- make_full_grid()
N <- nrow(grid) # 1050

# Validate grid
if (N != 1050) {
  stop(glue("Expected 1050 scenarios, got {N}"))
}

chunk_size <- ceiling(N / args$chunks)
start <- (args$chunk_id - 1) * chunk_size + 1
end   <- min(args$chunk_id * chunk_size, N)
scn_indices <- seq.int(start, end)

if (length(scn_indices) == 0) {
  stop(glue("No scenarios to run for chunk {args$chunk_id}"))
}

message(glue("Running chunk {args$chunk_id}/{args$chunks} -> scenarios [{start}:{end}] (n={length(scn_indices)})"))
message(glue("Using {args$workers} workers, {args$iters} iterations per scenario"))

# Check if output file already exists
outfile <- file.path(args$outdir, glue("sim_chunk_{args$chunk_id:03d}.rds"))
if (file.exists(outfile)) {
  response <- readline(prompt = glue("Output file {outfile} exists. Overwrite? (y/N): "))
  if (!tolower(response) %in% c("y", "yes")) {
    message("Exiting without overwriting.")
    quit(status = 0)
  }
}

# Set up parallel processing with error handling
tryCatch({
  future::plan(future::multisession, workers = args$workers)
  message(glue("Parallel plan set with {args$workers} workers"))
}, error = function(e) {
  warning(glue("Failed to set up parallel processing: {e$message}"))
  message("Falling back to sequential processing")
  future::plan(future::sequential)
  args$workers <- 1
})

# Split scenarios across workers more evenly
if (args$workers > 1) {
  # Create balanced chunks for parallel processing
  scenario_chunks <- split(scn_indices, sort(rep_len(1:args$workers, length(scn_indices))))
} else {
  scenario_chunks <- list(scn_indices)
}

message(glue("Processing {length(scenario_chunks)} parallel chunks"))

# Run scenarios with better error handling
start_time <- Sys.time()

res_list <- future_lapply(scenario_chunks, function(idx) {
  # Source framework in each worker
  source("replicate_framework.R", local = TRUE)
  
  tryCatch({
    run_scenarios_block(grid, scn_indices = idx, iters = args$iters)
  }, error = function(e) {
    warning(glue("Chunk processing failed: {e$message}"))
    # Return empty tibble with correct structure rather than NULL
    tibble(
      method = character(0),
      auc = numeric(0),
      cal_slope = numeric(0),
      cal_in_large = numeric(0),
      brier = numeric(0),
      rMSPE = numeric(0),
      MAPE = numeric(0),
      iter = integer(0),
      scn_id = integer(0),
      EPV = numeric(0),
      event_frac = numeric(0),
      p = integer(0),
      noise_frac = numeric(0),
      sparse = logical(0)
    )
  })
}, future.seed = TRUE)  # Ensure reproducible random numbers

end_time <- Sys.time()
runtime <- as.numeric(difftime(end_time, start_time, units = "mins"))
message(glue("Parallel processing completed in {round(runtime, 2)} minutes"))

# Combine results with validation
res <- tryCatch({
  combined <- dplyr::bind_rows(res_list)
  
  # Validate result structure
  expected_cols <- c("method", "auc", "cal_slope", "cal_in_large", "brier", 
                     "rMSPE", "MAPE", "iter", "scn_id", "EPV", "event_frac", 
                     "p", "noise_frac", "sparse")
  
  missing_cols <- setdiff(expected_cols, names(combined))
  if (length(missing_cols) > 0) {
    stop(glue("Missing columns in results: {paste(missing_cols, collapse = ', ')}"))
  }
  
  # Check for reasonable number of results
  expected_rows <- length(scn_indices) * args$iters * 11  # 11 methods
  actual_rows <- nrow(combined)
  
  if (actual_rows == 0) {
    stop("No results generated")
  }
  
  if (actual_rows < expected_rows * 0.8) {  # Allow for some failures
    warning(glue("Fewer results than expected: {actual_rows} vs {expected_rows}"))
  }
  
  message(glue("Combined results: {actual_rows} rows across {length(unique(combined$scn_id))} scenarios"))
  combined
  
}, error = function(e) {
  stop(glue("Failed to combine results: {e$message}"))
})

# Save raw results with backup
tryCatch({
  # Create backup if file exists
  if (file.exists(outfile)) {
    backup_file <- paste0(outfile, ".backup.", format(Sys.time(), "%Y%m%d_%H%M%S"))
    file.copy(outfile, backup_file)
    message(glue("Created backup: {backup_file}"))
  }
  
  saveRDS(res, outfile)
  message(glue("Saved results: {outfile}"))
  
  # Verify saved file
  test_load <- readRDS(outfile)
  if (nrow(test_load) != nrow(res)) {
    warning("Saved file verification failed")
  }
  
}, error = function(e) {
  stop(glue("Failed to save results: {e$message}"))
})

# Generate per-chunk rank summary with error handling
tryCatch({
  ranked <- res %>% 
    group_by(scn_id) %>% 
    group_modify(~ rank_with_rounding(.x)) %>% 
    ungroup()
  
  sumfile <- file.path(args$outdir, glue("sim_chunk_{args$chunk_id:03d}_rank_summary.csv"))
  
  summary_stats <- ranked %>%
    group_by(method) %>%
    summarise(
      mean_rank_auc   = mean(r_auc, na.rm = TRUE),
      mean_rank_cals  = mean(r_cals, na.rm = TRUE),
      mean_rank_brier = mean(r_brier, na.rm = TRUE),
      mean_rank_rmspe = mean(r_rmspe, na.rm = TRUE),
      mean_rank_mape  = mean(r_mape, na.rm = TRUE),
      n = dplyr::n(),
      na_count_auc = sum(is.na(r_auc)),
      na_count_cals = sum(is.na(r_cals)),
      na_count_brier = sum(is.na(r_brier)),
      .groups = 'drop'
    ) %>%
    arrange(mean_rank_auc)
  
  readr::write_csv(summary_stats, sumfile)
  message(glue("Saved summary: {sumfile}"))
  
  # Print quick summary to console
  cat("\nTop 5 methods by AUC rank:\n")
  print(summary_stats %>% head(5) %>% select(method, mean_rank_auc, mean_rank_brier, n))
  
}, error = function(e) {
  warning(glue("Failed to create summary: {e$message}"))
})

# Final validation and cleanup
message(glue("Chunk {args$chunk_id} completed successfully"))
message(glue("Total runtime: {round(runtime, 2)} minutes"))

# Close parallel workers
future::plan(future::sequential)

# Print memory usage if available
if (exists("gc")) {
  gc_info <- gc()
  message(glue("Memory usage: {sum(gc_info[,'used'])} MB"))
