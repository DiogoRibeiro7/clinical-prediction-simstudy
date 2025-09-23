# =======================================================================
# run_chunk.R
# 
# Parallel execution script for Lohmann et al. (2023) simulation replication
# Runs a specified chunk of the 1,050 scenarios with robust error handling
# and comprehensive logging.
#
# This script:
# - Divides scenarios into manageable chunks for parallel processing
# - Uses future.apply for within-chunk parallelization
# - Implements comprehensive error handling and recovery
# - Saves results incrementally with backup mechanisms  
# - Provides detailed progress monitoring and diagnostics
#
# Usage:
#   Rscript run_chunk.R --chunk_id 1 --chunks 50 --iters 20 --workers 4
#
# Author: Diogo Ribeiro
# Date: January 2025
# License: MIT
# =======================================================================

# Load required packages with informative error handling
suppressPackageStartupMessages({
  required_packages <- c("optparse", "future.apply", "dplyr", "readr", "glue")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    stop(paste("Missing required packages:", paste(missing_packages, collapse = ", "),
               "\nInstall with: install.packages(c(", 
               paste0('"', missing_packages, '"', collapse = ", "), "))"))
  }
  
  library(optparse)     # Command line argument parsing
  library(future.apply) # Parallel processing
  library(dplyr)        # Data manipulation  
  library(readr)        # File I/O
  library(glue)         # String interpolation
})

# Load simulation framework
source("replicate_framework.R")

# =======================================================================
# COMMAND LINE INTERFACE
# =======================================================================

# Define command line options with comprehensive help
option_list <- list(
  make_option(
    c("--chunks"), 
    type = "integer", 
    default = 50,
    help = paste("Total number of chunks to divide the 1,050 scenarios into.",
                 "Higher values create smaller, more manageable chunks",
                 "[default: %default]")
  ),
  make_option(
    c("--chunk_id"), 
    type = "integer", 
    default = 1,
    help = paste("Which specific chunk to run (1-based indexing).",
                 "Must be between 1 and --chunks",
                 "[default: %default]")
  ),
  make_option(
    c("--iters"), 
    type = "integer", 
    default = 20,
    help = paste("Number of Monte Carlo iterations per scenario.",
                 "Original paper used 20 iterations",
                 "[default: %default]")
  ),
  make_option(
    c("--outdir"), 
    type = "character", 
    default = "results",
    help = paste("Output directory for .rds result files.",
                 "Directory will be created if it doesn't exist",
                 "[default: '%default']")
  ),
  make_option(
    c("--workers"), 
    type = "integer", 
    default = parallel::detectCores() - 1,
    help = paste("Number of parallel workers for within-chunk processing.",
                 "Default uses all available cores minus 1 for system stability",
                 "[default: %default (auto-detected)]")
  ),
  make_option(
    c("--verbose"), 
    action = "store_true", 
    default = FALSE,
    help = "Enable verbose output with detailed progress information"
  ),
  make_option(
    c("--dry_run"), 
    action = "store_true", 
    default = FALSE,
    help = "Perform dry run - show what would be executed without running simulation"
  )
)

# Parse command line arguments
opt_parser <- OptionParser(
  option_list = option_list,
  description = paste(
    "\nRuns a chunk of scenarios from the Lohmann et al. (2023) replication study.",
    "This script is designed for embarrassingly parallel execution across multiple",
    "machines or job scheduler systems.\n",
    "\nExample usage:",
    "  # Run chunk 5 out of 50 total chunks with 4 workers",  
    "  Rscript run_chunk.R --chunk_id 5 --chunks 50 --workers 4",
    "\n  # Quick test run with 2 iterations",
    "  Rscript run_chunk.R --chunk_id 1 --iters 2 --dry_run",
    sep = "\n"
  ),
  epilogue = paste(
    "\nFor more information, see README.md or the original paper:",
    "Lohmann et al. (2023) Biometrical Journal 65(5):e700193"
  )
)

args <- parse_args(opt_parser)

# =======================================================================
# INPUT VALIDATION
# =======================================================================

#' Validate command line arguments
#' 
#' Performs comprehensive validation of user inputs with informative
#' error messages to prevent common mistakes.
validate_args <- function(args) {
  errors <- character(0)
  
  # Validate chunk parameters
  if (args$chunk_id < 1 || args$chunks < 1) {
    errors <- c(errors, "chunk_id and chunks must be positive integers")
  }
  
  if (args$chunk_id > args$chunks) {
    errors <- c(errors, glue("chunk_id ({args$chunk_id}) cannot exceed chunks ({args$chunks})"))
  }
  
  # Validate iteration count
  if (args$iters < 1) {
    errors <- c(errors, "iters must be a positive integer")
  }
  
  # Validate and adjust worker count
  max_workers <- parallel::detectCores()
  if (is.na(max_workers)) max_workers <- 1
  
  if (args$workers < 1) {
    warning("workers must be positive, setting to 1")
    args$workers <<- 1  # Modify in parent scope
  } else if (args$workers > max_workers) {
    warning(glue("Requested {args$workers} workers but only {max_workers} cores available, using {max_workers}"))
    args$workers <<- max_workers
  }
  
  # Check output directory permissions
  if (!args$dry_run) {
    tryCatch({
      dir.create(args$outdir, showWarnings = FALSE, recursive = TRUE)
      if (!dir.exists(args$outdir)) {
        errors <- c(errors, glue("Cannot create output directory: {args$outdir}"))
      }
    }, error = function(e) {
      errors <- c(errors, glue("Output directory error: {e$message}"))
    })
  }
  
  # Report validation results
  if (length(errors) > 0) {
    cat("❌ Input validation failed:\n")
    for (error in errors) {
      cat(glue("   • {error}\n"))
    }
    stop("Please fix the above errors and try again")
  }
  
  if (args$verbose) {
    cat("✅ Input validation passed\n")
  }
}

# Validate inputs
validate_args(args)

# =======================================================================
# SCENARIO PREPARATION
# =======================================================================

# Generate full scenario grid and validate
grid <- make_full_grid()
N_scenarios <- nrow(grid)

if (N_scenarios != 1050) {
  stop(glue("Expected 1,050 scenarios but got {N_scenarios}. Check make_full_grid() function."))
}

# Calculate chunk boundaries
chunk_size <- ceiling(N_scenarios / args$chunks)
start_idx <- (args$chunk_id - 1) * chunk_size + 1
end_idx   <- min(args$chunk_id * chunk_size, N_scenarios)
scn_indices <- seq.int(start_idx, end_idx)

# Validate chunk has scenarios to process
if (length(scn_indices) == 0) {
  stop(glue("Chunk {args$chunk_id} contains no scenarios to process"))
}

# Display execution plan
cat("🧪 LOHMANN ET AL. (2023) REPLICATION - CHUNK EXECUTION\n")
cat("=" * 60, "\n")
cat(glue("📊 Chunk: {args$chunk_id} of {args$chunks}\n"))
cat(glue("🎯 Scenarios: {start_idx} to {end_idx} ({length(scn_indices)} scenarios)\n"))
cat(glue("🔄 Iterations per scenario: {args$iters}\n"))  
cat(glue("⚡ Parallel workers: {args$workers}\n"))
cat(glue("📁 Output directory: {args$outdir}\n"))
cat(glue("🎲 Total evaluations: {length(scn_indices)} × {args$iters} × 11 methods = {length(scn_indices) * args$iters * 11:,}\n"))

# Estimate runtime
scenarios_per_minute <- 2  # Conservative estimate
estimated_minutes <- (length(scn_indices) * args$iters) / (args$workers * scenarios_per_minute)
cat(glue("⏱️  Estimated runtime: {round(estimated_minutes)} minutes\n"))
cat("-" * 60, "\n")

# Show sample scenarios for verification
if (args$verbose || args$dry_run) {
  cat("📋 Sample scenarios in this chunk:\n")
  sample_scenarios <- head(grid[scn_indices, ], 3)
  for (i in seq_len(nrow(sample_scenarios))) {
    s <- sample_scenarios[i, ]
    cat(glue("   {i}. ID {s$scn_id}: EPV={s$EPV}, p={s$p}, event_frac={round(s$event_frac,3)}, noise={s$noise_frac}, sparse={s$sparse}\n"))
  }
  if (length(scn_indices) > 3) {
    cat(glue("   ... and {length(scn_indices) - 3} more scenarios\n"))
  }
  cat("\n")
}

# Exit early for dry run
if (args$dry_run) {
  cat("🔍 DRY RUN COMPLETE - No simulation executed\n")
  cat("To run for real, remove --dry_run flag\n")
  quit(status = 0)
}

# =======================================================================
# OUTPUT FILE MANAGEMENT
# =======================================================================

# Define output file paths
outfile <- file.path(args$outdir, glue("sim_chunk_{args$chunk_id:03d}.rds"))
sumfile <- file.path(args$outdir, glue("sim_chunk_{args$chunk_id:03d}_summary.csv"))

# Handle existing output files
if (file.exists(outfile)) {
  response <- readline(prompt = glue("⚠️  Output file {basename(outfile)} exists. Overwrite? (y/N): "))
  if (!tolower(trimws(response)) %in% c("y", "yes")) {
    cat("❌ Execution cancelled by user\n")
    quit(status = 0)
  }
  
  # Create backup of existing file
  backup_file <- glue("{outfile}.backup.{format(Sys.time(), '%Y%m%d_%H%M%S')}")
  file.copy(outfile, backup_file)
  cat(glue("💾 Created backup: {basename(backup_file)}\n"))
}

cat(glue("💾 Results will be saved to: {basename(outfile)}\n"))
cat(glue("📊 Summary will be saved to: {basename(sumfile)}\n\n"))

# =======================================================================
# PARALLEL PROCESSING SETUP
# =======================================================================

#' Configure parallel processing with error handling
#' 
#' Sets up future.apply backend with appropriate error handling
#' and fallback to sequential processing if needed.
setup_parallel_processing <- function(workers, verbose = FALSE) {
  if (verbose) {
    cat(glue("⚙️  Setting up parallel processing with {workers} workers...\n"))
  }
  
  tryCatch({
    # Configure parallel backend
    future::plan(future::multisession, workers = workers)
    
    if (verbose) {
      cat("✅ Parallel processing configured successfully\n")
    }
    
    workers
  }, error = function(e) {
    warning(glue("❌ Failed to set up parallel processing: {e$message}"))
    cat("🔄 Falling back to sequential processing\n")
    
    # Fallback to sequential processing
    future::plan(future::sequential)
    1
  })
}

# Setup parallel processing
actual_workers <- setup_parallel_processing(args$workers, args$verbose)

# =======================================================================
# SCENARIO CHUNKING FOR PARALLEL EXECUTION
# =======================================================================

#' Distribute scenarios across workers for load balancing
#' 
#' Creates balanced chunks that distribute computational load evenly
#' across available workers.
create_worker_chunks <- function(scn_indices, n_workers) {
  if (n_workers == 1) {
    return(list(scn_indices))
  }
  
  # Create balanced distribution
  chunk_assignments <- sort(rep_len(1:n_workers, length(scn_indices)))
  
  # Split scenarios by worker assignment
  split(scn_indices, chunk_assignments)
}

# Create worker-specific chunks
if (actual_workers > 1) {
  scenario_chunks <- create_worker_chunks(scn_indices, actual_workers)
  cat(glue("📦 Created {length(scenario_chunks)} worker chunks (sizes: {paste(lengths(scenario_chunks), collapse=', ')})\n"))
} else {
  scenario_chunks <- list(scn_indices)
  cat("📦 Using single-threaded execution\n")
}

# =======================================================================
# MAIN SIMULATION EXECUTION
# =======================================================================

cat("\n🚀 STARTING SIMULATION\n")
cat("=" * 40, "\n")

# Record start time for performance tracking
start_time <- Sys.time()
cat(glue("⏰ Start time: {format(start_time, '%Y-%m-%d %H:%M:%S')}\n\n"))

#' Execute simulation with comprehensive error handling
#' 
#' Runs the simulation using future_lapply for parallel processing
#' with detailed error reporting and recovery mechanisms.
execute_simulation <- function(scenario_chunks, grid, iters, verbose) {
  
  # Define worker function that will run in parallel
  worker_function <- function(chunk_indices) {
    # Source framework in each worker process
    source("replicate_framework.R", local = TRUE)
    
    # Suppress routine output unless verbose
    if (!verbose) {
      sink(nullfile())
      on.exit(sink())
    }
    
    tryCatch({
      # Run scenarios in this chunk
      run_scenarios_block(grid, scn_indices = chunk_indices, iters = iters)
    }, error = function(e) {
      # Return structured error information instead of NULL
      warning(glue("Worker chunk processing failed: {e$message}"))
      
      # Return empty tibble with correct structure for binding
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
  }
  
  # Execute parallel computation
  if (verbose) {
    cat(glue("🔄 Executing {length(scenario_chunks)} parallel workers...\n"))
  }
  
  future_lapply(
    scenario_chunks, 
    worker_function,
    future.seed = TRUE,      # Ensure reproducible random numbers
    future.globals = FALSE   # Don't automatically export globals (we source manually)
  )
}

# Run the simulation
result_list <- execute_simulation(scenario_chunks, grid, args$iters, args$verbose)

# Record completion time
end_time <- Sys.time()
runtime_minutes <- as.numeric(difftime(end_time, start_time, units = "mins"))

cat("\n✅ SIMULATION COMPLETED\n")
cat("=" * 40, "\n")
cat(glue("⏰ End time: {format(end_time, '%Y-%m-%d %H:%M:%S')}\n"))
cat(glue("⏱️  Total runtime: {round(runtime_minutes, 2)} minutes\n"))
cat(glue("🚀 Processing rate: {round(length(scn_indices) * args$iters / runtime_minutes, 1)} evaluations/minute\n\n"))

# =======================================================================
# RESULTS PROCESSING AND VALIDATION
# =======================================================================

#' Process and validate simulation results
#' 
#' Combines results from parallel workers with comprehensive validation
#' and quality checks.
process_results <- function(result_list, expected_scenarios, iters, verbose) {
  
  if (verbose) {
    cat("📊 Processing simulation results...\n")
  }
  
  # Combine results with error handling
  combined_results <- tryCatch({
    dplyr::bind_rows(result_list)
  }, error = function(e) {
    stop(glue("❌ Failed to combine results from parallel workers: {e$message}"))
  })
  
  # Validate result structure
  expected_cols <- c("method", "auc", "cal_slope", "cal_in_large", "brier", 
                     "rMSPE", "MAPE", "iter", "scn_id", "EPV", "event_frac", 
                     "p", "noise_frac", "sparse")
  
  missing_cols <- setdiff(expected_cols, names(combined_results))
  if (length(missing_cols) > 0) {
    stop(glue("❌ Missing columns in results: {paste(missing_cols, collapse=', ')}"))
  }
  
  # Check result dimensions
  n_methods <- 11
  expected_rows <- expected_scenarios * iters * n_methods
  actual_rows <- nrow(combined_results)
  
  if (actual_rows == 0) {
    stop("❌ No results generated - all simulations failed")
  }
  
  # Calculate success rate
  success_rate <- (actual_rows / expected_rows) * 100
  
  if (verbose) {
    cat(glue("📈 Results summary:\n"))
    cat(glue("   • Expected rows: {expected_rows:,}\n"))
    cat(glue("   • Actual rows: {actual_rows:,}\n"))
    cat(glue("   • Success rate: {round(success_rate, 1)}%\n"))
    cat(glue("   • Unique scenarios: {length(unique(combined_results$scn_id))}\n"))
    cat(glue("   • Methods: {length(unique(combined_results$method))}\n"))
  }
  
  # Warn about low success rates
  if (success_rate < 80) {
    warning(glue("⚠️  Low success rate ({round(success_rate, 1)}%) - many simulations may have failed"))
  }
  
  # Check for missing values
  na_counts <- combined_results %>%
    summarise(across(c(auc, cal_slope, brier, rMSPE, MAPE), ~ sum(is.na(.x))))
  
  if (verbose && any(na_counts > 0)) {
    cat("⚠️  Missing values detected:\n")
    for (col in names(na_counts)[na_counts > 0]) {
      cat(glue("   • {col}: {na_counts[[col]]} missing\n"))
    }
  }
  
  combined_results
}

# Process the results
results <- process_results(result_list, length(scn_indices), args$iters, args$verbose)

# =======================================================================
# FILE OUTPUT WITH BACKUP AND VERIFICATION
# =======================================================================

#' Save results with backup and verification
#' 
#' Implements robust file saving with backup creation and integrity checks
save_results_safely <- function(results, outfile, verbose) {
  
  if (verbose) {
    cat("💾 Saving results...\n")
  }
  
  tryCatch({
    # Create backup if file exists
    if (file.exists(outfile)) {
      backup_file <- glue("{outfile}.backup.{format(Sys.time(), '%Y%m%d_%H%M%S')}")
      file.copy(outfile, backup_file)
      if (verbose) {
        cat(glue("📦 Created backup: {basename(backup_file)}\n"))
      }
    }
    
    # Save main results file
    saveRDS(results, outfile)
    if (verbose) {
      cat(glue("✅ Saved results: {basename(outfile)}\n"))
    }
    
    # Verify saved file integrity
    test_load <- readRDS(outfile)
    if (nrow(test_load) != nrow(results)) {
      warning("❌ File verification failed - saved file may be corrupted")
    } else if (verbose) {
      cat("✅ File integrity verified\n")
    }
    
    # Report file size
    file_size_mb <- round(file.size(outfile) / 1024^2, 2)
    if (verbose) {
      cat(glue("📊 File size: {file_size_mb} MB\n"))
    }
    
  }, error = function(e) {
    stop(glue("❌ Failed to save results: {e$message}"))
  })
}

# Save the main results file
save_results_safely(results, outfile, args$verbose)

# =======================================================================
# SUMMARY STATISTICS GENERATION
# =======================================================================

#' Generate chunk-level summary statistics
#' 
#' Creates summary rankings and performance statistics for this chunk
create_chunk_summary <- function(results, sumfile, verbose) {
  
  if (verbose) {
    cat("📊 Generating chunk summary...\n")
  }
  
  tryCatch({
    # Apply ranking methodology
    ranked_results <- results %>% 
      group_by(scn_id, iter) %>% 
      group_modify(~ rank_with_rounding(.x)) %>% 
      ungroup()
    
    # Calculate summary statistics by method
    summary_stats <- ranked_results %>%
      group_by(method) %>%
      summarise(
        # Mean ranks (primary metrics)
        mean_rank_auc   = mean(r_auc, na.rm = TRUE),
        mean_rank_cals  = mean(r_cals, na.rm = TRUE),
        mean_rank_brier = mean(r_brier, na.rm = TRUE),
        mean_rank_rmspe = mean(r_rmspe, na.rm = TRUE),
        mean_rank_mape  = mean(r_mape, na.rm = TRUE),
        
        # Median ranks for robustness
        median_rank_auc   = median(r_auc, na.rm = TRUE),
        median_rank_brier = median(r_brier, na.rm = TRUE),
        median_rank_cals  = median(r_cals, na.rm = TRUE),
        
        # Sample sizes and missing data
        n_evaluations = n(),
        na_count_auc = sum(is.na(r_auc)),
        na_count_cals = sum(is.na(r_cals)),
        na_count_brier = sum(is.na(r_brier)),
        
        # Performance metric summaries  
        mean_auc = mean(auc, na.rm = TRUE),
        mean_brier = mean(brier, na.rm = TRUE),
        mean_cal_slope = mean(cal_slope, na.rm = TRUE),
        
        .groups = 'drop'
      ) %>%
      arrange(mean_rank_auc)  # Sort by primary ranking metric
    
    # Save summary file
    readr::write_csv(summary_stats, sumfile)
    
    if (verbose) {
      cat(glue("✅ Saved summary: {basename(sumfile)}\n"))
      
      # Display top performers
      cat("\n🏆 TOP 5 METHODS BY AUC RANK:\n")
      top5 <- summary_stats %>% 
        head(5) %>% 
        select(method, mean_rank_auc, mean_rank_brier, mean_auc, n_evaluations)
      
      print(top5, width = Inf)
    }
    
  }, error = function(e) {
    warning(glue("❌ Failed to create summary: {e$message}"))
  })
}

# Generate chunk summary
create_chunk_summary(results, sumfile, args$verbose)

# =======================================================================
# FINAL REPORTING AND CLEANUP
# =======================================================================

# Generate final report
cat("\n📋 EXECUTION SUMMARY\n")
cat("=" * 40, "\n")
cat(glue("✅ Chunk {args$chunk_id}/{args$chunks} completed successfully\n"))
cat(glue("📊 Processed {length(scn_indices)} scenarios × {args$iters} iterations\n"))
cat(glue("🎯 Generated {nrow(results):,} method evaluations\n"))
cat(glue("⏱️  Total runtime: {round(runtime_minutes, 2)} minutes\n"))
cat(glue("🚀 Average speed: {round(length(scn_indices) * args$iters / runtime_minutes, 1)} scenarios/minute\n"))
cat(glue("💾 Results saved to: {basename(outfile)}\n"))
cat(glue("📊 Summary saved to: {basename(sumfile)}\n"))

# Memory usage report
if (args$verbose) {
  gc_info <- gc(verbose = FALSE)
  memory_mb <- sum(gc_info[,'used'])
  cat(glue("🧠 Peak memory usage: ~{round(memory_mb, 1)} MB\n"))
}

# Cleanup parallel workers
future::plan(future::sequential)

# Final status
cat("\n🎉 CHUNK EXECUTION COMPLETED SUCCESSFULLY!\n")
cat("\nTo aggregate results from all chunks, run:\n")
cat(glue("  Rscript aggregate_results.R {args$outdir}\n"))

# Exit with success
quit(status = 0)
