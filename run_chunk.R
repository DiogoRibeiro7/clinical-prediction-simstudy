# =======================================================================
# run_chunk.R
# Run a chunk of scenarios in parallel and save results to disk
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

dir.create(args$outdir, showWarnings = FALSE, recursive = TRUE)

grid <- make_full_grid()
N <- nrow(grid) # 1050
chunk_size <- ceiling(N / args$chunks)
start <- (args$chunk_id - 1) * chunk_size + 1
end   <- min(args$chunk_id * chunk_size, N)
scn_indices <- seq.int(start, end)

message(glue("Running chunk {args$chunk_id}/{args$chunks} -> scenarios [{start}:{end}] (n={length(scn_indices)})"))

# Parallel plan
future::plan(future::multisession, workers = args$workers)

# We’ll split the scenario indices across workers and bind
parts <- split(scn_indices, seq_along(scn_indices))

res_list <- future_lapply(parts, function(idx) {
  source("replicate_framework.R", local = TRUE)
  run_scenarios_block(grid, scn_indices = idx, iters = args$iters)
})

res <- dplyr::bind_rows(res_list)

# Save raw results (per-iteration, per-method)
outfile <- file.path(args$outdir, glue("sim_chunk_{args$chunk_id:03d}.rds"))
saveRDS(res, outfile)
message(glue("Saved: {outfile}"))

# Also save a per-chunk rank summary for quick sanity checks
ranked <- res %>% group_by(scn_id) %>% group_modify(~ rank_with_rounding(.x)) %>% ungroup()
sumfile <- file.path(args$outdir, glue("sim_chunk_{args$chunk_id:03d}_rank_summary.csv"))
ranked %>%
  group_by(method) %>%
  summarise(
    mean_rank_auc   = mean(r_auc, na.rm = TRUE),
    mean_rank_cals  = mean(r_cals, na.rm = TRUE),
    mean_rank_brier = mean(r_brier, na.rm = TRUE),
    mean_rank_rmspe = mean(r_rmspe, na.rm = TRUE),
    mean_rank_mape  = mean(r_mape, na.rm = TRUE),
    n = dplyr::n()
  ) %>%
  arrange(mean_rank_auc) %>%
  readr::write_csv(sumfile)
message(glue("Saved summary: {sumfile}"))
