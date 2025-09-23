# =======================================================================
# aggregate_results.R
# Combine all chunk files and produce final summaries + plots
# FIXED VERSION - improved error handling and diagnostics
# =======================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(ggplot2)
  library(glue)
})

source("replicate_framework.R")

args <- commandArgs(trailingOnly = TRUE)
outdir <- ifelse(length(args) >= 1, args[[1]], "results")
pattern <- "^sim_chunk_\\d{3}\\.rds$"

if (!dir.exists(outdir)) {
  stop(glue("Output directory {outdir} does not exist"))
}

files <- list.files(outdir, pattern = pattern, full.names = TRUE)
if (length(files) == 0) {
  stop(glue("No chunk files found matching pattern '{pattern}' in {outdir}"))
}

message(glue("Found {length(files)} chunk files"))

# Load and validate chunk files
raw_list <- vector("list", length(files))
failed_files <- character(0)

for (i in seq_along(files)) {
  tryCatch({
    chunk_data <- readRDS(files[i])
    
    # Validate chunk structure
    expected_cols <- c("method", "auc", "cal_slope", "cal_in_large", "brier", 
                       "rMSPE", "MAPE", "iter", "scn_id", "EPV", "event_frac", 
                       "p", "noise_frac", "sparse")
    
    missing_cols <- setdiff(expected_cols, names(chunk_data))
    if (length(missing_cols) > 0) {
      warning(glue("File {basename(files[i])} missing columns: {paste(missing_cols, collapse = ', ')}"))
    }
    
    if (nrow(chunk_data) == 0) {
      warning(glue("File {basename(files[i])} is empty"))
    }
    
    raw_list[[i]] <- chunk_data
    message(glue("Loaded {basename(files[i])}: {nrow(chunk_data)} rows"))
    
  }, error = function(e) {
    warning(glue("Failed to load {basename(files[i])}: {e$message}"))
    failed_files <- c(failed_files, files[i])
  })
}

if (length(failed_files) > 0) {
  message(glue("Failed to load {length(failed_files)} files: {paste(basename(failed_files), collapse = ', ')}"))
}

# Remove NULL entries and combine
raw_list <- raw_list[!sapply(raw_list, is.null)]
if (length(raw_list) == 0) {
  stop("No valid chunk files could be loaded")
}

raw <- tryCatch({
  combined <- purrr::map_dfr(raw_list, identity)
  message(glue("Combined data: {nrow(combined)} rows, {length(unique(combined$scn_id))} scenarios"))
  combined
}, error = function(e) {
  stop(glue("Failed to combine chunk data: {e$message}"))
})

# Data quality checks
message("\n=== Data Quality Summary ===")
cat("Scenarios per method:\n")
method_counts <- raw %>% count(method) %>% arrange(desc(n))
print(method_counts)

cat("\nMissing values by metric:\n")
na_summary <- raw %>%
  summarise(
    auc_na = sum(is.na(auc)),
    cal_slope_na = sum(is.na(cal_slope)),
    brier_na = sum(is.na(brier)),
    rmspe_na = sum(is.na(rMSPE)),
    mape_na = sum(is.na(MAPE)),
    total_rows = n()
  ) %>%
  pivot_longer(ends_with("_na"), names_to = "metric", values_to = "na_count") %>%
  mutate(pct_na = round(100 * na_count / total_rows, 2))
print(na_summary)

# Check for expected number of scenarios
expected_scenarios <- 1050
unique_scenarios <- length(unique(raw$scn_id))
if (unique_scenarios != expected_scenarios) {
  warning(glue("Expected {expected_scenarios} scenarios, found {unique_scenarios}"))
}

# Rank inside each scenario/iteration set with better error handling
ranked <- tryCatch({
  raw %>%
    group_by(scn_id, iter) %>%
    group_modify(~ rank_with_rounding(.x)) %>%
    ungroup()
}, error = function(e) {
  stop(glue("Failed to compute rankings: {e$message}"))
})

# Overall method ranking across all scenarios/iterations
summary_ranks <- ranked %>%
  group_by(method) %>%
  summarise(
    mean_rank_auc   = mean(r_auc,   na.rm = TRUE),
    mean_rank_cals  = mean(r_cals,  na.rm = TRUE),
    mean_rank_brier = mean(r_brier, na.rm = TRUE),
    mean_rank_rmspe = mean(r_rmspe, na.rm = TRUE),
    mean_rank_mape  = mean(r_mape,  na.rm = TRUE),
    median_rank_auc = median(r_auc, na.rm = TRUE),
    median_rank_cals = median(r_cals, na.rm = TRUE),
    median_rank_brier = median(r_brier, na.rm = TRUE),
    n = n(),
    na_count_auc = sum(is.na(r_auc)),
    na_count_cals = sum(is.na(r_cals)),
    na_count_brier = sum(is.na(r_brier))
  ) %>%
  arrange(mean_rank_auc)

# Save results with error handling
summary_file <- file.path(outdir, "summary_ranks_overall.csv")
tryCatch({
  readr::write_csv(summary_ranks, summary_file)
  message(glue("\nSaved overall rankings: {summary_file}"))
}, error = function(e) {
  stop(glue("Failed to save summary rankings: {e$message}"))
})

print(summary_ranks)

# Create more detailed summaries by scenario characteristics
cat("\n=== Performance by Scenario Characteristics ===\n")

# Performance by EPV
epv_summary <- ranked %>%
  group_by(EPV, method) %>%
  summarise(mean_rank_auc = mean(r_auc, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = method, values_from = mean_rank_auc) %>%
  arrange(EPV)

epv_file <- file.path(outdir, "summary_by_EPV.csv")
readr::write_csv(epv_summary, epv_file)
message(glue("Saved EPV summary: {epv_file}"))

# Performance by number of predictors
p_summary <- ranked %>%
  group_by(p, method) %>%
  summarise(mean_rank_auc = mean(r_auc, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = method, values_from = mean_rank_auc) %>%
  arrange(p)

p_file <- file.path(outdir, "summary_by_p.csv")
readr::write_csv(p_summary, p_file)
message(glue("Saved predictors summary: {p_file}"))

# Generate enhanced visualizations
message("\n=== Creating Visualizations ===")

# Create output directory for plots
plot_dir <- file.path(outdir, "plots")
dir.create(plot_dir, showWarnings = FALSE)

# Enhanced AUC plot
p1 <- ggplot(raw %>% filter(!is.na(auc)), aes(x = reorder(method, -auc, FUN = median), y = auc)) +
  geom_boxplot(outlier.alpha = 0.3, fill = "lightblue", alpha = 0.7) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 3)), 
               vjust = -0.5, size = 3, color = "red") +
  coord_flip() + 
  theme_bw() +
  theme(axis.text = element_text(size = 10)) +
  labs(title = "AUC Distribution by Method", 
       subtitle = glue("Based on {nrow(raw)} observations across {unique_scenarios} scenarios"),
       x = "Method", y = "AUC")

ggsave(file.path(plot_dir, "auc_by_method.png"), p1, width = 10, height = 8, dpi = 300)

# Enhanced calibration slope plot  
p2 <- ggplot(raw %>% filter(!is.na(cal_slope)), aes(x = reorder(method, abs(cal_slope - 1), FUN = median), y = cal_slope)) +
  geom_hline(yintercept = 1, linetype = 2, color = "red", size = 1) +
  geom_boxplot(outlier.alpha = 0.3, fill = "lightgreen", alpha = 0.7) +
  coord_flip() + 
  theme_bw() +
  theme(axis.text = element_text(size = 10)) +
  labs(title = "Calibration Slope by Method", 
       subtitle = "Red line shows perfect calibration (slope = 1)",
       x = "Method", y = "Calibration Slope (winsorized to [0.01, 10])")

ggsave(file.path(plot_dir, "cal_slope_by_method.png"), p2, width = 10, height = 8, dpi = 300)

# Enhanced Brier score plot
p3 <- ggplot(raw %>% filter(!is.na(brier)), aes(x = reorder(method, brier, FUN = median), y = brier)) +
  geom_boxplot(outlier.alpha = 0.3, fill = "lightyellow", alpha = 0.7) +
  coord_flip() + 
  theme_bw() +
  theme(axis.text = element_text(size = 10)) +
  labs(title = "Brier Score by Method", 
       subtitle = "Lower values indicate better performance",
       x = "Method", y = "Brier Score")

ggsave(file.path(plot_dir, "brier_by_method.png"), p3, width = 10, height = 8, dpi = 300)

# Performance by EPV visualization
p4 <- ranked %>%
  filter(!is.na(r_auc)) %>%
  group_by(EPV, method) %>%
  summarise(mean_rank = mean(r_auc), .groups = 'drop') %>%
  ggplot(aes(x = EPV, y = mean_rank, color = method)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_log10() +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(title = "AUC Ranking by Events Per Variable (EPV)",
       x = "Events Per Variable (log scale)", y = "Mean Rank (lower is better)")

ggsave(file.path(plot_dir, "performance_by_epv.png"), p4, width = 12, height = 8, dpi = 300)

# Performance by number of predictors
p5 <- ranked %>%
  filter(!is.na(r_auc)) %>%
  group_by(p, method) %>%
  summarise(mean_rank = mean(r_auc), .groups = 'drop') %>%
  ggplot(aes(x = p, y = mean_rank, color = method)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_log10() +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(title = "AUC Ranking by Number of Predictors",
       x = "Number of Predictors (log scale)", y = "Mean Rank (lower is better)")

ggsave(file.path(plot_dir, "performance_by_p.png"), p5, width = 12, height = 8, dpi = 300)

message(glue("Saved visualizations to: {plot_dir}"))

# Create comprehensive report
report_file <- file.path(outdir, "simulation_report.txt")
cat(
  "=== LOHMANN ET AL. (2023) SIMULATION REPLICATION REPORT ===\n",
  glue("Generated: {Sys.time()}\n"),
  glue("Total observations: {nrow(raw)}\n"),
  glue("Unique scenarios: {unique_scenarios}\n"),
  glue("Methods compared: {length(unique(raw$method))}\n"),
  "\nTOP 5 METHODS BY MEAN AUC RANK:\n",
  file = report_file
)

# Append ranking table to report
write.table(
  summary_ranks %>% head(5) %>% select(method, mean_rank_auc, mean_rank_brier, mean_rank_cals),
  file = report_file, append = TRUE, row.names = FALSE, quote = FALSE, sep = "\t"
)

message(glue("\nSimulation aggregation completed successfully!"))
message(glue("Results saved to: {outdir}"))
message(glue("Key findings in: {report_file}"))
