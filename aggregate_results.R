# =======================================================================
# aggregate_results.R
# Combine all chunk files and produce final summaries + plots
# =======================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(ggplot2)
})

source("replicate_framework.R")

args <- commandArgs(trailingOnly = TRUE)
outdir <- ifelse(length(args) >= 1, args[[1]], "results")
pattern <- "^sim_chunk_\\d{3}\\.rds$"

files <- list.files(outdir, pattern = pattern, full.names = TRUE)
stopifnot(length(files) > 0)

raw <- purrr::map_dfr(files, readRDS)

# Rank inside each scenario/iteration set
ranked <- raw %>%
  group_by(scn_id) %>%
  group_modify(~ rank_with_rounding(.x)) %>%
  ungroup()

# Overall method ranking across all scenarios/iterations
summary_ranks <- ranked %>%
  group_by(method) %>%
  summarise(
    mean_rank_auc   = mean(r_auc,   na.rm = TRUE),
    mean_rank_cals  = mean(r_cals,  na.rm = TRUE),
    mean_rank_brier = mean(r_brier, na.rm = TRUE),
    mean_rank_rmspe = mean(r_rmspe, na.rm = TRUE),
    mean_rank_mape  = mean(r_mape,  na.rm = TRUE),
    n = n()
  ) %>%
  arrange(mean_rank_auc)

readr::write_csv(summary_ranks, file.path(outdir, "summary_ranks_overall.csv"))
print(summary_ranks)

# Optional quick visuals (saved as PNGs)
p1 <- ggplot(raw, aes(x = method, y = auc)) +
  geom_boxplot(outlier.shape = NA) + coord_flip() + theme_bw() +
  labs(title = "AUC by method across all scenarios")
ggsave(file.path(outdir, "auc_by_method.png"), p1, width = 8, height = 6, dpi = 160)

p2 <- ggplot(raw, aes(x = method, y = cal_slope)) +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_boxplot(outlier.shape = NA) + coord_flip() + theme_bw() +
  labs(title = "Calibration slope by method (winsorized to [0.01, 10])")
ggsave(file.path(outdir, "cal_slope_by_method.png"), p2, width = 8, height = 6, dpi = 160)

p3 <- ggplot(raw, aes(x = method, y = brier)) +
  geom_boxplot(outlier.shape = NA) + coord_flip() + theme_bw() +
  labs(title = "Brier score by method (lower is better)")
ggsave(file.path(outdir, "brier_by_method.png"), p3, width = 8, height = 6, dpi = 160)

message("Aggregation complete.")

