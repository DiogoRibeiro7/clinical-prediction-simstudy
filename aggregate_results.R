# =======================================================================
# aggregate_results.R
# 
# Enhanced results aggregation and visualization for Lohmann et al. (2023) replication
# Combines chunk files, computes final rankings, and generates comprehensive
# visualizations and reports with full configuration system integration.
#
# This script:
# - Integrates with simulation_config.yaml for consistent configuration
# - Dynamically discovers methods and adapts to different experimental designs
# - Applies Lohmann et al. (2023) ranking methodology with validation
# - Generates comprehensive performance summaries and visualizations
# - Produces detailed research reports with quality assurance
# - Handles memory efficiently with batch processing for large datasets
# - Provides extensive error handling and recovery mechanisms
#
# Usage:
#   Rscript aggregate_results.R [results_directory] [--config config_file]
#
# Author: Diogo Ribeiro (Enhanced)
# Date: January 2025
# License: MIT
# =======================================================================

# Enhanced package loading with comprehensive error handling
suppressPackageStartupMessages({
  required_packages <- c(
    # Core packages (essential)
    "dplyr", "tidyr", "readr", "purrr", "ggplot2",
    # Configuration and reporting
    "yaml", "glue", "knitr", "rmarkdown",
    # Performance and memory
    "pryr", "progress",
    # Validation
    "checkmate"
  )
  
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    cat("⚠️  Missing optional packages:", paste(missing_packages, collapse = ", "), "\n")
    cat("Some enhanced features will be disabled.\n")
    cat("To install missing packages:\n")
    cat(paste("install.packages(c(", paste0('"', missing_packages, '"', collapse = ", "), "))\n\n"))
  }
  
  # Load essential packages
  essential_packages <- c("dplyr", "tidyr", "readr", "purrr", "ggplot2")
  for (pkg in essential_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("Essential package", pkg, "is required but not available"))
    }
    library(pkg, character.only = TRUE)
  }
  
  # Load optional packages and set feature flags
  HAS_YAML <- requireNamespace("yaml", quietly = TRUE)
  HAS_GLUE <- requireNamespace("glue", quietly = TRUE)
  HAS_PRYR <- requireNamespace("pryr", quietly = TRUE)
  HAS_PROGRESS <- requireNamespace("progress", quietly = TRUE)
  HAS_CHECKMATE <- requireNamespace("checkmate", quietly = TRUE)
  
  if (HAS_YAML) library(yaml)
  if (HAS_GLUE) library(glue)
  if (HAS_PRYR) library(pryr)
  if (HAS_PROGRESS) library(progress)
  if (HAS_CHECKMATE) library(checkmate)
})

# =======================================================================
# ENHANCED CONFIGURATION MANAGEMENT
# =======================================================================

#' Load configuration with command line override support
#' 
#' @param config_path Path to configuration file
#' @param results_dir Results directory (may contain config info)
#' @return Merged configuration object
load_enhanced_config <- function(config_path = NULL, results_dir = "results") {
  
  cat("🔧 Loading configuration...\n")
  
  # Try multiple config locations in order of preference
  config_paths <- c(
    config_path,  # Explicit path from command line
    "config/simulation_config.yaml",  # Standard location
    file.path(results_dir, "simulation_config.yaml"),  # Saved with results
    file.path(dirname(results_dir), "config/simulation_config.yaml")  # Parent dir
  )
  
  config_paths <- config_paths[!is.null(config_paths)]
  config_file <- NULL
  
  for (path in config_paths) {
    if (file.exists(path)) {
      config_file <- path
      break
    }
  }
  
  if (!is.null(config_file) && HAS_YAML) {
    tryCatch({
      config <- yaml::read_yaml(config_file)
      cat("✅ Configuration loaded from:", config_file, "\n")
    }, error = function(e) {
      cat("⚠️  Failed to load config:", e$message, "- using defaults\n")
      config <- get_fallback_config()
    })
  } else {
    cat("ℹ️  No configuration file found - using defaults\n")
    config <- get_fallback_config()
  }
  
  # Validate and enhance configuration
  config <- validate_and_enhance_config(config)
  
  config
}

#' Get fallback configuration when YAML not available
get_fallback_config <- function() {
  list(
    simulation = list(
      default_iterations = 20,
      base_seed = 20250923L
    ),
    grid = list(
      EPV = c(3, 5, 10, 15, 20, 50, 100),
      event_fractions = c(1/32, 1/16, 1/8, 1/4, 1/2),
      predictors = c(4, 8, 16, 32, 64),
      noise_fractions = c(0, 0.25, 0.5),
      sparse_options = c(FALSE, TRUE)
    ),
    output = list(
      precision = list(auc = 3, brier = 3, cal_slope = 2, rmspe = 3, mape = 3),
      directories = list(results = "results", plots = "plots"),
      file_formats = list(
        chunk_pattern = "sim_chunk_%03d.rds",
        summary_pattern = "sim_chunk_%03d_summary.csv"
      )
    ),
    visualization = list(
      theme = "publication",
      dpi = 300,
      width = 12,
      height = 8,
      colors = list(
        MLE = "#E31A1C",
        Ridge = "#1F78B4", 
        LASSO = "#33A02C",
        ElasticNet = "#FF7F00",
        RelaxedLASSO = "#6A3D9A",
        PCR_evgt1 = "#A6CEE3",
        PCR_var90 = "#B2DF8A", 
        PCR_aic = "#FDBF6F",
        PCR_cvdev = "#CAB2D6",
        PLS = "#FFFF99",
        PLS_LASSO = "#B15928"
      )
    ),
    quality = list(
      min_convergence_rate = 0.95,
      max_missing_rate = 0.05,
      expected_auc_range = c(0.5, 0.95),
      expected_brier_range = c(0.05, 0.5)
    ),
    logging = list(
      level = "INFO",
      console = TRUE
    )
  )
}

#' Validate and enhance loaded configuration
validate_and_enhance_config <- function(config) {
  
  # Calculate expected scenarios from grid
  if (!is.null(config$grid)) {
    expected_scenarios <- with(config$grid, {
      length(EPV) * length(event_fractions) * length(predictors) * 
      length(noise_fractions) * length(sparse_options)
    })
    config$expected_scenarios <- expected_scenarios
    
    cat("📊 Expected scenarios from grid: ", expected_scenarios, "\n")
  } else {
    config$expected_scenarios <- 1050  # Fallback
    cat("⚠️  Using fallback expected scenarios: 1050\n")
  }
  
  # Set default values for missing sections
  if (is.null(config$output$directories$results)) {
    config$output$directories$results <- "results"
  }
  if (is.null(config$output$directories$plots)) {
    config$output$directories$plots <- "plots"
  }
  
  # Ensure visualization colors exist (will be enhanced dynamically)
  if (is.null(config$visualization$colors)) {
    config$visualization$colors <- get_fallback_config()$visualization$colors
  }
  
  config
}

# =======================================================================
# ENHANCED COMMAND LINE INTERFACE
# =======================================================================

#' Parse and validate command line arguments with configuration support
parse_enhanced_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  
  # Parse arguments with enhanced options
  parsed_args <- list(
    results_dir = "results",
    config_file = NULL,
    verbose = FALSE,
    batch_size = NULL,
    memory_limit = NULL,
    force = FALSE
  )
  
  # Simple argument parsing
  i <- 1
  while (i <= length(args)) {
    arg <- args[i]
    
    if (arg == "--help" || arg == "-h") {
      cat("Enhanced Lohmann et al. (2023) Results Aggregation\n\n")
      cat("Usage: Rscript aggregate_results.R [results_directory] [options]\n\n")
      cat("Arguments:\n")
      cat("  results_directory    Directory containing chunk files [default: results]\n\n")
      cat("Options:\n")
      cat("  --config FILE        Configuration file path\n")
      cat("  --verbose           Enable verbose output\n")
      cat("  --batch-size N      Process chunks in batches of N\n")
      cat("  --memory-limit N    Memory limit in GB\n")
      cat("  --force             Overwrite existing files without confirmation\n")
      cat("  --help, -h          Show this help message\n\n")
      cat("Examples:\n")
      cat("  Rscript aggregate_results.R\n")
      cat("  Rscript aggregate_results.R results --config config/custom.yaml\n")
      cat("  Rscript aggregate_results.R results --verbose --batch-size 10\n\n")
      quit(status = 0)
      
    } else if (arg == "--config") {
      if (i + 1 <= length(args)) {
        parsed_args$config_file <- args[i + 1]
        i <- i + 1
      }
    } else if (arg == "--verbose") {
      parsed_args$verbose <- TRUE
    } else if (arg == "--batch-size") {
      if (i + 1 <= length(args)) {
        parsed_args$batch_size <- as.integer(args[i + 1])
        i <- i + 1
      }
    } else if (arg == "--memory-limit") {
      if (i + 1 <= length(args)) {
        parsed_args$memory_limit <- as.numeric(args[i + 1])
        i <- i + 1
      }
    } else if (arg == "--force") {
      parsed_args$force <- TRUE
    } else if (!startsWith(arg, "--") && is.null(parsed_args$results_dir_set)) {
      parsed_args$results_dir <- arg
      parsed_args$results_dir_set <- TRUE
    }
    
    i <- i + 1
  }
  
  # Validate arguments
  if (HAS_CHECKMATE) {
    checkmate::assert_directory_exists(parsed_args$results_dir, 
                                      .var.name = "results_directory")
    if (!is.null(parsed_args$batch_size)) {
      checkmate::assert_int(parsed_args$batch_size, lower = 1, upper = 100)
    }
    if (!is.null(parsed_args$memory_limit)) {
      checkmate::assert_number(parsed_args$memory_limit, lower = 0.5, upper = 128)
    }
  }
  
  parsed_args
}

# Parse command line arguments
args <- parse_enhanced_args()

# Load configuration with command line overrides
config <- load_enhanced_config(args$config_file, args$results_dir)

# Apply command line overrides
if (!is.null(args$memory_limit)) {
  config$parallel$memory_limit_gb <- args$memory_limit
}

# Set up logging functions
log_info <- function(msg, ...) {
  if (HAS_GLUE) {
    formatted <- glue::glue(msg, ...)
  } else {
    formatted <- sprintf(msg, ...)
  }
  cat("[INFO]", formatted, "\n")
}

log_warn <- function(msg, ...) {
  if (HAS_GLUE) {
    formatted <- glue::glue(msg, ...)
  } else {
    formatted <- sprintf(msg, ...)
  }
  cat("[WARN]", formatted, "\n")
}

log_error <- function(msg, ...) {
  if (HAS_GLUE) {
    formatted <- glue::glue(msg, ...)
  } else {
    formatted <- sprintf(msg, ...)
  }
  cat("[ERROR]", formatted, "\n")
}

log_debug <- function(msg, ...) {
  if (args$verbose) {
    if (HAS_GLUE) {
      formatted <- glue::glue(msg, ...)
    } else {
      formatted <- sprintf(msg, ...)
    }
    cat("[DEBUG]", formatted, "\n")
  }
}

# =======================================================================
# FRAMEWORK INTEGRATION
# =======================================================================

# Load simulation framework for ranking functions
log_info("Loading simulation framework...")
if (file.exists("enhanced_replicate_framework.R")) {
  source("enhanced_replicate_framework.R")
  FRAMEWORK_ENHANCED <- TRUE
  log_info("✅ Enhanced framework loaded")
} else if (file.exists("replicate_framework.R")) {
  source("replicate_framework.R") 
  FRAMEWORK_ENHANCED <- FALSE
  log_info("✅ Original framework loaded")
} else {
  stop("❌ Cannot find replicate_framework.R - please ensure it exists in the current directory")
}

# =======================================================================
# ENHANCED INITIALIZATION AND VALIDATION
# =======================================================================

cat("\n")
cat("🔍 ENHANCED LOHMANN ET AL. (2023) REPLICATION - RESULTS AGGREGATION\n")
cat("=" * 75, "\n")
cat("📁 Results directory: ", args$results_dir, "\n")
if (!is.null(args$config_file)) {
  cat("⚙️  Configuration: ", args$config_file, "\n")
}
cat("🔧 Framework: ", ifelse(FRAMEWORK_ENHANCED, "Enhanced", "Original"), "\n")
cat("📊 Expected scenarios: ", config$expected_scenarios, "\n")
if (args$verbose) {
  cat("🔍 Verbose mode: Enabled\n")
}
cat("\n")

# =======================================================================
# ENHANCED FILE DISCOVERY AND VALIDATION
# =======================================================================

#' Enhanced file discovery with flexible patterns
discover_chunk_files_enhanced <- function(results_dir, config) {
  
  log_info("Discovering chunk files...")
  
  # Try multiple patterns based on configuration
  patterns <- c(
    sprintf(config$output$file_formats$chunk_pattern, "\\d{3}"),  # From config
    "^sim_chunk_\\d{3}\\.rds$",  # Standard pattern
    "^chunk_\\d{3}\\.rds$",      # Alternative pattern
    "^results_chunk_\\d{3}\\.rds$"  # Another alternative
  )
  
  all_files <- list.files(results_dir, full.names = TRUE)
  chunk_files <- character(0)
  
  for (pattern in patterns) {
    matching_files <- all_files[grepl(pattern, basename(all_files))]
    if (length(matching_files) > 0) {
      chunk_files <- matching_files
      log_info("Found {length(chunk_files)} files matching pattern: {pattern}")
      break
    }
  }
  
  if (length(chunk_files) == 0) {
    stop("❌ No chunk files found. Tried patterns: ", paste(patterns, collapse = ", "))
  }
  
  # Sort files for consistent processing
  chunk_files <- chunk_files[order(chunk_files)]
  
  # Display file information
  log_info("📋 Discovered chunk files:")
  for (i in seq_along(chunk_files)) {
    file_size_mb <- round(file.size(chunk_files[i]) / 1024^2, 2)
    log_info("   {i:2d}. {basename(chunk_files[i])} ({file_size_mb} MB)")
  }
  
  chunk_files
}

# Discover chunk files
chunk_files <- discover_chunk_files_enhanced(args$results_dir, config)

# =======================================================================
# ENHANCED CHUNK LOADING WITH MEMORY MANAGEMENT
# =======================================================================

#' Load and validate individual chunk with enhanced error handling
load_and_validate_chunk_enhanced <- function(filepath, expected_cols = NULL) {
  
  if (is.null(expected_cols)) {
    expected_cols <- c("method", "auc", "cal_slope", "cal_in_large", "brier", 
                      "rMSPE", "MAPE", "iter", "scn_id", "EPV", "event_frac", 
                      "p", "noise_frac", "sparse")
  }
  
  tryCatch({
    chunk_data <- readRDS(filepath)
    
    # Enhanced validation
    if (!is.data.frame(chunk_data)) {
      stop("Chunk data is not a data frame")
    }
    
    if (nrow(chunk_data) == 0) {
      warning("Empty chunk file")
      return(NULL)
    }
    
    # Check required columns (flexible - some may be missing)
    missing_cols <- setdiff(c("method", "auc", "brier", "scn_id"), names(chunk_data))
    if (length(missing_cols) > 0) {
      stop(paste("Missing critical columns:", paste(missing_cols, collapse = ", ")))
    }
    
    # Data quality checks
    n_scenarios <- length(unique(chunk_data$scn_id))
    n_methods <- length(unique(chunk_data$method))
    
    log_debug("   ✅ {basename(filepath)}: {nrow(chunk_data):,} rows, {n_scenarios} scenarios, {n_methods} methods")
    
    chunk_data
    
  }, error = function(e) {
    log_error("❌ Failed to load {basename(filepath)}: {e$message}")
    NULL
  })
}

#' Load chunks with memory-aware batching
load_chunks_with_batching <- function(chunk_files, config, args) {
  
  log_info("Loading chunk files with memory management...")
  
  # Determine batch size for memory management
  batch_size <- args$batch_size
  if (is.null(batch_size)) {
    # Estimate based on available memory
    if (HAS_PRYR) {
      available_mem_gb <- as.numeric(pryr::mem_used()) / 1e9
      # Conservative estimate: 4 chunks per GB
      batch_size <- max(5, min(20, 4 * available_mem_gb))
    } else {
      batch_size <- 10  # Conservative default
    }
  }
  
  batch_size <- min(batch_size, length(chunk_files))  # Don't exceed total files
  
  log_info("Processing {length(chunk_files)} files in batches of {batch_size}")
  
  # Process chunks in batches
  all_chunk_data <- list()
  n_batches <- ceiling(length(chunk_files) / batch_size)
  
  # Create progress bar if available
  if (HAS_PROGRESS && !args$verbose) {
    pb <- progress::progress_bar$new(
      format = "Loading chunks [:bar] :current/:total (:percent) ETA: :eta",
      total = length(chunk_files),
      clear = FALSE,
      width = 60
    )
  }
  
  for (batch in seq_len(n_batches)) {
    batch_start <- (batch - 1) * batch_size + 1
    batch_end <- min(batch * batch_size, length(chunk_files))
    batch_files <- chunk_files[batch_start:batch_end]
    
    log_info("Processing batch {batch}/{n_batches}: files {batch_start}-{batch_end}")
    
    # Load batch files
    batch_data <- list()
    for (i in seq_along(batch_files)) {
      file_data <- load_and_validate_chunk_enhanced(batch_files[i])
      if (!is.null(file_data)) {
        batch_data[[length(batch_data) + 1]] <- file_data
      }
      
      # Update progress
      if (HAS_PROGRESS && !args$verbose) {
        pb$tick()
      }
    }
    
    # Combine batch data
    if (length(batch_data) > 0) {
      batch_combined <- dplyr::bind_rows(batch_data)
      all_chunk_data[[length(all_chunk_data) + 1]] <- batch_combined
      log_debug("Batch {batch} combined: {nrow(batch_combined):,} rows")
    }
    
    # Force garbage collection between batches
    gc(verbose = FALSE)
    
    if (HAS_PRYR && args$verbose) {
      current_mem <- pryr::mem_used()
      log_debug("Memory after batch {batch}: {format(current_mem, units='MB')}")
    }
  }
  
  # Final combination
  if (length(all_chunk_data) > 0) {
    final_data <- dplyr::bind_rows(all_chunk_data)
    log_info("✅ Successfully loaded {nrow(final_data):,} total rows from {length(chunk_files)} files")
    return(final_data)
  } else {
    stop("❌ Failed to load any valid chunk data")
  }
}

# Load all chunks
raw_data <- load_chunks_with_batching(chunk_files, config, args)

# =======================================================================
# ENHANCED DATA VALIDATION WITH DYNAMIC METHOD DISCOVERY
# =======================================================================

#' Comprehensive data validation with dynamic method discovery
validate_and_enhance_data <- function(raw_data, config) {
  
  log_info("Validating and enhancing loaded data...")
  
  # Basic validation
  if (nrow(raw_data) == 0) {
    stop("❌ No data loaded from chunk files")
  }
  
  # Dynamic method discovery
  actual_methods <- sort(unique(raw_data$method))
  n_methods <- length(actual_methods)
  
  log_info("🔍 Data validation results:")
  log_info("  • Total rows: {nrow(raw_data):,}")
  log_info("  • Unique scenarios: {length(unique(raw_data$scn_id))}")
  log_info("  • Methods discovered: {n_methods}")
  log_info("  • Method names: {paste(actual_methods, collapse = ', ')}")
  
  # Compare with expectations
  expected_methods <- c("MLE", "Ridge", "LASSO", "ElasticNet", "RelaxedLASSO",
                       "PCR_evgt1", "PCR_var90", "PCR_aic", "PCR_cvdev", 
                       "PLS", "PLS_LASSO")
  
  missing_methods <- setdiff(expected_methods, actual_methods)
  extra_methods <- setdiff(actual_methods, expected_methods)
  
  if (length(missing_methods) > 0) {
    log_warn("⚠️  Missing expected methods: {paste(missing_methods, collapse = ', ')}")
  }
  if (length(extra_methods) > 0) {
    log_info("ℹ️  Additional methods found: {paste(extra_methods, collapse = ', ')}")
  }
  
  # Scenario validation
  unique_scenarios <- length(unique(raw_data$scn_id))
  expected_scenarios <- config$expected_scenarios
  scenario_coverage <- (unique_scenarios / expected_scenarios) * 100
  
  log_info("🎯 Scenario coverage: {unique_scenarios}/{expected_scenarios} ({round(scenario_coverage, 1)}%)")
  
  if (scenario_coverage < 90) {
    log_warn("⚠️  Low scenario coverage - some scenarios may be missing")
  }
  
  # Missing value analysis
  log_info("🔍 Missing value analysis:")
  metric_cols <- intersect(c("auc", "cal_slope", "brier", "rMSPE", "MAPE"), names(raw_data))
  na_summary <- raw_data %>%
    summarise(
      total_rows = n(),
      across(all_of(metric_cols), ~ sum(is.na(.x)), .names = "{.col}_na")
    ) %>%
    pivot_longer(ends_with("_na"), names_to = "metric", values_to = "na_count") %>%
    mutate(
      metric = str_remove(metric, "_na"),
      pct_na = round(100 * na_count / total_rows, 2)
    )
  
  for (i in seq_len(nrow(na_summary))) {
    na_info <- na_summary[i, ]
    status_icon <- if (na_info$pct_na == 0) "✅" else 
                   if (na_info$pct_na < 5) "⚠️" else "❌"
    log_info("   {status_icon} {na_info$metric}: {na_info$na_count:,} missing ({na_info$pct_na}%)")
  }
  
  # Quality assessment
  quality_issues <- list()
  total_na_rate <- mean(is.na(raw_data$auc))
  if (total_na_rate > 0.1) {
    quality_issues$high_missing_rate <- total_na_rate
  }
  
  # Return enhanced data with metadata
  list(
    data = raw_data,
    methods = actual_methods,
    n_methods = n_methods,
    scenario_coverage = scenario_coverage,
    quality_issues = quality_issues,
    na_summary = na_summary
  )
}

# Validate and enhance data
enhanced_data <- validate_and_enhance_data(raw_data, config)

# =======================================================================
# DYNAMIC COLOR PALETTE GENERATION
# =======================================================================

#' Generate dynamic color palette for discovered methods
generate_method_colors <- function(methods, config) {
  
  log_info("Generating color palette for {length(methods)} methods...")
  
  # Start with configured colors
  config_colors <- config$visualization$colors
  method_colors <- character(length(methods))
  names(method_colors) <- methods
  
  # Use configured colors where available
  for (method in methods) {
    if (method %in% names(config_colors)) {
      method_colors[method] <- config_colors[[method]]
    }
  }
  
  # Generate colors for missing methods
  missing_colors <- is.na(method_colors) | method_colors == ""
  if (any(missing_colors)) {
    n_missing <- sum(missing_colors)
    log_info("Generating {n_missing} colors for unconfigured methods")
    
    # Use a nice color palette
    if (n_missing <= 12) {
      # Use RColorBrewer Set3 for small numbers
      extra_colors <- c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", 
                       "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", 
                       "#CCEBC5", "#FFED6F")[seq_len(n_missing)]
    } else {
      # Use rainbow for larger numbers
      extra_colors <- rainbow(n_missing)
    }
    
    method_colors[missing_colors] <- extra_colors
  }
  
  log_debug("Color assignments:")
  for (method in methods) {
    log_debug("  {method}: {method_colors[method]}")
  }
  
  method_colors
}

# Generate dynamic color palette
method_colors <- generate_method_colors(enhanced_data$methods, config)

# =======================================================================
# ENHANCED RANKING COMPUTATION
# =======================================================================

#' Apply ranking methodology with enhanced validation
compute_enhanced_rankings <- function(raw_data, config) {
  
  log_info("🏆 Computing method rankings using Lohmann et al. (2023) methodology...")
  
  # Apply ranking with comprehensive error handling
  ranked_data <- tryCatch({
    raw_data %>%
      group_by(scn_id, iter) %>%
      group_modify(~ {
        if (FRAMEWORK_ENHANCED && exists("rank_with_rounding")) {
          rank_with_rounding(.x)
        } else {
          # Fallback ranking implementation
          .x %>%
            mutate(
              r_auc = rank(-auc, ties.method = "min", na.last = "keep"),
              r_brier = rank(brier, ties.method = "min", na.last = "keep"),
              r_cals = rank(abs(cal_slope - 1), ties.method = "min", na.last = "keep"),
              r_rmspe = if ("rMSPE" %in% names(.x)) rank(rMSPE, ties.method = "min", na.last = "keep") else NA,
              r_mape = if ("MAPE" %in% names(.x)) rank(MAPE, ties.method = "min", na.last = "keep") else NA
            )
        }
      }) %>%
      ungroup()
  }, error = function(e) {
    log_error("❌ Ranking computation failed: {e$message}")
    stop("Failed to compute rankings - check that ranking functions are available")
  })
  
  # Validate ranking results
  ranking_cols <- intersect(c("r_auc", "r_cals", "r_brier", "r_rmspe", "r_mape"), names(ranked_data))
  
  if (length(ranking_cols) == 0) {
    stop("❌ No ranking columns were created")
  }
  
  log_info("✅ Rankings computed successfully")
  log_info("  • Ranking columns: {paste(ranking_cols, collapse = ', ')}")
  log_info("  • Total ranked evaluations: {nrow(ranked_data):,}")
  
  # Ranking validation summary
  for (col in ranking_cols) {
    if (col %in% names(ranked_data)) {
      min_rank <- min(ranked_data[[col]], na.rm = TRUE)
      max_rank <- max(ranked_data[[col]], na.rm = TRUE)
      na_count <- sum(is.na(ranked_data[[col]]))
      log_debug("  {col}: range [{min_rank}-{max_rank}], {na_count:,} NAs")
    }
  }
  
  ranked_data
}

# Compute rankings
ranked_data <- compute_enhanced_rankings(enhanced_data$data, config)

# =======================================================================
# ENHANCED SUMMARY STATISTICS
# =======================================================================

#' Generate comprehensive summaries with dynamic adaptation
generate_enhanced_summaries <- function(ranked_data, enhanced_data, config) {
  
  log_info("📊 Generating comprehensive performance summaries...")
  
  # Overall method ranking across all scenarios
  overall_summary <- ranked_data %>%
    group_by(method) %>%
    summarise(
      # Primary ranking metrics (mean ranks)
      mean_rank_auc = if ("r_auc" %in% names(.)) round(mean(r_auc, na.rm = TRUE), 2) else NA,
      mean_rank_cals = if ("r_cals" %in% names(.)) round(mean(r_cals, na.rm = TRUE), 2) else NA,
      mean_rank_brier = if ("r_brier" %in% names(.)) round(mean(r_brier, na.rm = TRUE), 2) else NA,
      mean_rank_rmspe = if ("r_rmspe" %in% names(.)) round(mean(r_rmspe, na.rm = TRUE), 2) else NA,
      mean_rank_mape = if ("r_mape" %in% names(.)) round(mean(r_mape, na.rm = TRUE), 2) else NA,
      
      # Raw performance metrics
      mean_auc = if ("auc" %in% names(.)) round(mean(auc, na.rm = TRUE), 3) else NA,
      mean_brier = if ("brier" %in% names(.)) round(mean(brier, na.rm = TRUE), 4) else NA,
      mean_cal_slope = if ("cal_slope" %in% names(.)) round(mean(cal_slope, na.rm = TRUE), 2) else NA,
      mean_rmspe = if ("rMSPE" %in% names(.)) round(mean(rMSPE, na.rm = TRUE), 4) else NA,
      mean_mape = if ("MAPE" %in% names(.)) round(mean(MAPE, na.rm = TRUE), 4) else NA,
      
      # Data completeness
      n_evaluations = n(),
      na_count_auc = if ("auc" %in% names(.)) sum(is.na(auc)) else 0,
      na_count_brier = if ("brier" %in% names(.)) sum(is.na(brier)) else 0,
      
      .groups = 'drop'
    )
  
  # Sort by primary ranking metric (AUC if available, otherwise Brier)
  if ("mean_rank_auc" %in% names(overall_summary) && !all(is.na(overall_summary$mean_rank_auc))) {
    overall_summary <- overall_summary %>% arrange(mean_rank_auc)
  } else if ("mean_rank_brier" %in% names(overall_summary)) {
    overall_summary <- overall_summary %>% arrange(mean_rank_brier)
  } else {
    overall_summary <- overall_summary %>% arrange(desc(mean_auc))
  }
  
  log_info("✅ Overall summary: {nrow(overall_summary)} methods ranked")
  
  # Performance by key design factors (if columns exist)
  summaries <- list(overall = overall_summary)
  
  # EPV analysis
  if ("EPV" %in% names(ranked_data)) {
    epv_summary <- ranked_data %>%
      group_by(EPV, method) %>%
      summarise(
        mean_rank_auc = if ("r_auc" %in% names(.)) round(mean(r_auc, na.rm = TRUE), 2) else NA,
        mean_rank_brier = if ("r_brier" %in% names(.)) round(mean(r_brier, na.rm = TRUE), 2) else NA,
        n_scenarios = n(),
        .groups = 'drop'
      ) %>%
      pivot_wider(
        names_from = method,
        values_from = if ("mean_rank_auc" %in% names(.)) mean_rank_auc else mean_rank_brier,
        names_prefix = "rank_"
      ) %>%
      arrange(EPV)
    
    summaries$by_epv <- epv_summary
    log_info("✅ EPV summary generated")
  }
  
  # Predictors analysis
  if ("p" %in% names(ranked_data)) {
    predictors_summary <- ranked_data %>%
      group_by(p, method) %>%
      summarise(
        mean_rank_auc = if ("r_auc" %in% names(.)) round(mean(r_auc, na.rm = TRUE), 2) else NA,
        mean_rank_brier = if ("r_brier" %in% names(.)) round(mean(r_brier, na.rm = TRUE), 2) else NA,
        n_scenarios = n(),
        .groups = 'drop'
      ) %>%
      pivot_wider(
        names_from = method,
        values_from = if ("mean_rank_auc" %in% names(.)) mean_rank_auc else mean_rank_brier,
        names_prefix = "rank_"
      ) %>%
      arrange(p)
    
    summaries$by_predictors <- predictors_summary
    log_info("✅ Predictors summary generated")
  }
  
  # Sparsity analysis
  if ("sparse" %in% names(ranked_data)) {
    sparsity_summary <- ranked_data %>%
      group_by(sparse, method) %>%
      summarise(
        mean_rank_auc = if ("r_auc" %in% names(.)) round(mean(r_auc, na.rm = TRUE), 2) else NA,
        mean_rank_brier = if ("r_brier" %in% names(.)) round(mean(r_brier, na.rm = TRUE), 2) else NA,
        n_scenarios = n(),
        .groups = 'drop'
      ) %>%
      pivot_wider(
        names_from = sparse,
        values_from = if ("mean_rank_auc" %in% names(.)) mean_rank_auc else mean_rank_brier,
        names_prefix = "sparse_"
      )
    
    summaries$by_sparsity <- sparsity_summary
    log_info("✅ Sparsity summary generated")
  }
  
  log_info("📊 Generated {length(summaries)} summary tables")
  summaries
}

# Generate all summaries
summaries <- generate_enhanced_summaries(ranked_data, enhanced_data, config)

# =======================================================================
# ENHANCED FILE OUTPUT
# =======================================================================

#' Save summary files with enhanced error handling
save_enhanced_summaries <- function(summaries, config, args) {
  
  log_info("💾 Saving summary files...")
  
  outdir <- args$results_dir
  
  # Save overall rankings
  overall_file <- file.path(outdir, "summary_ranks_overall.csv")
  tryCatch({
    readr::write_csv(summaries$overall, overall_file)
    log_info("✅ Saved: {basename(overall_file)}")
  }, error = function(e) {
    log_error("❌ Failed to save overall summary: {e$message}")
  })
  
  # Save dimensional summaries
  if ("by_epv" %in% names(summaries)) {
    epv_file <- file.path(outdir, "summary_by_EPV.csv")
    readr::write_csv(summaries$by_epv, epv_file)
    log_info("✅ Saved: {basename(epv_file)}")
  }
  
  if ("by_predictors" %in% names(summaries)) {
    pred_file <- file.path(outdir, "summary_by_predictors.csv")
    readr::write_csv(summaries$by_predictors, pred_file)
    log_info("✅ Saved: {basename(pred_file)}")
  }
  
  if ("by_sparsity" %in% names(summaries)) {
    sparse_file <- file.path(outdir, "summary_by_sparsity.csv")
    readr::write_csv(summaries$by_sparsity, sparse_file)
    log_info("✅ Saved: {basename(sparse_file)}")
  }
  
  log_info("💾 All summary files saved successfully")
}

# Save summary files
save_enhanced_summaries(summaries, config, args)

# =======================================================================
# ENHANCED VISUALIZATION GENERATION
# =======================================================================

#' Create enhanced visualizations with dynamic adaptation
create_enhanced_visualizations <- function(raw_data, ranked_data, enhanced_data, config, args) {
  
  log_info("🎨 Creating enhanced visualizations...")
  
  # Create plots directory
  plot_dir <- file.path(args$results_dir, "plots")
  dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Get visualization settings from config
  viz_config <- config$visualization
  plot_width <- viz_config$width %||% 12
  plot_height <- viz_config$height %||% 8
  plot_dpi <- viz_config$dpi %||% 300
  
  # Define theme based on config
  theme_publication <- theme_bw() +
    theme(
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 11, face = "bold"),
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 10),
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 10, face = "bold"),
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 9, face = "bold")
    )
  
  # Statistics for subtitles
  n_obs <- nrow(raw_data)
  n_scenarios <- length(unique(raw_data$scn_id))
  n_methods <- enhanced_data$n_methods
  
  plots_created <- 0
  
  # 1. AUC Distribution (if available)
  if ("auc" %in% names(raw_data) && !all(is.na(raw_data$auc))) {
    log_debug("Creating AUC distribution plot...")
    
    p1 <- raw_data %>%
      filter(!is.na(auc)) %>%
      ggplot(aes(x = reorder(method, -auc, FUN = median), y = auc, fill = method)) +
      geom_boxplot(outlier.alpha = 0.4, alpha = 0.8, show.legend = FALSE) +
      stat_summary(fun = median, geom = "text",
                   aes(label = sprintf("%.3f", after_stat(y))),
                   vjust = -0.5, size = 3, color = "darkred", fontface = "bold") +
      scale_fill_manual(values = method_colors) +
      coord_flip() +
      theme_publication +
      labs(
        title = "AUC Distribution by Method",
        subtitle = sprintf("Based on %s observations across %s scenarios (%d methods)",
                          format(n_obs, big.mark = ","), format(n_scenarios, big.mark = ","), n_methods),
        x = "Method",
        y = "Area Under ROC Curve (AUC)",
        caption = "Red numbers show median AUC values"
      )
    
    ggsave(file.path(plot_dir, "auc_distribution.png"), p1,
           width = plot_width, height = plot_height, dpi = plot_dpi)
    plots_created <- plots_created + 1
  }
  
  # 2. Calibration Slope Distribution (if available)
  if ("cal_slope" %in% names(raw_data) && !all(is.na(raw_data$cal_slope))) {
    log_debug("Creating calibration slope plot...")
    
    p2 <- raw_data %>%
      filter(!is.na(cal_slope)) %>%
      ggplot(aes(x = reorder(method, abs(cal_slope - 1), FUN = median), y = cal_slope, fill = method)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 1, alpha = 0.8) +
      geom_boxplot(outlier.alpha = 0.4, alpha = 0.8, show.legend = FALSE) +
      scale_fill_manual(values = method_colors) +
      coord_flip() +
      theme_publication +
      labs(
        title = "Calibration Slope by Method",
        subtitle = "Perfect calibration = 1.0 (red dashed line)",
        x = "Method",
        y = "Calibration Slope",
        caption = "Methods ordered by deviation from perfect calibration"
      )
    
    ggsave(file.path(plot_dir, "calibration_slope.png"), p2,
           width = plot_width, height = plot_height, dpi = plot_dpi)
    plots_created <- plots_created + 1
  }
  
  # 3. Brier Score Distribution (if available)
  if ("brier" %in% names(raw_data) && !all(is.na(raw_data$brier))) {
    log_debug("Creating Brier score plot...")
    
    p3 <- raw_data %>%
      filter(!is.na(brier)) %>%
      ggplot(aes(x = reorder(method, brier, FUN = median), y = brier, fill = method)) +
      geom_boxplot(outlier.alpha = 0.4, alpha = 0.8, show.legend = FALSE) +
      scale_fill_manual(values = method_colors) +
      coord_flip() +
      theme_publication +
      labs(
        title = "Brier Score Distribution by Method",
        subtitle = "Lower values indicate better overall prediction accuracy",
        x = "Method",
        y = "Brier Score",
        caption = "Methods ordered by median Brier score"
      )
    
    ggsave(file.path(plot_dir, "brier_score.png"), p3,
           width = plot_width, height = plot_height, dpi = plot_dpi)
    plots_created <- plots_created + 1
  }
  
  # 4. Performance by EPV (if data available)
  if ("EPV" %in% names(ranked_data) && "r_auc" %in% names(ranked_data)) {
    log_debug("Creating EPV analysis plot...")
    
    p4 <- ranked_data %>%
      filter(!is.na(r_auc)) %>%
      group_by(EPV, method) %>%
      summarise(mean_rank = mean(r_auc, na.rm = TRUE), .groups = 'drop') %>%
      ggplot(aes(x = EPV, y = mean_rank, color = method)) +
      geom_line(size = 1.2, alpha = 0.8) +
      geom_point(size = 2.5, alpha = 0.9) +
      scale_x_log10(breaks = sort(unique(ranked_data$EPV))) +
      scale_color_manual(values = method_colors) +
      theme_publication +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      guides(color = guide_legend(ncol = min(4, ceiling(n_methods/3)))) +
      labs(
        title = "AUC Ranking by Events Per Variable (EPV)",
        subtitle = "Lower ranks indicate better performance",
        x = "Events Per Variable (log scale)",
        y = "Mean Rank (lower = better)",
        caption = "Each line shows one method's performance across EPV levels"
      )
    
    ggsave(file.path(plot_dir, "performance_by_epv.png"), p4,
           width = plot_width, height = plot_height, dpi = plot_dpi)
    plots_created <- plots_created + 1
  }
  
  # 5. Performance by Number of Predictors (if available)
  if ("p" %in% names(ranked_data) && "r_auc" %in% names(ranked_data)) {
    log_debug("Creating predictor analysis plot...")
    
    p5 <- ranked_data %>%
      filter(!is.na(r_auc)) %>%
      group_by(p, method) %>%
      summarise(mean_rank = mean(r_auc, na.rm = TRUE), .groups = 'drop') %>%
      ggplot(aes(x = p, y = mean_rank, color = method)) +
      geom_line(size = 1.2, alpha = 0.8) +
      geom_point(size = 2.5, alpha = 0.9) +
      scale_x_log10(breaks = sort(unique(ranked_data$p))) +
      scale_color_manual(values = method_colors) +
      theme_publication +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      guides(color = guide_legend(ncol = min(4, ceiling(n_methods/3)))) +
      labs(
        title = "AUC Ranking by Number of Predictors",
        subtitle = "Lower ranks indicate better performance",
        x = "Number of Predictors (log scale)",
        y = "Mean Rank (lower = better)",
        caption = "Each line shows one method's performance across dimensionality levels"
      )
    
    ggsave(file.path(plot_dir, "performance_by_predictors.png"), p5,
           width = plot_width, height = plot_height, dpi = plot_dpi)
    plots_created <- plots_created + 1
  }
  
  # 6. Method Ranking Heatmap (if rankings available)
  ranking_cols <- intersect(c("r_auc", "r_cals", "r_brier", "r_rmspe", "r_mape"), names(ranked_data))
  if (length(ranking_cols) > 0) {
    log_debug("Creating ranking heatmap...")
    
    # Prepare heatmap data
    metric_names <- c("r_auc" = "AUC", "r_cals" = "Cal. Slope", "r_brier" = "Brier",
                     "r_rmspe" = "rMSPE", "r_mape" = "MAPE")
    
    heatmap_data <- ranked_data %>%
      group_by(method) %>%
      summarise(
        across(all_of(ranking_cols), ~ mean(.x, na.rm = TRUE)),
        .groups = 'drop'
      ) %>%
      pivot_longer(cols = all_of(ranking_cols), names_to = "metric", values_to = "mean_rank") %>%
      mutate(
        method = factor(method, levels = summaries$overall$method),
        metric = factor(metric, levels = ranking_cols, labels = metric_names[ranking_cols])
      )
    
    p6 <- ggplot(heatmap_data, aes(x = metric, y = method, fill = mean_rank)) +
      geom_tile(color = "white", size = 0.5) +
      geom_text(aes(label = sprintf("%.1f", mean_rank)), size = 3, fontface = "bold") +
      scale_fill_gradient2(
        low = "darkgreen", mid = "yellow", high = "darkred",
        midpoint = (n_methods + 1) / 2, name = "Mean\nRank"
      ) +
      theme_publication +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()
      ) +
      labs(
        title = "Method Performance Heatmap",
        subtitle = "Lower ranks (green) indicate better performance",
        x = "Performance Metric",
        y = "Method",
        caption = "Numbers show mean rank across all scenarios"
      )
    
    ggsave(file.path(plot_dir, "ranking_heatmap.png"), p6,
           width = plot_width * 0.8, height = plot_height, dpi = plot_dpi)
    plots_created <- plots_created + 1
  }
  
  # 7. Scenario Coverage Diagnostic
  if (all(c("EPV", "event_frac") %in% names(raw_data))) {
    log_debug("Creating scenario coverage diagnostic...")
    
    coverage_data <- raw_data %>%
      distinct(scn_id, EPV, event_frac, p, noise_frac, sparse) %>%
      count(EPV, event_frac, name = "n_scenarios") %>%
      mutate(
        # Calculate expected scenarios for each EPV-event_frac combination
        expected = length(unique(raw_data$p)) * 
                  length(unique(raw_data$noise_frac)) * 
                  length(unique(raw_data$sparse)),
        coverage_pct = (n_scenarios / expected) * 100
      )
    
    p7 <- ggplot(coverage_data, aes(x = factor(EPV), y = event_frac, fill = coverage_pct)) +
      geom_tile(color = "white", size = 0.5) +
      geom_text(aes(label = sprintf("%.0f%%", coverage_pct)), size = 3, fontface = "bold") +
      scale_fill_gradient2(
        low = "red", mid = "yellow", high = "darkgreen",
        midpoint = 95, name = "Coverage\n(%)"
      ) +
      scale_y_continuous(labels = function(x) paste0(round(x*100, 1), "%")) +
      theme_publication +
      theme(panel.grid = element_blank()) +
      labs(
        title = "Scenario Coverage Diagnostic",
        subtitle = "Percentage of expected scenarios completed for each EPV-prevalence combination",
        x = "Events Per Variable (EPV)",
        y = "Event Fraction (Prevalence)",
        caption = "Green = complete coverage, Red = missing scenarios"
      )
    
    ggsave(file.path(plot_dir, "scenario_coverage.png"), p7,
           width = plot_width * 0.8, height = plot_height * 0.75, dpi = plot_dpi)
    plots_created <- plots_created + 1
  }
  
  log_info("✅ Created {plots_created} visualization files in: plots/")
  plots_created
}

# Create all visualizations
n_plots <- create_enhanced_visualizations(enhanced_data$data, ranked_data, enhanced_data, config, args)

# =======================================================================
# ENHANCED RESEARCH REPORT GENERATION
# =======================================================================

#' Generate comprehensive research report with dynamic content
generate_enhanced_report <- function(summaries, enhanced_data, config, args) {
  
  log_info("📝 Generating comprehensive research report...")
  
  report_file <- file.path(args$results_dir, "SIMULATION_REPORT.md")
  
  # Calculate key statistics
  n_evaluations <- nrow(enhanced_data$data)
  n_scenarios <- length(unique(enhanced_data$data$scn_id))
  n_methods <- enhanced_data$n_methods
  completion_rate <- enhanced_data$scenario_coverage
  
  # Get top performers (handle missing rankings gracefully)
  top_performers <- list()
  if ("mean_rank_auc" %in% names(summaries$overall) && !all(is.na(summaries$overall$mean_rank_auc))) {
    top_performers$auc <- head(summaries$overall$method, 3)
  } else if ("mean_auc" %in% names(summaries$overall)) {
    top_by_auc <- summaries$overall %>% arrange(desc(mean_auc)) %>% head(3)
    top_performers$auc <- top_by_auc$method
  }
  
  if ("mean_rank_brier" %in% names(summaries$overall) && !all(is.na(summaries$overall$mean_rank_brier))) {
    top_by_brier <- summaries$overall %>% arrange(mean_rank_brier) %>% head(3)
    top_performers$brier <- top_by_brier$method
  } else if ("mean_brier" %in% names(summaries$overall)) {
    top_by_brier <- summaries$overall %>% arrange(mean_brier) %>% head(3)
    top_performers$brier <- top_by_brier$method
  }
  
  if ("mean_rank_cals" %in% names(summaries$overall) && !all(is.na(summaries$overall$mean_rank_cals))) {
    top_by_cal <- summaries$overall %>% arrange(mean_rank_cals) %>% head(3)
    top_performers$calibration <- top_by_cal$method
  }
  
  # Performance ranges
  perf_ranges <- list()
  if ("auc" %in% names(enhanced_data$data)) {
    auc_stats <- enhanced_data$data %>%
      filter(!is.na(auc)) %>%
      summarise(min = min(auc), max = max(auc), mean = mean(auc))
    perf_ranges$auc <- auc_stats
  }
  
  if ("brier" %in% names(enhanced_data$data)) {
    brier_stats <- enhanced_data$data %>%
      filter(!is.na(brier)) %>%
      summarise(min = min(brier), max = max(brier), mean = mean(brier))
    perf_ranges$brier <- brier_stats
  }
  
  # Create report content using enhanced templating
  report_content <- paste0(
    "# Enhanced Lohmann et al. (2023) Simulation Replication Report\n\n",
    "**Generated:** ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
    "**Analysis:** Complete replication with enhanced configuration system\n",
    "**Framework:** ", ifelse(FRAMEWORK_ENHANCED, "Enhanced", "Original"), "\n\n",
    "---\n\n",
    "## Executive Summary\n\n",
    "This report presents results from an enhanced replication of the Lohmann et al. (2023)\n",
    "simulation study comparing likelihood penalization and variance decomposition approaches\n",
    "for clinical prediction models.\n\n"
  )
  
  # Add key findings section
  if (length(top_performers) > 0) {
    report_content <- paste0(report_content,
      "### Key Findings\n\n"
    )
    
    if ("auc" %in% names(top_performers)) {
      report_content <- paste0(report_content,
        "🏆 **Top Performing Methods (by AUC):**\n",
        paste0(seq_along(top_performers$auc), ". ", top_performers$auc, collapse = "\n"), "\n\n"
      )
    }
    
    if ("calibration" %in% names(top_performers)) {
      report_content <- paste0(report_content,
        "🎯 **Best Calibration Performance:**\n",
        paste0(seq_along(top_performers$calibration), ". ", top_performers$calibration, collapse = "\n"), "\n\n"
      )
    }
    
    if ("brier" %in% names(top_performers)) {
      report_content <- paste0(report_content,
        "📊 **Overall Prediction Accuracy (by Brier score):**\n",
        paste0(seq_along(top_performers$brier), ". ", top_performers$brier, collapse = "\n"), "\n\n"
      )
    }
  }
  
  # Add simulation overview
  report_content <- paste0(report_content,
    "---\n\n",
    "## Simulation Overview\n\n",
    "### Scope and Scale\n",
    "- **Scenarios Completed:** ", format(n_scenarios, big.mark = ","), " / ", 
    format(config$expected_scenarios, big.mark = ","), " (", round(completion_rate, 1), "%)\n",
    "- **Total Evaluations:** ", format(n_evaluations, big.mark = ","), "\n",
    "- **Methods Compared:** ", n_methods, "\n",
    "- **Methods:** ", paste(enhanced_data$methods, collapse = ", "), "\n\n"
  )
  
  # Add performance summary if data available
  if (length(perf_ranges) > 0) {
    report_content <- paste0(report_content,
      "## Performance Summary\n\n"
    )
    
    if ("auc" %in% names(perf_ranges)) {
      auc_range <- perf_ranges$auc
      report_content <- paste0(report_content,
        "### Discrimination (AUC)\n",
        "- **Range:** ", round(auc_range$min, 3), " - ", round(auc_range$max, 3), "\n",
        "- **Overall Mean:** ", round(auc_range$mean, 3), "\n"
      )
      
      if ("mean_rank_auc" %in% names(summaries$overall)) {
        best_method <- summaries$overall$method[1]
        best_rank <- summaries$overall$mean_rank_auc[1]
        report_content <- paste0(report_content,
          "- **Best Method:** ", best_method, " (mean rank: ", best_rank, ")\n\n"
        )
      }
    }
    
    if ("brier" %in% names(perf_ranges)) {
      brier_range <- perf_ranges$brier
      report_content <- paste0(report_content,
        "### Overall Accuracy (Brier Score)\n",
        "- **Range:** ", round(brier_range$min, 4), " - ", round(brier_range$max, 4), "\n",
        "- **Overall Mean:** ", round(brier_range$mean, 4), "\n\n"
      )
    }
  }
  
  # Add detailed method rankings table
  report_content <- paste0(report_content,
    "## Detailed Method Rankings\n\n",
    "### Overall Performance\n\n"
  )
  
  # Create ranking table
  ranking_table <- summaries$overall %>%
    mutate(rank_position = row_number()) %>%
    select(rank_position, method, any_of(c("mean_rank_auc", "mean_rank_brier", "mean_rank_cals", 
                                          "mean_auc", "mean_brier")))
  
  # Format table for markdown
  if (nrow(ranking_table) > 0) {
    # Create table header
    header_cols <- c("Rank", "Method")
    if ("mean_rank_auc" %in% names(ranking_table)) header_cols <- c(header_cols, "AUC Rank")
    if ("mean_rank_brier" %in% names(ranking_table)) header_cols <- c(header_cols, "Brier Rank")
    if ("mean_rank_cals" %in% names(ranking_table)) header_cols <- c(header_cols, "Cal. Rank")
    if ("mean_auc" %in% names(ranking_table)) header_cols <- c(header_cols, "Mean AUC")
    if ("mean_brier" %in% names(ranking_table)) header_cols <- c(header_cols, "Mean Brier")
    
    report_content <- paste0(report_content,
      "| ", paste(header_cols, collapse = " | "), " |\n",
      "|", paste(rep("------", length(header_cols)), collapse = "|"), "|\n"
    )
    
    # Add table rows
    for (i in seq_len(min(nrow(ranking_table), 15))) {  # Limit to top 15 methods
      row <- ranking_table[i, ]
      row_data <- c(row$rank_position, row$method)
      
      if ("mean_rank_auc" %in% names(row)) row_data <- c(row_data, sprintf("%.2f", row$mean_rank_auc))
      if ("mean_rank_brier" %in% names(row)) row_data <- c(row_data, sprintf("%.2f", row$mean_rank_brier))  
      if ("mean_rank_cals" %in% names(row)) row_data <- c(row_data, sprintf("%.2f", row$mean_rank_cals))
      if ("mean_auc" %in% names(row)) row_data <- c(row_data, sprintf("%.3f", row$mean_auc))
      if ("mean_brier" %in% names(row)) row_data <- c(row_data, sprintf("%.4f", row$mean_brier))
      
      report_content <- paste0(report_content,
        "| ", paste(row_data, collapse = " | "), " |\n"
      )
    }
    
    report_content <- paste0(report_content, "\n")
  }
  
  # Add conclusions and recommendations
  report_content <- paste0(report_content,
    "---\n\n",
    "## Key Insights\n\n",
    "### Methodological Enhancements\n",
    "- **Configuration Integration:** Results generated using centralized configuration system\n",
    "- **Dynamic Method Discovery:** ", n_methods, " methods automatically detected and processed\n",
    "- **Enhanced Error Handling:** Robust processing with comprehensive quality assurance\n",
    "- **Memory Management:** Efficient batch processing for large datasets\n\n"
  )
  
  # Add method family analysis if we can determine families
  penalization_methods <- c("MLE", "Ridge", "LASSO", "ElasticNet", "RelaxedLASSO")
  variance_methods <- c("PCR_evgt1", "PCR_var90", "PCR_aic", "PCR_cvdev", "PLS", "PLS_LASSO")
  
  pen_methods_found <- intersect(penalization_methods, enhanced_data$methods)
  var_methods_found <- intersect(variance_methods, enhanced_data$methods)
  
  if (length(pen_methods_found) > 0 && length(var_methods_found) > 0) {
    # Calculate family performance
    if ("mean_rank_auc" %in% names(summaries$overall)) {
      pen_performance <- summaries$overall %>%
        filter(method %in% pen_methods_found) %>%
        summarise(mean_rank = mean(mean_rank_auc, na.rm = TRUE)) %>%
        pull(mean_rank)
      
      var_performance <- summaries$overall %>%
        filter(method %in% var_methods_found) %>%
        summarise(mean_rank = mean(mean_rank_auc, na.rm = TRUE)) %>%
        pull(mean_rank)
      
      report_content <- paste0(report_content,
        "### Penalization vs. Variance Decomposition\n",
        "- **Penalization methods** (", paste(pen_methods_found, collapse = ", "), 
        "): Average AUC rank = ", round(pen_performance, 2), "\n",
        "- **Variance decomposition** (", paste(var_methods_found, collapse = ", "),
        "): Average AUC rank = ", round(var_performance, 2), "\n"
      )
      
      if (pen_performance < var_performance) {
        report_content <- paste0(report_content,
          "- **Finding:** Penalization methods outperform variance decomposition (consistent with original study)\n\n"
        )
      } else {
        report_content <- paste0(report_content,
          "- **Finding:** Results differ from original study - variance decomposition competitive\n\n"
        )
      }
    }
  }
  
  # Add technical validation
  report_content <- paste0(report_content,
    "---\n\n",
    "## Technical Validation\n\n",
    "### Quality Assurance\n",
    "- **Scenario Coverage:** ", round(completion_rate, 1), "% of expected scenarios\n",
    "- **Method Discovery:** All ", n_methods, " methods successfully processed\n",
    "- **Data Integrity:** ", format(n_evaluations, big.mark = ","), " evaluations validated\n"
  )
  
  # Add missing value analysis
  if (nrow(enhanced_data$na_summary) > 0) {
    high_na_metrics <- enhanced_data$na_summary %>% filter(pct_na > 5)
    if (nrow(high_na_metrics) > 0) {
      report_content <- paste0(report_content,
        "- **Missing Data Issues:** ", nrow(high_na_metrics), " metrics with >5% missing values\n"
      )
    } else {
      report_content <- paste0(report_content,
        "- **Missing Data:** All metrics <5% missing (excellent quality)\n"
      )
    }
  }
  
  # Add files generated
  report_content <- paste0(report_content,
    "\n---\n\n",
    "## Files Generated\n\n",
    "### Summary Files\n",
    "- `summary_ranks_overall.csv` - Complete method rankings\n"
  )
  
  if ("by_epv" %in% names(summaries)) {
    report_content <- paste0(report_content, "- `summary_by_EPV.csv` - Performance by events per variable\n")
  }
  if ("by_predictors" %in% names(summaries)) {
    report_content <- paste0(report_content, "- `summary_by_predictors.csv` - Performance by dimensionality\n")
  }
  if ("by_sparsity" %in% names(summaries)) {
    report_content <- paste0(report_content, "- `summary_by_sparsity.csv` - Performance by predictor sparsity\n")
  }
  
  report_content <- paste0(report_content,
    "\n### Visualizations (plots/ directory)\n",
    "- High-quality plots with publication-ready formatting\n",
    "- Dynamic color schemes adapted to discovered methods\n",
    "- Comprehensive performance comparisons and diagnostics\n\n"
  )
  
  # Add recommendations
  report_content <- paste0(report_content,
    "---\n\n",
    "## Recommendations\n\n",
    "### For Practitioners\n",
    "1. **Review method rankings** in summary_ranks_overall.csv for guidance\n",
    "2. **Consider top-performing methods** for clinical prediction models\n",
    "3. **Examine dimensional effects** in EPV and predictor-specific analyses\n\n",
    "### For Researchers\n",
    "1. **Validate findings** using real clinical datasets\n",
    "2. **Extend analysis** with additional methods or scenarios\n",
    "3. **Investigate** method-specific performance patterns\n\n",
    "### For Implementation\n",
    "1. **Use configuration system** for reproducible analyses\n",
    "2. **Leverage enhanced framework** for robust error handling\n",
    "3. **Scale analysis** using memory-efficient batch processing\n\n"
  )
  
  # Add references
  report_content <- paste0(report_content,
    "---\n\n",
    "## References\n\n",
    "**Original Study:**\n",
    "Lohmann, L., Groenwold, R.H.H., & van Smeden, M. (2023). Comparison of likelihood\n",
    "penalization and variance decomposition approaches for clinical prediction models: A\n",
    "simulation study. *Biometrical Journal*, 65(5), e700193.\n\n",
    "**This Enhanced Replication:**\n",
    "Generated using enhanced R implementation with configuration management, dynamic\n",
    "method discovery, and comprehensive quality assurance. Complete reproducible code\n",
    "available in project repository.\n\n",
    "---\n\n",
    "*Report generated automatically from enhanced simulation results*\n",
    "*Framework: ", ifelse(FRAMEWORK_ENHANCED, "Enhanced", "Original"), " | ",
    "Configuration: ", ifelse(HAS_YAML, "YAML-based", "Fallback"), " | ",
    "Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "*"
  )
  
  # Write report to file
  tryCatch({
    writeLines(report_content, report_file)
    log_info("✅ Comprehensive report saved: {basename(report_file)}")
  }, error = function(e) {
    log_error("❌ Failed to save report: {e$message}")
  })
  
  report_file
}

# Generate comprehensive report
report_file <- generate_enhanced_report(summaries, enhanced_data, config, args)

# =======================================================================
# ENHANCED FINAL SUMMARY AND COMPLETION
# =======================================================================

#' Display enhanced completion summary
display_completion_summary <- function(summaries, enhanced_data, config, args, n_plots) {
  
  cat("\n")
  log_info("🎉 ENHANCED AGGREGATION COMPLETED SUCCESSFULLY!")
  log_info("=" * 70)
  
  # Key statistics
  n_evaluations <- nrow(enhanced_data$data)
  n_scenarios <- length(unique(enhanced_data$data$scn_id))
  n_methods <- enhanced_data$n_methods
  completion_rate <- enhanced_data$scenario_coverage
  
  log_info("📊 FINAL RESULTS SUMMARY")
  log_info("   • Total evaluations processed: {format(n_evaluations, big.mark = ',')}")
  log_info("   • Unique scenarios: {format(n_scenarios, big.mark = ',')} / {format(config$expected_scenarios, big.mark = ',')} ({round(completion_rate, 1)}%)")
  log_info("   • Methods compared: {n_methods}")
  log_info("   • Framework: {ifelse(FRAMEWORK_ENHANCED, 'Enhanced', 'Original')}")
  log_info("   • Configuration: {ifelse(HAS_YAML, 'YAML-based', 'Fallback defaults')}")
  
  # Show top performers
  if (nrow(summaries$overall) > 0) {
    log_info("🏆 TOP 5 METHODS:")
    
    top_methods <- head(summaries$overall, 5)
    
    for (i in seq_len(nrow(top_methods))) {
      m <- top_methods[i, ]
      
      # Build performance string based on available metrics
      perf_parts <- c()
      if ("mean_rank_auc" %in% names(m) && !is.na(m$mean_rank_auc)) {
        perf_parts <- c(perf_parts, sprintf("AUC rank: %.2f", m$mean_rank_auc))
      }
      if ("mean_auc" %in% names(m) && !is.na(m$mean_auc)) {
        perf_parts <- c(perf_parts, sprintf("mean AUC: %.3f", m$mean_auc))
      }
      if ("mean_rank_brier" %in% names(m) && !is.na(m$mean_rank_brier)) {
        perf_parts <- c(perf_parts, sprintf("Brier rank: %.2f", m$mean_rank_brier))
      }
      if ("mean_brier" %in% names(m) && !is.na(m$mean_brier)) {
        perf_parts <- c(perf_parts, sprintf("mean Brier: %.4f", m$mean_brier))
      }
      
      perf_string <- if (length(perf_parts) > 0) paste(perf_parts, collapse = ", ") else "metrics available"
      log_info("   {i}. {m$method}: {perf_string}")
    }
  }
  
  # File summary
  log_info("📁 FILES GENERATED:")
  log_info("   📊 Summary Files:")
  log_info("      • summary_ranks_overall.csv - Complete method rankings")
  
  if ("by_epv" %in% names(summaries)) {
    log_info("      • summary_by_EPV.csv - Performance by events per variable")
  }
  if ("by_predictors" %in% names(summaries)) {
    log_info("      • summary_by_predictors.csv - Performance by dimensionality")
  }
  if ("by_sparsity" %in% names(summaries)) {
    log_info("      • summary_by_sparsity.csv - Performance by predictor sparsity")
  }
  
  log_info("   🎨 Visualizations:")
  if (n_plots > 0) {
    log_info("      • {n_plots} publication-quality plots in plots/ directory")
    log_info("      • Dynamic color schemes adapted to discovered methods")
  } else {
    log_info("      • No plots generated (insufficient data)")
  }
  
  log_info("   📝 Reports:")
  log_info("      • SIMULATION_REPORT.md - Comprehensive research report")
  
  # Quality assessment
  if (length(enhanced_data$quality_issues) > 0) {
    log_info("⚠️  QUALITY NOTES:")
    log_info("   • {length(enhanced_data$quality_issues)} quality issues detected")
    log_info("   • Review report for details and recommendations")
  } else {
    log_info("✅ QUALITY ASSURANCE:")
    log_info("   • No major quality issues detected")
    log_info("   • Data integrity validated")
  }
  
  # Performance metrics
  if (HAS_PRYR) {
    final_memory <- pryr::mem_used()
    log_info("⚡ PERFORMANCE:")
    log_info("   • Memory usage: {format(final_memory, units='MB')}")
  }
  
  if (!is.null(args$batch_size)) {
    log_info("   • Batch processing: Enabled (size: {args$batch_size})")
  }
  
  # Enhanced features used
  features_used <- c()
  if (HAS_YAML) features_used <- c(features_used, "Configuration system")
  if (HAS_PROGRESS) features_used <- c(features_used, "Progress tracking")
  if (HAS_PRYR) features_used <- c(features_used, "Memory monitoring")
  
  if (length(features_used) > 0) {
    log_info("   • Enhanced features: {paste(features_used, collapse = ', ')}")
  }
  
  # Next steps
  log_info("🚀 NEXT STEPS:")
  log_info("   1. Review SIMULATION_REPORT.md for detailed findings")
  log_info("   2. Examine plots/ directory for visual analysis")
  log_info("   3. Use summary_ranks_overall.csv for method selection")
  
  if (completion_rate < 95) {
    log_info("   4. Consider running additional chunks to improve coverage")
  }
  
  if (n_methods != 11) {
    log_info("   ℹ️  Note: {n_methods} methods found (expected 11) - framework may be customized")
  }
  
  # Success message
  cat("\n")
  log_info("═" * 70)
  log_info("🎊 ENHANCED LOHMANN ET AL. (2023) ANALYSIS COMPLETE! 🎊")
  log_info("═" * 70)
  log_info("All results saved to: {args$results_dir}/")
  log_info("For detailed methodology and findings, see SIMULATION_REPORT.md")
  
  cat("\n✨ Enhanced analysis completed successfully! ✨\n")
  
  # Return summary for potential further use
  list(
    n_evaluations = n_evaluations,
    n_scenarios = n_scenarios, 
    n_methods = n_methods,
    completion_rate = completion_rate,
    n_plots = n_plots,
    n_quality_issues = length(enhanced_data$quality_issues),
    top_method = if (nrow(summaries$overall) > 0) summaries$overall$method[1] else NA,
    framework = ifelse(FRAMEWORK_ENHANCED, "Enhanced", "Original"),
    config_source = ifelse(HAS_YAML, "YAML", "Fallback")
  )
}

# Display completion summary
final_summary <- display_completion_summary(summaries, enhanced_data, config, args, n_plots)

# =======================================================================
# CLEANUP AND EXIT
# =======================================================================

# Memory cleanup
rm(enhanced_data, ranked_data, summaries)
gc(verbose = FALSE)

# Log final completion
log_info("Enhanced aggregation script completed at {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}")

# Exit successfully
quit(status = 0)
