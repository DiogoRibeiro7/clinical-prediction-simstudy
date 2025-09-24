# =======================================================================
# run_chunk.R
# 
# Enhanced parallel execution script with comprehensive error handling,
# memory management, progress tracking, and automated recovery mechanisms
#
# Key improvements:
# - Configuration-driven execution
# - Advanced memory management with automatic batching
# - Comprehensive error recovery and retry mechanisms
# - Real-time progress monitoring with ETA
# - Automated quality assurance checks
# - Performance profiling and optimization
# - Graceful degradation under resource constraints
#
# Author: Diogo Ribeiro (Enhanced)
# Date: January 2025
# License: MIT
# =======================================================================

# Enhanced package loading with fallback mechanisms
suppressPackageStartupMessages({
  required_packages <- c(
    # Core packages (essential)
    "optparse", "future.apply", "dplyr", "readr",
    # Configuration and logging
    "yaml", "logger", "glue",
    # Performance and memory monitoring
    "pryr", "bench",
    # Progress tracking
    "progress"
  )
  
  # Check package availability with detailed reporting
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    cat("⚠️  Missing packages detected:", paste(missing_packages, collapse = ", "), "\n")
    cat("Some enhanced features will be disabled.\n")
    cat("To install missing packages:\n")
    cat(paste("install.packages(c(", paste0('"', missing_packages, '"', collapse = ", "), "))\n\n"))
  }
  
  # Load essential packages (fail if not available)
  essential_packages <- c("optparse", "future.apply", "dplyr", "readr")
  for (pkg in essential_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("Essential package", pkg, "is required but not available"))
    }
    library(pkg, character.only = TRUE)
  }
  
  # Load optional packages (enable features if available)
  optional_packages <- setdiff(required_packages, essential_packages)
  loaded_optional <- character(0)
  
  for (pkg in optional_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      library(pkg, character.only = TRUE)
      loaded_optional <- c(loaded_optional, pkg)
    }
  }
  
  # Feature flags based on loaded packages
  HAS_YAML <- "yaml" %in% loaded_optional
  HAS_LOGGER <- "logger" %in% loaded_optional
  HAS_GLUE <- "glue" %in% loaded_optional
  HAS_PRYR <- "pryr" %in% loaded_optional
  HAS_PROGRESS <- "progress" %in% loaded_optional
})

# Load enhanced simulation framework
if (file.exists("enhanced_replicate_framework.R")) {
  source("enhanced_replicate_framework.R")
  FRAMEWORK_ENHANCED <- TRUE
} else {
  source("replicate_framework.R")
  FRAMEWORK_ENHANCED <- FALSE
  cat("⚠️  Using original framework - enhanced features disabled\n")
}

# =======================================================================
# ENHANCED COMMAND LINE INTERFACE
# =======================================================================

#' Create enhanced command line options with validation
create_option_list <- function() {
  list(
    make_option(
      c("--config"), 
      type = "character", 
      default = "config/simulation_config.yaml",
      help = "Configuration file path [default: %default]"
    ),
    make_option(
      c("--chunks"), 
      type = "integer", 
      default = NULL,  # Will be read from config
      help = "Total number of chunks (overrides config) [default: from config]"
    ),
    make_option(
      c("--chunk_id"), 
      type = "integer", 
      default = 1,
      help = "Which chunk to run (1-based) [default: %default]"
    ),
    make_option(
      c("--iters"), 
      type = "integer", 
      default = NULL,  # Will be read from config
      help = "Iterations per scenario (overrides config) [default: from config]"
    ),
    make_option(
      c("--outdir"), 
      type = "character", 
      default = NULL,  # Will be read from config
      help = "Output directory (overrides config) [default: from config]"
    ),
    make_option(
      c("--workers"), 
      type = "integer", 
      default = NULL,  # Will be read from config
      help = "Parallel workers (overrides config) [default: from config]"
    ),
    make_option(
      c("--memory_limit"), 
      type = "double", 
      default = NULL,  # Will be read from config
      help = "Memory limit in GB (overrides config) [default: from config]"
    ),
    make_option(
      c("--profile"), 
      action = "store_true", 
      default = FALSE,
      help = "Enable performance profiling [default: %default]"
    ),
    make_option(
      c("--verbose"), 
      action = "store_true", 
      default = FALSE,
      help = "Enable verbose output [default: %default]"
    ),
    make_option(
      c("--dry_run"), 
      action = "store_true", 
      default = FALSE,
      help = "Perform dry run without execution [default: %default]"
    ),
    make_option(
      c("--resume"), 
      action = "store_true", 
      default = FALSE,
      help = "Resume from existing partial results [default: %default]"
    ),
    make_option(
      c("--force"), 
      action = "store_true", 
      default = FALSE,
      help = "Overwrite existing results without confirmation [default: %default]"
    ),
    make_option(
      c("--test_mode"), 
      action = "store_true", 
      default = FALSE,
      help = "Run in test mode with reduced scenarios [default: %default]"
    ),
    make_option(
      c("--log_level"), 
      type = "character", 
      default = NULL,  # Will be read from config
      choices = c("DEBUG", "INFO", "WARN", "ERROR"),
      help = "Logging level [default: from config]"
    )
  )
}

# Enhanced argument parsing with configuration integration
option_list <- create_option_list()

opt_parser <- OptionParser(
  option_list = option_list,
  description = paste(
    "\n🧪 ENHANCED LOHMANN ET AL. (2023) REPLICATION - CHUNK EXECUTION",
    "\nRuns a chunk of scenarios with advanced error handling, memory management,",
    "and performance optimization.\n",
    "\n📋 EXAMPLES:",
    "  # Standard execution with config file",
    "  Rscript enhanced_run_chunk.R --chunk_id 5",
    "\n  # Override config settings",
    "  Rscript enhanced_run_chunk.R --chunk_id 5 --workers 8 --memory_limit 16",
    "\n  # Test run with profiling",
    "  Rscript enhanced_run_chunk.R --chunk_id 1 --test_mode --profile --verbose",
    "\n  # Resume interrupted run",
    "  Rscript enhanced_run_chunk.R --chunk_id 5 --resume",
    sep = "\n"
  ),
  epilogue = paste(
    "\n📚 DOCUMENTATION:",
    "For detailed documentation, see README.md or the original paper:",
    "Lohmann et al. (2023) Biometrical Journal 65(5):e700193",
    "\n🔧 CONFIGURATION:",
    "Most parameters can be set via config/simulation_config.yaml",
    "Command line arguments override configuration file settings."
  )
)

args <- parse_args(opt_parser)

# =======================================================================
# CONFIGURATION AND INITIALIZATION
# =======================================================================

#' Enhanced configuration loading with command line overrides
#' 
#' @param args Parsed command line arguments
#' @return Merged configuration object
load_and_merge_config <- function(args) {
  
  # Load base configuration
  if (FRAMEWORK_ENHANCED && HAS_YAML && file.exists(args$config)) {
    tryCatch({
      config <- load_config(args$config)
      cat("✅ Configuration loaded from:", args$config, "\n")
    }, error = function(e) {
      cat("⚠️  Failed to load config:", e$message, "- using defaults\n")
      config <- get_default_config()
    })
  } else {
    if (FRAMEWORK_ENHANCED) {
      config <- get_default_config()
    } else {
      # Fallback configuration for original framework
      config <- list(
        simulation = list(default_iterations = 20),
        parallel = list(default_workers = parallel::detectCores() - 1, chunk_size = 50, memory_limit_gb = 8),
        output = list(directories = list(results = "results")),
        logging = list(level = "INFO")
      )
    }
    cat("ℹ️  Using default configuration\n")
  }
  
  # Override with command line arguments
  if (!is.null(args$chunks)) {
    config$parallel$chunk_size <- args$chunks
  }
  if (!is.null(args$iters)) {
    config$simulation$default_iterations <- args$iters
  }
  if (!is.null(args$outdir)) {
    config$output$directories$results <- args$outdir
  }
  if (!is.null(args$workers)) {
    config$parallel$default_workers <- args$workers
  }
  if (!is.null(args$memory_limit)) {
    config$parallel$memory_limit_gb <- args$memory_limit
  }
  if (!is.null(args$log_level)) {
    config$logging$level <- args$log_level
  }
  
  # Apply test mode modifications
  if (args$test_mode) {
    config$simulation$default_iterations <- min(2, config$simulation$default_iterations)
    config$parallel$chunk_size <- min(10, config$parallel$chunk_size)
    cat("🧪 Test mode enabled: reduced iterations and scenarios\n")
  }
  
  config
}

# Load merged configuration
config <- load_and_merge_config(args)

# Initialize enhanced logging
if (FRAMEWORK_ENHANCED && HAS_LOGGER) {
  log_level <- if (args$verbose) "DEBUG" else config$logging$level
  
  log_file <- if (config$logging$file) {
    log_dir <- config$output$directories$logs
    dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
    file.path(log_dir, sprintf("chunk_%03d_%s.log", args$chunk_id, format(Sys.time(), "%Y%m%d_%H%M%S")))
  } else {
    NULL
  }
  
  init_logging(log_level, log_file, config$logging$console)
  
  if (HAS_GLUE) {
    log_info <- function(msg, ...) logger::log_info(glue::glue(msg, ...))
    log_warn <- function(msg, ...) logger::log_warn(glue::glue(msg, ...))
    log_error <- function(msg, ...) logger::log_error(glue::glue(msg, ...))
    log_debug <- function(msg, ...) logger::log_debug(glue::glue(msg, ...))
  }
  
} else {
  # Fallback logging functions
  log_info <- function(msg, ...) cat("[INFO]", sprintf(msg, ...), "\n")
  log_warn <- function(msg, ...) cat("[WARN]", sprintf(msg, ...), "\n") 
  log_error <- function(msg, ...) cat("[ERROR]", sprintf(msg, ...), "\n")
  log_debug <- function(msg, ...) if (args$verbose) cat("[DEBUG]", sprintf(msg, ...), "\n")
}

# =======================================================================
# ENHANCED INPUT VALIDATION
# =======================================================================

#' Comprehensive input validation with detailed error reporting
#' 
#' @param args Command line arguments
#' @param config Configuration object
#' @return Validated and potentially corrected parameters
validate_enhanced_args <- function(args, config) {
  
  log_info("Validating input parameters...")
  
  errors <- character(0)
  warnings <- character(0)
  
  # Extract key parameters
  chunk_id <- args$chunk_id
  chunks <- config$parallel$chunk_size
  iters <- config$simulation$default_iterations
  workers <- config$parallel$default_workers
  memory_limit <- config$parallel$memory_limit_gb
  outdir <- config$output$directories$results
  
  # Validate chunk parameters
  if (chunk_id < 1 || chunks < 1) {
    errors <- c(errors, "chunk_id and chunks must be positive integers")
  }
  
  if (chunk_id > chunks) {
    errors <- c(errors, sprintf("chunk_id (%d) cannot exceed chunks (%d)", chunk_id, chunks))
  }
  
  # Validate iteration count
  if (iters < 1) {
    errors <- c(errors, "iterations must be positive")
  } else if (iters < 5 && !args$test_mode) {
    warnings <- c(warnings, sprintf("Low iteration count (%d) may produce unreliable results", iters))
  }
  
  # Validate and adjust worker count
  max_workers <- parallel::detectCores()
  if (is.na(max_workers)) {
    max_workers <- 1
    warnings <- c(warnings, "Cannot detect CPU cores - assuming 1")
  }
  
  if (workers < 1) {
    workers <- 1
    warnings <- c(warnings, "Workers must be positive - setting to 1")
  } else if (workers > max_workers) {
    old_workers <- workers
    workers <- max_workers
    warnings <- c(warnings, sprintf("Requested %d workers but only %d cores available - using %d", 
                                   old_workers, max_workers, workers))
  }
  
  # Update config with corrected values
  config$parallel$default_workers <- workers
  
  # Validate memory limit
  if (memory_limit <= 0) {
    errors <- c(errors, "memory_limit must be positive")
  } else if (memory_limit > 64) {
    warnings <- c(warnings, sprintf("Very high memory limit (%.1f GB) - ensure sufficient RAM", memory_limit))
  }
  
  # Validate output directory
  if (!args$dry_run) {
    tryCatch({
      dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
      if (!dir.exists(outdir)) {
        errors <- c(errors, sprintf("Cannot create output directory: %s", outdir))
      } else {
        # Test write permissions
        test_file <- file.path(outdir, paste0("test_", Sys.getpid(), ".tmp"))
        writeLines("test", test_file)
        file.remove(test_file)
      }
    }, error = function(e) {
      errors <- c(errors, sprintf("Output directory error: %s", e$message))
    })
  }
  
  # Report validation results
  if (length(warnings) > 0) {
    log_warn("Validation warnings:")
    for (warning in warnings) {
      log_warn("  • {warning}")
    }
  }
  
  if (length(errors) > 0) {
    log_error("Validation errors:")
    for (error in errors) {
      log_error("  • {error}")
    }
    stop("Input validation failed - please fix the above errors")
  }
  
  log_info("✅ Input validation passed")
  
  list(config = config, warnings = warnings)
}

# Validate inputs
validation_result <- validate_enhanced_args(args, config)
config <- validation_result$config

# =======================================================================
# ENHANCED SCENARIO PREPARATION
# =======================================================================

#' Generate scenario grid with enhanced validation
#' 
#' @param config Configuration object
#' @param test_mode Whether to generate reduced grid for testing
#' @return Validated scenario grid
generate_enhanced_grid <- function(config, test_mode = FALSE) {
  
  log_info("Generating scenario grid...")
  
  if (FRAMEWORK_ENHANCED) {
    if (test_mode) {
      # Reduced grid for testing
      grid_params <- list(
        EPV = c(3, 10, 50),
        event_frac = c(0.125, 0.25),
        p = c(4, 16),
        noise_frac = c(0, 0.5),
        sparse = c(FALSE, TRUE)
      )
      
      grid <- expand.grid(grid_params, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(scn_id = dplyr::row_number())
      
      log_info("Generated test grid: {nrow(grid)} scenarios")
      
    } else {
      grid <- make_full_grid()
      log_info("Generated full grid: {nrow(grid)} scenarios")
    }
  } else {
    # Fallback for original framework
    source("replicate_framework.R")
    grid <- make_full_grid()
    
    if (test_mode && nrow(grid) > 50) {
      grid <- grid[sample(nrow(grid), 50), ]
      grid$scn_id <- seq_len(nrow(grid))
      log_info("Sampled test grid: {nrow(grid)} scenarios")
    }
  }
  
  # Validate grid
  required_cols <- c("EPV", "event_frac", "p", "noise_frac", "sparse", "scn_id")
  missing_cols <- setdiff(required_cols, names(grid))
  
  if (length(missing_cols) > 0) {
    stop(sprintf("Grid missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }
  
  if (nrow(grid) == 0) {
    stop("Empty scenario grid generated")
  }
  
  # Log grid summary
  log_debug("Grid summary:")
  log_debug("  EPV levels: {length(unique(grid$EPV))} (range: {min(grid$EPV)}-{max(grid$EPV)})")
  log_debug("  Event fraction levels: {length(unique(grid$event_frac))}")
  log_debug("  Predictor levels: {length(unique(grid$p))} (range: {min(grid$p)}-{max(grid$p)})")
  log_debug("  Noise fraction levels: {length(unique(grid$noise_frac))}")
  log_debug("  Sparse levels: {length(unique(grid$sparse))}")
  
  grid
}

# Generate scenario grid
grid <- generate_enhanced_grid(config, args$test_mode)
total_scenarios <- nrow(grid)

# =======================================================================
# ENHANCED CHUNK MANAGEMENT
# =======================================================================

#' Calculate chunk boundaries with enhanced logic
#' 
#' @param total_scenarios Total number of scenarios
#' @param chunk_id Current chunk ID (1-based)
#' @param total_chunks Total number of chunks
#' @param memory_limit Memory limit in GB
#' @param grid Scenario grid for memory estimation
#' @return List with chunk information
calculate_chunk_boundaries <- function(total_scenarios, chunk_id, total_chunks, memory_limit, grid) {
  
  log_info("Calculating chunk boundaries...")
  
  # Basic chunk size calculation
  base_chunk_size <- ceiling(total_scenarios / total_chunks)
  start_idx <- (chunk_id - 1) * base_chunk_size + 1
  end_idx <- min(chunk_id * base_chunk_size, total_scenarios)
  scn_indices <- seq.int(start_idx, end_idx)
  
  # Memory-based adjustment if framework supports it
  if (FRAMEWORK_ENHANCED && length(scn_indices) > 0) {
    suggested_batch <- suggest_batch_size(scn_indices, grid, memory_limit)
    
    if (suggested_batch < length(scn_indices)) {
      log_warn("Memory constraints detected: chunk size {length(scn_indices)} exceeds suggested batch size {suggested_batch}")
      log_warn("Consider using more chunks or increasing memory limit")
    }
  }
  
  # Validate chunk
  if (length(scn_indices) == 0) {
    stop(sprintf("Chunk %d contains no scenarios to process", chunk_id))
  }
  
  if (start_idx > total_scenarios) {
    stop(sprintf("Chunk %d starts beyond available scenarios (start: %d, total: %d)", 
                 chunk_id, start_idx, total_scenarios))
  }
  
  log_info("Chunk {chunk_id}/{total_chunks}: scenarios {start_idx}-{end_idx} ({length(scn_indices)} scenarios)")
  
  list(
    chunk_id = chunk_id,
    total_chunks = total_chunks,
    start_idx = start_idx,
    end_idx = end_idx,
    scn_indices = scn_indices,
    n_scenarios = length(scn_indices)
  )
}

# Calculate chunk boundaries
chunk_info <- calculate_chunk_boundaries(
  total_scenarios, 
  args$chunk_id, 
  config$parallel$chunk_size,
  config$parallel$memory_limit_gb,
  grid
)

# =======================================================================
# ENHANCED EXECUTION PLANNING
# =======================================================================

#' Create comprehensive execution plan
#' 
#' @param chunk_info Chunk information
#' @param config Configuration object
#' @param args Command line arguments
#' @return Execution plan with resource estimates
create_execution_plan <- function(chunk_info, config, args) {
  
  log_info("Creating execution plan...")
  
  # Basic calculations
  n_scenarios <- chunk_info$n_scenarios
  n_iterations <- config$simulation$default_iterations
  n_workers <- config$parallel$default_workers
  n_methods <- if (FRAMEWORK_ENHANCED) length(list_methods()$method) else 11
  
  total_evaluations <- n_scenarios * n_iterations * n_methods
  
  # Time estimation (conservative)
  evaluations_per_minute_per_worker <- 50  # Conservative estimate
  estimated_minutes <- total_evaluations / (n_workers * evaluations_per_minute_per_worker)
  
  # Memory estimation
  if (FRAMEWORK_ENHANCED && length(chunk_info$scn_indices) > 0) {
    sample_scenario <- grid[chunk_info$scn_indices[1], ]
    memory_per_scenario_mb <- estimate_scenario_memory(sample_scenario) / 1e6
    estimated_memory_gb <- (memory_per_scenario_mb * n_scenarios * n_iterations) / 1000
  } else {
    # Fallback estimation
    max_p <- max(grid$p[chunk_info$scn_indices])
    max_epv <- max(grid$EPV[chunk_info$scn_indices])
    min_event_frac <- min(grid$event_frac[chunk_info$scn_indices])
    
    max_n <- max(200, ceiling((max_epv * max_p) / min_event_frac))
    estimated_memory_gb <- (max_n * max_p * 8 * 3) / 1e9  # Rough estimate
  }
  
  plan <- list(
    # Execution parameters
    n_scenarios = n_scenarios,
    n_iterations = n_iterations,
    n_methods = n_methods,
    n_workers = n_workers,
    total_evaluations = total_evaluations,
    
    # Resource estimates
    estimated_runtime_minutes = estimated_minutes,
    estimated_memory_gb = estimated_memory_gb,
    memory_limit_gb = config$parallel$memory_limit_gb,
    
    # Timing
    start_time = Sys.time(),
    
    # Resource checks
    memory_sufficient = estimated_memory_gb <= config$parallel$memory_limit_gb,
    worker_efficiency = min(n_workers, n_scenarios)  # Actual useful workers
  )
  
  plan
}

# Create execution plan
execution_plan <- create_execution_plan(chunk_info, config, args)

# =======================================================================
# DISPLAY ENHANCED EXECUTION SUMMARY
# =======================================================================

#' Display comprehensive execution summary
display_execution_summary <- function(execution_plan, chunk_info, config, args) {
  
  cat("\n")
  cat("🧪 ENHANCED LOHMANN ET AL. (2023) REPLICATION - EXECUTION SUMMARY\n")
  cat("=" * 80, "\n")
  
  # Chunk information
  cat(sprintf("📊 Chunk Information:\n"))
  cat(sprintf("   • Chunk ID: %d of %d\n", chunk_info$chunk_id, chunk_info$total_chunks))
  cat(sprintf("   • Scenarios: %d to %d (%d total)\n", chunk_info$start_idx, chunk_info$end_idx, chunk_info$n_scenarios))
  cat(sprintf("   • Iterations per scenario: %d\n", execution_plan$n_iterations))
  cat(sprintf("   • Methods to evaluate: %d\n", execution_plan$n_methods))
  cat(sprintf("   • Total evaluations: %s\n", format(execution_plan$total_evaluations, big.mark = ",")))
  
  # Resource allocation
  cat(sprintf("\n⚡ Resource Allocation:\n"))
  cat(sprintf("   • Parallel workers: %d (efficiency: %d)\n", execution_plan$n_workers, execution_plan$worker_efficiency))
  cat(sprintf("   • Memory limit: %.1f GB\n", execution_plan$memory_limit_gb))
  cat(sprintf("   • Estimated memory usage: %.2f GB\n", execution_plan$estimated_memory_gb))
  
  # Memory warning
  if (!execution_plan$memory_sufficient) {
    cat(sprintf("   ⚠️  WARNING: Estimated memory (%.2f GB) exceeds limit (%.1f GB)\n", 
                execution_plan$estimated_memory_gb, execution_plan$memory_limit_gb))
  }
  
  # Timing estimates
  cat(sprintf("\n⏱️  Timing Estimates:\n"))
  cat(sprintf("   • Estimated runtime: %.0f minutes (%.1f hours)\n", 
              execution_plan$estimated_runtime_minutes, execution_plan$estimated_runtime_minutes / 60))
  
  if (execution_plan$estimated_runtime_minutes > 60) {
    eta <- Sys.time() + lubridate::minutes(execution_plan$estimated_runtime_minutes)
    cat(sprintf("   • Estimated completion: %s\n", format(eta, "%Y-%m-%d %H:%M")))
  }
  
  # Configuration summary
  cat(sprintf("\n🔧 Configuration:\n"))
  cat(sprintf("   • Framework: %s\n", ifelse(FRAMEWORK_ENHANCED, "Enhanced", "Original")))
  cat(sprintf("   • Output directory: %s\n", config$output$directories$results))
  cat(sprintf("   • Logging level: %s\n", config$logging$level))
  
  # Feature flags
  enabled_features <- character(0)
  if (HAS_LOGGER) enabled_features <- c(enabled_features, "Advanced logging")
  if (HAS_PROGRESS) enabled_features <- c(enabled_features, "Progress tracking")
  if (HAS_PRYR) enabled_features <- c(enabled_features, "Memory monitoring")
  if (args$profile) enabled_features <- c(enabled_features, "Performance profiling")
  
  if (length(enabled_features) > 0) {
    cat(sprintf("   • Enhanced features: %s\n", paste(enabled_features, collapse = ", ")))
  }
  
  # Mode indicators
  mode_flags <- character(0)
  if (args$test_mode) mode_flags <- c(mode_flags, "Test mode")
  if (args$dry_run) mode_flags <- c(mode_flags, "Dry run")
  if (args$resume) mode_flags <- c(mode_flags, "Resume mode")
  if (args$verbose) mode_flags <- c(mode_flags, "Verbose")
  
  if (length(mode_flags) > 0) {
    cat(sprintf("   • Execution modes: %s\n", paste(mode_flags, collapse = ", ")))
  }
  
  cat("\n")
  
  # Sample scenarios
  if (args$verbose || args$dry_run) {
    cat("📋 Sample Scenarios in This Chunk:\n")
    sample_scenarios <- head(grid[chunk_info$scn_indices, ], 3)
    for (i in seq_len(nrow(sample_scenarios))) {
      s <- sample_scenarios[i, ]
      cat(sprintf("   %d. ID %d: EPV=%d, p=%d, event_frac=%.3f, noise=%.1f, sparse=%s\n",
                  i, s$scn_id, s$EPV, s$p, s$event_frac, s$noise_frac, s$sparse))
    }
    if (chunk_info$n_scenarios > 3) {
      cat(sprintf("   ... and %d more scenarios\n", chunk_info$n_scenarios - 3))
    }
    cat("\n")
  }
  
  cat("-" * 80, "\n")
}

# Display execution summary
display_execution_summary(execution_plan, chunk_info, config, args)

# Early exit for dry run
if (args$dry_run) {
  cat("🔍 DRY RUN COMPLETED - No simulation executed\n")
  cat("To execute for real, remove --dry_run flag\n\n")
  
  if (args$verbose) {
    cat("Would execute the following:\n")
    cat(sprintf("  1. Setup parallel processing with %d workers\n", execution_plan$n_workers))
    cat(sprintf("  2. Process %d scenarios in batches\n", chunk_info$n_scenarios))
    cat(sprintf("  3. Generate %s total evaluations\n", format(execution_plan$total_evaluations, big.mark = ",")))
    cat(sprintf("  4. Save results to: %s\n", config$output$directories$results))
  }
  
  quit(status = 0)
}

# =======================================================================
# ENHANCED FILE MANAGEMENT
# =======================================================================

#' Enhanced file management with resume capability
#' 
#' @param chunk_info Chunk information
#' @param config Configuration object
#' @param args Command line arguments
#' @return File management plan
setup_file_management <- function(chunk_info, config, args) {
  
  log_info("Setting up file management...")
  
  outdir <- config$output$directories$results
  chunk_id <- chunk_info$chunk_id
  
  # Define file paths with enhanced naming
  if (HAS_GLUE) {
    outfile <- glue::glue("{outdir}/sim_chunk_{chunk_id:03d}.rds")
    sumfile <- glue::glue("{outdir}/sim_chunk_{chunk_id:03d}_summary.csv")
    metafile <- glue::glue("{outdir}/sim_chunk_{chunk_id:03d}_metadata.yaml")
    lockfile <- glue::glue("{outdir}/sim_chunk_{chunk_id:03d}.lock")
  } else {
    outfile <- sprintf("%s/sim_chunk_%03d.rds", outdir, chunk_id)
    sumfile <- sprintf("%s/sim_chunk_%03d_summary.csv", outdir, chunk_id)
    metafile <- sprintf("%s/sim_chunk_%03d_metadata.yaml", outdir, chunk_id)
    lockfile <- sprintf("%s/sim_chunk_%03d.lock", outdir, chunk_id)
  }
  
  # Handle existing files
  files_exist <- c(
    main = file.exists(outfile),
    summary = file.exists(sumfile),
    metadata = file.exists(metafile),
    lock = file.exists(lockfile)
  )
  
  # Check for lock file (indicates another process is running)
  if (files_exist["lock"]) {
    lock_info <- tryCatch({
      readLines(lockfile)
    }, error = function(e) "Unknown process")
    
    if (!args$force) {
      stop(sprintf("Lock file exists (%s), indicating another process is running this chunk.\n Process info: %s\n Use --force to override (dangerous!)",
                   lockfile, paste(lock_info, collapse = "; ")))
    } else {
      log_warn("Overriding lock file - this may cause conflicts!")
      unlink(lockfile)
    }
  }
  
  # Handle resume mode
  resume_info <- NULL
  if (args$resume && files_exist["main"]) {
    tryCatch({
      existing_results <- readRDS(outfile)
      completed_scenarios <- unique(existing_results$scn_id)
      remaining_scenarios <- setdiff(chunk_info$scn_indices, completed_scenarios)
      
      resume_info <- list(
        completed_scenarios = completed_scenarios,
        remaining_scenarios = remaining_scenarios,
        n_completed = length(completed_scenarios),
        n_remaining = length(remaining_scenarios),
        existing_results = existing_results
      )
      
      log_info("Resume mode: {resume_info$n_completed} scenarios completed, {resume_info$n_remaining} remaining")
      
    }, error = function(e) {
      log_warn("Failed to load existing results for resume: {e$message}")
      resume_info <- NULL
    })
  }
  
  # Handle file conflicts
  if (any(files_exist[c("main", "summary")]) && !args$resume && !args$force) {
    existing_files <- names(files_exist)[files_exist]
    response <- readline(prompt = sprintf("⚠️  Output files exist: %s\nOverwrite? (y/N): ",
                                         paste(basename(existing_files), collapse = ", ")))
    
    if (!tolower(trimws(response)) %in% c("y", "yes")) {
      cat("❌ Execution cancelled by user\n")
      quit(status = 0)
    }
  }
  
  # Create backup of existing files
  if (files_exist["main"] && !args$resume) {
    backup_suffix <- format(Sys.time(), "%Y%m%d_%H%M%S")
    backup_file <- sprintf("%s.backup.%s", outfile, backup_suffix)
    
    tryCatch({
      file.copy(outfile, backup_file)
      log_info("Created backup: {basename(backup_file)}")
    }, error = function(e) {
      log_warn("Failed to create backup: {e$message}")
    })
  }
  
  # Create lock file
  lock_content <- c(
    sprintf("PID: %d", Sys.getpid()),
    sprintf("Started: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    sprintf("Chunk: %d", chunk_id),
    sprintf("Host: %s", Sys.info()["nodename"]),
    sprintf("User: %s", Sys.info()["user"])
  )
  
  tryCatch({
    writeLines(lock_content, lockfile)
    log_debug("Created lock file: {lockfile}")
  }, error = function(e) {
    log_warn("Failed to create lock file: {e$message}")
  })
  
  list(
    outfile = outfile,
    sumfile = sumfile,
    metafile = metafile,
    lockfile = lockfile,
    resume_info = resume_info,
    files_exist = files_exist
  )
}

# Setup file management
file_mgmt <- setup_file_management(chunk_info, config, args)

log_info("Results will be saved to: {basename(file_mgmt$outfile)}")
if (!is.null(file_mgmt$resume_info)) {
  log_info("Resuming with {file_mgmt$resume_info$n_remaining} remaining scenarios")
}

# =======================================================================
# ENHANCED PARALLEL PROCESSING SETUP
# =======================================================================

#' Setup enhanced parallel processing with monitoring
#' 
#' @param n_workers Number of workers
#' @param memory_limit Memory limit in GB
#' @param verbose Verbose output flag
#' @return Parallel processing information
setup_enhanced_parallel <- function(n_workers, memory_limit, verbose = FALSE) {
  
  log_info("Setting up parallel processing...")
  
  # Detect optimal worker count
  max_cores <- parallel::detectCores()
  if (is.na(max_cores)) max_cores <- 1
  
  actual_workers <- min(n_workers, max_cores)
  
  if (actual_workers != n_workers) {
    log_warn("Requested {n_workers} workers, using {actual_workers} (max available)")
  }
  
  # Memory per worker calculation
  memory_per_worker_gb <- memory_limit / actual_workers
  
  if (memory_per_worker_gb < 0.5) {
    log_warn("Very low memory per worker ({memory_per_worker_gb:.2f} GB) - performance may suffer")
  }
  
  tryCatch({
    # Setup future backend with enhanced options
    if (actual_workers > 1) {
      future::plan(
        future::multisession,
        workers = actual_workers,
        gc = TRUE  # Enable garbage collection between tasks
      )
      
      parallel_mode <- "multisession"
      
    } else {
      future::plan(future::sequential)
      parallel_mode <- "sequential"
    }
    
    log_info("Parallel processing configured: {parallel_mode} with {actual_workers} workers")
    
    # Test parallel setup if verbose
    if (verbose && actual_workers > 1) {
      log_debug("Testing parallel setup...")
      test_result <- future.apply::future_sapply(1:actual_workers, function(i) {
        list(worker_id = i, pid = Sys.getpid(), time = Sys.time())
      }, future.seed = TRUE)
      
      unique_pids <- length(unique(sapply(test_result, function(x) x$pid)))
      log_debug("Parallel test completed: {unique_pids} unique worker processes")
    }
    
    list(
      n_workers = actual_workers,
      memory_per_worker_gb = memory_per_worker_gb,
      parallel_mode = parallel_mode,
      setup_successful = TRUE
    )
    
  }, error = function(e) {
    log_error("Parallel setup failed: {e$message}")
    log_info("Falling back to sequential processing")
    
    future::plan(future::sequential)
    
    list(
      n_workers = 1,
      memory_per_worker_gb = memory_limit,
      parallel_mode = "sequential_fallback",
      setup_successful = FALSE,
      error = e$message
    )
  })
}

# Setup parallel processing
parallel_info <- setup_enhanced_parallel(
  config$parallel$default_workers,
  config$parallel$memory_limit_gb,
  args$verbose
)

# Update execution plan with actual worker count
execution_plan$n_workers <- parallel_info$n_workers
execution_plan$estimated_runtime_minutes <- execution_plan$total_evaluations / 
  (parallel_info$n_workers * 50)  # Recalculate with actual workers

# =======================================================================
# ENHANCED PROGRESS MONITORING
# =======================================================================

#' Create progress monitoring system
#' 
#' @param total_scenarios Total scenarios to process
#' @param n_iterations Iterations per scenario
#' @return Progress monitoring object
create_progress_monitor <- function(total_scenarios, n_iterations) {
  
  if (HAS_PROGRESS && !args$verbose) {
    # Create progress bar
    progress_bar <- progress::progress_bar$new(
      format = "[:bar] :current/:total (:percent) | ETA: :eta | Rate: :rate scenarios/min",
      total = total_scenarios,
      clear = FALSE,
      width = 80
    )
    
    list(
      type = "progress_bar",
      bar = progress_bar,
      start_time = Sys.time(),
      update = function(current) {
        progress_bar$tick()
      }
    )
  } else {
    # Simple text-based progress
    list(
      type = "simple",
      start_time = Sys.time(),
      last_report = 0,
      update = function(current, total = total_scenarios) {
        percent <- (current / total) * 100
        if (percent - last_report >= 10 || current == total) {
          elapsed <- difftime(Sys.time(), start_time, units = "mins")
          rate <- current / as.numeric(elapsed)
          eta <- if (current > 0) (total - current) / rate else NA
          
          if (HAS_GLUE) {
            log_info("Progress: {current}/{total} ({percent:.1f}%) | Rate: {rate:.1f} scenarios/min | ETA: {eta:.0f} min")
          } else {
            log_info(sprintf("Progress: %d/%d (%.1f%%) | Rate: %.1f scenarios/min | ETA: %.0f min",
                           current, total, percent, rate, eta))
          }
          
          last_report <<- percent
        }
      }
    )
  }
}

# =======================================================================
# ENHANCED SIMULATION EXECUTION
# =======================================================================

#' Execute simulation with comprehensive error handling and monitoring
#' 
#' @param chunk_info Chunk information
#' @param config Configuration object
#' @param file_mgmt File management info
#' @param parallel_info Parallel processing info
#' @return Simulation results
execute_enhanced_simulation <- function(chunk_info, config, file_mgmt, parallel_info) {
  
  log_info("🚀 Starting enhanced simulation execution...")
  
  # Determine scenarios to process
  if (!is.null(file_mgmt$resume_info)) {
    scenarios_to_process <- file_mgmt$resume_info$remaining_scenarios
  } else {
    scenarios_to_process <- chunk_info$scn_indices
  }
  
  if (length(scenarios_to_process) == 0) {
    log_info("All scenarios already completed - nothing to process")
    return(file_mgmt$resume_info$existing_results)
  }
  
  # Memory-aware batching
  if (FRAMEWORK_ENHANCED) {
    batch_size <- suggest_batch_size(scenarios_to_process, grid, config$parallel$memory_limit_gb)
  } else {
    batch_size <- min(50, length(scenarios_to_process))  # Conservative fallback
  }
  
  log_info("Processing {length(scenarios_to_process)} scenarios in batches of {batch_size}")
  
  # Create progress monitor
  progress_monitor <- create_progress_monitor(length(scenarios_to_process), config$simulation$default_iterations)
  
  # Batch processing loop
  all_results <- list()
  processed_count <- 0
  
  for (batch_start in seq(1, length(scenarios_to_process), by = batch_size)) {
    batch_end <- min(batch_start + batch_size - 1, length(scenarios_to_process))
    batch_indices <- scenarios_to_process[batch_start:batch_end]
    
    log_info("Processing batch {ceiling(batch_start/batch_size)}: scenarios {batch_start}-{batch_end}")
    
    # Execute batch with enhanced error handling
    batch_results <- tryCatch({
      execute_scenario_batch(batch_indices, grid, config, parallel_info)
    }, error = function(e) {
      log_error("Batch execution failed: {e$message}")
      
      # Try sequential fallback for this batch
      log_info("Attempting sequential fallback for failed batch...")
      tryCatch({
        old_plan <- future::plan()
        future::plan(future::sequential)
        
        result <- execute_scenario_batch(batch_indices, grid, config, 
                                       list(n_workers = 1, parallel_mode = "sequential_fallback"))
        
        future::plan(old_plan)  # Restore original plan
        result
        
      }, error = function(e2) {
        log_error("Sequential fallback also failed: {e2$message}")
        
        # Return empty results for this batch
        tibble::tibble(
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
    })
    
    # Store batch results
    if (nrow(batch_results) > 0) {
      all_results[[length(all_results) + 1]] <- batch_results
      
      # Save intermediate results for recovery
      if (length(all_results) > 1) {
        combined_so_far <- dplyr::bind_rows(all_results)
        temp_file <- sprintf("%s.temp", file_mgmt$outfile)
        saveRDS(combined_so_far, temp_file)
        log_debug("Saved intermediate results: {nrow(combined_so_far)} rows")
      }
    }
    
    # Update progress
    processed_count <- processed_count + length(batch_indices)
    progress_monitor$update(processed_count)
    
    # Force garbage collection between batches
    gc(verbose = FALSE)
    
    # Memory monitoring
    if (HAS_PRYR) {
      mem_usage <- pryr::mem_used()
      log_debug("Memory usage after batch: {format(mem_usage, units='MB')}")
    }
  }
  
  # Combine all results
  if (length(all_results) > 0) {
    final_results <- dplyr::bind_rows(all_results)
    
    # Add resumed results if applicable
    if (!is.null(file_mgmt$resume_info)) {
      final_results <- dplyr::bind_rows(file_mgmt$resume_info$existing_results, final_results)
    }
    
    log_info("Simulation completed: {nrow(final_results)} total evaluations")
    
  } else {
    log_error("No results generated - all batches failed")
    
    if (!is.null(file_mgmt$resume_info)) {
      log_info("Returning existing results from resume")
      final_results <- file_mgmt$resume_info$existing_results
    } else {
      stop("Simulation failed completely with no fallback results")
    }
  }
  
  # Clean up temporary files
  temp_file <- sprintf("%s.temp", file_mgmt$outfile)
  if (file.exists(temp_file)) {
    unlink(temp_file)
  }
  
  final_results
}

#' Execute a batch of scenarios with worker-level error handling
#' 
#' @param batch_indices Scenario indices for this batch
#' @param grid Scenario grid
#' @param config Configuration object
#' @param parallel_info Parallel processing information
#' @return Batch results
execute_scenario_batch <- function(batch_indices, grid, config, parallel_info) {
  
  # Create worker chunks for load balancing
  if (parallel_info$n_workers > 1) {
    worker_chunks <- split(batch_indices, 
                          sort(rep_len(1:parallel_info$n_workers, length(batch_indices))))
  } else {
    worker_chunks <- list(batch_indices)
  }
  
  # Enhanced worker function with comprehensive error handling
  worker_function <- function(chunk_scenarios) {
    
    # Load framework in worker
    if (FRAMEWORK_ENHANCED) {
      if (file.exists("enhanced_replicate_framework.R")) {
        source("enhanced_replicate_framework.R", local = TRUE)
      } else if (file.exists("replicate_framework.R")) {
        source("replicate_framework.R", local = TRUE)
      }
    } else {
      source("replicate_framework.R", local = TRUE)
    }
    
    # Suppress non-essential output in workers
    if (!config$logging$console || config$logging$level != "DEBUG") {
      sink(nullfile())
      on.exit(sink(), add = TRUE)
    }
    
    # Execute scenarios with robust error handling
    tryCatch({
      if (FRAMEWORK_ENHANCED) {
        run_scenarios_block(grid, chunk_scenarios, config$simulation$default_iterations,
                          base_seed = config$simulation$base_seed)
      } else {
        # Use original framework function with proper parameters
        run_scenarios_block(grid, chunk_scenarios, config$simulation$default_iterations,
                          base_seed = 20250923L)
      }
    }, error = function(e) {
      # Return structured error result instead of failing completely
      warning(sprintf("Worker chunk failed: %s", e$message))
      
      # Create empty result with proper structure
      tibble::tibble(
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
  
  # Execute parallel work
  worker_results <- future.apply::future_lapply(
    worker_chunks,
    worker_function,
    future.seed = TRUE,
    future.globals = FALSE  # Explicit sourcing in workers
  )
  
  # Combine worker results
  valid_results <- worker_results[sapply(worker_results, function(x) nrow(x) > 0)]
  
  if (length(valid_results) > 0) {
    dplyr::bind_rows(valid_results)
  } else {
    # Return empty result with proper structure
    tibble::tibble(
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
  }
}

# =======================================================================
# MAIN EXECUTION
# =======================================================================

cat("\n")
log_info("🎬 STARTING MAIN EXECUTION")
log_info("=" * 50)

# Record execution start
execution_start_time <- Sys.time()
execution_plan$actual_start_time <- execution_start_time

log_info("Execution started: {format(execution_start_time, '%Y-%m-%d %H:%M:%S')}")

# Execute simulation with comprehensive error handling
simulation_results <- tryCatch({
  
  # Performance profiling if requested
  if (args$profile && requireNamespace("profvis", quietly = TRUE)) {
    log_info("Running with performance profiling enabled...")
    
    profvis_result <- profvis::profvis({
      execute_enhanced_simulation(chunk_info, config, file_mgmt, parallel_info)
    })
    
    # Save profiling results
    profile_file <- sprintf("%s/profile_chunk_%03d.html", 
                           config$output$directories$results, chunk_info$chunk_id)
    htmlwidgets::saveWidget(profvis_result, profile_file)
    log_info("Performance profile saved: {basename(profile_file)}")
    
    # Return the actual results (profvis wraps the return value)
    attr(profvis_result, "prof")$results
    
  } else {
    # Normal execution without profiling
    execute_enhanced_simulation(chunk_info, config, file_mgmt, parallel_info)
  }
  
}, error = function(e) {
  log_error("Simulation execution failed: {e$message}")
  
  # Cleanup lock file before failing
  if (file.exists(file_mgmt$lockfile)) {
    unlink(file_mgmt$lockfile)
  }
  
  stop(sprintf("Simulation execution failed: %s", e$message))
})

# Record completion time
execution_end_time <- Sys.time()
total_runtime <- difftime(execution_end_time, execution_start_time, units = "mins")

log_info("Simulation completed: {format(execution_end_time, '%Y-%m-%d %H:%M:%S')}")
log_info("Total runtime: {round(as.numeric(total_runtime), 2)} minutes")

# =======================================================================
# ENHANCED RESULTS PROCESSING AND VALIDATION
# =======================================================================

#' Process and validate simulation results
#' 
#' @param results Raw simulation results
#' @param chunk_info Chunk information
#' @param config Configuration object
#' @return Processed and validated results
process_and_validate_results <- function(results, chunk_info, config) {
  
  log_info("Processing and validating results...")
  
  # Basic validation
  if (nrow(results) == 0) {
    stop("No results to process - simulation may have failed completely")
  }
  
  # Expected dimensions
  n_methods <- if (FRAMEWORK_ENHANCED) length(list_methods()$method) else 11
  expected_rows <- chunk_info$n_scenarios * config$simulation$default_iterations * n_methods
  actual_rows <- nrow(results)
  completion_rate <- (actual_rows / expected_rows) * 100
  
  log_info("Results validation:")
  log_info("  Expected evaluations: {expected_rows:,}")
  log_info("  Actual evaluations: {actual_rows:,}")
  log_info("  Completion rate: {round(completion_rate, 1)}%")
  
  # Quality checks
  quality_issues <- list()
  
  # Check for missing values
  na_counts <- results %>%
    summarise(
      across(c(auc, cal_slope, brier, rMSPE, MAPE), ~ sum(is.na(.x))),
      .groups = 'drop'
    )
  
  total_na <- sum(na_counts)
  if (total_na > 0) {
    na_rate <- (total_na / (nrow(results) * 5)) * 100  # 5 main metrics
    quality_issues$missing_values <- list(count = total_na, rate = na_rate)
    log_warn("Missing values detected: {total_na:,} ({round(na_rate, 2)}%)")
  }
  
  # Check metric ranges
  auc_issues <- sum(results$auc < 0.4 | results$auc > 1, na.rm = TRUE)
  if (auc_issues > 0) {
    quality_issues$auc_range <- auc_issues
    log_warn("AUC range issues: {auc_issues} values outside [0.4, 1.0]")
  }
  
  brier_issues <- sum(results$brier < 0 | results$brier > 1, na.rm = TRUE) 
  if (brier_issues > 0) {
    quality_issues$brier_range <- brier_issues
    log_warn("Brier score range issues: {brier_issues} values outside [0, 1]")
  }
  
  # Method completion rates
  method_completion <- results %>%
    group_by(method) %>%
    summarise(
      n_evaluations = n(),
      completion_rate = (n() / (chunk_info$n_scenarios * config$simulation$default_iterations)) * 100,
      na_rate = mean(is.na(auc)) * 100,
      .groups = 'drop'
    ) %>%
    arrange(desc(completion_rate))
  
  log_info("Method completion rates:")
  for (i in seq_len(min(5, nrow(method_completion)))) {
    m <- method_completion[i, ]
    log_info("  {m$method}: {round(m$completion_rate, 1)}% ({m$n_evaluations:,} evals, {round(m$na_rate, 1)}% NA)")
  }
  
  # Scenario completion
  scenario_completion <- results %>%
    distinct(scn_id, iter) %>%
    count(scn_id) %>%
    summarise(
      scenarios_with_all_iters = sum(n == config$simulation$default_iterations),
      mean_iterations = mean(n),
      .groups = 'drop'
    )
  
  complete_scenarios <- scenario_completion$scenarios_with_all_iters
  log_info("Scenario completion: {complete_scenarios}/{chunk_info$n_scenarios} scenarios with all iterations")
  
  # Overall quality assessment
  if (completion_rate < 80) {
    quality_issues$low_completion <- completion_rate
    log_warn("Low overall completion rate: {round(completion_rate, 1)}%")
  }
  
  if (length(quality_issues) > 0) {
    log_warn("Quality issues detected: {length(quality_issues)} issue types")
  } else {
    log_info("✅ Results passed quality validation")
  }
  
  # Add metadata to results
  results$chunk_processed_at <- Sys.time()
  results$framework_version <- ifelse(FRAMEWORK_ENHANCED, "enhanced", "original")
  
  list(
    results = results,
    quality_issues = quality_issues,
    completion_rate = completion_rate,
    method_completion = method_completion,
    scenario_completion = scenario_completion
  )
}

# Process and validate results
processed_results <- process_and_validate_results(simulation_results, chunk_info, config)

# =======================================================================
# ENHANCED FILE OUTPUT WITH METADATA
# =======================================================================

#' Save results with comprehensive metadata
#' 
#' @param processed_results Processed results object
#' @param file_mgmt File management info
#' @param execution_info Execution information
#' @param config Configuration object
save_enhanced_results <- function(processed_results, file_mgmt, execution_info, config) {
  
  log_info("Saving enhanced results...")
  
  # Save main results file with backup
  tryCatch({
    saveRDS(processed_results$results, file_mgmt$outfile)
    
    # Verify saved file
    test_load <- readRDS(file_mgmt$outfile)
    if (nrow(test_load) != nrow(processed_results$results)) {
      stop("File verification failed - saved file corrupted")
    }
    
    file_size_mb <- round(file.size(file_mgmt$outfile) / 1024^2, 2)
    log_info("✅ Main results saved: {basename(file_mgmt$outfile)} ({file_size_mb} MB)")
    
  }, error = function(e) {
    log_error("Failed to save main results: {e$message}")
    stop(sprintf("Critical error: Cannot save results - %s", e$message))
  })
  
  # Generate and save summary with rankings
  if (FRAMEWORK_ENHANCED) {
    summary_results <- tryCatch({
      processed_results$results %>%
        group_by(scn_id, iter) %>%
        group_modify(~ rank_with_rounding(.x)) %>%
        ungroup() %>%
        group_by(method) %>%
        summarise(
          mean_rank_auc = mean(r_auc, na.rm = TRUE),
          mean_rank_brier = mean(r_brier, na.rm = TRUE), 
          mean_rank_cals = mean(r_cals, na.rm = TRUE),
          mean_auc = mean(auc, na.rm = TRUE),
          mean_brier = mean(brier, na.rm = TRUE),
          n_evaluations = n(),
          na_count = sum(is.na(auc)),
          .groups = 'drop'
        ) %>%
        arrange(mean_rank_auc)
    }, error = function(e) {
      log_warn("Failed to generate enhanced summary: {e$message}")
      
      # Fallback summary
      processed_results$results %>%
        group_by(method) %>%
        summarise(
          mean_auc = mean(auc, na.rm = TRUE),
          mean_brier = mean(brier, na.rm = TRUE),
          n_evaluations = n(),
          .groups = 'drop'
        )
    })
  } else {
    # Basic summary for original framework
    summary_results <- processed_results$results %>%
      group_by(method) %>%
      summarise(
        mean_auc = mean(auc, na.rm = TRUE),
        mean_brier = mean(brier, na.rm = TRUE),
        n_evaluations = n(),
        .groups = 'drop'
      )
  }
  
  # Save summary
  tryCatch({
    readr::write_csv(summary_results, file_mgmt$sumfile)
    log_info("✅ Summary saved: {basename(file_mgmt$sumfile)}")
  }, error = function(e) {
    log_warn("Failed to save summary: {e$message}")
  })
  
  # Save metadata (if YAML available)
  if (HAS_YAML) {
    metadata <- list(
      execution = list(
        chunk_id = chunk_info$chunk_id,
        total_chunks = chunk_info$total_chunks,
        scenarios_processed = chunk_info$n_scenarios,
        iterations = config$simulation$default_iterations,
        start_time = format(execution_info$actual_start_time),
        end_time = format(Sys.time()),
        runtime_minutes = round(as.numeric(total_runtime), 2),
        framework = ifelse(FRAMEWORK_ENHANCED, "enhanced", "original")
      ),
      
      configuration = list(
        workers = parallel_info$n_workers,
        memory_limit_gb = config$parallel$memory_limit_gb,
        config_file = args$config,
        log_level = config$logging$level
      ),
      
      results = list(
        total_evaluations = nrow(processed_results$results),
        completion_rate = round(processed_results$completion_rate, 2),
        quality_issues = length(processed_results$quality_issues),
        top_method = summary_results$method[1]
      ),
      
      system = list(
        hostname = Sys.info()["nodename"],
        user = Sys.info()["user"],
        r_version = paste(R.version$major, R.version$minor, sep = "."),
        platform = R.version$platform
      )
    )
    
    tryCatch({
      yaml::write_yaml(metadata, file_mgmt$metafile)
      log_info("✅ Metadata saved: {basename(file_mgmt$metafile)}")
    }, error = function(e) {
      log_warn("Failed to save metadata: {e$message}")
    })
  }
  
  # Display top performers
  if (nrow(summary_results) > 0) {
    log_info("🏆 Top 5 methods by performance:")
    top_methods <- head(summary_results, 5)
    for (i in seq_len(nrow(top_methods))) {
      m <- top_methods[i, ]
      if (FRAMEWORK_ENHANCED && "mean_rank_auc" %in% names(m)) {
        log_info("  {i}. {m$method}: AUC rank {round(m$mean_rank_auc, 2)}, mean AUC {round(m$mean_auc, 3)}")
      } else {
        log_info("  {i}. {m$method}: mean AUC {round(m$mean_auc, 3)}, mean Brier {round(m$mean_brier, 4)}")
      }
    }
  }
}

# Save all results
save_enhanced_results(processed_results, file_mgmt, execution_plan, config)

# =======================================================================
# CLEANUP AND FINAL REPORTING
# =======================================================================

#' Perform cleanup and generate final report
#' 
#' @param file_mgmt File management info
#' @param execution_info Execution information
#' @param processed_results Processed results
cleanup_and_report <- function(file_mgmt, execution_info, processed_results) {
  
  log_info("Performing cleanup and generating final report...")
  
  # Remove lock file
  if (file.exists(file_mgmt$lockfile)) {
    unlink(file_mgmt$lockfile)
    log_debug("Removed lock file")
  }
  
  # Cleanup parallel workers
  tryCatch({
    future::plan(future::sequential)
    log_debug("Parallel workers cleaned up")
  }, error = function(e) {
    log_warn("Failed to cleanup parallel workers: {e$message}")
  })
  
  # Memory cleanup
  gc(verbose = FALSE)
  
  # Generate final performance report
  actual_runtime <- as.numeric(total_runtime)
  estimated_runtime <- execution_info$estimated_runtime_minutes
  
  runtime_efficiency <- if (estimated_runtime > 0) {
    (estimated_runtime / actual_runtime) * 100
  } else {
    NA
  }
  
  evaluations_per_minute <- nrow(processed_results$results) / actual_runtime
  
  # Final report
  cat("\n")
  log_info("🎉 ENHANCED EXECUTION COMPLETED SUCCESSFULLY!")
  log_info("=" * 60)
  
  log_info("📊 EXECUTION SUMMARY:")
  log_info("  • Chunk: {chunk_info$chunk_id}/{chunk_info$total_chunks}")
  log_info("  • Scenarios processed: {chunk_info$n_scenarios}")
  log_info("  • Total evaluations: {nrow(processed_results$results):,}")
  log_info("  • Completion rate: {round(processed_results$completion_rate, 1)}%")
  
  log_info("⏱️  PERFORMANCE METRICS:")
  log_info("  • Total runtime: {round(actual_runtime, 2)} minutes")
  log_info("  • Processing rate: {round(evaluations_per_minute, 1)} evaluations/minute")
  
  if (!is.na(runtime_efficiency)) {
    efficiency_status <- if (runtime_efficiency >= 80) "Excellent" else
                        if (runtime_efficiency >= 60) "Good" else
                        if (runtime_efficiency >= 40) "Fair" else "Poor"
    log_info("  • Runtime efficiency: {round(runtime_efficiency, 1)}% ({efficiency_status})")
  }
  
  log_info("💾 OUTPUT FILES:")
  log_info("  • Main results: {basename(file_mgmt$outfile)}")
  log_info("  • Summary: {basename(file_mgmt$sumfile)}")
  if (HAS_YAML && file.exists(file_mgmt$metafile)) {
    log_info("  • Metadata: {basename(file_mgmt$metafile)}")
  }
  
  # Quality report
  if (length(processed_results$quality_issues) > 0) {
    log_info("⚠️  QUALITY ISSUES:")
    for (issue_name in names(processed_results$quality_issues)) {
      issue <- processed_results$quality_issues[[issue_name]]
      if (is.list(issue)) {
        log_info("  • {issue_name}: {issue$count} ({round(issue$rate, 1)}%)")
      } else {
        log_info("  • {issue_name}: {issue}")
      }
    }
  } else {
    log_info("✅ QUALITY: No issues detected")
  }
  
  # System resource usage
  if (HAS_PRYR) {
    final_memory <- pryr::mem_used()
    log_info("🧠 RESOURCES:")
    log_info("  • Peak memory usage: {format(final_memory, units='MB')}")
    log_info("  • Parallel workers: {parallel_info$n_workers}")
  }
  
  # Next steps
  log_info("🚀 NEXT STEPS:")
  log_info("  1. Review results in: {file_mgmt$outfile}")
  log_info("  2. Run additional chunks if needed")
  log_info("  3. Aggregate all chunks: Rscript aggregate_results.R {config$output$directories$results}")
  
  if (processed_results$completion_rate < 95) {
    log_info("  ⚠️  Consider re-running this chunk to improve completion rate")
  }
  
  cat("\n")
  log_info("✨ Enhanced chunk execution completed successfully! ✨")
  
  # Return summary for potential further processing
  list(
    chunk_id = chunk_info$chunk_id,
    runtime_minutes = actual_runtime,
    n_evaluations = nrow(processed_results$results),
    completion_rate = processed_results$completion_rate,
    n_quality_issues = length(processed_results$quality_issues),
    top_method = if (nrow(processed_results$results) > 0) {
      processed_results$results %>% 
        group_by(method) %>% 
        summarise(mean_auc = mean(auc, na.rm = TRUE), .groups = 'drop') %>%
        arrange(desc(mean_auc)) %>%
        slice(1) %>%
        pull(method)
    } else {
      NA
    }
  )
}

# Perform final cleanup and reporting
final_summary <- cleanup_and_report(file_mgmt, execution_plan, processed_results)

# =======================================================================
# EXIT WITH SUCCESS
# =======================================================================

# Final memory cleanup
rm(simulation_results, processed_results, grid)
gc(verbose = FALSE)

# Log final status
log_info("Enhanced chunk execution script completed at {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}")

# Exit successfully
quit(status = 0)
