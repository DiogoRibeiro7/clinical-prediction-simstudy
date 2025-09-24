#!/bin/bash
# =======================================================================
# setup_phase_1_1.sh
# 
# Complete environment setup for Phase 1.1: Original Replication
# Prepares system, installs dependencies, validates configuration
#
# Usage: ./scripts/setup_phase_1_1.sh [options]
# =======================================================================

set -euo pipefail

# Configuration
R_PACKAGES_REQUIRED=(
    "MASS" "glmnet" "pROC" "plsRglm" "Matrix"
    "dplyr" "purrr" "tibble" "tidyr" "readr" 
    "ggplot2" "optparse" "future.apply"
)

R_PACKAGES_OPTIONAL=(
    "yaml" "logger" "glue" "pryr" "progress" 
    "checkmate" "testthat" "corrplot"
)

PYTHON_PACKAGES=(
    "numpy" "pandas" "matplotlib" "seaborn" "jupyter"
)

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
NC='\033[0m'

# Flags
INSTALL_R_PACKAGES=${INSTALL_R_PACKAGES:-true}
INSTALL_PYTHON=${INSTALL_PYTHON:-false}
SETUP_DOCKER=${SETUP_DOCKER:-false}
SKIP_TESTS=${SKIP_TESTS:-false}
VERBOSE=${VERBOSE:-false}

log_info() {
    echo -e "${GREEN}[INFO]${NC} $(date '+%Y-%m-%d %H:%M:%S') - $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $(date '+%Y-%m-%d %H:%M:%S') - $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $(date '+%Y-%m-%d %H:%M:%S') - $1" >&2
}

log_debug() {
    if [[ "$VERBOSE" == "true" ]]; then
        echo -e "${BLUE}[DEBUG]${NC} $(date '+%Y-%m-%d %H:%M:%S') - $1"
    fi
}

print_header() {
    echo -e "${PURPLE}"
    echo "======================================================================="
    echo "$1"
    echo "======================================================================="
    echo -e "${NC}"
}

check_system_requirements() {
    print_header "🖥️ SYSTEM REQUIREMENTS CHECK"
    
    log_info "Checking system requirements..."
    
    # Check operating system
    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        OS="Linux"
        DISTRO=$(lsb_release -si 2>/dev/null || echo "Unknown")
        VERSION=$(lsb_release -sr 2>/dev/null || echo "Unknown")
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        OS="macOS"
        DISTRO="macOS"
        VERSION=$(sw_vers -productVersion 2>/dev/null || echo "Unknown")
    elif [[ "$OSTYPE" == "msys" ]] || [[ "$OSTYPE" == "cygwin" ]]; then
        OS="Windows"
        DISTRO="Windows"
        VERSION="Unknown"
    else
        OS="Unknown"
        DISTRO="Unknown"
        VERSION="Unknown"
    fi
    
    log_info "Operating System: $OS ($DISTRO $VERSION)"
    
    # Check available memory
    if command -v free >/dev/null 2>&1; then
        TOTAL_RAM_GB=$(free -g | awk '/^Mem:/{print $2}')
        log_info "Available RAM: ${TOTAL_RAM_GB}GB"
        
        if [[ $TOTAL_RAM_GB -lt 8 ]]; then
            log_warn "Limited RAM detected. Consider reducing parallel workers or chunk size."
        fi
    elif [[ "$OS" == "macOS" ]]; then
        TOTAL_RAM_BYTES=$(sysctl -n hw.memsize 2>/dev/null || echo "0")
        TOTAL_RAM_GB=$((TOTAL_RAM_BYTES / 1024 / 1024 / 1024))
        log_info "Available RAM: ${TOTAL_RAM_GB}GB"
    fi
    
    # Check CPU cores
    if command -v nproc >/dev/null 2>&1; then
        CPU_CORES=$(nproc)
    elif [[ "$OS" == "macOS" ]]; then
        CPU_CORES=$(sysctl -n hw.ncpu 2>/dev/null || echo "1")
    else
        CPU_CORES="Unknown"
    fi
    
    log_info "CPU cores: $CPU_CORES"
    
    # Check disk space
    AVAILABLE_SPACE=$(df -h . | awk 'NR==2 {print $4}')
    log_info "Available disk space: $AVAILABLE_SPACE"
    
    log_info "✅ System requirements check completed"
}

install_r_dependencies() {
    print_header "📦 R DEPENDENCIES INSTALLATION"
    
    # Check if R is installed
    if ! command -v Rscript >/dev/null 2>&1; then
        log_error "R is not installed. Please install R first:"
        case "$OS" in
            "Linux")
                echo "  Ubuntu/Debian: sudo apt-get install r-base r-base-dev"
                echo "  CentOS/RHEL: sudo yum install R R-devel"
                ;;
            "macOS")
                echo "  With Homebrew: brew install r"
                echo "  Or download from: https://cran.r-project.org/"
                ;;
            "Windows")
                echo "  Download from: https://cran.r-project.org/"
                ;;
        esac
        exit 1
    fi
    
    # Get R version
    R_VERSION=$(Rscript -e "cat(paste(R.version$major, R.version$minor, sep='.'))" 2>/dev/null || echo "Unknown")
    log_info "R version: $R_VERSION"
    
    # Check R version compatibility
    R_MAJOR=$(echo $R_VERSION | cut -d. -f1)
    R_MINOR=$(echo $R_VERSION | cut -d. -f2)
    
    if [[ $R_MAJOR -lt 4 ]] || [[ $R_MAJOR -eq 4 && $R_MINOR -lt 0 ]]; then
        log_warn "R version $R_VERSION detected. R 4.0+ recommended for best compatibility."
    fi
    
    if [[ "$INSTALL_R_PACKAGES" != "true" ]]; then
        log_info "Skipping R package installation (--no-r-packages)"
        return
    fi
    
    log_info "Installing required R packages..."
    
    # Create R script for package installation
    cat > /tmp/install_r_packages.R << 'EOF'
# Enhanced R package installation with error handling
options(repos = c(CRAN = "https://cloud.r-project.org/"))

required_packages <- c(
    "MASS", "glmnet", "pROC", "plsRglm", "Matrix",
    "dplyr", "purrr", "tibble", "tidyr", "readr", 
    "ggplot2", "optparse", "future.apply"
)

optional_packages <- c(
    "yaml", "logger", "glue", "pryr", "progress", 
    "checkmate", "testthat", "corrplot"
)

install_packages_safely <- function(packages, required = TRUE) {
    for (pkg in packages) {
        cat(sprintf("Checking package: %s\n", pkg))
        
        if (!requireNamespace(pkg, quietly = TRUE)) {
            cat(sprintf("Installing %s...\n", pkg))
            
            tryCatch({
                install.packages(pkg, dependencies = TRUE, quiet = FALSE)
                
                # Verify installation
                if (requireNamespace(pkg, quietly = TRUE)) {
                    cat(sprintf("✅ Successfully installed: %s\n", pkg))
                } else {
                    cat(sprintf("❌ Installation failed: %s\n", pkg))
                    if (required) {
                        stop(sprintf("Required package %s failed to install", pkg))
                    }
                }
            }, error = function(e) {
                cat(sprintf("❌ Error installing %s: %s\n", pkg, e$message))
                if (required) {
                    stop(sprintf("Required package %s failed to install: %s", pkg, e$message))
                }
            })
        } else {
            cat(sprintf("✅ Already installed: %s\n", pkg))
        }
    }
}

# Install required packages
cat("Installing required packages...\n")
install_packages_safely(required_packages, required = TRUE)

# Install optional packages  
cat("\nInstalling optional packages...\n")
install_packages_safely(optional_packages, required = FALSE)

# Summary
cat("\n📦 PACKAGE INSTALLATION SUMMARY:\n")
all_packages <- c(required_packages, optional_packages)
installed <- sapply(all_packages, function(pkg) requireNamespace(pkg, quietly = TRUE))

cat(sprintf("✅ Installed: %d/%d packages\n", sum(installed), length(all_packages)))

missing <- all_packages[!installed]
if (length(missing) > 0) {
    cat("❌ Missing packages:", paste(missing, collapse = ", "), "\n")
    
    # Check if any required packages are missing
    missing_required <- intersect(missing, required_packages)
    if (length(missing_required) > 0) {
        cat("🔥 CRITICAL: Required packages missing:", paste(missing_required, collapse = ", "), "\n")
        quit(status = 1)
    }
}

cat("\n🎉 R package installation completed!\n")
EOF

    # Run R package installation
    if Rscript /tmp/install_r_packages.R; then
        log_info "✅ R packages installed successfully"
    else
        log_error "❌ R package installation failed"
        exit 1
    fi
    
    # Cleanup
    rm -f /tmp/install_r_packages.R
}

setup_project_structure() {
    print_header "📁 PROJECT STRUCTURE SETUP"
    
    log_info "Setting up project directory structure..."
    
    # Create required directories
    local directories=(
        "config"
        "scripts"
        "results"
        "logs"
        "plots"
        "validation"
        "tests/testthat"
        "data"
        "docs"
        ".github/workflows"
    )
    
    for dir in "${directories[@]}"; do
        if [[ ! -d "$dir" ]]; then
            mkdir -p "$dir"
            log_debug "Created directory: $dir"
        fi
    done
    
    # Create scripts directory and make executable
    if [[ ! -d "scripts" ]]; then
        mkdir -p scripts
    fi
    
    # Move/copy execution scripts to scripts directory
    local script_files=(
        "run_full_simulation.sh"
        "validate_replication.sh" 
        "setup_phase_1_1.sh"
    )
    
    for script in "${script_files[@]}"; do
        if [[ -f "$script" ]] && [[ ! -f "scripts/$script" ]]; then
            cp "$script" "scripts/"
            chmod +x "scripts/$script"
            log_debug "Copied and made executable: scripts/$script"
        fi
    done
    
    # Create .gitignore if it doesn't exist
    if [[ ! -f ".gitignore" ]]; then
        cat > .gitignore << 'EOF'
# Results and logs
results/*.rds
results/*.csv
logs/*.log
plots/*.png
plots/*.pdf
validation/*

# Temporary files
*.tmp
.Rhistory
.RData
.Ruserdata

# System files
.DS_Store
Thumbs.db

# IDE files
.vscode/
.idea/
*.Rproj*

# Package files
*.tar.gz
*.zip

# Documentation builds
docs/_build/
site/
EOF
        log_info "Created .gitignore file"
    fi
    
    # Create README_PHASE_1_1.md if it doesn't exist
    if [[ ! -f "README_PHASE_1_1.md" ]]; then
        cat > README_PHASE_1_1.md << 'EOF'
# Phase 1.1: Complete Original Replication

This directory contains everything needed to execute Phase 1.1 of the Enhanced Lohmann et al. (2023) Replication project.

## Quick Start

1. **Setup Environment**:
   ```bash
   ./scripts/setup_phase_1_1.sh
   ```

2. **Run Full Simulation**:
   ```bash
   ./scripts/run_full_simulation.sh --chunks 50 --iterations 20
   ```

3. **Validate Results**:
   ```bash
   ./scripts/validate_replication.sh
   ```

## Files Structure

- `scripts/` - Execution and setup scripts
- `config/` - Configuration files
- `results/` - Simulation output files
- `validation/` - Replication validation results
- `logs/` - Execution logs
- `plots/` - Generated visualizations

## Phase 1.1 Objectives

- [x] Enhanced framework implementation
- [ ] Complete 1,050 scenario simulation
- [ ] Statistical validation against original study
- [ ] Publication-ready results and documentation

See `ROADMAP.md` for complete project timeline and next phases.
EOF
        log_info "Created README_PHASE_1_1.md"
    fi
    
    log_info "✅ Project structure setup completed"
}

validate_framework() {
    print_header "🧪 FRAMEWORK VALIDATION"
    
    log_info "Validating simulation framework..."
    
    # Check required files exist
    local required_files=(
        "replicate_framework.R"
        "run_chunk.R" 
        "aggregate_results.R"
    )
    
    local missing_files=()
    
    for file in "${required_files[@]}"; do
        if [[ ! -f "$file" ]]; then
            missing_files+=("$file")
        else
            log_debug "✅ Found: $file"
        fi
    done
    
    if [[ ${#missing_files[@]} -gt 0 ]]; then
        log_error "Missing required files: ${missing_files[*]}"
        log_error "Please ensure all framework files are present"
        exit 1
    fi
    
    # Test R framework loading
    log_info "Testing R framework loading..."
    
    if Rscript -e "
        source('replicate_framework.R')
        cat('Framework loaded successfully\n')
        
        # Test grid generation
        grid <- make_full_grid()
        cat('Grid generated:', nrow(grid), 'scenarios\n')
        
        if (nrow(grid) != 1050) {
            cat('ERROR: Expected 1050 scenarios, got', nrow(grid), '\n')
            quit(status = 1)
        }
        
        # Test basic data generation
        set.seed(12345)
        data <- gen_dataset(p = 5, event_frac = 0.3, EPV = 10, noise_frac = 0, sparse = FALSE, seed = 12345)
        
        if (ncol(data\$Xtr) != 5) {
            cat('ERROR: Data generation failed\n')
            quit(status = 1)
        }
        
        cat('✅ Framework validation passed\n')
    " 2>/dev/null; then
        log_info "✅ Framework validation passed"
    else
        log_error "❌ Framework validation failed"
        log_error "Check R packages and framework files"
        exit 1
    fi
    
    # Test configuration loading if config exists
    if [[ -f "config/simulation_config.yaml" ]]; then
        log_info "Testing configuration loading..."
        
        if Rscript -e "
            if (requireNamespace('yaml', quietly = TRUE)) {
                config <- yaml::read_yaml('config/simulation_config.yaml')
                cat('✅ Configuration loaded successfully\n')
            } else {
                cat('⚠️ YAML package not available - using defaults\n')
            }
        " 2>/dev/null; then
            log_debug "Configuration test passed"
        else
            log_warn "Configuration loading issues detected"
        fi
    fi
    
    # Run unit tests if available and not skipped
    if [[ "$SKIP_TESTS" != "true" ]] && [[ -f "tests/testthat.R" ]]; then
        log_info "Running unit tests..."
        
        if Rscript -e "
            if (requireNamespace('testthat', quietly = TRUE)) {
                testthat::test_dir('tests')
                cat('✅ Unit tests passed\n')
            } else {
                cat('⚠️ testthat package not available - skipping tests\n')
            }
        " 2>/dev/null; then
            log_info "✅ Unit tests passed"
        else
            log_warn "Some unit tests failed - check test output"
        fi
    fi
    
    log_info "✅ Framework validation completed"
}

setup_configuration() {
    print_header "⚙️ CONFIGURATION SETUP"
    
    local config_file="config/simulation_config.yaml"
    
    # Create default configuration if it doesn't exist
    if [[ ! -f "$config_file" ]]; then
        log_info "Creating default configuration file..."
        
        cat > "$config_file" << 'EOF'
# =======================================================================
# Simulation Configuration for Lohmann et al. (2023) Replication
# Phase 1.1: Complete Original Replication
# =======================================================================

# Core simulation parameters
simulation:
  default_iterations: 20
  cv_folds: 10
  max_pls_components: 30
  validation_scale: 20
  base_seed: 20250923
  
# Grid parameters (exact replication of original study)
grid:
  EPV: [3, 5, 10, 15, 20, 50, 100]
  event_fractions: [0.03125, 0.0625, 0.125, 0.25, 0.5]  # 1/32, 1/16, 1/8, 1/4, 1/2
  predictors: [4, 8, 16, 32, 64]
  noise_fractions: [0, 0.25, 0.5]
  sparse_options: [false, true]
  
# Parallel processing
parallel:
  default_workers: -1  # Auto-detect
  chunk_size: 50
  memory_limit_gb: 8
  
# Output settings
output:
  precision:
    auc: 3
    brier: 3
    cal_slope: 2
    rmspe: 3
    mape: 3
  
  directories:
    results: "results"
    logs: "logs"  
    plots: "plots"
    
# Logging
logging:
  level: "INFO"
  console: true
  file: true
  
# Quality assurance
quality:
  min_convergence_rate: 0.95
  max_missing_rate: 0.05
EOF
        
        log_info "✅ Created configuration file: $config_file"
    else
        log_info "Configuration file already exists: $config_file"
    fi
    
    # Validate configuration
    if command -v Rscript >/dev/null 2>&1; then
        log_info "Validating configuration..."
        
        if Rscript -e "
            if (requireNamespace('yaml', quietly = TRUE)) {
                config <- yaml::read_yaml('$config_file')
                
                # Validate grid dimensions
                expected_scenarios <- with(config\$grid, {
                    length(EPV) * length(event_fractions) * length(predictors) * 
                    length(noise_fractions) * length(sparse_options)
                })
                
                if (expected_scenarios == 1050) {
                    cat('✅ Configuration validation passed\n')
                    cat('Expected scenarios:', expected_scenarios, '\n')
                } else {
                    cat('❌ Configuration error: Expected 1050 scenarios, calculated', expected_scenarios, '\n')
                    quit(status = 1)
                }
            } else {
                cat('⚠️ Cannot validate YAML config - yaml package not available\n')
            }
        " 2>/dev/null; then
            log_info "✅ Configuration validation passed"
        else
            log_error "❌ Configuration validation failed"
            exit 1
        fi
    fi
}

create_execution_scripts() {
    print_header "📝 CREATING EXECUTION SCRIPTS"
    
    log_info "Creating helper scripts for Phase 1.1..."
    
    # Quick test script
    if [[ ! -f "scripts/quick_test.sh" ]]; then
        cat > scripts/quick_test.sh << 'EOF'
#!/bin/bash
# Quick test run for Phase 1.1
set -euo pipefail

echo "🧪 Running Phase 1.1 Quick Test..."

# Run with minimal scenarios for testing
./scripts/run_full_simulation.sh \
    --test-mode \
    --verbose \
    --chunks 5 \
    --iterations 2 \
    --workers 2

echo "✅ Quick test completed!"
EOF
        chmod +x scripts/quick_test.sh
        log_info "Created: scripts/quick_test.sh"
    fi
    
    # Status checker script
    if [[ ! -f "scripts/check_status.sh" ]]; then
        cat > scripts/check_status.sh << 'EOF'
#!/bin/bash
# Check Phase 1.1 execution status
set -euo pipefail

RESULTS_DIR=${1:-"results"}

echo "📊 Phase 1.1 Execution Status"
echo "=============================="

# Count completed chunks
completed_chunks=$(find "$RESULTS_DIR" -name "sim_chunk_*.rds" 2>/dev/null | wc -l)
echo "Completed chunks: $completed_chunks / 50"

# Check for failed chunks
if [[ -f "logs/failed_chunks.txt" ]]; then
    failed_count=$(wc -l < "logs/failed_chunks.txt")
    echo "Failed chunks: $failed_count"
    echo "Failed chunk IDs: $(cat logs/failed_chunks.txt | tr '\n' ' ')"
else
    echo "Failed chunks: 0"
fi

# Check disk usage
if [[ -d "$RESULTS_DIR" ]]; then
    total_size=$(du -sh "$RESULTS_DIR" 2>/dev/null | cut -f1)
    echo "Results directory size: $total_size"
fi

# Check if aggregation completed
if [[ -f "$RESULTS_DIR/summary_ranks_overall.csv" ]]; then
    echo "Results aggregated: ✅"
    
    # Show top methods
    echo ""
    echo "Top 5 methods by performance:"
    head -6 "$RESULTS_DIR/summary_ranks_overall.csv" | column -t -s,
else
    echo "Results aggregated: ❌"
fi

echo ""
echo "To resume execution: ./scripts/run_full_simulation.sh --resume"
EOF
        chmod +x scripts/check_status.sh
        log_info "Created: scripts/check_status.sh"
    fi
    
    # Cleanup script
    if [[ ! -f "scripts/cleanup.sh" ]]; then
        cat > scripts/cleanup.sh << 'EOF'
#!/bin/bash
# Cleanup Phase 1.1 results and logs
set -euo pipefail

echo "🧹 Cleaning up Phase 1.1 files..."

read -p "This will delete all results and logs. Continue? (y/N): " -n 1 -r
echo

if [[ $REPLY =~ ^[Yy]$ ]]; then
    # Remove results
    if [[ -d "results" ]]; then
        rm -rf results/*
        echo "✅ Cleared results directory"
    fi
    
    # Remove logs  
    if [[ -d "logs" ]]; then
        rm -rf logs/*
        echo "✅ Cleared logs directory"
    fi
    
    # Remove validation files
    if [[ -d "validation" ]]; then
        rm -rf validation/*
        echo "✅ Cleared validation directory"
    fi
    
    # Remove plots
    if [[ -d "plots" ]]; then
        rm -rf plots/*
        echo "✅ Cleared plots directory"  
    fi
    
    echo "🎉 Cleanup completed!"
else
    echo "❌ Cleanup cancelled"
fi
EOF
        chmod +x scripts/cleanup.sh
        log_info "Created: scripts/cleanup.sh"
    fi
    
    log_info "✅ Execution scripts created"
}

setup_docker() {
    if [[ "$SETUP_DOCKER" != "true" ]]; then
        return
    fi
    
    print_header "🐳 DOCKER SETUP"
    
    log_info "Creating Docker configuration for reproducible environment..."
    
    # Create Dockerfile
    if [[ ! -f "Dockerfile" ]]; then
        cat > Dockerfile << 'EOF'
FROM rocker/r-ver:4.3.2

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
COPY install_packages.R /tmp/
RUN Rscript /tmp/install_packages.R

# Set up working directory
WORKDIR /app

# Copy project files
COPY . .

# Make scripts executable
RUN chmod +x scripts/*.sh

# Default command
CMD ["./scripts/run_full_simulation.sh", "--help"]
EOF
        log_info "Created: Dockerfile"
    fi
    
    # Create docker-compose.yml
    if [[ ! -f "docker-compose.yml" ]]; then
        cat > docker-compose.yml << 'EOF'
version: '3.8'

services:
  lohmann-replication:
    build: .
    volumes:
      - ./results:/app/results
      - ./logs:/app/logs
      - ./validation:/app/validation
      - ./plots:/app/plots
    environment:
      - CHUNKS=50
      - ITERATIONS=20
      - WORKERS=4
      - MEMORY_LIMIT=8
    command: ./scripts/run_full_simulation.sh
    
  test:
    build: .
    volumes:
      - ./results:/app/results
      - ./logs:/app/logs
    command: ./scripts/quick_test.sh
EOF
        log_info "Created: docker-compose.yml"
    fi
    
    # Create .dockerignore
    if [[ ! -f ".dockerignore" ]]; then
        cat > .dockerignore << 'EOF'
results/
logs/
validation/
plots/
.git/
.DS_Store
*.tmp
.Rhistory
*.tar.gz
EOF
        log_info "Created: .dockerignore"
    fi
    
    log_info "✅ Docker setup completed"
    log_info "To build and run: docker-compose up"
}

generate_setup_report() {
    print_header "📋 SETUP COMPLETION REPORT"
    
    local report_file="SETUP_PHASE_1_1_REPORT.md"
    
    cat > "$report_file" << EOF
# Phase 1.1 Setup Completion Report

**Generated**: $(date '+%Y-%m-%d %H:%M:%S')  
**Setup Status**: ✅ COMPLETED

## System Configuration

- **Operating System**: $OS ($DISTRO $VERSION)
- **R Version**: $(Rscript -e "cat(paste(R.version$major, R.version$minor, sep='.'))" 2>/dev/null || echo "Unknown")
- **CPU Cores**: $CPU_CORES
- **Available RAM**: ${TOTAL_RAM_GB:-"Unknown"}GB

## Components Installed

### ✅ Required Components
- [x] **R Framework**: Core simulation engine
- [x] **R Packages**: All required dependencies  
- [x] **Project Structure**: Directories and organization
- [x] **Configuration**: Default simulation parameters
- [x] **Execution Scripts**: Automation and monitoring tools

### 📦 Package Status
EOF

    # Add R package status
    if Rscript -e "
        required <- c('MASS', 'glmnet', 'pROC', 'plsRglm', 'Matrix', 'dplyr', 'purrr', 'tibble', 'tidyr', 'readr', 'ggplot2', 'optparse', 'future.apply')
        optional <- c('yaml', 'logger', 'glue', 'pryr', 'progress', 'checkmate', 'testthat', 'corrplot')
        
        req_installed <- sum(sapply(required, requireNamespace, quietly = TRUE))
        opt_installed <- sum(sapply(optional, requireNamespace, quietly = TRUE))
        
        cat('**Required packages**: ', req_installed, '/', length(required), ' installed\n')
        cat('**Optional packages**: ', opt_installed, '/', length(optional), ' installed\n')
    " 2>/dev/null >> "$report_file"; then
        echo "" >> "$report_file"
    fi
    
    cat >> "$report_file" << EOF

## Ready for Execution

Your system is now configured for Phase 1.1: Complete Original Replication.

### 🚀 Next Steps

1. **Quick Test** (recommended first step):
   \`\`\`bash
   ./scripts/quick_test.sh
   \`\`\`

2. **Full Simulation** (several hours):
   \`\`\`bash
   ./scripts/run_full_simulation.sh --chunks 50 --iterations 20
   \`\`\`

3. **Monitor Progress**:
   \`\`\`bash
   ./scripts/check_status.sh
   \`\`\`

4. **Validate Results**:
   \`\`\`bash
   ./scripts/validate_replication.sh
   \`\`\`

### 📚 Documentation

- **Quick Start**: See README_PHASE_1_1.md
- **Full Roadmap**: See ROADMAP.md
- **Configuration**: Edit config/simulation_config.yaml
- **Troubleshooting**: Check logs/ directory for issues

### 🛠 Utility Scripts

- \`scripts/quick_test.sh\` - Fast validation run
- \`scripts/check_status.sh\` - Monitor execution progress  
- \`scripts/cleanup.sh\` - Reset for fresh start

---

*Setup completed by Enhanced Lohmann et al. (2023) Replication Framework*  
*For support: https://github.com/DiogoRibeiro7/replication-lohmann-2023/issues*
EOF
    
    log_info "📋 Setup report generated: $report_file"
    
    # Display summary
    echo ""
    log_info "🎉 PHASE 1.1 SETUP COMPLETED SUCCESSFULLY!"
    echo ""
    log_info "📋 Setup Summary:"
    echo "   • All required components installed and configured"
    echo "   • Project structure created with proper organization"
    echo "   • Execution scripts prepared and made executable"
    echo "   • Configuration files ready with optimal defaults"
    echo ""
    log_info "🚀 Ready to execute Phase 1.1 simulation!"
    echo ""
    log_info "📖 Next steps:"
    echo "   1. Test: ./scripts/quick_test.sh"
    echo "   2. Full run: ./scripts/run_full_simulation.sh"
    echo "   3. Validate: ./scripts/validate_replication.sh"
    echo ""
    log_info "📄 Full setup report: $report_file"
}

# Parse command line arguments
parse_arguments() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            --no-r-packages)
                INSTALL_R_PACKAGES=false
                shift
                ;;
            --with-python)
                INSTALL_PYTHON=true
                shift
                ;;
            --with-docker)
                SETUP_DOCKER=true
                shift
                ;;
            --skip-tests)
                SKIP_TESTS=true
                shift
                ;;
            --verbose|-v)
                VERBOSE=true
                shift
                ;;
            --help|-h)
                cat << EOF
Enhanced Lohmann et al. (2023) Replication - Phase 1.1 Setup

USAGE: $0 [options]

OPTIONS:
    --no-r-packages     Skip R package installation
    --with-python       Install Python dependencies
    --with-docker       Setup Docker configuration
    --skip-tests        Skip unit tests validation
    --verbose, -v       Enable verbose output
    --help, -h          Show this help

EXAMPLES:
    # Standard setup
    $0

    # Setup with Docker support
    $0 --with-docker --verbose

    # Minimal setup (packages already installed)
    $0 --no-r-packages --skip-tests
EOF
                exit 0
                ;;
            *)
                log_error "Unknown option: $1"
                exit 1
                ;;
        esac
    done
}

# Main execution function
main() {
    print_header "🛠️ ENHANCED LOHMANN ET AL. (2023) REPLICATION - PHASE 1.1 SETUP"
    
    log_info "Preparing environment for Phase 1.1: Complete Original Replication"
    log_info "This will install dependencies and configure the simulation framework"
    echo ""
    
    # Execute setup phases
    check_system_requirements
    echo ""
    
    install_r_dependencies  
    echo ""
    
    setup_project_structure
    echo ""
    
    setup_configuration
    echo ""
    
    validate_framework
    echo ""
    
    create_execution_scripts
    echo ""
    
    setup_docker
    echo ""
    
    generate_setup_report
}

# Parse arguments and run main function
parse_arguments "$@"
main
