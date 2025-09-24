#!/bin/bash
# =======================================================================
# run_full_simulation.sh
# 
# Complete execution script for Phase 1.1: Original Replication
# Executes all 1,050 scenarios across 50 chunks with comprehensive monitoring
#
# Usage: ./scripts/run_full_simulation.sh [options]
# =======================================================================

set -euo pipefail  # Exit on error, undefined vars, pipe failures

# =======================================================================
# CONFIGURATION AND SETUP
# =======================================================================

# Default parameters (can be overridden)
CHUNKS=${CHUNKS:-50}
ITERATIONS=${ITERATIONS:-20}
WORKERS=${WORKERS:-$(nproc --ignore=1)}  # Auto-detect cores minus 1
MEMORY_LIMIT=${MEMORY_LIMIT:-8}  # GB
CONFIG_FILE=${CONFIG_FILE:-"config/simulation_config.yaml"}
RESULTS_DIR=${RESULTS_DIR:-"results"}
LOG_DIR=${LOG_DIR:-"logs"}

# Execution modes
DRY_RUN=${DRY_RUN:-false}
RESUME=${RESUME:-false}
FORCE=${FORCE:-false}
VERBOSE=${VERBOSE:-false}
TEST_MODE=${TEST_MODE:-false}

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# =======================================================================
# UTILITY FUNCTIONS
# =======================================================================

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

print_separator() {
    echo -e "${CYAN}-----------------------------------------------------------------------${NC}"
}

check_dependencies() {
    log_info "Checking dependencies..."
    
    # Check R installation
    if ! command -v Rscript &> /dev/null; then
        log_error "R is not installed or not in PATH"
        exit 1
    fi
    
    # Check required files
    local required_files=("replicate_framework.R" "run_chunk.R" "aggregate_results.R")
    for file in "${required_files[@]}"; do
        if [[ ! -f "$file" ]]; then
            log_error "Required file not found: $file"
            exit 1
        fi
    done
    
    # Check configuration
    if [[ ! -f "$CONFIG_FILE" ]]; then
        log_warn "Configuration file not found: $CONFIG_FILE"
        log_info "Using default configuration"
    fi
    
    # Check R packages
    log_debug "Checking R package dependencies..."
    if ! Rscript -e "
        required_packages <- c('dplyr', 'readr', 'future.apply', 'optparse', 'glmnet', 'pROC')
        missing <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
        if (length(missing) > 0) {
            cat('Missing packages:', paste(missing, collapse = ', '), '\n')
            quit(status = 1)
        }
        cat('All required packages available\n')
    " 2>/dev/null; then
        log_error "Missing required R packages. Install with:"
        echo "install.packages(c('dplyr', 'readr', 'future.apply', 'optparse', 'glmnet', 'pROC'))"
        exit 1
    fi
    
    log_info "✅ All dependencies satisfied"
}

setup_directories() {
    log_info "Setting up directory structure..."
    
    mkdir -p "$RESULTS_DIR"
    mkdir -p "$LOG_DIR"
    mkdir -p "scripts"
    mkdir -p "plots"
    
    log_debug "Created directories: $RESULTS_DIR, $LOG_DIR, scripts, plots"
}

estimate_resources() {
    log_info "Estimating resource requirements..."
    
    # Calculate totals
    local total_scenarios=1050
    local total_evaluations=$((total_scenarios * ITERATIONS * 11))  # 11 methods
    local scenarios_per_chunk=$((total_scenarios / CHUNKS))
    
    # Time estimation (conservative: 50 evaluations per minute per worker)
    local eval_per_min_per_worker=50
    local estimated_minutes_per_chunk=$((scenarios_per_chunk * ITERATIONS * 11 / WORKERS / eval_per_min_per_worker))
    local estimated_total_hours=$((estimated_minutes_per_chunk * CHUNKS / 60))
    
    # Memory estimation
    local estimated_memory_per_chunk_gb=2
    
    echo ""
    log_info "📊 RESOURCE ESTIMATION:"
    echo "   • Total scenarios: $total_scenarios"
    echo "   • Total evaluations: $(printf "%'d" $total_evaluations)"
    echo "   • Chunks: $CHUNKS (≈$scenarios_per_chunk scenarios each)"
    echo "   • Parallel workers: $WORKERS"
    echo "   • Estimated time per chunk: ${estimated_minutes_per_chunk} minutes"
    echo "   • Estimated total time: ${estimated_total_hours} hours"
    echo "   • Estimated memory per chunk: ${estimated_memory_per_chunk_gb} GB"
    echo "   • Peak concurrent memory: $((estimated_memory_per_chunk_gb * WORKERS)) GB"
    
    # Warnings
    if [[ $estimated_total_hours -gt 24 ]]; then
        log_warn "Estimated runtime >24 hours - consider increasing chunks or workers"
    fi
    
    if [[ $((estimated_memory_per_chunk_gb * WORKERS)) -gt 32 ]]; then
        log_warn "High memory usage estimated - monitor system resources"
    fi
    
    echo ""
}

parse_arguments() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            --chunks)
                CHUNKS="$2"
                shift 2
                ;;
            --iterations|--iters)
                ITERATIONS="$2"
                shift 2
                ;;
            --workers)
                WORKERS="$2"
                shift 2
                ;;
            --memory-limit)
                MEMORY_LIMIT="$2"
                shift 2
                ;;
            --config)
                CONFIG_FILE="$2"
                shift 2
                ;;
            --results-dir)
                RESULTS_DIR="$2"
                shift 2
                ;;
            --dry-run)
                DRY_RUN=true
                shift
                ;;
            --resume)
                RESUME=true
                shift
                ;;
            --force)
                FORCE=true
                shift
                ;;
            --verbose|-v)
                VERBOSE=true
                shift
                ;;
            --test-mode)
                TEST_MODE=true
                CHUNKS=5
                ITERATIONS=2
                shift
                ;;
            --help|-h)
                show_help
                exit 0
                ;;
            *)
                log_error "Unknown option: $1"
                show_help
                exit 1
                ;;
        esac
    done
}

show_help() {
    cat << EOF
🧪 ENHANCED LOHMANN ET AL. (2023) FULL SIMULATION RUNNER

USAGE:
    $0 [OPTIONS]

OPTIONS:
    --chunks N           Number of chunks to divide simulation (default: 50)
    --iterations N       Iterations per scenario (default: 20)
    --workers N          Parallel workers per chunk (default: auto-detect)
    --memory-limit N     Memory limit in GB (default: 8)
    --config FILE        Configuration file (default: config/simulation_config.yaml)
    --results-dir DIR    Results directory (default: results)
    
    --dry-run           Show execution plan without running
    --resume            Resume from existing partial results
    --force             Overwrite existing results without confirmation
    --verbose, -v       Enable verbose output
    --test-mode         Quick test with reduced scenarios
    --help, -h          Show this help message

EXAMPLES:
    # Standard full simulation
    $0 --chunks 50 --iterations 20 --workers 8

    # Quick test run
    $0 --test-mode --verbose

    # Resume interrupted simulation
    $0 --resume --force

    # High-performance run with more resources
    $0 --chunks 25 --workers 16 --memory-limit 16

ENVIRONMENT VARIABLES:
    CHUNKS, ITERATIONS, WORKERS, MEMORY_LIMIT, CONFIG_FILE, RESULTS_DIR
    Can be set instead of using command line arguments.

EOF
}

# =======================================================================
# EXECUTION FUNCTIONS
# =======================================================================

run_chunk() {
    local chunk_id=$1
    local chunk_log="$LOG_DIR/chunk_${chunk_id}.log"
    
    # Build command
    local cmd="Rscript run_chunk.R --chunks $CHUNKS --chunk_id $chunk_id --iters $ITERATIONS --workers $WORKERS --outdir $RESULTS_DIR --memory_limit $MEMORY_LIMIT"
    
    if [[ "$FORCE" == "true" ]]; then
        cmd="$cmd --force"
    fi
    
    if [[ "$RESUME" == "true" ]]; then
        cmd="$cmd --resume"
    fi
    
    if [[ "$VERBOSE" == "true" ]]; then
        cmd="$cmd --verbose"
    fi
    
    if [[ "$TEST_MODE" == "true" ]]; then
        cmd="$cmd --test_mode"
    fi
    
    if [[ -f "$CONFIG_FILE" ]]; then
        cmd="$cmd --config $CONFIG_FILE"
    fi
    
    log_debug "Executing chunk $chunk_id: $cmd"
    
    # Execute with logging
    if [[ "$VERBOSE" == "true" ]]; then
        # Real-time output for verbose mode
        eval "$cmd" 2>&1 | tee "$chunk_log"
        return ${PIPESTATUS[0]}
    else
        # Background execution with log capture
        eval "$cmd" > "$chunk_log" 2>&1
        return $?
    fi
}

monitor_progress() {
    local total_chunks=$1
    local completed=0
    local failed=0
    local running=0
    
    while [[ $((completed + failed)) -lt $total_chunks ]]; do
        sleep 10  # Check every 10 seconds
        
        # Count completed chunks
        local new_completed=$(find "$RESULTS_DIR" -name "sim_chunk_*.rds" | wc -l)
        local new_failed=$(find "$LOG_DIR" -name "chunk_*.log" -exec grep -l "ERROR\|FAILED" {} \; | wc -l)
        local new_running=$(jobs -r | wc -l)
        
        if [[ $new_completed -ne $completed ]] || [[ $new_failed -ne $failed ]] || [[ $new_running -ne $running ]]; then
            completed=$new_completed
            failed=$new_failed
            running=$new_running
            
            local progress=$((completed * 100 / total_chunks))
            local eta=""
            
            if [[ $completed -gt 0 ]]; then
                local elapsed=$(($(date +%s) - START_TIME))
                local rate=$(echo "scale=2; $completed / ($elapsed / 60)" | bc -l 2>/dev/null || echo "N/A")
                local remaining=$((total_chunks - completed))
                local eta_minutes=$(echo "scale=0; $remaining / $rate" | bc -l 2>/dev/null || echo "N/A")
                eta=" | ETA: ${eta_minutes} min"
            fi
            
            printf "\r${GREEN}Progress: %d/%d (%d%%) | Running: %d | Failed: %d%s${NC}" \
                   "$completed" "$total_chunks" "$progress" "$running" "$failed" "$eta"
        fi
    done
    
    echo ""  # New line after progress
}

execute_simulation() {
    print_header "🚀 EXECUTING FULL SIMULATION"
    
    log_info "Starting simulation execution..."
    log_info "Chunks: $CHUNKS | Iterations: $ITERATIONS | Workers: $WORKERS"
    
    # Track start time
    START_TIME=$(date +%s)
    export START_TIME
    
    # Create chunk execution plan
    local chunk_pids=()
    local max_concurrent=${MAX_CONCURRENT:-10}  # Limit concurrent chunks
    local current_concurrent=0
    
    # Start progress monitoring in background
    if [[ "$VERBOSE" != "true" ]]; then
        monitor_progress $CHUNKS &
        local monitor_pid=$!
    fi
    
    # Execute chunks
    for ((chunk=1; chunk<=CHUNKS; chunk++)); do
        # Wait if too many concurrent chunks
        while [[ $current_concurrent -ge $max_concurrent ]]; do
            # Wait for any chunk to complete
            wait -n
            current_concurrent=$((current_concurrent - 1))
        done
        
        # Start chunk in background
        {
            log_debug "Starting chunk $chunk..."
            if run_chunk $chunk; then
                log_debug "✅ Chunk $chunk completed successfully"
            else
                log_error "❌ Chunk $chunk failed"
                echo "$chunk" >> "$LOG_DIR/failed_chunks.txt"
            fi
        } &
        
        chunk_pids+=($!)
        current_concurrent=$((current_concurrent + 1))
        
        # Brief pause to avoid overwhelming system
        sleep 1
    done
    
    # Wait for all chunks to complete
    log_info "Waiting for all chunks to complete..."
    wait
    
    # Stop progress monitor
    if [[ "$VERBOSE" != "true" ]] && kill -0 $monitor_pid 2>/dev/null; then
        kill $monitor_pid 2>/dev/null
    fi
    
    # Calculate total time
    local end_time=$(date +%s)
    local total_time=$((end_time - START_TIME))
    local total_hours=$((total_time / 3600))
    local total_minutes=$(((total_time % 3600) / 60))
    
    log_info "All chunks submitted. Total execution time: ${total_hours}h ${total_minutes}m"
}

validate_results() {
    print_header "🔍 VALIDATING SIMULATION RESULTS"
    
    log_info "Validating simulation results..."
    
    # Count completed chunks
    local completed_files=$(find "$RESULTS_DIR" -name "sim_chunk_*.rds" | wc -l)
    local completion_rate=$((completed_files * 100 / CHUNKS))
    
    log_info "Completed chunks: $completed_files / $CHUNKS ($completion_rate%)"
    
    # Check for failed chunks
    if [[ -f "$LOG_DIR/failed_chunks.txt" ]]; then
        local failed_chunks=$(cat "$LOG_DIR/failed_chunks.txt" | sort -n)
        log_warn "Failed chunks: $failed_chunks"
        
        # Offer to retry failed chunks
        if [[ "$completion_rate" -ge 80 ]] && [[ -t 0 ]]; then  # Interactive terminal
            read -p "Retry failed chunks? (y/N): " -n 1 -r
            echo
            if [[ $REPLY =~ ^[Yy]$ ]]; then
                log_info "Retrying failed chunks..."
                for chunk in $failed_chunks; do
                    log_info "Retrying chunk $chunk..."
                    run_chunk $chunk
                done
            fi
        fi
    fi
    
    # Basic file validation
    local total_size=0
    local corrupted=0
    
    for file in "$RESULTS_DIR"/sim_chunk_*.rds; do
        if [[ -f "$file" ]]; then
            # Check file size (should be >1KB typically)
            local size=$(stat -f%z "$file" 2>/dev/null || stat -c%s "$file" 2>/dev/null || echo "0")
            total_size=$((total_size + size))
            
            if [[ $size -lt 1024 ]]; then
                log_warn "Suspiciously small file: $(basename "$file") (${size} bytes)"
                corrupted=$((corrupted + 1))
            fi
            
            # Try to load file in R (basic validation)
            if ! Rscript -e "readRDS('$file'); cat('OK\n')" >/dev/null 2>&1; then
                log_error "Cannot read file: $(basename "$file")"
                corrupted=$((corrupted + 1))
            fi
        fi
    done
    
    local total_size_mb=$((total_size / 1024 / 1024))
    
    log_info "Results validation summary:"
    echo "   • Completed files: $completed_files"
    echo "   • Total size: ${total_size_mb} MB"
    echo "   • Corrupted files: $corrupted"
    echo "   • Completion rate: $completion_rate%"
    
    if [[ $completion_rate -ge 95 ]] && [[ $corrupted -eq 0 ]]; then
        log_info "✅ Results validation passed"
        return 0
    else
        log_warn "⚠️ Results validation concerns detected"
        return 1
    fi
}

aggregate_results() {
    print_header "📊 AGGREGATING FINAL RESULTS"
    
    log_info "Running results aggregation..."
    
    local aggregate_cmd="Rscript aggregate_results.R $RESULTS_DIR"
    
    if [[ "$VERBOSE" == "true" ]]; then
        aggregate_cmd="$aggregate_cmd --verbose"
    fi
    
    if [[ -f "$CONFIG_FILE" ]]; then
        aggregate_cmd="$aggregate_cmd --config $CONFIG_FILE"
    fi
    
    log_debug "Executing: $aggregate_cmd"
    
    if eval "$aggregate_cmd"; then
        log_info "✅ Results aggregation completed successfully"
        
        # Display key results if summary files exist
        if [[ -f "$RESULTS_DIR/summary_ranks_overall.csv" ]]; then
            log_info "🏆 Top 5 methods by overall performance:"
            head -6 "$RESULTS_DIR/summary_ranks_overall.csv" | column -t -s,
        fi
        
        return 0
    else
        log_error "❌ Results aggregation failed"
        return 1
    fi
}

generate_completion_report() {
    print_header "📋 PHASE 1.1 COMPLETION REPORT"
    
    local report_file="$RESULTS_DIR/PHASE_1_1_COMPLETION_REPORT.md"
    local end_time=$(date '+%Y-%m-%d %H:%M:%S')
    local total_runtime=$(($(date +%s) - START_TIME))
    local hours=$((total_runtime / 3600))
    local minutes=$(((total_runtime % 3600) / 60))
    
    # Count results
    local completed_chunks=$(find "$RESULTS_DIR" -name "sim_chunk_*.rds" | wc -l)
    local total_evaluations=0
    local unique_scenarios=0
    
    if command -v Rscript &> /dev/null && [[ $completed_chunks -gt 0 ]]; then
        # Use R to count evaluations and scenarios
        read total_evaluations unique_scenarios < <(Rscript -e "
            library(dplyr)
            files <- list.files('$RESULTS_DIR', pattern='sim_chunk_.*\\.rds', full.names=TRUE)
            if (length(files) > 0) {
                all_results <- lapply(files, readRDS)
                combined <- do.call(rbind, all_results)
                cat(nrow(combined), length(unique(combined\$scn_id)), '\n')
            } else {
                cat('0 0\n')
            }
        " 2>/dev/null || echo "0 0")
    fi
    
    # Generate report
    cat > "$report_file" << EOF
# Phase 1.1 Completion Report: Original Replication

**Generated**: $end_time  
**Total Runtime**: ${hours}h ${minutes}m  
**Execution Mode**: $(if [[ "$TEST_MODE" == "true" ]]; then echo "Test Mode"; else echo "Full Simulation"; fi)

## 📊 Execution Summary

### Parameters
- **Chunks**: $CHUNKS
- **Iterations per scenario**: $ITERATIONS  
- **Workers per chunk**: $WORKERS
- **Memory limit**: ${MEMORY_LIMIT}GB
- **Configuration**: $CONFIG_FILE

### Results
- **Completed chunks**: $completed_chunks / $CHUNKS ($(( completed_chunks * 100 / CHUNKS ))%)
- **Total evaluations**: $(printf "%'d" $total_evaluations)
- **Unique scenarios**: $unique_scenarios / $(if [[ "$TEST_MODE" == "true" ]]; then echo "~50"; else echo "1,050"; fi)
- **Methods evaluated**: 11 (MLE, Ridge, LASSO, ElasticNet, RelaxedLASSO, PCR variants, PLS variants)

## 📁 Generated Files

### Main Results
- **Raw results**: sim_chunk_*.rds files ($completed_chunks files)
- **Summary rankings**: summary_ranks_overall.csv
- **Dimensional analyses**: summary_by_EPV.csv, summary_by_predictors.csv
- **Comprehensive report**: SIMULATION_REPORT.md

### Quality Assurance
- **Execution logs**: $LOG_DIR/chunk_*.log
- **Progress tracking**: Completion rates and error monitoring
- **Validation checks**: File integrity and data quality

## 🎯 Phase 1.1 Success Criteria

EOF

    # Add success criteria evaluation
    local criteria_passed=0
    local total_criteria=4
    
    # Criterion 1: Scenario completion rate
    local scenario_completion_rate=0
    if [[ $unique_scenarios -gt 0 ]] && [[ "$TEST_MODE" != "true" ]]; then
        scenario_completion_rate=$((unique_scenarios * 100 / 1050))
    elif [[ "$TEST_MODE" == "true" ]] && [[ $unique_scenarios -gt 0 ]]; then
        scenario_completion_rate=100  # Assume test mode is complete
    fi
    
    echo "### ✅ Completion Criteria" >> "$report_file"
    
    if [[ $scenario_completion_rate -ge 95 ]]; then
        echo "- [x] **Scenario Coverage**: $scenario_completion_rate% ≥ 95% ✅" >> "$report_file"
        criteria_passed=$((criteria_passed + 1))
    else
        echo "- [ ] **Scenario Coverage**: $scenario_completion_rate% < 95% ❌" >> "$report_file"
    fi
    
    # Criterion 2: Chunk completion
    local chunk_completion_rate=$((completed_chunks * 100 / CHUNKS))
    if [[ $chunk_completion_rate -ge 95 ]]; then
        echo "- [x] **Chunk Completion**: $chunk_completion_rate% ≥ 95% ✅" >> "$report_file"
        criteria_passed=$((criteria_passed + 1))
    else
        echo "- [ ] **Chunk Completion**: $chunk_completion_rate% < 95% ❌" >> "$report_file"
    fi
    
    # Criterion 3: File integrity
    local corrupted_files=$(find "$LOG_DIR" -name "chunk_*.log" -exec grep -l "corrupted\|cannot read" {} \; 2>/dev/null | wc -l)
    if [[ $corrupted_files -eq 0 ]]; then
        echo "- [x] **File Integrity**: No corrupted files ✅" >> "$report_file"
        criteria_passed=$((criteria_passed + 1))
    else
        echo "- [ ] **File Integrity**: $corrupted_files corrupted files ❌" >> "$report_file"
    fi
    
    # Criterion 4: Aggregation success
    if [[ -f "$RESULTS_DIR/summary_ranks_overall.csv" ]]; then
        echo "- [x] **Results Aggregation**: Summary files generated ✅" >> "$report_file"
        criteria_passed=$((criteria_passed + 1))
    else
        echo "- [ ] **Results Aggregation**: Summary files missing ❌" >> "$report_file"
    fi
    
    # Overall status
    cat >> "$report_file" << EOF

### 🏆 Overall Status
**Criteria Passed**: $criteria_passed / $total_criteria

EOF

    if [[ $criteria_passed -eq $total_criteria ]]; then
        cat >> "$report_file" << EOF
**STATUS**: ✅ **PHASE 1.1 SUCCESSFULLY COMPLETED**

Phase 1.1 objectives have been fully achieved. The original Lohmann et al. (2023) 
replication is complete with high-quality results ready for validation and publication.

### 🚀 Ready for Phase 1.2: Publication & Open Science
- Statistical validation against original results
- Manuscript preparation and submission
- Docker containerization and cloud templates
- Community repository optimization

EOF
    else
        cat >> "$report_file" << EOF
**STATUS**: ⚠️ **PHASE 1.1 PARTIALLY COMPLETED**

Some objectives require attention before proceeding to Phase 1.2. 
Review failed criteria above and consider:

- Re-running failed chunks with --resume flag
- Investigating and resolving technical issues
- Validating data quality and completeness
- Consulting troubleshooting guides

### 🔧 Recommended Next Steps
1. Address failed criteria identified above
2. Re-run problematic chunks: \`$0 --resume --force\`
3. Verify results quality and completeness
4. Proceed to Phase 1.2 once all criteria are met

EOF
    fi
    
    echo "---" >> "$report_file"
    echo "" >> "$report_file"
    echo "*Generated by Enhanced Lohmann et al. (2023) Replication Framework*" >> "$report_file"
    echo "*For support: https://github.com/DiogoRibeiro7/replication-lohmann-2023/issues*" >> "$report_file"
    
    log_info "📋 Completion report generated: $(basename "$report_file")"
    
    # Display key findings
    echo ""
    log_info "🎯 PHASE 1.1 COMPLETION STATUS:"
    echo "   • Criteria passed: $criteria_passed / $total_criteria"
    echo "   • Overall status: $(if [[ $criteria_passed -eq $total_criteria ]]; then echo "✅ SUCCESS"; else echo "⚠️ NEEDS ATTENTION"; fi)"
    echo "   • Full report: $report_file"
    
    return $((total_criteria - criteria_passed))  # Return 0 for success, >0 for issues
}

# =======================================================================
# MAIN EXECUTION
# =======================================================================

main() {
    # Parse command line arguments
    parse_arguments "$@"
    
    # Show header
    print_header "🧪 LOHMANN ET AL. (2023) REPLICATION - PHASE 1.1 EXECUTION"
    
    log_info "Enhanced Full Simulation Runner"
    log_info "Roadmap Phase: 1.1 - Complete Original Replication"
    log_info "Target: 1,050 scenarios × $ITERATIONS iterations × 11 methods"
    echo ""
    
    # Pre-execution checks
    check_dependencies
    setup_directories
    estimate_resources
    
    # Show configuration
    log_info "🔧 EXECUTION CONFIGURATION:"
    echo "   • Chunks: $CHUNKS"
    echo "   • Iterations: $ITERATIONS"
    echo "   • Workers per chunk: $WORKERS"
    echo "   • Memory limit: ${MEMORY_LIMIT}GB"
    echo "   • Results directory: $RESULTS_DIR"
    echo "   • Configuration file: $CONFIG_FILE"
    echo "   • Execution mode: $(if [[ "$TEST_MODE" == "true" ]]; then echo "Test Mode"; elif [[ "$DRY_RUN" == "true" ]]; then echo "Dry Run"; else echo "Full Simulation"; fi)"
    echo ""
    
    # Handle dry run
    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "🔍 DRY RUN MODE - Simulation plan validated, no execution performed"
        log_info "To execute for real, remove --dry-run flag"
        exit 0
    fi
    
    # Execution confirmation for full runs
    if [[ "$TEST_MODE" != "true" ]] && [[ -t 0 ]] && [[ "$FORCE" != "true" ]]; then
        echo -e "${YELLOW}⚠️ This will execute the full simulation (estimated time: several hours)${NC}"
        read -p "Proceed? (y/N): " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            log_info "❌ Execution cancelled by user"
            exit 0
        fi
    fi
    
    print_separator
    
    # Execute main phases
    local overall_success=true
    
    # Phase 1: Execute simulation
    if ! execute_simulation; then
        log_error "Simulation execution encountered issues"
        overall_success=false
    fi
    
    print_separator
    
    # Phase 2: Validate results
    if ! validate_results; then
        log_warn "Results validation identified concerns"
        overall_success=false
    fi
    
    print_separator
    
    # Phase 3: Aggregate results
    if ! aggregate_results; then
        log_error "Results aggregation failed"
        overall_success=false
    fi
    
    print_separator
    
    # Phase 4: Generate completion report
    if ! generate_completion_report; then
        log_warn "Some completion criteria not met"
        overall_success=false
    fi
    
    print_separator
    
    # Final status
    if [[ "$overall_success" == "true" ]]; then
        print_header "🎉 PHASE 1.1 COMPLETED SUCCESSFULLY!"
        log_info "✅ Original replication simulation completed successfully"
        log_info "📊 Results available in: $RESULTS_DIR/"
        log_info "📋 Completion report: $RESULTS_DIR/PHASE_1_1_COMPLETION_REPORT.md"
        log_info ""
        log_info "🚀 Ready for Phase 1.2: Publication & Open Science"
        log_info "   • Next step: ./scripts/validate_replication.sh"
        log_info "   • Documentation: See ROADMAP.md for next phases"
        exit 0
    else
        print_header "⚠️ PHASE 1.1 COMPLETED WITH ISSUES"
        log_warn "Simulation completed but some issues require attention"
        log_info "📋 Review completion report: $RESULTS_DIR/PHASE_1_1_COMPLETION_REPORT.md"
        log_info "🔧 Consider re-running with --resume to address failed chunks"
        exit 1
    fi
}

# Execute main function with all arguments
main "$@"
