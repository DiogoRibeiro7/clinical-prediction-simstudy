# 🚀 Phase 1.1 Execution Guide
## Complete Original Replication - Step by Step

> **Objective**: Execute the complete Lohmann et al. (2023) replication with 1,050 scenarios and validate results against the original study.

---

## 📋 Prerequisites

- ✅ Enhanced replication framework (your current repo)
- ✅ R 4.0+ installed with required packages
- ✅ 8GB+ RAM recommended 
- ✅ 2GB+ free disk space
- ✅ 4+ CPU cores for parallel processing

---

## 🛠️ Step 1: Environment Setup

### Option A: Automated Setup (Recommended)
```bash
# Make setup script executable
chmod +x scripts/setup_phase_1_1.sh

# Run complete setup
./scripts/setup_phase_1_1.sh --verbose

# For systems with Docker:
./scripts/setup_phase_1_1.sh --with-docker --verbose
```

### Option B: Manual Setup
```bash
# Create directory structure
mkdir -p {scripts,results,logs,plots,validation,config}

# Install R packages manually
Rscript -e "install.packages(c('MASS', 'glmnet', 'pROC', 'plsRglm', 'Matrix', 'dplyr', 'purrr', 'readr', 'future.apply', 'optparse'))"

# Copy execution scripts to scripts directory
chmod +x run_chunk.R aggregate_results.R
```

---

## 🧪 Step 2: Quick Validation Test

Before running the full simulation, validate your setup:

```bash
# Quick test with minimal scenarios (5-10 minutes)
./scripts/quick_test.sh

# Or manually:
./scripts/run_full_simulation.sh --test-mode --verbose --chunks 5 --iterations 2
```

**Expected output:**
- 5 chunks completed successfully
- Results files created in `results/`
- Summary generated with method rankings
- No critical errors in logs

---

## 🚀 Step 3: Full Simulation Execution

### Standard Execution (Recommended)
```bash
# Full simulation with optimal settings
./scripts/run_full_simulation.sh \
    --chunks 50 \
    --iterations 20 \
    --workers auto \
    --memory-limit 8
```

### High-Performance Execution
```bash
# For systems with 16+ cores and 16GB+ RAM
./scripts/run_full_simulation.sh \
    --chunks 25 \
    --iterations 20 \
    --workers 16 \
    --memory-limit 16
```

### Conservative Execution
```bash
# For systems with limited resources
./scripts/run_full_simulation.sh \
    --chunks 100 \
    --iterations 20 \
    --workers 2 \
    --memory-limit 4
```

### Execution Options
- `--chunks`: Number of parallel chunks (more chunks = smaller memory footprint)
- `--iterations`: Repetitions per scenario (20 matches original study)
- `--workers`: Parallel workers per chunk (auto-detects optimal)
- `--memory-limit`: Memory limit in GB per chunk
- `--resume`: Resume from interrupted execution
- `--force`: Overwrite existing results
- `--verbose`: Detailed output and progress

---

## 📊 Step 4: Monitor Progress

### Check Status
```bash
# Monitor execution progress
./scripts/check_status.sh

# View real-time logs (in separate terminal)
tail -f logs/chunk_*.log
```

### Expected Timeline
| System Spec | Estimated Time |
|-------------|----------------|
| 4 cores, 8GB RAM | 8-12 hours |
| 8 cores, 16GB RAM | 4-6 hours |
| 16 cores, 32GB RAM | 2-3 hours |

### Progress Indicators
- **Chunk completion**: 50/50 chunks with .rds files
- **Log files**: No critical errors in `logs/`
- **Results size**: ~1-2GB total in `results/`
- **Aggregation**: `summary_ranks_overall.csv` created

---

## 🔍 Step 5: Results Validation

### Automatic Validation
```bash
# Validate against original study findings
./scripts/validate_replication.sh --verbose

# This will:
# - Compare method rankings
# - Verify performance ranges  
# - Generate validation plots
# - Create validation report
```

### Manual Validation
```bash
# Check key results manually
head -10 results/summary_ranks_overall.csv

# Verify expected top methods:
# 1. Ridge, LASSO, ElasticNet should rank highly
# 2. Penalization > Variance decomposition overall
# 3. AUC range: 0.55-0.85 typically
# 4. Brier range: 0.15-0.35 typically
```

---

## 🎯 Success Criteria

Phase 1.1 is **successfully completed** when:

- [x] **Scenario Coverage**: ≥95% of 1,050 scenarios completed
- [x] **Chunk Success**: ≥95% of chunks executed without errors  
- [x] **Data Quality**: <5% missing values in key metrics
- [x] **Replication Validity**: Method rankings correlate >0.8 with original
- [x] **File Generation**: All summary files and reports created
- [x] **Statistical Validation**: Performance ranges within expected bounds

---

## 🛠️ Troubleshooting

### Common Issues

**Issue**: `Error: package 'glmnet' is not available`
```bash
# Solution: Install missing packages
Rscript -e "install.packages('glmnet', dependencies=TRUE)"
```

**Issue**: `Memory allocation error`
```bash
# Solution: Reduce memory usage
./scripts/run_full_simulation.sh --chunks 100 --workers 2 --memory-limit 4
```

**Issue**: `Permission denied: scripts/run_full_simulation.sh`
```bash
# Solution: Make scripts executable
chmod +x scripts/*.sh
```

**Issue**: Failed chunks detected
```bash
# Solution: Resume execution
./scripts/run_full_simulation.sh --resume --force

# Or check specific failed chunks
cat logs/failed_chunks.txt
```

### Performance Issues

**Slow execution**:
- Increase `--workers` if you have more CPU cores
- Decrease `--chunks` to use more parallel workers
- Check system resources with `htop` or `top`

**High memory usage**:
- Increase `--chunks` to reduce memory per chunk
- Decrease `--workers` to reduce concurrent memory usage
- Close other applications during execution

**Disk space issues**:
- Each chunk generates ~20-50MB of results
- Total space needed: ~2-3GB for full simulation
- Clean up with `./scripts/cleanup.sh` if needed

### Getting Help

1. **Check logs**: `less logs/chunk_001.log`
2. **Review configuration**: `cat config/simulation_config.yaml`  
3. **Validate setup**: `./scripts/setup_phase_1_1.sh --verbose`
4. **GitHub Issues**: [Report problems](https://github.com/DiogoRibeiro7/replication-lohmann-2023/issues)

---

## 📊 Understanding Results

### Key Output Files

#### `results/summary_ranks_overall.csv`
```csv
method,mean_rank_auc,mean_rank_brier,mean_auc,mean_brier,n_evaluations
Ridge,2.45,2.67,0.724,0.186,21000
LASSO,2.89,2.34,0.718,0.188,21000
ElasticNet,3.12,3.01,0.715,0.189,21000
```

#### `results/SIMULATION_REPORT.md`
- Executive summary of findings
- Top performing methods
- Performance by EPV and dimensionality
- Quality assurance metrics
- Comparison with original study

#### `validation/VALIDATION_REPORT.md`
- Statistical validation results
- Correlation with original findings
- Method ranking agreement
- Performance range verification

### Expected Findings

Based on Lohmann et al. (2023), you should observe:

1. **Method Rankings**: Ridge and LASSO typically in top 3
2. **Family Performance**: Penalization methods outperform variance decomposition
3. **EPV Effects**: Performance gaps increase with higher EPV
4. **Dimensionality**: Penalization advantages grow with more predictors

---

## 🎉 Completion Actions

### When Phase 1.1 Succeeds

```bash
# 1. Generate completion report
cat results/PHASE_1_1_COMPLETION_REPORT.md

# 2. Archive results (optional)
tar -czf phase_1_1_results_$(date +%Y%m%d).tar.gz results/ validation/ plots/

# 3. Prepare for Phase 1.2
echo "✅ Phase 1.1 completed successfully!"
echo "🚀 Ready for Phase 1.2: Publication & Open Science"
```

### Next Steps to Phase 1.2

1. **Manuscript Preparation**:
   - Draft replication paper
   - Compare findings with original
   - Highlight methodological improvements

2. **Open Science Package**:
   - Create Docker containers
   - Set up cloud execution templates
   - Prepare data/code archive

3. **Community Engagement**:
   - Submit preprint to bioRxiv/medRxiv
   - Present at conferences
   - Engage with original authors

---

## 🔄 Alternative Execution Methods

### Docker Execution (Recommended for Reproducibility)
```bash
# Build container
docker-compose build

# Run full simulation in container
docker-compose up

# Run test in container
docker-compose run test
```

### Cloud Execution (For Large-Scale Runs)
```bash
# AWS Batch (if configured)
aws batch submit-job --job-name lohmann-replication \
  --job-queue simulation-queue \
  --job-definition lohmann-job-def

# Google Cloud (if configured)
gcloud batch jobs submit lohmann-replication \
  --location=us-central1 \
  --config=batch-config.yaml
```

### SLURM/HPC Execution
```bash
# Create SLURM job script
cat > submit_simulation.slurm << 'EOF'
#!/bin/bash
#SBATCH --job-name=lohmann_replication
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=16
#SBATCH --time=12:00:00
#SBATCH --mem=32G

module load R/4.3.2
./scripts/run_full_simulation.sh --chunks 50 --workers 16
EOF

# Submit job
sbatch submit_simulation.slurm
```

---

## 📈 Performance Optimization Tips

### System Optimization
```bash
# Increase R memory limit
export R_MAX_VSIZE=32Gb

# Disable R graphics for faster execution
export R_DEFAULT_DEVICE=png

# Use faster linear algebra libraries (if available)
export R_LIBS_USER=/path/to/optimized/libs
```

### Configuration Tuning
```yaml
# config/simulation_config.yaml optimizations
parallel:
  default_workers: 16  # Match your CPU cores
  chunk_size: 25      # Fewer chunks for more parallelism
  memory_limit_gb: 16  # Match your available RAM

simulation:
  cv_folds: 5         # Reduce from 10 if needed for speed
```

### Resource Monitoring
```bash
# Monitor system resources during execution
watch -n 30 'free -h && df -h . && ps aux | head -20'

# Monitor R processes specifically
watch -n 10 'ps aux | grep -E "(Rscript|R CMD)" | head -10'

# Check network I/O if using networked storage
iostat -x 1
```

---

## 🎯 Phase 1.1 Checklist

### Pre-Execution
- [ ] System meets minimum requirements (8GB RAM, 4+ cores)
- [ ] All R packages installed and tested
- [ ] Framework validation passed
- [ ] Configuration file reviewed and optimized
- [ ] Quick test run completed successfully

### During Execution  
- [ ] All 50 chunks started successfully
- [ ] Progress monitoring shows steady completion
- [ ] No critical errors in log files
- [ ] System resources within acceptable limits
- [ ] Results files being generated in `results/`

### Post-Execution
- [ ] ≥95% scenario completion rate achieved
- [ ] Results aggregation completed without errors
- [ ] Summary files generated (`summary_ranks_overall.csv`, etc.)
- [ ] Statistical validation shows strong correlation with original
- [ ] Completion report indicates Phase 1.1 success
- [ ] All deliverables ready for Phase 1.2

### Quality Assurance
- [ ] Method rankings align with original study expectations
- [ ] Performance ranges within clinical bounds (AUC: 0.55-0.85)
- [ ] Missing value rates <5% for key metrics
- [ ] No corrupted or suspicious result files
- [ ] Validation plots show reasonable method distributions

---

## 🏁 Success Confirmation

Your Phase 1.1 execution is **successful** when you see:

```
🎉 PHASE 1.1 COMPLETED SUCCESSFULLY!
✅ Original replication simulation completed successfully
📊 Results available in: results/
📋 Completion report: results/PHASE_1_1_COMPLETION_REPORT.md

🚀 Ready for Phase 1.2: Publication & Open Science
   • Next step: ./scripts/validate_replication.sh
   • Documentation: See ROADMAP.md for next phases
```

**Congratulations!** You've successfully completed the most computationally intensive part of the project. Your enhanced framework has processed over 230,000 model evaluations across 1,050 scenarios, validating and extending the original Lohmann et al. (2023) findings.

---

## 📞 Support and Community

- **Issues**: [GitHub Issues](https://github.com/DiogoRibeiro7/replication-lohmann-2023/issues)
- **Discussions**: [GitHub Discussions](https://github.com/DiogoRibeiro7/replication-lohmann-2023/discussions)  
- **Documentation**: Check README.md and ROADMAP.md
- **Original Paper**: [Lohmann et al. (2023) Biometrical Journal](https://doi.org/10.1002/bimj.202200108)

---

*This guide is part of the Enhanced Lohmann et al. (2023) Replication Project*  
*Phase 1.1: Complete Original Replication*  
*For the latest version, see: [Project Repository](https://github.com/DiogoRibeiro7/clinical-prediction-simstudy)*
