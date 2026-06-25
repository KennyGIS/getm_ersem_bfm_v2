# pre_run_check.sh

`pre_run_check.sh` is a pre-submit readiness check for the live GETM-BFM
monthly workflow. It is meant to catch common setup problems before spending
hours on a model month.

The script reads the selected `run_all` script and `run.getm`. It does not
execute the model, submit Slurm jobs, or modify the live `CO2.nml`.

## Where To Put It

Upload `pre_run_check.sh` into the active HPC run directory, next to:

```text
run_all_co2_opt
run.getm
co2_data_find.sh
CO2.nml
par_setup.dat
```

Current active directory:

```text
/export/lv9/user/klarsen/version_1/home/GETM_ERSEM_SETUPS/nwes/
```

## Basic Use

From the active `nwes` run directory:

```bash
bash pre_run_check.sh
```

By default this checks:

```text
run_all_co2_opt
run.getm
```

## Choose A Different Run Script

Use `--run-script` if you renamed or copied the run-all script:

```bash
bash pre_run_check.sh --run-script run_all_co2_opt
bash pre_run_check.sh --run-script run_all_co2_test
```

Use `--getm-script` if the GETM runner has a different name:

```bash
bash pre_run_check.sh --run-script run_all_co2_opt --getm-script run.getm
```

Short options are also supported:

```bash
bash pre_run_check.sh -r run_all_co2_opt -g run.getm
```

Show help:

```bash
bash pre_run_check.sh --help
```

## What It Checks And Why

### Script Syntax

Checks:

- `run_all_co2_opt`
- `run.getm`
- `co2_data_find.sh`

Why:

This catches shell syntax errors before a Slurm allocation starts.

### Run Window

Reads from the selected run script:

- `YEAR_START`
- `YEAR_STOP`
- `MONTHS`
- `BEGIN_MONTH`
- `CONF`

Why:

These decide which model months run and which restart directory is required.

### Model Paths

Reads and checks paths from the selected run script:

- `VERSION_ROOT`
- `DOMAIN_DIR`
- `GETMDIR_WRAPPER`
- `DIR_BDY2D`
- `out/<CONF>`

Why:

The model needs these paths to find the domain setup, GETM wrapper, boundary
files, and writable output tree.

### Source Symlinks

Checks selected BFM source links under:

```text
<VERSION_ROOT>/home/BFM_SOURCES/bfm_2016/src/BFM/General
```

Expected links:

- `ControlNutsBdy.F90 -> ControlNutsBdy.F90.johan`
- `GlobalDefsBFM.model -> GlobalDefsBFM.model.orig`

Why:

These legacy source selectors can be damaged when a tree is copied or packaged
without preserving symlinks. If they become tiny regular files containing only
the target filename, BFM code generation can fail during compilation.

### CO2 Setup

Checks:

- `co2_data_find.sh` is called by the run script.
- `co2_input/co2_monthly_bfm.dat` exists.
- `CO2.nml` exists.
- CO2 lookup works for each configured month.

Why:

This catches the August/September month parsing issue and missing CO2 data
before the model starts. The check runs in a temporary directory, so it does not
change the live `CO2.nml`.

### Optimized Postprocessing

Reads from the selected run script:

- `POSTPROCESS_DIR`
- `POSTPROCESS_SCRIPT`
- `POSTPROCESS_ROOT`
- `POSTPROCESS_SCRATCH`

Checks:

- postprocess sbatch exists and parses as shell
- 4 CPU / 24 GB request is present
- no exclusive-node request is present
- required postprocessing scripts/config files exist
- `run.getm` submits postprocessing with a Slurm dependency

Why:

This verifies that postprocessing can run as a small dependent job after
`move_files`, without blocking the model workflow or reserving a whole node.

### Restart Inputs

Checks:

- `par_setup.dat`
- `out/<CONF>/<YEAR_START>/<BEGIN_MONTH>/restart.????.in`
- restart file count matches the rank count in `par_setup.dat`
- restart range starts at `restart.0000.in` and ends at the expected final rank

Why:

The model cannot resume if the first requested month is missing hotstart files.

## Interpreting Output

Successful checks print:

```text
OK: ...
```

Non-fatal warnings print:

```text
WARN: ...
```

Required failures print:

```text
FAIL: ...
```

If any required check fails, the script exits nonzero and the run should not be
submitted until the issue is fixed.

## Typical Command Before Submitting

```bash
cd /export/lv9/user/klarsen/version_1/home/GETM_ERSEM_SETUPS/nwes
bash pre_run_check.sh --run-script run_all_co2_opt
```

For the current 2015 resume, the selected run script should report:

```text
YEAR_START=2015
YEAR_STOP=2015
MONTHS="08 09 10"
BEGIN_MONTH="08"
```

The restart check expects the rank count from `par_setup.dat`. With
`par_setup.dat` reporting 318 ranks, the expected restart range is
`restart.0000.in` through `restart.0317.in`; `restart.0318.in` is not required.

If it ends with:

```text
OK: pre-run checks passed
```

then submit:

```bash
sbatch run_all_co2_opt
```
