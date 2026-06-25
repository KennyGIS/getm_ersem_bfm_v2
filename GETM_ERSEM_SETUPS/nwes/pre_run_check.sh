#!/usr/bin/env bash
set -euo pipefail

RUN_SCRIPT=run_all_co2_opt
GETM_SCRIPT=run.getm

usage() {
  cat <<'EOF'
Usage:
  pre_run_check.sh [--run-script RUN_ALL_SCRIPT] [--getm-script RUN_GETM_SCRIPT]

Defaults:
  RUN_ALL_SCRIPT=run_all_co2_opt
  RUN_GETM_SCRIPT=run.getm

Examples:
  bash pre_run_check.sh
  bash pre_run_check.sh --run-script run_all_co2_opt --getm-script run.getm
  bash pre_run_check.sh --run-script run_all_co2_test

The script reads the run scripts to check paths/settings. It does not execute
run_all or start the model.
EOF
}

while [ $# -gt 0 ]; do
  case "$1" in
    --run-script|-r)
      if [ $# -lt 2 ]; then
        echo "ERROR: --run-script requires a value" >&2
        exit 2
      fi
      RUN_SCRIPT=$2
      shift 2
      ;;
    --getm-script|-g)
      if [ $# -lt 2 ]; then
        echo "ERROR: --getm-script requires a value" >&2
        exit 2
      fi
      GETM_SCRIPT=$2
      shift 2
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      echo "ERROR: unknown argument: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

failures=0

pass() {
  printf "OK: %s\n" "$*"
}

warn() {
  printf "WARN: %s\n" "$*" >&2
}

fail() {
  printf "FAIL: %s\n" "$*" >&2
  failures=$((failures + 1))
}

section() {
  printf "\n== %s ==\n" "$*"
}

read_assignment() {
  local name=$1
  local file=$2
  awk -F= -v key="$name" '
    $1 == key {
      val=$0
      sub("^[^=]*=", "", val)
      gsub(/^[[:space:]]+|[[:space:]]+$/, "", val)
      gsub(/^"|"$/, "", val)
      print val
      exit
    }
  ' "$file"
}

resolve_shell_value() {
  local raw=$1
  local version_root=${2:-}
  local domain_name=${3:-}
  local domain_dir=${4:-}
  local getmdir_wrapper=${5:-}
  local postprocess_dir=${6:-}

  (
    VERSION_ROOT=$version_root \
    DOMAIN_NAME=$domain_name \
    DOMAIN_DIR=$domain_dir \
    GETMDIR_WRAPPER=$getmdir_wrapper \
    POSTPROCESS_DIR=$postprocess_dir \
    eval "printf '%s\n' \"$raw\""
  )
}

expand_postprocess_script() {
  local script_value=$1
  local postprocess_dir=$2
  resolve_shell_value "$script_value" "" "" "" "" "$postprocess_dir"
}

expand_known_path() {
  local value=$1
  local version_root=${2:-}
  local domain_name=${3:-}
  local domain_dir=${4:-}
  local getmdir_wrapper=${5:-}

  resolve_shell_value "$value" "$version_root" "$domain_name" "$domain_dir" "$getmdir_wrapper" ""
}

check_existing_dir() {
  local label=$1
  local path=$2
  if [ -d "$path" ]; then
    pass "$label exists: $path"
    if [ -x "$path" ]; then
      pass "$label is searchable"
    else
      fail "$label is not searchable: $path"
    fi
  else
    fail "$label does not exist: $path"
  fi
}

check_existing_file() {
  local label=$1
  local path=$2
  if [ -f "$path" ]; then
    pass "$label exists: $path"
  else
    fail "$label does not exist: $path"
  fi
}

check_symlink_target() {
  local label=$1
  local link_path=$2
  local expected_target=$3
  local actual_target
  local link_dir

  printf "%s expected symlink: %s -> %s\n" "$label" "$link_path" "$expected_target"

  if [ -L "$link_path" ]; then
    actual_target=$(readlink "$link_path")
    if [ "$actual_target" = "$expected_target" ]; then
      pass "$label symlink target is correct: $actual_target"
    else
      fail "$label symlink target is $actual_target, expected $expected_target"
    fi

    link_dir=$(dirname "$link_path")
    if [ -e "$link_dir/$actual_target" ]; then
      pass "$label symlink target exists: $link_dir/$actual_target"
    else
      fail "$label symlink target is missing: $link_dir/$actual_target"
    fi
  elif [ -e "$link_path" ]; then
    fail "$label is not a symlink: $link_path"
    if [ -f "$link_path" ]; then
      actual_target=$(cat "$link_path" 2>/dev/null || true)
      if [ "$actual_target" = "$expected_target" ]; then
        fail "$label looks like a broken copied symlink text file; recreate it with ln -s"
      fi
    fi
  else
    fail "$label symlink is missing: $link_path"
  fi
}

check_writable_target() {
  local label=$1
  local path=$2
  local parent
  if [ -e "$path" ]; then
    if [ -w "$path" ]; then
      pass "$label is writable: $path"
    else
      fail "$label exists but is not writable: $path"
    fi
  else
    parent=$(dirname "$path")
    if [ -d "$parent" ] && [ -w "$parent" ]; then
      pass "$label can be created; parent is writable: $parent"
    else
      fail "$label cannot be created; parent missing or not writable: $parent"
    fi
  fi
}

section "Script Syntax"

if [ -f "$RUN_SCRIPT" ]; then
  if bash -n "$RUN_SCRIPT"; then
    pass "$RUN_SCRIPT parses as shell"
  else
    fail "$RUN_SCRIPT has shell syntax errors"
  fi
else
  fail "run script not found: $RUN_SCRIPT"
fi

if [ -f "$GETM_SCRIPT" ]; then
  if bash -n "$GETM_SCRIPT"; then
    pass "$GETM_SCRIPT parses as shell"
  else
    fail "$GETM_SCRIPT has shell syntax errors"
  fi
else
  fail "GETM script not found: $GETM_SCRIPT"
fi

if [ -f co2_data_find.sh ]; then
  if bash -n co2_data_find.sh; then
    pass "co2_data_find.sh parses as shell"
  else
    fail "co2_data_find.sh has shell syntax errors"
  fi
else
  fail "co2_data_find.sh not found in current directory"
fi

section "Run Window"

year_start=$(read_assignment YEAR_START "$RUN_SCRIPT")
year_stop=$(read_assignment YEAR_STOP "$RUN_SCRIPT")
months=$(read_assignment MONTHS "$RUN_SCRIPT")
begin_month=$(read_assignment BEGIN_MONTH "$RUN_SCRIPT")
conf=$(read_assignment CONF "$RUN_SCRIPT")

printf "YEAR_START=%s\n" "${year_start:-MISSING}"
printf "YEAR_STOP=%s\n" "${year_stop:-MISSING}"
printf "MONTHS=%s\n" "${months:-MISSING}"
printf "BEGIN_MONTH=%s\n" "${begin_month:-MISSING}"
printf "CONF=%s\n" "${conf:-MISSING}"

[ -n "$year_start" ] || fail "YEAR_START is missing"
[ -n "$year_stop" ] || fail "YEAR_STOP is missing"
[ -n "$months" ] || fail "MONTHS is missing"
[ -n "$begin_month" ] || fail "BEGIN_MONTH is missing"
[ -n "$conf" ] || fail "CONF is missing"

section "Model Paths From Run Script"

version_root_raw=$(read_assignment VERSION_ROOT "$RUN_SCRIPT")
domain_name_raw=$(read_assignment DOMAIN_NAME "$RUN_SCRIPT")
domain_dir_raw=$(read_assignment DOMAIN_DIR "$RUN_SCRIPT")
getmdir_wrapper_raw=$(read_assignment GETMDIR_WRAPPER "$RUN_SCRIPT")
dir_bdy2d_raw=$(read_assignment DIR_BDY2D "$RUN_SCRIPT")
runid=$(read_assignment RUNID "$RUN_SCRIPT")
nprocesses=$(read_assignment NPROCESSES "$RUN_SCRIPT")

version_root=$(expand_known_path "$version_root_raw")
domain_name=$(expand_known_path "$domain_name_raw" "$version_root")
domain_dir=$(expand_known_path "$domain_dir_raw" "$version_root" "$domain_name")
getmdir_wrapper=$(expand_known_path "$getmdir_wrapper_raw" "$version_root" "$domain_name" "$domain_dir")
dir_bdy2d=$(expand_known_path "$dir_bdy2d_raw" "$version_root" "$domain_name" "$domain_dir" "$getmdir_wrapper")

printf "VERSION_ROOT=%s\n" "${version_root:-MISSING}"
printf "DOMAIN_NAME=%s\n" "${domain_name:-MISSING}"
printf "DOMAIN_DIR=%s\n" "${domain_dir:-MISSING}"
printf "GETMDIR_WRAPPER=%s\n" "${getmdir_wrapper:-MISSING}"
printf "DIR_BDY2D=%s\n" "${dir_bdy2d:-MISSING}"
printf "RUNID=%s\n" "${runid:-MISSING}"
printf "NPROCESSES=%s\n" "${nprocesses:-MISSING}"

[ -n "$version_root" ] && check_existing_dir "VERSION_ROOT" "$version_root"
[ -n "$domain_dir" ] && check_existing_dir "DOMAIN_DIR" "$domain_dir"
[ -n "$getmdir_wrapper" ] && check_existing_dir "GETMDIR_WRAPPER" "$getmdir_wrapper"
[ -n "$dir_bdy2d" ] && check_existing_dir "DIR_BDY2D" "$dir_bdy2d"

if [ -n "$domain_dir" ]; then
  current_dir=$(pwd -P)
  domain_real=$(cd "$domain_dir" 2>/dev/null && pwd -P || printf "%s" "$domain_dir")
  if [ "$current_dir" = "$domain_real" ]; then
    pass "current directory matches DOMAIN_DIR"
  else
    warn "current directory does not match DOMAIN_DIR; current=$current_dir DOMAIN_DIR=$domain_real"
  fi
fi

if [ -n "$dir_bdy2d" ]; then
  check_existing_dir "2D boundary directory" "$dir_bdy2d/boundary"
fi

if [ -n "$year_start" ] && [ -n "$dir_bdy2d" ]; then
  if [ "$year_start" -ge 2014 ] && [ "$year_start" -lt 2022 ]; then
    check_existing_file "2D tide boundary file for 2014-2022 window" \
      "$dir_bdy2d/boundary/tides.2014-01-01_2022-01-010.4.nc"
  elif [ "$year_start" -ge 2022 ] && [ "$year_start" -lt 2030 ]; then
    check_existing_file "2D tide boundary file for 2022-2030 window" \
      "$dir_bdy2d/boundary/tides.2022-01-01_2030-01-010.4.nc"
  else
    warn "boundary tide file check is not implemented for YEAR_START=$year_start"
  fi
fi

check_existing_file "run.getm in current directory" "$GETM_SCRIPT"
check_existing_file "co2_data_find.sh in current directory" "co2_data_find.sh"
check_existing_file "CO2.nml in current directory" "CO2.nml"
check_existing_file "output.yaml in current directory" "output.yaml"
check_existing_file "par_setup.dat in current directory" "par_setup.dat"

section "Source Symlinks"

if [ -n "$version_root" ]; then
  bfm_general="$version_root/home/BFM_SOURCES/bfm_2016/src/BFM/General"
  printf "BFM General source directory=%s\n" "$bfm_general"

  if [ -d "$bfm_general" ]; then
    check_symlink_target \
      "BFM nutrient boundary source selector" \
      "$bfm_general/ControlNutsBdy.F90" \
      "ControlNutsBdy.F90.johan"

    check_symlink_target \
      "BFM global definition selector" \
      "$bfm_general/GlobalDefsBFM.model" \
      "GlobalDefsBFM.model.orig"
  else
    warn "BFM General source directory not found; skipping source symlink checks: $bfm_general"
  fi
else
  warn "VERSION_ROOT is missing; skipping source symlink checks"
fi

out_root="out/$conf"
check_writable_target "model output root" "$out_root"

section "CO2 Update Hook"

if grep -q './co2_data_find.sh || exit 1' "$RUN_SCRIPT"; then
  pass "$RUN_SCRIPT calls co2_data_find.sh and exits if CO2 lookup fails"
else
  fail "$RUN_SCRIPT does not contain './co2_data_find.sh || exit 1'"
fi

if grep -q 'grep pCO2_air CO2.nml' "$RUN_SCRIPT"; then
  pass "$RUN_SCRIPT prints pCO2_air after updating CO2.nml"
else
  warn "$RUN_SCRIPT does not print pCO2_air after CO2 update"
fi

section "CO2 Lookup"

if [ -f co2_input/co2_monthly_bfm.dat ]; then
  pass "co2_input/co2_monthly_bfm.dat exists"
else
  fail "co2_input/co2_monthly_bfm.dat is missing"
fi

if [ -f CO2.nml ]; then
  pass "CO2.nml exists"
else
  fail "CO2.nml is missing"
fi

if [ -f co2_data_find.sh ] && [ -f CO2.nml ] && [ -f co2_input/co2_monthly_bfm.dat ] && [ -n "$year_start" ] && [ -n "$months" ]; then
  tmpdir=$(mktemp -d)
  trap 'rm -rf "$tmpdir"' EXIT
  cp co2_data_find.sh "$tmpdir/"
  cp CO2.nml "$tmpdir/"
  mkdir -p "$tmpdir/co2_input"
  cp co2_input/co2_monthly_bfm.dat "$tmpdir/co2_input/"

  for month in $months; do
    if (cd "$tmpdir" && YEAR_START="$year_start" MONTHS="$month" ./co2_data_find.sh >/tmp/pre_run_co2_check.out 2>/tmp/pre_run_co2_check.err); then
      value=$(grep 'pCO2_air[[:space:]]*=' "$tmpdir/CO2.nml" | tail -n 1 | tr -d '[:space:]')
      pass "CO2 lookup works for YEAR_START=$year_start MONTHS=$month ($value)"
    else
      fail "CO2 lookup failed for YEAR_START=$year_start MONTHS=$month: $(cat /tmp/pre_run_co2_check.err)"
    fi
  done
fi

section "Postprocessing Hook"

postprocess_dir=$(read_assignment POSTPROCESS_DIR "$RUN_SCRIPT")
postprocess_root_raw=$(read_assignment POSTPROCESS_ROOT "$RUN_SCRIPT")
postprocess_scratch_raw=$(read_assignment POSTPROCESS_SCRATCH "$RUN_SCRIPT")
postprocess_script_value=$(read_assignment POSTPROCESS_SCRIPT "$RUN_SCRIPT")
postprocess_script=$(expand_postprocess_script "$postprocess_script_value" "$postprocess_dir")
postprocess_dir=$(resolve_shell_value "$postprocess_dir" "$version_root" "$domain_name" "$domain_dir" "$getmdir_wrapper" "")
postprocess_root=$(resolve_shell_value "$postprocess_root_raw" "$version_root" "$domain_name" "$domain_dir" "$getmdir_wrapper" "$postprocess_dir")
postprocess_scratch=$(resolve_shell_value "$postprocess_scratch_raw" "$version_root" "$domain_name" "$domain_dir" "$getmdir_wrapper" "$postprocess_dir")

printf "POSTPROCESS_DIR=%s\n" "${postprocess_dir:-MISSING}"
printf "POSTPROCESS_SCRIPT=%s\n" "${postprocess_script:-MISSING}"
printf "POSTPROCESS_ROOT=%s\n" "${postprocess_root:-MISSING}"
printf "POSTPROCESS_SCRATCH=%s\n" "${postprocess_scratch:-MISSING}"

if [ -n "$postprocess_script" ] && [ -f "$postprocess_script" ]; then
  pass "postprocess sbatch exists: $postprocess_script"
else
  fail "postprocess sbatch not found: ${postprocess_script:-MISSING}"
fi

if [ -f "$postprocess_script" ]; then
  if bash -n "$postprocess_script"; then
    pass "postprocess sbatch parses as shell"
  else
    fail "postprocess sbatch has shell syntax errors"
  fi

  if grep -q -- '--cpus-per-task=4' "$postprocess_script"; then
    pass "postprocess requests 4 CPUs"
  else
    warn "postprocess sbatch does not show --cpus-per-task=4"
  fi

  if grep -q -- '--mem=24G' "$postprocess_script"; then
    pass "postprocess requests 24G memory"
  else
    warn "postprocess sbatch does not show --mem=24G"
  fi

  if grep -q -- '--exclusive' "$postprocess_script"; then
    fail "postprocess sbatch requests exclusive node access"
  else
    pass "postprocess sbatch does not request exclusive node access"
  fi

  if grep -q 'finalize_month.sh' "$postprocess_script"; then
    pass "postprocess sbatch calls finalize_month.sh"
  else
    fail "postprocess sbatch does not call finalize_month.sh"
  fi

  postprocess_python=$(awk -F= '/^[[:space:]]*export POSTPROCESS_PYTHON=/ {
    val=$0
    sub("^[^=]*=", "", val)
    gsub(/^"|"$/, "", val)
    print val
    exit
  }' "$postprocess_script")
  if [ -n "$postprocess_python" ]; then
    printf "POSTPROCESS_PYTHON=%s\n" "$postprocess_python"
    if [ -x "$postprocess_python" ]; then
      pass "postprocess Python exists and is executable"
    else
      fail "postprocess Python is missing or not executable: $postprocess_python"
    fi
  else
    warn "POSTPROCESS_PYTHON was not found in postprocess sbatch"
  fi
fi

if [ -n "$postprocess_dir" ]; then
  if [ -d "$postprocess_dir" ]; then
    pass "POSTPROCESS_DIR exists: $postprocess_dir"
  else
    fail "POSTPROCESS_DIR does not exist: $postprocess_dir"
  fi

  for required in \
    postprocess_outputs.inp \
    finalize_month.sh \
    process_one_month.py \
    append_month_to_archive.py \
    postprocess_config_loader.py \
    postprocess_single_variable.py
  do
    if [ -f "$postprocess_dir/$required" ]; then
      pass "postprocess required file exists: $required"
    else
      fail "postprocess required file missing: $postprocess_dir/$required"
    fi
  done

  for shell_script in finalize_month.sh postprocess_month.sbatch; do
    if [ -f "$postprocess_dir/$shell_script" ]; then
      if bash -n "$postprocess_dir/$shell_script"; then
        pass "$shell_script parses as shell"
      else
        fail "$shell_script has shell syntax errors"
      fi
    fi
  done

  if [ -s "$postprocess_dir/postprocess_outputs.inp" ]; then
    pass "postprocess_outputs.inp is non-empty"
  else
    fail "postprocess_outputs.inp is missing or empty"
  fi
fi

[ -n "$postprocess_root" ] && check_writable_target "POSTPROCESS_ROOT" "$postprocess_root"
[ -n "$postprocess_scratch" ] && check_writable_target "POSTPROCESS_SCRATCH" "$postprocess_scratch"

if grep -q -- '--dependency="$postprocess_dependency"' "$GETM_SCRIPT"; then
  pass "$GETM_SCRIPT submits postprocessing with a Slurm dependency"
else
  fail "$GETM_SCRIPT does not show dependent postprocessing submission"
fi

section "Restart Inputs"

if [ -f par_setup.dat ]; then
  par_setup_ref=$(head -n 1 par_setup.dat | awk '{print $1}')
  if [ -n "$par_setup_ref" ] && [ -f "$par_setup_ref" ]; then
    ranks=$(head -n 1 "$par_setup_ref" | awk '{print $1}')
    pass "par_setup.dat found; subdomain spec=$par_setup_ref ranks=$ranks"
  elif printf '%s\n' "$par_setup_ref" | grep -Eq '^[0-9]+$'; then
    ranks=$par_setup_ref
    pass "par_setup.dat found; ranks=$ranks"
  else
    ranks=""
    fail "par_setup.dat found but did not resolve to a numeric rank count: $par_setup_ref"
  fi
else
  fail "par_setup.dat not found"
  ranks=""
fi

if [ -n "$year_start" ] && [ -n "$begin_month" ] && [ -n "$conf" ]; then
  restart_dir="out/$conf/$year_start/$begin_month"
  printf "restart_dir=%s\n" "$restart_dir"

  if [ -d "$restart_dir" ]; then
    pass "restart directory exists"
    count=$(find "$restart_dir" -maxdepth 1 -name 'restart.????.in' | wc -l)
    printf "restart input count=%s\n" "$count"

    if [ -n "$ranks" ]; then
      if [ "$count" -eq "$ranks" ]; then
        pass "restart input count matches par_setup.dat"
      else
        fail "restart input count ($count) does not match par_setup.dat ($ranks)"
      fi

      last=$((ranks - 1))
      last_file=$(printf "%s/restart.%04d.in" "$restart_dir" "$last")
      if [ -f "$restart_dir/restart.0000.in" ] && [ -f "$last_file" ]; then
        pass "restart range includes restart.0000.in through restart.$(printf "%04d" "$last").in"
      else
        fail "restart range is missing restart.0000.in or restart.$(printf "%04d" "$last").in"
      fi
    fi
  else
    fail "restart directory does not exist: $restart_dir"
  fi
fi

section "Summary"

if [ "$failures" -eq 0 ]; then
  pass "pre-run checks passed"
  exit 0
else
  fail "$failures check(s) failed"
  exit 1
fi
