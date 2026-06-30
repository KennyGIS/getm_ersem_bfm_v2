#!/bin/sh

# GETM configure wrapper for the Git-backed version_v2 layout.
#
# This keeps the current functional NWES choices from getm_configure.sh:
#   - gfortran compiler
#   - Spherical coordinates
#   - GETM_USE_FABM=off
#   - GETM_USE_PARALLEL=on
#   - GETM_USE_STATIC=ON
#   - GETM_USE_BFM=ON
#   - GETM_FLAGS="-D_DELAY_SLOW_IP_ -D_SLR_V26_ -D_NEW_DAF_"
#
# Optional shared build configuration:
#   Set BUILD_CONFIG=/path/to/build_config.env to source one shell-compatible
#   config file before defaults are applied. This is the most portable option
#   for model_base_setup.sh, getm_configure_v2.sh, and compile_all_git without
#   adding a YAML/NML parser dependency.
#
# Example build_config.env keys:
#   VERSION_ROOT=/export/lv9/user/klarsen/version_v2
#   GETM_BASE=$VERSION_ROOT/home/GETM_SOURCES/getm_coupled_bfm_2016
#   GOTM_BASE=$VERSION_ROOT/home/GOTM_SOURCES/gotm_coupled_bfm_2016
#   BFM_BASE=$VERSION_ROOT/home/BFM_SOURCES/bfm_2016
#   FABM_BASE=$VERSION_ROOT/home/fabm-git/fabm
#   GOTM_BUILD_DIR=$VERSION_ROOT/tools/getm/build/gfortran/gotm
#   install_prefix=$VERSION_ROOT/local/getm
#   CMAKE_BIN=cmake
#   COMPILATION_MODE=production
#   GETM_USE_FABM=off
#   GETM_USE_PARALLEL=on
#   GETM_USE_STATIC=ON
#   GETM_USE_BFM=ON
#
# Warning: GETM_USE_FABM, GETM_USE_BFM, GETM_USE_PARALLEL, GETM_USE_STATIC,
# coordinate, and GETM_FLAGS are model-configuration choices. Changing them can
# have unknown scientific and runtime consequences for the current NWES setup.

set -eu

say() {
    printf '%s\n' "$*"
}

die() {
    printf 'ERROR: %s\n' "$*" >&2
    exit 1
}

validate_cmd() {
    command -v "$1" >/dev/null 2>&1 || die "Missing required command: $1"
}

validate_dir() {
    [ -d "$1" ] || die "Missing required directory: $1"
}

validate_file() {
    [ -f "$1" ] || die "Missing required file: $1"
}

if [ "${BUILD_CONFIG:-}" ]; then
    validate_file "$BUILD_CONFIG"
    # shellcheck disable=SC1090
    . "$BUILD_CONFIG"
    say "Loaded build configuration: $BUILD_CONFIG"
fi

say "COMPILATION_MODE: ${COMPILATION_MODE:-}"

CMAKE_BIN=${CMAKE_BIN:=cmake}
validate_cmd "$CMAKE_BIN"
validate_cmd gfortran
validate_cmd nf-config

#export ifort=$FORTRAN_COMPILER
#export CMAKE_Fortran_Compiler=$ifort

###### CMake Debug
#echo "DEBUG: PATH=$PATH"
#echo "DEBUG: which cmake=$(which cmake)"
#"$CMAKE_BIN" --version | head -1
######

say "COMPILATION_MODE: ${COMPILATION_MODE:-}"

# If VERSION_ROOT is set, use it as the default staged-build root. Otherwise,
# retain the historical defaults from getm_configure.sh.
if [ "${VERSION_ROOT:-}" ]; then
    GETM_BASE=${GETM_BASE:=$VERSION_ROOT/home/GETM_SOURCES/getm_coupled_bfm_2016/}
    GOTM_BASE=${GOTM_BASE:=$VERSION_ROOT/home/GOTM_SOURCES/gotm_coupled_bfm_2016/}
    BFM_BASE=${BFM_BASE:=$VERSION_ROOT/home/BFM_SOURCES/bfm_2016/}
    FABM_BASE=${FABM_BASE:=$VERSION_ROOT/home/fabm-git/fabm/}
    # Force the staged GOTM build path when VERSION_ROOT is set. Older shell
    # sessions may export GOTM_BUILD_DIR from version_2 or version_stable, and
    # allowing that through breaks the isolated build.
    GOTM_BUILD_DIR=$VERSION_ROOT/tools/getm/build/gfortran/gotm
    install_prefix=${install_prefix:=$VERSION_ROOT/local/getm}
else
    GETM_BASE=${GETM_BASE:=$HOME/home/GETM_SOURCES/getm_coupled_bfm_2016/}
    GOTM_BASE=${GOTM_BASE:=$HOME/home/GOTM_SOURCES/gotm_coupled_bfm_2016/}
    BFM_BASE=${BFM_BASE:=$HOME/home/BFM_SOURCES/bfm_2016/}
    FABM_BASE=${FABM_BASE:=$HOME/home/fabm-git/fabm/}
    GOTM_BUILD_DIR=${GOTM_BUILD_DIR:=$HOME/home/build/gotm}
    install_prefix=${install_prefix:=~/local/getm}
fi

# Default Fortran compiler. This is intentionally fixed to gfortran because
# the current model workflow is validated with gfortran and it is open-source.
export compiler=gfortran

# The current model inputs are spherical. Users adapting the model must confirm
# that their grid/input coordinates are spherical before using this default.
export coordinate=Spherical

# Current functional NWES model options. Keep these stable unless a new setup is
# deliberately validating a different model configuration.
GETM_USE_FABM=${GETM_USE_FABM:=off}
GETM_USE_PARALLEL=${GETM_USE_PARALLEL:=on}
GETM_USE_STATIC=${GETM_USE_STATIC:=ON}
GETM_USE_BFM=${GETM_USE_BFM:=ON}
GETM_FLAGS=${GETM_FLAGS:="-D_DELAY_SLOW_IP_ -D_SLR_V26_ -D_NEW_DAF_"}

say "Coordinate warning: using coordinate=Spherical; verify all model inputs use spherical coordinates."

say "Validating configure environment..."
validate_dir "$GETM_BASE"
validate_dir "$GETM_BASE/src"
validate_dir "$GOTM_BASE"
validate_dir "$BFM_BASE"
validate_dir "$FABM_BASE"

current_dir=$(pwd -P)
if [ "${VERSION_ROOT:-}" ]; then
    expected_build_root="$VERSION_ROOT/tools/getm/build"
    if [ "$current_dir" = "$expected_build_root" ]; then
        say "Build-root validation OK: $current_dir"
    else
        die "Build-root mismatch. Expected to run from $expected_build_root, but current directory is $current_dir"
    fi
else
    say "Build-root validation skipped: VERSION_ROOT is not set."
fi

say "CMake: $("$CMAKE_BIN" --version | sed -n '1p')"
say "Fortran compiler: $(gfortran --version | sed -n '1p')"
say "GETM_BASE: $GETM_BASE"
say "GOTM_BASE: $GOTM_BASE"
say "BFM_BASE: $BFM_BASE"
say "FABM_BASE: $FABM_BASE"
say "GOTM_BUILD_DIR: $GOTM_BUILD_DIR"
say "install_prefix: $install_prefix"
say "NetCDF Fortran configuration:"
nf-config --all

nf_fc=$(nf-config --fc 2>/dev/null || true)
case "$nf_fc" in
    *gfortran*|gfortran|"")
        say "NetCDF compiler validation OK: nf-config --fc reports '${nf_fc:-not reported}'."
        ;;
    *)
        die "NetCDF compiler mismatch: nf-config --fc reports '$nf_fc', but this wrapper uses gfortran"
        ;;
esac

say "Validation completed successfully."

export GETM_BASE
export GOTM_BASE
export BFM_BASE
export BFMDIR="$BFM_BASE"
export FABM_BASE
export GOTM_BUILD_DIR
export install_prefix

# ready to configure
mkdir -p $compiler
cd $compiler
"$CMAKE_BIN" $GETM_BASE/src \
      -DGETM_EMBED_VERSION=on \
      -DGOTM_BASE=$GOTM_BASE \
      -DGETM_USE_FABM=$GETM_USE_FABM \
      -DFABM_BASE=$FABM_BASE/ \
      -DCMAKE_Fortran_COMPILER=$compiler \
      -DGETM_USE_PARALLEL=$GETM_USE_PARALLEL \
      -DGETM_USE_STATIC=$GETM_USE_STATIC \
      -DGETM_COORDINATE_TYPE=$coordinate \
      -DGETM_FLAGS="$GETM_FLAGS" \
      -DGETM_USE_BFM=$GETM_USE_BFM \
      -DCMAKE_INSTALL_PREFIX=$install_prefix/$compiler
#      -DCMAKE_BUILD_TYPE=Debug
#      -DCMAKE_BUILD_TYPE=Production
#      -DGETM_FLAGS="-DMUDFLAT" \
#      -DCMAKE_Fortran_FLAGS="-g -C -check -traceback -check noarg_temp_created"
#      -DCMAKE_Fortran_FLAGS="-pr"
#      -DGETM_FLAGS="-D_SLR_V26_" \
#      -DCMAKE_BUILD_TYPE=Debug

# Proposed path-safe CMake invocation for a future cleanup. The active command
# above intentionally preserves the currently functional style.
#
# "$CMAKE_BIN" "$GETM_BASE/src" \
#       -DGETM_EMBED_VERSION=on \
#       -DGOTM_BASE="$GOTM_BASE" \
#       -DGETM_USE_FABM="$GETM_USE_FABM" \
#       -DFABM_BASE="$FABM_BASE/" \
#       -DCMAKE_Fortran_COMPILER="$compiler" \
#       -DGETM_USE_PARALLEL="$GETM_USE_PARALLEL" \
#       -DGETM_USE_STATIC="$GETM_USE_STATIC" \
#       -DGETM_COORDINATE_TYPE="$coordinate" \
#       -DGETM_FLAGS="$GETM_FLAGS" \
#       -DGETM_USE_BFM="$GETM_USE_BFM" \
#       -DCMAKE_INSTALL_PREFIX="$install_prefix/$compiler"

#GETM_FLAGS:
#-D_SLR_V26_:       old friction behaviour
#-D_DELAY_SLOW_IP_: Internal pressure: improved stability deep water
#-D_NEW_DAF_:       new flooding and drying
