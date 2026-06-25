#!/usr/bin/env bash
set -eu

usage() {
  cat <<'USAGE'
Usage:
  add_constant_river_carbonate_vars.sh INPUT_RIVERS_NC OUTPUT_RIVERS_NC [O3H_MMOL_EQ_M3] [O3C_MG_C_M3]

Adds explicit per-river carbonate variables to a GETM-BFM rivers.nc file:
  r*_O3h(time)  total alkalinity, mmol eq/m3
  r*_O3c(time)  DIC as carbon mass, mg C/m3

Defaults are diagnostic constants:
  O3H_MMOL_EQ_M3 = 2200
  O3C_MG_C_M3   = 25212  (2101 mmol C/m3 * 12 mg C/mmol C)

The script intentionally adds explicit per-river variables instead of relying
on limit_O3h:function="yes", because that function path can mutate the model
column directly in the current GETM-BFM river implementation.
USAGE
}

if [ "${1:-}" = "-h" ] || [ "${1:-}" = "--help" ]; then
  usage
  exit 0
fi

if [ "$#" -lt 2 ] || [ "$#" -gt 4 ]; then
  usage >&2
  exit 2
fi

infile=$1
outfile=$2
o3h_value=${3:-2200}
o3c_value=${4:-25212}

case "$o3h_value" in
  *.*|*e*|*E*) ;;
  *) o3h_value="${o3h_value}.0" ;;
esac

case "$o3c_value" in
  *.*|*e*|*E*) ;;
  *) o3c_value="${o3c_value}.0" ;;
esac

if [ ! -f "$infile" ]; then
  echo "ERROR: input file not found: $infile" >&2
  exit 1
fi

for tool in ncdump ncap2 ncatted; do
  if ! command -v "$tool" >/dev/null 2>&1; then
    echo "ERROR: required command not found: $tool" >&2
    exit 1
  fi
done

if [ "$infile" != "$outfile" ]; then
  cp "$infile" "$outfile"
fi

tmp_script=$(mktemp)
trap 'rm -f "$tmp_script"' EXIT

ncdump -h "$outfile" |
  awk '
    /^[[:space:]]*(double|float)[[:space:]]+r[0-9]+\(time\)[[:space:]]*;/ {
      name=$2
      sub(/\(time\).*/, "", name)
      print name
    }
  ' |
  sort -V |
  while IFS= read -r river; do
    [ -n "$river" ] || continue
    printf "%s_O3h[time]=%s;\n" "$river" "$o3h_value" >> "$tmp_script"
    printf "%s_O3c[time]=%s;\n" "$river" "$o3c_value" >> "$tmp_script"
  done

if [ ! -s "$tmp_script" ]; then
  echo "ERROR: no base river variables like r93(time) found in $outfile" >&2
  exit 1
fi

ncap2 -O -S "$tmp_script" "$outfile" "$outfile"

ncdump -h "$outfile" |
  awk '
    /^[[:space:]]*(double|float)[[:space:]]+r[0-9]+_O3[hc]\(time\)[[:space:]]*;/ {
      name=$2
      sub(/\(time\).*/, "", name)
      print name
    }
  ' |
  while IFS= read -r var; do
    case "$var" in
      *_O3h)
        ncatted -O \
          -a long_name,"$var",o,c,"River total alkalinity" \
          -a units,"$var",o,c,"mmol eq/m3" \
          -a missing_value,"$var",o,d,-9999.0 \
          -a _FillValue,"$var",o,d,-9999.0 \
          "$outfile"
        ;;
      *_O3c)
        ncatted -O \
          -a long_name,"$var",o,c,"River dissolved inorganic carbon" \
          -a units,"$var",o,c,"mg C/m3" \
          -a missing_value,"$var",o,d,-9999.0 \
          -a _FillValue,"$var",o,d,-9999.0 \
          "$outfile"
        ;;
    esac
  done

echo "Wrote $outfile"
echo "Added $(grep -c '_O3h' "$tmp_script") O3h variables and $(grep -c '_O3c' "$tmp_script") O3c variables."
echo "Values: O3h=$o3h_value mmol eq/m3, O3c=$o3c_value mg C/m3"
