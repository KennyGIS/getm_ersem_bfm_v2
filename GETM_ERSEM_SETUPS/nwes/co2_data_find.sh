#!/bin/bash

CO2_LOOKUP_FILE="./co2_input/co2_monthly_bfm.dat"
CO2_NML_FILE="CO2.nml"

get_co2_for_year_month() {
    local year_raw="$1"
    local month_raw="$2"
    local year month co2_value

    if [ -z "$year_raw" ] || [ -z "$month_raw" ]; then
        echo "ERROR: get_co2_for_year_month requires YEAR and MONTH" >&2
        return 1
    fi

    year="$year_raw"
    case "$year" in
        [0-9][0-9][0-9][0-9]) ;;
        *)
            echo "ERROR: invalid year: $year_raw" >&2
            return 1
            ;;
    esac

    month_no_zero="${month_raw#0}"
    case "$month_no_zero" in
        1|2|3|4|5|6|7|8|9) month="0$month_no_zero" ;;
        10|11|12) month="$month_no_zero" ;;
        *)
            echo "ERROR: invalid month: $month_raw" >&2
            return 1
            ;;
    esac

    if [ ! -f "$CO2_LOOKUP_FILE" ]; then
        echo "ERROR: CO2 lookup file not found: $CO2_LOOKUP_FILE" >&2
        return 1
    fi

    co2_value=$(
        awk -v y="$year" -v m="$month" '
            BEGIN { found=0 }
            /^#/ { next }
            NF < 3 { next }
            {
                fy = sprintf("%04d", $1)
                fm = sprintf("%02d", $2)
                if (fy == y && fm == m) {
                    print $3
                    found=1
                    exit
                }
            }
            END {
                if (found == 0) exit 2
            }
        ' "$CO2_LOOKUP_FILE"
    )
    local status=$?

    if [ $status -ne 0 ] || [ -z "$co2_value" ]; then
        echo "ERROR: No CO2 value found for year=$year month=$month in $CO2_LOOKUP_FILE" >&2
        return 1
    fi

    printf "%s\n" "$co2_value"
    return 0
}

update_co2_nml() {
    local year_raw="$1"
    local month_raw="$2"
    local co2_ppm pco2_atm tmpfile

    if [ ! -f "$CO2_NML_FILE" ]; then
        echo "ERROR: CO2 namelist file not found: $CO2_NML_FILE" >&2
        return 1
    fi

    co2_ppm=$(get_co2_for_year_month "$year_raw" "$month_raw") || return 1
    pco2_atm=$(awk -v v="$co2_ppm" 'BEGIN { printf "%.10f", v * 1.0e-6 }')

    tmpfile=$(mktemp)

    awk -v newval="$pco2_atm" '
        BEGIN { replaced=0 }
        /^[[:space:]]*pCO2_air[[:space:]]*=/ {
            print "pCO2_air = " newval ","
            replaced=1
            next
        }
        { print }
        END {
            if (replaced == 0) exit 2
        }
    ' "$CO2_NML_FILE" > "$tmpfile"

    local status=$?
    if [ $status -ne 0 ]; then
        rm -f "$tmpfile"
        echo "ERROR: Failed to replace pCO2_air in $CO2_NML_FILE" >&2
        return 1
    fi

    mv "$tmpfile" "$CO2_NML_FILE"

    echo "Updated $CO2_NML_FILE with pCO2_air = $pco2_atm for YEAR=$year_raw MONTH=$month_raw (CO2=$co2_ppm ppm)"
    return 0
}

# Run directly if script is executed, not sourced
if [ "${BASH_SOURCE[0]}" = "$0" ]; then
    if [ -z "$YEAR_START" ] || [ -z "$MONTHS" ]; then
        echo "ERROR: YEAR_START and MONTHS must be set"
        exit 1
    fi

    update_co2_nml "$YEAR_START" "$MONTHS"
fi
