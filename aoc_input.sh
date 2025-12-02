#!/bin/zsh

# Load AOC_SESSION from .env file if it exists
if [[ -f ".env" ]]; then
  eval "$(grep '^AOC_SESSION=' .env)"
fi

function download_aoc_input() {
  local year_num=$1
  local day_num=$2
  local day_padded=$(printf "%02d" "$day_num")
  local target_dir="${year_num}/day_${day_padded}"

  echo "Year: $year_num, Day: $day_num"

  curl "https://adventofcode.com/${year_num}/day/${day_num}/input" \
    -H "cookie: session=$AOC_SESSION" \
    --compressed >> "${target_dir}/input.txt"
}

download_aoc_input $1 $2
