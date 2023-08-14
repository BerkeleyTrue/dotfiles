#!/usr/bin/env zsh

# Declare a cache variable
typeset -gAh GHANIMA_CACHE

# Get cache value from a given key
# USAGE:
#   ghanima::cache::get <key>
ghanima::cache::get() {
  local key="$1"
  echo -n "${GHANIMA_CACHE[$key]}"
}

# Set cache value for a given key
# USAGE:
#   ghanima::cache::set <key> <value>
ghanima::cache::set() {
  local key="$1" value="$2"
  GHANIMA_CACHE[$key]="$value"
}

# Clear cache
# USAGE:
#   ghanima::cache::clear
ghanima::cache::clear() {
  GHANIMA_CACHE=( )
}
