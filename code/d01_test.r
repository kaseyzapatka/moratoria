# ==========================================================================
# Purpose: Crosswalk counties from 2012 and 2017 to 2020 boundaries for Connecticut
# Author: Tim Thomas/Kasey Zapatka

# Notes: 
# ----------  
# As of 2022, Connecticut uses 9 planning regions instead of the traditional counties
# as used in other states. Unfortunately, crosswalks to only county equivalants will not 
# be made available until 2027. To harmonize pre-2022 counties into 2022+ boundaries, we
# pull tract data, harmonize those to 2022 boundaries, and aggregated up to the county. 
# Then we can replace the Connecticut county data with the updated harmonized data.  

# Script:
# ----------  
# 1. PULL MISSING COUNTY DATA
# 2. CREATE CROSSWALK
# 3. CROSSWALK  
# 4. COLLAPSE TO COUNTY BOUNDARIES 
# 5. PROCESS FOR JOIN 

# ==========================================================================


# ==========================================================================
# PACKAGES AND OPTIONS
# ==========================================================================
# clear all
remove(list = ls())

#
# Load packages
# --------------------------------------------------------------------------
# Loading required packages and setting defaults
librarian::shelf(tidyverse, tidycensus, tigris, sf, tmap, timathomas/colorout, qs, janitor)

#
# open data
# --------------------------------------------------------------------------

monthly <- read_csv("data/monthly_state_data_download.csv")
weekly <- read_csv("data/weekly_state_data_download.csv")


#
# monthly
# --------------------------------------------------------------------------

monthly |> 
  filter(name == "New York") |> 
  arrange(date) |> 
  print()

monthly |> 
  distinct(name) |> 
  print(n = 100)

monthly |> 
  group_by(name) |> 
  summarize(min = min(date), 
            max = max(date), 
            count = n()) |> 
  filter(count >= 116) |> 
  glimpse() |> 
  pull(name) |> 
  print()

monthly |> 
  group_by(name) |> 
  filter(between(as.Date(date),as.Date("2016-01-04"),as.Date("2023-12-31"))) |> 
  mutate(count = n()) |>
  ungroup() |> 
  filter(count >= 95) |> 
  distinct(name, count) |>
  glimpse() |> 
  pull(name) |> 
  print()

#
# weekly
# --------------------------------------------------------------------------

weekly |> 
  filter(name == "New Hampshire") |> 
  arrange(date) |> 
  print()

weekly |> 
  distinct(name) |> 
  print(n = 100)


weekly |> 
  group_by(name) |> 
  summarize(min = min(date), 
            max = max(date), 
            count = n()) |> 
  filter(count >= 504) |> 
  glimpse() |> 
  pull(name) |> 
  print()

weekly |> 
  group_by(name) |> 
  #filter(between(date, 2016-01-01, 2023-12-31)) |> 
  filter(between(as.Date(date),as.Date("2016-01-04"),as.Date("2023-12-31"))) |> 
  mutate(count = n()) |>
  ungroup() |> 
  filter(count >= 416) |> 
  distinct(name, count) |>
  glimpse() |> 
  pull(name) |> 
  print()
