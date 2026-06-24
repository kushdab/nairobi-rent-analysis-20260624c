# Nairobi Rental Price Analysis (2021-2026)

This project provides an automated analysis of rental price trends across five major neighborhoods in Nairobi: Westlands, Kilimani, Karen, Langata, and Roysambu.

## Project Overview

The analysis covers a 5-year period concluding in June 2026. It utilizes synthetic data modeled on historical Nairobi real estate market behaviors, accounting for annual inflation, neighborhood-specific demand, and seasonal fluctuations.

## Structure
- `analysis.R`: The main processing script that generates data, performs statistical analysis, and creates visualizations.
- `nairobi_rent_trends.png`: Line chart showing price trajectories over time (generated after running script).
- `annual_comparison.png`: Bar chart comparing annual averages (generated after running script).

## Requirements
- R (version 4.0 or higher)
- R Packages: `tidyverse`, `lubridate`, `scales`, `broom`

## Usage
1. Ensure the required libraries are installed: `install.packages(c('tidyverse', 'lubridate', 'scales', 'broom'))`.
2. Run the script: `source('analysis.R')`.
3. Review the terminal output for growth coefficients and the generated PNG files for visual insights.