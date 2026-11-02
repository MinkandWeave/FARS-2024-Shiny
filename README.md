# FARS 2024 Shiny Dashboard

**STAT 442 – Exploratory Data Analysis**

**Author:** Eli Homan

## Project Overview

This interactive Shiny dashboard compares collision patterns among rural and urban fatal traffic crashes in the United States during 2024.

Users can select a state and roadway type to explore differences in fatal crash categories.

## Features

- Interactive state and roadway filters
- Rural and urban fatal crash totals
- Comparison chart showing percentages by collision type
- Automatically generated summary of the largest observed difference
- Additional tables and explanations

## Data Source

National Highway Traffic Safety Administration (NHTSA) – Fatality Analysis Reporting System (FARS), 2024.

## Live Dashboard

https://elihoman12.shinyapps.io/FARS_2024_Shiny/

## Files

- `app.R` – Shiny dashboard
- `prepare_data.R` – Data cleaning and preparation
- `data/crash_summary.rds` – Prepared crash data

## Interpretation

The dashboard compares patterns among fatal crashes. It does not measure the risk of a crash per mile driven or establish causes.
