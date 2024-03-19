# Enforcement Chapter - North Pacific Fishery Monitoring Program

# Purpose

The purpose of this chapter is to inform the public through the Council process of the potential violations with maritime law that have been reported by fishery observers.

# Organization

This repository is organized into two folders that are pretty self-explanatory

-   scripts

-   charts_and_tables

## Scripts

Five main scripts are used

-   **AR_1_get_data_queries.R** - this is the first to be run and gets the data used in the project. [It generates the output 'AR_Statements_data.rdata']{.underline}

-   **AR_2_rolling_join_scripts.R** - this file as explained calculates rates and other information used in outputs. It generates tables in .csv form that are stored in charts_and_tables folder. [It uses AR_Statements_data.rdata as an input. and generates AR_summary_scripts.rdata.]{.underline}

-   **AR_3_rate_calcs.R** - this file as explained calculates rates and other information used in outputs. [It uses AR_2_rolling_join_scripts.rdata as an input.]{.underline}

-   **AR_4_tables_generation.R** - Generates charts. It generates tables in .csv form that are stored in charts_and_tables folder. [It uses AR_3_rate_cacls.rdata as an input.]{.underline}
-     
-   **AR_charts_generation.R** - Generates charts. [Uses AR_rate_calcs.rdata as input.]{.underline}

## Important Notes

Outputs currently exclude confidential reporting where less than three observers are present in any given summary output.
