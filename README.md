# Enforcement Chapter - North Pacific Fishery Monitoring Program
## Collaborators
The primary author(s) responsible for maintaining this repository are:
* Craig H. Faunce (craig.faunce@noaa.gov)
* Andy Kingham (andy.kingham@noaa.gov)

## Purpose

The purpose of this chapter is to inform the public through the Council process of the potential violations with maritime law that have been reported by fishery observers.

## Organization

This repository is organized into two folders that are pretty self-explanatory

-   scripts
-   charts
-   tables

### Scripts

Five main scripts are used

-   **AR_1_get_data_queries.R** - this is the first to be run and gets the data used in the project. [It generates the output 'AR_Statements_data.rdata']{.underline}

-   **AR_2_rolling_join_scripts.R** - this file as explained calculates rates and other information used in outputs. It generates tables in .csv form that are stored in charts_and_tables folder. [It uses AR_Statements_data.rdata as an input. and generates AR_summary_scripts.rdata.]{.underline}

-   **AR_3_rate_calcs.R** - this file as explained calculates rates and other information used in outputs. [It uses AR_2_rolling_join_scripts.rdata as an input.]{.underline}

-   **AR_4_tables_generation.R** - Generates charts. It generates tables in .csv form that are stored in charts_and_tables folder. [It uses AR_3_rate_cacls.rdata as an input.]{.underline}     

-   **AR_5_charts.R** - Generates charts. [Uses AR_rate_calcs.rdata as input.]{.underline}

## Important Notes

Releases correspond to the code used for the product.  Reverting to a version will place the code at the point of the start of that project.
For this reason releases should be made when the Team has finished the product and code is considered finalized.
Outputs currently exclude confidential reporting where less than three observers are present in any given summary output.

For details on how to connect to the AFSC database using the `eval(parse())` method, see [this page](https://github.com/Alaska-Fisheries-Monitoring-Analytics/AnnRpt-Deployment-Chapter/blob/master/database_connection.md).

## Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project content is provided on an "as is" basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
