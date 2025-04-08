#Make an ODDS trips not logged table

library(FMAtools)
library(tidyverse)
library(flextable)
# # load ODDS data from google sheet
# # the ORIGINAL spreadsheet can be found at this link:
# # https://docs.google.com/spreadsheets/d/17BsqLarIh-8gqu9W5VWiEvFTjjnzaK-R48TXweXc2ag/edit?usp=sharing
# # here we are using a COPY of this spreadsheet, made 4/17/2025

odds_file <- "possible_trips_not_logged_or_logged_incorrectly_copy_4_7_2025.xlsx"

#Download data. Run this line manually during development, but it won't run during knitting.
gdrive_download(odds_file, gdrive_set_dribble("Projects/Annual Report OLE chapter 5/2024_data"))

library(readxl)
#Identify the available sheets
excel_sheets(odds_file)

odds_dat <- read_excel(odds_file, sheet = "2024 Issues")

#Data cleanup
odds_dat$`Issue Category` <- ifelse(grepl("not logged", odds_dat$`Issue Category`, ignore.case = TRUE), 
                                    "No Logged Trip", 
                                    odds_dat$`Issue Category`)
odds_dat$`Issue Category` <- ifelse(grepl("canceled", odds_dat$`Issue Category`, ignore.case = TRUE), 
                                    "Canceled Trip Fished", 
                                    odds_dat$`Issue Category`)
odds_dat$`Issue Category` <- ifelse(grepl("NMFS", odds_dat$`Issue Category`, ignore.case = TRUE), 
                                    "Incorrect FMP Area", 
                                    odds_dat$`Issue Category`)
odds_dat$`Trip Ending Port` <- ifelse(grepl("San Point", odds_dat$`Trip Ending Port`, ignore.case = TRUE), 
                                    "Sand Point", 
                                    odds_dat$`Trip Ending Port`)

#Make table
odds_table <- 
  merge(odds_dat %>% group_by(Port = `Trip Ending Port`, 
                        Issue = `Issue Category`) %>% 
      summarize(total_records = n()) %>%
      pivot_wider(names_from = Issue, values_from = total_records)
,
    odds_dat %>% group_by(Port = `Trip Ending Port`) %>% 
      summarize(total_records = n(),
                Cases = sum(!is.na(`Case Number`)))
) %>% arrange(desc(total_records)) %>%
  rename(`Ending Port` = Port,
         `Records (#)` = total_records,
         `Cases (#)` = Cases)

#Make Flextable
odds_table <- autofit(flextable(odds_table))
odds_table
