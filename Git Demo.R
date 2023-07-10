library(tidyverse)   #contains dplyr
library(openxlsx)
library(readxl)  #read excel files
library(odbc)  # library to connect to the database
library(stringr)  
library(data.table)   # added from previous R
library(ggplot2)
library(ggrepel)

### Set variables
current_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
setwd("..")
datasources <- paste0(getwd(), "/Re_Enrollment/Data Sources/")
save <- paste0(getwd(), "/Re_Enrollment/Files to QA/")


### Establish Connection
con2 <- dbConnect(odbc(),
                  .connection_string = paste0("Driver={SQL Server Native Client 11.0};",
                                              "driver={SQL Server};server=dsdb1.dps.detroit.k12.mi.us;",
                                              "database=DATACOM2;trusted_connection=true"),
                  timeout = 10)
con3 <- dbConnect(odbc(),
                  .connection_string = paste0("Driver={SQL Server Native Client 11.0};",
                                              "driver={SQL Server};server=dsdev.dps.detroit.k12.mi.us;",
                                              "database=SchoolGrades;trusted_connection=true"),
                  timeout = 10)


sch_metric <- dbGetQuery(con2, "SELECT t1.*, t2.METRIC_NAME FROM SchoolGrades.dbo.sch_metric_summary t1 left join SchoolGrades.dbo.sg_metric t2 on t1.metric_id = t2.metric_id") %>% 
  filter(SG_YEAR_END == 2022 & ITERATION_ID==1 | SG_YEAR_END == 2023 & ITERATION_ID==0) %>% 
  filter(METRIC_ID %in% c(28,29,30,31,32,33,34)) %>%
  mutate(threshold = case_when(METRIC_ID %in% c(28, 29) ~ 0.35, 
                               METRIC_ID %in% c(32, 33) ~ 0.1,
                               METRIC_ID %in% c(30, 31) ~ 0.15,
                               METRIC_ID %in% c(34) ~ 1))

ggplot(sch_metric, aes(x=METRIC_PCT)) + 
  stat_ecdf(aes(color = METRIC_PCT)) + 
  facet_grid(METRIC_NAME ~ SG_YEAR_END, labeller = label_value) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  xlim(0, 1) +
  labs(y = "% of Schools") +
  geom_vline(aes(xintercept = threshold), color = "red", size = 1, data = sch_metric) +
  geom_text(
    aes(
      y = sch_metric$METRIC_PCT[sch_metric$threshold == threshold],
      label = round(sch_metric$METRIC_PCT[sch_metric$threshold == threshold], 2)
    ),
    size = 2,
    color = "black"
  )
  geom_text(
    aes(
      y = sch_metric$METRIC_PCT[sch_metric$threshold == threshold], 
      label = sch_metric$METRIC_PCT[sch_metric$threshold == threshold]), 
    size = 1, 
    color = "black")

  
  
  
#### PANORAMA
  
cclc <- dbGetQuery(con3, "SELECT 
       [METRIC_ABBREVIATION]
      ,[REPORT_CARD]
      ,[EEM_CODE]
      ,[JUNE_PRELIM_NUMERATOR]
      ,[JULY_PRELIM_NUMERATOR]
      ,[NUMERATOR_VARIANCE]
      ,[JUNE_PRELIM_DENOMINATOR]
      ,[JULY_PRELIM_DENOMINATOR]
      ,[DENOMINATOR_VARIANCE]
      ,[JUNE_PRELIM_PCT]
      ,[JULY_PRELIM_PCT]
      ,[PCT_VARIANCE]
  FROM [SchoolGrades].[dbo].[JUNE_PRELIMINARY_JULY_PRELIMINARY_METRIC_COMPARISON]
  where METRIC_ABBREVIATION  like '%CCLC%'") %>% 
  pivot_wider(id_cols = c(EEM_CODE, REPORT_CARD), names_from = "METRIC_ABBREVIATION", values_from = "PCT_VARIANCE", values_fn = list)

june_grades <- dbGetQuery(con2, "SELECT distinct
       t1.[EEM_CODE]
      ,t2.BUILDING_NAME
      ,t1.[REPORT_CARD]
      ,[TOTAL_POINTS_POSSIBLE]
      ,[TOTAL_POINTS_EARNED]
      ,[PERCENT_POINTS_EARNED]
      ,[LETTER_GRADE]
  FROM [SchoolGrades].[dbo].[SCH_REPORT_CARD_SUMMARY] t1
  left join
  [SchoolGrades].[dbo].SG_SCHOOLS t2
  on t1.eem_code = t2.eem_code
  where t1.SG_YEAR_END = 2023 and t1.ITERATION_ID = 0 and t2.SG_YEAR_END = 2023") #%>%

changers <- cclc %>% 
  left_join(june_grades, by=c("EEM_CODE" = "EEM_CODE","REPORT_CARD" = "REPORT_CARD")) %>% 
  filter(REPORT_CARD != 'K8') %>% 
  mutate(new_overall = (
                            TOTAL_POINTS_EARNED +as.numeric(CCR_CCLC_Enrolled)*100 + as.numeric(CCR_CCLC_Passing)*100)/TOTAL_POINTS_POSSIBLE) %>% 
  mutate(new_overall = round(new_overall,2))

write_csv(changers, file = "cclc.csv", na = "NA")



