getwd()
options(warn = -1)
#options(warn = 0)
 library(tidyverse)
 library(gt)
 library(flextable)
 library(rmarkdown)
 library(tinytex)
 library(ggplot2)
 library(datasets)
 library(dplyr)
#install.packages('officer', 'flextable')
 library(officer)
 library(flextable)
#Set the directory for output table.
setwd("/Users/emilywichmann/Desktop/JHC Lab/Pitt Paper")
#Read the dataset
sdud <- read.csv(file="/Users/emilywichmann/Desktop/JHC Lab/Pitt Paper/sdud2025_updated2025.csv", header=TRUE, sep=",")
 summary(sdud)
 # convert variable
 #remove . and replace with _ in variable names
 names(sdud) <- gsub("\\.", "_", names(sdud))
 names(sdud)
 
 sdud$Suppression_Used <- as.logical(sdud$Suppression_Used)
 class(sdud$Suppression_Used)
freq_table_suppression <- table(sdud$Suppression_Used)
 print(freq_table_suppression)
## 
 ## FALSE TRUE 
 ## 146 143
sdud_filtered <- sdud[sdud$Suppression_Used != "TRUE" & !is.na(sdud$Suppression_Used), ]
 summary(sdud_filtered)
summary(sdud$Suppression_Used)
sdud_filtered2 <- sdud_filtered[sdud_filtered$State != "XX" & !is.na(sdud_filtered$State), ]
 
 State_freq2 <- table(sdud_filtered2$State)
 print(State_freq2)

quarterly_summary <- sdud_filtered2 %>%
 group_by(Utilization_Type, Quarter) %>%
 summarise(
 Total_Reimbursed = round(sum(`Medicaid_Amount_Reimbursed`, na.rm = TRUE), 2),
 Total_Prescriptions = round(sum(`Number_of_Prescriptions`, na.rm = TRUE), 2),
 Total_Units = sum(`Units_Reimbursed`, na.rm = TRUE),
 groups = "drop" 
 ) %>%
 # Calculate the "Total" row for each quarter
 group_by(Quarter) %>%
 group_modify(~ {
 total_row <- tibble(
 Utilization = "Total",
 Total_Reimbursed = sum(.x$Total_Reimbursed),
 Total_Prescriptions = sum(.x$Total_Prescriptions),
 Total_Units = sum(.x$Total_Units)
 )
 bind_rows(total_row, .x)
 }) %>%
 # Calculate Percentage Share
 mutate(
 Share_Total = (Total_Reimbursed / first(Total_Reimbursed)) * 100
 ) %>%
 select(Quarter, Utilization_Type, Utilization, Total_Reimbursed, Share_Total, Total_Prescriptions, Total_Units)
#ungroup()

write.csv(quarterly_summary, "Quarterly_Medicaid_Summary12.csv", row.names = FALSE)
final_table <- quarterly_summary %>%
 gt(rowname_col = "Utilization", groupname_col = "Quarter") %>%
 tab_header(
 title = "Table 1. National Medicaid utilization and reimbursement for suzetrigine (Journavx®)",
 subtitle = "during first year after FDA approval (2025)"
 ) %>%
 cols_label(
 Total_Reimbursed = "Total Reimbursed ($)a",
 Share_Total = "Share of Total Reimbursed (%)",
 Total_Prescriptions = "Number of Prescriptions",
 Total_Units = "Number of Units (tablets)"
 ) %>%
 # Formatting values
 fmt_currency(columns = Total_Reimbursed, currency = "USD") %>%
 fmt_number(columns = Share_Total, decimals = 2) %>%
 #fmt_number(columns = c(Num_Prescriptions, Num_Units), decimals = 0, use_seps = TRUE) %>%
 # Visual Styling
 tab_options(
 row_group.font.weight = "bold",
 table.width = pct(100)
 )
 # Display the table
 final_table
quarterly_summary_allQ <- sdud_filtered2 %>%
 group_by(Utilization_Type) %>%
 summarise(
 Total_Reimbursed = round(sum(`Medicaid_Amount_Reimbursed`, na.rm = TRUE), 2),
 Total_Prescriptions = round(sum(`Number_of_Prescriptions`, na.rm = TRUE), 2),
 Total_Units = sum(`Units_Reimbursed`, na.rm = TRUE),
 groups = "drop" 
 ) %>%
 #group_by(Quarter) %>%
 group_modify(~ {
 total_row <- tibble(
 Utilization = "Total",
 Total_Reimbursed = sum(.x$Total_Reimbursed),
 Total_Prescriptions = sum(.x$Total_Prescriptions),
 Total_Units = sum(.x$Total_Units)
 )
 bind_rows(total_row, .x)
 }) %>%
 mutate(
 Share_Total = (Total_Reimbursed / first(Total_Reimbursed)) * 100
 ) %>%
 select(Utilization_Type, Utilization, Total_Reimbursed, Share_Total, Total_Prescriptions, Total_Units) %>%
 ungroup()
 print(quarterly_summary_allQ)
write.csv(quarterly_summary_allQ, "quarterly_summary_allQ3.csv", row.names = FALSE)
allQ_final_table <- quarterly_summary_allQ %>%
 gt(rowname_col = "Utilization", groupname_col = "Quarter") %>%
 tab_header(
 title = "Table 1. National Medicaid utilization and reimbursement for suzetrigine (Journavx®)",
 subtitle = "during first year after FDA approval (2025) for FFSU and MCOU regardless the state and quarter"
 ) %>%
 cols_label(
 Total_Reimbursed = "Total Reimbursed ($)a",
 Share_Total = "Share of Total Reimbursed (%)",
 Total_Prescriptions = "Number of Prescriptions",
 Total_Units = "Number of Units (tablets)"
 ) %>%
 # Formatting values
 fmt_currency(columns = Total_Reimbursed, currency = "USD") %>%
 fmt_number(columns = Share_Total, decimals = 2) %>%
 #fmt_number(columns = c(Num_Prescriptions, Num_Units), decimals = 0, use_seps = TRUE) %>%
 # Visual Styling
 tab_options(
 row_group.font.weight = "bold",
 table.width = pct(100)
 )
 
 # Display the table
 allQ_final_table
xx <- sdud_filtered%>%
 filter(State == "XX")
 #View(xx)
 
 xx_summary <- xx %>%
 group_by(Utilization_Type, Quarter) %>%
 summarise(
 Total_Reimbursed = round(sum(`Medicaid_Amount_Reimbursed`, na.rm = TRUE), 2),
 Total_Prescriptions = round(sum(`Number_of_Prescriptions`, na.rm = TRUE), 2),
 Total_Units = sum(`Units_Reimbursed`, na.rm = TRUE),
 groups = "drop" 
 ) %>%
 group_by(Quarter) %>%
 group_modify(~ {
 total_row <- tibble(
 Utilization = "Total",
 Total_Reimbursed = sum(.x$Total_Reimbursed),
 Total_Prescriptions = sum(.x$Total_Prescriptions),
 Total_Units = sum(.x$Total_Units)
 )
 bind_rows(total_row, .x)
 }) %>%
 mutate(
 Share_Total = (Total_Reimbursed / first(Total_Reimbursed)) * 100
 ) %>%
 select(Quarter, Utilization_Type, Utilization, Total_Reimbursed, Share_Total, Total_Prescriptions, Total_Units) %>%
 ungroup()

write.csv(xx_summary, "xx_summary3.csv", row.names = FALSE)
xx_final_table <- xx_summary %>%
 gt(rowname_col = "Utilization", groupname_col = "Quarter") %>%
 tab_header(
 title = "National Medicaid utilization and reimbursement for suzetrigine (Journavx®) for State is XX",
 subtitle = "during first year after FDA approval (2025) for total of FFCU and MCOU regardless of Suppression and states"
 ) %>%
 cols_label(
 Total_Reimbursed = "Total Reimbursed ($)a",
 Share_Total = "Share of Total Reimbursed (%)",
 Total_Prescriptions = "Number of Prescriptions",
 Total_Units = "Number of Units (tablets)"
 ) %>%
 # Formatting values
 fmt_currency(columns = Total_Reimbursed, currency = "USD") %>%
 fmt_number(columns = Share_Total, decimals = 2) %>%
 #fmt_number(columns = c(Num_Prescriptions, Num_Units), decimals = 0, use_seps = TRUE) %>%
 # Visual Styling
 tab_options(
 row_group.font.weight = "bold",
 table.width = pct(100)
 )
 
 # Display the table
 xx_final_table
xx_allQ <- xx %>%
 group_by(Utilization_Type) %>%
 summarise(
 Total_Reimbursed = round(sum(`Medicaid_Amount_Reimbursed`, na.rm = TRUE), 2),
 Total_Prescriptions = round(sum(`Number_of_Prescriptions`, na.rm = TRUE), 2),
 Total_Units = sum(`Units_Reimbursed`, na.rm = TRUE),
 groups = "drop" 
 ) %>%
 #group_by(Quarter) %>%
 group_modify(~ {
 total_row <- tibble(
 Utilization = "Total",
 Total_Reimbursed = sum(.x$Total_Reimbursed),
 Total_Prescriptions = sum(.x$Total_Prescriptions),
 Total_Units = sum(.x$Total_Units)
 )
 bind_rows(total_row, .x)
 }) %>%
 mutate(
 Share_Total = (Total_Reimbursed / first(Total_Reimbursed)) * 100
 ) %>%
 select(Utilization_Type, Utilization, Total_Reimbursed, Share_Total, Total_Prescriptions, Total_Units) %>%
 ungroup()
 print(xx_allQ)

write.csv(xx_allQ, "xx_allQ", row.names = FALSE)
xx_allQ_final_table <- xx_allQ %>%
 gt(rowname_col = "Utilization", groupname_col = "Quarter") %>%
 tab_header(
 title = "Table 1. National Medicaid utilization and reimbursement for suzetrigine (Journavx®) for state='XX'",
 subtitle = "during first year after FDA approval (2025) for FFSU and MCOU regardless the state and quarter"
 ) %>%
 cols_label(
 Total_Reimbursed = "Total Reimbursed ($)a",
 Share_Total = "Share of Total Reimbursed (%)",
 Total_Prescriptions = "Number of Prescriptions",
 Total_Units = "Number of Units (tablets)"
 ) %>%
 # Formatting values
 fmt_currency(columns = Total_Reimbursed, currency = "USD") %>%
 fmt_number(columns = Share_Total, decimals = 2) %>%
 #fmt_number(columns = c(Num_Prescriptions, Num_Units), decimals = 0, use_seps = TRUE) %>%
 # Visual Styling
 tab_options(
 row_group.font.weight = "bold",
 table.width = pct(100)
 )
 
 # Display the table
 xx_allQ_final_table
sum_prescrip_state <- sdud_filtered2 %>%
 group_by(`State`) %>%
 summarize(Total_Prescriptions = sum(Number_of_Prescriptions, na.rm = TRUE),
 #arrange(desc(Total_Prescriptions)) %>%
 Total_Reimbursed = sum(Medicaid_Amount_Reimbursed, na.rm = TRUE)) 
 
 #RENAME THE STATE ABBREVIATION TO FULL NAME
 sum_prescrip_state$State <- case_when(
 sum_prescrip_state$State == "NY" ~ "New York",
 sum_prescrip_state$State == "NC" ~ "North Carolina",
 sum_prescrip_state$State == "TN" ~ "Tennessee",
 sum_prescrip_state$State == "KY" ~ "Kentucky",
 sum_prescrip_state$State == "IN" ~ "Indiana",
 sum_prescrip_state$State == "IA" ~ "Iowa",
 sum_prescrip_state$State == "TX" ~ "Texas",
 sum_prescrip_state$State == "CO" ~ "Colorado",
 sum_prescrip_state$State == "CT" ~ "Connecticut",
 sum_prescrip_state$State == "GA" ~ "Georgia",
 sum_prescrip_state$State == "AR" ~ "Arkansas",
 sum_prescrip_state$State == "LA" ~ "Louisiana",
 sum_prescrip_state$State == "VA" ~ "Virginia",
 sum_prescrip_state$State == "PA" ~ "Pennsylvania",
 sum_prescrip_state$State == "MA" ~ "Massachusetts",
 sum_prescrip_state$State == "WA" ~ "Washington",
 sum_prescrip_state$State == "SD" ~ "South Dakota",
 sum_prescrip_state$State == "WV" ~ "West Virginia",
 sum_prescrip_state$State == "MN" ~ "Minnesota",
 sum_prescrip_state$State == "OH" ~ "Ohio",
 sum_prescrip_state$State == "MI" ~ "Michigan",
 sum_prescrip_state$State == "AZ" ~ "Arizona",
 sum_prescrip_state$State == "NE" ~ "Nebraska",
 sum_prescrip_state$State == "SC" ~ "South Carolina",
 sum_prescrip_state$State == "NJ" ~ "New Jersey",
 sum_prescrip_state$State == "IL" ~ "Illinois",
 sum_prescrip_state$State == "UT" ~ "Utah",
 sum_prescrip_state$State == "CA" ~ "California",
 sum_prescrip_state$State == "FL" ~ "Florida",
 sum_prescrip_state$State == "OK" ~ "Oklahoma",
 sum_prescrip_state$State == "WI" ~ "Wisconsin",
 sum_prescrip_state$State == "MD" ~ "Maryland",
 sum_prescrip_state$State == "OR" ~ "Oregon",
 sum_prescrip_state$State == "DE" ~ "Delaware",
 sum_prescrip_state$State == "HI" ~ "Hawaii",
 sum_prescrip_state$State == "NH" ~ "New Hampshire",
 sum_prescrip_state$State == "NV" ~ "Nevada",
 # Keep the original value for any other state
 TRUE ~ sum_prescrip_state$State
 )
 
 write.csv(sum_prescrip_state, "sum_prescrip_state2.csv", row.names = FALSE)
 print(sum_prescrip_state, n= Inf)

State2 <- read.csv(file="Users/emilywichmann/Desktop/JHC Lab/Pitt Paper/MEBS_q1.csv", header=TRUE, sep=",")
 
State2 <- read.csv(file="/Users/emilywichmann/Desktop/JHC Lab/Pitt Paper/MEBS_q2.csv", header=TRUE, sep=",")
combined_data <- rbind(MBES_q1, MBES_q2)
 summary(combined_data)
 
summary_data <- combined_data %>%
 group_by(State) %>% # Replace with your grouping variable
 summarise(total_enroll = sum(Total_Medicaid_Enrollees, na.rm = TRUE)) # Replace with your variable to sum
 
 summary_data$State <- case_when(
 summary_data$State == "New York " ~ "New York",
 summary_data$State == "North Carolina " ~ "North Carolina",
 summary_data$State == "Tennessee " ~ "Tennessee",
 summary_data$State == "Kentucky " ~ "Kentucky",
 summary_data$State == "Indiana " ~ "Indiana",
 summary_data$State == "Iowa " ~ "Iowa",
 summary_data$State == "Texas " ~ "Texas",
 summary_data$State == "Colorado " ~ "Colorado",
 summary_data$State == "Connecticut " ~ "Connecticut",
 summary_data$State == "Georgia " ~ "Georgia",
 summary_data$State == "Arkansas " ~ "Arkansas",
 summary_data$State == "Louisiana " ~ "Louisiana",
 summary_data$State == "Virginia " ~ "Virginia",
 summary_data$State == "Pennsylvania " ~ "Pennsylvania",
 summary_data$State == "Massachusetts " ~ "Massachusetts",
 summary_data$State == "Washington " ~ "Washington",
 summary_data$State == "South Dakota " ~ "South Dakota",
 summary_data$State == "West Virginia " ~ "West Virginia",
 summary_data$State == "Minnesota " ~ "Minnesota",
 summary_data$State == "Ohio " ~ "Ohio",
 summary_data$State == "Michigan " ~ "Michigan",
 summary_data$State == "Arizona " ~ "Arizona",
 summary_data$State == "Nebraska " ~ "Nebraska",
 summary_data$State == "South Carolina " ~ "South Carolina",
 summary_data$State == "New Jersey " ~ "New Jersey",
 summary_data$State == "Illinois " ~ "Illinois",
 summary_data$State == "Utah " ~ "Utah",
 summary_data$State == "California " ~ "California",
 summary_data$State == "Florida " ~ "Florida",
 summary_data$State == "Oklahoma " ~ "Oklahoma",
 summary_data$State == "Wisconsin " ~ "Wisconsin",
 summary_data$State == "Maryland " ~ "Maryland",
 summary_data$State == "Oregon " ~ "Oregon",
 
 
 # Keep the original value for any other state
 TRUE ~ summary_data$State
 )
 
 write.csv(summary_data, "summary_data_enroll2.csv", row.names = FALSE)
 
 print(summary_data, n= Inf)

total_enroll_average <- summary_data %>%
 mutate(average_enroll = total_enroll/6)
 View(total_enroll_average)
combined_data <- left_join(sum_prescrip_state, total_enroll_average, by = 'State')
 
 write.csv(combined_data, "combined_data2.csv", row.names = FALSE)
 
 print(combined_data, n= Inf)

combined_data_pernt <- combined_data %>%
 mutate(prescription_rate = (combined_data$Total_Prescriptions / combined_data$average_enroll) * 100000)
 
 print(combined_data_pernt, n= Inf)
## # A tibble: 37 × 6
 ## State Total_Prescriptions Total_Reimbursed total_enroll average_enroll
 ## <chr> <int> <dbl> <int> <dbl>
 ## 1 Arkansas 183 81053. 4822692 803782 
 ## 2 Arizona 70 37181. 12879495 2146582.
 ## 3 California 52 13670. 89203434 14867239 
 ## 4 Colorado 271 84090. 7184488 1197415.
 ## 5 Connecticut 235 167968. 6416523 1069420.
 ## 6 Delaware 20 7056. NA NA 
 ## 7 Florida 58 38396. 24478075 4079679.
 ## 8 Georgia 239 102457. 12627401 2104567.
 ## 9 Hawaii 11 6748. NA NA 
 ## 10 Iowa 299 219478. 3601940 600323.
 ## 11 Illinois 58 35083. 17291880 2881980 
 ## 12 Indiana 315 115957. 10840985 1806831.
 ## 13 Kentucky 271 119385. 8115330 1352555 
 ## 14 Louisiana 330 160706. 11025383 1837564.
 ## 15 Massachuset… 205 74061. 11871897 1978650.
 ## 16 Maryland 31 11624. 7825973 1304329.
 ## 17 Michigan 57 36621. 14232297 2372050.
 ## 18 Minnesota 64 30712. 6984363 1164060.
 ## 19 North Carol… 704 383454. 18930969 3155162.
 ## 20 Nebraska 58 30118. 2053346 342224.
 ## 21 New Hampshi… 45 19756. NA NA 
 ## 22 New Jersey 48 16769. 9998259 1666376.
 ## 23 Nevada 31 11519. NA NA 
 ## 24 New York 2836 1208148. 41322194 6887032.
 ## 25 Ohio 60 24405. 18270142 3045024.
 ## 26 Oklahoma 184 78344 5830359 971726.
 ## 27 Oregon 26 13068. 7792608 1298768 
 ## 28 Pennsylvania 131 58634. 17931897 2988650.
 ## 29 South Carol… 98 50472. 7576497 1262750.
 ## 30 South Dakota 158 44397. 811929 135322.
 ## 31 Tennessee 369 147920. 9058444 1509741.
 ## 32 Texas 405 193973. 26225984 4370997.
 ## 33 Utah 98 26682. 2049782 341630.
 ## 34 Virginia 202 90927. 10529443 1754907.
 ## 35 Washington 131 83749. 10974762 1829127 
 ## 36 Wisconsin 58 23795. 7431480 1238580 
 ## 37 West Virgin… 115 44089. 3073373 512229.
 ## # ℹ 1 more variable: prescription_rate <dbl>
prescription_rate_top20 <- combined_data_pernt %>%
 slice_max(order_by = prescription_rate, n=20)
 
 prescription_rate_top20_fianl <- prescription_rate_top20 %>%
 select (- total_enroll)
 
 prescription_rate_top20_fianl2 <- prescription_rate_top20_fianl[, c("State", "Total_Prescriptions", "average_enroll", "prescription_rate", "Total_Reimbursed")] %>%
 rename(
 "State" = State,
 "Number of Prescriptions" = Total_Prescriptions ,
 "Average number of enrollment" = average_enroll ,
 "Number of Prescriptions per 100k enrollees" = prescription_rate,
 "Total amount of Reimbursement" = Total_Reimbursed)
 
 print(prescription_rate_top20_fianl2)
## # A tibble: 20 × 5
 ## State Number of Prescripti…¹ Average number of en…² Number of Prescripti…³
 ## <chr> <int> <dbl> <dbl>
 ## 1 South D… 158 135322. 117. 
 ## 2 Iowa 299 600323. 49.8 
 ## 3 New York 2836 6887032. 41.2 
 ## 4 Utah 98 341630. 28.7 
 ## 5 Tenness… 369 1509741. 24.4 
 ## 6 Arkansas 183 803782 22.8 
 ## 7 Colorado 271 1197415. 22.6 
 ## 8 West Vi… 115 512229. 22.5 
 ## 9 North C… 704 3155162. 22.3 
 ## 10 Connect… 235 1069420. 22.0 
 ## 11 Kentucky 271 1352555 20.0 
 ## 12 Oklahoma 184 971726. 18.9 
 ## 13 Louisia… 330 1837564. 18.0 
 ## 14 Indiana 315 1806831. 17.4 
 ## 15 Nebraska 58 342224. 16.9 
 ## 16 Virginia 202 1754907. 11.5 
 ## 17 Georgia 239 2104567. 11.4 
 ## 18 Massach… 205 1978650. 10.4 
 ## 19 Texas 405 4370997. 9.27
 ## 20 South C… 98 1262750. 7.76
 ## # ℹ abbreviated names: ¹​`Number of Prescriptions`,
 ## # ²​`Average number of enrollment`,
 ## # ³​`Number of Prescriptions per 100k enrollees`
 ## # ℹ 1 more variable: `Total amount of Reimbursement` <dbl>
write.csv(prescription_rate_top20_fianl2, "prescription_rate_top20_fianl_2.csv", row.names = FALSE)
