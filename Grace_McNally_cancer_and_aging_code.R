library(readr)
library(gtsummary)
library(tidyverse)
library(dplyr)

#add table labels caption function 
#one decimal place convention across the tables

# Build a basic, overall summary table
midus_table_1 <- midus_data %>%
  # 1. Select the primary variables (excluding race_eth since we aren't stratifying by it)
  select(
    grimage2, dunedinpace, bmi, age, 
    sex, marital, education, income, num_chron, 
    smoking, alcohol, famcancer, cancerrisk, efficacy
  ) %>%
  # 2. Build the summary table for the entire sample
  tbl_summary(
    # Force discrete risk/efficacy scores to display as continuous numeric averages
    type = list(
      c(grimage2, dunedinpace, bmi, age, income, num_chron, cancerrisk, efficacy) ~ "continuous"
    ),
    
    # Use standard Mean (SD) for continuous, and clean fractions for categorical
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    
    # Hide missing/Unknown rows to keep the layout crisp
    missing = "ifany",
  
  # 3. Apply the same professional labels
  label = list(
    grimage2 ~ "GrimAge",
    dunedinpace ~ "DunedinPACE",
    bmi ~ "BMI (kg/m²)",
    age ~ "Patient Age",
    sex ~ "Sex",
    marital ~ "Marital Status",
    education ~ "Education Level",
    income ~ "Annual Income ($)",
    num_chron ~ "Number of Chronic Conditions",
    smoking ~ "Smoking Status",
    alcohol ~ "Alcohol Consumption",
    famcancer ~ "Family History of Cancer",
    cancerrisk ~ "Cancer Risk Score",
    efficacy ~ "Self-Efficacy Score"
   ))%>%
  bold_labels()

# 4. Save your basic table to a Word Document
as_gt(midus_table_1) %>% 
  gt::gtsave(filename = "midus_table_1_basic.docx")


#Table 2 
#need to add p value
#add overall 

midus_data <- readRDS("Data/midus_cancer_aging_analysis_dataset.rds")


table1 <- midus_data %>%
  select(race_eth, grimage2, dunedinpace, bmi, age, 
         sex, marital, education, income, num_chron, 
         smoking, alcohol, famcancer, cancerrisk, efficacy) %>%
  tbl_summary(
    by = race_eth,
    type = list(
      c(grimage2, dunedinpace, bmi, age, income, num_chron, cancerrisk, efficacy) ~ "continuous2"
    ), #continuous 2 splits into N and Mean (SD) rows 
    # Define the exact layout styles from your example image
    statistic = list(
      all_continuous() ~ c("{N_nonmiss}", "{mean} ({sd})"), # Separate rows for N and Mean
      all_categorical() ~ "{n} ({p}%)"                # Clean fraction format
    ),
    #hiding missing values??
    missing = "ifany", 
    #applying clear labels 
    
    #do we want to use all these variables?? 
    label = list(
      grimage2 ~ "GrimAge", #outcomes 
      dunedinpace ~ "DunedinPACE", #outcomes 
      bmi ~ "BMI (kg/m²)",
      age ~ "Patient Age",
      sex ~ "Sex",
      marital ~ "Marital Status",
      education ~ "Education Level",
      income ~ "Annual Income ($)",
      num_chron ~ "Number of Chronic Conditions",
      smoking ~ "Smoking Status",
      alcohol ~ "Alcohol Consumption",
      famcancer ~ "Family History of Cancer",
      cancerrisk ~ "Cancer Risk Score", #is this the perceived cancer risk?
      efficacy ~ "Self-Efficacy Score")
    
  )%>%
  bold_labels()

table1   

as_gt(table1) %>% 
  gt::gtsave(filename = "midus_table_1_final.docx")
