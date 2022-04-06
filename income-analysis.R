# Load libraries and refresh environment after each code execution
rm(list = ls())
library(ggplot2)
library(Hmisc)

column_names = c("age","worker_class", "industry_code", "occupation_code", "education",
    "wage_per_hour", "enrolled_in_edu_inst_last_wk", "marital_status", "industry_type", "occupation_type",
    "race", "hispanic_origin", "sex", "member_of_labour_union", "reason_for_unemployment",
    "full_or_part_time_employment_stat", "capital_gains", "capital_losses", "divdends_from_stocks",
    "tax_filer_status", "region_previous_residence", "state_of_previous_residence", "householder_status",
    "detailed_household_summary_in_household", "instance_weight", "migration_code_change_in_msa",
    "migration_code_change_in_reg", "migration_code_move_within_reg", "live_in_this_house_1_year_ago",
    "migration_prev_res_in_sunbelt", "num_persons_worked_for_employer", "family_members_under_18",
    "country_of_birth_father", "country_of_birth_mother", "country_of_birth", "citizenship",
    "own_business_or_self_employed", "fill_inc_questionnaire_for_veterans_admin", "veteran_benefits",
    "weeks_worked_in_year", "year", "income_threshold")

# Read in dataset and include column names
raw_census_dataset <- read.table("census-income.data", header=FALSE, sep=',', 
                                 strip.white=TRUE, col.names=column_names, fill=FALSE)

# Cleaning function to clean dataset
clean_income_dataset <- function(dataset_data){
  names(dataset_data) <- column_names 
  
  # Remove individuals under 16
  dataset_data <- dataset_data[dataset_data$age >= 16, ]
  
  # Remove 'Not in universe' occupation types
  dataset_data <- dataset_data[dataset_data$occupation_type != 'Not in universe', ]
  
  # Removing non-desirable features
  dataset_data$occupation_code <- NULL
  dataset_data$year <- NULL
  dataset_data$own_business_or_self_employed <- NULL
  dataset_data$occupation_type <- NULL
  dataset_data$industry_code <- NULL
  dataset_data$state_of_previous_residence <- NULL
  dataset_data$region_previous_residence <- NULL
  dataset_data$tax_filer_status <- NULL
  dataset_data$reason_for_unemployment <- NULL
  num_persons_worked_for_employer <- NULL
  dataset_data$live_in_this_house_1_year_ago <- NULL
  dataset_data$migration_code_change_in_msa <- NULL
  dataset_data$migration_code_change_in_reg <- NULL
  dataset_data$migration_code_move_within_reg <- NULL
  dataset_data$migration_prev_res_in_sunbelt <- NULL
  dataset_data$hispanic_origin <- NULL
  dataset_data$veteran_benefits <- NULL
  dataset_data$full_or_part_time_employment_stat <- NULL
  dataset_data$detailed_household_and_family_stat <- NULL
  dataset_data$fill_inc_questionnaire_for_veterans_admin <- NULL
  dataset_data$enrolled_in_edu_inst_last_wk <- NULL
  dataset_data$member_of_labour_union <- NULL
  dataset_data$family_members_under_18 <- NULL
  
  # Remove NA, ? values
  question_mark <- dataset_data == " ?"
  data_set_data <- na.omit(dataset_data)

  return(dataset_data)
}

# Clean our raw dataset
census_income <- clean_income_dataset(raw_census_dataset)


# Compare income levels to ages

