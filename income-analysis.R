rm(list = ls()) # Refresh environment after each code execution

census_income <- read.table("census-income.data", sep=',', strip.white=TRUE, fill=FALSE)
colnames(census_income) <- c(
  "age",
  "worker_class",
  "industry_code",
  "occupation_code",
  "education",
  "wage_per_hour",
  "enrolled_in_edu_inst_last_wk",
  "marital_status",
  "industry_type",
  "occupation_type",
  "race",
  "hispanic_origin",
  "sex",
  "member_of_labour_union",
  "reason_for_unemployment",
  "full_or_part_time_employment_stat",
  "capital_gains",
  "capital_losses",
  "divdends_from_stocks",
  "tax_filer_status",
  "region_previous_residence",
  "state_of_previous_residence",
  "detailed_household_and_family_stat",
  "detailed_household_summary_in_household",
  "instance_weight",
  "migration_code_change_in_msa",
  "migration_code_change_in_reg",
  "migration_code_move_within_reg",
  "live_in_this_house_1_year_ago",
  "migration_prev_res_in_sunbelt",
  "num_persons_worked_for_employer",
  "family_members_under_18",
  "country_of_birth_father",
  "country_of_birth_mother",
  "country_of_birth",
  "citizenship",
  "own_business_or_self_employed",
  "fill_inc_questionnaire_for_veterans_admin",
  "veteran_benefits",
  "weeks_worked_in_year",
  "year",
  "income_threshold"
  )

census_income <- census_income[census_income$age >= 18,]
census_income$occupation_code <- NULL
census_income$industry_code <- NULL
census_income$migration_code_change_in_msa <- NULL
census_income$migration_code_change_in_reg <- NULL
census_income$migration_code_move_within_reg <- NULL
census_income$migration_prev_res_in_sunbelt <- NULL
census_income$hispanic_origin <- NULL
census_income$fill_inc_questionnaire_for_veterans_admin <- NULL