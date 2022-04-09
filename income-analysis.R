# Load libraries and refresh environment after each code execution
(list = ls())
graphics.off()
library(tidyverse)
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

# Summarise dataset
summary(raw_census_dataset)

# Statistical description of dataset features
describe(raw_census_dataset)

# Cleaning function to clean dataset
clean_income_dataset <- function(dataset_data){
  
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
  
  # Change numeric values into categorical 
  dataset_data[which(dataset_data$num_persons_worked_for_employer ==0), 'num_persons_worked_for_employer'] = 'Not in universe'
  dataset_data[which(dataset_data$num_persons_worked_for_employer ==1), 'num_persons_worked_for_employer'] = 'Under 10'
  dataset_data[which(dataset_data$num_persons_worked_for_employer ==2), 'num_persons_worked_for_employer'] = '10-24'
  dataset_data[which(dataset_data$num_persons_worked_for_employer ==3), 'num_persons_worked_for_employer'] = '25-99'
  dataset_data[which(dataset_data$num_persons_worked_for_employer ==4), 'num_persons_worked_for_employer'] = '100-499'
  dataset_data[which(dataset_data$num_persons_worked_for_employer ==5), 'num_persons_worked_for_employer'] = '500-999'
  dataset_data[which(dataset_data$num_persons_worked_for_employer ==6), 'num_persons_worked_for_employer'] = '1000+'
  
  # Change ? countries to 'Unknown'
  dataset_data[which(dataset_data$country_of_birth =='?'), 'country_of_birth'] = 'Unknown'
  dataset_data[which(dataset_data$country_of_birth_father =='?'), 'country_of_birth_father'] = 'Unknown'
  dataset_data[which(dataset_data$country_of_birth_mother =='?'), 'country_of_birth_mother'] = 'Unknown'

  return(dataset_data)
}

# Clean our raw dataset
census_income <- clean_income_dataset(raw_census_dataset)

# Education bar chart
education_level <- c("Children", "Less than 1st grade", "1st 2nd 3rd or 4th grade", "5th or 6th grade",
                     "7th and 8th grade", "9th grade", "10th grade", "11th grade", "12th grade no diploma",
                     "High school graduate", "Some college but no degree", "Associates degree-academic program",
                     "Associates degree-occup /vocational", "Bachelors degree(BA AB BS)", 
                     "Prof school degree (MD DDS DVM LLB JD)", "Masters degree(MA MS MEng MEd MSW MBA)", 
                     "Doctorate degree(PhD EdD)")
ggplot(raw_census_dataset, aes(y=factor(education, levels=education_level))) +
  geom_bar(aes(fill = income_threshold)) +
  labs(title="Frequency of different Education levels", x="Frequency", y="Education", fill="Income Threshold")

# Box plot of ages compared to income thresholds
boxplot(age~income_threshold, data=raw_census_dataset, main="Ages compared to income thresholds", 
        xlab="Income Threshold", ylab="Ages", col="deepskyblue", border="deepskyblue4")


# Income threshold proportions fill plot compared to race categories
plotdata_race_pct <- raw_census_dataset %>%
  group_by(race, income_threshold) %>%
  summarize(n = n()) %>%
  mutate(pct = n/sum(n), lbl=scales::percent(pct))
plotdata_race_pct
ggplot(plotdata_race_pct,  aes(x = race, y=pct,  fill = income_threshold)) + 
  geom_bar(stat="identity", position="fill") +
  geom_text(aes(label = lbl),  size = 3,  position = position_stack(vjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1),  labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(title="Income threshold proportions compared to different races", x="Race", y="Proportion", fill="Income Threshold") +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
