# Load libraries and refresh environment after each code execution
rm(list = ls())
graphics.off()

# Load libraries
library(tidyverse)
library(Hmisc)
library(corrplot)
library(FactoMineR)
library(skimr)
library(mlbench)
library(caret)
library(randomForest)
library(e1071)
library(corrplot)
library(lares)
library(rpart)

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
                                 strip.white=TRUE, col.names=column_names, fill=FALSE, stringsAsFactors = TRUE)

# Class of dataset
class(raw_census_dataset)

# Skim function
skim(raw_census_dataset)

# Dimensions of dataset
dim(raw_census_dataset)

# Summarise dataset
summary(raw_census_dataset)

# Statistical description of dataset features
describe(raw_census_dataset)

# Condensed description of unique values
str(raw_census_dataset, vec.len=8)

# Cleaning function to clean dataset
clean_income_dataset <- function(dataset_data){
  
  # Remove individuals under 16
  dataset_data <- dataset_data[dataset_data$age >= 16, ]
  
  # Remove 'Not in universe' occupation types
  dataset_data <- dataset_data[dataset_data$occupation_type != 'Not in universe', ]
  
  # Removing non-desirable features
  dataset_data$year <- NULL
  dataset_data$occupation_type <- NULL
  dataset_data$industry_code <- NULL
  dataset_data$region_previous_residence <- NULL
  dataset_data$tax_filer_status <- NULL
  dataset_data$migration_code_change_in_msa <- NULL
  dataset_data$migration_code_change_in_reg <- NULL
  dataset_data$migration_code_move_within_reg <- NULL
  dataset_data$migration_prev_res_in_sunbelt <- NULL
  dataset_data$instance_weight <- NULL
  dataset_data$country_of_birth_mother <- NULL
  dataset_data$country_of_birth_father <- NULL
  
  # Maybe delete these
  dataset_data$hispanic_origin <- NULL
  dataset_data$veteran_benefits <- NULL
  dataset_data$full_or_part_time_employment_stat <- NULL
  dataset_data$detailed_household_and_family_stat <- NULL
  dataset_data$fill_inc_questionnaire_for_veterans_admin <- NULL
  dataset_data$enrolled_in_edu_inst_last_wk <- NULL
  dataset_data$member_of_labour_union <- NULL
  dataset_data$family_members_under_18 <- NULL
  dataset_data$own_business_or_self_employed <- NULL
  dataset_data$live_in_this_house_1_year_ago <- NULL
  dataset_data$reason_for_unemployment <- NULL
  dataset_data$state_of_previous_residence <- NULL
  
  # Remove NA, ? values
  question_mark <- dataset_data == " ?"
  data_set_data <- na.omit(dataset_data)
  
  # Combine non-housholder categories
  levels(dataset_data$detailed_household_summary_in_household)[match("Spouse of householder", levels(dataset_data$detailed_household_summary_in_household))] <- "Non-householder"
  levels(dataset_data$detailed_household_summary_in_household)[match("Other relative of householder", levels(dataset_data$detailed_household_summary_in_household))] <- "Non-householder"
  levels(dataset_data$detailed_household_summary_in_household)[match("Child 18 or older", levels(dataset_data$detailed_household_summary_in_household))] <- "Non-householder"
  levels(dataset_data$detailed_household_summary_in_household)[match("Child under 18 never married", levels(dataset_data$detailed_household_summary_in_household))] <- "Non-householder"
  levels(dataset_data$detailed_household_summary_in_household)[match("Nonrelative of householder", levels(dataset_data$detailed_household_summary_in_household))] <- "Non-householder"
  levels(dataset_data$detailed_household_summary_in_household)[match("Group Quarters- Secondary individual", levels(dataset_data$detailed_household_summary_in_household))] <- "Non-householder"
  levels(dataset_data$detailed_household_summary_in_household)[match("Child under 18 never married", levels(dataset_data$detailed_household_summary_in_household))] <- "Non-householder"
  levels(dataset_data$detailed_household_summary_in_household)[match("Child under 18 ever married", levels(dataset_data$detailed_household_summary_in_household))] <- "Non-householder"
  
  # Change numeric values of num persons worked for employer into categorical 
  dataset_data[which(dataset_data$num_persons_worked_for_employer ==0), 'num_persons_worked_for_employer'] = 'Employee'
  dataset_data[which(dataset_data$num_persons_worked_for_employer ==1), 'num_persons_worked_for_employer'] = 'Under 10'
  dataset_data[which(dataset_data$num_persons_worked_for_employer ==2), 'num_persons_worked_for_employer'] = '10-24'
  dataset_data[which(dataset_data$num_persons_worked_for_employer ==3), 'num_persons_worked_for_employer'] = '25-99'
  dataset_data[which(dataset_data$num_persons_worked_for_employer ==4), 'num_persons_worked_for_employer'] = '100-499'
  dataset_data[which(dataset_data$num_persons_worked_for_employer ==5), 'num_persons_worked_for_employer'] = '500-999'
  dataset_data[which(dataset_data$num_persons_worked_for_employer ==6), 'num_persons_worked_for_employer'] = '1000+'
  

  return(dataset_data)
}

# Education bar chart
education_level <- c("Children", "Less than 1st grade", "1st 2nd 3rd or 4th grade", "5th or 6th grade",
                     "7th and 8th grade", "9th grade", "10th grade", "11th grade", "12th grade no diploma",
                     "High school graduate", "Some college but no degree", "Associates degree-academic program",
                     "Associates degree-occup /vocational", "Bachelors degree(BA AB BS)", 
                     "Prof school degree (MD DDS DVM LLB JD)", "Masters degree(MA MS MEng MEd MSW MBA)", 
                     "Doctorate degree(PhD EdD)")
ggplot(raw_census_dataset, aes(y=factor(education, levels=education_level))) +
  geom_bar(aes(fill=income_threshold)) +
  labs(title="Frequency of different Education levels", x="Frequency", y="Education", fill="Income Threshold")

# Industry bar chart
ggplot(raw_census_dataset, aes(y=factor(industry_type))) +
  geom_bar(aes(fill=income_threshold)) +
  labs(title="Frequency of different Industry types", x="Frequency", y="Industry", fill="Income Threshold")

# Income threshold proportions fill plot compared to industry types
plotdata_industry_pct <- raw_census_dataset %>%
  dplyr::group_by(industry_type, income_threshold) %>%
  dplyr::summarize(n = n()) %>%
  dplyr::mutate(pct = n/sum(n), lbl=scales::percent(pct))
plotdata_industry_pct
ggplot(plotdata_industry_pct,  aes(x = industry_type, y=pct,  fill = income_threshold)) + 
  geom_bar(stat="identity", position="fill") +
  geom_text(aes(label = lbl),  size = 3,  position = position_stack(vjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1),  labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(title="Income threshold proportions compared to different industry types", 
       x="Industry type", y="Proportion", fill="Income Threshold") +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))

# Box plot of ages compared to income thresholds
boxplot(age~income_threshold, data=raw_census_dataset, main="Ages compared to income thresholds", 
        xlab="Income Threshold", ylab="Ages", col="deepskyblue", border="deepskyblue4")

# Income threshold proportions fill plot compared to race categories
plotdata_race_pct <- raw_census_dataset %>%
  dplyr::group_by(race, income_threshold) %>%
  dplyr::summarize(n = n()) %>%
  dplyr::mutate(pct = n/sum(n), lbl=scales::percent(pct))
plotdata_race_pct
ggplot(plotdata_race_pct,  aes(x = race, y=pct,  fill = income_threshold)) + 
  geom_bar(stat="identity", position="fill") +
  geom_text(aes(label = lbl),  size = 3,  position = position_stack(vjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1),  labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(title="Income threshold proportions compared to different races", x="Race", y="Proportion", fill="Income Threshold") +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
  
# Income threshold frequency bar plot compared to different races with percentage labels
ggplot(raw_census_dataset %>% 
         dplyr::group_by(race, income_threshold) %>%
         dplyr::summarize(n = n()) %>%
         dplyr::mutate(pct=n/sum(n)),
       aes(race, n, fill=income_threshold)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100), "%")),
            position=position_stack(vjust=0.8),
            size = 2.5) +
  labs(title="Income threshold frequency compared to different races", x="Race", 
       y="Frequency", fill="Income Threshold")

# Income threshold frequency bar plot compared to different sex with percentage labels
ggplot(raw_census_dataset %>% 
         dplyr::group_by(sex, income_threshold) %>%
         dplyr::summarize(n = n()) %>%
         dplyr::mutate(pct=n/sum(n)),
       aes(sex, n, fill=income_threshold)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100), "%")),
            position=position_stack(vjust=0.8), size = 2.5) +
  labs(title="Income threshold frequency compared to sex", x="Sex", 
       y="Frequency", fill="Income Threshold")

# Clean our dataset / feature engineering
census_income <- clean_income_dataset(raw_census_dataset)

# top 10 most ranked cross-correlations
numeric_raw_census <- select_if(census_income, is.numeric)
corr_cross(numeric_raw_census, max_pvalue=0.05, top=10)

# Clean our dataset / feature engineering
census_income <- clean_income_dataset(raw_census_dataset)

# Decision tree 
tree <- rpart(income_threshold~., data=raw_census_dataset, method="class")
important_variables <- varImp(tree)
important_variables$feature <- rownames(important_variables)
rownames(important_variables) <- NULL
print(important_variables)

important_variables <- important_variables[order(-important_variables$Overall),]
