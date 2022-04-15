# Load libraries and refresh environment after each code execution
rm(list = ls())
graphics.off()

# Load libraries
library(tidyverse)
library(Hmisc)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(skimr)
library(mlbench)
library(caret)
library(randomForest)
library(e1071)
library(corrplot)
library(lares)
library(devtools)
library(rpart)
library(nnet)
library(NeuralNetTools)
library(ipred)
library(ROCR)

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

# Read in test dataset and include column names
test_raw_census_dataset <- read.table("census-income.test", header=FALSE, sep=',', 
                                 strip.white=TRUE, col.names=column_names, fill=FALSE)
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

# PCA
census_numeric <- raw_census_dataset %>% dplyr::select(where(is.numeric))
census_pca <- prcomp(census_numeric, center=TRUE, scale=TRUE)
fviz_pca_var(census_pca, col.var="contrib", repel=TRUE)

# Recursive partitioning feature importance 
tree <- rpart(income_threshold~., data=raw_census_dataset, method="class")
rp_important_variables <- varImp(tree)

# Random forest feature importance
question_mark <- raw_census_dataset == "?"
is.na(raw_census_dataset) <- question_mark
rf_imp_df <- na.omit(raw_census_dataset)
rf_imp_df[sapply(rf_imp_df, is.character)] <- lapply(rf_imp_df[sapply(rf_imp_df, is.character)], as.factor)
regressor <- randomForest(income_threshold~., data=rf_imp_df, importance=TRUE)
rf_important_variables <- varImp(regressor)
p1 <- predict(regressor, rf_imp_df)

# Cleaning function to clean dataset
clean_income_dataset <- function(dataset_data){
  
  # Remove individuals under 16
  dataset_data <- dataset_data[dataset_data$age >= 16, ]
  
  # Removing non-desirable features
  dataset_data$year <- NULL
  dataset_data$occupation_code <- NULL
  dataset_data$industry_code <- NULL
  dataset_data$region_previous_residence <- NULL
  dataset_data$migration_code_change_in_msa <- NULL
  dataset_data$migration_code_change_in_reg <- NULL
  dataset_data$migration_code_move_within_reg <- NULL
  dataset_data$migration_prev_res_in_sunbelt <- NULL
  dataset_data$instance_weight <- NULL
  dataset_data$country_of_birth_mother <- NULL
  dataset_data$country_of_birth_father <- NULL
  dataset_data$hispanic_origin <- NULL
  dataset_data$veteran_benefits <- NULL
  dataset_data$detailed_household_and_family_stat <- NULL
  dataset_data$fill_inc_questionnaire_for_veterans_admin <- NULL
  dataset_data$enrolled_in_edu_inst_last_wk <- NULL
  dataset_data$member_of_labour_union <- NULL
  dataset_data$own_business_or_self_employed <- NULL
  dataset_data$live_in_this_house_1_year_ago <- NULL
  dataset_data$state_of_previous_residence <- NULL
  dataset_data$reason_for_unemployment <- NULL
  dataset_data$family_members_under_18 <- NULL
  dataset_data$householder_status <- NULL
  dataset_data$full_or_part_time_employment_stat <- NULL
  
  # Remove 'Not in universe' occupation types
  dataset_data <- dataset_data[dataset_data$occupation_type != 'Not in universe', ]
  
  # Remove NA, ? values
  question_mark <- dataset_data == "?"
  is.na(dataset_data) <- question_mark
  dataset_data <- na.omit(dataset_data)
  
  # Convert strings to factors
  dataset_data[sapply(dataset_data, is.character)] <- lapply(dataset_data[sapply(dataset_data, is.character)], as.factor)
  
  # Combine non-housholder categories
  levels(dataset_data$detailed_household_summary_in_household)[match("Spouse of householder", levels(dataset_data$detailed_household_summary_in_household))] <- "Non-householder"
  levels(dataset_data$detailed_household_summary_in_household)[match("Other relative of householder", levels(dataset_data$detailed_household_summary_in_household))] <- "Non-householder"
  levels(dataset_data$detailed_household_summary_in_household)[match("Child 18 or older", levels(dataset_data$detailed_household_summary_in_household))] <- "Non-householder"
  levels(dataset_data$detailed_household_summary_in_household)[match("Child under 18 never married", levels(dataset_data$detailed_household_summary_in_household))] <- "Non-householder"
  levels(dataset_data$detailed_household_summary_in_household)[match("Nonrelative of householder", levels(dataset_data$detailed_household_summary_in_household))] <- "Non-householder"
  levels(dataset_data$detailed_household_summary_in_household)[match("Group Quarters- Secondary individual", levels(dataset_data$detailed_household_summary_in_household))] <- "Non-householder"
  levels(dataset_data$detailed_household_summary_in_household)[match("Child under 18 never married", levels(dataset_data$detailed_household_summary_in_household))] <- "Non-householder"
  levels(dataset_data$detailed_household_summary_in_household)[match("Child under 18 ever married", levels(dataset_data$detailed_household_summary_in_household))] <- "Non-householder"
  
  # Combine marital status categories
  levels(dataset_data$marital_status)[match("Married-A F spouse present", levels(dataset_data$marital_status))] <- "Married"
  levels(dataset_data$marital_status)[match("Married-spouse absent", levels(dataset_data$marital_status))] <- "Married"
  levels(dataset_data$marital_status)[match("Married-civilian spouse present", levels(dataset_data$marital_status))] <- "Married"
  levels(dataset_data$marital_status)[match("Married-A F spouse present", levels(dataset_data$marital_status))] <- "Married"
  
  # Combine citizenship categories
  levels(dataset_data$citizenship)[match("Native- Born in the United States", levels(dataset_data$citizenship))] <- "US Citizen"
  levels(dataset_data$citizenship)[match("Foreign born- Not a citizen of U S", levels(dataset_data$citizenship))] <- "Non-US Citizen"
  levels(dataset_data$citizenship)[match("Foreign born- U S citizen by naturalization", levels(dataset_data$citizenship))] <- "US Citizen"
  levels(dataset_data$citizenship)[match("Native- Born abroad of American Parent(s)", levels(dataset_data$citizenship))] <- "US Citizen"
  levels(dataset_data$citizenship)[match("Native- Born in Puerto Rico or U S Outlying", levels(dataset_data$citizenship))] <- "US Citizen"
  
  # Change numeric values of num persons worked for employer into categorical 
  dataset_data$num_persons_worked_for_employer <- as.factor(dataset_data$num_persons_worked_for_employer)
  levels(dataset_data$num_persons_worked_for_employer)[match("0", levels(dataset_data$num_persons_worked_for_employer))] <- "Employee"
  levels(dataset_data$num_persons_worked_for_employer)[match("1", levels(dataset_data$num_persons_worked_for_employer))] <- "Under 10"
  levels(dataset_data$num_persons_worked_for_employer)[match("2", levels(dataset_data$num_persons_worked_for_employer))] <- "10-24"
  levels(dataset_data$num_persons_worked_for_employer)[match("3", levels(dataset_data$num_persons_worked_for_employer))] <- "25-99"
  levels(dataset_data$num_persons_worked_for_employer)[match("4", levels(dataset_data$num_persons_worked_for_employer))] <- "100-499"
  levels(dataset_data$num_persons_worked_for_employer)[match("5", levels(dataset_data$num_persons_worked_for_employer))] <- "500-999"
  levels(dataset_data$num_persons_worked_for_employer)[match("6", levels(dataset_data$num_persons_worked_for_employer))] <- "1000+"
  
  # Combine tax filer categories
  levels(dataset_data$tax_filer_status)[match("Joint both under 65", levels(dataset_data$tax_filer_status))] <- "Joint"
  levels(dataset_data$tax_filer_status)[match("Joint one under 65 & one 65+", levels(dataset_data$tax_filer_status))] <- "Joint"
  levels(dataset_data$tax_filer_status)[match("Joint both 65+", levels(dataset_data$tax_filer_status))] <- "Joint"
  
  # Combine worker class 
  levels(dataset_data$worker_class)[match("Self-employed-not incorporated", levels(dataset_data$worker_class))] <- "Self-employed"
  levels(dataset_data$worker_class)[match("Self-employed-incorporated", levels(dataset_data$worker_class))] <- "Self-employed"

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

# Clean both our datasets / feature engineering
census_income <- clean_income_dataset(raw_census_dataset)
test_census_income <- clean_income_dataset(test_raw_census_dataset)
skim(census_income)
skim(test_census_income)

# 

# Neural Networks
census_model_nn <- nnet(income_threshold~., data=census_income, size=8, maxit=500, decay=0.0001)
test_census_income_nn <- test_census_income
test_census_income_nn$pred_nn <- predict(census_model_nn, test_census_income_nn, type="class")
confusionMatrix(table(test_census_income_nn$pred_nn, test_census_income_nn$income_threshold))

# Random Forest
set.seed(256)
census_model_rf <- randomForest(income_threshold~., data=census_income)
rf_prediction <- predict(census_model_rf, census_income)
rf_prediction_test <- predict(census_model_rf, test_census_income)
confusionMatrix(rf_prediction_test, test_census_income$income_threshold)

# Bagging
census_model_b <- bagging(formula=income_threshold~., data=census_income, nbagg=100, coob=TRUE, 
                          control=rpart.control(minsplit = 2, cp=0))
b_prediction <- predict(census_model_b, census_income)
b_prediction_test <- predict(census_model_b, test_census_income)
confusionMatrix(b_prediction_test, test_census_income$income_threshold)

# ROC Curve
# Random Forest
rf_prediction_test.prob <- predict(census_model_rf, test_census_income, type="prob")
rf_prediction_test <- predict(census_model_rf, test_census_income, type="class")
pr_rf <- prediction(rf_prediction_test.prob[,2], test_census_income$income_threshold)
prf_rf <- performance(pr_rf, measure="tpr", x.measure="fpr")
rf_df <- data.frame(FP = prf_rf@x.values[[1]], TP = prf_rf@y.values[[1]])
# Neural Networks
nn_prediction <- predict(census_model_nn, newdata = test_census_income_nn, type = 'raw')
pr_nn <- prediction(nn_prediction, test_census_income_nn$income_threshold)
prf_nn <- performance(pr_nn, measure="tpr", x.measure="fpr")
nn_df <- data.frame(FP = prf_nn@x.values[[1]], TP = prf_nn@y.values[[1]])
# Bagging
rf_roc_prediction_test.prob <- predict(census_model_b, test_census_income, type="prob")
rf_roc_prediction_test <- predict(census_model_rf, test_census_income, type="class")
pr_b <- prediction(rf_roc_prediction_test.prob[,2], test_census_income$income_threshold)
prf_b <- performance(pr_b, measure="tpr", x.measure="fpr")
b_df <- data.frame(FP = prf_b@x.values[[1]], TP = prf_b@y.values[[1]])

# ROC plot
roc_plot <- ggplot() +
  geom_line(data=rf_df, aes(x=FP, y=TP, color="Random Forest")) +
  geom_line(data=nn_df, aes(x=FP, y=TP, color="Neural Network")) +
  geom_line(data=b_df, aes(x=FP, y=TP, color="Bagging")) +
  geom_segment(aes(x=0, xend=1, y=0, yend=1)) +
  ggtitle('ROC Curve plot for Neural Network') +
  labs(x="False Positive Rate", y="True Positive Rate")
roc_plot

# AUC values
auc_nn <- performance(pr_nn, measure="auc")@y.values[[1]]
auc_rf <- performance(pr_rf, measure="auc")@y.values[[1]]
auc_b <- performance(pr_b, measure="auc")@y.values[[1]]
Model <- c("Neural Network", "Random Forest", "Bagging")
auc_values <- c(auc_nn, auc_rf, auc_b)
auc_df <- data.frame(Model, auc_values)
print(auc_df)

