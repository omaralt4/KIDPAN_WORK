library("randomForestSRC")
library("survival")
library("readr")
library("pec")
library("ranger")
library("aorsf")
library("caTools")
library("survAUC")
data <- read_csv("data_ready_45.csv")


data <- as.data.frame(data)

data["time_frame"] = data["time_frame"] + 1

sample = sample.split(data$GRF_STAT_PA, SplitRatio = 0.75)

train = subset(data, sample == TRUE)
test = subset(data, sample == FALSE)

RF_obj <- orsf(formula = Surv(time_frame, GRF_STAT_PA) ~ ., data = train, n_tree = 50,  split_rule = "cstat",
                importance="permute", verbose_progress = TRUE)
# Calculate variable importance


predictions <- predict(RF_obj, new_data = test, pred_type = "surv")


c_index <- concordance(Surv(time_frame, GRF_STAT_PA) ~ predictions)
print(paste("Concordance Index:", c_index))


c_index <- UnoC(Surv(train$time_frame, train$GRF_STAT_PA), 
                Surv(test$time_frame, test$GRF_STAT_PA), 
                predictions)
print(paste("Uno's C-Index:", c_index))
# Print the RSF model summary
print(RF_obj)



# Get the variable importance from the RSF model
var_imp <- RF_obj$importance

# Calculate the Relative Variable Importance (RVI) as a percentage of the total
rvi <- (var_imp / sum(var_imp)) * 100

# Calculate the cumulative sum of RVI


# Create a data frame with Feature names, VarImp, RVI, and Cumulative Sum
feature_importance_df <- data.frame(
  Feature = names(var_imp),
  VarImp = var_imp,
  RVI = rvi
)

# Sort the table by variable importance (descending order)
feature_importance_df <- feature_importance_df[order(-feature_importance_df$VarImp), ]
feature_importance_df$Cumulative_RVI <- cumsum(feature_importance_df$RVI)
# Print the feature importance table
print(feature_importance_df)



concordance_result <- survConcordance(Surv(time_frame,GRF_STAT_PA) ~ predict(RF_obj), data = data)
