library("randomForestSRC")
library("survival")
library("readr")
library("pec")
library("ranger")

data <- read_csv("data_ready_final.csv")


data <- as.data.frame(data)


RF_obj <- rfsrc(Surv(time_frame,GRF_STAT_PA)~., data, ntree = 50,  membership = TRUE,
                importance=TRUE)
# Calculate variable importance

# Print the RSF model summary
print(RF_obj)



# Get the variable importance from the RSF model
var_imp <- RF_obj$importance

# Calculate the Relative Variable Importance (RVI) as a percentage of the total
rvi <- (var_imp / sum(var_imp)) * 100

# Calculate the cumulative sum of RVI
cumulative_rvi <- cumsum(rvi)

# Create a data frame with Feature names, VarImp, RVI, and Cumulative Sum
feature_importance_df <- data.frame(
  Feature = names(var_imp),
  VarImp = var_imp,
  RVI = rvi,
  Cumulative_RVI = cumulative_rvi
)

# Sort the table by variable importance (descending order)
feature_importance_df <- feature_importance_df[order(-feature_importance_df$VarImp), ]

# Print the feature importance table
print(feature_importance_df)
