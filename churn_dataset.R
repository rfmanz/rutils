library(rutils)
# Load --------------------------------------------------------------------

 all_data  = fread(cmd = 'unzip -p /home/r/Downloads/archive.zip  telecom_users.csv',drop = "V1", stringsAsFactors = TRUE)

# FE ----------------------------------------------------------------------


all_data = setDT((churn_recipe =
    all_data %>%
    recipe(Churn~ .) %>%
    step_rm(customerID) %>%
    step_impute_knn(all_predictors(), neighbors = 6) %>%
    step_normalize(all_numeric()) %>%
    step_integer(all_nominal_predictors()) %>%
    prep(all_data) %>%
    bake(all_data))
    )
levels(all_data$Churn) = c(0,1)
all_data
# Split -------------------------------------------------------------------


split  = initial_split(all_data, prop = 0.8)
tr  = training(split)
te   = testing(split)

# dim(all_data)
# dim(tr)
# dim(te)


# model -------------------------------------------------------------------

# logistic baseline -----
#regression = glm(tr$Churn~.,tr[,-"Churn"], family = binomial)

log_reg <- train(
  Churn ~ .,
  data = tr,
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)

mean(te$Churn== predict(log_reg, te))

# caret gbm ---------------------------------------------------------------

boosted <- train(
  Churn ~ .,
  data = tr,
  method = "gbm",
  trControl = trainControl(method = "repeatedcv", number = 10,repeats = 10),
  verbose= FALSE
)
boosted
mean(te$Churn== predict(boosted, te))



# python lgbm -------------------------------------------------------------





library(lightgbm)
# lgbm ----
oob <- numeric(nrow(tr))
pred <- numeric(nrow(te))
imp <- tibble()
scores <- c()
kfolds <- 5
y= all_data$Churn



tr = as.matrix(tr)


for (rs in vfold_cv(tr, kfolds)$splits) {
  cat("Fold id:", rs$id$id, "\n")
  tri <- rs$in_id

  m_lgb <- lgb.train(params = list(objective = "binary",
                                   metric = "auc",
                                   feature_pre_filter = FALSE,
                                   learning_rate = 0.0035,
                                   num_leaves = 120,
                                   min_child_samples = 75,
                                   sub_feature = 0.4,
                                   sub_row = 1,
                                   subsample_freq = 0,
                                   lambda_l1 = 4.6,
                                   lambda_l2 = 1.9),
                     data = lgb.Dataset(tr[tri, ], label = y[tri]),
                     nrounds = 30000,
                     valids = list(val = lgb.Dataset(tr[-tri, ], label = y[-tri])),
                     early_stopping_rounds = 350,
                     eval_freq = 700,
                     verbose = -1)

  oob[-tri] <- predict(m_lgb, tr[-tri, ])
  pred <- pred + predict(m_lgb, te) / kfolds

  imp <- bind_rows(imp, lgb.importance(m_lgb))

  scores <- c(scores, m_lgb$best_score)
  cat("\tScore:", tail(scores, 1), "\n")
}



churn_recipe =  recipe(Churn ~ ., data = train)

churn_recipe = step_knnimpute(all_predictors(), neighbors = 6)

train[,.N,Churn]

describe_df(train)

plot_bar(train[,-c("customerID","Churn","V1")])
str(train[,-c("customerID","Churn","V1")])


trainIndex <- createDataPartition(train$Churn, p = .8,
                                  list = FALSE,
                                  times = 1)



split_1  <- initial_split(train, prop = 0.7)
train_3  <- training(split_1)
test_3   <- testing(split_1)



#Demographic features
train[,list(gender,SeniorCitizen,Partner,Dependents)]

#Service features
train[,.(PhoneService,MultipleLines,InternetService,)]
train[,.(OnlineBackup,OnlineSecurity,DeviceProtection,TechSupport)]
train[,.(StreamingTV,StreamingMovies)]
train[,.(Contract,MonthlyCharges,TotalCharges,tenure)]
train[,list(PaperlessBilling,PaymentMethod)]

train[is.na(TotalCharges),]
train[,describe(TotalCharges)]

ggplot(all_data, aes(tenure, TotalCharges)) +
  geom_point(pch = 21, fill = "gray25", color = "white", size = 1.5) +
  scale_x_continuous(name = "tenure") +
  scale_y_continuous(name = "TotalCharges")

train_nonulls = train[!is.na(TotalCharges)]
str(train_nonulls)
train_nonulls2 =  train_nonulls[,-c("customerID","Churn","V1")]

library(reticulate)
train_nonulls2encoded = as.data.table(py$train)

print("Numeric Variables")
train[,.(Names = names(.SD),
         Min = lapply(.SD,function(x) round(min(x,na.rm=TRUE),0)),
         Mean = lapply(.SD,function(x) round(mean(x,na.rm=TRUE),0)),
         Max = lapply(.SD,function(x) round(max(x,na.rm=TRUE),0)),
         Mode= lapply(.SD, function(x) round(fmode(x),0)),
        Pct_Nulls = lapply(.SD, function(x) percent(sum(is.na(x))/nrow(.SD),0.01)),
        Count_Nulls = lapply(.SD, function(x) sum(is.na(x))),
      Count_Not_Null = lapply(.SD, function(x) sum(!is.na(x)))),
    .SDcols = is.numeric]

print("Categorical Variables")

train[,customerID:=NULL]
train[,.(Names = names(.SD),
         Pct_Nulls = lapply(.SD, function(x) percent(sum(is.na(x))/nrow(.SD),0.01)),
         Count_Nulls = lapply(.SD, function(x) sum(is.na(x))),
         Count_Not_Null = lapply(.SD, function(x) sum(!is.na(x))),
         Levels = lapply(.SD, function(x) length(levels(as.factor(x)))),
         Unique_values = fifelse(lapply(.SD, function(x) length(levels(as.factor(x)))) > 5, lapply(.SD, function(x) length(levels(as.factor(x)))), lapply(.SD, function(x)levels(as.factor(x))))),
      .SDcols = is.character]



describe_df(r.train)
cat(names(table(as.factor(train$PaymentMethod))),sep = "|")

knitr::kable(train)

prop.table(table(is.na(train$TotalCharges)))*100

describe_df = function(df){
   numeric = df[,.(Names = names(.SD),
           Min = lapply(.SD,function(x) round(min(x,na.rm=TRUE),0)),
           Mean = lapply(.SD,function(x) round(mean(x,na.rm=TRUE),0)),
           Max = lapply(.SD,function(x) round(max(x,na.rm=TRUE),0)),
           Mode= lapply(.SD, function(x) round(fmode(x),0)),
           Pct_Nulls = lapply(.SD, function(x) percent(sum(is.na(x))/nrow(.SD),0.01)),
           Count_Nulls = lapply(.SD, function(x) sum(is.na(x))),
           Count_Not_Null = lapply(.SD, function(x) sum(!is.na(x)))),
        .SDcols = is.numeric]

  categorical = df[,.(Names = names(.SD),
           Pct_Nulls = lapply(.SD, function(x) percent(sum(is.na(x))/nrow(.SD),0.01)),
           Count_Nulls = lapply(.SD, function(x) sum(is.na(x))),
           Count_Not_Null = lapply(.SD, function(x) sum(!is.na(x))),
           Levels = lapply(.SD, function(x) length(levels(as.factor(x)))),
           Unique_values = fifelse(lapply(.SD, function(x) length(levels(as.factor(x)))) > 5, lapply(.SD, function(x) length(levels(as.factor(x)))), lapply(.SD, function(x)levels(as.factor(x))))),
        .SDcols = is.character]

#output = list(numeric, categorical)
comment(numeric)= '-----------------------------------NUMERIC--------------------------------------'
comment(categorical)= '------------------------------CATEGORICAL---------------------------------------'
for (i in list(numeric, categorical)) {
  print_empty_line()
  print(comment(i))
      print(i)

    print_empty_line()
}
}

describe_df(train)


deparse(substitute(train))
print("Numeric Variables")

print("Categorical Variables")







summary(regression)


train_nonulls2[,lapply(.SD, is.null)]


train_nonulls[]
train[,.SD,.SDcols = is.numeric]




str(train)
rutils::pct_nulls(train)
table(train$PhoneService)
pct_nulls






