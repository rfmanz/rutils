library(rutils)
train  = fread(cmd = 'unzip -p /home/r/Downloads/archive.zip  telecom_users.csv')


# customerID - customer id
# gender - client gender (male / female)
# SeniorCitizen - is the client retired (1, 0)
# Partner - is the client married (Yes, No)
# tenure - how many months a person has been a client of the company
# PhoneService - is the telephone service connected (Yes, No)
# MultipleLines - are multiple phone lines connected (Yes, No, No phone service)
# InternetService - client's Internet service provider (DSL, Fiber optic, No)
# OnlineSecurity - is the online security service connected (Yes, No, No internet service)
# OnlineBackup - is the online backup service activated (Yes, No, No internet service)
# DeviceProtection - does the client have equipment insurance (Yes, No, No internet service)
# TechSupport - is the technical support service connected (Yes, No, No internet service)
# StreamingTV - is the streaming TV service connected (Yes, No, No internet service)
# StreamingMovies - is the streaming cinema service activated (Yes, No, No internet service)
# Contract - type of customer contract (Month-to-month, One year, Two year)
# PaperlessBilling - whether the client uses paperless billing (Yes, No)
# PaymentMethod - payment method (Electronic check, Mailed check, Bank transfer (automatic), Credit card (automatic))
# MonthlyCharges - current monthly payment
# TotalCharges - the total amount that the client paid for the services for the entire time
# Churn - whether there was a churn (Yes or No)


plot_bar(train[,-c("customerID","Churn","V1")])
str(train[,-c("customerID","Churn","V1")])

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

ggplot(train, aes(tenure, TotalCharges)) +
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
         #Levels = lapply(.SD, function(x) length(levels(as.factor(x)))),
         Unique_values = fifelse(lapply(.SD, function(x) length(levels(as.factor(x)))) > 5, lapply(.SD, function(x) length(levels(as.factor(x)))), lapply(.SD, function(x)levels(as.factor(x))))),
      .SDcols = is.character]



describe_df(r.train)
cat(names(table(as.factor(train$PaymentMethod))),sep = "|")



prop.table(table(is.na(train$TotalCharges)))*100

describe_df = function(df){
print("Numeric Variables")
df[,.(Names = names(.SD),
        Min = lapply(.SD,function(x) min(x,na.rm=TRUE)),
        Mean = lapply(.SD,function(x)mean(x,na.rm=TRUE)),
        Mode= lapply(.SD,fmode),
        pct_Nulls = lapply(.SD, sum(is.na))),
     .SDcols = is.numeric]}

describe_df(train)

Mode(train$tenure)
describe(train$tenure)
table(cut2(train$tenure,cuts = c(1,4,8,9)))

mfv(train$StreamingMovies, na_rm = TRUE)
str(train)

for (j in which(sapply(${1:object},class)=='character')) {

  set(${1:object},j=j, value = toupper(trimws(iconv(${1:object}[[j]],from = "UTF-8", to = "LATIN1"))))

  set(${1:object}, i = which(is.na(${1:object}[[j]])|is.null(${1:object}[[j]])|${1:object}[[j]]==" "|${1:object}[[j]]=="NULL"|${1:object}[[j]]=="NA"|${1:object}[[j]]==""), j = j, value = NA_char

      acter_)

}


plot_bar(train$Churn)

str(train_nonulls2encoded)


regression = glm(train_nonulls2encoded$Churn~.,train_nonulls2encoded[,-"Churn"], family = binomial)



summary(regression)


train_nonulls2[,lapply(.SD, is.null)]


train_nonulls[]
train[,.SD,.SDcols = is.numeric]




str(train)
rutils::pct_nulls(train)
table(train$PhoneService)
pct_nulls

