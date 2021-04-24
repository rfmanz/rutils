#' @export
# Find nas by column----
pct_nulls = function(df)
  df[,.(names = names(df), count_nulls = lapply(.SD, function(x) sum(is.na(x))), pct= lapply(.SD, function(x) scales::percent(sum(is.na(x))/nrow(df),0.01)))][order(as.numeric(count_nulls)),]
#' @export
# remove all variables
rmlist = function()
  rm(list=ls())
#' @export
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
#' @export
character_tofactor = function(df){
for(j in which(sapply(all_data,class)=='character'))
  set(all_data,,j=j,value = as.factor(all_data[[j]]))}


