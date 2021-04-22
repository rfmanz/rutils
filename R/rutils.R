# Find nas by column----
pct_nulls = function(df)
  df[,.(names = names(df), count_nulls = lapply(.SD, function(x) sum(is.na(x))), pct= lapply(.SD, function(x) scales::percent(sum(is.na(x))/nrow(df),0.01)))][order(as.numeric(count_nulls)),]

