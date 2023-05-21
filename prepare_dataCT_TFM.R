# Prepare dataframe to apply bivariate CT  -----

load("../../df_tfm.rds") 

# df = long df with variables
# df_person  list of people 

# Prepare dataframe ----

## Total sample ----------

# with 5 or more measures per variable

length(df_person) #15241 participants

bd <- lapply(df_person, function(x) if (sum(!is.na(x$MHI)) >= 5){
  bd <- x
  return(x)
}
)

bd <- lapply(bd, function(x) if (sum(!is.na(x$NFC)) >= 5){
  bd <- x
  return(x)
}
)

bd <- bd[-which(sapply(bd, is.null))] #remove NULL
length(bd) # we're left with 3575 people of original 15241 (with 5 measures) 

which(sapply(bd, function(x) {any(is.na(x$sem))})) # subject 1426 doesn't have
# time data in one row so we remove it

bd[[2202]] <- bd[[2202]][-1, ] #fixed

length(bd)

# Prepare data WITHOUT 2019, 2020, 2021 ------------

names(df)
table(df$wave)
df <- df[-which(df$wave == c("nov19", "dic20")), ]
table(df$wave)

df_person <- list()
id <- df$nomem_encr # all users id
id <- unique(id) 
length(id) #15171

for (i in 1:length(id)) {
  df_person[[i]] <- df[which(df$nomem_encr == id[i]), ]
} 

length(df_person)

# 6 or more measures per person

bd <- lapply(df_person, function(x) if (sum(!is.na(x$MHI)) >= 6){
  bd <- x
  return(x)
}
)

bd <- lapply(bd, function(x) if (sum(!is.na(x$NFC)) >= 6){
  bd <- x
  return(x)
}
)

bd <- bd[-which(sapply(bd, is.null))] #remove NULL
length(bd) # we're left with 2284 people of original 15241

length(bd)
bd[[2]]

bd


summary(df$sem)

