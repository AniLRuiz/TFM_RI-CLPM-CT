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
