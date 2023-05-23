# libraries
library(rio)
library(haven)
library(lubridate) # for time variables
library(data.table)

# 1.IMPORT DATABASES ####

## Background variables ####
### here you should insert the location in your pc to get access to the data
### previously downloaded from de LISS website

nov7 <- import("../nfc/background /1. OTRAS/1. noviembre2007/noviembre2007.sav") # example

#list
background <- list(nov7 = nov7, may8 = may8, dic8 = dic8, may9 = may9, jun9 = jun9, nov9 = nov9, 
                   dic9 = dic9, may10 = may10, dic10 = dic10, may11 = may11,
                   dic11 = dic11, may12 = may12, dic12 = dic12, may13 = may13,
                   jun13 = jun13, dic13 = dic13, may14 = may14, dic14 = dic14,
                   may15 = may15, dic15 = dic15, may16 = may16, dic16 = dic16,
                   may17 = may17, dic17 = dic17, may18 = may18, jun18 = jun18, 
                   dic18 = dic18, nov19 = nov19, nov20 = nov20, dic20 = dic20, 
                   dic21 = dic21)


# change names of the principal variables

sapply(background, names) # they don't have the same number of arguments, so we 
# prepare it

names(background[[1]])[c(1,2,4,7,18,23)] <- c("id", "house", "sex", "age", 
                                              "net_income", "EdLv")

names(background[[2]])[c(1,2,4,7,19,24)] <- c("id", "house", "sex", "age", 
                                              "net_income", "EdLv")

names(background[[3]])[c(1,2,4,7,20,29)] <- c("id", "house", "sex", "age", 
                                              "net_income", "EdLv")
names(background[[4]])[c(1,2,4,7,20,29)] <- c("id", "house", "sex", "age", 
                                              "net_income", "EdLv")
names(background[[5]])[c(1,2,4,7,20,29)] <- c("id", "house", "sex", "age", 
                                              "net_income", "EdLv")
names(background[[6]])[c(1,2,4,7,20,29)] <- c("id", "house", "sex", "age", 
                                              "net_income", "EdLv")
names(background[[7]])[c(1,2,4,7,20,29)] <- c("id", "house", "sex", "age", 
                                              "net_income", "EdLv")
names(background[[8]])[c(1,2,4,7,20,29)] <- c("id", "house", "sex", "age", 
                                              "net_income", "EdLv")
names(background[[9]])[c(1,2,4,7,20,29)] <- c("id", "house", "sex", "age", 
                                              "net_income", "EdLv")

for(i in 10:31){
  names(background[[i]])[c(1,2,4,7,20,29)] <- c("id", "house", "sex", "age", 
                                                "net_income", "EdLv")
}


saveRDS(background, file = "background_L.rds")


# Health variables (MHI included) *note: MHI is SDA (TFM)

h8 <- import("../nfc/health/ola 2 /BD.sav") #example

# list
healthL <- list(h7 = h7, h8 = h8, h9 = h9, h10 = h10,
                h11 = h11, h12 = h12, h13 = h13, h15 = h15, h16 = h16, 
                h17 = h17, h18 = h18, h19 = h19, h20 = h20, h21 = h21)


# name all the MHI and time variables the same in all databases ####
# (also save it)

mhi_name <- c(paste0("MHI", 1:5))
time_name <-c("day_started", "time_started", "day_finished",
              "time_finished", "duration_s")


sapply(healthL, names)

# MHI names for first 4 databases

names(healthL[[1]][6:10]) <- mhi_name

for (i in 2:5){
  names(healthL[[i]])[14:18] <- mhi_name
}

# times name
for (i in 2:5){
  names(healthL[[i]])[255:259] <- time_name
}

# MHI names for the rest of databases

for(i in 6:14) {
  names(healthL[[i]])[13:17] <- mhi_name
}

names(healthL[[5]])[254:258] <- time_name 
names(healthL[[6]])[256:260] <- time_name
names(healthL[[7]])[234:238] <- time_name
names(healthL[[8]])[234:238] <- time_name
names(healthL[[9]])[234:238] <- time_name
names(healthL[[10]])[234:238] <- time_name
names(healthL[[11]])[203:207] <- c("day_started", "day_finished", "time_started",
                                   "time_finished", "duration_s")
names(healthL[[12]])[206:210] <- c("day_started", "day_finished", "time_started",
                                   "time_finished", "duration_s")
names(healthL[[13]])[205:209] <- time_name


saveRDS(healthL, file = "healthL.rds")

sapply(healthL, names)

## Personality variables (NFC included) note: NFC is NDC (TFM) ####

p8 <- import("bbdd/nfc/personality/ola 1/cp08a_1p_EN.sav") # example

# List
personalityL <- list(p8 = p8, p9 = p9, p10 = p10, p11 = p11, p12 = p12,
                     p13 = p13, p14 = p14, p15 = p15, p17 = p17, p18 = p18,
                     p19 = p19, p20 = p20, p21 = p21, p22 = p22)


saveRDS(personalityL, file = "personalityL.rds")

# name all the NFC, SWLE and BIG-V and time variables the same in all databases ####
# (also save it)

nfc_names <- paste0("NFC", 1:18)
time_name

sapply(personalityL, names)

names(personalityL[[1]])[169:186] <- nfc_names
names(personalityL[[2]])[170:187] <- nfc_names
names(personalityL[[3]])[171:188] <- nfc_names
names(personalityL[[4]])[171:188] <- nfc_names
names(personalityL[[5]])[170:187] <- nfc_names
names(personalityL[[6]])[174:191] <- nfc_names
names(personalityL[[7]])[169:186] <- nfc_names
names(personalityL[[8]])[170:187] <- nfc_names
names(personalityL[[9]])[169:186] <- nfc_names
names(personalityL[[10]])[170:187] <- nfc_names

saveRDS(personalityL, file = "personalityL.rds")

# we clean up the environment

rm(h8, h9, h10, h11, h12, h13, h15, h16, h17, h18, h19, h20, h21,
   p8, p9, p10, p11, p12, p13, p14, p15, p17, p18, p19, p20, p21, p22, 
   may8, dic8, may9, jun9, nov9, dic9, may10, dic10, may11, 
   dic11, may12, dic12, may13, jun13, dic13, may14, dic14,
   may15, dic15, may16, dic16, may17, dic17, may18, jun18, 
   dic18, nov19, nov20, dic20, dic21) 


# 2. Prepare the time-invariant database ####

## Step 1: select all the participants id (even if they're repeated) ####

hp <- sapply(healthL, subset, select = nomem_encr)
pp <- sapply(personalityL, subset, select = nomem_encr)

## Step 2: pass all the elements to a single vector ####

hpv <- unlist(hp)
hpv <- unique(hpv)

ppv <- unlist(pp)
ppv <- unlist(ppv)

id <- c(hpv, ppv)
id <- unique(id)

length(id) # n = 15454 different people 

## Step 3: include all the ids in the "base" database ####

house <- rep(NA, 16810) 
sex <- rep(NA, 16810) 
age <- rep(NA, 16810) 
EdLv <- rep(NA, 16810)
net_income <- rep(NA,16810) 
wave <- rep(NA, 16810)


inv_df <- data.frame(id = id, house = house, sex = sex, 
                     age = age, net_income = net_income, EdLv = EdLv,
                     wave = wave)

## Step 4: fill up the NA spaces with the most antique variable in background ####

# First, we subset all the background datasets to only variables that (for now)
# interests us

background2 <- lapply(background, subset, select = c(id, house, sex, age,
                                                     net_income, EdLv))

#loop for fill up the NA spaces in the "base" database (inv_df)

for(i in 1:length(id)) {
  for(j in 1:length(background2)) {
    if (any(background2[[j]]$id == id[i]) == T) {
      inv_df[i, c(1:6)] <- background2[[j]][which(background2[[j]]$id == id[i]), ]
      inv_df[i, 7] <- names(background2)[j] #so the wave of the data for every person is available
      if (!all(is.na(inv_df[i, ]) )) break #if we have already find data from the person, we break the loop
    }
  }
}

# write_sav(inv_df, "inv_df.sav") # to quickly check the dataset (everything went well)

# check id's of people we don't have the data

table(inv_df$wave, useNA="always") 

# remove that data

inv_df <- inv_df[-which(is.na(inv_df$wave)),]

# 3.  Prepare the time variant data ####

## Time Variant MHI ####

### Step 1: subset all the databases to only id's, items from MHI and times variables ####

healthL_2 <- lapply(healthL, subset, select =c(nomem_encr, MHI1, MHI2, MHI3, MHI4, MHI5, 
                                               day_started, time_started, day_finished, time_finished,
                                               duration_s))

save(healthL_2, file = "MHI_df")

### Step 2: unlist and merge in THE database ####

mhi <- rbind(healthL_2$h8, healthL_2$h9)
mhi <- rbind(mhi, healthL_2$h10)
mhi <- rbind(mhi, healthL_2$h11)
mhi <- rbind(mhi, healthL_2$h12)
mhi <- rbind(mhi, healthL_2$h13)
mhi <- rbind(mhi, healthL_2$h14)
mhi <- rbind(mhi, healthL_2$h15)
mhi <- rbind(mhi, healthL_2$h16)
mhi <- rbind(mhi, healthL_2$h17)
mhi <- rbind(mhi, healthL_2$h18)
mhi <- rbind(mhi, healthL_2$h19)
mhi <- rbind(mhi, healthL_2$h20)

### Step 3: work with the "time" variable ####

reference <- dmy("5-5-2008")  # reference day: 5-5-2008
mhi$day <- dmy(mhi$day_started) - reference
class(mhi$day) #difftime
mhi$day <- as.numeric(mhi$day)
head(mhi)


summary(mhi)

### Step 4: order the database by id's and days ####

mhidt <- as.data.table(mhi)
setkey(mhidt, nomem_encr, day)
class(mhidt)
mhi <- as.data.frame(mhidt)
head(mhi)

### Step 5: remove NA's ####

summary(mhi)
mhi <- mhi[-which(is.na(mhi$MHI1)), ] # mismos sujetos con NA mhi1 son aquellos
# con NA en MHI2, MHI3, MHI4 y MHI5. 

### Step 6: calculate the average of MHI items (reversing and else) ####

# Revert items

mhi$MHI3 <- 7 - mhi$MHI3
mhi$MHI5 <- 7 - mhi$MHI5

# calculate average MHI

mhi$MHI <- apply(mhi[, c(2:6)], 1, mean)


### Step 7: rescale day variable to months

mhi$month <- mhi$day/30


### Step 8; rescale day variable to semester

mhi$sem <- mhi$day/180

### Step 9: rescale day variable to year

mhi$year <- mhi$day/365


## Time Variant NFC ####

### Step 1: subset all the databases to only id's, items from NFC and times variables ####

sapply(personalityL, names)

personalityL2 <- personalityL
personalityL2 <- personalityL2[-c(11:14)] # this is to subset the datasets
# that includes NFC variables

personalityL3 <- lapply(personalityL2, subset, select = c(nomem_encr, NFC1, NFC2, NFC3, NFC4, NFC5,
                                                          NFC6, NFC7, NFC8, NFC9, NFC10, NFC11, NFC12,
                                                          NFC13, NFC14, NFC15, NFC16, NFC17, NFC18,
                                                          day_started, time_started, day_finished, time_finished,
                                                          duration_s))



# save(personalityL_2, file = "personality_df")

### Step 2: unlist and merge in THE database ####

nfc <- rbind(personalityL3$p8, personalityL3$p9)
nfc <- rbind(nfc, personalityL3$p10)
nfc <- rbind(nfc, personalityL3$p11)
nfc <- rbind(nfc, personalityL3$p12)
nfc <- rbind(nfc, personalityL3$p13)
nfc <- rbind(nfc, personalityL3$p14)
nfc <- rbind(nfc, personalityL3$p15)
nfc <- rbind(nfc, personalityL3$p16)
nfc <- rbind(nfc, personalityL3$p17)
nfc <- rbind(nfc, personalityL3$p18)

### Step 3: work with the "time" variable ####

reference <- dmy("5-5-2008")  # reference day: 5-5-2008
nfc$day <- dmy(nfc$day_started) - reference
class(nfc$day) #difftime
nfc$day <- as.numeric(nfc$day)


### Step 4: order the database by id's and days ####

nfcdt <- as.data.table(nfc)
setkey(nfcdt, nomem_encr, day)
class(nfcdt)
nfc <- as.data.frame(nfcdt)
head(nfc)

### Step 5: remove NA's ####

summary(nfc) #15168 NFC 1 to 9 and 15210 NFC 10 to 18
names(nfc)
nfc <- nfc[-which(is.na(nfc$NFC1)), -c(20:24)] 
summary(nfc) # 42 en NFC10 hasta NFC18 

nfc <- nfc[-which(is.na(nfc$NFC14)), ]
summary(nfc)

### Step 6: calculate the average of NFC items (reversing and else) ####

# Revert items

nfc$NFC3 <- 8 - nfc$NFC3
nfc$NFC4 <- 8 - nfc$NFC4
nfc$NFC5 <- 8 - nfc$NFC5
nfc$NFC7 <- 8 - nfc$NFC7
nfc$NFC8 <- 8 - nfc$NFC8
nfc$NFC9 <- 8 - nfc$NFC9
nfc$NFC12 <- 8 - nfc$NFC12
nfc$NFC16 <- 8 - nfc$NFC16
nfc$NFC17 <- 8 - nfc$NFC17

# calculate average NFC

nfc$NFC <- apply(nfc[, c(2:19)], 1, mean)

### Step 7: rescale day variable to months

nfc$month <- nfc$day/30
head(nfc, 20)

### Step 8: rescale day variable to semesters

nfc$sem <- nfc$day/180
head(nfc, 20)

### Step 9: rescale day variable to year

nfc$year <- nfc$day/365
head(nfc, 20)

# 4. Merge background data with the average and the days ####

## MHI ####

names(inv_df)[1] <- "nomem_encr"
mhi_df <- merge(inv_df, mhi, by = "nomem_encr")

mhidt2 <- as.data.table(mhi_df) # order the database with id and day
setkey(mhidt2, nomem_encr, day)
class(mhidt2)
mhi_df <- as.data.frame(mhidt2)

head(mhi_df, 20)

names(mhi_df)
mhi_df2 <- mhi_df[, c(1:7, 18:22)] #subset variables that interest me (remove other time vars)

head(mhi_df2, 20)

save(mhi_df2, file = "mhi_df.rds")

## NFC ####

nfc_df <- merge(inv_df, nfc, by = "nomem_encr")
head(nfc_df)

nfcdt2  <- as.data.table(nfc_df)  # order the database with id and day
setkey(nfcdt2, nomem_encr, day)
class(nfcdt2)
nfc_df <- as.data.frame(nfcdt2)

head(nfc_df)
names(nfc_df)
nfc_df2 <- nfc_df[, c(1:7, 26:30)]

save(nfc_df, file = "nfc_df.rds")

head(nfc_df2, 20)

get_id <- setdiff(nfc$nomem_encr, mhi$nomem_encr) 
length(get_id) # There are 1645 people that
# answered at least one survey of personality.

# 5. Merge NFC and MHI databases ####

# to use rbind (the simplest way I can think of) we need that all the columns 
# have the same names. So we add a MHI and NFC column in each df. 

dim(mhi_df2) # check number of subjects
nfc_f <- rep(NA, 67688) # fake NFC column
mhi_df2$NFC <- nfc_f

dim(nfc_df) # check number of subjects
mhi_f <- rep(NA, 39238) # fake MHI column
nfc_df2$MHI <- mhi_f  

# check all the column's name are the same
names(nfc_df2)
names(mhi_df2)


#rbind 
df <- rbind(mhi_df2, nfc_df2)
head(df, 20)
tail(df, 20)

#order by id and day
df <- as.data.table(df)
setkey(df, nomem_encr, day)
class(df)
df <- as.data.frame(df)

#check
head(df, 20)

# remove days where NFC and also MHI are NA's

length(which(is.na(df$MHI) & is.na(df$NFC))) #42 
df <- df[-which(is.na(df$MHI) & is.na(df$NFC)), ]

# check

head(df, 20)
tail(df, 20)

# 7. List with a dataframe per person ####

df_person <- list()
id <- df$nomem_encr # all users id
id <- unique(id) 
length(id) #15241

for (i in 1:length(id)) {
  df_person[[i]] <- df[which(df$nomem_encr == id[i]), ]
} 

length(df_person)

#write_sav(df, "df_dr.sav") 

save(df, df_person, file = "df_tfm.rds")

