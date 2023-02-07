#-
#- Packages
#-
#-------------------------------------------------------------------------------
library(readxl)
library(dplyr)
###################################################
#### Reading in Data
###################################################

NFN.df <- read_excel("NFN_combined_final.xlsx", na='.')
View(NFN.df)
#-------------------------------------------------------------------------------
#-
#- Data Cleaning and Processing
#-
#-------------------------------------------------------------------------------






# clean first prenatal care visit

NFN.df$`Date of 1st Prenatal Care Visit` = ifelse(grepl(pattern='/99/', x=NFN.df$`Date of 1st Prenatal Care Visit`), 
                                                 NA, 
                                                 NFN.df$`Date of 1st Prenatal Care Visit`)

NFN.df$`Date of 1st Prenatal Care Visit` = as.Date(as.numeric(NFN.df$`Date of 1st Prenatal Care Visit`), 
                                                  origin=as.Date('1899-12-30'))

# clean last prenatal care visit

NFN.df$`Date of Last Prenatal Care Visit` = ifelse(grepl(pattern='/99/', x=NFN.df$`Date of Last Prenatal Care Visit`), 
                                                  NA, 
                                                  NFN.df$`Date of Last Prenatal Care Visit`)

NFN.df$`Date of Last Prenatal Care Visit` = as.Date(as.numeric(NFN.df$`Date of Last Prenatal Care Visit`), 
                                                   origin=as.Date('1899-12-30'))

# clean first clinic visit

NFN.df$`Clinic Date #1` = ifelse(grepl(pattern='/99/', x=NFN.df$`Clinic Date #1`), 
                                                   NA, 
                                                   NFN.df$`Clinic Date #1`)
NFN.df$`Clinic Date #1` = as.Date(as.numeric(NFN.df$`Clinic Date #1`), 
                                  origin = as.Date('1899-12-30'))

NFN.df$Case = ifelse(!is.na(NFN.df$`NFN Case ID`), "yes", "no") #Case Id was simple large numbers 
table(NFN.df$Case) # check

#- Compute low birth weight indicator

NFN.df$lbw.ind = cut(NFN.df$`Child Birth Weight (grams)`,              # Numeric input vector
    4,                  # Number or vector of breaks
    labels = c("VLBW", "LBW", "NBW", "HBW"),           # Labels for each group
    include.lowest = TRUE,  # Whether to include the lowest 'break' or not
    ordered_result = FALSE,  # Whether to order the factor result or not
)

#- Birth weight classification
NFN.df$lbw.class = case_when( NFN.df$lbw.ind == "VLBW"~ 1,
                              NFN.df$lbw.ind == "LBW"~ 1,
                              NFN.df$lbw.ind == "NBW"~0,
                              NFN.df$lbw.ind == "HBW"~0
)

#- Date Calculations

NFN.df$lastmen_pren = as.numeric(as.Date(NFN.df$`Date of Last Normal Menses`) - 
                                   as.Date(NFN.df$`Date of Last Prenatal Care Visit`)) #Days between last menses and last prenatal visit 

NFN.df$lastmen_first.C.visit = as.Date(NFN.df$`Date of Last Normal Menses`) - as.Date(NFN.df$`Clinic Date #1`) #Last menses & first clinic visit (Clinic date 1)

NFN.df$lastmen_referral.date = as.Date(NFN.df$`Date of Last Normal Menses`) - as.Date(NFN.df$referred) #Last names & referral date





