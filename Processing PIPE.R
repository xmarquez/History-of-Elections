# require(foreign)
# PIPE <- read.dta("../Data/PIPE_master_092011.dta")
# PIPE <- transform(PIPE, polstatus_names = 
#
# calculating turnout (warning - some turnouts exceed 100%)
# PIPE <- transform(PIPE, turnout_leg = legpart_pr / eligible_pr)
# PIPE <- transform(PIPE, turnout_pres = prespart_pr / eligible_pr)
#
# Processing DD
# dd <- read.csv("../Data/ddrevisited-data-v1.csv")
# dd <- transform(dd, regime_type = factor(democracy,
#                                          labels=c("Dictatorship","Democracy")))
#
# PIPE <- ddply(PIPE,
#                 .(country), 
#                 transform, 
#                 cowcodes2 = max(cowcodes,na.rm=TRUE))
#
# 
# To find capitals
# require(cshapes)
# 
# cshp.data <- cshp()
# cshp.yearly <- cshapes2yearly(cshp.data, 
#                               vars=c("CNTRY_NAME","CAPNAME","CAPLONG","CAPLAT"),
#                               useGW=FALSE)
# 
# names(cshp.yearly)[1] <- "cowcodes2"
# 
# PIPE <- merge(cshp.yearly, PIPE, all=TRUE)

source("auxiliary functions.R")

library(plyr)
PIPE <- read.csv("PIPE.csv")
# Uncomment this bit if you haven't merged it already
# codes <- read.csv(paste(getwd(), "/../Data/codes.csv", sep=""))
# names(codes)[3] <- "country"
# PIPE <- merge(PIPE, codes,all=TRUE)
# rm(codes)

# Take off a few rows which only have capitals, not data
PIPE <- subset(PIPE, !is.na(country))

# Add an extra franchise value, for those countries with at least
# partly elected legislatures, but no franchise information in the dataset, 
# because (e.g.) franchise is determined at the subnational level
# SN/O stands for "subnational/other"

PIPE <- transform(PIPE, 
                  f_full = ifelse(is.na(f),
                                  ifelse(leg_composition >= 5,
                                         "SN/O",NA), f))

PIPE$f_full <- ifelse(PIPE$f_full==70, 7 , PIPE$f_full)

PIPE$f_full_descriptions <- as.factor(PIPE$f_full)

levels(PIPE$f_full_descriptions) <- c("0: No suffrage",
                                      "1: Estate representation, no female",
                                      "12: Estate representation, equal female",
                                      "2: Property only, no female",
                                      "21: Property only, narrower female",
                                      "22: Property only, equal female",
                                      "3: Property or income or taxes etc.. AND literacy, no female",
                                      "31: Property or income or taxes etc. AND literacy, narrower female",
                                      "32: Property or income or taxes etc. AND literacy, equal female",
                                      "4: Property or income or taxes or profession or education, no female",
                                      "41: Property or income or taxes or profession or education, some female",
                                      "42: Property or income or taxes or profession or education, equal female",
                                      "5: Literacy only or property etc., no female",
                                      "51: Literacy only or property etc., some female",
                                      "52: Literacy only or property etc., equal female",
                                      "6: Economic independence, no female",
                                      "61: Economic independence, some female",
                                      "62: Economic independence, equal female",
                                      "7: All adult males, no female",
                                      "71: All adult males, some female",
                                      "72: Universal",
                                      "SN/O: Subnational determination/Other")

PIPE <- transform(PIPE, f_full_2 = ifelse(is.na(f),
                                          ifelse(leg_composition >= 5,
                                                 0.05, NA), f))

PIPE$f_full_2 <-ifelse(PIPE$f_full_2 < 10, PIPE$f_full_2*10, PIPE$f_full_2)



PIPE <- PIPE[ order(PIPE$country, PIPE$year), ]
PIPE <- ddply(PIPE, .(country), transform, ext_type_2 = c(NA,diff(f_full_2)))

PIPE <- ddply(PIPE, .(country), transform, f_prev = c(NA,f_full_2[1: length(country) - 1]))

PIPE <- transform(PIPE, ext_type_3 = as.character(paste(ext_type, 
                                                     ext_type_2, 
                                                     "Current f:",
                                                     f_full_2,
                                                     "Previous f:",
                                                     f_prev)))

PIPE$ext_type_3 <- as.character(PIPE$ext_type_3)

full.contractions <- ((PIPE$ext_type_2 < 0) & (PIPE$ext_type_2 != -8) & PIPE$f_full_2 != 0.5)
full.contractions <- ifelse(is.na(full.contractions), FALSE, full.contractions)
full.expansions <- (PIPE$ext_type_2 > 0 & PIPE$f_prev != 0.5) | (PIPE$ext_type > 0)
full.expansions <- ifelse(is.na(full.expansions), FALSE, full.expansions)
sure.nochanges <- (PIPE$ext_type_2 == 0)
sure.nochanges <- ifelse(is.na(sure.nochanges), FALSE, sure.nochanges)
sure.NAs <- (is.na(PIPE$ext_type) & is.na(PIPE$ext_type_2) & is.na(PIPE$f_full_2))
other.NAs <- is.na(PIPE$ext_type) & (PIPE$f_full_2 == 0.5 | PIPE$f_prev == 0.5) 
extra.nochanges <- PIPE$ext_type == 0 & is.na(PIPE$ext_type_2)
extra.nochanges <- ifelse(is.na(extra.nochanges), FALSE, extra.nochanges)
nochanges.from.sno <- PIPE$ext_type == 0 & PIPE$f_prev == 0.5 
nochanges.from.sno <- ifelse(is.na(nochanges.from.sno), FALSE, nochanges.from.sno)

extra.expansions <- PIPE$ext_type > 0 & is.na(PIPE$ext_type_2)
extra.expansions <- ifelse(is.na(extra.expansions), FALSE, extra.expansions)
extra.contractions <- PIPE$ext_type < 0 & is.na(PIPE$ext_type_2)
extra.contractions <- ifelse(is.na(extra.contractions), FALSE, extra.contractions)
first.entry <- is.na(PIPE$f_prev) & !is.na(PIPE$f_full_2) & is.na(PIPE$ext_type)


mixed <- PIPE$ext_type_2 == -8
mixed <- ifelse(is.na(mixed), FALSE, mixed)

PIPE$ext_type_3[first.entry] <- "Entry, no other info"
PIPE$ext_type_3[full.contractions] <- "Contraction"
PIPE$ext_type_3[extra.contractions] <- "Contraction"
PIPE$ext_type_3[full.expansions] <- "Expansion"
PIPE$ext_type_3[extra.expansions] <- "Expansion"
PIPE$ext_type_3[sure.NAs] <- NA
PIPE$ext_type_3[sure.nochanges] <- "No change"
PIPE$ext_type_3[extra.nochanges] <- "No change"
PIPE$ext_type_3[nochanges.from.sno] <- "No change"
PIPE$ext_type_3[mixed] <- "Mixed"

PIPE$ext_type_3 <- as.factor(PIPE$ext_type_3)
summary(PIPE$ext_type_3)

levels(PIPE$ext_type_3)[5:7] <- NA
PIPE$changes <- as.factor(PIPE$ext_type_3)
levels(PIPE$changes) <- c(TRUE, TRUE, TRUE, TRUE, FALSE)

PIPE$symbols <- PIPE$ext_type_3
levels(PIPE$symbols) <- c(25, 21, 24, 23, 21)

# Add a simplified franchise value (better for graphing)
PIPE$f_simple <- as.factor(PIPE$f_full)

levels(PIPE$f_simple) <- c("No suffrage",
                           "Class restricted, no female",
                           "Class restricted, some female",
                           "Class restricted, no female",
                           "Class restricted, some female",
                           "Class restricted, some female",
                           "Class restricted, no female",
                           "Class restricted, some female",
                           "Class restricted, some female",
                           "Class restricted, no female",
                           "Class restricted, some female",
                           "Class restricted, some female",
                           "Class restricted, no female",
                           "Class restricted, some female",
                           "Class restricted, some female",
                           "Class restricted, no female",
                           "Class restricted, some female",
                           "Class restricted, some female",
                           "All adult males",
                           "All adult males, some female",
                           "Universal (Non-class restrictions possible)",
                           "Subnational/Other")



PIPE$ext_type_descriptions_1 <- PIPE$ext_type 
PIPE$ext_type_descriptions_2 <- PIPE$ext_type 

PIPE$ext_type_descriptions_1 <- ifelse(PIPE$ext_type_descriptions == 0,
                                     NA,
                                     PIPE$ext_type_descriptions)

PIPE$ext_type_descriptions_1 <- as.factor(PIPE$ext_type_descriptions_1)
PIPE$ext_type_descriptions_2 <- as.factor(PIPE$ext_type_descriptions_2)
PIPE$oth_exclusions_2 <- ifelse(PIPE$oth_exclusions == 0,NA,PIPE$oth_exclusions)
PIPE$oth_exclusions_labels <- paste(PIPE$countryn,
                                    ":",
                                    PIPE$oth_exclusions)
PIPE$oth_exclusions_descriptions <- as.factor(PIPE$oth_exclusions_2)
levels(PIPE$oth_exclusions_descriptions) <- c("-1:? ",
                                              "1: Ethnicity",
                                              "2: Territory",
                                              "3: Religion",
                                              "4: Sympathizers of some political parties excluded",
                                              "5: Slaves excluded",
                                              "6: Military excluded",
                                              "7: Priests or nuns excluded",
                                              "35: 3 and 5 together",
                                              "46: 4 and 6 together",
                                              "48: 4 and the propertied excluded",
                                              "57: 5 and 7 together",
                                              "67: 6 and 7 together",
                                              "78: 7 and the propertied excluded",
                                              "236: 2 3 and 6 together")
PIPE$opposition_2 <- as.factor(PIPE$opposition)
levels(PIPE$opposition_2) <- c("No","Yes")

PIPE$salterel_2 <- as.factor(PIPE$salterel)
levels(PIPE$salterel_2) <- c("Inc. defeat, challenger takeover after interregnum",
                             "Non-partisan",
                             "Incumbent remains in office",
                             "Party and leader change")

levels(PIPE$ext_type_descriptions_1) <- c("Contraction",
                                          "Exp. by class",
                                          "Exp. by gender",
                                          "Exp. by class and gender")
levels(PIPE$ext_type_descriptions_2) <- c("Contraction",
                                          "No change",
                                          "Exp. by class",
                                          "Exp. by gender",
                                          "Exp. by class and gender")

PIPE$compulsory_2 <- as.factor(PIPE$compulsory)
levels(PIPE$compulsory_2) <- c("Unknown","No","Yes")

PIPE.salterel <- PIPE[ !is.na(PIPE$salterel) , ]

PIPE.salterel <- ddply(PIPE.salterel,
                       .(country),
                       transform,
                       salterel_count = length(salterel))

PIPE <- ddply(PIPE, .(country), transform, mean_turnout_leg = mean(turnout_leg,na.rm=TRUE))
PIPE <- ddply(PIPE, .(country), transform, median_turnout_leg = median(turnout_leg,na.rm=TRUE))

PIPE <- ddply(PIPE, .(country), transform, max_turnout_leg = max(turnout_leg,na.rm=TRUE))
PIPE <- ddply(PIPE, .(country), transform, min_turnout_leg = min(turnout_leg,na.rm=TRUE))

PIPE$const_inforce_2 <- ifelse(PIPE$const_inforce != 0, "Yes","No")
PIPE$const_inforce_2 <- as.factor(PIPE$const_inforce_2)

PIPE <- ddply(PIPE, .(year), transform, total_elections_year = sum(legelec, na.rm=TRUE) + sum(preselec, na.rm=TRUE))

PIPE <- ddply(PIPE, .(country), transform, total_elections_country = sum(legelec, na.rm=TRUE) + sum(preselec, na.rm=TRUE))              

PIPE <- ddply(PIPE, .(year), transform, total_leg_elections_year = sum(legelec, na.rm=TRUE))

PIPE <- ddply(PIPE, .(year, opposition_2), transform, total_elections_year_opp = sum(legelec, na.rm=TRUE) + sum(preselec, na.rm=TRUE))

PIPE <- ddply(PIPE, .(year, un_continent_name), transform, total_elections_year_continent = sum(legelec, na.rm=TRUE) + sum(preselec, na.rm=TRUE))

PIPE <- ddply(PIPE, .(year), transform, elections_as_prop_of_countries = total_elections_year / length(country))

PIPE$leg_composition_2 <- cut(PIPE$leg_composition, c(-1,0.99,4.99,8.99,12), labels = c("No legislature","Fully appointed","Partly appointed","Fully elected"))





# Add a consistent color palette for the simplified and complex franchise value

PIPE$f_simple_color <- addColorColumn(PIPE$f_simple)
PIPE$f_color <- addColorColumn(PIPE$f)
PIPE$f_extension_color <- addColorColumn(PIPE$ext_type_descriptions_1)
PIPE$oth_exclusions_color <- addColorColumn(PIPE$oth_exclusions_descriptions)
PIPE$opposition_color <- addColorColumn(PIPE$opposition)
PIPE$salterel_color <- addColorColumn(PIPE$salterel)

print("Finished Processing PIPE")
