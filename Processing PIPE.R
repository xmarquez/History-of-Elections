# In order to reproduce the graphs, you will need access to the PIPE dataset by
# Adam Przeworski et. al. This dataset is available here:
# https://sites.google.com/a/nyu.edu/adam-przeworski/home/data
#
# The dataset requires a bit of cleanup before the graphs below can be reproduced
# exactly. The country names (column countryn) are a bit jumbled (and some are 
# missing), and franchise information for Russia is missing. I used Google Refine 
# to cleanup country names and fix a number of minor issues; the JSON extract of
# most of these changes is pasted to the file "cleanup.JSON." I also used the 
# cshapes package by Nils Weidmann to add most capitals (others I added manually). The 
# additional data generated during the process (changed country names, franchise 
# info for Russia, capital names and latitudes and longitudes, useful codes for 
# merging with other datasets) is all available in the file "Additional Data and Codes for PIPE.csv", 
# which you should save in your working directory and then run this file to 
# process the PIPE dataset so that the graphs in the post can be replicated.

library(foreign)
library(RColorBrewer)
library(plyr)

PIPE <- read.dta("../Data/PIPE_master_092011.dta") # change directory as appropriate 
PIPE.additional.data <- read.csv("Additional Data and Codes for PIPE.csv")

# To clean up countrynames and add additional franchise info, we first rename the 
# columns of the dataset to be replaced.
# 
# This way we can just replace them by the columns of cleaned up names and 
# modified franchise information

columns.to.change <- c("countryn","f","ext_type","oth_exclusions")
names(PIPE)[ names(PIPE) %in% columns.to.change] <- paste(columns.to.change,"_original",sep="")

# And now we merge the original dataset with the additional data - latitudes and 
# longitudes, capital names, franchise information for Russia, and various useful codes

PIPE <- merge(PIPE,PIPE.additional.data,all=TRUE)
PIPE <- PIPE[ order(PIPE$country, PIPE$year), ]
rm(PIPE.additional.data)

# We add a column for turnout (warning - some turnouts exceed 100%)
PIPE <- transform(PIPE, turnout_leg = legpart_pr / eligible_pr)
PIPE <- transform(PIPE, turnout_pres = prespart_pr / eligible_pr)

# Add an extra franchise value, for those countries with at least
# partly elected legislatures, but no franchise information in the dataset, 
# because (e.g.) franchise is determined at the subnational level
# SN/O stands for "subnational/other"

PIPE <- transform(PIPE, 
                  f_full = ifelse(is.na(f),
                                  ifelse(leg_composition >= 5,
                                         "SN/O",NA), ifelse(f==70, 7 , f)))


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


# This variable indicates changes in franchise that are not captured by ext_type
# since ext_type is sometimes recorded in years where no changes happened
PIPE <- ddply(PIPE, .(country), transform, ext_type_2 = c(NA,diff(f_full_2)))

# This is mostly a hack to suss out the inconsistencies between ext_type and
# ext_type_2

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


# Add descriptions for the franchise extensions

PIPE$ext_type_descriptions_1 <- PIPE$ext_type 
PIPE$ext_type_descriptions_2 <- PIPE$ext_type 

PIPE$ext_type_descriptions_1 <- ifelse(PIPE$ext_type_descriptions_1 == 0,
                                     NA,
                                     PIPE$ext_type_descriptions_1)

PIPE$ext_type_descriptions_1 <- as.factor(PIPE$ext_type_descriptions_1)
PIPE$ext_type_descriptions_2 <- as.factor(PIPE$ext_type_descriptions_2)
levels(PIPE$ext_type_descriptions_1) <- c("Contraction",
                                          "Exp. by class",
                                          "Exp. by gender",
                                          "Exp. by class and gender")
levels(PIPE$ext_type_descriptions_2) <- c("Contraction",
                                          "No change",
                                          "Exp. by class",
                                          "Exp. by gender",
                                          "Exp. by class and gender")

# Add descriptions for the "other exclusions"

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

# Add descriptions for the "opposition" variable
PIPE$opposition_2 <- as.factor(PIPE$opposition)
levels(PIPE$opposition_2) <- c("No","Yes")

# Add descriptions for the "Strong Alternation" variable
PIPE$salterel_2 <- as.factor(PIPE$salterel)
levels(PIPE$salterel_2) <- c("Inc. defeat, challenger takeover after interregnum",
                             "Non-partisan",
                             "Incumbent remains in office",
                             "Party and leader change")

# Add descriptions for the "Compulsory voting" variable

PIPE$compulsory_2 <- as.factor(PIPE$compulsory)
levels(PIPE$compulsory_2) <- c("Unknown","No","Yes")

# This is necessary to do the graphs on "Strong Alternation"
PIPE.salterel <- PIPE[ !is.na(PIPE$salterel) , ]

PIPE.salterel <- ddply(PIPE.salterel,
                       .(country),
                       transform,
                       salterel_count = length(salterel))

# Calculates mean, median, max, and min turnout per country
PIPE <- ddply(PIPE, .(country), transform, mean_turnout_leg = mean(turnout_leg,na.rm=TRUE))
PIPE <- ddply(PIPE, .(country), transform, median_turnout_leg = median(turnout_leg,na.rm=TRUE))

PIPE <- ddply(PIPE, .(country), transform, max_turnout_leg = max(turnout_leg,na.rm=TRUE))
PIPE <- ddply(PIPE, .(country), transform, min_turnout_leg = min(turnout_leg,na.rm=TRUE))

# Add descriptions for the "Constitution in force" variable
PIPE$const_inforce_2 <- ifelse(PIPE$const_inforce != 0, "Yes","No")
PIPE$const_inforce_2 <- as.factor(PIPE$const_inforce_2)

# Calculate total elections per country/year/region
PIPE <- ddply(PIPE, .(year), transform, total_elections_year = sum(legelec, na.rm=TRUE) + sum(preselec, na.rm=TRUE))

PIPE <- ddply(PIPE, .(country), transform, total_elections_country = sum(legelec, na.rm=TRUE) + sum(preselec, na.rm=TRUE))              

PIPE <- ddply(PIPE, .(year), transform, total_leg_elections_year = sum(legelec, na.rm=TRUE))

PIPE <- ddply(PIPE, .(year, opposition_2), transform, total_elections_year_opp = sum(legelec, na.rm=TRUE) + sum(preselec, na.rm=TRUE))

PIPE <- ddply(PIPE, .(year, un_continent_name), transform, total_elections_year_continent = sum(legelec, na.rm=TRUE) + sum(preselec, na.rm=TRUE))

PIPE <- ddply(PIPE, .(year), transform, elections_as_prop_of_countries = total_elections_year / length(country))

PIPE$leg_composition_2 <- cut(PIPE$leg_composition, c(-1,0.99,4.99,8.99,12), labels = c("No legislature","Fully appointed","Partly appointed","Fully elected"))

# Add a consistent color palette for the simplified and complex franchise value
addColorColumn <- function(column) {
  column <- as.factor(column)
  num.colors <- length(levels(column))
  if(num.colors > 11) {
    pal <- rainbow(num.colors)    
  }
  else {
    pal <- brewer.pal(num.colors,"RdYlGn")
  }
  
  levels(column) <- pal
  return(column)
}

PIPE$f_simple_color <- addColorColumn(PIPE$f_simple)
PIPE$f_color <- addColorColumn(PIPE$f)
PIPE$f_extension_color <- addColorColumn(PIPE$ext_type_descriptions_1)
PIPE$oth_exclusions_color <- addColorColumn(PIPE$oth_exclusions_descriptions)
PIPE$opposition_color <- addColorColumn(PIPE$opposition)
PIPE$salterel_color <- addColorColumn(PIPE$salterel)

rm(columns.to.change, extra.contractions, extra.expansions, extra.nochanges, first.entry, full.contractions, full.expansions, mixed, nochanges.from.sno, other.NAs, sure.NAs, sure.nochanges)
print("Finished Processing PIPE")
