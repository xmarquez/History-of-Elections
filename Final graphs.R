# Source file for all the graphs in this post:
# http://abandonedfootnotes.blogspot.com/
# 
# In order to reproduce the graphs, you will need access to the PIPE dataset by
# Adam Przeworski et. al. This dataset is available here:
# https://sites.google.com/a/nyu.edu/adam-przeworski/home/data
#
# The dataset requires a bit of cleanup before the graphs below can be reproduced
# exactly. I have used Google Refine to cleanup country names and fix a number of 
# minor issues; the JSON extract of these changes is pasted to the file 
# "cleanup.JSON." Save the dataset as a csv file, and then process it in Google
# Refine by applying the JSON extract. See also the file "Processing PIPE.R",  
# which you will need also, for further details.
#
# Finally, you will need the files "codes.csv", and "auxiliary functions.R". Place 
# them all in the same directory as this file and the PIPE dataset.

source("auxiliary functions.R")
source("Processing PIPE.R", echo=TRUE)
library(ggplot2)
library(plyr)



# Slideshow 1: Suffrage throughout history

suffrage.map <- function(year) {
  
  data.year <- PIPE[PIPE$year == year, ]
  
  map("world",col="lightgrey", mar = c(0,1.1,0,1.1)) #plots outline of world
  
  #colors
  bubble.colors <- data.year$f_simple_color
  border.colors <- data.year$oth_exclusions_color
  
  # text
  bubble.labels <- ifelse(data.year$changes == TRUE,
                          paste(as.character(data.year$countryn),
                              ": ",
                              as.character(data.year$f_full), sep=""),
                          as.character(data.year$f_full))
  #Symbols
  
  bubble.symbols <- as.numeric(as.character(data.year$symbols)) 
  #legend labels and colors
  leg.colors <- as.character(levels(PIPE$f_simple_color))
  leg.labels <- levels(as.factor(PIPE$f_simple))
  print(leg.labels)
  
  leg.2.symbols <- c(21,24,25,23)
  leg.2.labels <- c("No change/NA","Expansion","Contraction","Mixed")
  
  leg.3.colors <- c("white",as.character(levels(PIPE$oth_exclusions_color)))
  leg.3.labels <- c("None/NA",levels(PIPE$oth_exclusions_descriptions))
  
  points(data.year$CAPLONG,
         data.year$CAPLAT,
         bg = as.character(bubble.colors),
         col = as.character(border.colors),
         pch = bubble.symbols,
         cex = 1.6)
  
  text(data.year$CAPLONG,
       data.year$CAPLAT,
       labels = bubble.labels,
       cex = 0.6)
  
  title(main=paste("The extent of the franchise in the world, ",year))
  
  legend(x="bottom",
         legend = leg.labels,
         fill = leg.colors,
         ncol = 3,
         cex = 0.6,
         title = "Franchise types (color)")
  
  legend(x="topleft",
         legend = leg.2.labels,
         pch = leg.2.symbols,
         cex = 0.6,
         title = "Franchise changes (shape)")

  legend(x="bottomleft",
         legend = leg.3.labels,
         lty = 1,
         col = leg.3.colors,
         cex = 0.6,
         title = "Other exclusions (border)")
  
}

levels(PIPE$f_simple_color)[7] <- "lightgrey"
for (year in 1788:2008) {
  png(file = paste(getwd(), "/Maps/", year,".png", sep = ""), 
      width = 2048, height = 1536)
  suffrage.map(year)
  dev.off()
}


# Figure 1: Franchise types worldwide, 1788-2008

all.countries <- qplot(data = PIPE,
                       x = year, 
                       fill = f_simple, 
                       binwidth= 1,
                       ylab="Number of countries")
all.countries + guides(fill = guide_legend(title="Franchise type"))


# Figure 2: class and gender expansions and contractions of the franchise, 
# 1788-2008, at five year intervals

suffrage.changes <- qplot(data = PIPE[ PIPE$ext_type != 0, ],
                          x = year,
                          fill = ext_type_descriptions_1,
                          binwidth = 5,
                          ylab = "Number of changes, 5 year periods")
suffrage.changes + guides(fill = guide_legend(title="Franchise change types"))

# Figure 3: Class and gender expansions and contractions of the franchise, by
# region, 1788-2008, at five year intervals

suffrage.changes.by.region <- qplot(data = PIPE[ !is.na(PIPE$ext_type_descriptions_1), ],
                                    x = year,
                                    fill = reorder(un_region_name,aclp_region),
                                    binwidth = 5,
                                    ylab = "Number of changes, 5 year periods")
suffrage.changes.by.region + guides(fill = guide_legend(title="Region")) + facet_grid(ext_type_descriptions_1 ~ un_continent_name)

# Figure 4: Other exclusions 1788-2008

other.exclusions <- qplot(data = PIPE[ !is.na(PIPE$oth_exclusions_2), ],
                                x = year,
                                fill = oth_exclusions_descriptions,
                                binwidth = 1)
other.exclusions + guides(fill = guide_legend(title="Other exclusions"))

# Figure 5: Other exclusions 1788-2008, by region

other.exclusions.by.region <- qplot(data = PIPE[ !is.na(PIPE$oth_exclusions_2), ],
                                    x = year,
                                    fill = un_region_name,
                                    binwidth = 5,
                                    ylab = "Number of country-years with other exclusions, 5 year periods")
other.exclusions.by.region + guides(fill = guide_legend(title="Region")) + facet_grid(oth_exclusions_2 ~ un_continent_name)

# Figure 6: Composition of legislatures, 1788-2008
leg.composition.graph <- qplot(data = PIPE,
                               x = year,
                               fill = leg_composition_2,
                               binwidth = 1) + guides(fill = guide_legend("Composition of legislature"))
leg.composition.graph

# Figure 7: Geographical distribution of elected egislatures in the year 2000
png(file = paste(getwd(), "/Figure 3 Map of legislature composition 2000", ".png", sep = ""),
    width = 1000, height = 800)
map("world", col="lightgrey")
freq.palette <- brewer.pal(4,"RdYlGn")
palette(freq.palette)
leg.txt <- levels(PIPE$leg_composition_2)
points(x = PIPE$CAPLONG[ PIPE$year == 2000 ], y = PIPE$CAPLAT[ PIPE$year == 2000 ], pch=21, col = "black", bg = PIPE$leg_composition_2[ PIPE$year == 2000 ], cex = 1.5)
legend(x = "bottom", legend = leg.txt, fill = freq.palette, horiz = TRUE, title = "Composition of legislatures, 2000. White circles indicate no information in dataset.")
dev.off()

# Figure 8: Elections per year 1788-2008
elections.per.year <- qplot(data = PIPE,
                            x = year,
                            y = total_elections_year,
                            ylab = "Legislative and presidential elections per year") + geom_smooth() 
elections.per.year 


# Figure 9: elections per year as a proportion of states since 1917
elections.as.prop.of.states <- qplot(data = PIPE[ PIPE$year >= 1917, ]
                                     ,x = year,
                                     y = elections_as_prop_of_countries, 
                                     ylab = "Elections per year as a proportion of number of states",
                                     size = total_elections_year) + geom_smooth() + guides(size = guide_legend("Elections per year"))

elections.as.prop.of.states


# Figure 10: Elections per year 1788-2008 with opposition
elections.per.year.opp <- qplot(data = PIPE,
                                x = year,
                                y = total_elections_year_opp,
                                colour = opposition_2, 
                                ylab = "Number of elections per year") + geom_smooth() + guides(colour = guide_legend(title="Opposition?"))
elections.per.year.opp

# Figure 11: Opposition in politics, according to whether the constitution is in force

opposition <- qplot(data = PIPE[ !is.na(PIPE$const_inforce_2), ],
                    x=year,
                    fill = opposition_2,
                    ylab = "Number of states",
                    binwidth = 1) + guides(fill = guide_legend(title="Opposition?")) + facet_grid(. ~ const_inforce_2) 
opposition + opts(title = "Constitution in force?", plot.title = theme_text(size=12))

# Figure 12: Opposition in politics, 1788-2008, by region
opposition.by.region <- qplot(data = PIPE,
                              x = year,
                              fill = un_region_name,
                              binwidth = 1,
                              ylab = "Number of states")
opposition.by.region + guides(fill = guide_legend(title="Region")) + facet_grid(opposition_2 ~ un_continent_name)

# Figure 13: Opposition in politics, by franchise type
opposition.by.franchise <- qplot(data=PIPE,
                                 x=f_simple,
                                 fill=opposition_2,
                                 binwidth=1,
                                 xlab = "Franchise type", 
                                 position="fill",
                                 ylab = "Proportion of cases")
opposition.by.franchise + guides(fill = guide_legend(title="Opposition?")) + coord_flip()


# Figure 14: Proportion of elections won by incumbent candidate, per year
incumbent_win_year <- table(PIPE$year,PIPE$salterel_2)
incumbent_win_year <- round(prop.table(incumbent_win_year,1)*100,2)
incumbent_win_year <- as.data.frame(incumbent_win_year)
names(incumbent_win_year) <- c("year","incumbent","freq")
incumbent_win_year$year <- 1788:2008

incumbent.win.year <- qplot(data = incumbent_win_year,
                            x = year,
                            y = freq,
                            colour = incumbent,
                            geom = "smooth",
                            ylab = "Percent of elections")
incumbent.win.year + geom_point() + guides(colour = guide_legend(title="Electoral outcome: ")) + opts(legend.position = "bottom")

# Try this to see how it varies by franchise
# salterel.by.franchise <- qplot(data=PIPE[ !is.na(PIPE$salterel_2), ],
#                                x=f_simple,
#                                fill=salterel_2,
#                                binwidth=1,
#                                xlab = "Franchise type", 
#                                position="fill",
#                                ylab = "Proportion of cases")
# salterel.by.franchise + guides(fill = guide_legend(title="Alternation?")) + coord_flip() + opts(legend.position = "bottom")


# Figure 15: Strong alternation in politics, by country
PIPE.salterel <- PIPE[ !is.na(PIPE$salterel) , ]

PIPE.salterel <- ddply(PIPE.salterel,
                       .(country),
                       transform,
                       salterel_count = length(salterel))

salterel.by.country <- qplot(data = PIPE.salterel,
                             x = reorder(countryn, salterel_count),
                             fill = salterel_2,
                             binwidth = 1,
                             xlab = "Country",
                             ylab = "Number of elections")
salterel.by.country <- salterel.by.country + guides(fill = guide_legend(title="Electoral outcome: ")) + coord_flip()+ opts(legend.position = "bottom") + geom_vline(xintercept = c(192,181,173), colour = "black")
salterel.by.country

# Try also, to see the distribution by region:
# salterel_by_country + facet_grid(. ~ un_continent_name)


# Figure 16: Proportion of elections won by incumbent party by country

incumbent_win <- table(PIPE$country,PIPE$salterel)
incumbent_win <- round(prop.table(incumbent_win,1)*100,2)
incumbent_win <- as.data.frame(incumbent_win)
names(incumbent_win) <- c("country","incumbent","freq")
incumbent_win <- incumbent_win[ incumbent_win$incumbent == 0, ]
incumbent_win <- incumbent_win[ !is.nan(incumbent_win$freq), ]

coords <- unique(PIPE.salterel[ , c("country",
                           "countryn",
                           "CAPLONG",
                           "CAPLAT",
                           "un_continent_name",
                           "un_region_name", "salterel_count")])
coords <- coords[ complete.cases(coords), ]
PIPE.incumbent <- merge(incumbent_win, coords)

prop.incumbent.wins.by.country <- qplot(data=PIPE.incumbent,
                                        x=reorder(countryn, freq),
                                        y=freq,
                                        color=un_continent_name,
                                        binwidth=1,
                                        size = salterel_count,
                                        xlab = "Country",
                                        ylab = "Proportion of elections won by incumbent party")
prop.incumbent.wins.by.country + guides(color = guide_legend(title="Region"), size = guide_legend(title = "N. of elections")) + coord_flip() + geom_vline(xintercept = c(21,52,90), colour = "black") + geom_hline(yintercept = 50, colour = "red") 

# Figure 17: Map showing proportion of elections won by incumbent party by country
# Size of circle is proportional to the number of elections the country has experienced
png(file = paste(getwd(), "/Figure 17 Map of incumbent win proportions", ".png", sep = ""),
    width = 1000, height = 800)
map("world", col="lightgrey")
freq.palette <- brewer.pal(11,"RdYlGn")
palette(freq.palette)
PIPE.incumbent$colorBuckets <- as.numeric(cut(PIPE.incumbent$freq, breaks = c(50, 55, 60, 65, 70, 75, 80, 85, 90, 100)))
leg.txt <- c(" < 50%","50-55%","55-60%","60-65%","65-70%","70-75%","75-80%","80-85%","85-90%","90-95%","95-100%")
points(x = PIPE.incumbent$CAPLONG, y = PIPE.incumbent$CAPLAT, pch=21, col = "black", bg = PIPE.incumbent$colorBuckets, cex = sqrt(PIPE.incumbent$salterel_count * 10 / 71))
legend(x = "bottom", legend = leg.txt, fill = freq.palette, horiz = TRUE, title = "Percentage of elections where incumbent party remained in power. Size of circles is proportional to the number of elections")
dev.off()

# Figure 18: Participation in elections (ratio of voters to total population) by franchise, scatterplot
participation <- qplot(data=PIPE[ !is.na(PIPE$legpart_pr) , ], 
                       x=year, 
                       y=legpart_pr,
                       geom="point",
                       ylab = "Ratio of voters to total population",
                       colour = f_simple) 

participation + guides(color = guide_legend(title="Franchise types"))

# Figure 19: Participation in elections (ratio of voters to total population) by franchise, boxplot
participation.2 <- qplot(data=PIPE[ !is.na(PIPE$legpart_pr) , ], 
                       x=f_simple, 
                       y=legpart_pr,
                       geom="boxplot",
                       ylab = "Ratio of voters to total population",
                         xlab = "Franchise types",
                       colour = opposition_2) 

participation.2 + guides(color = guide_legend(title="Opposition?")) + coord_flip()

# Figure 20: Turnout in elections with and without opposition
PIPE.turnout <- PIPE[ !is.na(PIPE$turnout_leg) , ]
turnout_leg.opposition <- qplot(data=PIPE.turnout[ !is.na(PIPE.turnout$opposition_2) , ],
                           x=year,
                           y=turnout_leg,
                           geom="point",
                           color=opposition_2,
                           ylab="Turnout in legislative elections")
turnout_leg.opposition + geom_smooth() + guides(color = guide_legend(title="opposition?"))


# Figure 21: Turnout in elections with and without opposition (boxplot)
turnout_leg.opposition.boxplot <- qplot(data=PIPE.turnout[ !is.na(PIPE.turnout$opposition_2) , ], 
                                        x=opposition_2,
                                        y=turnout_leg,
                                        colour = compulsory_2,
                                        geom="boxplot",
                                        ylab="Voter turnout in legislative elections, 1788-2008, all countries",
                                        xlab="Opposition?")
turnout_leg.opposition.boxplot + guides(color = guide_legend(title="Compulsory voting?")) + coord_flip() + opts(legend.position = "bottom")


# Figure 22: Turnout in all countries
turnout_country_3 <- qplot(data=PIPE[ !is.na(PIPE$turnout_leg), ],
                           y=turnout_leg,
                           x=reorder(countryn, median_turnout_leg),
                           geom = "boxplot",
                           ylab = "Turnout in legislative elections",
                           xlab = "Country",
                           colour = compulsory_2)
turnout_country_3 <- turnout_country_3 + coord_flip() 
turnout_country_3 <- turnout_country_3 + geom_hline(yintercept= 1,color= "red")
turnout_country_3 <- turnout_country_3 + guides(color = guide_legend(title="Compulsory voting?"))
#identify the USA, Venezuela, New Zealand
turnout_country_3 <- turnout_country_3 + geom_vline(xintercept= c(21,132,139),color= "black")

turnout_country_3 + opts(legend.position = "bottom")

# try also this, for turnouts with and without opposition
# turnout_country_3 + facet_grid(. ~ opposition_2)
# or this, for turnouts by continent
# turnout_country_3 + facet_grid(. ~ un_continent_name)
