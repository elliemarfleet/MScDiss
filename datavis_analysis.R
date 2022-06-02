## Data vis and analysis script


# import libraries 
library(readr)
library(dplyr) 
library(tidyr)
library(ggplot2)

# read in names file
names <- read_csv("names.csv")

# recode cluster numbers to correspond to worst - best sustainability 
names$Cluster[names$Cluster==0] <- 'Cluster 1'
names$Cluster[names$Cluster==1] <- 'Cluster 3'
names$Cluster[names$Cluster==2] <- 'Cluster 4'
names$Cluster[names$Cluster==3] <- 'Cluster 2'

# GET COUNTS FOR CLUSTERS
library(plyr)
count(names, "Cluster")

# extract individual clusters for analysis 
one = subset(names, Cluster == "Cluster 1")
two = subset(names, Cluster == "Cluster 2")
three = subset(names, Cluster == "Cluster 3")
four = subset(names, Cluster == "Cluster 4")

## Location of household regions per cluster
# Grouped bar
ggplot(names, aes(fill=HHoldGOR_B01ID,x=Cluster)) + 
  geom_bar(position = position_fill(reverse = TRUE)) + ggtitle("Household locations per cluster") + labs(fill = "Household region") + scale_y_continuous(labels=percent_format())

# plots of mode frequencies

## Frequency of bike use
# change the values in bike frequency so they're more meaningful
names$Bicycle3Freq_B01ID[names$Bicycle3Freq_B01ID==1] <- 'At least once a day'
names$Bicycle3Freq_B01ID[names$Bicycle3Freq_B01ID==2] <- '5 or more times a week, but not every day'
names$Bicycle3Freq_B01ID[names$Bicycle3Freq_B01ID==3] <- '3 or 4 times a week'
names$Bicycle3Freq_B01ID[names$Bicycle3Freq_B01ID==4] <- 'Once or twice a week'
names$Bicycle3Freq_B01ID[names$Bicycle3Freq_B01ID==5] <- 'Less than that but more than twice a month'
names$Bicycle3Freq_B01ID[names$Bicycle3Freq_B01ID==6] <- 'Once or twice a month'
names$Bicycle3Freq_B01ID[names$Bicycle3Freq_B01ID==7] <- 'Less than that but more than twice a year'
names$Bicycle3Freq_B01ID[names$Bicycle3Freq_B01ID==8] <- 'Once or twice a year'
names$Bicycle3Freq_B01ID[names$Bicycle3Freq_B01ID==9] <- 'Less than once a year'
names$Bicycle3Freq_B01ID[names$Bicycle3Freq_B01ID==10] <- 'Never'

ggplot(names, aes(Cluster, fill=factor(Bicycle3Freq_B01ID, levels=c("At least once a day", "5 or more times a week, but not every day", "3 or 4 times a week", "Once or twice a week", "Less than that but more than twice a month", "Once or twice a month", "Less than that but more than twice a year", "Once or twice a year", "Less than once a year", "Never")))) + geom_bar(position = position_fill(reverse = TRUE)) + labs(fill = "Bicycle3Freq_B01ID") + coord_flip() +
  scale_fill_manual(values=c("At least once a day" = "darkolivegreen", "5 or more times a week, but not every day" = "darkolivegreen4", "3 or 4 times a week" = "darkolivegreen3", "Once or twice a week" = "darkolivegreen2", "Less than that but more than twice a month" = "yellow", "Once or twice a month" = "gold2", "Less than that but more than twice a year" = "darkorange1", "Once or twice a year" = "darkorange2", "Less than once a year" = "red", "Never" = "red4")) + ggtitle("Cycling frequency per cluster") + labs(y='Proportion in cluster', fill='Frequency of use')


## Frequency of private car use
# change the values in car frequency so they're more meaningful
names$PrivCar2_B01ID[names$PrivCar2_B01ID==1] <- 'At least once a day'
names$PrivCar2_B01ID[names$PrivCar2_B01ID==2] <- '5 or more times a week, but not every day'
names$PrivCar2_B01ID[names$PrivCar2_B01ID==3] <- '3 or 4 times a week'
names$PrivCar2_B01ID[names$PrivCar2_B01ID==4] <- 'Once or twice a week'
names$PrivCar2_B01ID[names$PrivCar2_B01ID==5] <- 'Less than that but more than twice a month'
names$PrivCar2_B01ID[names$PrivCar2_B01ID==6] <- 'Once or twice a month'
names$PrivCar2_B01ID[names$PrivCar2_B01ID==7] <- 'Less than that but more than twice a year'
names$PrivCar2_B01ID[names$PrivCar2_B01ID==8] <- 'Once or twice a year'
names$PrivCar2_B01ID[names$PrivCar2_B01ID==9] <- 'Less than once a year'
names$PrivCar2_B01ID[names$PrivCar2_B01ID==10] <- 'Never'

ggplot(names, aes(Cluster, fill=factor(PrivCar2_B01ID, levels=c("At least once a day", "5 or more times a week, but not every day", "3 or 4 times a week", "Once or twice a week", "Less than that but more than twice a month", "Once or twice a month", "Less than that but more than twice a year", "Once or twice a year", "Less than once a year", "Never")))) + geom_bar(position = position_fill(reverse = TRUE)) + labs(fill = "PrivCar2_B01ID") + coord_flip() +
  scale_fill_manual(values=c("At least once a day" = "darkolivegreen", "5 or more times a week, but not every day" = "darkolivegreen4", "3 or 4 times a week" = "darkolivegreen3", "Once or twice a week" = "darkolivegreen2", "Less than that but more than twice a month" = "yellow", "Once or twice a month" = "gold2", "Less than that but more than twice a year" = "darkorange1", "Once or twice a year" = "darkorange2", "Less than once a year" = "red", "Never" = "red4")) + ggtitle("Frequency of private car use per cluster") + labs(y='Proportion in cluster', fill='Frequency of use')


## Frequency of bus use 
# change the values in bus frequency so they're more meaningful
names$OrdBus2Freq_B01ID[names$OrdBus2Freq_B01ID==1] <- 'At least once a day'
names$OrdBus2Freq_B01ID[names$OrdBus2Freq_B01ID==2] <- '5 or more times a week, but not every day'
names$OrdBus2Freq_B01ID[names$OrdBus2Freq_B01ID==3] <- '3 or 4 times a week'
names$OrdBus2Freq_B01ID[names$OrdBus2Freq_B01ID==4] <- 'Once or twice a week'
names$OrdBus2Freq_B01ID[names$OrdBus2Freq_B01ID==5] <- 'Less than that but more than twice a month'
names$OrdBus2Freq_B01ID[names$OrdBus2Freq_B01ID==6] <- 'Once or twice a month'
names$OrdBus2Freq_B01ID[names$OrdBus2Freq_B01ID==7] <- 'Less than that but more than twice a year'
names$OrdBus2Freq_B01ID[names$OrdBus2Freq_B01ID==8] <- 'Once or twice a year'
names$OrdBus2Freq_B01ID[names$OrdBus2Freq_B01ID==9] <- 'Less than once a year'
names$OrdBus2Freq_B01ID[names$OrdBus2Freq_B01ID==10] <- 'Never'

ggplot(names, aes(Cluster, fill=factor(OrdBus2Freq_B01ID, levels=c("At least once a day", "5 or more times a week, but not every day", "3 or 4 times a week", "Once or twice a week", "Less than that but more than twice a month", "Once or twice a month", "Less than that but more than twice a year", "Once or twice a year", "Less than once a year", "Never")))) + geom_bar(position = position_fill(reverse = TRUE)) + labs(fill = "OrdBus2Freq_B01ID") + coord_flip() +
  scale_fill_manual(values=c("At least once a day" = "darkolivegreen", "5 or more times a week, but not every day" = "darkolivegreen4", "3 or 4 times a week" = "darkolivegreen3", "Once or twice a week" = "darkolivegreen2", "Less than that but more than twice a month" = "yellow", "Once or twice a month" = "gold2", "Less than that but more than twice a year" = "darkorange1", "Once or twice a year" = "darkorange2", "Less than once a year" = "red", "Never" = "red4")) + ggtitle("Frequency of bus use per cluster") + labs(y='Proportion in cluster', fill='Frequency of use')


## Frequency of train use 
# change the values in train frequency so they're more meaningful
names$Train2Freq_B01ID[names$Train2Freq_B01ID==1] <- 'At least once a day'
names$Train2Freq_B01ID[names$Train2Freq_B01ID==2] <- '5 or more times a week, but not every day'
names$Train2Freq_B01ID[names$Train2Freq_B01ID==3] <- '3 or 4 times a week'
names$Train2Freq_B01ID[names$Train2Freq_B01ID==4] <- 'Once or twice a week'
names$Train2Freq_B01ID[names$Train2Freq_B01ID==5] <- 'Less than that but more than twice a month'
names$Train2Freq_B01ID[names$Train2Freq_B01ID==6] <- 'Once or twice a month'
names$Train2Freq_B01ID[names$Train2Freq_B01ID==7] <- 'Less than that but more than twice a year'
names$Train2Freq_B01ID[names$Train2Freq_B01ID==8] <- 'Once or twice a year'
names$Train2Freq_B01ID[names$Train2Freq_B01ID==9] <- 'Less than once a year'
names$Train2Freq_B01ID[names$Train2Freq_B01ID==10] <- 'Never'

ggplot(names, aes(Cluster, fill=factor(Train2Freq_B01ID, levels=c("At least once a day", "5 or more times a week, but not every day", "3 or 4 times a week", "Once or twice a week", "Less than that but more than twice a month", "Once or twice a month", "Less than that but more than twice a year", "Once or twice a year", "Less than once a year", "Never")))) + geom_bar(position = position_fill(reverse = TRUE)) + labs(fill = "Train2Freq_B01ID") + coord_flip() +
  scale_fill_manual(values=c("At least once a day" = "darkolivegreen", "5 or more times a week, but not every day" = "darkolivegreen4", "3 or 4 times a week" = "darkolivegreen3", "Once or twice a week" = "darkolivegreen2", "Less than that but more than twice a month" = "yellow", "Once or twice a month" = "gold2", "Less than that but more than twice a year" = "darkorange1", "Once or twice a year" = "darkorange2", "Less than once a year" = "red", "Never" = "red4")) + ggtitle("Frequency of train use per cluster") + labs(y='Proportion in cluster', fill='Frequency of use')


## Frequency of walking
# change the values in walk frequency so they're more meaningful
names$Walk2Freq_B01ID[names$Walk2Freq_B01ID==1] <- 'At least once a day'
names$Walk2Freq_B01ID[names$Walk2Freq_B01ID==2] <- '5 or more times a week, but not every day'
names$Walk2Freq_B01ID[names$Walk2Freq_B01ID==3] <- '3 or 4 times a week'
names$Walk2Freq_B01ID[names$Walk2Freq_B01ID==4] <- 'Once or twice a week'
names$Walk2Freq_B01ID[names$Walk2Freq_B01ID==5] <- 'Less than that but more than twice a month'
names$Walk2Freq_B01ID[names$Walk2Freq_B01ID==6] <- 'Once or twice a month'
names$Walk2Freq_B01ID[names$Walk2Freq_B01ID==7] <- 'Less than that but more than twice a year'
names$Walk2Freq_B01ID[names$Walk2Freq_B01ID==8] <- 'Once or twice a year'
names$Walk2Freq_B01ID[names$Walk2Freq_B01ID==9] <- 'Less than once a year'
names$Walk2Freq_B01ID[names$Walk2Freq_B01ID==10] <- 'Never'


ggplot(names, aes(Cluster, fill=factor(Walk2Freq_B01ID, levels=c("At least once a day", "5 or more times a week, but not every day", "3 or 4 times a week", "Once or twice a week", "Less than that but more than twice a month", "Once or twice a month", "Less than that but more than twice a year", "Once or twice a year", "Less than once a year", "Never")))) + geom_bar(position = position_fill(reverse = TRUE)) + labs(fill = "Walk2Freq_B01ID") + coord_flip() +
  scale_fill_manual(values=c("At least once a day" = "darkolivegreen", "5 or more times a week, but not every day" = "darkolivegreen4", "3 or 4 times a week" = "darkolivegreen3", "Once or twice a week" = "darkolivegreen2", "Less than that but more than twice a month" = "yellow", "Once or twice a month" = "gold2", "Less than that but more than twice a year" = "darkorange1", "Once or twice a year" = "darkorange2", "Less than once a year" = "red", "Never" = "red4")) + ggtitle("Walking frequency per cluster") + labs(y='Proportion in cluster', fill='Frequency of use')


## Taxi use frequency
# change the values in train frequency so they're more meaningful
names$TaxiCab2Freq_B01ID[names$TaxiCab2Freq_B01ID==1] <- 'At least once a day'
names$TaxiCab2Freq_B01ID[names$TaxiCab2Freq_B01ID==2] <- '5 or more times a week, but not every day'
names$TaxiCab2Freq_B01ID[names$TaxiCab2Freq_B01ID==3] <- '3 or 4 times a week'
names$TaxiCab2Freq_B01ID[names$TaxiCab2Freq_B01ID==4] <- 'Once or twice a week'
names$TaxiCab2Freq_B01ID[names$TaxiCab2Freq_B01ID==5] <- 'Less than that but more than twice a month'
names$TaxiCab2Freq_B01ID[names$TaxiCab2Freq_B01ID==6] <- 'Once or twice a month'
names$TaxiCab2Freq_B01ID[names$TaxiCab2Freq_B01ID==7] <- 'Less than that but more than twice a year'
names$TaxiCab2Freq_B01ID[names$TaxiCab2Freq_B01ID==8] <- 'Once or twice a year'
names$TaxiCab2Freq_B01ID[names$TaxiCab2Freq_B01ID==9] <- 'Less than once a year'
names$TaxiCab2Freq_B01ID[names$TaxiCab2Freq_B01ID==10] <- 'Never'

ggplot(names, aes(Cluster, fill=factor(TaxiCab2Freq_B01ID, levels=c("At least once a day", "5 or more times a week, but not every day", "3 or 4 times a week", "Once or twice a week", "Less than that but more than twice a month", "Once or twice a month", "Less than that but more than twice a year", "Once or twice a year", "Less than once a year", "Never")))) + geom_bar(position = position_fill(reverse = TRUE)) + labs(fill = "TaxiCab2Freq_B01ID") + coord_flip() +
  scale_fill_manual(values=c("At least once a day" = "darkolivegreen", "5 or more times a week, but not every day" = "darkolivegreen4", "3 or 4 times a week" = "darkolivegreen3", "Once or twice a week" = "darkolivegreen2", "Less than that but more than twice a month" = "yellow", "Once or twice a month" = "gold2", "Less than that but more than twice a year" = "darkorange1", "Once or twice a year" = "darkorange2", "Less than once a year" = "red", "Never" = "red4")) + ggtitle("Frequency of taxi use per cluster") + labs(y='Proportion in cluster', fill='Frequency of use')


## encouragement of cycling
# recode labels to responses
names$EncCycM_B01ID[names$EncCycM_B01ID==1] <- 'Safer roads'
names$EncCycM_B01ID[names$EncCycM_B01ID==2] <- 'Off-road and segregated cycle paths'
names$EncCycM_B01ID[names$EncCycM_B01ID==3] <- 'Safe cycle lanes'
names$EncCycM_B01ID[names$EncCycM_B01ID==4] <- 'Promotion of local cycling routes'
names$EncCycM_B01ID[names$EncCycM_B01ID==5] <- 'Secure storage / parking provision'
names$EncCycM_B01ID[names$EncCycM_B01ID==6] <- 'Access to showers / changing facilities at work'
names$EncCycM_B01ID[names$EncCycM_B01ID==7] <- 'Well-maintained road surfaces for cycling'
names$EncCycM_B01ID[names$EncCycM_B01ID==8] <- 'Better signposting of safer cycle routes'
names$EncCycM_B01ID[names$EncCycM_B01ID==9] <- 'Training to help me ride a bike or increase my confidence'
names$EncCycM_B01ID[names$EncCycM_B01ID==10] <- 'Cycle maintenance courses'
names$EncCycM_B01ID[names$EncCycM_B01ID==11] <- 'Better cycle hire facilities'
names$EncCycM_B01ID[names$EncCycM_B01ID==12] <- 'Nothing'
names$EncCycM_B01ID[names$EncCycM_B01ID==97] <- 'Other'
names$EncCycM_B01ID[names$EncCycM_B01ID==-8] <- '-8'

# Grouped bar
library(scales)
ggplot(names, aes(fill=EncCycM_B01ID,x=Cluster)) + 
  geom_bar(position = position_fill(reverse = TRUE)) + ggtitle("Encouragement of cycling per cluster") + labs(fill = "Response") + scale_y_continuous(labels=percent_format()) 

### Number of household bikes per cluster
# i.e. do clusters 1 and 2 possess household bikes they're just not using them?

# recode responses
names$NumBike[names$NumBike==0] <- 'No bikes'
names$NumBike[names$NumBike==1] <- 'One bike'
names$NumBike[names$NumBike==2] <- 'Two bikes'
names$NumBike[names$NumBike==3] <- 'Three bikes'
names$NumBike[names$NumBike==4] <- 'Four bikes'
names$NumBike[names$NumBike==5] <- 'Five bikes'
names$NumBike[names$NumBike==6] <- 'Six bikes'
names$NumBike[names$NumBike==7] <- 'Seven bikes'

ggplot(names, aes(Cluster, fill=factor(NumBike, levels=c("No bikes", "One bike", "Two bikes", "Three bikes", "Four bikes", "Five bikes", "Six bikes", "Seven bikes")))) + 
    geom_bar(position = position_fill(reverse = TRUE)) + ggtitle("Household bikes per cluster") + labs(fill = "Number of bikes") +
  scale_fill_manual(values=c("No bikes" = "blue", "One bike" = "palegreen", "Two bikes" = "palegreen1", "Three bikes" = "palegreen2", "Four bikes" = "palegreen3", "Five bikes" = "palegreen4", "Six bikes" = "seagreen4", "Seven bikes" = "olivedrab4")) + scale_y_continuous(labels=percent_format())

### Just Leeds responses as to what would encourage them to cycle more 

# Get the leeds LA responses
leeds <- subset(names, HHoldOSLAUA_B01ID=="E08000035")

library(scales)
ggplot(leeds, aes(fill=EncCycM_B01ID,x=Cluster)) + 
  geom_bar(position = position_fill(reverse = TRUE)) + ggtitle("Encouragement of cycling per cluster: Leeds") + labs(fill = "Response") + scale_y_continuous(labels=percent_format()) 


# creation of correlation matrix

# join PT metric to lookup table
pt <- read_csv("PTmetric.csv")
lookup <- read_csv("lookup.csv")

# join on LSOAcode
pt2 <- merge(pt, lookup, by = "LSOA11code")

# get avergae PT metric per MSOA as its currently LSOA level data
# aggregate column 6
b <- aggregate(pt2[, 6], list(pt2$`Public transport accessibility metric`), mean)

f1 <- pt2 %>% group_by(MSOA11CD) %>%
  mutate(PT_Mean = mean(`Public transport accessibility metric`))

# extract this MSOA column and PT mean per MSOA
f2 <- f1[c("MSOA11CD", "PT_Mean")]

# remove duplicated rows
f2 <- f2[!duplicated(f2), ]

# join number of cars per msoa / over 40
cars <- read_csv('cars2.csv')
ages <- read_csv('agegroups2.csv')
lonli <- read_csv("lonelinessMSOA.csv")

# join these to f2
final <- merge(f2, cars, by = "MSOA11CD")
final <- merge(final, ages, by = "MSOA11CD")
final <- merge(final, lonli, by = "MSOA11CD")

# create correlation matrix
library(DataExplorer)
plot_correlation(final, type = 'all', cor_args = list(method = "spearman", 
                                                      use = "pairwise.complete.obs"), 
                 ggtheme = theme_minimal(base_size = 6), na.omit(TRUE))