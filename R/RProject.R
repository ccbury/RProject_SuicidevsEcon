install.packages("tidyverse", "dplyr", "patchwork", "hrbrthemes")

library(ggplot2)
library(dplyr)
library(tidyverse)
library(patchwork)
library(hrbrthemes)

# Constants for graphing
incomeColor <- "#0000FF"
casesColor <- "#FF0000"
coeff <- 1000


# Read income from csv file.
incomeOrig <- read.csv("income.csv")
# Change names of columns so that when pivot is performed they will be integers
colnames(incomeOrig)[3]<-2013
colnames(incomeOrig)[4]<-2014
colnames(incomeOrig)[5]<-2015
colnames(incomeOrig)[6]<-2016
colnames(incomeOrig)[7]<-2017
# Remove all entries except total income per person
income <- incomeOrig[apply(incomeOrig,1,function(Type){
  any(grepl("Total Income per Person", Type))}),]
income <- income[apply(income,1,function(Type){
  !any(grepl("Index of Total Income", Type))}),]

# Pivot table so that 2013, 2014 ect become entried under a year column
# As a result the value for income will be stored under Income instead
incomePivot <- income %>%
  pivot_longer(-c(Type, Area),
               names_to = "Year",
               values_to = "Income")
incomePivot$Year <- as.integer(incomePivot$Year)


# Read population from csv file
# Create new set where population is divided by 100,000
# This is for creating a suicide rate per 100,000 later
population <- read.csv("population.csv")
populationDivided <- population %>%
  mutate(Population = Population / 100000)


# Read suicides from csv file
# Remove all entried except those containing information on both Sexes
suicides <- read.csv("VSD30.csv")
maleSuicides <- suicides[apply(suicides,1,function(Sex){
  any(grepl("Male", Sex))}),]
maleSuicidesCapita = merge(maleSuicides, populationDivided)
maleSuicidesCapita <- mutate(maleSuicidesCapita, Cases = Cases / Population)
maleSuicidesCapita <- maleSuicidesCapita[, -c(4,5)]

femaleSuicides <- suicides[apply(suicides,1,function(Sex){
  any(grepl("Female", Sex))}),]
femaleSuicidesCapita = merge(femaleSuicides, populationDivided)
femaleSuicidesCapita <- mutate(femaleSuicidesCapita, Cases = Cases / Population)
femaleSuicidesCapita <- femaleSuicidesCapita[, -c(4,5)]

suicides <- suicides[apply(suicides,1,function(Sex){
  any(grepl("Both sexes", Sex))}),]

# Create suicidesCapita by merging population
# This adds populationDivided so that we can get suicides per 100,000 people
suicidesCapita = merge(suicides, populationDivided)
suicidesCapita <- mutate(suicidesCapita, Cases = Cases / Population)
suicidesCapita <- suicidesCapita[, -c(4,5)]

# This final Merge compiles incomePivot and SuicidesCapita so that graphing is possible
incomeCasesGraph <- merge(incomePivot, suicidesCapita)
incomeCasesGraph.combined = incomeCasesGraph %>%
  unite(YearArea, c(Area, Year),sep=" ", remove=FALSE)
ggplot(head(incomeCasesGraph.combined, 80), aes(x=YearArea)) +
  geom_col(aes(y=Income), color="blue",fill=incomeColor, alpha=.6, size =.1) +
  geom_col(aes(y=Cases*coeff), color="red",fill=casesColor, alpha=.6, size =.1) +
  scale_y_continuous(
    name = ("Income per Person (Euro)"),
    sec.axis = sec_axis(~./coeff,name="Cases per 100,000")) +
  ggtitle("Suicide per 100,000 vs Income per Person by County & Year")+
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = incomeColor, size=13, face ="bold"),
    axis.title.y.right = element_text(color = casesColor, size=13, face ="bold"),
    axis.text.x = element_text(angle=60,hjust=1, size=8),
    axis.title.x = element_text(size=13, color="gray", face ="bold"))
# This graph is the same except it shows the bar in descending order of income per person. rather than by county and year
ggplot(head(incomeCasesGraph.combined, 80), aes(x=reorder(YearArea, -Income))) +
  geom_col(aes(y=Income), color="blue",fill=incomeColor, alpha=.6, size =.1) +
  geom_col(aes(y=Cases*coeff), color="red",fill=casesColor, alpha=.6, size =.1) +
  scale_y_continuous(
    name = ("Income per Person (Euro)"),
    sec.axis = sec_axis(~./coeff,name="Cases per 100,000")) +
  ggtitle("Suicide / 100,000 vs Income / Person (County & Year), Descending Income")+
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = incomeColor, size=13),
    axis.title.y.right = element_text(color = casesColor, size=13),
    axis.text.x = element_text(angle=60,hjust=1, size=8),
    axis.title.x = element_text(size=13, color="gray"))
# Plot same graph for Male cases only
maleIncomeCasesGraph <- merge(incomePivot, maleSuicidesCapita)
maleIncomeCasesGraph.combined = maleIncomeCasesGraph %>%
  unite(YearArea, c(Area, Year),sep=" ", remove=FALSE)
ggplot(head(maleIncomeCasesGraph.combined, 80), aes(x=YearArea)) +
  geom_col(aes(y=Income), color="blue",fill=incomeColor, alpha=.6, size =.1) +
  geom_col(aes(y=Cases*coeff), color="red",fill=casesColor, alpha=.6, size =.1) +
  scale_y_continuous(
    name = ("Income per Person (Euro)"),
    sec.axis = sec_axis(~./coeff,name="Cases per 100,000")) +
  ggtitle("Male Suicide / 100,000 vs Income per Person by County & Year")+
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = incomeColor, size=13),
    axis.title.y.right = element_text(color = casesColor, size=13),
    axis.text.x = element_text(angle=60,hjust=1, size=8),
    axis.title.x = element_text(size=13, color="gray"))
# Plot same graph for female cases only
femaleIncomeCasesGraph <- merge(incomePivot, femaleSuicidesCapita)
femaleIncomeCasesGraph.combined = femaleIncomeCasesGraph %>%
  unite(YearArea, c(Area, Year),sep=" ", remove=FALSE)
ggplot(head(femaleIncomeCasesGraph.combined, 80), aes(x=YearArea)) +
  geom_col(aes(y=Income), color="blue",fill=incomeColor, alpha=.6, size =.1) +
  geom_col(aes(y=Cases*coeff), color="red",fill=casesColor, alpha=.6, size =.1) +
  scale_y_continuous(
    name = ("Income per Person (Euro)"),
    sec.axis = sec_axis(~./coeff,name="Cases per 100,000")) +
  ggtitle("Female Suicide / 100,000 vs Income per Person by County & Year")+
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = incomeColor, size=13),
    axis.title.y.right = element_text(color = casesColor, size=13),
    axis.text.x = element_text(angle=60,hjust=1, size=8),
    axis.title.x = element_text(size=13, color="gray"))


incomeSelfEmployed <- incomeOrig[apply(incomeOrig,1,function(Type){
  any(grepl("Primary Income", Type))}),]


incomeSelfPivot <- incomeSelfEmployed %>%
  pivot_longer(-c(Type, Area),
               names_to = "Year",
               values_to = "Income")
incomeSelfPivot$Year <- as.integer(incomeSelfPivot$Year)

selfIncomeCases <- merge(incomeSelfPivot, suicidesCapita)
selfIncomeCases.combined = selfIncomeCases %>%
  unite(YearArea, c(Area, Year),sep=" ", remove=FALSE)

selfIncomeCases.combined <- selfIncomeCases.combined[apply(selfIncomeCases.combined,1,function(Area){
  !any(grepl("Dublin", Area))}),]

ggplot(head(selfIncomeCases.combined, 80),  aes(x=reorder(YearArea, -Income))) +
  geom_col(aes(y=Income), color="red",fill=casesColor, alpha=.6, size =.1) +
  geom_col(aes(y=Cases*200), color="blue",fill=incomeColor, alpha=.6, size =.1) +
  scale_y_continuous(
    name = ("Disposable Income per Person (Euro)"),
    sec.axis = sec_axis(~./200, name="Cases per 100,000")) +
  ggtitle("Disposable Income per Person (Euro) vs Suicide Cases per 100,000")+
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = casesColor, size=13),
    axis.title.y.right = element_text(color = incomeColor, size=13),
    axis.text.x = element_text(angle=60,hjust=1, size=8),
    axis.title.x = element_text(size=13, color="gray"))


View(selfIncomeCases.combined)
View(selfIncomeCases)

# Merge income and suicides then combine year and area
sci <- merge(incomePivot, suicidesCapita)
sci.combined = sci %>%
  unite(YearArea, c(Area, Year),sep=" ", remove=FALSE)
# create datasets for each year available individually
sci.x2013 <- sci[sci$Year == 2013,]
sci.x2014 <- sci[sci$Year == 2014,]
sci.x2015 <- sci[sci$Year == 2015,]
sci.x2016 <- sci[sci$Year == 2016,]
sci.x2017 <- sci[sci$Year == 2017,]
# plotting graphs for 2017 to 2013 individually
ggplot(head(sci.x2017, 80), aes(x=Area)) +
  geom_col(aes(y=Income), color="blue",fill=incomeColor, alpha=.6, size =1) +
  geom_col(aes(y=Cases*coeff), color="red",fill=casesColor, alpha=.6, size =1) +
  scale_y_continuous(
    name = ("Income per Person (Euro)"),
    sec.axis = sec_axis(~./coeff,name="Cases per 100,000")) +
  ggtitle("Suicide per 100,000 vs Income per Person (2017)")+
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = incomeColor, size=13),
    axis.title.y.right = element_text(color = casesColor, size=13))
ggplot(head(sci.x2016, 80), aes(x=Area)) +
  geom_col(aes(y=Income), color="blue",fill=incomeColor, alpha=.6, size =1) +
  geom_col(aes(y=Cases*coeff), color="red",fill=casesColor, alpha=.6, size =1) +
  scale_y_continuous(
    name = ("Income per Person (Euro)"),
    sec.axis = sec_axis(~./coeff,name="Cases per 100,000")) +
  ggtitle("Suicide per 100,000 vs Income per Person (2016)")+
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = incomeColor, size=13),
    axis.title.y.right = element_text(color = casesColor, size=13))
ggplot(head(sci.x2015, 80), aes(x=Area)) +
  geom_col(aes(y=Income), color="blue",fill=incomeColor, alpha=.6, size =1) +
  geom_col(aes(y=Cases*coeff), color="red",fill=casesColor, alpha=.6, size =1) +
  scale_y_continuous(
    name = ("Income per Person (Euro)"),
    sec.axis = sec_axis(~./coeff,name="Cases per 100,000")) +
  ggtitle("Suicide per 100,000 vs Income per Person (2015)")+
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = incomeColor, size=13),
    axis.title.y.right = element_text(color = casesColor, size=13))
ggplot(head(sci.x2014, 80), aes(x=Area)) +
  geom_col(aes(y=Income), color="blue",fill=incomeColor, alpha=.6, size =1) +
  geom_col(aes(y=Cases*coeff), color="red",fill=casesColor, alpha=.6, size =1) +
  scale_y_continuous(
    name = ("Income per Person (Euro)"),
    sec.axis = sec_axis(~./coeff,name="Cases per 100,000")) +
  ggtitle("Suicide per 100,000 vs Income per Person (2014)")+
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = incomeColor, size=13),
    axis.title.y.right = element_text(color = casesColor, size=13))
ggplot(head(sci.x2013, 80), aes(x=Area)) +
  geom_col(aes(y=Income), color="blue",fill=incomeColor, alpha=.6, size =1) +
  geom_col(aes(y=Cases*coeff), color="red",fill=casesColor, alpha=.6, size =1) +
  scale_y_continuous(
    name = ("Income per Person (Euro)"),
    sec.axis = sec_axis(~./coeff,name="Cases per 100,000")) +
  ggtitle("Suicide per 100,000 vs Income per Person (2013)")+
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = incomeColor, size=13),
    axis.title.y.right = element_text(color = casesColor, size=13))

#Views for all data sets used
View(incomeOrig)
View(income)
View(incomeSelfEmployed)
View(suicides)
View(maleSuicides)
View(femaleSuicides)
View(population)

View(incomePivot)
View(incomeSelfPivot)
View(suicidesCapita)
View(maleSuicidesCapita)
View(femaleSuicidesCapita)
View(populationDivided)
View(sci)
View(sci.combined)
