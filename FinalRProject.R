# Open Data with R - Final Project
## Sophia Wiesenfeld (swiesenfeld0@gatech.edu)
## September 20th, 2019

#Packages Used: installing and loading

#install.packages("corrplot")
#install.packages("tidyverse") 
#install.packages("skimr")
#install.packages("Hmisc")
#install.packages("manifestoR")
#install.packages("ggplot2")
#install.packages("predict3d")

library(tidyverse)
library(skimr)
library(Hmisc)
library(ggplot2)
library(manifestoR)
library(corrplot)
library(predict3d)

#Accessing and Loading the Manifesto Project Database-- account needed for API key
#save your generated API key in a txt file named manifesto_apikey.txt
#Codebook found at https://manifesto-project.wzb.eu/down/data/2019a/codebooks/codebook_MPDataset_MPDS2019a.pdf
mp_setapikey("manifesto_apikey.txt")
fullDataset <- mp_maindataset()

###################################
#Data Transformations - for ease of use

#filter by USA manifestos
manifestoUSA <- filter(fullDataset, country=="61")
manifestoUSA <- manifestoUSA %>% mutate(fraKai = franzmann_kaiser(manifestoUSA))

#Removing the two outlier parties that exist in the dataset, only Republican and Democrat manifestos remain
manifestoUSA <- filter(manifestoUSA, parfam != 20)
manifestoUSA <- filter(manifestoUSA, parfam != 90)

#Making the dataset numeric and simplifying some columns
#date is in the format YYYYMM
#parfam - "Tentative grouping of political parties and alliances into party families"
#per101-per706 are mentions of political stances
#100s - External Relations (ie Internationalism)
#200s - Freedom and Democracy
#300s - Political System
#400s - Economy
#500s - Welfare and Quality of Life (ie Education Expansion)
#600s - Fabric of Society (ie Positive/Negative Multiculturalism)
#700s - Social Groups (ie Labor Groups, Minority Groups)
#RILE is the most common method of calculating a manifesto's left-right position based on selected per-values by Laver and Budge
mpNumeric <- select(manifestoUSA,date,parfam,
                                 per101,per102,per103,per104,per105,per106,per107,per108,per109,per110,
                                 per201,per202,per203,per204,
                                 per301,per302,per303,per304,per305,
                                 per401,per402,per403,per404,per405,per406,per407,per408,per409,per410,per411,per412,per413,per414,per415,per416,
                                 per501,per502,per503,per504,per505,per506,per507,
                                 per601,per602,per603,per604,per605,per606,per607,per608,
                                 per701,per702,per703,per704,per705,per706,
                                 rile,planeco,markeco,welfare,intpeace, fraKai)

#Creating a value 'politParty' of 0 or 1 for the Democratic and Republican Parties, respectively.
#For future linear regression model
mpNumericRepublicanDemocratOnly <- mpNumericRepublicanDemocratOnly %>% mutate(politParty = parfam/30 - 1)

############################
#Exploratory Data Analysis
#Investigating the hypothesis that USA manifestos reflect the idea that the US emerged as a global super power after WWII, was that belief reflected at that time as well as now?
#Seeking correlation between potential per-values that might reflect this
#per107 - Internationalism: Positive. Need for international co-operation, including co-operation with specific countries other than those coded in 101.
#per202 - Democracy. Favorable mentions of democracy as the "only game in town".
#per305 - Political Authority. References the manifesto party's competence to govern and/or other party's lack of such competence. Also includes favorable mentions of the desirability of a strong and/or stable government in general.
#per601 - National Way of Life: Positive. Favorable mentions of the manifesto country's nation, history, and general appeals.
explDataAnalysis <- select(mpNumericRepublicanDemocratOnly, date, politParty, per107, per202, per305, per601)

#Showing general correlations
corrplot(cor(explDataAnalysis))

#Shows spike in political authority ideology for both political parties after WWII
ggplot(explDataAnalysis, aes(x=date, y=per305, color=politParty)) + geom_point()

#and that spike is not driven by or limited to either Party
ggplot(explDataAnalysis, aes(x=date, y=per305, color=politParty)) + geom_point() +geom_line() + facet_grid(. ~ politParty)
#############################
#Data Models
#Comparing left/right scaling methods for determining political party stance by per-values alone
# manifestoR package very necessary

#Variation in RILE score for Democratic Party manifestos and Republican manifestos(Laver & Budge 1992)
ggplot(mpNumericRepublicanDemocratOnly, aes(x=date, y=rile, color=politParty)) + geom_point()

#Variations in fraKai score, another L/R scaling method (Franzmann & Kaiser 2006)
ggplot(mpNumericRepublicanDemocratOnly, aes(x=date, y=fraKai, color=politParty)) + geom_point()

#vanilla
mpNumericRepublicanDemocratOnly <- mpNumericRepublicanDemocratOnly %>% mutate(vanilla = vanilla(manifestoUSA))
ggplot(mpNumericRepublicanDemocratOnly, aes(x=date, y=vanilla, color=politParty)) + geom_point()

#creating the models:
scalingModelRile <- glm(politParty ~ rile,
                          family=binomial(link=logit), data=mpNumericRepublicanDemocratOnly)
summary(scalingModelRile)

scalingModelfraKai <- glm(politParty ~ fraKai + date,
                        family=binomial(link=logit), data=mpNumericRepublicanDemocratOnly)
summary(scalingModelfraKai)
#it seems like the fraKai model doesn't work, potentially due to the NA values created by the fraKai method

scalingModelVanilla <- glm(politParty ~ vanilla,
                           family=binomial(link=logit), data=mpNumericRepublicanDemocratOnly)
summary(scalingModelVanilla)

#visualizing the differences
ggpredict(scalingModelRile,se=TRUE,interactive=TRUE, show.text=FALSE)

ggpredict(scalingModelVanilla,se=TRUE,interactive=TRUE, show.text=FALSE)
#It seems like both RILE and vanilla scores can predict a political party with a high level of accuracy
