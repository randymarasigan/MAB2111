#-------------------------------------------------------------------------------------
# Submitted by: Randy A. Marasigan
# MAB-2111
#
# Assignment 01 in R
# 
# 1. WHO dataset
# b. country with the biggets population
# c. population of Malaysia
# d. country with the lowest literacy
# e. Richest country in Europe based on GNI
# f. Mean Life expectancy of countries in Africa
# g. Number of countries with population greater than 10,000
# h. Top 5 countries in the Americas with the highest child mortality
# 
# 2. NBA dataset (Historical NBA Performance.xlsx) 
# a. The year Bulls has the highest winning percentage 
# b. Teams with an even win-loss record in a year
# 
# 3. Seasons_Stats.csv
# a. Bonus: Player with the highest 3-pt attempt rate in a season. 
# b. Bonus: Player with the highest free throw rate in a season. 
# c. What year/season does Lebron James scored the highest? 
# d. What year/season does Michael Jordan scored the highest? 
# e. Player efficiency rating of Kobe Bryant in the year where his MP is the lowest?
#   
# 4. National Universities Rankings.csv
# a. University with the most number of undergrads 
# b. Average Tuition in the Top 10 University
#--------------------------------------------------------------------------------------
# Note: Please set your working directory for this code to run
# together with the datasets
# 1. WHO dataset
# Read the dataset using read.csv
WHO <- read.csv('WHO.csv')

# 1.b. country with the biggest population
# Answer = China

# Get the max in Population column
max_pop <- max(WHO$Population)
# subset dataset with Population == max_pop
biggest_pop <- subset(WHO, Population == max_pop)
# Get the country name
country_biggest_pop <- biggest_pop$Country
#Print Country with biggest population
country_biggest_pop

# 1.c. population of Malaysia
# Answer = 29,240

# Get Country == Malyasia from WHO dataset
malaysia <- subset(WHO, Country == 'Malaysia')
# Print population
malaysia$Population

# GRADED ASSIGNMENT STARTS HERE------------------------------------

# 1.d. country with the lowest literacy
# Answer = Mali

# GEt the min value from LiteracyRate
low_literacy <- min(WHO$LiteracyRate, na.rm = TRUE)
# subset to the lowest literacy rate record
lowest_literacy_rate <- subset(WHO, LiteracyRate == low_literacy)
# print result = Country
lowest_literacy_rate$Country

# 1.e. Richest country in Europe based on GNI
# Answer = Luxembourg

# subset data where Region == Europe
europe <- subset(WHO, Region == 'Europe')
# get the max GNI
europe_max_gni <-max(europe$GNI, na.rm = TRUE)
country_europe_max_gni <- subset(europe, GNI == europe_max_gni)
# print the name of the country
country_europe_max_gni$Country

# 1.f. mean life expectancy of countries in Africa
#answer = 57.95652

# subset data where Region == Europe
africa <- subset(WHO, Region == 'Africa')
#mle in africe use mean of LifeExpectancy column
mle_africa <- mean(africa$LifeExpectancy, na.rm = TRUE)
# print answer
mle_africa

# 1.g. number of countries with a population over 10M
# Answer = 86

# subset where Population is greater than 10M;
countries_10M <- WHO$Population > 10000
# print the sum of all countries with population > 10M
sum(countries_10M)

# 1.h. Top 5 countries in the Americas with the highest child mortality
# Answer: Haiti, Bolivia, Guyana, Bolivia (Plurinational State of), Dominican Repulic

# subset data where Region == Americas
americas <- subset(WHO, Region == 'Americas')

# order result in decreasing order
americas_childMortality <- order(americas$ChildMortality, decreasing = TRUE)

# re-order
dF_americas_childMortality <- americas[americas_childMortality,]

# get top 5 using head function
top5_americas_childMortality <- head(dF_americas_childMortality,5)

# Print answer: Haiti, Guyana, Bolivia (Plurinational State of), Guatemala, and Domican Republic
top5_americas_childMortality$Country

#----------------------------------------------------------------------------------------
# 2. NBA dataset (Historical NBA Performance.xlsx)
# since dataset is in excel you must install the readxl package
# or the XLSX package as per Sir Elmer Peramo; I'm not using it because 
#I am having some error loading the library
# Update: According to Sir Elmer, you can convert the data to csv thus red.csv is accepted

#found readxl for reading excel files
install.packages("readxl")
# then use the library readxl
library("readxl")
#warning message: package 'readxl' was built under R version 3.5.1
#but go on

# read the Historical NBA Performance.xlsx
nba <- read_excel('Historical NBA Performance.xlsx')
# or if you converted the excel file to csv then use
#nba <- read.csv('Historical NBA Performance.csv')

# 2.a. The year Bulls has the highest winning percentage 
# Answer: 1995-96 with 0.878 Winning Percentage
# subset all Bulls record
bulls <- subset(nba, Team == 'Bulls', na.rm = TRUE)
# get highest winning percentage
bulls_highest <- max(bulls$'Winning Percentage')
bulls_highest_win <- subset(bulls, `Winning Percentage` == bulls_highest)
# print answer 
bulls_highest_win
#print answer just the year/season: 1995-96
bulls_highest_win$Year

# 2.b. Teams with an even win-loss record in a year
# Answer: 53 observations
# get 0.5 win percentage
even_win_loss <- subset(nba, `Winning Percentage` == 0.5)
#print the team names only
even_win_loss$Team
#print all (Year, Team, Record, 'Winning Percentage')
even_win_loss

#----------------------------------------------------------------------------------------
# 3. Seasons_Stats.csv

# load the seasons stats data 
nba_seasons_stats <- read.csv('Seasons_Stats.csv')

# BONUS
#3.a. Player with the highest 3-pt attempt rate in a season. 

# use 3PAr as 3-Point Attempt Rate = X3PAr
highest_X3PAr <- max(nba_seasons_stats$X3PAr, na.rm =TRUE)
# the result is a dataframe with 160 rows
players_highest_X3PAr <- subset(nba_seasons_stats, X3PAr == highest_X3PAr)
# Players with highest X3PAr (The result will be multiple players)
players_highest_X3PAr$Player

#BONUS
# 3.b. Player with the highest free throw rate in a season. 
# User FTr - Free Throw Rate
highest_FTr <- max(nba_seasons_stats$FTr, na.rm = TRUE)
players_highest_FTr <- subset(nba_seasons_stats, FTr == highest_FTr)
#Print players with highest FTr
#Answer: Dwayne Jones and Andris Biedrins
players_highest_FTr
#Print names only
players_highest_FTr$Player

# 3.c. What year/season does Lebron James scored the highest?
#find LeBron
lebron <- subset(nba_seasons_stats, Player == 'LeBron James')
#Get max points
lebron_max <- max(lebron$PTS, na.rm = TRUE)
#Get the record with max points
lebron_max_pts <- subset(lebron, PTS == lebron_max) 
# print answer: Year 2006
lebron_max_pts$Year

# 3.d. What year/season does Michael Jordan scored the highest? 
#Find Jordan with the asterisk
jordan <- subset(nba_seasons_stats, Player == 'Michael Jordan*')
#Get max points
jordan_max <- max(jordan$PTS, na.rm = TRUE)
#Get the record with max points
jordan_max_pts <- subset(jordan, PTS == jordan_max) 
# print answer: Year 1987
jordan_max_pts$Year

# 3.e. Player efficiency rating of Kobe Bryant in the year where his MP is the lowest?
#answer: 10.7
# find kobe
kobe <- subset(nba_seasons_stats, Player =='Kobe Bryant')
#Use PER - Player Efficiency Rating and MP 
#find Kobe's lowest MP
kobe_minMP <- min(kobe$MP, na.rm = TRUE)
#kobe_minMP = 177
#get the subset record of that min MP
kobe_minMP_sub <- subset(kobe, MP == kobe_minMP)
#Print record
kobe_minMP_sub
#Print what Year?
kobe_minMP_sub$Year
#Use the lowest MP to find Kobe's lowest efficiency rating
kobe_lowest_PER <- subset(kobe, MP == kobe_minMP)$PER
#print answer PER: 10.7
kobe_lowest_PER


#----------------------------------------------------------------------------------------
# 4. National Universities Rankings.csv
univ_rank <-read.csv('National Universities Rankings.csv')
#check data
univ_rank
#view(univ_rank)

# a. University with the most number of undergrads
#Answer: University of Central Florida with 54513
#check names to see columns
names(univ_rank)
#Check class to check if numeric
class(univ_rank$Undergrad.Enrollment) #factor
#since data in the column is not numeric, use gsub to clean the comma and then coerce to numeric
univ_rank$undergrad_num <- as.numeric(gsub("[,]", "", univ_rank$Undergrad.Enrollment))
#check data if cleaned
univ_rank$undergrad_num

# Print answer using which.max and $Name
#Answer: University of Central Florida
univ_rank[which.max(univ_rank$undergrad_num),]$Name

# b. Average Tuition in the Top 10 University
#Answer: 49895.2
#Get Top Ten Universities
top_10_univ <- subset(univ_rank[order(univ_rank$Rank),][1:10,])
#check subsetted data
top_10_univ

#check class of Tuition.and.fees
class(top_10_univ$Tuition.and.fees)
# it is a factor and not numeric, need to coerce to numeric
# remove "$" and "," with "" and create a new column
top_10_univ$fee_numeric <- as.numeric(gsub("[$]|[,]", "", top_10_univ$Tuition.and.fees))
# check if $ and , was removed
top_10_univ$fee_numeric
#check if numeric
class(top_10_univ$fee_numeric)
#Get Mean of top 10 universities
#print answer: 49895.2
mean(top_10_univ$fee_numeric)

