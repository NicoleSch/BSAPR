#Set working directory
setwd("~/BSAPR Assessment")

#Create dataset

id <- c(1,2,3,4,5)
id <- 1:5
id <- seq (1,10, 0.1) #Last number is the step
id <- seq(1,5,1) #Last number is the step

names <- c("Mark", "Jack", "Jill", "Anna", "Tom")

gender <- c(0,0,1,1,0)

a<-1:5
b<-6:10
matc <- cbind(a,b) #create a matrix; by columns
matr <- rbind(a,b) #create a matrix; by rows

dat <- data.frame(ID=id, Name=names, Gender=gender) #Create data frame

dat[1,1] #Get data out of a dataframe, 1st is row, 2nd is column
dat[,1] #Gives back the first column
dat[1,] #Gives back the first row
dat[,c("ID","Gender")]

dat$ID
dat$Gender #Gives back the whole column -> $

sel <- dat$Gender ==  1 #Check/compare my data for equal values
show(sel)
show(!sel) #! says not
sum(sel)

dat[sel, ] #Gives back all the girls


# Open data
data <- read.csv("data/raw/perulung_ems.csv")

str(data) #Gives an overview, what valuables are in there
head(data, 1) #Gives the first value of the data
head(data, 10) #Gives 10 values
tail(data, 1) #Gives the last values
View(data) #Opens up data -> Cave: V capitalized
summary(data)

data$sex_f <- factor(data$sex, levels=c(0,1), labels=c("f","m")) #Making a factor out of the sex
head(data)
table(data$sex)
table(data$sex_f)
summary(data$sex) #Gives a mean etc.
summary(data$sex_f) #When it is a categorical value, then it is not giving back means etc. 
                    #It does not make sense to have a mean of genders

library(tidyverse)


data %>% select(sex, sex_f) %>% filter(sex==1) %>% table #select: select columns, filter: selects rows
