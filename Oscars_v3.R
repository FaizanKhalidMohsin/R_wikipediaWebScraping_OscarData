#Get award winner information other awards -> 
#Get Oscar Winners #https://datahub.io/rufuspollock/oscars-nominees-and-winners#resource-data
#http://blog.arjun.io/oscars/machine-learning/2016/02/23/predicting-the-oscars.html
#https://www.gokhan.io/post/scraping-wikipedia/
#http://blog.corynissen.com/2015/01/using-rvest-to-scrape-html-table.html
#https://machinelearningmastery.com/machine-learning-in-r-step-by-step/

#Metadata information from http://www.omdbapi.com/

# install.packages("rvest") #download from wiki
# install.packages("plyr") #for pipes in functions
# install.packages("dplyr") #for pipes in functions - dplyr is re-imagined plry package
# install.packages('xml2') 
# install.packages("caret", dependencies=c("Depends", "Suggests")) #machine learning



list.of.packages <- c("rvest", "Rcpp", "dplyr","xml2" )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)




library('xml2')
library("rvest") #install package
library("plyr")
library("dplyr")
library("caret")

################################### SERIOUS SCRAPING ########################################

#create a function to extract nominees +  winners from wikipedia tables and append them together

f <- function(url,a,b,c) { 
  
  x <- url %>%
    read_html() %>% #read the url
    html_nodes(xpath = paste('//*[@id="mw-content-text"]/div/table[',a,']')) %>% #select the 90s table we want to scrape
    html_table(fill = TRUE) #convert to table
  x <- data.frame(x)
  y <- url %>%
    read_html() %>%
    html_nodes(xpath = paste('//*[@id="mw-content-text"]/div/table[',b,']'))  %>%
    html_table(fill = TRUE)
  y <- data.frame(y)
  z <- url %>%
    read_html() %>%
    html_nodes(xpath = paste('//*[@id="mw-content-text"]/div/table[',c,']'))  %>%
    html_table(fill = TRUE)
  z <- data.frame(z)
  DF <- rbind.fill(x,y,z) #this is to ignore non matching column names (from the plyr package)
  return(DF)
}

#Getting Data
#Golden Globes
gg<- f("https://en.wikipedia.org/wiki/Golden_Globe_Award_for_Best_Motion_Picture_%E2%80%93_Drama",7,8,9)


#SAGS
sag<- f("https://en.wikipedia.org/wiki/Screen_Actors_Guild_Award_for_Outstanding_Performance_by_a_Cast_in_a_Motion_Picture",2,3,4)

#DIRECTOR'S GUILD

dg <- f("https://en.wikipedia.org/wiki/Directors_Guild_of_America_Award_for_Outstanding_Directing_%E2%80%93_Feature_Film", 7,8,9)

#BAFTAS

baf<- f('https://en.wikipedia.org/wiki/BAFTA_Award_for_Best_Film',7,8,9)

#OSCARS
aa <- f("https://en.wikipedia.org/wiki/Academy_Award_for_Best_Picture",10,11,12)


############## SERIOUS STRUCTURE OF DATA ############################################################

# WE WANT A COMPLETE TABLE WITH "YEAR", "FILM", "AAWON", "BAFWON", "DGWON", "GGWON", SAGWON"

#Overall Functions

### this function trims all film titles of special characters###
tr <- function(x){
  y <- trimws(gsub("[[:punct:]]","",x))
  return(y)
}

### this function trims years of all special characters and spaces ####
yr <- function(x){
  y <- strtrim(x,4)
  return(y)
}

### GOLDEN GLOBES
=
#adding a new win column and fixing column names

gg$ggwin <- "NO" #default is no
colnames(gg) <- c("year", 'film', 'director', 'producer', 'ggwin')

#removing columns we don't need
gg$director <- NULL
gg$producer <- NULL

gg$year <- yr(gg$year) #cleaning year data
gg$film <- tr(gg$film) #cleaning film data

#adding winners and nominees
gg$ggwin[seq(1, nrow(gg),5)] <- "YES" #every 5th entry is a win

########SAGS#####

#adding a new win column and fixing column names
sag$sagwin <- "NO" #default is no
colnames(sag) <- c("year", 'film', 'cast', 'sagwin') 

#removing columns we don't need
sag$cast <- NULL 

sag$year <- yr(sag$year) #cleaning year data
sag$film <- tr(sag$film) #cleaning film data

#adding winners and losers
sag$sagwin[seq(1, 46 ,5)] <- "YES" #every 5th entry is a win until 2004
sag$sagwin[seq(52, nrow(sag) ,5)] <- "YES" #every 5th entry is a win after

#####DIRECTOR'S GUILD########

#adding a new win column and fixing column names
dg$dgwin <- "NO"
colnames(dg) <- c("year","people","film", "ref", "dgwin")

#removing columns we don't need
dg$people <- NULL
dg$ref <- NULL

dg$year <- yr(dg$year) #cleaning year data
dg$film <- tr(dg$film) #cleaning film data

dg$dgwin[seq(1,nrow(dg),5)] <- "YES" #every 6th entry is a win

#######BAFTAS

#adding a new win column and fixing column names

baf$bafwin <- "NO" #default is no
colnames(baf) <- c("category", 'film', 'dir', 'prod', "ctr", "year", "nom", "bafwin") 

#removing columns we don't need
baf$category <- NULL
baf$dir <- NULL
baf$prod <- NULL
baf$ctr <- NULL
baf$nom <- NULL

baf$year <- yr(baf$year) #cleaning year data
baf$film <- tr(baf$film) #cleaning film data

baf1 = baf
  
# # This shoud give errors
# for (row in 2: length(baf$year)) {
#   if(baf$year[row] = NA) {
#     baf$year[row] = baf$year[row-1]
#   }
# }
# 
# # This should also give errors
# for (row in 2:length(baf$year)){ # 2 so you don't affect column names
#   if(baf$year[row] == "") {    # if its empty...
#     baf$year[row] = baf$year[row-1] # ...replace with previous row's value
#   }
# }


# Corrected
for (row in 1: length(baf$year)) {
  if(is.na(baf$year[row])) {
    baf$year[row] = baf$year[row-1]
  }
}


######OSCARS########

#adding a new win column and fixing column names
aa$aawin <- "NO"
colnames(aa) <- c("year","film","producer", "aawin")

#removing columns we don't need
aa$producer <- NULL

aa$year <- yr(aa$year) #cleaning year data
aa$film <- tr(aa$film) #cleaning film data

aa <- na.omit(aa) #remove all values with N/A in them 
rownames(aa) <- NULL  #rest the row numbers b/c they were screwed up

aatable <- as.data.frame(table(aa$year)) 
#table shows:
  #from 1990 to 2008 (19 years) there have been 5 nominees therefore = 95
aa$aawin[seq(1, 95 , 5)] <- "YES"

# 2009 & 2010 (2 years) had 10 nominees = 20 ; 95 + 20 = 115
aa$aawin[seq(95 + 1, 115 , 10)] <- "YES"

# 2011 - 2013 had 9 nominees (3 years) = 27 ; 115 + 27 = 142
aa$aawin[seq(115 + 1, 142 , 9)] <- "YES"

# 2014 - 2015 had 8 nominees again (2 years) = 16; 142 + 16 = 158
aa$aawin[seq(142 + 1, 158 , 8)] <- "YES"

# 2016 - 2017 had 9 nominees again (2 years) = 18 ; 158 + 18 = 176
aa$aawin[seq(158 + 1, 176 , 9)] <- "YES"

# 2018 had 8 nominees (1 year)  = 8
aa$aawin[seq(176+1, nrow(aa), 8)] <- "YES"
rm(aatable)


################# MERGING DATA FRAMES ##############################

mov <- join_all(list(dg,gg,sag,aa), by = "film", type = "full") # get everything in one data frame
mov <- mov[order(mov$year), ] # ordering all the movies by year
rownames(mov) <- NULL #removing garbage row names




####  Looking at the data set
str(mov)
summary(mov)
###

################### ML DIRTY WORK #################


mov <- data.frame(lapply(mov, factor, exclude = NULL)) #converting all data into factors for ease of use with 
#ML added exclude NULL since we need to add NAs as a factor as default is not included

sapply(mov, class) #to check if all factors
levels(mov$aawin) #to check if NAs a factor as well 

######## partition data - 80% is training, 20% is testing
validation_index <- createDataPartition(mov$aawin, p = 0.80, list = FALSE) #splitting the data into 80%
val <- mov[-validation_index, ] #creating the other 20%
mov <- mov[validation_index, ] #the new dataset we're gonna play iwth 

# Should call it test and train data sets.
?createDataPartition
# Can use sample function. 

###### visualizing data #####

###### summarize the class distribution #####
percentage <- prop.table(table(mov$aawin)) * 100
cbind(freq=table(mov$aawin), percentage=percentage)

######## creating testing ground #####

control <- trainControl(method = "cv", number =10) #10 fold cross validation to measure accuracy
metric <- "Accuracy"

################ BUILDING MODELS ############

# a) linear algorithms
set.seed(7)
fit.lda <- train(aawin~., data = mov, method = "lda", metric = metric, trControl = control)

# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(aawin~., data = mov, method = "rpart", metric = metric, trControl = control)

# kNN
set.seed(7)
fit.knn <- train(aawin~., data = mov, method = "knn", metric = metric, trControl = control)

# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(aawin~., data = mov, method = "svmRadial", metric = metric, trControl = control)

# Random Forest - NOT WORKING
#set.seed(7)
#fit.rf <- train(aawin~., data = mov, method = "rf", metric = metric, trControl = control)

###### How to find best model

# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm)) #, rf=fit.rf))

summary(results)


#### Looks like lda is the most accurate - Linear Discriminant Analysis it is!

predictions <- predict(fit.lda, val)
confusionMatrix(predictions, val$aawin, positive = "YES")



################################### DRAFT WORK ########################################

#PRODUCER'S GUILD IS WILDING OUT

pg <- f("https://en.wikipedia.org/wiki/Producers_Guild_of_America_Award_for_Best_Theatrical_Motion_Picture",
        '//*[@id="mw-content-text"]/div/table[3]',
        '//*[@id="mw-content-text"]/div/table[4]',
        '//*[@id="mw-content-text"]/div/table[5]')

#Independant Spirit Awards IS UNSTRUCTURED LIST

# url <- "https://en.wikipedia.org/wiki/Independent_Spirit_Award_for_Best_Film"

#Critics choice awards IS UNSTRUCTURED LIST # https://en.wikipedia.org/wiki/Critics%27_Choice_Movie_Award_for_Best_Picture



