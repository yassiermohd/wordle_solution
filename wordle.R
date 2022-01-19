library(httr)
library(jsonlite)
library(readr)
library(stringr)
library(dplyr)
library(tidyverse)

setwd("C:/Users/yassier/OneDrive - The SEACEN Centre/Desktop")

#Setup Dictionary


OxfordDict <- read_csv("OxfordDict.txt", col_names = FALSE)
OxfordDict$X2 <- word(OxfordDict$X1,1)
OxfordDict <- OxfordDict %>% select(-1) 
OxfordDict$X2 <- toupper(OxfordDict$X2)
OxfordDict <- OxfordDict  %>% filter(nchar(X2)==5) 

i=1
for (x in OxfordDict$X2){
  OxfordDict$X2[i] <- rawToChar(unique(charToRaw(OxfordDict$X2[i])))
  i=i+1
}

OxfordDict <- OxfordDict  %>% filter(nchar(X2)==5) 

OxfordDict <- as.data.frame(OxfordDict)
head(OxfordDict)


###### get estimation starting point

estimation_work <- data.frame(letter = LETTERS)
estimation_work
solution <- unique(OxfordDict)
allword<- paste(solution$X2, collapse="")

i=1
for (x in LETTERS){
  estimation_work$freq[i] <- sum(unlist(strsplit(allword,""))==LETTERS[i])
  i=i+1
}

estimation_work


### assign power

estimation_solution <- unique(solution)
estimation_solution

estimation_solution$power = 0 

  


i=1


estimation_solution$X2[1]


for (x in estimation_solution$X2){
  estimation_split <- strsplit(estimation_solution$X2[i], "")[[1]]
  estimation_solution$power[i] = 0 
  
  j=1
  for (y in estimation_split) {
    
    temp <- estimation_work %>% filter(grepl(estimation_split[j], letter)) 
    estimation_solution$power[i] <- estimation_solution$power[i]+sum(temp$freq)
    
    j=j+1
    
  }
  
  i=i+1
}



estimation_solution



estimation_solution <- estimation_solution %>% filter(!grepl("\\W", X2))


estimation_solution <- estimation_solution %>% arrange(desc(power)) %>% slice(1)

estimation_solution$X2





#######   1st round


#Read

input_read <- estimation_solution$X2




input_score <- readline("Enter your score")

#Analyze

guess_work <- data.frame(letter = unlist(strsplit(input_read, split = "")), score= unlist(strsplit(input_score, split = "")))
guess_work

#Get a list of letter that is correct & need to be rearrange

#Create compulsory 

guess_work$regex <- "\\w"

guess_work

i=1
for (x in guess_work$score){
  if (guess_work$score[i] == "2"){
    guess_work$regex[i] <- paste("[",guess_work$letter[i], "]", sep="" )
  }
  
  i=i+1
}

guess_work

compulsory <- paste(guess_work$regex, collapse="")
compulsory

# Create remove

remove <- guess_work %>% filter(score=="0") %>%  select(-2)
remove <- as.list(remove$letter) 
remove <- unlist(remove, recursive = FALSE)
remove

#create rearrange

rearrange <- guess_work %>% filter(score=="1") %>%  select(-2)
rearrange <- as.list(rearrange$letter) 
rearrange <- unlist(rearrange, recursive = FALSE)
rearrange


# Filter database

#rearrange


i=1

for (x in rearrange){
  solution <- solution %>% filter(grepl(rearrange[i], X2))
  print('hello')
  i=i+1
}

solution


#remove

i=1

for (x in remove){
  solution <- solution %>% filter(!grepl(remove[i], X2))
  print('hello')
  i=i+1
}

solution

#compulsory

solution <- solution %>% filter(grepl(compulsory, X2))

solution



#######################



# STEP 2

### Estimation

###### get estimation starting point

estimation_work <- data.frame(letter = LETTERS)
estimation_work

allword<- paste(solution$X2, collapse="")

i=1
for (x in LETTERS){
  estimation_work$freq[i] <- sum(unlist(strsplit(allword,""))==LETTERS[i])
  i=i+1
}

estimation_work


### assign power

estimation_solution <- unique(solution)
estimation_solution

estimation_solution$power = 0 




i=1


estimation_solution$X2[1]


for (x in estimation_solution$X2){
  estimation_split <- strsplit(estimation_solution$X2[i], "")[[1]]
  estimation_solution$power[i] = 0 
  
  j=1
  for (y in estimation_split) {
    
    temp <- estimation_work %>% filter(grepl(estimation_split[j], letter)) 
    estimation_solution$power[i] <- estimation_solution$power[i]+sum(temp$freq)
    
    j=j+1
    
  }
  
  i=i+1
}



estimation_solution



estimation_solution <- estimation_solution %>% filter(!grepl("\\W", X2))


estimation_solution <- estimation_solution %>% arrange(desc(power)) %>% slice(1)

estimation_solution$X2




#Read

input_read <- estimation_solution$X2



input_score <- readline("Enter your score")

#Analyze

guess_work <- data.frame(letter = unlist(strsplit(input_read, split = "")), score= unlist(strsplit(input_score, split = "")))
guess_work

#Get a list of letter that is correct & need to be rearrange

#Create compulsory 

guess_work$regex <- "\\w"

guess_work

i=1
for (x in guess_work$score){
  if (guess_work$score[i] == "2"){
    guess_work$regex[i] <- paste("[",guess_work$letter[i], "]", sep="" )
  }
  
  i=i+1
}

guess_work

compulsory <- paste(guess_work$regex, collapse="")
compulsory

# Create remove

remove <- guess_work %>% filter(score=="0") %>%  select(-2)
remove <- as.list(remove$letter) 
remove <- unlist(remove, recursive = FALSE)
remove

#create rearrange

rearrange <- guess_work %>% filter(score=="1") %>%  select(-2)
rearrange <- as.list(rearrange$letter) 
rearrange <- unlist(rearrange, recursive = FALSE)
rearrange


# Filter database

#rearrange


i=1

for (x in rearrange){
  solution <- solution %>% filter(grepl(rearrange[i], X2))
  print('hello')
  i=i+1
}

solution


#remove

i=1

for (x in remove){
  solution <- solution %>% filter(!grepl(remove[i], X2))
  print('hello')
  i=i+1
}

solution

#compulsory

solution <- solution %>% filter(grepl(compulsory, X2))

solution



# STEP 3

## Estimation

estimation_work <- data.frame(letter = LETTERS)
estimation_work

allword<- paste(solution$X2, collapse="")

i=1
for (x in LETTERS){
  estimation_work$freq[i] <- sum(unlist(strsplit(allword,""))==LETTERS[i])
  i=i+1
}

estimation_work


### assign power

estimation_solution <- unique(solution)
estimation_solution

estimation_solution$power = 0 




i=1


estimation_solution$X2[1]


for (x in estimation_solution$X2){
  estimation_split <- strsplit(estimation_solution$X2[i], "")[[1]]
  estimation_solution$power[i] = 0 
  
  j=1
  for (y in estimation_split) {
    
    temp <- estimation_work %>% filter(grepl(estimation_split[j], letter)) 
    estimation_solution$power[i] <- estimation_solution$power[i]+sum(temp$freq)
    
    j=j+1
    
  }
  
  i=i+1
}



estimation_solution



estimation_solution <- estimation_solution %>% filter(!grepl("\\W", X2))


estimation_solution <- estimation_solution %>% arrange(desc(power)) %>% slice(1)

estimation_solution$X2



#Read

input_read <- estimation_solution$X2




input_score <- readline("Enter your score")

#Analyze

guess_work <- data.frame(letter = unlist(strsplit(input_read, split = "")), score= unlist(strsplit(input_score, split = "")))
guess_work

#Get a list of letter that is correct & need to be rearrange

#Create compulsory 

guess_work$regex <- "\\w"

guess_work

i=1
for (x in guess_work$score){
  if (guess_work$score[i] == "2"){
    guess_work$regex[i] <- paste("[",guess_work$letter[i], "]", sep="" )
  }
  
  i=i+1
}

guess_work

compulsory <- paste(guess_work$regex, collapse="")
compulsory

# Create remove

remove <- guess_work %>% filter(score=="0") %>%  select(-2)
remove <- as.list(remove$letter) 
remove <- unlist(remove, recursive = FALSE)
remove

#create rearrange

rearrange <- guess_work %>% filter(score=="1") %>%  select(-2)
rearrange <- as.list(rearrange$letter) 
rearrange <- unlist(rearrange, recursive = FALSE)
rearrange


# Filter database

#rearrange


i=1

for (x in rearrange){
  solution <- solution %>% filter(grepl(rearrange[i], X2))
  print('hello')
  i=i+1
}

solution


#remove

i=1

for (x in remove){
  solution <- solution %>% filter(!grepl(remove[i], X2))
  print('hello')
  i=i+1
}

solution

#compulsory

solution <- solution %>% filter(grepl(compulsory, X2))

solution