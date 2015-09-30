setwd("/Users/YuchaoGuo/Desktop/PS3")#Set working directory

#Question 2

#(a)
library(XML)
library(stringr)

#This is the funtion to select the first debate
Debate_first<-function(year){
  debateMain<-htmlParse("http://www.debates.org/index.php?page=debate-transcripts")
  debate.transcript<-getNodeSet(debateMain,"//a[@href]")
  matchpoint<-sapply(debate.transcript,xmlValue)
  url<-sapply(debate.transcript, xmlGetAttr, "href")[grep(paste(year,'.*','The First',sep=""),matchpoint)]
  return(url)
}
url2012 <- Debate_first(2012)


#This is the function to select all debate in a certain year(Most 3 debates, here we select all three debates)
Debate_all<-function(year){
  debateMain<-htmlParse("http://www.debates.org/index.php?page=debate-transcripts")
  debate.transcript<-getNodeSet(debateMain,"//a[@href]")
  matchpoint<-sapply(debate.transcript,xmlValue)
  url1<-sapply(debate.transcript, xmlGetAttr, "href")[grep(paste(year,'.*','The First',sep=""),matchpoint)]
  url2<-sapply(debate.transcript, xmlGetAttr, "href")[grep(paste(year,'.*','The Second',sep=""),matchpoint)]
  url3<-sapply(debate.transcript, xmlGetAttr, "href")[grep(paste(year,'.*','The Third',sep=""),matchpoint)]
  url <- as.list(rbind(url1,url2,url3))
  return(url)
}


#######################(b)################
##########################################

#This is the function to extract the body
extract_body <- function(year){
  url <- Debate_first(year)
  doc <- htmlParse(url)
  text <- getNodeSet(doc, "//p/text()")
  list <- unclass(text)
  for (i in 1:length(list)){
    list[[i]] <- toString.XMLNode(list[i])
    list[[i]] <- str_replace_all(list[[i]], "\\[\\[1\\]\\]","") #remove [[1]] in the text
    list[[i]] <- str_replace_all(list[[i]],"\n","") #remove \n
    list[[i]] <- str_replace_all(list[[i]],"\\\"","") #remove\
  }
  list[1] <- NULL
  return(list)
}

Debate_1996 <- extract_body(1996)
Debate_2000 <- extract_body(2000)
Debate_2004 <- extract_body(2004)
Debate_2008 <- extract_body(2008)
Debate_2012 <- extract_body(2012)  


##########################(c)##########
##############Put everything in a chunk

#Function to make chunks
Debate_in_chunks<-function(year){
Debate_list<- extract_body(year)
mylist <- list()#Declare an empty list for future use
chunk_vector <- grep("^[A-Z]*:",Debate_list) #Find the indice for capital letters(Names)
chunk_vector <- c(chunk_vector, (length(Debate_list)+1))
for(i in 1:(length(chunk_vector)-1)){
  mylist[[i]] <- paste(unlist(Debate_list[chunk_vector[i]:(chunk_vector[i+1]-1)]), collapse = "")
  #### names(mylist[[i]]) <- unlist(str_split(mylist[[i]][1], ":"))[1]
}
mylist <- unlist(mylist)
return(mylist)
}

Debate_in_chunks_1996 <- Debate_in_chunks(1996)
Debate_in_chunks_2000 <- Debate_in_chunks(2000)
Debate_in_chunks_2004 <- Debate_in_chunks(2004)
Debate_in_chunks_2008 <- Debate_in_chunks(2008)
Debate_in_chunks_2012 <- Debate_in_chunks(2012)
#This function would tell us how many laughters and how many applauses
COUNTER <- function(year){
  num_APP<-sum(str_count(unlist(Debate_in_chunks(year)), "\\(APPLAUSE\\)"))
  num_app<-sum(str_count(unlist(Debate_in_chunks(year)), "\\(Applause\\)"))
  num_LAU<-sum(str_count(unlist(Debate_in_chunks(year)),"\\(LAUGHTER\\)"))
  print(paste("There are/is",num_APP+num_app,"APPLAUSES in",year,"Debate", collapse = " "))
  print(paste("There are/is",num_LAU,"LAUGHTERS in",year,"Debate", collapse = " "))
}
COUNTER(1996)
COUNTER(2000)
COUNTER(2004)
COUNTER(2008)
COUNTER(2012)

#This funciton would clean all applause and laughters
#We can use str_replace to replace all applause and laughters
Clean<-function(year){
  cleanchunk <- str_replace(Debate_in_chunks(year), "\\(APPLAUSE\\)","")
  cleanchunk <- str_replace(Debate_in_chunks(year), "\\(Applause\\)","")
  cleanchunk <- str_replace(Debate_in_chunks(year), "\\(LAUGHTER\\)","")
  return (cleanchunk)
}

Clean_chunk_1996 <- Clean(1996)
Clean_chunk_2000 <- Clean(2000)
Clean_chunk_2004 <- Clean(2004)
Clean_chunk_2008 <- Clean(2008)
Clean_chunk_2012 <- Clean(2012)

head(Clean_chunk_2004) #illustrate


###################(d)###########
#################################
word_split <- function(year) {
  vector <- str_split(Clean(year)," ")
  vector <- unlist(vector)
  return(vector)
}

word_1996 <- word_split(1996) # a vector, each word an element for 1996
word_2000 <- word_split(2000) # a vector, each word an element for 2000
word_2004 <- word_split(2004) # a vector, each word an element for 2004
word_2008 <- word_split(2008) # a vector, each word an element for 2008
word_2012 <- word_split(2012) # a vector, each word an element for 2012

head(word_2000) #illustrate

#This function would combine all the word in one sentence.
sentence_builder<-function(year){
  words <- word_split(year)
  vector1 <- grep((".*[a-z][a-z]\\."), words)
  vector2 <- grep((".*[a-z][a-z]\\?"), words)
  vector3 <- grep((".*[a-z][a-z]\\!"), words)
  index <- unique(sort(c(vector1,vector2,vector3)))
  sentence <- c()
  sentence[1] <- paste(words[1:index[1]], collapse = " ")
  for (i in 2:length(index)-1){
    sentence[i] <- paste(words[(index[i]+1):index[i+1]], collapse = " ")
  }
  return(sentence)
}

sentence_1996 <- sentence_builder(1996)
sentence_2000 <- sentence_builder(2000)
sentence_2004 <- sentence_builder(2004)
sentence_2008 <- sentence_builder(2008)
sentence_2012 <- sentence_builder(2012)

head(sentence_2004) #illustrate


######(e)######
###############

#First I would declare an data.frame to store all the information
info <- as.data.frame(matrix(NA, nrow = 10, ncol =3), row.names = c("1996 CLINTON","1996 DOLE", "2000 BUSH", 
                                                                    "2000 GORE", "2004 BUSH", "2004 KERRY",
                                                                    "2008 OBAMA","2008 MCCAIN", "2012 OBAMA",
                                                                    "2012 ROMNEY"))
colnames(info)<-(c("number of words","number of characters","average word length"))

Num_words <- function(name, year){
  chunks <- Clean(year)
  index <- grep(paste0("^",name,collapse = ""), chunks)
  chunk_sub <- unlist(chunks[index])
  chunk_sub <- str_split(chunk_sub, " ")
  num <- length(unlist(chunk_sub))
}

CLINTON_1996_nw <- Num_words("CLINTON", 1996)
DOLE_1996_nw <- Num_words("DOLE", 1996)
BUSH_2000_nw <- Num_words("BUSH", 2000)
GORE_2000_nw <- Num_words("GORE", 2000)
BUSH_2004_nw <- Num_words("BUSH", 2004)
KERRY_2004_nw <- Num_words("KERRY", 2004)
OBAMA_2008_nw <- Num_words("OBAMA", 2008)
MCCAIN_2008_nw <- Num_words("MCCAIN", 2008)
OBAMA_2012_nw <- Num_words("OBAMA", 2012)
ROMNEY_2012_nw <- Num_words("ROMNEY", 2012)

number_of_words <- c(CLINTON_1996_nw, DOLE_1996_nw, BUSH_2000_nw, GORE_2000_nw, BUSH_2004_nw, KERRY_2004_nw,
        OBAMA_2008_nw, MCCAIN_2008_nw, OBAMA_2012_nw, ROMNEY_2012_nw)

Num_chara <- function(name, year){
  chunks <- Clean(year)
  index <- grep(name, chunks)
  chunk_sub <- unlist(chunks[index])
  num <- sum(str_count(unlist(chunk_sub)))
}

CLINTON_1996_nc <- Num_chara("CLINTON", 1996)
DOLE_1996_nc <- Num_chara("DOLE", 1996)
BUSH_2000_nc <- Num_chara("BUSH", 2000)
GORE_2000_nc <- Num_chara("GORE", 2000)
BUSH_2004_nc <- Num_chara("BUSH", 2004)
KERRY_2004_nc <- Num_chara("KERRY", 2004)
OBAMA_2008_nc <- Num_chara("OBAMA", 2008)
MCCAIN_2008_nc <- Num_chara("MCCAIN", 2008)
OBAMA_2012_nc <- Num_chara("OBAMA", 2012)
ROMNEY_2012_nc <- Num_chara("ROMNEY", 2012)

number_of_characters <- c(CLINTON_1996_nc, DOLE_1996_nc, BUSH_2000_nc, GORE_2000_nc, BUSH_2004_nc,KERRY_2004_nc,
                          OBAMA_2008_nc, MCCAIN_2008_nc, OBAMA_2012_nc, ROMNEY_2012_nc)

CLINTON_1996_av <- CLINTON_1996_nc/CLINTON_1996_nw
DOLE_1996_av <- DOLE_1996_nc/DOLE_1996_nw
BUSH_2000_av <- BUSH_2000_nc/BUSH_2000_nw
GORE_2000_av <- GORE_2000_nc/GORE_2000_nw
BUSH_2004_av <- BUSH_2004_nc/BUSH_2004_nw
KERRY_2004_av <- KERRY_2004_nc/KERRY_2004_nw
OBAMA_2008_av <- OBAMA_2008_nc/OBAMA_2008_nw
MCCAIN_2008_av <- MCCAIN_2008_nc/MCCAIN_2008_nw
OBAMA_2012_av <- OBAMA_2012_nc/OBAMA_2012_nw
ROMNEY_2012_av <- ROMNEY_2012_nc/ROMNEY_2012_nw

number_of_average <- c(CLINTON_1996_av, DOLE_1996_av, BUSH_2000_av, GORE_2000_av, BUSH_2004_av, KERRY_2004_av,
                       OBAMA_2008_av, MCCAIN_2008_av, OBAMA_2012_av, ROMNEY_2012_av)

info[,1] <- number_of_words
info[,2] <- number_of_characters
info[,3] <- number_of_average

print(info)
####1.From the result, we can see that in 2008, the number of words are much more than other years, 
####however, by inspect the web page, we can see that in 2008 the transcript was recoded twice for some reason
####
####2.Obama, Kerry, Mccain havae longer average word leghth compared with other candidates.
####3.Bush seems to have the shortest average word length.

####(f)#####

#This is the function to translate name into year
name_to_year <- function(name){
  translator <- c("CLINTON 1996"=1996,"DOLE 1996"=1996, "BUSH 2000"=2000, "GORE 2000"=2000, "BUSH 2004"=2004, "KERRY 2004"=2004, 
                  "OBAMA 2008"=2008, "MCCAIN 2008"=2008,"OBAMA 2012"=2012, "ROMNEY 2012"=2012)
  year <- translator[name]
  return(year)
}

#Here, I declare a new dataframe to store information
name_vector <- c("CLINTON 1996","DOLE 1996", "BUSH 2000", "GORE 2000", "BUSH 2004", "KERRY 2004", 
                 "OBAMA 2008", "MCCAIN 2008","OBAMA 2012", "ROMNEY 2012")
exp_vector <- c("I ", "We "," we ","America(n){0,1}"," democra(cy|tic){0,1} "," republic ","Democrat(ic){0,1}",
                "Republican ", " free(dom){0,1} "," war ","God"," God bless ","(Jesus|Christ|Christian)")
count_table <- as.data.frame(matrix(NA, nrow = length(name_vector), ncol = length(exp_vector)))
rownames(count_table) <- name_vector
colnames(count_table) <-exp_vector

fill_in_table<- function(){
  for(i in 1:length(name_vector)){
    year <- as.numeric(name_to_year(name_vector[i]))
    chunks <- Clean(year)
    index <- grep(unlist(str_split(name_vector[i]," "))[1], chunks)
    chunk_sub <- chunks[index]
    for (j in 1:length(exp_vector)){
      num <- sum(str_count(chunk_sub, exp_vector[j]))
      count_table[i,j] <- num
    }
  }
  return(count_table)
}

count_table <- fill_in_table()




#Question 3
###(a)
#in part a, I have already used vectorization method

myWalk <- function(n, final){
  if (n < 0){
    print("Can't take negative move")
  }else if(n == 0){
    print("Can't take 0 steps")
  }else if(!is.integer(n)){
    print("Must take an integer value, remember to add an L after the number to tell r this is an integer")
  }else if(!is.logical(final)){
    print("final must be TRUE or FALSE")
  }else{
    path <- matrix(0, ncol = 2, nrow = n)
    # generate the indices to set the deltas
    indx <- cbind(seq(n), sample(c(1, 2), n, TRUE))
    # now set the values best on the random index we got
    path[indx] <- sample(c(-1, 1), n, TRUE)
    rwdetail <- matrix(0, ncol=2, nrow = n)
    rwdetail[,1] <- cumsum(path[,1])
    rwdetail[,2] <- cumsum(path[,2])
  #return the value, if final is TRUE, return the final position, otherwise return a matrix containing all the steps
  if(final){
    finalposition <- c(sum(path[,1]),sum(path[,2]))
      return(finalposition)
    }else{
      return (rwdetail)
    }
  }
}
RandomWalk <- myWalk(100,FALSE)
RandomWalk2 <- myWalk(-12,FALSE)

#######(c)
#This is the constructor function
myWalk_constructor <- function(n){
    if (n < 0){
      print("Can't take negative move")
    }else if(n == 0){
      print("Can't take 0 steps")
    }else if(!is.integer(n)){
      print("Must take an integer value, remember to add an L after the number to tell r this is an integer")
    }else{
    rw <- matrix(0, ncol = 2, nrow = n)
    # generate the indices to set the deltas
    indx <- cbind(seq(n), sample(c(1, 2), n, TRUE))
    # now set the values best on the random index we got
    rw[indx] <- sample(c(-1, 1), n, TRUE)
    rwdetail <- matrix(0, ncol=2, nrow = n)
    rwdetail[,1] <- cumsum(rw[,1])
    rwdetail[,2] <- cumsum(rw[,2])
    my_list <- list(rwpath = rwdetail, endpoint=c(sum(rw[,1]),sum(rw[,2])))
    class(my_list) <- 'rw'
    return(my_list) # This my_list contains both the path and ending point
   }
}

#print method for class rw
print <- function(rwdetail, ...){
  UseMethod("print") 
}
####if option = FALSE, it show the whole path, otherwise, it shows the end point
print.rw <- function(rwdetail,option=FALSE){
  if (!is.logical(option)){
    print("Option can only be TRUE or FALSE")
  }else{
    if(!option){
      my_matrix <- rwdetail[[1]]
      colnames(my_matrix) <- c("x","y")
      print(my_matrix)
      print("This is a matrix for position after each move")
    }else{
      print(c(rwdetail[[2]]))
      print("This is the ending point")
    }
    }
}

random <- myWalk_constructor(20L)
print(random)
print(random, TRUE)

#plot method
plot <- function(rwdetail, ...){
  UseMethod('plot')
}
plot.rw <- function(rwdetail){
  matrix_path <- rwdetail[[1]]
  plength=length(matrix_path[,1])
  plot(matrix_path[,1],matrix_path[,2],xlab='x', ylab='y',type='n',
       main="2D random walk")
  arrows(matrix_path[1:(plength-1),1],matrix_path[1:(plength-1),2],
         matrix_path[2:(plength),1],matrix_path[2:(plength),2],
         col = "green",length=0.05)
}

plot(random)

######[ operator
`[.rw` <- function(rwdetail, ...) UseMethod('[.rw')
`[.rw` <- function(rwdetail,n){
  matrix_path <- rwdetail[[1]]
  print(paste0("the ",n,"th step is at"))
  print(unique(matrix_path[n,]))
}
random[5]

#####replace
`origin<-` <- function(rwdetial, ...) UseMethod("origin<-") 
`origin<-.rw` <- function(rwdetail, value){
  matrix_path <- rwdetail[[1]]
  matrix_path[,1]=matrix_path[,1]+value[1]
  matrix_path[,2]=matrix_path[,2]+value[2]
  print("here is the path matrix for your random walk after changing the origin point")
  print(matrix_path)
}

origin(random)<-c(1,2)


