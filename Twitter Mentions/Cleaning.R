# * Command Line * 

# Create a directory: > cd /Users/University/Tweet
# Split the file:     > split -b 800m /Users/University/Desktop/Tweet/tweets2009-07.txt
#                       tweet.txt

# Time to Create 5 dataframe not cleaned
start_time <- Sys.time()

# ----------------
# * Libraries * 
library(data.table)
library(dplyr)
library(reshape2)
library(tidyr)
library(igraph)
# ----------------
# * Cleaning* 
setwd("~/Desktop/Tweet")

# Import and look 
df = fread("tweet.txt", blank.lines.skip = T, header = F)
head(df,1) 
tail(df,3)
# Remove some rows to lighten the df:
df = df[1:(nrow(df) - 4300000),]
# Check the date of the last row (ok it ends  here 2009-07-06 01:05:04)
tail(df,3)

# Spread the dataframe 
ty = ifelse(df$V1 == 'T',T,F)
mydata_1 = df[ty]
ty_1 = ifelse(df$V1 =='U',T,F)
mydata_2 = df[ty_1]
ty_2 = ifelse(df$V1 =='W',T,F)
mydata_3 = df[ty_2]

# Column bind togheter and look 
df_largematrix = cbind(mydata_1$V2,mydata_2$V2,mydata_3$V2)
head(df_largematrix)

# Create Indexes each for one of the 5days to analyze 
index1 = grepl("2009-07-01",df_largematrix[,1])
index2 = grepl("2009-07-02",df_largematrix[,1])
index3 = grepl("2009-07-03",df_largematrix[,1])
index4 = grepl("2009-07-04",df_largematrix[,1])
index5 = grepl("2009-07-05",df_largematrix[,1])
# Create 5 Dataframes
df1 = as.data.frame(df_largematrix[index1,])[,2:3]
df2 = as.data.frame(df_largematrix[index2,])[,2:3]
df3 = as.data.frame(df_largematrix[index3,])[,2:3]
df4 = as.data.frame(df_largematrix[index4,])[,2:3]
df5 = as.data.frame(df_largematrix[index5,])[,2:3]

# Set columns names
names(df1) = c("User","Tweet")
names(df2) = c("User","Tweet")
names(df3) = c("User","Tweet")
names(df4) = c("User","Tweet")
names(df5) = c("User","Tweet")

# Take the username in the User column 
df1$User = gsub(".*com/","", df1$User)
df2$User = gsub(".*com/","", df2$User)
df3$User = gsub(".*com/","", df3$User)
df4$User = gsub(".*com/","", df4$User)
df5$User = gsub(".*com/","", df5$User)

# Using regmatches and gregexpr this gives you a list with hashtags per tweet,
# hastag is of format @ followed by any number of letters or digits 
list1 = regmatches(df1$Tweet,gregexpr("@(\\d|\\w)+",df1$Tweet))
df1$Tag <- sapply(list1, paste0, collapse=" ") 

list2 = regmatches(df2$Tweet,gregexpr("@(\\d|\\w)+",df2$Tweet))
df2$Tag <- sapply(list2, paste0, collapse=" ") 

list3 = regmatches(df3$Tweet,gregexpr("@(\\d|\\w)+",df3$Tweet))
df3$Tag <- sapply(list3, paste0, collapse=" ") 

list4 = regmatches(df4$Tweet,gregexpr("@(\\d|\\w)+",df4$Tweet))
df4$Tag <- sapply(list4, paste0, collapse=" ") 

list5 = regmatches(df5$Tweet,gregexpr("@(\\d|\\w)+",df5$Tweet))
df5$Tag <- sapply(list5, paste0, collapse=" ") 

# Be large then delete (the warning is ok)
library(tidyr)
df1 = separate(df1, Tag, into = as.character(seq(1:20)), sep = " ")
df2 = separate(df2, Tag, into = as.character(seq(1:20)), sep = " ")
df3 = separate(df3, Tag, into = as.character(seq(1:30)), sep = " ")
df4 = separate(df4, Tag, into = as.character(seq(1:20)), sep = " ")
df5 = separate(df5, Tag, into = as.character(seq(1:20)), sep = " ")

# Remove all columns that have NA's
df1 <- df1[,colSums(is.na(df1))  < nrow(df1)]
df2 <- df2[,colSums(is.na(df2))  < nrow(df2)]
df3 <- df3[,colSums(is.na(df3))  < nrow(df3)]
df4 <- df4[,colSums(is.na(df4))  < nrow(df4)]
df5 <- df5[,colSums(is.na(df5))  < nrow(df5)]
# Remove @
df1 = as.data.frame(lapply(df1, function(y) gsub("@", "", y)))
df2 = as.data.frame(lapply(df2, function(y) gsub("@", "", y)))
df3 = as.data.frame(lapply(df3, function(y) gsub("@", "", y)))
df4 = as.data.frame(lapply(df4, function(y) gsub("@", "", y)))
df5 = as.data.frame(lapply(df5, function(y) gsub("@", "", y)))

# Remove the whole comment
df1$Tweet = NULL
df2$Tweet = NULL
df3$Tweet = NULL
df4$Tweet = NULL
df5$Tweet = NULL

# Check structure
str(df1)
# Keep all rows with at least one tag 
df1 =  df1[!(df1$X1==""), ]
df2 =  df2[!(df2$X1==""), ]
df3 =  df3[!(df3$X1==""), ]
df4 =  df4[!(df4$X1==""), ]
df5 =  df5[!(df5$X1==""), ]

# Convert to character
library(dplyr)
df1 = df1 %>%
  mutate_all(as.character)

df2 = df2 %>%
  mutate_all(as.character)

df3 = df3 %>%
  mutate_all(as.character)

df4 = df4 %>%
  mutate_all(as.character)

df5 = df5 %>%
  mutate_all(as.character)

# --------------------
# Check with a subset 
bb_sub = df1[1:3,]; bb_sub
# From wide to large
long <- melt(bb_sub, id.vars = c("User")); long
long = na.omit(long); long
long = long[order(long$User), ][,c("User","value")]; long
# Calculate weigth
long = as.data.frame( long %>%
                     group_by(User, value) %>%
                     mutate(weight = n()) %>% 
                     distinct(.))
names(long) = c("from","to","weight");long

# Graph with 6vertex 3 edges -> it's ok!
bb_graph <- graph_from_data_frame(long, directed = T)
# -------------
# From wide to large
long1 <- melt(df1, id.vars = c("User"))
long2 <- melt(df2, id.vars = c("User"))
long3 <- melt(df3, id.vars = c("User"))
long4 <- melt(df4, id.vars = c("User"))
long5 <- melt(df5, id.vars = c("User"))

# Remove Na's
long1 = na.omit(long1)
long2 = na.omit(long2)
long3 = na.omit(long3)
long4 = na.omit(long4)
long5 = na.omit(long5)

# Sort by name
library(gtools)
long1 = long1[order(long1$User), ][,c("User","value")]
long2 = long2[order(long2$User), ][,c("User","value")]
long3 = long3[order(long3$User), ][,c("User","value")]
long4 = long4[order(long4$User), ][,c("User","value")]
long5 = long5[order(long5$User), ][,c("User","value")]

# Check if order is correct
head(long5$User)
tail(long5$User)

# Calculate weigth
long1 = as.data.frame( long1 %>%
                     group_by(User, value) %>%
                     mutate(weight = n()) %>% 
                     distinct(.))

long2 = as.data.frame( long2 %>%
                     group_by(User, value) %>%
                     mutate(weight = n()) %>% 
                     distinct(.))

long3 = as.data.frame( long3 %>%
                     group_by(User, value) %>%
                     mutate(weight = n()) %>% 
                     distinct(.))

long4 = as.data.frame(long4 %>%
                     group_by(User, value) %>%
                     mutate(weight = n()) %>% 
                     distinct(.))

long5 = as.data.frame(long5 %>%
                     group_by(User, value) %>%
                     mutate(weight = n()) %>% 
                     distinct(.))

# Set proper name columns
names(long1) = c("from","to","weight")
names(long2) = c("from","to","weight")
names(long3) = c("from","to","weight")
names(long4) = c("from","to","weight")
names(long5) = c("from","to","weight")

# Check max weight as an example
max(long1$weight); max(long2$weight); max(long3$weight); max(long4$weight); max(long5$weight)

# Privacy 
# users1 <- sprintf("user%d",seq(1:nrow(long1)))

# Create csv files 
csv1 = write.csv2(long1, 'July_1.csv',row.names = FALSE)
csv2 = write.csv2(long2, 'July_2.csv', row.names = FALSE)
csv3 = write.csv2(long3, 'July_3.csv', row.names = FALSE)
csv4 = write.csv2(long4, 'July_4.csv', row.names = FALSE)
csv5 = write.csv2(long5, 'July_5.csv', row.names = FALSE)

# End time
end_time <- Sys.time()
# Differences
start_time - end_time
