
setwd("C:/Users/DELL/Desktop/Udemy R/Advanced Analytics in R/Section 4 Apply functions")

Chicago <- read.csv("Chicago-F.csv",row.names = 1)
NewYork <- read.csv("NewYork-F.csv",row.names = 1)
Houston <- read.csv("Houston-F.csv",row.names = 1)
SanFrancisco <- read.csv("SanFrancisco-F.csv",row.names = 1)

Chicago <- as.matrix(Chicago)
NewYork <- as.matrix(NewYork)
Houston <- as.matrix(Houston)
SanFrancisco <- as.matrix(SanFrancisco)

Weather <- list(Chicago=Chicago,NewYork=NewYork,Houston=Houston,SanFrancisco=SanFrancisco)

# using apply()
#1.A table showing the annual averages of each observed metric for every city

apply(Chicago,1,mean)
apply(NewYork,1,mean)
apply(Houston,1,mean)
apply(SanFrancisco,1,mean)


#Recreating apply function with loops
output <- NULL

for(i in 1:5)
{
  output[i] <- mean(Chicago[i,])
}

names(output) <- rownames(Chicago)

# Using lapply()
#1.A table showing the annual averages of each observed metric for every city
lapply(Weather,rowMeans)


#Combining lapply with the [] operator
lapply(Weather,"[",1,)

lapply(Weather,"[", ,3)

#2. A table showing by how much temperature fluctuates each month from min to
#max (in %). Take min temperature as the base

sapply(Weather,function(x) round((x[1,] - x[2,])/x[2,],2))

sapply(Weather,"[",1,7)

#1.A table showing the annual averages of each observed metric for every city
round(sapply(Weather,rowMeans), 2)

#3. A table showing the annual maximums of each observed metric for every city
sapply(Weather,apply,1,max )

#4. A table showing the annual minimums of each observed metric for every city
sapply(Weather,apply,1,min )

#5. A table showing in which months the annual maximums of each metric were
#observed in every city (Advanced)

sapply(Weather,function(y)  apply(y ,1 , function(x) names(which.max(x))))



