
setwd("C:/Users/DELL/Desktop/Udemy R/Advanced Analytics in R/Section 3 List")

util <- read.csv("P3-Machine-Utilization.csv")

util$Timestamp <- factor(util$Timestamp)
util$Machine <- factor(util$Machine)

#Adding new column
util$Utilization <- 1 - util$Percent.Idle

#Handling Date-Times
util$PosixTime <- as.POSIXct(util$Timestamp, format="%d/%m/%Y %H:%M")

#Rearranging columns
util$Timestamp <- NULL
util <- util[,c(4,1,2,3)]

RL1 <- util[util$Machine == "RL1",]

RL1$Machine <- factor(RL1$Machine)

#Character: Machine name
#Vector: (min, mean, max) utilisation for the month (excluding unknown hours)
#Logical: Has utilisation ever fallen below 90%? TRUE / FALSE

util_stats_rl1 <- c(min(RL1$Utilization, na.rm = T),
                    mean(RL1$Utilization, na.rm = T),
                    max(RL1$Utilization, na.rm = T))

util_less_than_90 <-  length(which(RL1$Utilization < 0.90)) > 0

list_rl1 <- list("RL1",util_stats_rl1,util_less_than_90)

# naming list components
names(list_rl1) <- c("Machine","Stats","Low.Threshold")

# Vector :All hours where utilisation is unknown (NAs)
 list_rl1$UnknownHours <- RL1[is.na(RL1$Utilization),"PosixTime"]

#Dataframe: For this machine
 list_rl1$Data <- RL1
 
#Plot: For all machines
 library(ggplot2)
 
 f <- ggplot(data = util)
 myplot <- f + geom_line(aes(x = PosixTime,y = Utilization,
                   colour = Machine),size = 1.2) +
 facet_grid(Machine~.) + geom_hline(yintercept = 0.90,
                                    colour = "Gray",size = 1.2,
                                    linetype = 4)

 list_rl1$Plot <- myplot   






