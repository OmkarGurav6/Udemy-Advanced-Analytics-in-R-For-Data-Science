

setwd("C:/Users/DELL/Desktop/Udemy R/Advanced Analytics in R/Section 2 Data preparation")

fin <- read.csv("P3-Future-500-The-Dataset.csv",na.strings = c(""))
                                                #converting "" into NA  

fin$Name <- factor(fin$Name)
fin$Industry <- factor(fin$Industry)
fin$State <- factor(fin$State)
fin$City <- factor(fin$City)
fin$Growth <- factor(fin$Growth)
fin$Revenue <- factor(fin$Revenue)
fin$Expenses <- factor(fin$Expenses)

fin$ID <- factor(fin$ID)
fin$Inception <- factor(fin$Inception)

#fin$Profit <- factor(fin$Profit)
# Wrong way to convert factors into non factors
#fin$Profit <- as.numeric(as.character())

head(fin)
#using sub() and gsub()
#converts factor to char
fin$Expenses <- gsub(" Dollars","",fin$Expenses)
fin$Expenses <- gsub(",","",fin$Expenses) 
fin$Revenue <- gsub("\\$","",fin$Revenue) #\\ is used to create escape sequence
fin$Revenue <- gsub(",","",fin$Revenue)
fin$Growth <- gsub("%","",fin$Growth)
head(fin)

fin$Expenses <- as.numeric(fin$Expenses)
fin$Revenue <- as.numeric(fin$Revenue)
fin$Growth <- as.numeric(fin$Growth)

#incomplete cases having NA
fin[!complete.cases(fin),]

#filtering dataframe using which() to get non missing values
fin[which(fin$Employees == 45),]


is.na(fin$Industry)
fin[is.na(fin$Industry),]

fin_backup <- fin

#removing rows with NA in Industry column
fin[!complete.cases(fin),]
fin <- fin[!is.na(fin$Industry),]

fin
#resetting the dataframe index method 1
rownames(fin) <- 1:nrow(fin)

#method 2 
rownames(fin) <- NULL
fin

#replacing missing data : Factual Analysis
fin[!complete.cases(fin),]

fin[is.na(fin$State) & fin$City == "New York","State"] <- "NY"
fin[c(11,377),]

fin[is.na(fin$State) & fin$City == "San Francisco","State"] <- "CA"

fin[c(82,265),]

#replacing missing data : median imputation method

fin[!complete.cases(fin),]

med_empl_retail <- median(fin[fin$Industry == "Retail","Employees"],na.rm = T)

fin[is.na(fin$Employees) & fin$Industry == "Retail","Employees"] <- med_empl_retail 
fin[3,]
  
med_empl_fina <- median(fin[fin$Industry == "Financial Services","Employees"], na.rm = T)

fin[is.na(fin$Employees) & fin$Industry == "Financial Services","Employees" ] <- med_empl_fina

fin[330,]

med_growth_constr <- median(fin[fin$Industry == "Construction","Growth"], na.rm = T)

fin[is.na(fin$Growth) & fin$Industry == "Construction","Growth" ] <- med_growth_constr

fin[8,]

fin[!complete.cases(fin),]

med_reve_cons <- median(fin[fin$Industry == "Construction","Revenue"], na.rm = T)

fin[is.na(fin$Revenue) & fin$Industry == "Construction","Revenue" ] <- med_reve_cons

fin[c(8,42),]

med_exp_cons <- median(fin[fin$Industry == "Construction","Expenses"], na.rm = T)

fin[is.na(fin$Expenses & fin$Profit) & fin$Industry == "Construction","Expenses" ] <- med_exp_cons

fin[c(8,42),]

#replacing missing data : deriving values method

fin[is.na(fin$Profit),"Profit"] <- fin[is.na(fin$Profit),"Revenue"] - fin[is.na(fin$Profit),"Expenses"]

fin[c(8,42),]

fin[is.na(fin$Expenses),"Expenses"] <- fin[is.na(fin$Expenses),"Revenue"] - fin[is.na(fin$Expenses),"Profit"]

fin[15,]

#visualization

# A scatterplot classified by industry showing revenue, expenses, profit

library(ggplot2)

p <- ggplot(data = fin)

p + geom_point(aes(x = Revenue, y = Expenses,
                   colour = Industry, size = Profit))


# A scatterplot that includes industry trends for the expenses~revenue relationship

d <- ggplot(data = fin,aes(x = Revenue, y = Expenses,
                           colour = Industry))
d
d + geom_point() + geom_smooth(fill = NA,size = 1.5)

# BoxPlots showing growth by industry

e <- ggplot(data = fin,aes(x = Industry, y = Growth,
                           colour = Industry))

e + geom_jitter() + geom_boxplot(outlier.colour = NA,
                                 size = 1,alpha = 0.6)

