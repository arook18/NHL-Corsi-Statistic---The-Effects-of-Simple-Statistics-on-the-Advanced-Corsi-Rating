library(readxl)
Ind_Capstone_Data <- read_excel("Desktop/LIU/Graduate Fall/MDA 620 - Data-Driven Decision-Making/Ind Capstone Data.xlsx")
View(Ind_Capstone_Data)
glimpse(Ind_Capstone_Data)
Ind_Capstone_Data$

#Finding mean, standard deviation, min, and max
mean(Ind_Capstone_Data$CF)
mean(Ind_Capstone_Data$CA)
mean(Ind_Capstone_Data$CFP)
mean(Ind_Capstone_Data$oZSP)
mean(Ind_Capstone_Data$dZSP)
mean(Ind_Capstone_Data$FOW)
mean(Ind_Capstone_Data$FOL)
mean(Ind_Capstone_Data$FOP)
mean(Ind_Capstone_Data$HIT)
mean(Ind_Capstone_Data$BLK)

#Finding mean, standard deviation, min, and max
sd(Ind_Capstone_Data$CF)
sd(Ind_Capstone_Data$CA)
sd(Ind_Capstone_Data$CFP)
sd(Ind_Capstone_Data$oZSP)
sd(Ind_Capstone_Data$dZSP)
sd(Ind_Capstone_Data$FOW)
sd(Ind_Capstone_Data$FOL)
sd(Ind_Capstone_Data$FOP)
sd(Ind_Capstone_Data$HIT)
sd(Ind_Capstone_Data$BLK)

#Finding mean, standard deviation, min, and max
min(Ind_Capstone_Data$CF)
min(Ind_Capstone_Data$CA)
min(Ind_Capstone_Data$CFP)
min(Ind_Capstone_Data$oZSP)
min(Ind_Capstone_Data$dZSP)
min(Ind_Capstone_Data$FOW)
min(Ind_Capstone_Data$FOL)
min(Ind_Capstone_Data$FOP)
min(Ind_Capstone_Data$HIT)
min(Ind_Capstone_Data$BLK)

#Finding mean, standard deviation, min, and max
max(Ind_Capstone_Data$CF)
max(Ind_Capstone_Data$CA)
max(Ind_Capstone_Data$CFP)
max(Ind_Capstone_Data$oZSP)
max(Ind_Capstone_Data$dZSP)
max(Ind_Capstone_Data$FOW)
max(Ind_Capstone_Data$FOL)
max(Ind_Capstone_Data$FOP)
max(Ind_Capstone_Data$HIT)
max(Ind_Capstone_Data$BLK)

#Plotting blocked shots vs hits to see if there is a linear relationship
ggplot(data=Ind_Capstone_Data, aes(x=BLK, y=HIT)) + xlab("Block Shots (BLK)") + ylab("Hits (HIT)") +geom_point()

#Creating Model 1, linear regression model with oZSP
sodel <- lm(CFP ~ oZSP, data=Ind_Capstone_Data)
summary(sodel)

#Creating Model 1, linear regression model with FOP
fodel <- lm(CFP ~ FOP, data=Ind_Capstone_Data)
summary(fodel)

#Creating Model 1, linear regression model with HIT
hodel <- lm(CFP ~ HIT, data=Ind_Capstone_Data)
summary(hodel)

#Creating Model 1, linear regression model with BLK
bodel <- lm(CFP ~ BLK, data=Ind_Capstone_Data)
summary(bodel)

# Creating Model 2, multiple regression model
model <- lm(CFP ~ oZSP+FOP+HIT+BLK,data = Ind_Capstone_Data)
summary(model) 
