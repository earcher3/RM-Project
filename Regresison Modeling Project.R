setwd("C:/Users/EKArc/OneDrive/Documents/2024 Spring Classes/Regression Modeling/Data")
Data=read.csv("Data.csv",header=TRUE,sep=",")
names(Data)
attach(Data)

Data=subset(Data,select= -c(X))
Data=na.omit(Data)
