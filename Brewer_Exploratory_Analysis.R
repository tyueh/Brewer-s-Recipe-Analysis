## Load the data  ####
raw_data = read.csv(file.choose(),na.strings='N/A') ## recipeData.csv
dim(raw_data)

## clean data
style = raw_data[,4]
num_data = raw_data[,c(seq(-1,-4,-1),-17,-18,-21,-22,-23)] ## select all numeric 
head(num_data)
dim(num_data)
num_data = na.omit(num_data) ## remove all missing values in numeric data
dim(num_data)
head(num_data)

library(ggplot2)
## EXPLORATORY ANALYSIS
##########################################################################
##########################################################################
dim(num_data)
summary(num_data[,-1])

## style distribution
barplot(prop.table(table(num_data$StyleID))*100, las=1, ylim=c(0,18),
        xlab= 'style', ylab='Percentage (%)', main='Beer Style Distribution',
        col=c(rep('grey',6), 'blue',rep('grey',2), 'green', rep('grey', 120), 'yellow', rep('grey', 41)))
legend('topright', c('American IPA','American Pale Ale','Saison'), lty=c(1,1), lwd=c(2.5,2.5),col=c('blue','green', 'yellow')) 
box()

## variables distribution
normalize = function(x){return ((x - min(x)) / (max(x) - min(x)))}
norm_data = as.data.frame(lapply(num_data[,-1], normalize))
boxplot(norm_data, las = 2, pch=16, cex=0.2, main='Variables Distribution (min-max)')

## scatter plot
pairs(FG~Size.L.+OG+ABV+IBU,data=num_data, 
      main='Scatterplot Matrix 1')
pairs(FG~Color+BoilSize+BoilTime+BoilGravity,data=num_data, 
      main='Scatterplot Matrix 2')
pairs(FG~Efficiency+MashThickness+PitchRate+PrimaryTemp,data=num_data, 
      main='Scatterplot Matrix 3')

## correlation
library(corrplot)
mat = num_data[,2:13]
cor(mat) #### correlation matrix
corrplot(cor(mat), method='circle')

##########################################################################
##########################################################################