## Load the data
raw_data = read.csv(file.choose(),na.strings='N/A') ## recipeData.csv
dim(raw_data)

## Checking missing values
name = names(raw_data)
name
head(raw_data)
for(i in 1:23){
  print(paste(name[i], ': missing', sum(is.na(raw_data[,i])), 'values'))
}

## clean data
num_data = raw_data[,c(seq(-1,-4,-1),-17,-18,-21,-22,-23)] ## select all numeric 
head(num_data)
dim(num_data)
num_data = na.omit(num_data) ## remove all missing values in numeric data
dim(num_data)
head(num_data)

## Adequency test
cor_mat = cor(num_data[,-1])
bartlett.test(num_data)
library(psych)
KMO(cor_mat)

## Determine the number of components
p = prcomp(num_data[,-1], center=T, scale=T )
summary(p)
plot(p, main='Scree Plot')
abline(1,0)

## Rotation and interpretation
p1 = principal(num_data[,-1], rotate="varimax", nfactors=4, scores=TRUE)
print(p1$loadings, cutoff=.4, sort=T)
scor = p1$scores
head(scor)

## visualization
PCA_Plot = function(pcaData){
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = data.frame(pcaData$rotation, .names = row.names(pcaData$rotation))
  p + geom_text(data=loadings, mapping=aes(x = PC1, y = PC2, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")
}
PCA_Plot(p)
PCA_Plot_Secondary = function(pcaData){
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = data.frame(pcaData$rotation, .names = row.names(pcaData$rotation))
  p + geom_text(data=loadings, mapping=aes(x = PC3, y = PC4, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC3", y = "PC4")
}
PCA_Plot_Secondary(p)

## Missing variables form grouped components
plot(density(num_data$Efficiency), main='Distribution of Efficiency')
plot(density(num_data$MashThickness), main='Distribution of MashThickness')


