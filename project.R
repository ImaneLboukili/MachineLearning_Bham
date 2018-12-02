data = read.csv("housing.csv",header = TRUE)
summary(data)
#Getting rid of NAs
data = data[complete.cases(data),]

#One hot encoding
#install.packages("onehot")
library(onehot)
#Create OHE object
transform=onehot(data)
#Transform OHE object into a data frame 
data_ohe = as.data.frame(predict(transform,data))
attach(data_ohe)
#Test correlation between variables

#Correlation matrix
matrix_corr = cor(data_ohe)
#Is cor significative
library(Hmisc)
res_corr = rcorr(as.matrix(data_ohe), type=c("pearson","spearman"))

++++++++++++++++++++++++++++
  # flattenCorrMatrix
  # ++++++++++++++++++++++++++++
  # cormat : matrix of the correlation coefficients
  # pmat : matrix of the correlation p-values
  flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
      row = rownames(cormat)[row(cormat)[ut]],
      column = rownames(cormat)[col(cormat)[ut]],
      cor  =(cormat)[ut],
      p = pmat[ut]
    )
  }


correlation=flattenCorrMatrix(res_corr$r, res_corr$P)
#Visulasing correlation
# install.packages("corrplot")
library(corrplot)
X11()
corrplot(matrix_corr, type="upper", order="hclust", tl.col="black", tl.srt=45)
heatmap(x = matrix_corr, col = colorRampPalette(c("blue", "white", "red"))(20), symm = TRUE)



# split input and output
input <- data_ohe[,c(1:8,10:14)]
output <- data_ohe[,9]



##############################
#                            #
#   POLYNOMIAL REGRESSION    #
#                            #
##############################

set.seed(28)
#y= polym(longitude, latitude, housing_median_age, total_rooms, total_bedrooms, population, households, median_income, degree=3, raw=TRUE) 
#prediction = predict(y)


test1=lm(median_house_value ~ poly(longitude,2,raw=TRUE)+poly(latitude,2,raw=TRUE)+poly(housing_median_age,2,raw=TRUE)+ poly(total_rooms,2,raw=TRUE)+ poly(total_bedrooms,2,raw=TRUE)+ poly(population,2,raw=TRUE)+ poly(households,2,raw=TRUE)+ poly(median_income,2,raw=TRUE), data = data_ohe)
pred = predict(test1)
accuracy=(median_house_value-pred)/median_house_value *100

