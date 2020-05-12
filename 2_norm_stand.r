# IMPORT BY USING THE SUITABLE ENCODING
fifa = read.csv( "1_cleaned_data.csv")

#SELECTS NUMERICAL COLUMNS ONLY
library("dplyr")
numfifa = select_if(fifa, is.numeric)

#NORMALIZE FUNTION USING MIN-MAX VALUES
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

#NORMALIZING THE DATA USING USER-DEFINED NORMALIZED FUNCTIONS
#UNITIZATION WITH ZERO MINIMUM ((x-min)/range)
mmfifa <- as.data.frame(lapply(numfifa, normalize))
write.csv(mmfifa, "2_minmax.csv", row.names = FALSE)

#NORMALIZING THE DATA THROUGH NATURAL LOGARITHMS
logfifa = as.data.frame(lapply(numfifa, log))
write.csv(logfifa, "2_log.csv", row.names = FALSE)

#STANDARDIZATION ((x-mean)/sd)
library("clusterSim")
sfifa = data.Normalization(numfifa, type="n1", normalization = "column")
write.csv(sfifa, "2_standard.csv", row.names = FALSE)

#NORMALIZATION ((x-mean)/sqrt(sum((x-mean)^2)))
nfifa = data.Normalization(numfifa, type="n12", normalization = "column")
write.csv(nfifa, "2_norm.csv", row.names = FALSE)


# #NORMALITY TEST

# This library is already loaded: library("dplyr")
library("ggpubr")

#DENSITY PLOT OF AGE
print(ggdensity(fifa$Age, main="Density plot of Age", xlab = "Ages"))

#DENSITY PLOT OF AGE[mmfifa]
print(ggdensity(mmfifa$Age, main="Density plot of Age[mmfifa]", xlab = "Ages"))

#QQ PLOT OF WAGE
print(ggqqplot(fifa$Wage, main="QQ Plot of Wage"))

#QQ PLOT OF WAGE[sfifa]
print(ggqqplot(sfifa$Wage, main="QQ Plot of Wage[sfifa]"))


