fifa = read.csv("1_cleaned_data.csv")
options(scipen = 999)
library("GGally")

#HEAT MAP
print(ggcorr(fifa))

#SCATTER PLOT
print(ggplot(fifa, aes(x=Release.Clause, y =Value)) + geom_jitter()+geom_smooth(method = lm))


#PEARSON CORRELATION COEFFICIENT
print(cor(fifa$Release.Clause,fifa$Value,  method = "pearson", use = "complete.obs"))
#PEARSON TEST FOR ASSOCIATION/CORRELATION BETWEEN SAMPLES
print(cor.test(fifa$Release.Clause,fifa$Value, method="pearson"))

#KENDALL CORRELATION COEFFICIENT
print(cor(fifa$Release.Clause,fifa$Value,  method = "kendall", use = "complete.obs"))
#KENDALL TEST FOR ASSOCIATION/CORRELATION BETWEEN SAMPLES
print(cor.test(fifa$Release.Clause,fifa$Value, method="kendall"))

#SPREARMAN CORRELATION COEFFICIENT
print(cor(fifa$Release.Clause,fifa$Value,  method = "spearman", use = "complete.obs"))
#SPEARMAN TEST FOR ASSOCIATION/CORRELATION BETWEEN SAMPLES
print(cor.test(fifa$Release.Clause,fifa$Value, method="spearman"))

