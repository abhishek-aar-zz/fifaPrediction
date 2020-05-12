fifa= read.csv("2_standard.csv")
View(fifa)

# We know that the mean of the standardized data should be 0 and the sd should be 1
print(sd(fifa$Overall))
print(mean(fifa$Overall))

# One sample t-test---------------------------
# Ho : Mean = 0    [Null Hypothesis]
# H1 : Mean != 0   [Alternate Hypothesis]
t.test(fifa$Overall, mu = 0)


# Ho : Mean =< 0    [Null Hypothesis]
# H1 : Mean > 0   [Alternate Hypothesis]
t.test(fifa$Value,alternative = 'greater', mu = 0)

#--------------------------------------------


# Two sample t-test---------------------------

# Ho : Mean1 = Mean2 => (Mean1 - Mean2) = 0    [Null Hypothesis]
# H1 : Mean1 != Mean2   [Alternate Hypothesis]
t.test(fifa$Overall,fifa$Potential,alternative = 'two.sided', mu = 0)


# Ho : (Mean1 - Mean2) =< 0    [Null Hypothesis]
# H1 : (Mean1 - Mean2) >  0   [Alternate Hypothesis]
t.test(fifa$Overall,fifa$Potential,alternative = 'greater', mu = 0)

#--------------------------------------------


# Chi square test for independence
tab = table(fifa$Overall,fifa$Wage)
chisq.test(fifa$Overall,fifa$Wage,correct = T)


