# Now in the second portion of the class, we're going to analyze the ToothGrowth data in the R datasets package.
#     Load the ToothGrowth data and perform some basic exploratory data analyses 
#     Provide a basic summary of the data.
#     Use confidence intervals and hypothesis tests to compare tooth growth by supp and dose. (Use the techniques from class even if there's other approaches worth considering)
# State your conclusions and the assumptions needed for your conclusion


#library laoding
library(ggplot2)
library(plyr)
library(dplyr)
library(xtable)
library(gridExtra)
library(doBy)
#load the data
tooth_data <- ToothGrowth

head(tooth_data)

summary(tooth_data)
#check the distribution of the data
ggplot(tooth_data, aes(len)) + geom_histogram(alpha = .20, colour = "black", aes(y = ..density..)) + labs(title="Distribution of len of tooth by dose",
                                                         y="Density") + facet_grid(. ~ dose)
#do a box plot of the data
ggplot(tooth_data, aes(x=supp, y=len)) + geom_boxplot(aes(x=supp, y=len,fill =factor(dose))) + labs(title="Length of tooth by supplement and dosage",
                                                                                                     y="Length of tooth", x="Supplement") +
  guides(fill=guide_legend(title="Dosage")) + theme_bw()

#summary of the data
tooth_summary <- ddply(tooth_data, c("dose", "supp"), summarise,
               count    = sum(!is.na(len)),
               mean = mean(len, na.rm=TRUE),
               sd   = sd(len, na.rm=TRUE),
               se   = sd / sqrt(count) ,
               ci = se*(qt(.95/2 + .5, count-1)))
xtable(tooth_summary)
#plotting the data with error bars

par(mfrow = c(1,2))
p1 <- ggplot(tooth_summary, aes(x=factor(dose), y=mean, fill=supp)) +   geom_bar(position=position_dodge(), stat="identity", colour="red",  size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),size=.3,width=.2,position=position_dodge(.9)) +
  labs(title="The Effect of Vitamin C on\nTooth Growth", y="Length of tooth", x="Dose (mg)") +
    scale_fill_hue(name="Supplement type",   breaks=c("OJ", "VC"),labels=c("Orange juice", "Ascorbic acid")) + theme_bw()
 
pd <- position_dodge(.1)
p2 <- ggplot(tooth_summary, aes(x=supp, y=mean, colour=factor(dose), group=dose)) + 
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), colour="red", width=.1, position=pd) + geom_line(position=pd) +
  geom_point(position=pd, size=3) + theme_bw() +  
  labs(title="The Effect of Vitamin C on Tooth Growth \n by Supplement and the CI", y="Length of tooth", x="Supplement") + guides(fill=guide_legend(title="Dosage (mg)"))
grid.arrange(p1, p2)

#ttest on tthe data
t.test(tooth_data$len~tooth_data$supp , var.equal = T)

dose.5 <- tooth_data[tooth_data$dose == 0.5,]
dose.1 <- tooth_data[tooth_data$dose == 1,]
dose.2 <- tooth_data[tooth_data$dose == 2,]

dose.5Test <- t.test(dose.5$len~dose.5$supp , var.equal = T)
dose.1Test <- t.test(dose.1$len~dose.1$supp , var.equal = T)
dose.2Test <- t.test(dose.2$len~dose.2$supp , var.equal = T)

conf.5 <- dose.5Test$conf[1:2]
conf.1 <- dose.1Test$conf[1:2]
conf.2 <- dose.2Test$conf[1:2]

pvalue.5 <- dose.5Test$p.value
pvalue.1 <- dose.1Test$p.value
pvalue.2 <- dose.2Test$p.value

