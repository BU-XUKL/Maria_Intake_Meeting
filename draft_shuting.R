######data summary
library(dplyr)
library(tidyr)
library(ggplot2)
library("formattable")



summ <- aggdata %>%                              
  group_by(Taxa,Site,variable) %>% 
  summarize(min = min(value),
            median = median(value),
            mean = mean(value),
            max = max(value),
            sd = sd(value),
            count=n())
formattable(summ)

##compare mean###
ggplot(summ)+
  geom_point(aes(x=Taxa,y=mean,color=Site,shape=variable))

ggplot(summ)+
  geom_point(aes(x=Taxa,y=median,color=Site,shape=variable))

## check correlation of isotopes in 2 sites, sites show no difference of the c~n pattern in two sites
cor(data2[which(data2$Site=="Tlajinga"),3:5])
cor(data2[which(data2$Site=="BAOX"),3:5])

pairs(data2[which(data2$Site=="Tlajinga"),3:5])
pairs(data2[which(data2$Site=="BAOX"),3:5])

## ratio of c/n
datatest <- data
datatest$ratio <- datatest$d13C_VPDB/datatest$d15N_air
datatest$id <- c(1:68)

ggplot(datatest)+
  geom_point(aes(x=id,y=ratio,color=Taxa))

## linear regression
summary(fit.lm.d13C_VPDB <- lm(d13C_VPDB~Site, data = data))
summary(fit.lm.d15N_air <- lm(d15N_air~Site, data = data))
summary(fit.lm.ap_13C_VPDB <- lm(ap_13C_VPDB~Site, data = data))

summary(fit.lm.d13C_VPDB.Taxa <- lm(d13C_VPDB~Site+Taxa:Site, data = data))
summary(fit.lm.d15N_air.Taxa <- lm(d15N_air~Site+Taxa:Site, data = data))
summary(fit.lm.ap_13C_VPDB.Taxa <- lm(ap_13C_VPDB~Site+Taxa:Site, data = data))

## t-test + power calculate
#var.test(test.turkey.d13C[test.turkey.d13C$Site=="BAOX",4],test.turkey.d13C[test.turkey.d13C$Site=="Tlajinga",4])
var.test(value~Site, data = test.turkey.d13C) #test equality of variances 
t.test(value~Site, data = test.turkey.d13C, var.equal=TRUE)
library(pwr)
d <- (mean(with(test.turkey.d13C, (value[Site == "Tlajinga"])))-mean(with(test.turkey.d13C, (value[Site == "BAOX"]))))/sqrt((var(with(test.turkey.d13C, (value[Site == "Tlajinga"])))+var(with(test.turkey.d13C, (value[Site == "BAOX"]))))/2)
pwr.t.test(n = 7, d = d, sig.level = 0.01, type="two.sample",alternative="two.sided") #power = 0.04653823
pwr.t.test( d = d, sig.level = 0.01, power = 0.8, type="two.sample",alternative="two.sided") #n = 79

## two way anova
d13c <- aggdata %>% filter(variable=="d13C_VPDB")
d15n <- aggdata %>% filter(variable=="d15N_air")
ap13c <- aggdata %>% filter(variable=="ap_13C_VPDB")

fit_d13c <- lm(value ~ Site + Taxa + Site : Taxa, data = d13c)
summary(fit_d13c) ##only taxa:turkey&deer significant
fit_d15n <- lm(value ~ Site + Taxa + Site : Taxa, data = d15n)
summary(fit_d15n) ##only taxa:turkey significant
fit_ap13c <- lm(value ~ Site + Taxa + Site : Taxa, data = ap13c)
summary(fit_ap13c) ##only taxa:turkey significant

plot(fit_d13c, which = 2, add.smooth = FALSE) ##qq plot
plot(fit_d13c, which = 3, add.smooth = FALSE) ##check same variance

anova(fit_d13c)
summary(aov(value ~ Site + Taxa + Site : Taxa, data = d13c)) ##same
anova(fit_d15n) ##site is significant, pvalue=0.012<0.05, >0.01
anova(fit_ap13c)

##locate difference
TukeyHSD(aov(fit_d15n), which = c("Site", "Taxa"))
library(agricolae)
HSD.test(aov(fit_d15n), trt = c("Site", "Taxa"), console = TRUE)

##plot result
# step 1. calculate means for each combination
d13c_stat <- 
  d13c %>% 
  group_by(Site, Taxa) %>% 
  summarise(Means = mean(value), SEs = sd(value)/sqrt(n()))

ggplot(d13c_stat, 
       aes(x = Site, y = Means, fill = Taxa,
           ymin = Means - SEs, ymax = Means + SEs)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(position = position_dodge(0.9), width=.2) +
  xlab("Site") + ylab("d13c yield")

