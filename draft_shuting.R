######data summary
library(dplyr)
library(tidyr)
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
