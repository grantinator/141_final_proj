library(corrplot)
library(dplyr)
setwd("~/Desktop/CampusClimateProject")
climate = read.csv("campusclimate.csv")


#examine structure of dataframe
str(climate)

# Total number of NA's 
sum(is.na(climate))

# How many NA's in each variable
lapply(climate, function(x) sum(is.na(x)))

climate_clean = na.omit(climate)
dim(climate_clean) # removing NA's leaves us with 13 observations lol. So na.omit is a no go.

# NA count over 50% NA's
vars_drop = names(which(colMeans(is.na(climate)) > 0.5))

climate_clean = na.omit(climate[,-which(names(climate) %in% vars_drop)])
climate_numeric = climate_clean  %>% select_if(is.numeric)

correlation = climate_clean %>% 
		select_if(is.numeric) %>% 
		cor() %>% 
		round(digits=2) %>% 
		as.data.frame()
		
# Threshold correlation change so we can decide desired threshold
THRESHOLD_COR = 0.55
		
correlation[abs(correlation) < THRESHOLD_COR] <- NA
head(correlation)
academics correlation %>% filter_all(any_vars(!is.na(.))) %>% select("academicsp")

correlation = climate_numeric_clean %>% cor() %>% round(digits=2) %>% as.data.frame()
		
#Threshold correlation change so we can decide desired threshold
THRESHOLD_COR = 0.55
		
correlation[abs(correlation) < THRESHOLD_COR] <- NA
correlation %>% filter_all(any_vars(!is.na(.)))


## Exploratory Data Analysis
```{r}


summary(climate$academicsp)
summary(climate$exclusionaryp)
summary(climate$prejudiceenvp)

library(plyr)
mu_sex <- ddply(climate, "new_sex", summarise, grp.mean=mean(academicsp))
head(mu)

mu_firstgen <- ddply(climate, "FIRSTGEN", summarise, grp.mean=mean(academicsp))
head(mu_firstgen)

library(ggplot2)

#Density Plot showing academic satisfaction by Gender - conjoint
ggplot(climate, aes(x=academicsp, fill=new_sex)) + geom_density(alpha=0.4,binwidth = 10) + xlab("Academic Satisfaction") + ggtitle("Academic Satisfaction by Gender")+ geom_vline(data=mu, aes(xintercept=grp.mean, color=new_sex), linetype="dashed")

#Density Plot showing academic satisfaction by Gender - partitioned
ggplot(climate, aes(x=academicsp)) + geom_density(binwidth = 10) + facet_wrap(vars(climate$FIRSTGEN))

unique(climate$new_sex)

#Density Plot showing academic satisfaction by whether a student is firstgen or not - conjoint
ggplot(climate, aes(x=academicsp, fill=FIRSTGEN)) + geom_density(alpha=0.4,binwidth = 10) + xlab("Academic Satisfaction") + ggtitle("Academic Satisfaction by whether a student is firstgen or not")+ geom_vline(data=mu_firstgen, aes(xintercept=grp.mean, color=FIRSTGEN), linetype="dashed")

#Density Plot showing Prejudice experienced whether a student is firstgen or not - conjoint
ggplot(climate, aes(x=prejudiceenvp, color=NorthCampus)) + geom_density(binwidth = 10) + xlab("Prejudice Experienced ") + ggtitle("Prejudice Experienced by whether a student is firstgen or not")


```





