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


## Exploratory Data Analysis [NISH]
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


```{r}

#first gen to GPA
ggplot(climate, aes(x=climate$gpa)) + geom_histogram(binwidth = 10, stat = "count") + facet_wrap(vars(climate$FIRSTGEN))

#North Campus to GPA
ggplot(climate, aes(x=climate$gpa)) + geom_histogram(binwidth = 10, stat = "count") + facet_wrap(vars(climate$NorthCampus))

#Sex to GPA
ggplot(climate, aes(x=climate$gpa)) + geom_histogram(binwidth = 10, stat = "count") + facet_wrap(vars(climate$new_sex))

#Ethnicity to GPA
ggplot(climate, aes(x=climate$gpa)) + geom_histogram(binwidth = 10, stat = "count") + facet_wrap(vars(climate$Ethnicity))

#comfort to GPA
ggplot(climate, aes(x=climate$gpa)) + geom_histogram(binwidth = 10, stat = "count") + facet_wrap(vars(climate$overallcomfort))

#Income to GPA
ggplot(climate, aes(x=climate$gpa)) + geom_histogram(binwidth = 10, stat = "count") + facet_wrap(vars(climate$LowFamilyIncomeIndicator))

#Type of student to GPA
ggplot(climate, aes(x=climate$gpa)) + geom_histogram(binwidth = 10, stat = "count") + facet_wrap(vars(climate$student_level_application_uplw))

#sexual orientation to GPA
ggplot(climate, aes(x=climate$gpa)) + geom_histogram(binwidth = 10, stat = "count") + facet_wrap(vars(climate$orientation))

#housing type to GPA
ggplot(climate, aes(x=climate$gpa)) + geom_histogram(binwidth = 10, stat = "count") + facet_wrap(vars(climate$housing_type))

#employment status to GPA
ggplot(climate, aes(x=climate$gpa)) + geom_histogram(binwidth = 10, stat = "count") + facet_wrap(vars(climate$EmploymentIndicator))

#How Students entered college to GPA
ggplot(climate, aes(x=climate$gpa)) + geom_histogram(binwidth = 10, stat = "count") + facet_wrap(vars(climate$UGONLY))

#Disability to GPA
ggplot(climate, aes(x=climate$gpa)) + geom_histogram(binwidth = 10, stat = "count") + facet_wrap(vars(climate$DisabilityIndicator))

```


## Data Crunching [NISH]
```{r}

table(climate$leaveUCLA)

unique(climate$famincome)
unique(climate$gpa)

## Dichotamize GPA to be able to run logistic 
climate$gpa_recoded <- factor(length(climate$gpa))
levels(climate$gpa_recoded) = c("3 and Above", "Below 3")
climate$gpa_recoded[which(climate$gpa  == "3.5 and above")] <- "3 and Above"
climate$gpa_recoded[which(climate$gpa  == "3 - 3.49")] <- "3 and Above"
climate$gpa_recoded[which(climate$gpa  == "Below 2.49")] <- "Below 3"
climate$gpa_recoded[which(climate$gpa  == "2.5 - 2.99")] <- "Below 3"

#Recoding as 0's and 1's to run logistic
#climate$gpa_recoded[which(climate$gpa_recoded  == "Below 3")] <- 0
#climate$gpa_recoded[which(climate$gpa_recoded  == "3 and Above")] <- 1
#unique(climate$gpa_recoded)

```

## Bivariate Tests [NISH]
```{r}
anova_academicsp <- aov(gpa_recoded ~ academicsp, data = climate)
summary(anova_academicsp) ##Academic Satisfaction is Super Significant

anova_witnesssex<- aov(gpa_recoded ~ witnessexcluconduct, data = climate)
summary(anova_witnesssex) ## Wintessed Sexual Conduct is not Significant

anova_overallcomfort <- aov(gpa_recoded ~ overallcomfort, data = climate)
summary(anova_overallcomfort) ##Overall Comfort is Significant

anova_FIRSTGEN <- aov(gpa_recoded ~ FIRSTGEN, data = climate)
summary(anova_FIRSTGEN) ##Whether a student is FirstGen or not Comfort is Super Significant

anova_prejudiceenvp<- aov(gpa_recoded ~ prejudiceenvp, data = climate)
summary(anova_prejudiceenvp) ##Whether a student experiences prejudice or not is NOT Significant

anova_overallclimatep <- aov(gpa_recoded ~ overallclimatep, data = climate)
summary(anova_overallclimatep) ##Overall Climate is NOT Significant

climate$prejudiceenvp
# Chi-Square to see if there is a significant difference in GPA between different family income groups
chisq.test(climate$famincome,climate$gpa_recoded) #Yes there is a significant difference but we dont know between whic groups 


```


# Modelling attempt [NISH]
```{r}

## Finding row indices that have NA's for GPA_recoded and Political View 
unique(c(which(is.na(climate$PoliticalView) == TRUE),which(is.na(climate$gpa_recoded) == TRUE)))

logistic_gpa_academicsp <- glm(gpa_recoded~academicsp, data=climate[complete.cases(climate[,c("academicsp", "gpa_recoded")]),], family="binomial") ## G
summary(logistic_gpa_academicsp)

#Summary shows academicsp is significant and resid dev. / null dev. = 0.91
model_pval = 1 - pchisq(5126.8, 5380)
model_pval #Model is not significant compared to model containing all variables?
```
       
#recode (Significant) famincome to have few levels
new_income_lvls = c("Low Income", "Medium Income", "High Income")
low_income = c("Below $10,000", "$10,000-$19,999", "$20,000-$29,999", "$30,000 - $39,999", "$40,000 - $49,999")
med_income = c("$50,000 - $59,999", "$60,000- $69,999", "$70,000- $79,999", "$80,000 - $89,999", "$90,000- $99,999", 
               "$100,000 - $124,999", "$125,000 - $149,999", "150,000 - $199,999")
high_income = c("$200,000 - $249,999", "$250,000 - $299,999", "$300,000 - $399,999", "$400,000 - $499,999", "$500,000 or more")

recoded_income = factor(length(climate$famincome))
levels(recoded_income) = c("Low Income", "Med Income", "High Income")
recoded_income[which(climate$famincome %in% low_income)] = "Low Income"
recoded_income[which(climate$famincome %in% med_income)] = "Med Income"
recoded_income[which(climate$famincome %in% high_income)] = "High Income"
climate = cbind(climate, recoded_income)

logistic_gpa_income <- glm(gpa_recoded~recoded_income, data=climate[complete.cases(climate[,c("recoded_income", "gpa_recoded")]),], family="binomial") ## G
summary(logistic_gpa_income)
  
       
 
# Variable Manipulation 
```{r}

####### QUESTION 10 ########
q10 = climate[,grep("Q10", colnames(climate))]

reverseScale = function(val) { 
  return(6 - val)    
}
q10 = apply(q10[,-9], 2, reverseScale)

q10[is.na(q10)] = 0

####### QUESTION 12 ########
q12 = climate[,grep("Q12", colnames(climate))]
q12[which(is.na(q12) == T),] <- 0
q12[q12 < 5] <- 1 #experienced
q12[q12 == 5] <- 0 #Not applicable
       

####### QUESTION 61 ##########
q61 = climate[,grep("Q61", colnames(climate))]
q61[is.na(q61)] = 0
dim(q61)
colSums(q61)

## Analysis for Q61
colSums(q61) %>% sum()

barplot(colSums(q61), las = 3)

## Highest : 4 is derogatory remarks, 14 is racial profiling


###### QUESTION 63 ##########

q63 = climate[,grep("Q63", colnames(climate))]
q63[is.na(q63)] = 0
dim(q63)
       
## Analysis for Q63
colSums(q63)

colSums(q63) %>% sum()

barplot(colSums(q63), las = 3) 

## Highest: 2 is while in class, lab or clinical study 
## Lowest: 16 is on-campus transportion. ## 11 is athletic facilities 

```

# Variable Manipulation (Continued)
```{r}
###### QUESTION 75 ##########
q75 = climate[,grep("Q75", colnames(climate))]
q75[is.na(q75)] = 0

###### QUESTION 76 ##########
q76 = climate[,grep("Q76", colnames(climate))]
q76[is.na(q76)] = 0
       
####### QUESTION 77 ##########
q77 = climate[,grep("Q77", colnames(climate))]

q77[q77 <= 2] = 2
q77[q77 == 3] = 1
q77[q77 == 4] = 1
q77[q77 == 5] = 0
q77[is.na(q77)] = 0
       
#2 is agree
#1 is disagree
#0 is don't know
 
####### QUESTION 78 ##########     
q78 = climate[,grep("Q78", colnames(climate))]

reverseScale = function(val) { 
  return(6 - val)    
}
q78 = apply(q78[,-c(3,9)], 2, reverseScale) %>% as.data.frame()

q78[is.na(q78)] = 0
        
####### QUESTION 80 ##########
q80 = climate[,grep("Q80", colnames(climate))]
q80[q80 <= 2] = 2
q80[q80 == 3] = 1
q80[q80 == 4] = 1
q80[q80 == 5] = 0
q80[is.na(q80)] = 0

###### QUESTION 82 #######       
q82 = climate[,grep("Q82", colnames(climate))]
q82[q82 <= 2] = 1
q82[q82 == 3] = 2
q82[q82 == 4] = 0

##### QUESTION 84 #######
q84 = climate[,grep("Q84", colnames(climate))]
q84[q84 <= 2] = 2
q84[q84 == 3] = 1
q84[q84 == 4] = 1
q84[q84 == 5] = 0
q84[is.na(q84)] = 0

##### QUESTION 85 #######
q85 = climate[,grep("Q85", colnames(climate))]

q85[q85 <= 2] = 2
q85[q85 == 3] = 1
q85[q85 == 4] = 1
q85[q85 == 5] = 0
q85[is.na(q85)] = 0

q86 = climate[,grep("Q86", colnames(climate))]

q86[q86 <= 2] = 2
q86[q86 == 3] = 1
q86[q86 == 4] = 1
q86[q86 == 5] = 0
q86[is.na(q86)] = 0
       
```


# Data Analysis on Adjusted Variables
```{r}

###### QUESTION 85 #######       


# Following dataframes are each ethnicity answering about their perceived racial discrimination about their race at UCLA.
# So, African Americans answering about experienced discrimination against African Americans at UCLA and so on

q85Ethnic = cbind(q85, "ethnicity"=climate$Ethnicity)

AfrAm = q85Ethnic %>% filter(ethnicity == "African American/Black")
AfrAmIntersect = AfrAm[,'Q85_A_1'] %>% unlist() %>% as.factor()

AsiAm = q85Ethnic %>%  filter(ethnicity == "Asian/Asian Am")
AsiAmIntersect = AsiAm[,'Q85_A_3'] %>% unlist() %>% as.factor()

HisLat = q85Ethnic %>%  filter(ethnicity == "Hispanic/Latino") 
HisLatIntersect = HisLat[,'Q85_A_4'] %>% unlist() %>% as.factor()

White = q85Ethnic %>% filter(ethnicity == "White") 
WhiteIntersect = White[,'Q85_A_7'] %>% unlist() %>% as.factor()


# Plots of above dataframes
par(mfrow=c(2,2), mar = c(5,4,4,2))
barplot(table(AfrAmIntersect)/length(AfrAmIntersect), main = "African American Intersect", col = c("darkred", "maroon", "darkgreen"))
barplot(table(AsiAmIntersect)/length(AsiAmIntersect), main = "Asian American Intersect", col = c("darkred", "maroon", "darkgreen"))
barplot(table(HisLatIntersect)/length(HisLatIntersect), main = "Hispanic Latino Intersect", col = c("darkred", "maroon", "darkgreen"))
barplot(table(WhiteIntersect)/length(WhiteIntersect), main = "White Intersect", col = c("darkred", "maroon", "darkgreen"))
mtext("Self-Percieved Respect for Student's own Ethnicity",side = 3,line = -1, outer = TRUE, cex = 1.0)
```


```{r}
###### QUESTION 77 #######       
unique(q77$Q77_A_1)

barplot(climate$Q77_A_1)
table(q77$Q77_A_1)

barplot(table(q77$Q77_A_1),main="Learning environment at UCLA is welcoming for students based on their Age",col = c("lightblue", "red", "darkgreen"))

barplot(table(q77$Q77_A_6),main="Learning environment at UCLA is welcoming for students based on their Gender Identity",col = c("lightblue", "red", "darkgreen"))
```
