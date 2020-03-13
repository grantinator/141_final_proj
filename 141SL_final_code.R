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
climate$gpa_recoded <- factor(climate$gpa)
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
q12[is.na(q12)] <- 0
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
       
####### QUESTION 77 - MAKE BINARY ##########
q77 = climate[,grep("Q77", colnames(climate))]

q77[q77 <= 2] = 2
q77[q77 == 3] = 1
q77[q77 == 4] = 1
q77[q77 == 5] = NA

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
        
####### QUESTION 80 - MAKE BINARY ##########
q80 = climate[,grep("Q80", colnames(climate))]
q80[q80 <= 2] = 2
q80[q80 == 3] = 1
q80[q80 == 4] = 1
q80[q80 == 5] = NA

###### QUESTION 82 - MAKE BINARY #######       
q82 = climate[,grep("Q82", colnames(climate))]
q82[q82 <= 2] = 1
q82[q82 == 3] = 2
q82[q82 == 4] = NA

##### QUESTION 84- MAKE BINARY #######
q84 = climate[,grep("Q84", colnames(climate))]
q84[q84 <= 2] = 2
q84[q84 == 3] = 1
q84[q84 == 4] = 1
q84[q84 == 5] = NA


##### QUESTION 85- MAKE BINARY #######
q85 = climate[,grep("Q85", colnames(climate))]

q85[q85 <= 2] = 2
q85[q85 == 3] = 1
q85[q85 == 4] = 1
q85[q85 == 5] = NA
q85[is.na(q85)] = NA

##### QUESTION 86 - MAKE BINARY #######

q86 = climate[,grep("Q86", colnames(climate))]

q86[q86 <= 2] = 2
q86[q86 == 3] = 1
q86[q86 == 4] = 1
q86[q86 == 5] = NA
      
       
climate.clean = data.frame(climate[,1:22],q10,q12,q61,q63,q75,q76,q77,q78,q80,q82,q85)
```

# Data Analysis on Adjusted Variables
```{r}

###### QUESTION 85 #######      
# Following dataframes are each ethnicity answering about their perceived racial discrimination about their race at UCLA.
# So, African Americans answering about experienced discrimination against African Americans at UCLA and so on

q85Ethnic = cbind(q85, "ethnicity"=climate$Ethnicity)

AfrAmIntersect = na.omit(q85Ethnic) %>% filter(ethnicity == "African American/Black") %>% select(Q85_A_1) %>% unlist() %>% as.factor()
AsiAmIntersect = na.omit(q85Ethnic) %>%  filter(ethnicity == "Asian/Asian Am") %>% select(Q85_A_3) %>% unlist() %>% as.factor()
HisLatIntersect = na.omit(q85Ethnic) %>%  filter(ethnicity == "Hispanic/Latino") %>% select(Q85_A_4) %>% unlist() %>% as.factor()
WhiteIntersect = na.omit(q85Ethnic) %>% filter(ethnicity == "White") %>% select(Q85_A_7) %>% unlist() %>% as.factor()

# Plots of above dataframes
par(mfrow=c(2,2))
barplot(table(AfrAmIntersect)/length(AfrAmIntersect), main = "African American Perception of UCLA Climate", col = c("red", "darkgreen"))
barplot(table(AsiAmIntersect)/length(AsiAmIntersect), main = "Asian American Perception of UCLA Climate", col = c("red", "darkgreen"))
barplot(table(HisLatIntersect)/length(HisLatIntersect), main = "Hispanic Latino Perception of UCLA Climate", col = c("red", "darkgreen"))
barplot(table(WhiteIntersect)/length(WhiteIntersect), main = "White Perception of UCLA Climate", col = c("red", "darkgreen"))
legend(1, 1, legend=c("Disrespectful", "Respectful"), col=c("red", "darkgreen"), lty=1:2, cex=0.8)

 ```

###### QUESTION 77 PLOTS #######       

```{r}
unique(q77$Q77_A_1)

barplot(climate$Q77_A_1)
table(q77$Q77_A_1)

par(mfrow=c(1,2))
barplot(table(na.omit(q77)$Q77_A_1)/length(na.omit(q77)$Q77_A_1),main="Learning environment at UCLA is welcoming for students based on their Age",col = c("red", "lightblue"))
barplot(table(na.omit(q77)$Q77_A_1)/length(na.omit(q77)$Q77_A_1),main="Learning environment at UCLA is welcoming for students based on their Gender Identity",col = c("red", "lightblue"))
legend(0.2, 0.8, legend=c("Not Welcoming", "Welcoming"), col=c("red", "lightblue"), lty=1:2, cex=0.8)
```
       
       
#### Gender ####
### Q10 Aggregate
q10Male = climate.clean %>% filter(new_sex == "Male") %>% select(contains("Q10")) %>% rowSums()
q10Fem = climate.clean %>% filter(new_sex == "Female") %>% select(contains("Q10")) %>% rowSums()
q10Other = climate.clean %>% filter(new_sex == "Other/Unknown") %>% select(contains("Q10")) %>% rowSums()
### Male Vs. Female
t.test(q10Male, q10Fem) #significant
### Male vs Other/Unknown
t.test(q10Male, q10Other) #significant
### Female vs Other/Unknown
t.test(q10Other, q10Fem) #significant

### Q77
q77Male = climate.clean %>% filter(new_sex == "Male") %>% select(contains("Q77")) %>% rowSums()
q77Fem = climate.clean %>% filter(new_sex == "Female") %>% select(contains("Q77")) %>% rowSums()
q77Other = climate.clean %>% filter(new_sex == "Other/Unknown") %>% select(contains("Q77")) %>% rowSums()
### Male vs Female
t.test(q77Male, q77Fem) # !!Not significant
### Male vs Other/Unknown
t.test(q77Male, q77Other) # Signficant
### Female vs Other/Unknown
t.test(q77Fem, q77Other) # Significant

### Q77
q77Male = climate.clean %>% filter(new_sex == "Male") %>% select(contains("Q77")) %>% rowSums()
q77Fem = climate.clean %>% filter(new_sex == "Female") %>% select(contains("Q77")) %>% rowSums()
q77Other = climate.clean %>% filter(new_sex == "Other/Unknown") %>% select(contains("Q77")) %>% rowSums()
### Male vs Female
t.test(q77Male, q77Fem) # !!Not significant
### Male vs Other/Unknown
t.test(q77Male, q77Other) # Signficant
### Female vs Other/Unknown
t.test(q77Fem, q77Other) # Significant


### Q78
q78Male = climate.clean %>% filter(new_sex == "Male") %>% select(contains("Q78")) %>% rowSums()
q78Fem = climate.clean %>% filter(new_sex == "Female") %>% select(contains("Q78")) %>% rowSums()
q78Other = climate.clean %>% filter(new_sex == "Other/Unknown") %>% select(contains("Q78")) %>% rowSums()
### Male vs Female
t.test(q78Male, q78Fem) # Significant
### Male vs Other/Unknown
t.test(q78Male, q78Other) # Signficant
### Female vs Other/Unknown
t.test(q78Fem, q78Other) # Significant

#### Transfer vs four year ####
###Q10
q10Fy = climate.clean %>% filter(UGONLY == "fy") %>% select(contains("Q10")) %>% rowSums()
q10Transfer = climate.clean %>% filter(UGONLY == "transfer") %>% select(contains("Q10")) %>% rowSums()
### Left vs right
t.test(q10Fy, q10Transfer) #significant

###Q78
q78Fy = climate.clean %>% filter(UGONLY == "fy") %>% select(contains("Q78")) %>% rowSums()
q78Transfer = climate.clean %>% filter(UGONLY == "transfer") %>% select(contains("Q78")) %>% rowSums()
### Left vs right
t.test(q78Fy, q78Transfer) #not significant

###Q80
q80Fy = climate.clean %>% filter(UGONLY == "fy") %>% select(contains("Q80")) %>% rowSums()
q80Transfer = climate.clean %>% filter(UGONLY == "transfer") %>% select(contains("Q80")) %>% rowSums()
### Left vs right
t.test(q80Fy, q80Transfer) #significant

#### Ethnicity ####
### Q10
q10Black = climate.clean %>% filter(Ethnicity == "African American/Black") %>% select(contains("Q10")) %>% rowSums()
q10Asian = climate.clean %>% filter(Ethnicity == "Asian/Asian Am" ) %>% select(contains("Q10")) %>% rowSums()
q10Latino = climate.clean %>% filter(Ethnicity == "Hispanic/Latino") %>% select(contains("Q10")) %>% rowSums()
q10Intl = climate.clean %>% filter(Ethnicity == "International") %>% select(contains("Q10")) %>% rowSums()
q10Min = climate.clean %>% filter(Ethnicity == "Minority, Multi-Min., URM") %>% select(contains("Q10")) %>% rowSums()
q10White = climate.clean %>% filter(Ethnicity == "White") %>% select(contains("Q10")) %>% rowSums()

## Black vs Asian
t.test(q10Black, q10Asian) #Significant
## Black vs Latino 
t.test(q10Black, q10Latino) #Significant
## Black vs Intl
t.test(q10Black, q10Intl) #Significant
#Black vs Minority
t.test(q10Black, q10Min) #significant
# Black vs white
t.test(q10Black, q10White) #Significant
# Asian vs Latino
t.test(q10Asian, q10Latino) #Significant
# Asian vs Intnl
t.test(q10Asian, q10Intl) #Significant
#Asian vs Minority
t.test(q10Asian, q10Min) #Significant
# Asian vs white
t.test(q10Asian, q10White) #Significant
# Latino vs Intl
t.test(q10Latino, q10Intl) #Significant
# Latino vs Minority
t.test(q10Latino, q10Min) #Significant
# Latino vs White
t.test(q10Latino, q10Min) #significant
#Min vs White 
t.test(q10Min, q10White) #significant

### Q12_A_9
q12Black = climate.clean %>% filter(Ethnicity == "African American/Black") %>% select(contains("Q12_A_9"))
q12Asian = climate.clean %>% filter(Ethnicity == "Asian/Asian Am" ) %>% select(contains("Q12_A_9"))
q12Latino = climate.clean %>% filter(Ethnicity == "Hispanic/Latino") %>% select(contains("Q12_A_9"))
q12Intl = climate.clean %>% filter(Ethnicity == "International") %>% select(contains("Q12_A_9"))
q12Min = climate.clean %>% filter(Ethnicity == "Minority, Multi-Min., URM") %>% select(contains("Q12_A_9"))
q12White = climate.clean %>% filter(Ethnicity == "White") %>% select(contains("Q12_A_9"))

## Black vs Asian
t.test(q12Black, q12Asian) # not Significant
## Black vs Latino 
t.test(q12Black, q12Latino) # not Significant
## Black vs Intl
t.test(q12Black, q12Intl) # not Significant
#Black vs Minority
t.test(q12Black, q12Min) #significant
# Black vs white
t.test(q12Black, q12White) #Significant
# Asian vs Latino
t.test(q12Asian, q12Latino) # not Significant
# Asian vs Intnl
t.test(q12Asian, q12Intl) # not Significant
#Asian vs Minority
t.test(q12Asian, q12Min) #Significant
# Asian vs white
t.test(q12Asian, q12White) #Significant
# Latino vs Intl
t.test(q12Latino, q12Intl) # not Significant
# Latino vs Minority
t.test(q12Latino, q12Min) #Significant
# Latino vs White
t.test(q12Latino, q12Min) #significant
#Min vs White 
t.test(q12Min, q12White) # not Significant


### Q76_A_1
q76Black = climate.clean %>% filter(Ethnicity == "African American/Black") %>% select(contains("Q76_A_1"))
q76Asian = climate.clean %>% filter(Ethnicity == "Asian/Asian Am" ) %>% select(contains("Q76_A_1"))
q76Latino = climate.clean %>% filter(Ethnicity == "Hispanic/Latino") %>% select(contains("Q76_A_1"))
q76Intl = climate.clean %>% filter(Ethnicity == "International") %>% select(contains("Q76_A_1"))
q76Min = climate.clean %>% filter(Ethnicity == "Minority, Multi-Min., URM") %>% select(contains("Q76_A_1"))
q76White = climate.clean %>% filter(Ethnicity == "White") %>% select(contains("Q76_A_1"))

## Black vs Asian
t.test(q76Black, q76Asian) # Significant
## Black vs Latino 
t.test(q76Black, q76Latino) # Significant
## Black vs Intl
t.test(q76Black, q76Intl) # Significant
#Black vs Minority
t.test(q76Black, q76Min) #significant
# Black vs white
t.test(q76Black, q76White) #Significant
# Asian vs Latino
t.test(q76Asian, q76Latino) # Significant
# Asian vs Intnl
t.test(q76Asian, q76Intl) # Significant
#Asian vs Minority
t.test(q76Asian, q76Min) #Significant
# Asian vs white
t.test(q76Asian, q76White) #Significant
# Latino vs Intl
t.test(q76Latino, q76Intl) #Significant
# Latino vs Minority
t.test(q76Latino, q76Min) #Significant
# Latino vs White
t.test(q76Latino, q76Min) #significant
#Min vs White 
t.test(q76Min, q76White) # not Significant


### Q80_A_6
q80Black = climate.clean %>% filter(Ethnicity == "African American/Black") %>% select(contains("Q80_A_6"))
q80Asian = climate.clean %>% filter(Ethnicity == "Asian/Asian Am" ) %>% select(contains("Q80_A_6"))
q80Latino = climate.clean %>% filter(Ethnicity == "Hispanic/Latino") %>% select(contains("Q80_A_6"))
q80Intl = climate.clean %>% filter(Ethnicity == "International") %>% select(contains("Q80_A_6"))
q80Min = climate.clean %>% filter(Ethnicity == "Minority, Multi-Min., URM") %>% select(contains("Q80_A_6"))
q80White = climate.clean %>% filter(Ethnicity == "White") %>% select(contains("Q80_A_6"))

## Black vs Asian
t.test(q80Black, q80Asian) # Significant
## Black vs Latino 
t.test(q80Black, q80Latino) # not Significant
## Black vs Intl
t.test(q80Black, q80Intl) # Significant
#Black vs Minority
t.test(q80Black, q80Min) #significant
# Black vs white
t.test(q80Black, q80White) #Significant
# Asian vs Latino
t.test(q80Asian, q80Latino) # Significant
# Asian vs Intnl
t.test(q80Asian, q80Intl) # not Significant
#Asian vs Minority
t.test(q80Asian, q80Min) #Significant
# Asian vs white
t.test(q80Asian, q80White) #Significant
# Latino vs Intl
t.test(q80Latino, q80Intl) #Significant
# Latino vs Minority
t.test(q80Latino, q80Min) #Significant
# Latino vs White
t.test(q80Latino, q80Min) #significant
#Min vs White 
t.test(q80Min, q80White) # not Significant


### Q85
q85Black = climate.clean %>% filter(Ethnicity == "African American/Black") %>% select(contains("Q85")) %>% rowSums()
q85Asian = climate.clean %>% filter(Ethnicity == "Asian/Asian Am" ) %>% select(contains("Q85")) %>% rowSums()
q85Latino = climate.clean %>% filter(Ethnicity == "Hispanic/Latino") %>% select(contains("Q85")) %>% rowSums()
q85Intl = climate.clean %>% filter(Ethnicity == "International") %>% select(contains("Q85")) %>% rowSums()
q85Min = climate.clean %>% filter(Ethnicity == "Minority, Multi-Min., URM") %>% select(contains("Q85")) %>% rowSums()
q85White = climate.clean %>% filter(Ethnicity == "White") %>% select(contains("Q85")) %>% rowSums()

## Black vs Asian
t.test(q85Black, q85Asian) # Significant
## Black vs Latino 
t.test(q85Black, q85Latino) # Significant
## Black vs Intl
t.test(q85Black, q85Intl) # Significant
#Black vs Minority
t.test(q85Black, q85Min) #significant
# Black vs white
t.test(q85Black, q85White) #Significant
# Asian vs Latino
t.test(q85Asian, q85Latino) # Significant
# Asian vs Intnl
t.test(q85Asian, q85Intl) #Significant
#Asian vs Minority
t.test(q85Asian, q85Min) #Significant
# Asian vs white
t.test(q85Asian, q85White) #Significant
# Latino vs Intl
t.test(q85Latino, q85Intl) #Significant
# Latino vs Minority
t.test(q85Latino, q85Min) #Significant
# Latino vs White
t.test(q85Latino, q85Min) #significant
#Min vs White 
t.test(q85Min, q85White) #Significant

#### North Campus vs South Campus ####
q10NC = climate.clean %>% filter(NorthCampus == "yes") %>% select(contains("Q10")) %>% rowSums()
q10SC = climate.clean %>% filter(NorthCampus == "no") %>% select(contains("Q10")) %>% rowSums()

t.test(q10NC, q10SC) #Significant

### Q77
q77NC = climate.clean %>% filter(NorthCampus == "yes") %>% select(contains("Q77")) %>% rowSums()
q77SC = climate.clean %>% filter(NorthCampus == "no") %>% select(contains("Q77")) %>% rowSums()

t.test(q77NC, q77SC) #Not Significant


### Q78
q78NC = climate.clean %>% filter(NorthCampus == "yes") %>% select(contains("Q78")) %>% rowSums()
q78SC = climate.clean %>% filter(NorthCampus == "no") %>% select(contains("Q78")) %>% rowSums()

t.test(q78NC, q78SC) #Significant

#### Low Income vs High Income ####

### Q10
q10LowInc =q78NC = climate.clean %>% filter(LowFamilyIncomeIndicator == "Low-Income") %>% select(contains("Q10")) %>% rowSums()
q10HiInc = climate.clean %>% filter(LowFamilyIncomeIndicator == "Not Low Income") %>% select(contains("Q10")) %>% rowSums()

t.test(q10LowInc, q10HiInc) #Significant

### Q12_A_30
q12LowInc =q78NC = climate.clean %>% filter(LowFamilyIncomeIndicator == "Low-Income") %>% select(contains("Q12_A_30")) %>% rowSums()
q12HiInc = climate.clean %>% filter(LowFamilyIncomeIndicator == "Not Low Income") %>% select(contains("Q12_A_30")) %>% rowSums()

t.test(q12LowInc, q12HiInc) #not Significant p-val = 0.98

### Q75_A_16
q75_16LowInc =q78NC = climate.clean %>% filter(LowFamilyIncomeIndicator == "Low-Income") %>% select(contains("Q75_A_16")) %>% rowSums()
q75_16HiInc = climate.clean %>% filter(LowFamilyIncomeIndicator == "Not Low Income") %>% select(contains("Q75_A_16")) %>% rowSums()

t.test(q75_16LowInc, q75_16HiInc) #Significant

### Q75_A_17
q75_17LowInc =q78NC = climate.clean %>% filter(LowFamilyIncomeIndicator == "Low-Income") %>% select(contains("Q75_A_17")) %>% rowSums()
q75_17HiInc = climate.clean %>% filter(LowFamilyIncomeIndicator == "Not Low Income") %>% select(contains("Q75_A_17")) %>% rowSums()

t.test(q75_17LowInc, q75_17HiInc) #Significant

### Q76_A_6
q76LowInc = climate.clean %>% filter(LowFamilyIncomeIndicator == "Low-Income") %>% select(contains("Q76_A_6")) %>% rowSums()
q76HiInc = climate.clean %>% filter(LowFamilyIncomeIndicator == "Not Low Income") %>% select(contains("Q76_A_6")) %>% rowSums()

t.test(q76LowInc, q76HiInc) #Significant

### Q77_A_23
q77LowInc = climate.clean %>% filter(LowFamilyIncomeIndicator == "Low-Income") %>% select(contains("Q77_A_23")) %>% rowSums()
q77HiInc = climate.clean %>% filter(LowFamilyIncomeIndicator == "Not Low Income") %>% select(contains("Q77_A_23")) %>% rowSums()

t.test(q77LowInc, q77HiInc) #Significant

### Q80_A_26
q80LowInc = climate.clean %>% filter(LowFamilyIncomeIndicator == "Low-Income") %>% select(contains("Q80_A_26")) %>% rowSums()
q80HiInc = climate.clean %>% filter(LowFamilyIncomeIndicator == "Not Low Income") %>% select(contains("Q80_A_26")) %>% rowSums()

t.test(q80LowInc, q80HiInc) #Significant
       
       

###### Disability Testing -  Mainly Q82. How accessible is our campus) ###########
```{r}

## Aggregate of accessibility scores across all locations + course instructions/materials 
q82_notdisabled <- climate.clean %>% filter(DisabilityIndicator== "No Disability") %>% select(contains("q82")) %>% na.omit() %>% rowSums()

q82_disabled <- climate.clean %>% filter(DisabilityIndicator== "Disability") %>% select(contains("q82")) %>% na.omit() %>% rowSums()

t.test(q82_notdisabled, q82_disabled) # Significant

## Classroom/Lab accessibility
q82_classlab_disabled <- climate.clean %>% filter(DisabilityIndicator== "Disability") %>% select(contains("q82")) %>% select(Q82_A_3) %>% na.omit() %>% unlist()

q82_classlab_notdisabled <- climate.clean %>% filter(DisabilityIndicator== "No Disability") %>% select(contains("q82")) %>% select(Q82_A_3) %>% na.omit() %>% unlist()

t.test(q82_classlab_disabled, q82_classlab_notdisabled) #Significant

## Course Instruction accessibility
q82_instruction_disabled <- climate.clean %>% filter(DisabilityIndicator== "Disability") %>% select(contains("q82")) %>% select(Q82_A_22) %>% na.omit() %>% unlist()

q82_instruction_notdisabled <- climate.clean %>% filter(DisabilityIndicator== "No Disability") %>% select(contains("q82")) %>% select(Q82_A_22) %>% na.omit() %>% unlist()

t.test(q82_instruction_disabled, q82_instruction_notdisabled) #Significant

## University Housing
q82_unihousing_disabled <- climate.clean %>% filter(DisabilityIndicator== "Disability") %>% select(contains("q82")) %>% select(Q82_A_4) %>% na.omit() %>% unlist()

q82_unihousing_notdisabled <- climate.clean %>% filter(DisabilityIndicator== "No Disability") %>% select(contains("q82")) %>% select(Q82_A_4) %>% na.omit() %>% unlist()

t.test(q82_unihousing_disabled, q82_unihousing_notdisabled) #Significant

## Restrooms
q82_restrooms_disabled <- climate.clean %>% filter(DisabilityIndicator== "Disability") %>% select(contains("q82")) %>% select(Q82_A_16) %>% na.omit() %>% unlist()

q82_restrooms_notdisabled <- climate.clean %>% filter(DisabilityIndicator== "No Disability") %>% select(contains("q82")) %>% select(Q82_A_16) %>% na.omit() %>% unlist()

t.test(q82_restrooms_disabled, q82_restrooms_notdisabled) #Significant

## Walkways and Pedestrian Paths
q82_walkways_disabled <- climate.clean %>% filter(DisabilityIndicator== "Disability") %>% select(contains("q82")) %>% select(Q82_A_18) %>% na.omit() %>% unlist()

q82_walkways_notdisabled <- climate.clean %>% filter(DisabilityIndicator== "No Disability") %>% select(contains("q82")) %>% select(Q82_A_18) %>% na.omit() %>% unlist()

t.test(q82_walkways_disabled, q82_walkways_notdisabled) #Significant

q75_walkways_disabled <- climate.clean %>% filter(DisabilityIndicator== "Disability") %>% select(contains("q82")) %>% select(Q82_A_18) %>% na.omit() %>% unlist()

q82_walkways_notdisabled <- climate.clean %>% filter(DisabilityIndicator== "No Disability") %>% select(contains("q82")) %>% select(Q82_A_18) %>% na.omit() %>% unlist()

t.test(q82_walkways_disabled, q82_walkways_notdisabled) #Significant


## Frequency count of where people are experiencing disability
q63_disability <- climate.clean %>% filter(DisabilityIndicator== "Disability") %>% select(contains("q63")) %>% colSums()

barplot(q63_disability, las = 3) 

```


###### LGBTQ Testing  ###########
```{r}

## Aggregate of q10 across sexual orientation ##

q10_Hetero = climate.clean %>% filter(orientation == "Heterosexual") %>% select(contains("Q10")) %>% rowSums()

q10_LGBQ = climate.clean %>% filter(orientation == "LGBQ or Other") %>% select(contains("Q10")) %>% rowSums()

t.test(q10_Hetero, q10_LGBQ) # Significant 


##  q80_A_25 - Residence Halls Tension with Regard to Person's Sexual Orientation  ##

Q80_A_25_LGBQ <-  climate.clean %>% filter(orientation == "LGBQ or Other") %>% select(Q80_A_25) %>% na.omit() %>% unlist() 

Q80_A_25_hetero <- climate.clean %>% filter(orientation == "Heterosexual") %>% select(Q80_A_25) %>% na.omit() %>% unlist() 

t.test(Q80_A_25_hetero, Q80_A_25_LGBQ) # Significant 

##  q77_A_22 - Classroom Welcoming Based on Sexual Orientation  ##

Q77_A_22_LGBQ <-  climate.clean %>% filter(orientation == "LGBQ or Other") %>% select(Q77_A_22) %>% na.omit() %>% unlist() 

Q77_A_22_hetero <- climate.clean %>% filter(orientation == "Heterosexual") %>% select(Q77_A_22) %>% na.omit() %>% unlist() 

t.test(Q77_A_22_hetero, Q77_A_22_LGBQ) # Significant 


```
#### Transfer vs four year VISUALIZATION ####
```{r}
###Q10
q10Fy = climate.clean %>% filter(UGONLY == "fy") %>% select(contains("Q10")) %>% rowSums()
q10Transfer = climate.clean %>% filter(UGONLY == "transfer") %>% select(contains("Q10")) %>% rowSums()
### Left vs right
t.test(q10Fy, q10Transfer) #significant

###Q78
q78Fy = climate.clean %>% filter(UGONLY == "fy") %>% select(contains("Q78")) %>% rowSums()
q78Transfer = climate.clean %>% filter(UGONLY == "transfer") %>% select(contains("Q78")) %>% rowSums()
### Left vs right
t.test(q78Fy, q78Transfer) #not significant

###Q80
q80Fy = climate.clean %>% filter(UGONLY == "fy") %>% select(contains("Q80")) 
q80Transfer = climate.clean %>% filter(UGONLY == "transfer") %>% select(contains("Q80")) 
### Left vs right
t.test(q80Fy, q80Transfer) #significant

summary_fy <- apply(q80Fy,2,table)
sum(summary_fy[1,]) / sum(sum(summary_fy[2,]),sum(summary_fy[1,]))
sum(summary_fy[2,]) /sum(sum(summary_fy[2,]),sum(summary_fy[1,]))

summary_fy_prop <- sort(unlist(summary_fy[2,]/sum(summary_fy[2,])))*100

summary_transfers <- apply(q80Transfer,2,table)
sum(summary_transfers[1,]) / sum(sum(summary_transfers[2,]),sum(summary_transfers[1,]))
sum(summary_transfers[2,]) /sum(sum(summary_transfers[2,]),sum(summary_transfers[1,]))

max(summary_transfers[,2])

summary_fy_transfer <- unlist(summary_transfers[2,]/sum(summary_transfers[2,]))*100

library(ggplot2)

barplot(summary_fy[2,])
barplot(transfers)

summary_fy[2,]/ summary_transfers[2]
data.frame(fy=summary_fy[2,], ty = summary_transfers[2])

question <- row.names(cbind(summary_fy_prop,summary_fy_transfer))
side.bar = data.frame(rep(question,2),
           c(rep("Non-Transfer",26),rep("Transfer",26)),
           c(summary_fy_prop,summary_fy_transfer))

colnames(side.bar) <- c("Question", "Student Type", "Proportion")
str(side.bar)

ggplot(side.bar,aes(fill=`Student Type`,y=Proportion, x= Question))+ geom_bar(stat = "identity",position = "dodge")+theme(axis.text.x = element_text(angle=90,hjust = 1))




```


#### Transfer vs four year Visualization ####

```{r}

summary_fy <- apply(q80Fy,2,table)
sum(summary_fy[1,]) / sum(sum(summary_fy[2,]),sum(summary_fy[1,]))
sum(summary_fy[2,]) /sum(sum(summary_fy[2,]),sum(summary_fy[1,]))

summary_fy_prop <- sort(unlist(summary_fy[2,]/sum(summary_fy[2,])))*100

summary_transfers <- apply(q80Transfer,2,table)
sum(summary_transfers[1,]) / sum(sum(summary_transfers[2,]),sum(summary_transfers[1,]))
sum(summary_transfers[2,]) /sum(sum(summary_transfers[2,]),sum(summary_transfers[1,]))

max(summary_transfers[,2])

summary_fy_transfer <- unlist(summary_transfers[2,]/sum(summary_transfers[2,]))*100

library(ggplot2)

barplot(summary_fy[2,])
barplot(transfers)

summary_fy[2,]/ summary_transfers[2]
data.frame(fy=summary_fy[2,], ty = summary_transfers[2])

question <- row.names(cbind(summary_fy_prop,summary_fy_transfer))
side.bar = data.frame(rep(question,2),
           c(rep("Non-Transfer",26),rep("Transfer",26)),
           c(summary_fy_prop,summary_fy_transfer))

colnames(side.bar) <- c("Question", "Student Type", "Proportion")
str(side.bar)

ggplot(side.bar,aes(fill=`Student Type`,y=Proportion, x= Question))+ geom_bar(stat = "identity",position = "dodge")+theme(axis.text.x = element_text(angle=90,hjust = 1))

## Selecting only the ones that have the greatest difference ##
side.bar.subset = side.bar[which(side.bar$Question=="Q80_A_5"|side.bar$Question=="Q80_A_14"|side.bar$Question=="Q80_A_12"|side.bar$Question=="Q80_A_3"|side.bar$Question=="Q80_A_6" ), ]

side.bar.subset$Question <- recode(side.bar.subset$Question, Q80_A_5 = "English proficiency",Q80_A_14 = "Military/veteran status",Q80_A_12="Marital status",Q80_A_3="Country of origin",Q80_A_6= "Ethnicity",Q80_A_10="International Status ",Q80_A_22= "Political views")


ggplot(side.bar.subset,aes(fill=`Student Type`,y=Proportion, x= Question))+ geom_bar(stat = "identity",position = "dodge", color="Black")+scale_fill_manual(values=c("mediumblue", "lightblue"))+ theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(color="Black", size=16, face="bold.italic"),axis.text=element_text(size=12), axis.title=element_text(size=14))+ylab("Percentage of Responses")+xlab(" Demographic Factor") + labs(title = "Factors Influencing Perceived Residence Hall Tension") 


```

## Disability and accessibility plots ##

```{r}
q82_disabled <- climate.clean %>% filter(DisabilityIndicator== "Disability") %>% na.omit()

summary_dis <- apply(disabled,2,table)[c(2,4), -c(13:15)]

as.vector(summary_dis[1,] / apply(summary_dis,2,sum))

row.names(summary_dis) = c("Fully Accessible", "Accesible with Accomodations")

colnames(summary_dis)<- c("Athletic Facilities","Off-Campus UCLA Buildings",
                          "Off-Campus Student Housing","On-Campus Transportation/Parking", "Other Campus Buildings", "Recreational Facilities" ,"Restrooms"
,"Studios/Performing Arts Spaces"
,"Walkways and Pedestrian Paths"
,"Braille Signage"
, "Hearing Loops","Classroom Buildings","Classrooms"
,"University Housing"
,"Computer Labs"
,"Dining Facilities"
,"Elevators"
,"Health and Wellness Centers"
,"Library")

access.df <- data.frame(
              rbind(
                cbind(rep(c("Fully Accessible"),19), colnames(summary_dis)) , 
                cbind(rep(c("Accessible with Accomodations"),19), 
                              colnames(summary_dis))
                    )
                )

count <- c(as.vector(summary_dis[1,]),as.vector(summary_dis[2,]))

access.dis.df = data.frame(access.df,count) 

colnames(access.dis.df) <- c("Accessibility", "Location","Number of Responses")

ggplot(data=access.dis.df, aes(x=Location, y=`Number of Responses`, fill=Accessibility)) +
  geom_bar(stat="identity",color = "Black") + 
  scale_fill_brewer(palette="Paired")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1),plot.title = element_text(color="Black", size=14, face="bold.italic")) +   ggtitle("Accessibility by Location for Students with Disabilities")

prop = as.vector(summary_dis[1,]) / as.vector(apply(summary_dis,2,sum))*100
colnames(prop) <-  c("Athletic Facilities","Off-Campus UCLA Buildings",
                     "Off-Campus Student Housing","On-Campus 
                     Transportation/Parking", "Other Campus Buildings", 
                     "Recreational Facilities" ,"Restrooms",
                     "Studios/Performing Arts Spaces", 
                     "Walkways and Pedestrian Paths","Braille Signage", 
                     "Hearing Loops","Classroom Buildings","Classrooms",
                     "University Housing","Computer Labs","Dining Facilities",
                     "Elevators","Health and Wellness Centers","Library")


summary_inaccess <- data.frame(apply(disabled,2,table)[c(3), -c(13:15)], Location)

Location <- c("Athletic Facilities","Off-Campus UCLA Buildings",
                     "Off-Campus Student Housing","On-Campus 
                     Transportation/Parking", "Other Campus Buildings", 
                     "Recreational Facilities" ,"Restrooms",
                     "Studios/Performing Arts Spaces", 
                     "Walkways and Pedestrian Paths","Braille Signage", 
                     "Hearing Loops","Classroom Buildings","Classrooms",
                     "University Housing","Computer Labs","Dining Facilities",
                     "Elevators","Health and Wellness Centers","Library")

colnames(summary_inaccess)[1] = "Number of Responses"
summary_inaccess

ggplot(data=summary_inaccess, aes(x=Location, y=`Number of Responses`)) +
  geom_bar(stat="identity",color= "Black", fill = "lightblue") + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1),plot.title = element_text(color="Black", size=14, face="bold.italic")) +   ggtitle("Inaccessibility of Locations for Students with Disabilities")
```


```{r}
q80

is.na(q82) <- 0

sum(q80=="NA",na.rm=TRUE)

table(climate$Q80_A_14)

dim(climate.clean)
dim(climate)
```


```{r}

q76LowInc = climate.clean %>% filter(LowFamilyIncomeIndicator == "Low-Income") %>% select(contains("Q76_A_6")) %>% table() %>% as.vector
q76HiInc = climate.clean %>% filter(LowFamilyIncomeIndicator == "Not Low Income") %>% select(contains("Q76_A_6"))%>% table() %>% as.vector

c(q76LowInc,q76HiInc)


```

