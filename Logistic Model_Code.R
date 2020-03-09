
---
title: "Logistic Model_Code.R"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
library(readr)
setwd("~/Desktop/UCLA Related/UCLA Lectures/Stats 141SL/Final Project/CampusClimateProject-20200208")
climate = read_csv("campusclimate.csv")

q10 = climate[,grep("Q10", colnames(climate))]
summary(q10)
q10[q10 == 6] <- NA
summary(q10)
reverseScale = function(val) { 
  return(6 - val)    
}

q10.clean = data.frame(apply(q10[,-9], 2, reverseScale),q10$Q10_A_9)

summary(q10.clean)

q10.clean = data.frame(q10.clean, rowMeans(q10.clean, na.rm = T))
colnames(q10.clean)[10] <- "q10.academicSF"
```

```{r}
q75 = climate[,grep("Q75", colnames(climate))]
q75.adjust = apply(q75, 2, reverseScale)
q75.clean = data.frame(q75.adjust, rowMeans(q75.adjust, na.rm = T))
colnames(q75.clean)[19] <- "q75.friendly"
```

```{r}
q76 = climate[,grep("Q76", colnames(climate))]
q76.adjust = apply(q76, 2, reverseScale)
q76.clean = data.frame(q76.adjust, rowMeans(q76.adjust, na.rm = T))
colnames(q76.clean)[9] <- "q76.racism"
```

```{r}
famIncomeBinary = ifelse(climate$famincome %in% c("Below $10,000", "$20,000-$29,999", "$30,000 - $39,999", "$40,000 - $49,999", "$50,000 - $59,999", "$60,000- $69,999"), "LowIncome", "HighIncome")
unique(famIncomeBinary)
```


```{r}
scale.df = data.frame(q10.clean$q10.academicSF, q75.clean$q75.friendly, q76.clean$q76.racism)
colnames(scale.df) = c("q10","q75","q76")

scale.df = data.frame(scale.df, climate$leaveUCLA, climate$FIRSTGEN,  climate$DisabilityIndicator, famIncomeBinary, climate$NorthCampus, climate$orientation)

scale.complete = scale.df[complete.cases(scale.df),]
colnames(scale.complete)[4] <- "leaveUCLA"
colnames(scale.complete)[5] <- "FIRSTGEN"
colnames(scale.complete)[6] <- "DisabilityIndicator"
colnames(scale.complete)[7] <- "IncomeLevel"
colnames(scale.complete)[8] <- "NorthCampus"
colnames(scale.complete)[9] <- "orientation"

```

```{r}
# What proportion 
dim(scale.complete)[1]/dim(climate)[1]

mylogit <- glm(leaveUCLA ~ q10 + q75 + q76 + FIRSTGEN + DisabilityIndicator + IncomeLevel + NorthCampus , data = scale.complete, family = "binomial")

s <- summary(mylogit)

library(gridExtra)
library(grid)
library(xtable)

library(knitr)
library(kableExtra)
dt <- round(s$coefficients,2) %>% data.frame()

kable(dt) %>%
  kable_styling("striped", full_width = F) %>%
  row_spec(0, angle = -45)


```
