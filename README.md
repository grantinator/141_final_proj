# 141_final_project Instructions

## Questions to answer: 

1. What variables are most related to someone considering leaving UCLA (leaveUCLA var)
2. What are the attitudes towards certain topics of campus culture for groups defined by ethnicity, student type, and more
3. What contributes to students having high academic satisfaction
4. What can UCLA do to improve student satisfaction and campus culture as well as prevent students from leaving
5. Can we build a model to predict whether or not a student will consider leaving UCLA


## Things to do 

### Cleaning up Data : 
1. Recoding subquestions in a question to ordinal or binary categorical variables
2. Recode NAs that represent "no answer" as a level in the factor
3. Reshape data by groups for subsetted analysis

gpa : ordinal regression - rank categorize, or just dichotomize. recasting optimization/ rank optimization 

chi-square/goodness of fit : when you do  a 4 way. when you hsave 2-3 more significant 

2 strongsest predictors may be collinear. start doing efficient 

### Outcome Variables we are considering
1. GPA - Possible ordinal regression
2. Leaving - Y/N so we can run a logitisic for this 


## Conversation with Client: 

### What would be most useful for you to know :

1. The goal is to be able to identify actionable problems that the college can solve to help improve student lives. If you can associate kinds of experiences that help successful students that would inform campus affairs. What experiences help students feel like they are part of the community?
3. Sense of belonging. Did you experience exclusionary behaviour. Model to predict students leaving is an important outcome. 
4. Students who have discomfort in classes/department and what predicts a student in that class. 
5. Look at what kinds of experiences seem to enhance or depress some of your outcome variables. What seems to be making a difference for everybody? 

### Data Collection/Source: 

Data was collected over time. Survey in 2013. But then data was collected over time and the same questions were not necessarily asked again, that's why you probably see a lot of NA’s. Therefore, if you take 1000 cases it should be significant. 

### Which questions were mandatory? 

How is the data aggregated? Students worked on it. Not native to school. 

### What are we thinking of doing? 

1. We are thinking of finding best predictors of Academic satisfaction/High GPA - downside that students in a certain group. Maybe we should focus on different kinds of experiences they need.  
2. Some sort of predictive model that looks at what kinds of experiences that enhance or depress some of your outcome variables. What seems to be making a difference for everybody? 


## Classification of Variables we will be using 

### 1. Agree-Disagree Scale

Q10 (Academic Satisfaction) 

Q75 (Climate at UCLA is friendly) 

Q76 (Climate at UCLA is prejudiced) 

Q77 (Welcome at UCLA)

Q78 (I feel valued/respected by peers/faculty)

Q90 (Various measures that would improve the campus climate at UCLA) ** USEFUL FOR RECOMMENDATIONS

### 2. Mark All That Apply Scale [We will run analysis on this - like frequency plots] 
Q61 (what forms of exlusionary behavior are you experiencing)

Q63 (Locations of exlusionary behavior)

### 3. Ones we need to combine levels and convert to a binary
Q12 (Personally experienced exlusionary behavior) 

Q80 (I Perceive Tension in REsidence Hall)

Q82 (How accessible are various locations)

Q84 (How respectful climate if for various groups)

Q85 (Respectful climate for different ethnic groups)

Q86 (Before enrollment) 

Q87 (Courses have included sufficient materials, perspectives)

### Repeated Measures 
Q86 and Q84

### Content to use in Final Report 
Insights from report : http://campusclimate.ucop.edu/_common/files/pdf-climate/ucla-full-report.pdf?fbclid=IwAR0a6NJ6MIGUqwC4p-iB1a4sgmtpuxS46i_NtPY6eL-PwrcuJ4zJxaCASjc
Climate, for the purposes of this project is considered “the current attitudes, behaviors, and standards of faculty, staff, administrators and students concerning the level of respect for individual needs, abilities, and potential”  (pg 16) 


## Groups and Problems to answer

1. NC vs South Campus (Done) :  
    i) Aggregate of q10 - Academic Satisfaction (Aggregate)
    ii) Aggregate of q77 - Classroom Environment Welcoming Rating (Aggregate)
    iii) Aggregate q78 - Classroom Academic and Faculty Satisfaction (Aggregate)

2. Liberal vs Conservative : (Not enough observations for conservative)
    i) Aggregate of q10 - Academic Satisfaction (Aggregate)
    ii) q12_A_23 -  Experienced Exclusion Based on Political Views (Binary Count)
    iii) q77_A_19 - Classroom Environment Welcoming Rating Based on Political Views     
                    (Binary Count)
    iv) q78_A_7 - Encourages Free and Open Discussion of Difficult Topics (Binary Count)
    v) q80_A_22 - Residence Halls Tension With Regard to Person's Political View (Binary   
                  Count)
 
3. Low Income vs High Income :
    i) Aggregate of q10 - Academic Satisfaction (Aggregate)
    ii) q12_A_30 - Experienced Exclusion Based on Socioeconomic Status (Binary Count)
    iii) q75_A_16 - Friendliness of Climate for People of High Socioeconomic Status 
         (1-5 scale)
    iv) q75_A_17 - Friendliness of Climate for People of Low Socioeconomic Status 
        (1-5 scale)
    v) q76_A_6 - Overall Climate Regarding Classism Socioeconomic Status (1-5 Scale)
    vi) q77_A_23 - Classroom Environment Welcoming Rating for Socioeconomic Status 
        (Binary Count)
    vii) Q80_A_26 - Residence Halls Tension With Regard to Person's Socioeconomic Status            (Binary Count)

4. International vs Non-International (Kind of done with ethnicity)
    i) Aggregate of q10 - Academic Satisfaction (Aggregate)
    ii) q12_A_13 - Experienced Exclusion Based on International Status (Binary Count)
    iii) q75_A_1 - Overall Friendliness of Climate (1-5 Scale)
    iv) q75_A_13 - Friendliness of Climate for Non-U.S. Citizens(1-5 scale)
    v) q77_A_9 - Classroom Welcoming based on International Status (Binary Count)
    vi) q80_A_19 - Residence Halls Tension With Regard to Person's International Status                      (Binary Count)

5. Transfer Students vs Non-Transfer Students (Done)
    i) Aggregate of q10 - Academic Satisfaction (Aggregate)
    ii) Aggregate of q78 - Classroom Academic and Faculty Satisfaction (Aggregate)
    iii) Aggregate of q80 - Residence Hall Tension (Aggregate)
    
6. Male vs Female (Done)
    i) Aggregate of q10 - Academic Satisfaction (Aggregate)
    ii) q12_A_10 -  Experienced Exclusion Based on Gender Identity (Binary Count)
    iii) q75_A_9 - Friendliness of Climate for Men (1-5 Scale)
    iv) q75_A_10 - Friendliness of Climate for Women (1-5 Scale)
    v) q76_A_2 - Overall Climate Regarding Sexism (1-5 Scale)

7. Ethnicity - AfrAm, HispLat, AsiAm, White (Done)
    i) Aggregate of q10 - Academic Satisfaction (Aggregate)
    ii) q12_A_9 - Experienced Exclusion Based on Ethnicity (Binary Count)
    ii) q76_A_1 - Overall Climate Regarding Racism (1-5 Scale)
    iii) q80_A_6 - Residence Halls Tension with Regard to Person's Ethnicity 
                   (Binary Count)
    iii) Corresponding q85 - Respect Towards Racial Ethnic Backgrounds (Binary Count)
    
8. Disabilty
    i) Aggregate of q10 - Academic Satisfaction (Aggregate)
    ii) q12_A_21 - Experienced Exclusion Based on Physical Disability (Binary Count)
    iii) q75_A_3 - Friendliness of Climate for Persons with Disabilities (1-5 Scale)
    iv) q76_A_8 - Overall Climate Towards Disability (1-5 Scale)
    v) q77_A_10 - Classroom Welcoming Rating for Learning Disabilities (Binary Count)
    vi) q77_A_18 - Classroom Welcoming Rating for Physical Disabilities (Binary Count) 
    vii) q80_A_11 - Residence Halls Tension with Regard to Person's Learning Disability
                   (Binary Count)
    viii) q80_A_21 - Residence Halls Tension with Regard to Person's Physical Disability
                    (Binary Count)      
    ix) q82 - Location Accesibility (Binary Count)
    
 9. LGBTQ+
     i) Aggregate of q10 - Academic Satisfaction (Aggregate)
     ii) q12_A_11 - Experienced Exclusion Based on Gender Expression (Binary Count)
     iii) q12_A_29 - Experienced Exclusion Based on Sexual Orientation (Binary Count)
     iv) q75_A_4 - Friendliness of Climate for LGB People (1-5 Scale)
     v) q75_A_18 - Friendliness of Climate for Transgender People (1-5 Scale)
     vi) q76_A_3 - Overall Climate Regarding Homophobia (1-5 Scale)
     vii) q76_A_4 - Overall Climate Regarding Transphobia (1-5 Scale)
     viii) q77_A_7 - Classroom Welcoming Based on Gender Expression (Binary Count)
     ix) q77_A_22 - Classroom Welcoming Based on Sexual Orientation (Binary Count)
     x) q80_A_8 - Residence Halls Tension with Regard to Person's Gender Expression
                  (Binary Count) 
     xi) q80_A_25 - Residence Halls Tension with Regard to Person's Sexual Orientation
                    (Binary Count)              
                  
     
     



