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

1. NC vs South Campus :  i) Aggregate of q10, ii) Aggregate of q77 (welcoming classrom), iii) aggregate q78 (classroom setting + valued/respected by peers) 

2. Liberal vs Conservative : i) Aggregate of q10, ii) q12_A_23 Welcoming for political, iii) q77_A_19 (classroom environment), iv) q78_A_ , v) q80 (reslife) , vi) q61 (Isolated or left out when work was required in groups)  

3. Low Income vs High Income 

