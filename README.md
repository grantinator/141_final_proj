# 141_final_proj


## Questions to answer: 

1. What variables are most related to someone considering leaving UCLA (leaveUCLA var)
2. Ideal student profile using GPA as metric
3. Which student profiles feel most at home at UCLA
4. Which variables contribute the most to academic success at UCLA
5. Can we build a model to predict a student’s estimated success at UCLA


## Things to do 

### Cleaning up Data : 
1. Consider flagging and dropping entries that seem to be systematically inaccurate responses by student - like all 1's, all 2's. 
gpa : ordinal regression - rank categorize, or just dichotomize. recasting optimization/ rank optimization 

chi-square/goodness of fit : when you do  a 4 way. when you hsave 2-3 more significant 

2 strongsest predictors may be collinear. start doing efficient 

### Outcome Variables we are considering
1. GPA - clustered categorical is hard to model
2. Leaving - Y/N so we can run a logitisic for this 


## Conversation with Client: 

### What would be most useful for you to know :

1. What experiences help students feel like they are part of the community?  
2. If you can associate kinds of experiences that help successful students that would inform campus affairs. 
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
