#Name and TP Number
#Dipta Protim Guha
#TP063351

#import dataset
data = read.csv("C:\\Users\\aritr\\Downloads\\student.csv", header=TRUE)
data

#install packages
install.packages("tidyverse")
install.packages("gridExtra")

#load Packages
library(tidyverse)
library(gridExtra)

#view table form
View(data)

#view first 6 rows of dataset
head(data)

#view all the column names
names(data)

#count the number of column and rows
nrow(data)
ncol(data)

#summary of dataset
summary(data)

#remove the index column from the dataset
data$index = NULL
data

#analysis 3.1 which gender group has a better average final grade
#male
average_grade_male=select(data, sex, G3) %>% filter(sex == "M") %>% summarise(mean(G3))
average_grade_male

#female
average_grade_female=select(data, sex, G3) %>% filter(sex == "F") %>% summarise(mean(G3))
average_grade_female


#analysis 3.2 number of male and female students
count_male=filter(data, sex == "M")
nrow(count_male)


count_female=filter(data, sex == "F")
nrow(count_female)


#analysis 3.3 average of all 3 grade columns combine for male and female students
avaerage_grade_all_grades_male=sample_frac(data,1) %>% mutate(All_Grades_out_of_60=G1+G2+G3) %>% select(sex, All_Grades_out_of_60)%>% 
  filter(sex == "M") %>% summarise(mean(All_Grades_out_of_60))
avaerage_grade_all_grades_male

avaerage_grade_all_grades_female=sample_frac(data,1) %>% mutate(All_Grades_out_of_60=G1+G2+G3) %>% select(sex, All_Grades_out_of_60) %>% 
  filter(sex == "F") %>% summarise(mean(All_Grades_out_of_60))
avaerage_grade_all_grades_female


#3.4 analysis Which gender group scored the most number of distinctions?
distinction_male= filter(data, sex == "M", G3 >= 16) 
i=nrow(distinction_male)

distinction_female=filter(data, sex == "F", G3 >= 16)
q=nrow(distinction_female)

a=c(i,q)
b=c("male","female")
pie(a,b,radius = 1, main = "Most distinctions", col = c("red","blue"),clockwise = TRUE)


#4.1 Analysis 1: How many students are there in each school? 
count_student_GP= filter(data, school == "GP")
nrow(count_student_GP)

count_student_MS= filter(data, school == "MS")
nrow(count_student_MS)


#4.2 Analysis 2: Which school students have a better average grade.
data %>% select(school, G1, G2, G3) %>% mutate(all_grades=G1+G2+G3) %>% subset(school == "GP") %>% 
  summarise(mean(all_grades))

data %>% select(school, G1, G2, G3) %>% mutate(all_grades=G1+G2+G3) %>% subset(school == "MS") %>% 
  summarise(mean(all_grades))


table(data$school, data$G3)



#4.3 Analysis 3: Which school students have more failures in their final Grade? 
GP=data %>% filter(school == "GP", G3<10) %>% 
  ggplot(aes(x=G3))+geom_histogram(colour="black",aes(fill=..count..))+
  scale_fill_gradient("count", low = "green", high = "red")+
  ggtitle("Student from Gabriel Pereira with score less than 10")


MS=data %>% filter(school == "MS", G3<10) %>% 
  ggplot(aes(x=G3))+geom_histogram(colour="black",aes(fill=..count..))+
  scale_fill_gradient("count", low = "green", high = "red")+
  ggtitle("Student from Mausinho da Silveira with score less than 10")

grid.arrange(GP,MS)

#Number of students and fail/students percentage. 
GP_count=data %>% filter(school == "GP", G3 < 10)
MS_count=data %>% filter(school == "MS", G3 < 10)
nrow(GP_count)
nrow(MS_count)

GP_percent=246/749 
round(GP_percent,digits = 2)

MS_percent=55/173
round(MS_percent,digits = 2)




#4.4 Analysis 4: Which school students have more distinctions in their final Grade? 
data %>% filter(G3>=16) %>% ggplot(aes(x=school))+
  geom_bar(fill=c("red","blue"))+
  ggtitle("Number of students who achieved distinction from each school")

#Number of students and fail/students percentage. 
GP_count=data %>% filter(school == "GP", G3 >= 16)
MS_count=data %>% filter(school == "MS", G3 >= 16)
nrow(GP_count)
nrow(MS_count)

GP_percent=77/749 
round(GP_percent,digits = 2)

MS_percent=17/173
round(MS_percent,digits = 2)



#5.1 Analysis 1: Does weekend alcohol consumption affect students' final grade? 
data %>% ggplot(aes(x=Walc, y=G3))+
  geom_point(aes(color=Walc))+
  stat_smooth(method = lm)+
  labs(title = "Relationship between weekened alcahol consumption and students' final grade", 
       x= "weekend alcohol consumption (1 - very low to 5 - very high", y="Final Grade")


#5.2 Analysis 2: Does daily alcohol consumption affect students' final grade? 
data %>% ggplot(aes(x=Dalc, y=G3))+
  geom_point(aes(color=Dalc))+facet_wrap(~Dalc)+
  labs(title = "Relationship between daily alcahol consumption and students' final grade", 
       x= "daily alcohol consumption (1 - very low to 5 - very high", y="Final Grade")


#5.3 Analysis 3: Does workday alcohol consumption lead to health issues?
data %>% ggplot(aes(x=Dalc, y=health))+geom_point(aes(color=Dalc))+facet_wrap(~Dalc)+
  labs(title = "Relationship between daily alcohol consumption and student health", 
       x= "daily alchol consumtion (1 - very low to 5 - very high", 
       y="Student health status (numeric: from 1 - very bad to 5 - very good")



#5.4 Analysis 4: Can health lead to absences which can lead to a drop in grade? 
data %>% ggplot(aes(x=absences, y=G3))+geom_point(aes(shape=factor(Dalc),color=factor(Dalc)))+
  labs(title = "Relationship between absences and students' final grade", 
       x= "Absences", y="Final Grade")

                                                                         
#6.1 Analysis 1: Relationship between romance and students final Grade 
data %>% 
  ggplot(aes(romantic, G3))+
  geom_boxplot(aes(fill=romantic))+
  labs(title = "Relationship between romance and students final Grade ", 
       x= "Romatic involvement", y="Final Grade")


#6.2 Analysis 2: Does romance cause students to go out more and study less which can lead to drops in their final grades? 
romantic_table=data %>% select(romantic, goout, studytime, G3) %>% group_by(romantic) %>% 
  summarise(mean(goout), mean(studytime), mean(G3))

View(romantic_table)


#6.3 Analysis 3: Does romance causes more absences for students and does that affect their final grades? 
data %>% 
  ggplot(aes(romantic, absences))+
  geom_violin(aes(fill=romantic))+ stat_summary(fun=mean, geom="point", size=2, color="yellow")+
  labs(title = "Relationship between romance and absences ", 
       x= "Romatic involvement", y="Number of absences")

#7.1 Analysis 1: Does students' location from school affect their Final Grade? 
data %>% ggplot(aes(x=G3))+
  geom_density(aes(fill=address))+
  labs(title = "Density graph of relationship between students' final grade and address", 
       x= "Final Grade")

#7.2 Analysis 2: Does students' location from school affect their travel time? 
data %>% ggplot(aes(x=traveltime))+
  geom_density(aes(fill=address))+
  labs(title = "Density graph of relationship between students' travel time and address", 
       x= "Travel time")

#7.3 Analysis 3: Does students' travel time affect their final grade?
data %>% ggplot(aes(x=traveltime, y=G3))+geom_point(aes(color=traveltime))+
  stat_smooth(method = lm)+
  labs(title = "Relationship between travel time and students' final grade", 
       x= "Travel Time", y="Final Grade")
