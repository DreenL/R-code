library(tidyverse)
library(ggplot2)
library(dplyr)
library(rio)

# Data cleaning
Data_Health<-read.csv("healthcare.csv")
glimpse(Data_Health)
Data_Health[Data_Health == "N/A"]  <- NA
Data_Health <- Data_Health%>%drop_na()
View(Data_Health)

Data_Health <- Data_Health %>%
  mutate(gender = as.factor(gender),
         ever_married = as.factor(ever_married),
         work_type = as.factor(work_type),
         Residence_type = as.factor(Residence_type),
         smoking_status = as.factor(smoking_status),
         stroke = as.factor(stroke),
         
         age=as.numeric(age),
         avg_glucose_level = as.numeric(avg_glucose_level),
         bmi = as.numeric(bmi))

# Question 1 

#1st variable -- BMI(numeric)
Numeric_BMI <- Data_Health %>%
                  summarise(mean_BMI = mean(bmi, na.rm = TRUE),
                  sd_BMI = sd(bmi, na.rm = TRUE))
Numeric_BMI
#histogram
Graph_BMI <- ggplot(Data_Health, aes(x=bmi)) + 
                geom_histogram(binwidth=7,fill="lightyellow",linetype="solid",color="black",linewidth=1)+
                geom_vline(aes(xintercept=mean(bmi)),
                           color="red",linetype="dashed",  linewidth=1.5)+
                labs(x="BMI",
                     y="Count", 
                     title="Distribution of BMI")+  
                theme_bw()+
                theme(plot.title = element_text(size=10),
                      axis.text.x= element_text(size=12),
                      axis.text.y= element_text(size=12),
                      axis.title=element_text(size=12))
Graph_BMI

#2nd variable -- avg_glucose_level(numeric)
Numeric_glucose <- Data_Health %>%
                        summarise(mean_Avg_glucose = mean(avg_glucose_level, na.rm = TRUE),
                        sd_Avg_glucose = sd(avg_glucose_level, na.rm = TRUE))
Numeric_glucose

#histogram
Graph_glucose <- ggplot(Data_Health, aes(x=avg_glucose_level)) + 
                          geom_histogram(binwidth=15,fill="lightgrey",linetype="solid",color="black",linewidth=1)+
                          geom_vline(aes(xintercept=mean(avg_glucose_level)),
                                     color="red",linetype="twodash",  linewidth=1.5)+
                          labs(x="Average of Glucose Level",
                               y="Count", 
                               title="Distribution of Average of Glucose Level")+  
                          theme_bw()+
                          theme(plot.title = element_text(size=10),
                                axis.text.x= element_text(size=12),
                                axis.text.y= element_text(size=12),
                                axis.title=element_text(size=12))
Graph_glucose

#3rd variable -- gender(factor)
Count_gender <- Data_Health %>% count(gender)
Count_gender

Graph_Gender <- ggplot(Data_Health, aes(gender))+
                  geom_bar(stat="count", width = 0.5)+
  
                  labs(x="Gender",
                       y="Count", 
                       title="Gender Count")+ 
                  theme_bw()+
                  theme(plot.title = element_text(size=10),
                        axis.text.x= element_text(size=12),
                        axis.text.y= element_text(size=12),
                        axis.title=element_text(size=12))

Graph_Gender

#4th variable -- work_type(factor)
Count_work_type <- Data_Health %>% group_by(gender) %>% count(work_type)
Count_work_type

#5th variable -- age(numeric)
Numeric_age <- Data_Health %>% summarise(max(age), min(age),
                          range = max(age, na.rm = TRUE) - min(age, na.rm=TRUE),
                          median = median(age, na.rm = TRUE))
Numeric_age

Data_Health$group_age <- cut(Data_Health$age, breaks = seq(0,82,by=41), right = TRUE)
Count_gender_by_age_range <- Data_Health %>% 
                                group_by(group_age) %>%
                                count(gender)
Count_gender_by_age_range

hist_age <- hist(Data_Health$age, breaks = 41, main = "age by break-41")
hist_age

# Question 2
Q2i <- ggplot(Data_Health, aes(x=age, y=bmi)) +
         geom_point(aes(color=gender)) +
         labs(y="bmi", x="age", 
              title="Health - Age vs BMI against Gender ")
Q2i

Q2ii <- ggplot(Data_Health, aes(x=age, color=gender, 
         fill = gender)) +
         geom_histogram(alpha=0.5, position="identity")
Q2ii

Q2iii <- ggplot(Data_Health, aes(x = gender, y = ever_married)) + 
          geom_bar(stat = "identity")

Q2iii

#A count variable and a factor
Q2iv <- ggplot(Data_Health, aes(x=gender)) +
        geom_histogram(color="black", fill="white")

Q2iv

# Question 3
# response variable -- BMI

fit1 <- glm(bmi ~ ever_married + gender, 
            family = gaussian, data = Data_Health)
summary(fit1)

fit2 <- glm(bmi ~ ever_married + stroke, 
            family = gaussian, data = Data_Health)
summary(fit2)




# Question 4
# response variable -- gender
install.packages("pROC")
library(pROC)

linmod1<-glm(gender ~ bmi + avg_glucose_level, family=binomial, data=Data_Health)
summary(linmod1)
linmod1


linmod2<-glm(gender ~ bmi + age, family=binomial, data=Data_Health)
summary(linmod2)
linmod2


# Question 5
library(cluster)
#Subset only numerical variables -- glucose and BMI
numHealth <- Data_Health %>% 
                select(avg_glucose_level,
                      bmi)
#Standardize variables before starting your analysis.
numHealth <- scale(numHealth)
#create a matrix of distance by the manhattan distance
distHealth <- dist(numHealth, method = "manhattan")
#use the hclust function to create the Dendrogram
dendroHealth <- hclust(distHealth, method = "complete")
plot(dendroHealth)

#Create two clusters using K-means
kclust2 <- kmeans(numHealth, centers = 2)
Data_Health$cluster_kmean2 <- as.factor(kclust2$cluster)
kclust2$centers

kclust2$tot.withinss

kclust1 <- kmeans(numHealth, centers = 1)
kclust1$tot.withinss

kclust3 <- kmeans(numHealth, centers = 3)
kclust3$tot.withinss

table(Data_Health$cluster_kmean2)




football<-football%>%mutate(league = as.factor(league))
football<-football%>%filter(!age==0)
plot(Data_Health$ever_married, Data_Health$bmi)
abline(25.2147, 5.6360, col = 2)
