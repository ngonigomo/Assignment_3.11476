installed.packages("devtools")
library(readxl)
mytitanic =read_excel("C:/Users/eugine.dube/Documents/Academics/DataScience/R_Assignment_Files/titanic3.xls")
#a. Preprocess the passenger names to come up with a list of titles thatrepresent families
mytitanic$tittle <-substring(mytitanic$name,regexpr(",",mytitanic$name)+2,regexpr("\\.",mytitanic$name)-1)



library(dplyr)

#Processing tittles 
mytitanic[mytitanic$tittle %in% c('Mme'),'tittle'] ='Mrs'
mytitanic[mytitanic$tittle %in% c('Sir'),'tittle'] ='Mr'
mytitanic[mytitanic$tittle %in% c('Ms','Mlle'),'tittle'] ='Miss'
mytitanic[mytitanic$tittle %in% c('Lady','Major','Don','Dona','Capt','Col','Jonkheer',"the Countess"),'tittle'] ='Others'

#represent using appropriate visualization graph.
library(ggplot2)
table(mytitanic$tittle)
ggplot(mytitanic,aes(x= mytitanic$tittle)) + 
  geom_bar(stat = 'count')  +   labs(x = 'Tittle')  + labs(y ='Tittle Counts')  
  



#b. Represent the proportion of people survived from the family size using a graph.
mytitanic$familysize <-mytitanic$sibsp + mytitanic$parch + 1
ggplot(mytitanic,aes(x= mytitanic$familysize,  fill = factor(mytitanic$survived ))) + 
  geom_bar(stat = 'count')  +   labs(x = 'Family Size')  + labs(y ='Survived') 





#Impute the missing values in Age variable using Mice Library, create two
#different graphs showing Age distribution before and after imputation.
install.packages("mice")
#library(mice)
set.seed(8)
computed_df=mytitanic[, names(mytitanic) %in% c('age','sibsp','parch','fare','embarked')] 
ageimputed = mice(computed_df, method = "rf", m=5)
imputedage = complete(ageimputed)
par(mfrow=c(1,2))
hist(mytitanic$age, main = "Before Imputation", col = "red")
hist(imputedage$age, main = "After Imputation", col = "green")
