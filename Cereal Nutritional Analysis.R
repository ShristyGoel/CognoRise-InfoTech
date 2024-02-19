#Nutrition data on 80 Cereals product

"Fields in the dataset:
Name: Name of cereal
mfr: Manufacturer of cereal
A = American Home Food Products
G = General Mills
K = Kelloggs
N = Nabisco
P = Post
Q = Quaker Oats
R = Ralston Purina
type:cold and hot
calories: calories per serving
protein: grams of protein
fat: grams of fat
sodium: milligrams of sodium
fiber: grams of dietary fiber
carbo: grams of complex carbohydrates
sugars: grams of sugars
potass: milligrams of potassium
vitamins: vitamins and minerals - 0, 25, or 100, indicating the typical 
          percentage of FDA recommended
shelf: display shelf (1, 2, or 3, counting from the floor)
weight: weight in ounces of one serving
cups: number of cups in one serving
rating: a rating of the cereals (Possibly from Consumer Reports?)"

#Load necessary libraries
install.packages("dplyr")
library(vctrs)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(cluster)
library(corrplot)
library(gridExtra)
library(reshape2) 

#Loading the data set
cereal<-read.csv("C:/Users/hp/OneDrive/Desktop/cereal.csv")
cereal

#To view the data
View(cereal)

#Displaying the first few rows of the data set
head(cereal)

#Exploring the structure of the data
str(cereal)

#Checking for missing values
sum(is.na(cereal))

#Finding descriptive statistics of the data set
summary(cereal)

#Replacing -1 values with NA
cereal[cereal==-1]<-NA
cereal

#Imputing missing values with the median of their respective columns
cereal$carbo[is.na(cereal$carbo)] <- median(cereal$carbo)
cereal$sugars[is.na(cereal$sugars)] <- median(cereal$sugars)
cereal$potass[is.na(cereal$potass)] <- median(cereal$potass)
cereal

#Data Aggregation:Aggregating data to calculate summary statistics for different groups.
#Average fats by manufacturer
avg_fat_by_manufacturer <- tapply(cereal$fat, cereal$mfr, mean)
print(avg_fat_by_manufacturer)

#Average protein by manufacturer
avg_protein_by_manufacturer <- tapply(cereal$protein, cereal$mfr, mean)
print(avg_protein_by_manufacturer)

"It shows that the average protein for different groups is higher 
than that of average fat by manufacturer."

#Viewing summary of the data set after imputation
summary(cereal)

"Summary Interpretation:
Calories: Range from 50 to 160 per serving, with most around 110, 
indicating a moderate calorie range.
Protein: Generally between 1 and 6 grams, with an average slightly above 
2.5 grams, suggesting low protein content.
Fat: Ranges from 0 to 5 grams per serving, mostly around 1 gram,
indicating low fat content in most cereals.
Sodium: Wide range (0 to 320 mg per serving), with an average of 160 mg, 
suggesting varied sodium levels.
Fiber: Varies significantly (0 to 14 grams per serving), with an average 
of 2 grams, indicating varied fiber content.
Carbohydrates: Range from 5 to 23 grams, with most cereals having moderate
carbohydrate levels.
Sugars: Vary significantly from 0 to 15 grams per serving, with an average 
of 7 grams, indicating varied sweetness.
Potassium: Wide range (15 to 330 mg per serving), with some cereals being 
good potassium sources.
Vitamins: Vary widely, indicating a range of fortification levels,
with many cereals fortified.
Shelf: Ranges from 1 to 3, with no clear correlation to nutritional value.
Weight: Serving weight varies from 0.5 to 1.5, typically around 1,
standardizing serving sizes.
Cups: Recommended serving size varies, typically between 0.67 and 1 cup.
Rating: Wide range (18 to 94), with an average of 42.7, 
indicating diverse consumer preferences."

#Data filtering and subsetting
#Subsetting data for cereals with fiber content greater than 5
high_fiber_cereals <- cereal[cereal$fiber > 5, ]
high_fiber_cereals

#Visualizing the distribution of various nutrients
#Box plot of calories by cereal type
boxplot(calories ~ type, data = cereal, main = "Calories by Cereal Type")

#Box plot of protein by cereal type
boxplot(protein ~ type, data = cereal, main = "Protein by Cereal Type")

#Creating individual plots for each nutrient
#Histogram with a normal distribution curve for Calories
p_calories <- ggplot(cereal, aes(x=calories)) + 
  geom_histogram(aes(y=..density..), binwidth = 10, fill="seagreen", color="black") + 
stat_function(fun = dnorm, args = list(mean = mean(cereal$calories),
                               sd = sd(cereal$calories)),
              linewidth=1) + ggtitle("Calories Distribution")

#Histogram with a normal distribution curve for Proteins
p_protein <- ggplot(cereal, aes(x=protein)) + 
  geom_histogram(aes(y=..density..),binwidth =1,fill="lightblue",color="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(cereal$protein),
                         sd = sd(cereal$protein)),
                size = 1) + ggtitle("Protein Distribution")

#Histogram with a normal distribution curve for Fat
p_fat <- ggplot(cereal, aes(x=fat)) + 
  geom_histogram(aes(y=..density..),binwidth=1,fill="peachpuff",color="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(cereal$fat), 
            sd = sd(cereal$fat)), size = 1) + 
  ggtitle("Fat Distribution")

#Histogram with a normal distribution curve for Sodium
p_sodium <- ggplot(cereal, aes(x=sodium)) + 
  geom_histogram(aes(y=..density..),binwidth =30,fill="orange", color="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(cereal$sodium),
                      sd = sd(cereal$sodium)),
                size = 1) +  ggtitle("Sodium Distribution")

#Histogram with a normal distribution curve for Fiber
p_fiber <- ggplot(cereal, aes(x=fiber)) + 
  geom_histogram(aes(y=..density..),binwidth =1,fill="brown", color="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(cereal$fiber),
  sd = sd(cereal$fiber)), size = 1)+ 
  ggtitle("Fiber Distribution")

#Histogram with a normal distribution curve for Carbohydrates
p_carbo <- ggplot(cereal, aes(x=carbo)) + 
  geom_histogram(aes(y=..density..), binwidth =2,fill="purple", color="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(cereal$carbo),
  sd = sd(cereal$carbo)), size = 1)+ 
  ggtitle("Carbohydrates Distribution")

#Histogram with a normal distribution curve for Sugars
p_sugars <- ggplot(cereal, aes(x=sugars)) + 
  geom_histogram(aes(y=..density..),binwidth = 2,fill="pink", color="black") + 
  stat_function(fun = dnorm, args = list(mean =mean(cereal$sugars),
     sd = sd(cereal$sugars)), size = 1)+ 
  ggtitle("Sugars Distribution")

#Histogram with a normal distribution curve for Potassium
p_potass <- ggplot(cereal, aes(x=potass)) + 
  geom_histogram(aes(y=..density..),binwidth =30, fill="green",color="black") + 
  stat_function(fun = dnorm, args = list(mean =mean(cereal$potass),
      sd = sd(cereal$potass)), size = 1)+ 
  ggtitle("Potassium Distribution")

#Arranging the plots in a grid
grid.arrange(p_calories, p_protein, p_fat, p_sodium, p_fiber, p_carbo,
             p_sugars,p_potass, ncol=3)

"Interpretation: The visualizations indicate that calories, fat, and sugars
are generally lower in higher-rated cereals,while fibre and protein are higher. 
This suggests that healthier cereals tend to be rated more favourably."

#Correlation heat map
correlation_matrix <- cor(cereal %>% select(-name, -mfr, -type, -shelf,
                                            -weight,-cups),use="complete.obs") 
#Excluding non-numeric columns
corrplot(correlation_matrix, method="color")

"Interpretation: Strong negative correlations between rating and sugars/
sodium/calories suggest consumers prefer cereals lower in these attributes. 
Positive correlations between fiber/protein and rating indicate 
these are desirable attributes."

#Bar plots for average ratings by manufacturer
avg_rating_by_mfr <- cereal %>% group_by(mfr) %>% summarise(avg_rating =
                  mean(rating))
ggplot(avg_rating_by_mfr, aes(x=mfr, y=avg_rating))+geom_bar(stat="identity", 
      colour ="violet") + ggtitle("Average Cereal Rating by Manufacturer")

"Interpretation: The bar plot reveals that certain manufacturers 
consistently produce higher-rated cereals, possibly indicating a better
overall product quality or alignment with consumer preferences."

#Comparing cold and hot cereals
avg_nutrients_by_type <- cereal %>% group_by(type) %>% summarise(across(c
(calories, protein, fat, sodium, fiber, carbo, sugars, 
potass), mean))

melted_avg_nutrients_by_type <- melt(avg_nutrients_by_type, id.vars = "type")

ggplot(melted_avg_nutrients_by_type, aes(x=type, y=value, fill=variable)) + 
  geom_bar(stat="identity", position="dodge") + facet_wrap(~variable, scales=
   "free_y")+theme(axis.text.x = element_text(angle=90)) + labs(fill="Nutrient")

"Interpretation: Hot cereals are generally higher in fibre and protein and 
lower in sugar and sodium, which aligns with their higher average ratings."

#Scatter plots of nutrients vs ratings
p_fiber_rating <- ggplot(cereal, aes(x=fiber, y=rating)) + geom_point() +
  ggtitle("Fiber vs Rating")+ geom_smooth(method=lm, color="yellow", se = F)

p_protein_rating <- ggplot(cereal, aes(x=protein, y=rating)) + geom_point() + 
  ggtitle("Protein vs Rating") + geom_smooth(method=lm, color="coral", se = F)

p_sugars_rating <- ggplot(cereal, aes(x=sugars, y=rating)) + geom_point() +
  ggtitle("Sugars vs Rating")+ geom_smooth(method=lm, color="blue", se = F)

p_fat_rating <- ggplot(cereal, aes(x=fat, y=rating)) + geom_point() + 
  ggtitle("Fat vs Rating")+ geom_smooth(method=lm, color="turquoise", se = F)

#Displaying scatter plots
grid.arrange(p_fiber_rating, p_protein_rating, p_sugars_rating, p_fat_rating,
             ncol=2)

"Interpretation: Higher fiber and protein contents are positively associated 
with higher cereal ratings, suggesting a consumer preference for these
nutrients.Higher sugar and fat content is negatively associated with ratings,
indicating a preference for less sugary and fatty cereals."

#Regression Analysis:To explore relationships between variables.
#Example: Linear regression to predict calories based on protein and fiber
model <- lm(calories ~ protein + fiber, data = cereal)
summary(model)


