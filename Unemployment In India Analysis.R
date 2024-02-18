#Unemployment in India Analysis

"The data set tells how lock-down affected employment opportunities and
how the unemployment rate increased during the Covid-19."
"The dataset contains monthly unemployment data of all the states in India
spanning from 31st May, 2019 to 30th June, 2020. 
"
"Data set Overview: It explains the unemployment and employment rate in
percentage for different states in India during the pandemic peroid.
States: States constituting the Indian sub-continent.
Date: The specific dates of unemployment rate recordings.
Measuring Frequency: The regularity of measurement collection (monthly).
Estimated Unemployment Rate (%): The proportion of unemployed individuals
in each Indian state.
Estimated Employed Individuals: The tally of presently engaged individuals.
Estimated Labour Participation Rate (%): The percentage of the working-age
populaion (16-64 years) actively involved in the job market, 
including both employed individuals and those actively seeking jobs."

#Loading the data from the excel file
data<-read.csv("C:/Users/hp/OneDrive/Desktop/Unemployment in India.csv")

#Viewing the data 
View(data)

#Installing and loading dplyr package to perform data manipulation operations
install.packages("dplyr")
library(dplyr)

#Cleaning the data
#Renaming the column names
colnames(data)[4]="Estimated Unemployment Rate"
colnames(data)[6]="Estimated Labour Participation Rate"

#Removing duplicate rows to ensure accuracy of the analysis
data<-unique(data)

#Removing rows with all NAs
data<-na.omit(data)

#Finding the summary statistics of the unemployment rate
summary(data$`Estimated Unemployment Rate`)
sd(data$`Estimated Unemployment Rate`)

"Finding:The unemployment rate is ranging from 0% to 76.74%.
The sd is quite high at 10.72%,indicating significant variability.
50% of the data (the median) falls below 8.35%, which suggests that the 
distribution might be right-skewed with some very high unemployment rates
pulling the mean upwards."

#Finding the summary statistics of the employment status
summary(data$Estimated.Employed)
sd(data$Estimated.Employed)

"The number of employed individuals also varies widely across the dataset.
The standard deviation is nearly as large as the mean, indicating a 
substantial spread in the number of employed people across different
regions or times.
The median is less than half of the maximum value, again suggesting a 
right-skewed distribution."

#Finding the summary statistics of the labour force participation rate
summary(data$`Estimated Labour Participation Rate`)
sd(data$`Estimated Labour Participation Rate`)

"The labor force participation rate has a mean of 42.63% and a standard
deviation of 8.11%, suggesting moderate variability.
The median is very close to the mean, which indicates a symmetric
distribution of labor participation rates."

"Visualizing these distributions and looking at how the unemployment
rate changed over time, especially in light of the COVID-19 pandemic,
as well as differences between regions."

#Installing and loading ggplot2 package
install.packages("ggplot2")
library(ggplot2)

#Histogram of estimated unemployment rate
ggplot(data, aes(x = `Estimated Unemployment Rate`)) +
  geom_histogram(binwidth = 2, fill = "pink",colour="black", alpha = 2) + 
labs(title = "Estimated Unemployment Rate (%)", x = "Unemployment Rate (%)",
     y = "Frequency")

"The distribution shows a peak at lower percentages, indicating that most 
data points have a relatively low unemployment rate.
There is a long tail toward higher percentages, confirming the right-skewness 
suggested by the summary statistics.
The presence of very high unemployment rates indicates Covid-19 period."

ggplot(data, aes(x = `Estimated.Employed`)) +
  geom_histogram(fill = "green",colour="black", alpha = 2) + 
  labs(title = "Distribution of Estimated Employed", x = "Employed",
       y = "Frequency")

"There is a long tail toward higher percentages, confirming the 
right-skewness suggested by the summary statistics.
The presence of very high unemployment rates indicates Covid-19 period."

ggplot(data, aes(x = `Estimated Labour Participation Rate`)) +
  geom_histogram(binwidth = 3, fill = "red",colour = "black", alpha = 0.7) + 
  labs(title = "Distribution of Estimated Labour Participation Rate(%) ", 
       x = "Labour Participation Rate(%)", y = "Frequency")

"The distribution appears more symmetric with a clear central peak and 
tails on both sides.This suggests that the labor participation rate varies
less dramatically across different regions or over time than the unemployment."

#Scatter plot for unemployment rate over time
ggplot(data,aes(x = Date, y = `Estimated Unemployment Rate`, 
                 group = Area, color = Area)) +geom_point() 
labs(title = "Unemployment Rate Over Time by Area", x = "Date",
     y = "Unemployment Rate (%)")

"The unemployment rate fluctuates over time, with some peaks and troughs 
indicating periods of higher and lower unemployment.There is a noticeable
spike around mid-2020, which aligns with the onset of the COVID-19 pandemic
and the subsequent lockdowns and economic disruptions.The plot shows
the overall trend without specifically focusing on the COVID-19 period.
The unemployment rate in urban areas seems to be generally higher than 
in rural areas.The plot, highlights the impact of the COVID-19
pandemic more clearly. Both rural and urban areas experienced a significant
increase in unemployment rates during the pandemic, with urban areas
being more affected.After the spike, there is a downward trend
indicating a recovery phase,but the rates have not returned to the
pre-pandemic levels, especially in urban areas."

# State-wise analysis
state_unemployment <- data %>%
  group_by(Region) %>%
  summarize(Average_Unemployment = mean(`Estimated Unemployment Rate`)) %>%
  arrange(Average_Unemployment)

ggplot(state_unemployment, aes(x = Average_Unemployment, y = reorder(Region, 
                      Average_Unemployment))) +
  geom_bar(stat = "identity", fill = "coral") +
  labs(title = "Average Unemployment Rate by State", x = "Average
       Unemployment Rate (%)", y = "State")


"The bar chart shows the average unemployment rate for each state over
the time period covered by the dataset."

"There is a wide range in the average unemployment rates across
different states,indicating regional disparities in employment conditions.
Some states have notably higher average unemployment rates, 
while others have managed to maintain lower averages.
This state-wise analysis is valuable for policymakers to identify 
which regions may need more attention and resources to combat unemployment."

#Urban vs. Rural analysis
urban_rural_unemployment <- data %>%
  group_by(Area, Date) %>%
  summarize(Average_Unemployment = mean(`Estimated Unemployment Rate`))

ggplot(urban_rural_unemployment, aes(x = Date, y = Average_Unemployment,
          group = Area, color = Area)) +geom_line() +
  labs(title = "Urban vs. Rural Unemployment Over Time", x = "Date", 
       y = "Average Unemployment Rate (%)")

"The line plot illustrates the urban versus rural unemployment 
rate trends over time"

"Both urban and rural areas show fluctuations in unemployment rates,
with urban areas generally experiencing higher rates than rural areas
throughout the dataset's timeframe.The spike around mid-2020,
likely corresponding to the COVID-19 pandemic's impact,is clearly visible
in both urban and rural areas, with urban areas showing a sharper increase.
Post the spike, there appears to be a gradual decline, suggesting a 
recovery from the peak unemployment rates experienced during the pandemic."

# Pre and Post COVID-19 analysis
pre_pandemic <- filter(data, Date < as.Date("2020-03-01"))
post_pandemic <- filter(data, Date >= as.Date("2020-03-01"))

mean(pre_pandemic$`Estimated Unemployment Rate`)
mean(post_pandemic$`Estimated Unemployment Rate`)

"Pre-pandemic average unemployment rate: approximately 9.51%
Post-pandemic average unemployment rate: approximately 17.77%
This indicates a significant increase in the unemployment rate following 
the onset of the pandemic. The data suggests that the economic impact
of COVID-19 was considerable,nearly doubling the unemployment 
rate on average across the regions in the dataset."
