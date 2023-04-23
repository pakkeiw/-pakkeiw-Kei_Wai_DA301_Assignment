# 1. Prepare your workstation.

# Determine your working directory
getwd()

# Change your current directory.
setwd(dir='C:/Users/pakke/Desktop/LSE/COURSE_3')


# 1. Prepare your workstation.

# Import tidyverse library.
library(tidyverse)

# Import a CSV file.
data <- read.csv(file.choose(), header=T)

# Remove redundant columns (Ranking, Year, Genre, Publisher) by creating a subset of the data frame.
sales <- data[, !(names(data) %in% c("Ranking", "Year", "Genre", "Publisher"))]


# Create a summary of the new data frame
summary(sales)
str(sales)

# Create scatterplots to gain insights into the sales data.
# import ggplot2 package

# Create scatterplot of Global_Sales vs. NA_Sales
ggplot(sales, aes(x = NA_Sales, y = Global_Sales)) + 
  geom_point() +
  labs(x = "North America Sales (millions)", y = "Global Sales (millions)",
       title = "Scatterplot of Global Sales vs. North America Sales")

# Save the plot as a PNG file
ggsave("global_vs_na_sales.png")

# Create scatterplot of Global_Sales vs. EU_Sales
ggplot(sales, aes(x = EU_Sales, y = Global_Sales)) + 
  geom_point() +
  labs(x = "Europe Sales (millions)", y = "Global Sales (millions)",
       title = "Scatterplot of Global Sales vs. Europe Sales")

# Save the graph as a PNG file
ggsave("scatterplot.png")

# By analyzing the scatterplots, we observed that the sales in Europe and North America show a strong correlation with the Global Sales. This suggests that these two regions make significant contributions to the overall Global Sales.


# Plot a histogram of Global Sales
hist(sales$Global_Sales,
     main = "Histogram of Global Sales",
     xlab = "Global Sales")

# Save the graph as a PNG file
ggsave("global_sales_histogram.png")

# From histogram of Global Sales, we found most of the sales are less than 10

# Create a new data frame with total Global Sales for each platform
sales_by_platform <- aggregate(Global_Sales ~ Platform, data = sales, sum)

# Sort the data frame in descending order of Global Sales
sales_by_platform <- sales_by_platform[order(-sales_by_platform$Global_Sales), ]

# Create a bar plot of Global Sales by Platform
barplot(sales_by_platform$Global_Sales, 
        names.arg = sales_by_platform$Platform,
        main = "Global Sales by Platform",
        xlab = "Platform",
        ylab = "Global Sales",
        las = 2)

# From bar plot of Global Sales by Platform, we found Wii, X360 and PS3 are the platforms that contributed the most to the Global Sales.

# Use a barplot to compare the sales of Europe and North America and found NA is having more sales than
# Create a new data frame with total EU and NA sales
sales_total <- data.frame(
  Region = c("EU", "NA"),
  Total_Sales = c(sum(sales$EU_Sales), sum(sales$NA_Sales))
)

# Create a bar chart of EU and NA sales
ggplot(sales_total, aes(x = Region, y = Total_Sales, fill = Region)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Sales (millions)",
       title = "Comparison of EU and NA Sales")

# From the bar chart of EU and NA sales, we found North America is having more sales than Europe.

# Create a boxplot of Global Sales by Platform
boxplot(Global_Sales ~ Platform, data = sales,
        main = "Global Sales by Platform",
        xlab = "Platform",
        ylab = "Global Sales",
        las = 2)

# Add a horizontal line for the median Global Sales
abline(h = median(sales$Global_Sales), col = "red")

# Add a legend for the median line
legend("topleft", legend = "Median Global Sales", lty = 1, col = "red")

# Key insights derived from the boxplot:
# The PS2 and NES were very successful platforms with high median and maximum global sales, suggesting that they were very popular with consumers.
# The consistent sales figures for the Atari GEN platform indicate that it had a stable consumer base, which could be attributed to its retro appeal.
# The Wii platform had a very high maximum global sales figure, indicating that it was very successful overall. However, its lower median sales figure could suggest that it had some very popular games but also some less successful ones.
# The fact that newer platforms such as the PS4, XOne, and Nintendo Switch are not included in this data suggests that the gaming industry is constantly evolving and new platforms are regularly being introduced.
# The wide variation in sales figures between platforms highlights the importance of platform choice for game developers, as choosing the right platform can greatly impact a game's success.

#View the data frame to sense-check the data set.
#Determine the min, max and mean values of all the sales data (three columns).
#Create a summary of the data frame.


# Calculate the minimum, maximum, and mean values for all sales data
min_sales <- apply(sales[, c("NA_Sales", "EU_Sales", "Global_Sales")], 2, min)
max_sales <- apply(sales[, c("NA_Sales", "EU_Sales", "Global_Sales")], 2, max)
mean_sales <- apply(sales[, c("NA_Sales", "EU_Sales", "Global_Sales")], 2, mean)

# Print the results
cat("Minimum sales:", min_sales, "\n")
cat("Maximum sales:", max_sales, "\n")
cat("Mean sales:", mean_sales, "\n")

# Summary of dataframe
summary(sales)

# use the aggregate() function to sum the values in the "sales" data frame grouped by product:
sales_sum <- aggregate(cbind(NA_Sales, EU_Sales, Global_Sales) ~ Product, data = sales, sum)

# Create a summary of the new data frame.
summary(sales_sum)

#Create scatterplots to gain insights into the sales data.
library(ggplot2)

# Create scatterplot for NA_Sales vs. EU_Sales
ggplot(sales, aes(x = NA_Sales, y = EU_Sales)) +
  geom_point() +
  xlab("North America Sales (millions)") +
  ylab("Europe Sales (millions)") +
  ggtitle("North America vs. Europe Sales")

# Create scatterplot for Global_Sales vs. EU_Sales
ggplot(sales, aes(x = Global_Sales, y = EU_Sales)) +
  geom_point() +
  xlab("Global Sales (millions)") +
  ylab("Europe Sales (millions)") +
  ggtitle("Global vs. Europe Sales")

# Create scatterplot for Global_Sales vs. NA_Sales
ggplot(sales, aes(x = Global_Sales, y = NA_Sales)) +
  geom_point() +
  xlab("Global Sales (millions)") +
  ylab("North America Sales (millions)") +
  ggtitle("Global vs. North America Sales")

#Create histograms to gain insights into the sales data.
# Histogram of NA_Sales
hist(sales$NA_Sales, main="NA Sales", xlab="Sales in millions")

# Histogram of EU_Sales
hist(sales$EU_Sales, main="EU Sales", xlab="Sales in millions")

# Histogram of Global_Sales
hist(sales$Global_Sales, main="Global Sales", xlab="Sales in millions")

#Create boxplots to gain insights into the sales data.
# Combined boxplot of sales data
boxplot(sales[,3:5], main="Sales Data", ylab="Sales in millions", names=c("NA Sales", "EU Sales", "Global Sales"))

# Combined boxplot of sales data without outliers
boxplot(sales[,3:5], main="Sales Data", ylab="Sales in millions", names=c("NA Sales", "EU Sales", "Global Sales"), outline=FALSE)

# Determine the normality of the data set (sales data).
# Create and explore Q-Q plots for all sales data.
# Q-Q plot of NA_Sales
qqnorm(sales$NA_Sales, main="Q-Q Plot of NA Sales")
qqline(sales$NA_Sales, col="red")

# Q-Q plot of EU_Sales
qqnorm(sales$EU_Sales, main="Q-Q Plot of EU Sales")
qqline(sales$EU_Sales, col="green")

# Q-Q plot of Global_Sales
qqnorm(sales$Global_Sales, main="Q-Q Plot of Global Sales")
qqline(sales$Global_Sales, col="blue")

# Perform a Shapiro-Wilk test on all the sales data.
# Shapiro-Wilk test of NA_Sales
shapiro.test(sales$NA_Sales)
# This result  p-value < 2.2e-16 suggests that the variable is not normally distributed, since the p-value is less than the typical significance level of 0.05.

# Shapiro-Wilk test of EU_Sales
shapiro.test(sales$EU_Sales)
# This result  p-value < 2.2e-16 suggests that the variable is not normally distributed, since the p-value is less than the typical significance level of 0.05.

# Shapiro-Wilk test of Global_Sales
shapiro.test(sales$Global_Sales)
# This result  p-value < 2.2e-16 suggests that the variable is not normally distributed, since the p-value is less than the typical significance level of 0.05.

# Determine the Skewness and Kurtosis of all the sales data.
library(moments)

# Skewness of NA_Sales
skewness(sales$NA_Sales)

# Kurtosis of NA_Sales
kurtosis(sales$NA_Sales)

# Skewness of EU_Sales
skewness(sales$EU_Sales)

# Kurtosis of EU_Sales
kurtosis(sales$EU_Sales)

# Skewness of Global_Sales
skewness(sales$Global_Sales)

# Kurtosis of Global_Sales
kurtosis(sales$Global_Sales)

# The output you provided shows the skewness and kurtosis values for the "NA_Sales", "EU_Sales", and "Global_Sales" columns in the "sales" data frame.

# For the "NA_Sales" column, the skewness is 4.30921 and the kurtosis is 31.36852. This indicates a highly right-skewed distribution with heavy tails.

# For the "EU_Sales" column, the skewness is 4.818688 and the kurtosis is 44.68924. This also indicates a highly right-skewed distribution with even heavier tails than "NA_Sales".

# For the "Global_Sales" column, the skewness is 4.045582 and the kurtosis is 32.63966. This also indicates a highly right-skewed distribution with heavy tails.

# These results suggest that the sales data for all regions are highly non-normal and have highly skewed distributions with heavy tails. 

# Determine if there is any correlation between the sales data columns.
# Correlation matrix of sales data
cor(sales[,3:5])

# The correlation values in the matrix suggest that there are strong positive correlations between all pairs of sales data columns, with the highest correlation (0.935) between "NA_Sales" and "Global_Sales".

# Overall, this suggests that there is a high degree of similarity between the sales data across regions. This can be useful information for marketing and sales strategies, as it suggests that trends in one region may also apply to other regions. However, it is important to keep in mind that correlation does not imply causation, and further analysis is needed to identify any underlying factors driving the sales trends.

# Choose the type of plot you think best suits the data set and what you want to investigate. Explain your answer in your report.
# Given the nature of the data set, I believe a scatter plot would be the most appropriate type of plot to investigate the relationship between the sales data columns. A scatter plot is a graph that displays values for two continuous variables as points, with one variable on the x-axis and the other variable on the y-axis. Each point represents a pair of values for the two variables, and the position of the point reflects the values of the variables.

# In the case of the sales data, a scatter plot would allow us to visualize the relationship between pairs of sales data columns (e.g. "NA_Sales" vs "EU_Sales", "NA_Sales" vs "Global_Sales", etc.). By plotting each pair of data points as a single point on the scatter plot, we can see the overall trend and identify any patterns or outliers in the data.

# Additionally, scatter plots allow us to identify any correlations between the variables. If there is a positive correlation between two variables, the points on the scatter plot will form a roughly linear pattern that slopes upwards. If there is a negative correlation, the points will form a roughly linear pattern that slopes downwards. If there is no correlation, the points will be scattered randomly across the plot.
# Overall, a scatter plot is an effective way to visually explore the sales data and identify any patterns or trends between the sales data columns. It can also help us to identify any outliers or unusual data points that may need further investigation.

# Scatter plot of NA_Sales vs EU_Sales with trend line
plot(sales$NA_Sales, sales$EU_Sales, xlab = "NA Sales", ylab = "EU Sales", main = "NA Sales vs EU Sales")
abline(lm(sales$EU_Sales ~ sales$NA_Sales), col = "red")

# Scatter plot of NA_Sales vs Global_Sales with trend line
plot(sales$NA_Sales, sales$Global_Sales, xlab = "NA Sales", ylab = "Global Sales", main = "NA Sales vs Global Sales")
abline(lm(sales$Global_Sales ~ sales$NA_Sales), col = "red")

# Scatter plot of EU_Sales vs Global_Sales with trend line
plot(sales$EU_Sales, sales$Global_Sales, xlab = "EU Sales", ylab = "Global Sales", main = "EU Sales vs Global Sales")
abline(lm(sales$Global_Sales ~ sales$EU_Sales), col = "red")

# Load the corrplot package
library(corrplot)

# Create a correlation matrix of the sales data
sales_cor <- cor(sales[c("EU_Sales", "NA_Sales", "Global_Sales")])

# Visualize the correlation matrix
corrplot(sales_cor, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")


# Summarise (150–200 words) any insights you've discovered as well as anything you would like to explore further.
# In this analysis of the sales data, several insights have been discovered. Firstly, all three sales data columns ("NA_Sales", "EU_Sales", and "Global_Sales") are highly non-normal with highly skewed distributions and heavy tails, as indicated by the skewness and kurtosis values. Secondly, there is a high degree of positive correlation between all pairs of sales data columns, with the highest correlation between "NA_Sales" and "Global_Sales". Finally, the scatter plots with trend lines show a strong positive relationship between each pair of sales data columns, with a generally linear trend.

# These insights suggest that there are similarities in the sales trends across regions, and that a strong positive relationship exists between the sales data columns. This could indicate that trends in one region may also apply to other regions, and that marketing and sales strategies that work in one region may also work in other regions. However, further investigation is needed to identify any underlying factors driving the sales trends, and to explore any regional differences or variations that may exist.

# One area for further exploration is to investigate the impact of specific game genres or titles on sales trends, and to identify any regional differences in preferences for different genres or titles. Additionally, it may be useful to explore the impact of external factors such as economic conditions or cultural influences on the sales trends, and to develop marketing and sales strategies that take these factors into account.

# Load the sales dataset
sales <- read.csv(choose.files(),header = TRUE)

# Create a simple linear regression model of Global_Sales vs. EU_Sales
lm_model <- lm(Global_Sales ~ EU_Sales, data = sales)

# View the summary output of the model
summary(lm_model)

# Determine the correlation between the North America and Global sales columns
cor(sales$NA_Sales, sales$Global_Sales, method = "pearson")

# Determine the correlation between the Europe and Global sales columns
cor(sales$EU_Sales, sales$Global_Sales, method = "pearson")

# Create a scatter plot of EU_Sales vs. Global_Sales
plot(sales$EU_Sales, sales$Global_Sales, xlab = "EU Sales", ylab = "Global Sales", main = "Scatterplot of EU Sales vs. Global Sales")

# Add the regression line to the plot
abline(lm_model, col = "red")

# Create a residual plot to check for linearity and homoscedasticity
plot(lm_model, which = 1)

# Create a scatter plot of NA_Sales vs. Global_Sales
plot(sales$NA_Sales, sales$Global_Sales, xlab = "EU Sales", ylab = "Global Sales", main = "Scatterplot of NA Sales vs. Global Sales")

# Add the regression line to the plot
abline(lm_model, col = "red")

# Create a residual plot to check for linearity and homoscedasticity
plot(lm_model, which = 1)

# Select only the numeric columns of the sales dataframe
sales_num_cols <- sales[, sapply(sales, is.numeric)]

head(sales_num_cols)

# Calculate the correlation matrix between the sales columns
# We exclude the first column ("Ranking") because we don't want to include it in the correlation calculation
sales_cor <- cor(sales_num_cols[, -1])

# Print the correlation matrix to the console
print(sales_cor)

# Fit a linear regression model to the sales data
sales_lm <- lm(Global_Sales ~ NA_Sales + EU_Sales, data = sales_num_cols)

# Use the model to predict global sales for each provided value of NA_Sales and EU_Sales
sales_pred <- data.frame(
  NA_Sales = c(34.02, 3.93, 2.73, 2.26, 22.08),
  EU_Sales = c(23.80, 1.56, 0.65, 0.97, 0.52)
)

sales_pred$Predicted_Global_Sales <- predict(sales_lm, newdata = sales_pred)

# Compare the predicted global sales to the observed global sales
sales_pred$Observed_Global_Sales <- c(67.85, 33.00, 29.37, 27.06, 25.72)

print(sales_pred)

#In this analysis, we explored a dataset containing information about video game sales in various regions. We first selected all the numeric columns of the dataset and calculated the pairwise correlation coefficients between the sales columns. We found that sales in North America and Europe were strongly positively correlated with global sales.

#Next, we used a linear regression model to predict global sales based on sales in North America and Europe. We found that the model generally underestimated the actual values of global sales, suggesting that there may be other variables that are important predictors of global sales that are not included in the model.

#One limitation of our analysis is that the sample size is small, with only five observations. Additionally, there may be other factors that influence video game sales, such as the genre of the game, the platform it is released on, and the marketing and promotion efforts surrounding the game.

#In future analyses, it would be interesting to explore the relationship between video game sales and these other variables, as well as to investigate whether there are differences in sales patterns between different genres, platforms, and regions. We could also explore more advanced machine learning techniques to improve our predictions of global sales.

