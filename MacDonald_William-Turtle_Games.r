
'''
install.packages(readxl)
install.packages(dplyr)
install.packages(ggplot2)
install.packages(moments)
install.packages(corrplot)
install.packages(mgcv)
install.packages(httpgd)
install.packages(showtext)
install.packages(curl)
install.packages("reshape2")
install.packages("ggpubr")
install.packages("GGally") 
'''
library(readxl)
library(dplyr)
library(ggplot2)
library(moments)
library(corrplot)
library(mgcv)
library(httpgd)
library(showtext)
library(curl)
library(reshape2)
library(ggpubr)
library(GGally)

# Import Data
data <- read.csv("c:/Users/willi/OneDrive/Documents/LSE - Data Analytics/Course 3/LSE_DA301_assignment_files_new/turtle_reviews.csv")

# Import Data
synthetic_data <- read.csv("C:/Users/willi/OneDrive/Documents/LSE - Data Analytics/Course 3/LSE_DA301_assignment_files_new/synthetic_data_for_regression.csv")

# Drop unnecessary columns
data <- data %>%
  select(-language, -platform)

# View the data
colnames(data)

data <- data %>%
  rename(`remuneration_k` = `remuneration..k..`, `spending_score` = `spending_score..1.100.`)
# View the data
colnames(data)

# Create a summary of the new data frame
summary(data)

# Have a look at data types
column_data_types <- sapply(data, class)
print(column_data_types)

# Define colors
color_map <- c("thriving" = '#AAD3E3',
               "maroon" = '#8F223A',
               "champagne_pink" = '#F2DFCE',
               "old_lace" = '#FFF1E0',
               "floral_white" = '#FFF9F5',
               "metallic_seaweed" = '#0D7680')

# Define the custom theme
ft_theme <- function() {
  theme_minimal(base_family = "Outfit")
    theme(
      plot.background = element_rect(fill = '#FDF1E6', color = NA),
      panel.background = element_rect(fill = '#FDF1E6', color = NA),
      legend.background = element_rect(fill = '#FDF1E6', color = NA),
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 16, hjust = 0.5),
      plot.caption = element_text(size = 12, color = "#666666"),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 12),
      axis.ticks = element_line(color = '#C3BAB2'),
      axis.line = element_line(color = '#CBC1B9'),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "#E4D9D0"),
      panel.grid.minor = element_blank()
    )
}

# Load custom font
font_add_google("Outfit", "Outfit")
showtext_auto()

# Initialize httpgd graphics device
hgd()

#Histogram for Loyalty Points
ggplot(data, aes(x = loyalty_points)) +
  geom_histogram(binwidth = 100, fill = color_map['thriving'], color = "black", alpha = 0.7) +
  labs(title = "Distribution of Loyalty Points", x = "Loyalty Points", y = "Frequency") +
  ft_theme()
# geom_histogram is a histogram
# geom_point is a scatter plot
# geom_line is a line plot
# geom_bar is a bar plot
# geom_boxplot is a boxplot
# geom_density is a density plot

# Scatterplot of Income v. Loyalty Points
ggplot(data, aes(x = remuneration_k, y = loyalty_points)) +
  geom_point(alpha = 0.5, color = color_map['maroon']) +
  labs(title = "Loyalty Points vs Income", x = "Income (K)", y = "Loyalty Points") +
  ft_theme()


  # Scatterplot of Spending v. Loyalty Points
ggplot(data, aes(x = spending_score, y = loyalty_points)) +
  geom_point(alpha = 0.5, color = color_map['maroon']) +
  labs(title = "Loyalty Points vs Spending", x = "Spending Score", y = "Loyalty Points") +
  ft_theme()

# Scatterplot of Age v. Loyalty Points
ggplot(data, aes(x = age, y = loyalty_points)) +
  geom_point(alpha = 0.5, color = color_map['maroon']) +
  labs(title = "Loyalty Points vs Income", x = "Age", y = "Loyalty Points") +
  ft_theme()

# Boxplot of Education and Loyalty Points
ggplot(data, aes(x = as.factor(education), y = loyalty_points)) +
  geom_boxplot(alpha=0.5, color=color_map['maroon']) +
  labs(title = "Education and Loyalty Points", x = "Education", y = "Loyalty points") +
  ft_theme()
# Boxplot of Gender and Loyalty Points
ggplot(data, aes(x = as.factor(gender), y = loyalty_points)) +
  geom_boxplot(alpha=0.5, color=color_map['maroon']) +
  labs(title = "Gender and Loyalty Points", x = "Gender", y = "Loyalty points") +
  ft_theme()

# Correlation
# Drop unnecessary columns for correlation analysis
numeric_data <- data %>%
  select_if(is.numeric)
# Calculate the correlation matrix
correlation_matrix <- cor(numeric_data, use = "complete.obs")

# Melt the correlation matrix into long format
correlation_melt <- melt(correlation_matrix)

# Create the correlation heatmap using ggplot2
ggplot(data = correlation_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = color_map['maroon'], high = color_map['metallic_seaweed'], mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  labs(title = "Correlation Heatmap",
       x = "Variables",
       y = "Variables") +
  ft_theme() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


# Create pair plots with custom colors
pairs_plot <- ggpairs(data[, c('loyalty_points', 'spending_score', 'remuneration_k', 'age')],
                      lower = list(continuous = wrap("smooth", color = color_map['metallic_seaweed'])),
                      diag = list(continuous = wrap("barDiag", fill = color_map['thriving'], color = color_map['maroon'])),
                      upper = list(continuous = wrap("cor", size = 6, colour = color_map['maroon'])))

# Apply custom theme
pairs_plot <- pairs_plot + ft_theme()

# Display the pair plot
print(pairs_plot)



# Measures of Shape

# Shapiro-Wilk test for normality
shapiro.test(data$loyalty_points)

# Skewness and Kurtosis
skewness(data$loyalty_points)
kurtosis(data$loyalty_points)

# Calculate Range
range_loyalty_points <- range(data$loyalty_points)

# Calculate Difference between highest and lowest values
difference_high_low <- diff(loyalty_points)

# Calculate Interquartile Range (IQR)
iqr_loyalty_points <- IQR(data$loyalty_points)

# Calculate Variance
variance_loyalty_points <- var(data$loyalty_points)

# Calculate Standard Deviation
std_deviation_loyalty_points <- sd(data$loyalty_points)

# Display results
list(
  Range = range_loyalty_points,
  Difference = difference_high_low,
  IQR = iqr_loyalty_points,
  Variance = variance_loyalty_points,
  Standard_Deviation = std_deviation_loyalty_points
)

# More Measures of Shape

scores <- data$loyalty_points

# Calculate mean, median, and mode
mean_score <- mean(scores)
median_score <- median(scores)
mode_score <- as.numeric(names(sort(table(scores), decreasing = TRUE)[1]))

# Print the results
cat("Mean:", mean_score, "\n")
cat("Median:", median_score, "\n")
cat("Mode:", mode_score, "\n")

# Multiple Linear Regression

# Create the multiple linear regression model
model <- lm(loyalty_points ~ age + remuneration_k + spending_score, data = data)


# Visualising the model

# Plot actual vs. predicted values with a different smoothing method if necessary
ggplot(data, aes(x = loyalty_points, y = predict(model, data))) +
  geom_point() +
  stat_smooth(method = "loess") +  # You can change 'loess' to 'lm' or other methods
  labs(x = 'Actual Loyalty Points', y = 'Predicted Loyalty Points') +
  ggtitle('Actual vs. Predicted Loyalty Points') + ft_theme()


# Display the plots in httpgd viewer
hgd_browse()
