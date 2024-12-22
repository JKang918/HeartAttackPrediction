## EDA (Exploratory Data Analysis)
# Data structures, Statistical distributions, NA values
# This data set is already preprocessed, no NA values
str(train)
summary(train)
colSums(is.na(train))

# BOXPLOTS by response
# create a list to store all boxplots
plots <- list()
# select all the predictors (exclude the response variable)
numeric_vars <- colnames(train)[colnames(train) != "output"]
# for loop to create all boxplots
for (var in numeric_vars) {
  p <- ggplot(train, aes_string(x = "factor(output)", y = var)) +
    geom_boxplot(fill = "lightblue") +
    labs(
      title = paste(var, "by Heart Attack Occurrence"),
      x = "Heart Attack (0 = None, 1 = Yes)",
      y = var
    ) +
    theme_minimal()
  plots[[var]] <- p # store the plot in the list
}

# print the generated box plots one by one
for (var in numeric_vars) {
  print(plots[[var]])
}

# Histograms
# !!The histogram part was conducted by another student!!
hist1=ggplot(train, aes(x=age)) + geom_histogram(binwidth=5,
                                                 fill="deepskyblue", color="black")
hist2=ggplot(train, aes(x=sex))+stat_count(width=1,
                                           fill="deepskyblue", color="black")
hist3=ggplot(train, aes(x=cp)) + stat_count(width=1,
                                            fill="deepskyblue", color="black")
hist4 = ggplot(train, aes(x=trtbps))+geom_histogram(binwidth=10,
                                                    fill="deepskyblue", color="black")
grid.arrange(hist1, hist2, hist3, hist4, ncol=2, nrow=2)
hist5 = ggplot(train, aes(x=chol)) + geom_histogram(binwidth=100,
                                                    fill="deepskyblue", color="black")
hist6 = ggplot(train, aes(x=fbs)) + stat_count(width=1,
                                               fill="deepskyblue", color="black")
hist7 = ggplot(train, aes(x=restecg))+stat_count(width=1,
                                                 fill="deepskyblue", color="black")
hist8 = ggplot(train, aes(x=thalachh)) + geom_histogram(binwidth=50,
                                                        fill="deepskyblue", color="black")
grid.arrange(hist5, hist6, hist7, hist8, ncol=2, nrow=2)
hist9 = ggplot(train, aes(x=exng)) + stat_count(width=1,
                                                fill="deepskyblue", color="black")
hist10 = ggplot(train, aes(x=oldpeak))+geom_histogram(binwidth=1,
                                                      fill="deepskyblue", color="black")
hist11 = ggplot(train, aes(x=slp))+stat_count(width=1,
                                              fill="deepskyblue", color="black")
hist12 = ggplot(train, aes(x=caa)) + stat_count(width=1,
                                                fill="deepskyblue", color="black")
grid.arrange(hist9, hist10, hist11, hist12, ncol=2, nrow=2)
hist13 = ggplot(train, aes(x=thall)) + stat_count(width=1,
                                                  fill="deepskyblue", color="black")
hist14 = ggplot(train, aes(x=output)) + stat_count(width=1,
                                                   fill="deepskyblue", color="black")
grid.arrange(hist13, hist14, ncol=2)
# !!The histogram part was conducted by another student!!

# Outliers
# !!The outlier part was conducted by another student!!
# Function to identify and remove outliers based on IQR
remove_outliers <- function(df, columns) {
  for (col in columns) {
    Q1 <- quantile(df[[col]], 0.25) # First quartile
    Q3 <- quantile(df[[col]], 0.75) # Third quartile
    IQR <- Q3 - Q1 # Interquartile range
    lower_bound <- Q1 - 1.5 * IQR # Lower bound
    upper_bound <- Q3 + 1.5 * IQR # Upper bound
    # Filter rows within bounds
    df <- df[df[[col]] >= lower_bound & df[[col]] <= upper_bound, ]
  }
  return(df)
}
train <- remove_outliers(train, c('chol', 'oldpeak'))
# !!The outlier part was conducted by another student!!

# Correlation heatmap
corr_matrix <- cor(train[, sapply(train, is.numeric) &
                           colnames(train) != "output"], use = "complete.obs")
# make it a long form
melted_corr <- reshape2::melt(corr_matrix)
melted_corr <- melted_corr[upper.tri(corr_matrix, diag = FALSE), ]
colnames(melted_corr) <- c("Var1", "Var2", "value")
# check variable order
melted_corr$Var1 <- factor(melted_corr$Var1, levels =
                             colnames(corr_matrix))
melted_corr$Var2 <- factor(melted_corr$Var2, levels =
                             colnames(corr_matrix))
# create a label for high correlation (|ρ| > 0.5)
melted_corr$label <- ifelse(abs(melted_corr$value) > 0.5,
                            round(melted_corr$value, 2), "")
# correlation heatmap
ggplot(melted_corr, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = label), color = "black", size = 3) + # add
  the label (for high |ρ|)
scale_fill_gradientn(
  colors = brewer.pal(n = 11, name = "RdBu"),
  limits = c(-1, 1),
  name = "Correlation"
) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Corrected Correlation Heatmap"
  ) +
  coord_fixed()