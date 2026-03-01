C# QuickStats Toolkit (R)

## Overview
QuickStats Toolkit is a lightweight, reusable R script for rapid exploratory data analysis (EDA), descriptive statistics, basic statistical testing, and visualization on tabular datasets (CSV or data frames).

Originally developed as a general-purpose data analysis utility and refactored into a reusable toolkit for research workflows, teaching, and quick statistical inspection.

This script is particularly useful for:
- Rapid dataset screening
- Descriptive statistical summaries
- Bootstrapped confidence intervals
- Normality testing
- Group comparisons (parametric & non-parametric)
- Quick visualization (boxplots, scatterplots, histograms, QQ plots)

---

## What It Does
The toolkit performs end-to-end exploratory and statistical analysis including:

### Data Handling
- Reads CSV datasets
- Converts numerical variables to categorical (factor) where needed
- Creates subsets of data based on conditions
- Handles missing values (NA-aware calculations)

### Descriptive Statistics
- Mean, median, variance, standard deviation
- Standard error (SE)
- Sample size (N)
- Minimum and maximum values
- Mode (numeric and categorical)
- 95% confidence intervals (parametric)
- Bootstrapped confidence intervals (non-parametric)

### Statistical Testing
- Variance equality tests (F-test)
- Parametric tests (t-tests: paired & unpaired)
- Non-parametric tests (Wilcoxon, Mann-Whitney)
- Correlation analysis (Pearson & Spearman)
- Chi-square goodness-of-fit tests
- Chi-square contingency table tests
- Fisher’s exact test (fallback for small samples)

### Distribution & Diagnostics
- Histogram generation
- QQ plots for normality assessment
- Shapiro-Wilk normality testing
- Data transformations (log, sqrt, exponential)

### Visualization
- Boxplots for grouped comparisons
- Scatterplots with regression lines
- Multi-graph support (Mac/Windows devices)
- Custom axis labels, colours, and formatting

---

## Example Usage

## Example Usage

### 1. Load the Dataset
```r
df <- read.csv(file.choose())
cat > r/quickstats_toolkit/README.md << 'EOF'

2. Convert a Variable to Factor (Categorical)
```r 
df$group <- as.factor(df$group)

3. Create a Subset
```r 
subset_df <- subset(df, group == "A")

4. Run Descriptive Statistics
```r 
mean(df$value, na.rm = TRUE)
median(df$value, na.rm = TRUE)
sd(df$value, na.rm = TRUE)

5. Perform Statistical Tests
```r 
# Parametric test (t-test)
t.test(df$outcome ~ df$group, var.equal = TRUE)

# Non-parametric test (Wilcoxon / Mann-Whitney)
wilcox.test(df$outcome ~ df$group)

# Correlation analysis
cor.test(df$x, df$y)

6. Generate Plots
```r 
# Boxplot for group comparison
boxplot(df$outcome ~ df$group,
        col = c("violet", "lightgreen"),
        xlab = "Group",
        ylab = "Outcome")

# Histogram for distribution inspection
hist(df$value)

# Normality diagnostics
qqnorm(df$value)
qqline(df$value)
