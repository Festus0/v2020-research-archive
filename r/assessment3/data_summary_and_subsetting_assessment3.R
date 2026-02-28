############################################
### R scripts - Assessment 3 and beyond! ###
############################################


#############################################################
### Reading the database and obtaining summary statistics ###

dframe1 <- read.csv(file.choose())  
names (dframe1)
summary(dframe1)


#############################################################
###Changing variables read as numerical into a factor (categorical variable)

dframe1$x <- as.factor(dframe1$x) # you need to replace both "x" for the variable name

#############################################################
### Creating a subset of data ####

dframe2 <- subset (dframe1, x == "A") # in which x is the variable name and A the group or value you wish
					 	  # e.g. x could be "sex" and A could be "F" (for female) 
newdata <- mydata[1:5,]   # a different way of creating a subset, this time asking R to filter 
			 	          # from all the variables the data cases 1 to 5.

################################################################################################################
### Descriptive statistics ####

              
mean (dframe1$x, na.rm = T)  # Obtain the mean

median (dframe1$x, na.rm = T)              # Median

                                           # Sample size
length (dframe1$x) - sum (is.na(dframe1$x))  # # i.e. length of column of data, minus the number of missing values
			                       
## A "function" for calculating the Mode of a numerical variable##
mode <- function(x) { t0 <- table(x)    
as.numeric (names(t0)[t0==max(t0)]) } 

mode (dframe1$x)    # Use the functon you created to obtain the Mode
   
## A "function" for calculating the Mode of a categorical variable###
mode <- function(x) { t0 <- table(x)    
as.factor (names(t0)[t0==max(t0)]) } 

mode (dframe1$x)          # Use the functon you created to obtain the Mode


var(dframe1$x, na.rm=T)               # Variance



sqrt(var(dframe1$x, na.rm = T))  # Square root of the variance = standard deviation  


## In order to calculate the SE you need the SD and sample size##       
SD <- sqrt(var(dframe1$x, na.rm = T))# Create an object called SD defined as the standard deviation
N  <- length(dframe1$x)-sum(is.na(dframe1$x)) # Create an object called N defined as sample size
SE <- SD/sqrt(N) # Create an object called SE defined as standard error
SE               # Use function to obtain SE

min(dframe1$x,  na.rm = T)  # Minumum value (you also get it from summary(dframe1)
max(dframe1$x,  na.rm = T)  # Maximum value (you also get it from summary(dframe1)


# A function for calculating the 95% confidence interval limits## 
##(Parametric method; assumes normal distribution of data)##

lowerlimit <- (mean (dframe1$x, na.rm = T) - 1.96*SE)
upperlimit <- (mean (dframe1$x, na.rm = T) + 1.96*SE)
lowerlimit                                         # Report the lower limit
upperlimit                                         # Report the upper limit
interval.width <- upperlimit - lowerlimit
interval.width                                     # Report the width of the confidence interval


# Bootstrapping method for calculating 95% confidence intervals (does not assume data are normally distributed)
a <- numeric(100000)              # Function for bootstrapping method
for (i in 1:100000) a[i] <- mean(sample(dframe1$x, replace = T ), na.rm = T)  # applies the function to the "x" variable
# NB this can take some time to run on slow computers!

quantile (a,c(0.025, 0.975))  # Gives 95% bootstrapped CI limits
quantile (a,c(0.005, 0.995))  # Gives 99% bootstrapped CI limits


###############################################################################################################
### Plotting graphs ####

windows()# Windows users: use this script before plotting each graph to allow you to view multiple graphs
quartz() # Mac users: use this script before plotting each graph to allow you to view multiple graphs

#### Graph of differences - Boxplot ##


### If you have a continuous variable and a categorical variable
and wish to plot one in relation to the other (~)####

boxplot(dframe1$y ~ dframe1$x,         # The basic boxplot command
varwidth = TRUE,                        # width of boxes represents sample size
notch = TRUE,                           # add a notch indicating 95% confidence intervals
names = c("A", "B"),                     # labels the different boxes
col=c("violet", "light green"),          # colours the different boxes
xlab = "name of x axis",                  # adds an x-axis label
ylab = "name and units of y axis",      # adds a y-axis label
cex.lab = 1.6,                          # adjusts the size of the axis labels
cex.axis = 1.3,                         # adjusts the size of the axis numbering
las = 1                                 # alters the axis numbering orientation
)                                       # end of "arguments" (closing the brackets completes the command)

### If you have two columns of continuos data (x1 and x2)
and wish to plot one and the other (,)####

boxplot(dframe1$x1, dframe1$x2,         # The basic boxplot command
varwidth = TRUE,                        # width of boxes represents sample size
notch = TRUE,                           # add a notch indicating 95% confidence intervals
names = c("A", "B"),                     # labels the different boxes
col=c("violet", "light green"),          # colours the different boxes
xlab = "name of x axis",                  # adds an x-axis label
ylab = "name and units of y axis",      # adds a y-axis label
cex.lab = 1.6,                          # adjusts the size of the axis labels
cex.axis = 1.3,                         # adjusts the size of the axis numbering
las = 1                                 # alters the axis numbering orientation
)


#### Graph of correlations - Scatterplot ##
plot (dframe1$Maths ~ dframe1$Biology)
,              # specifies what to plot
  xlab = "x axis title and units",                 # changes the labels on
  ylab = "y axis title and units",                  # the x & y axes
  cex.lab = 1.5,                          # increases the axis label font size
  pch = 20,                               # changes symbols to solid dots
  cex = 1.7,                              # increases the symbol size
  cex.axis = 1.3,                         # increases the size of the axis numbering
  col = "dark grey")                           # changes the colour of the symbols


abline (lm (dframe1$A ~ dframe1$B),       # adds a fitted line
col = "black",                              # changes colour of line
lwd = 3) 


type="o" # creates a line that links the dots 
type="b"
 

#################################################################################################################
#### Testing for a normal distribution ####

hist(dframe1$x) # creates an histogram of variable x

qqnorm(dframe1$x) # creates a QQ-plot for the variable x
qqline(dframe1$x) # adds the normality line to the QQplot

shapiro.test(dframe1$1)

#### Data transformations#####

log   # takes the natural logarithm 
log10 # takes the logarithm base 10
sqrt  # takes the square root
^2    # squares the values 
^3    # cubes the values
exp   # returns the exponential  

## Examples of how to apply these functions to the data:
hist(dframe1$x^2)             # gives an histogran of the squared values of x
shapirto.test(sqrt(dframe1$x) # performs a normality test in the square root transformed values of x
mean(log(dframe1$x))          # gives the mean of the logges values of x
plot (exp(dframe1$y) ~ log10(dframe1$x)) # plots the exponents of y in relation to the logged 10 vales of x

###Alternatively you can create objects defined by these functions and use the objects
squaredx <- dframe1$x^3
hist (squaredx)

########################################
#####Performing statistical tests#######


##### Testing for equality of variances ####

var.test(dframe1$y ~ dframe1$xf) 
# OR
var.test(dframe1$x1 ,dframe1$x2) 

##NOTE##
# use ~ if you have one column for a continuous variable that you wish to 
compare in relation to a categorical variable with two groups in a different column
# use , if you have two columns of continuous data (group x1 and group x2) you wish to compare 

### Performing a parametric test for comparing means - t-test#####

##Two-sample t-test for un-matched data
t.test (dframe1$y ~ dframe1$xf, var.equal=T)# if variances are similar; var.equal=F if they are not 
# OR
t.test (dframe1$A , dframe1$B, var.equal=T) 

##Two-sample t-test for matched data
t.test (dframe1$y ~ dframe1$x, paired = T) 
# OR
t.test (dframe1$x1 , dframe1$x2, paired =T)

##NOTE##
# use ~ if you have one column for a continuous variable that you wish to 
compare in relation to a categorical variable with two groups in a different column
# use , if you have two columns of continuous data (group x1 and group x2) you wish to compare 

##One-sample t-test
t.test (dframe1$x, mu=0) # e.g. does my sample come from a distribution with a mean zero?



### Performing a non-parametric test for comparing averages (ranks)#####

##Mann-Whitney U test for un-matched groups

wilcox.test (dframe1$y ~ dframe1$x) 
# OR
wilcox.test (dframe1$x1, dframe1$x2)


##Wilcoxon rank sum test for matched groups

wilcox.test (dframe1$y ~ dframe1$x, paired = T) 
# OR
wilcox.test (dframe1$x1, dframe1$x2, paired = T)

##Wilcoxon one-sample test 
wilcox.test (dframe1$x, mu= 5)# e.g. does my sample come from a distribution with a mean five?


##NOTE 1##
# use ~ if you have one column for a continuous variable that you wish to 
compare in relation to a categorical variable with two groups in a different column
# use , if you have two columns of continuous data (group x1 and group x2) you wish to compare 

##NOTE 2##
# the value of the test statistic for these tests will differ depending in which order you put your variables


#################################################################################################################
### Testing for an association between variables ####

## Performing a parametric test - Pearson's correlation

cor.test(dframe1$y, dframe1$x)

Rsq <- r*r # the R-squared gives an indication of how much variation in one variable is explained by the variation in the other variable 
Rsq 

## Performing a non-parametric test - Spearman's rank correlation

cor.test (dframe1$y, dframe1$x, method = "spearman")  



##########################################################################################
### Chi-square goodness of fit test####
## If the data is on a spreadsheet and you need R to calculate proportions
count <- table (dframe1$x)          # create the table of proportions for variable x
count 

## If you are given proportions enter the counts directly into a table
count <-matrix(c(234, 66), nrow = 2)
count 
dimnames(count) = list(c("name1", "name2"))	# adds column names
count                                           # re-check the table

chisq.test (count) # will test for equal proportions (50:50 ratio)

null.probs <- c(3/4,1/4) 
chisq.test(count, p=null.probs) # will test for the proportions given as null (assumed)


##########################################################################################
### Chi-square Contingency table test####

##2x2 table###

## If the data is on a spreadsheet and you need R to calculate proportions
count <- table (dframe1$x, dframe1$x2)   # create the table of proportions for variables x1 and x2
count                                     # check the table

dimnames(count) = list(
c("name 1", "name 2"),   # adds row names (variable x1)
c("name 1", "name 2"))	# adds column names (variable x2)
count

chisq.test(count)


## If you are given proportions enter the counts directly into a table

count <-matrix(c(100,150,130,110), nrow = 2) # go down per column (column1/row1, column1/row2, etc) 
count

# If you want, you can add more informative column and row names to your table
dimnames(count) = list(
c("row1", "row2"),            # adds row names
c("column1", "column2"))	# adds column names
count

chisq.test (count, correct = F)

# Or, if the chi-square test can not be performed 
fisher.test (count)


###2x3 table###
count <-matrix(c(v1,v2,v3,v4,v5,v6), nrow = 3) # go down per column (column1/row1, column1/row2, etc) 
count

# If you want, you can add more informative column and row names to your table
dimnames(count) = list(
c("row1", "row2", "row3"),   # adds row names
c("column1", "column2"))	# adds column names
count

chisq.test (count)
