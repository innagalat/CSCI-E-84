source(file.path("src", "lib", "utils.R"))
library('ProjectTemplate')
reload.project()


#load(data.file.path)
states <- c("AK", "IN") 

# 1) Creating histograms for quantitative variables
# Create a histogram of your assign STATE for our group project. You will plot the bins based on party and candidates.
# ?! "Table 2. Electoral & Pop Vote" C1 = STATE
# "2012 Pres General Results"
# "2012 Pres Primary Results"


# 
#df <- df[-c(1,2,3,4,nrow(df)-2,nrow(df)-1,nrow(df)), ]
df <- as.data.frame(tab$`Table 2.  2012 Presidential Electoral and Popular Vote`)
names(df) <- c(tolower(df[3,][[1]]), 
               paste(tolower(df[3,][[2]]), df[4,][[2]], sep = " - "), 
               paste(tolower(df[3,][[2]]), df[4,][[3]], sep = " - "), 
               paste(tolower(df[3,][[3]]), df[4,][[4]], sep = " - "), 
               paste(tolower(df[3,][[3]]), df[4,][[5]], sep = " - "), 
               paste(tolower(df[3,][[3]]), df[4,][[6]], sep = " - "),
               paste(df[4,][[7]]))
df <- filter(df, as.character(state) %in% states)

#
df <- as.data.frame(tab$`Table 3.  2012 General Election Votes Cast for U.S. President, Senate and House`)
names(df) <- df[3,]
df <- filter(df, State %in% states)

#
df <- as.data.frame(tab$`Table 4.  2012 General Election Votes Cast by Party`)
names(df) <- df[4,]
df <- filter(df, State %in% states)

#
df <- as.data.frame(tab$`Table 5.  2012 Primary and General Election Votes Cast for U.S. Congress`)
names(df) <- c(tolower(df[4,][[1]]), 
               paste(tolower(df[3,][[2]]), df[4,][[2]], sep = " - "), 
               paste(tolower(df[3,][[3]]), df[4,][[3]], sep = " - "), 
               paste(tolower(df[3,][[4]]), df[4,][[4]], sep = " - "), 
               paste(tolower(df[3,][[5]]), df[4,][[5]], sep = " - "))
df <- filter(df, state %in% states)

#
df <- as.data.frame(tab$`Table 6.  2012 Votes Cast for the U.S. Senate by Party`)
names(df) <- c(tolower(df[4,][[1]]), 
               paste(tolower(df[3,][[2]]), df[4,][[2]], sep = " - "), 
               paste(tolower(df[3,][[3]]), df[4,][[3]], sep = " - "), 
               paste(tolower(df[3,][[4]]), df[4,][[4]], sep = " - "), 
               paste(tolower(df[3,][[5]]), df[4,][[5]], sep = " - "),
               paste(tolower(df[3,][[6]]), df[4,][[6]], sep = " - "),
               paste(tolower(df[3,][[7]]), df[4,][[7]], sep = " - "))
df <- filter(df, state %in% states)

#
df <- as.data.frame(tab$`Table 7.  2012 Votes Cast for the U.S. House of Representatives by Party`)
names(df) <- c(tolower(df[4,][[1]]), 
               paste(tolower(df[3,][[2]]), df[4,][[2]], sep = " - "), 
               paste(tolower(df[3,][[3]]), df[4,][[3]], sep = " - "), 
               paste(tolower(df[3,][[4]]), df[4,][[4]], sep = " - "), 
               paste(tolower(df[3,][[5]]), df[4,][[5]], sep = " - "),
               paste(tolower(df[3,][[6]]), df[4,][[6]], sep = " - "),
               paste(tolower(df[3,][[7]]), df[4,][[7]], sep = " - "))
df <- filter(df, state %in% states)



# 2) Creating box plots for quantitative variables
# Create a box and scattered plot based on party and candidates.
# 
# 3) Overlaying plots
# Create an overlay based on party red and blue lines as shown in Lynda.com exercise.
# 
# 4) Calculating frequencies
# Calculate the frequencies of wins for each party win with all states.
# 
# 5) Calculating descriptive
# Create a summary using descriptive statistics off voter turnout in all states. Return the values of percentage for each party.
# 
# 6) Using a single proportion: Hypothesis test and confidence interval
# Create a hypothesis statement based on your state regarding who will win the 2016 President Election. Use the historical l data provided the above mention data set.
# 
# 7) Using a single mean: Hypothesis test and confidence interval
# Create a Hypothesis test in RStudio based on the historical data regarding the outcome of your state. Find a source that shows the results from 1960 to 2012 and state the probability of the historical data who will win in 2016.  Use your state only!
#   You must state both the Null (Ho) and Alternative (Ha).
# 
# 8) Using a single categorical variable: One sample chi-square test
# State how you would create a one sample Pearson chi-square test to test your hypothesis in question 7. Create a sample in RStudio based on Lynda.com practice exercise.
# 
# 9) Create a decision tree in your state based on voter turnout. Group your candidates in the following category based off of the historical data provided by the FEC.
# Clinton, Johnson, Stine, and Trump are the candidates. Based off of percentages off voter turnout in 2012 who will win your state.  So you will put them into categories like D (Dem), R (Rep), and O (Other).
# 
# 10) Create a scatter plot with liner regression line to show each counties voting pattern for each party
# Select your state and their counties to plot the strengthen and weakness for each district. Use a color scheme that match the party colors – red, blue, and green.  Yes, green is for other.