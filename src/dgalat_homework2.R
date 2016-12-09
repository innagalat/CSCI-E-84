source(file.path("src", "lib", "utils.R"))
library("ProjectTemplate")
reload.project()
options(scipen = 999)

get.appendix.a.column.names <- function(df) {
    i <- 0
    names.vec <- c(tolower(df[6, 1]))
    for (year in as.numeric(df[5, seq(2, ncol(df), 2)])) {
        i <- i + 2
        names.vec <- c(names.vec, paste(year, gsub("\\(|\\)", "", df[7, i]), sep = " - "), 
            paste(year, gsub("\\(|\\)", "", df[7, i + 1]), sep = " - "))
    }
    return(names.vec)
}

find.party.name.from.abbr <- function(abbr.list) {
    label.dict <- tab$`2012 Party Labels`[5:116, c(1, 3)]
    names(label.dict) <- c("abbr", "name")
    results <- c()
    for (abbr in abbr.list) {
        results <- c(results, label.dict[label.dict["abbr"] == as.character(abbr), 
            "name"])
    }
    return(results)
}

state.names <- c("Indiana", "Alaska")
df <- tab$`2012 Presidential General Election Results`
df <- df[1:535, ]

# Question 1
plot.q1 <- function() {
    
    df.q1 <- filter(df, STATE %in% state.names)
    df.q1 <- df.q1[-c(6, 19), ]
    AK <- df.q1[1:5, ]
    IN <- df.q1[6:nrow(df.q1), ]
    
    pl <- ggplot(AK, aes(x = AK$PARTY, y = AK$`GENERAL %`))
    pl + geom_bar(stat = "identity") + xlab("Party") + ylab("Proportion of voters") + 
        ggtitle("Voters per party in Alaska")
    
    pl <- ggplot(AK, aes(x = AK$`LAST NAME`, y = AK$`GENERAL %`))
    pl + geom_bar(stat = "identity") + xlab("Party") + ylab("Proportion of voters") + 
        ggtitle("Voters per candidate in Alaska")
    
    aggr.dat <- aggregate(IN$`GENERAL %` ~ IN$PARTY, FUN = sum)
    pl <- ggplot(aggr.dat, aes(x = aggr.dat[, 1], y = aggr.dat[, 2]))
    pl + geom_bar(stat = "identity") + xlab("Party") + ylab("Proportion of voters") + 
        ggtitle("Voters per party in Indiana")
    
    pl <- ggplot(IN, aes(x = IN$`LAST NAME`, y = IN$`GENERAL %`))
    pl + geom_bar(stat = "identity") + xlab("Party") + ylab("Proportion of voters") + 
        ggtitle("Voters per candidate in Indiana") + theme(axis.text.x = element_text(angle = 45, 
        hjust = 1, vjust = 1))
}

# Question 2
plot.q2 <- function() {
    q2 <- filter(df, is.na(`TOTAL VOTES`))
    q2.aggr <- data.frame()
    for (state in unique(df$STATE)) {
        d <- filter(q2, STATE == state)
        d <- aggregate(d$`GENERAL RESULTS` ~ d$PARTY, FUN = sum)
        d <- cbind(state, d)
        q2.aggr <- rbind(q2.aggr, d)
    }
    names(q2.aggr) <- c(names(q2.aggr)[1], "party", "votes")
    q2.aggr <- mutate(q2.aggr, party = replace(party, party == "Combined Parties:", 
        "D"))
    
    # scatter party
    ggplot(q2.aggr, aes(x = party, y = votes, color = party)) + geom_point(shape = 1, 
        alpha = 3/4) + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) + 
        xlab("Party") + ylab("Number of voters in a state") + ggtitle("Voters per party for all states")
    
    # boxplot party
    plot(as.factor(find.party.name.from.abbr(q2.aggr$party)), q2.aggr$votes, xlab = "Parties", 
        ylab = "Number of voters in a state", cex.axis = 0.45, las = 2)
    
    # scatter candidate
    ggplot(q2, aes(x = q2$`LAST NAME`, y = q2$`GENERAL RESULTS`, color = q2$`LAST NAME`)) + 
        geom_point(shape = 1, alpha = 3/4) + theme(axis.text.x = element_text(angle = 45, 
        hjust = 1, vjust = 1)) + xlab("Candidates") + ylab("Number of voters in a state") + 
        ggtitle("Voters per candidate for all states")
    
    # boxplot candidate
    plot(as.factor(q2$`LAST NAME`), q2$`GENERAL RESULTS`, xlab = "Parties", ylab = "Number of voters in a state", 
        cex.axis = 0.45, las = 2)
}

# Question 3
plot.q3 <- function() {
    get.overlayed.hist <- function(dat, label, colour) {
        if (colour == "green") {
            x.lim <- c(0, max(dat) * 1.25)
        } else {
            x.lim <- c(0, 1)
        }
        hist(dat, density = 20, breaks = 20, prob = T, xlim = x.lim, xlab = "Proportion of people voting for a party in all states 1996 to 2012", 
            main = label)
        lines(density(dat), add = TRUE, col = "brown")
        curve(dnorm(x, mean = mean(dat), sd = sd(dat)), col = colour, lwd = 2, add = TRUE, 
            yaxt = "n")
    }
    
    df.historic <- tab$`Appendix A. Table:  1996-2012 Presidential General Election Percentage of Popular Vote Received by State (Democratic/Republican Nominees)`
    names(df.historic) <- get.appendix.a.column.names(df.historic)
    df.historic <- df.historic[c(8:58), ]
    
    df.q3 <- df.historic %>% gather(df.q3, "state")
    df.q3 <- cbind(df.q3, t(as.data.frame(strsplit(df.q3[, 2], " - "))))
    df.q3 <- df.q3[, -2]
    rownames(df.q3) <- 1:nrow(df.q3)
    names(df.q3) <- c(names(df.q3)[1], "value", "year", "party")
    
    # Normalizing values
    for (i in seq(1:nrow(df.q3))) {
        if (df.q3[i, "value"] > 1) {
            df.q3[i, "value"] <- as.numeric(df.q3[i, "value"])/100
        }
    }
    # Calculating missing values
    for (s in unique(df.q3$state)) {
        for (y in seq(2012, 1996, -4)) {
            year.df <- filter(df.q3, year == y, state == s)
            df.q3 <- rbind(df.q3, c(s, 1 - sum(as.numeric(year.df[, "value"])), y, 
                "O"))
        }
    }
    
    get.overlayed.hist(as.numeric(filter(df.q3, party == "D")$value), "Democrat votes distribution", 
        "blue")
    get.overlayed.hist(as.numeric(filter(df.q3, party == "R")$value), "Republican votes distribution", 
        "red")
    get.overlayed.hist(as.numeric(df.q3[is.na(df.q3$party), ]$value), "Other votes distribution", 
        "green")
}

# Question 4
answer.q4 <- function() {
    df.q4 <- filter(df, is.na(`TOTAL VOTES`), `WINNER INDICATOR` == "W")
    republican.wins <- count(filter(df.q4, `LAST NAME` == "Romney"))[[1]]
    democrat.wins <- count(filter(df.q4, `LAST NAME` == "Obama"))[[1]]
    print(paste0("Number of republican wins: ", republican.wins))
    print(paste0("Number of democrat wins: ", democrat.wins))
}

# Question 5
answer.q5 <- function() {
    df.q5 <- tab$`Table 4.  2012 General Election Votes Cast by Party`
    names(df.q5) <- df.q5[4, ]
    df.q5 <- df.q5[-c(1:4, 61:nrow(df.q5)), ]
    df.q5$`Democratic Candidates`[is.na(df.q5$`Democratic Candidates`)] <- 0
    df.q5$`Republican Candidates`[is.na(df.q5$`Republican Candidates`)] <- 0
    df.q5$`Other Candidates`[is.na(df.q5$`Other Candidates`)] <- 0
    cat("Democratic turnout summary: ")
    print(summary(as.numeric(df.q5[, 2])))
    cat("Republican turnout summary: ")
    print(summary(as.numeric(df.q5[, 3])))
    cat("Other turnout summary: ")
    print(summary(as.numeric(df.q5[, 4])))
}

# Question 6 I want to check if Democrates will lose, so we test if they will
# recieve 50% of votes

# test the claim that Democrates will lose.  Does the evidence from sample
# support the claim?

# Ho: p0=0.05 Democrats will win election, given that they receive only 50% of
# votes vs Ha: p0 is not 0.05,

# let x- proportion # of wins of D over the sample of n periods, with
# hypothesised p0=0.05 prob of democrats winning


df.q6 <- filter(df, is.na(`TOTAL VOTES`), `WINNER INDICATOR` == "W")
republican.wins <- count(filter(df.q6, `LAST NAME` == "Romney"))[[1]]
democrat.wins <- count(filter(df.q6, `LAST NAME` == "Obama"))[[1]]
total.voting <- republican.wins + democrat.wins
republican.wins <- filter(df.q6, `LAST NAME` == "Romney")
x <- republican.wins
n <- total.voting

# prop.test(x, n, p = 0.5, conf.level = 0.95, alternative = 'two.sided')
# prop.test returns a warning message, therefore binomial proportion test is used
# this is a test for comparing independent samples (!)  we only have one sample,
# democrats and republican votes are not independent therefore we use binomial
# proportion test


# Binomial proportion test
smpl.prop <- x/n
p0 <- 0.45
z <- (smpl.prop - p0)/sqrt(smpl.prop * (1 - smpl.prop)/n)
alpha <- 0.05  # Type 1 error - % that we reject H0, given that H0 is true
# since Ho: p0=0.05 vs Ha: p0 is not 0.05, use 2 sided test; Z~Norm(0,1)
z.norm <- qnorm(1 - alpha/2)
if (abs(z) < z.norm) {
    print("Accept Ho, Not enough evidence to reject Ho at 5%")
} else {
    print("Statistical evidence suggests, that Ho does not hold, therefore accept Ha")
}

# Type 2 error, rejecting Ha, given that Ha is true same as the probability that
# observed value is not the rejection region when Ha is true
lower.bound <- -(z.norm * sqrt(smpl.prop * (1 - smpl.prop)/n)) + p0
upper.bound <- (z.norm * sqrt(smpl.prop * (1 - smpl.prop)/n)) + p0

pHa <- round(mean(republican.wins$`GENERAL %`), 1)

# pHa <- 0.6
rej.reg.upper.bound <- (upper.bound - pHa) * sqrt(n)/sqrt(smpl.prop * (1 - smpl.prop))
rej.reg.lower.bound <- (lower.bound - pHa) * sqrt(n)/sqrt(smpl.prop * (1 - smpl.prop))
beta <- 1 - pnorm(rej.reg.upper.bound) + pnorm(rej.reg.lower.bound)
beta
print(paste0("The value of the alpha is ", round(alpha * 100, 1), "%"))
print(paste0("The value of the beta is ", round(beta * 100, 1), "%"))

# The large value of β tells us that sample of size n = 51 frequently will fail
# to detect a difference of 0.1 from the hypothesized proportion p=0.5 which
# means that HT (based on the given input) has a weak power and shows suboptimal
# results by passing Ho hypothesis; one of the solutions is to increase sample
# size n




# Question 7

df.q7 <- read_excel(file.path("data", "US Presidential Results & PVIs by State 1828-2012.xlsx"))
df.q7 <- df.q7[df.q7[1] == "Indiana", ]
names(df.q7) <- as.numeric(names(df.q7))
hist.subset <- data.frame()
for (year in seq(2012, 1960, -4)) {
    col.num <- grep(year, colnames(df.q7))[1]
    data.row <- c(as.numeric(year), as.numeric(df.q7[, c(col.num, col.num + 1)]))
    hist.subset <- rbind(hist.subset, data.row)
}
names(hist.subset) <- c("year", "democrat", "republican")

# Claim: Democrates will lose the election with mu<0.5 Null Hypothesis is that
# Republicans will score 50% of voters, therefore win Alternative hypothesis:
# Republicans will get less than 50 % of voters and lose Test to perform at alpha
# 5% significance Ho: mu = 0.5001 Ha: mu < 0.5001
muH0 <- 0.5001
alpha <- 0.95
# by hand: sample mean and sample standard deviation
sample.mu <- mean(hist.subset[, 3])
sample.s <- sd(hist.subset[, 3])
# calculation of the test statistics under Ho; mu=0.5001
ts <- (sample.mu - muH0)/sample.s * sqrt(nrow(hist.subset))
# perform a one sided t-test. Rejection region RR={ts<t value at 95%}, dof of
# t.test are (number of sampled values munis 1)
qt(1 - alpha, nrow(hist.subset))
if (ts > qt(1 - alpha, nrow(hist.subset))) {
    print("Statistical evidence suggests that Ho cannot be rejected at 5%, because the sample does not significantly deviates from mu")
} else {
    print("There is enough evidence to reject Ho at alpha=5 significance level, because sample significantly deviates from mu=0.5")
}

# # Alternatively, we can test using p value ttest <- t.test(x = (hist.subset[,
# 2]), mu = muH0, alternative='less', conf.level = alpha) if (ttest$p.value >
# 0.05) { print('Statistical evidence suggests that Ho cannot be rejected at 5%,
# because the sample does not significantly deviates from mu=0.5') } else {
# print('There is enough evidence to reject Ho, because sample mean significantly
# deviates from mu=0.5') }



# Question 8 Ho: votes are equally distributed between both parties Ha: votes are
# not equally distributed between both parties

# Goodness of fit, we are testing that votes are equally distributed between
# parties this is an equivalent of saying mu = 0.5

voters <- colMeans(hist.subset[, 2:3]) * 100

chi1 <- chisq.test(voters)
chi1

if (chi1$p.value > 0.05) {
    print("Statistical evidence suggests that Ho cannot be rejected at 5%, because the sample does not significantly deviates from mu=0.5")
} else {
    print("There is enough evidence to reject Ho, because sample significantly deviates from mu=0.5")
}



# Question 9
plot.q9 <- function() {
    IN <- filter(df, STATE == "Indiana", is.na(`TOTAL VOTES`))
    IN <- aggregate(IN$`GENERAL %` ~ IN$PARTY, FUN = sum)
    names(IN) <- c("party", "result")
    fit <- rpart(result ~ ., method = "anova", data = IN, control = rpart.control(minsplit = 2, 
        minbucket = 1))
    prp(fit, main = "Regr. Tree based on Party voting turnout")
}

# Question 10
plot.q10 <- function() {
    df.historic <- tab$`Appendix A. Table:  1996-2012 Presidential General Election Percentage of Popular Vote Received by State (Democratic/Republican Nominees)`
    names(df.historic) <- get.appendix.a.column.names(df.historic)
    df.historic <- df.historic[c(8:58), ]
    
    df.q10 <- filter(df.historic, state == state.names[1]) %>% gather(df.q10, "state")
    df.q10 <- cbind(df.q10, t(as.data.frame(strsplit(df.q10[, 2], " - "))))
    df.q10 <- df.q10[, -2]
    rownames(df.q10) <- 1:nrow(df.q10)
    names(df.q10) <- c(names(df.q10)[1], "value", "year", "party")
    
    # Normalizing values
    for (i in seq(1:nrow(df.q10))) {
        if (df.q10[i, "value"] > 1) {
            df.q10[i, "value"] <- as.numeric(df.q10[i, "value"])/100
        }
    }
    # Calculating missing values
    for (s in unique(df.q10$state)) {
        for (y in seq(2012, 1996, -4)) {
            year.df <- filter(df.q10, year == y, state == s)
            df.q10 <- rbind(df.q10, c(s, 1 - sum(as.numeric(year.df[, "value"])), 
                y, "O"))
        }
    }
    
    df.q10$colour[df.q10$party == "R"] <- "red"
    df.q10$colour[df.q10$party == "D"] <- "blue"
    df.q10$colour[df.q10$party == "O"] <- "green"
    plot(df.q10$year, df.q10$value, col = df.q10$colour, ylim = c(0, max(as.numeric(df.q10$value)) * 
        2), main = "Indiana voting patterns", xlab = "Year", ylab = "Proportion of voters")
    
    legend("topright", c("D", "R", "O"), lty = 1, col = c("blue", "red", "green"), 
        bty = "n", cex = 0.5)
    abline(lm(as.numeric(df.q10$value[df.q10$party == "D"]) ~ as.numeric(df.q10$year[df.q10$party == 
        "D"])), col = "blue")
    abline(lm(as.numeric(df.q10$value[df.q10$party == "R"]) ~ as.numeric(df.q10$year[df.q10$party == 
        "R"])), col = "red")
    abline(lm(as.numeric(df.q10$value[df.q10$party == "O"]) ~ as.numeric(df.q10$year[df.q10$party == 
        "O"])), col = "green")
}
