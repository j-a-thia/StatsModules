library(ggplot2)

####   SNIPPET 1   ####
# Params: 
# n = sample size (both groups)
# eff = effect size (set as negative or positive number)
# s = standard deviation
n <- 50
eff <- 0.025
s <- 1

####   SNIPPET 2   ####
# Group:, x or y.
# Group y0 is Ho TRUE, and group y1 is Ho FALSE.
x <- rnorm(n=n, sd=s, mean=50)
y0 <- rnorm(n=n, sd=s, mean=50)
y1 <- rnorm(n=n, sd=s, mean=50+eff)

# Print object contents to screen, they contain
# simulated values.
x
y0
y1

####   SNIPPET 3   ####
# Plotting
plot(ggplot(data.frame(vals=c(x,y0,y1)
                       , group=c(rep('x', n), rep('y0', n), rep('y1', n)))
            , aes(x=vals, fill=group))
     + theme(legend.position='none'
             , strip.text.x = element_text(face='bold'))
     + geom_histogram(colour='black')
     + scale_fill_manual(values=c(x='#737373', y0='#d65cd4', y1='#51d5e1'))
     + facet_wrap(~ group, ncol=1)
     + labs(x='Values', y='Frequency')
     + geom_vline(xintercept=50, linetype='dashed', colour='black')
)

####   SNIPPET 4   ####
# Functions:
# sqrt() = calcualtes square root
# sd() = calculates the standard deviation
# Remember that SD^2 = variance
se0 <- sqrt((sd(x)^2/n)+(sd(y0)^2/n))
se1 <- sqrt((sd(x)^2/n)+(sd(y1)^2/n))

# Means
x.mean <- mean(x)
y0.mean <- mean(y0)
y1.mean <- mean(y1)

# Observed t-scores
t0 <- (x.mean-y0.mean-0)/se0
t1 <- (x.mean-y1.mean-0)/se1


####   SNIPPET 5   ####
# Take a look at the simulated t-statistics
t0 
t1

# P-value: Probability of observing t if Ho = TRUE,
# given the number of degree of freedom (df).

# Two-tailed test
# p-value: simulated difference is 0 (Ho = TRUE).
2*(1-pt(abs(t0), df=n+n-2, lower.tail=TRUE))
# p-value: simulated difference isn't 0 (Ho = FALSE).
2*(1-pt(abs(t1), df=n+n-2, lower.tail=TRUE))


####   EXTRA   ####
# This returns probability to left of t-values
pt(t0, df=n+n-2, lower.tail=TRUE)
pt(t1, df=n+n-2, lower.tail=TRUE)

# This returns probability to the right of t-values
pt(t0, df=n+n-2, lower.tail=FALSE)
pt(t1, df=n+n-2, lower.tail=FALSE)

# Can see results from above code is comparable 
# to that produced from R's built in t-test.
# With actual t-test function: Ho = TRUE
t.test(x, y0, alternative='less', var.equal=TRUE)
t.test(x, y0, alternative='greater', var.equal=TRUE)
t.test(x, y0, alternative='two.sided', var.equal=TRUE)

# With actual t-test function: Ho = FALSE
t.test(x, y1, alternative='less', var.equal=TRUE)
t.test(x, y1, alternative='greater', var.equal=TRUE)
t.test(x, y1, alternative='two.sided', var.equal=TRUE)