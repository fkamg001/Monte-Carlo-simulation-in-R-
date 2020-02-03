# I confirm that the following report and associated code is my own work, 
# except where clearly indicated. 
# loading required libraries
install.packages("dslabs") 
help(package = "dslabs")
library("dslabs") 
library('dplyr')
library('ggplot2')
library('paramtest')
library('pwr')
library('knitr')


# loading the opinion polls data into R
data("polls_us_election_2016")
summary(polls_us_election_2016)

# Cleaning the dataset 
polls.election.2016 <- polls_us_election_2016 %>%
  # Renaming columns associated with Trump and Clinton polls to clarify 
  rename('Rawpolls_Trump' = rawpoll_trump,'Rawpolls_Clinton' = rawpoll_clinton) %>%
  # Removing other unrequired columns from the dataset
  select(-rawpoll_johnson, -rawpoll_mcmullin, -adjpoll_johnson, 
         -adjpoll_mcmullin, -adjpoll_trump, -adjpoll_clinton)

# Conducting preliminary analysis
head(polls.election.2016)
str(polls.election.2016)
summary(polls.election.2016)
View(polls.election.2016)


# Checking assumptions of the dataset
qqnorm(polls.election.2016$Rawpolls_Clinton) 
qqline(polls.election.2016$Rawpolls_Clinton, col ='red')
shapiro.test(polls.election.2016$Rawpolls_Clinton)

qqnorm(polls.election.2016$Rawpolls_Trump) 
qqline(polls.election.2016$Rawpolls_Trump, col = 'red')
shapiro.test(polls.election.2016$Rawpolls_Trump)

# Independence: 
# Conducting Chisqaure independence test 
chisq.test(polls.election.2016$Rawpolls_Trump,polls.election.2016$Rawpolls_Clinton)

# Evaluating the sample variances of the two groups, 
# using a Fisher's F-test to verify the homoskedasticity (homogeneity of variances)
var.test(polls.election.2016$Rawpolls_Trump,polls.election.2016$Rawpolls_Clinton)

# Plotting the histograms of polls
hist(polls.election.2016$Rawpolls_Clinton, probability = TRUE, 
     xlab = "Percentage for Hillary Clinton",
     main = "Distribution of Raw Polls", col = 'skyblue3',pch = 16 )
lines(density(polls.election.2016$Rawpolls_Clinton), col = "darkblue", lwd = 3)


hist(polls.election.2016$Rawpolls_Trump, probability = TRUE, 
     xlab = "Percentage for Donald Trump",
     main = "Distribution of Raw Polls", col = 'brown2',pch = 16)
lines(density(polls.election.2016$Rawpolls_Trump), col = "brown4", lwd = 3)

# Conducting the Wilcoxon Signed-Rank Test (non-parametric test) 
# Without assuming the data to have normal distribution
# Testing at 0.05 significance level 

wilcox.test(polls.election.2016$Rawpolls_Clinton, polls.election.2016$Rawpolls_Trump, 
            paired = T)

# Performing parametric t-test on two gropus
t.test(polls.election.2016$Rawpolls_Clinton, polls.election.2016$Rawpolls_Trump, 
       var.equal=TRUE) 

# Calculating means and standard deviations of each group
mean.Rawpolls.Clinton <- mean(polls.election.2016$Rawpolls_Clinton)
Sd.Rawpolls.Clinton <- sd(polls.election.2016$Rawpolls_Clinton)

mean.Rawpolls.Trump <- mean(polls.election.2016$Rawpolls_Trump)
Sd.Rawpolls.Trump <- sd(polls.election.2016$Rawpolls_Trump)

##--------------------------------Simulation(Power Calculation)-------------------------------------------

test_func <- function(simNum, N, d) {
# Purpose: Calculating the estimated power analytically usung simulation. 
# Simulations repeatedly generate random data set based on the predefined model. 
# It then analyze each data set and count the proportion of results that are significant. 
# That proportion is the estimated power for the model.

# Input:   
# creating a function that simulates normally distributed data for two groups
  
  x1 <- rnorm(N, mean.Rawpolls.Clinton, Sd.Rawpolls.Trump)
  x2 <- rnorm(N, mean.Rawpolls.Trump, Sd.Rawpolls.Trump)
  
  # Performing non-parametric t-test on the generated dataset 
  t <- wilcox.test(x1, x2, paired = TRUE)  
  stat <- t$statistic
  p <- t$p.value

# Output: Returning t-statistic and p-value as a named vector
# Along with a boolean value determining whether the test is significant or not
  return(c(t=stat, p=p, sig=(p < 0.05)))
}

# Summarising the 'sig' value, which (by default) calculates the mean 
# Giving the proportion of simulations that were significant 
# Using the grid_search() to run the function repeatedly and collating the results
# The grid_search() function allows to run the function iteratively with varying parameters
# Testing at N = 25, N = 50, and N = 100
power.t_test.vary2 <- grid_search(test_func, 
                                  params=list(N = c(100,250,750), 
                                  d=c(0.2, 0.5,0.8)), n.iter=1000, output='data.frame')

# Calculating the power of the simulated data
power_nonparametric <- results(power.t_test.vary2) %>%
  group_by(N.test, d.test) %>%
  summarise(power=mean(sig))

# Presenting the outcome in a table
kable(power_nonparametric)

# Plotting the results for non-parametric t-test 
Plot.non.parametric.test <- ggplot(power_nonparametric, aes(x=N.test, 
                                                        y=power, 
                                                        group = factor(d.test), 
                                                        colour = factor(d.test))) +
  geom_point() + geom_line() + ggtitle("Wilcoxon-Test") + ylim(c(0.4, 1)) +
  labs(x='Sample Size', y='Power', colour="Cohen's d") +
  theme_grey(base_size = 14)
print(Plot.non.parametric.test)

test_func.parametric <- function(simNum, N, d) {
# Purpose: Calculating the estimated power analytically usung simulation 
# Simulations repeatedly generate random data set based on the predefined model 
# It then analyze each dataset and count the proportion of results that are significant 
# That proportion is the estimated power for the model
  
# Input:   
# Creating a function that simulates normally distributed data for two groups
  
  x1 <- rnorm(N, mean.Rawpolls.Clinton, Sd.Rawpolls.Trump)
  x2 <- rnorm(N, mean.Rawpolls.Trump, Sd.Rawpolls.Trump)

# Performing parametric t-test on the generated dataset  
  t <- t.test(x1, x2, var.equal = TRUE)  
  stat <- t$statistic
  p <- t$p.value

# Output: Returning t-statistic and p-value as a named vector
# Along with a boolean value determining whether the test is significant or not  
  return(c(t=stat, p=p, sig=(p < 0.05)))
  
}


cohens_d <- function(x, x2) {
  lx <- length(x1)- 1
  ly <- length(x2)- 1
# Calculating mean difference (numerator)
  md  <- abs(mean(x1) - mean(x2))        
# Calculating the common standard deviation
  csd <- lx * var(x1) + ly * var(x2)
  csd <- csd/(lx + ly)
  csd <- sqrt(csd)                     
# Returning cohen's d  
  cd  <- md/csd                        
}
res <- cohens_d(x1, x2)

# Using the grid_search() function to run the function iteratively with varying parameters
power_ttest_parametric <- grid_search(test_func.parametric, params=list(N=c(100,250,750), 
                                                                     d=c(0.2,0.5,0.8)),
                                      n.iter=1000, output='data.frame')
# Calculating the power of the simulated data for parametric t-test
power_parametric <- results(power_ttest_parametric) %>%
  group_by(N.test, d.test) %>%
  summarise(power=mean(sig))
# Viewing the result in a table
kable(power_parametric)

# Plotting the power under different sitations
Plot.parametric.test <- ggplot(power_parametric, aes(x = N.test, 
                                           y = power, 
                                           group = factor(d.test), 
                                           colour = factor(d.test))) +
  geom_point() + geom_line() + ggtitle("Two Sample t-test") + ylim(c(0.4, 1)) +
  labs(x='Sample Size', y='Power', colour="Cohen's d") +
  theme_grey(base_size = 14)
print(Plot.parametric.test)
#-----------------------------Simulation(size calculation)-----------------------------------

test_func_parametric <- function(simNum, N, d) {
  # Purpose: Calculating the estimated size analytically usung simulation 
  # Simulations repeatedly generate random data set based on the predefined model 
  # It then analyze each dataset and count the proportion of results that are significant 
  # That proportion is the estimated size of the model
  
  # Input:   
  # Creating a function that simulates normally distributed data for two groups
  
  x1 <- rnorm(N, mean.Rawpolls.Clinton, Sd.Rawpolls.Trump)
  x2 <- rnorm(N, mean.Rawpolls.Trump, Sd.Rawpolls.Trump)
  
  # Performing parametric t-test on the generated dataset  
  t <- t.test(x1, x2, var.equal = TRUE)  
  stat <- t$statistic
  p <- t$p.value
  
  # Output: Returning t-statistic and p-value as a named vector
  # Along with a boolean value determining whether the test is significant or not  
  return(c(t=stat, p=p, sig=(p > 0.05)))
  
}

# Calculating the power of the simulated data for parametric t-test
size_ttest.parametric <- grid_search(test_func_parametric, params=list(N=c(100,250,750),
                                                                       d='constant'), 
                          n.iter=1000, output='data.frame')
Size_parametric <- results(size_ttest.parametric) %>%
  group_by(N.test, d.test) %>%
  summarise(size=mean(sig))

# Viewing the result in a table
kable(Size_parametric)

# Plotting the size under different sitations
Plot.Size <- ggplot(Size_parametric, aes(x = N.test, y = size, 
                                         group = factor(d.test), colour = factor(d.test))) +
  geom_point() + geom_line() + ggtitle("Two Sample t-test") + ylim(c(0, 1)) +
  labs(x='Sample Size', y='Size', colour="Effect size") +
  theme_grey(base_size = 14)

print(Plot.Size)


test_func_nonparametric <- function(simNum, N, d) {
  # Purpose: Calculating the estimated size analytically usung simulation 
  # Simulations repeatedly generate random data set based on the predefined model 
  # It then analyze each dataset and count the proportion of results that are significant 
  # That proportion is the estimated size of the model
  
  # Input:   
  # Creating a function that simulates normally distributed data for two groups
  
  x1 <- rnorm(N, mean.Rawpolls.Clinton, Sd.Rawpolls.Trump)
  x2 <- rnorm(N, mean.Rawpolls.Trump, Sd.Rawpolls.Trump)
  
  # Performing parametric t-test on the generated dataset  
  t <- wilcox.test(x1, x2, var.equal = TRUE)  
  stat <- t$statistic
  p <- t$p.value
  
  # Output: Returning t-statistic and p-value as a named vector
  # Along with a boolean value determining whether the test is significant or not  
  return(c(t=stat, p=p, sig=(p > 0.05)))
  
}

# Calculating the power of the simulated data for parametric t-test
size_ttest.nonparametric <- grid_search(test_func_nonparametric, params=list(N=c(100,250,750),
                                                                             d='constant'), 
                                     n.iter=1000, output='data.frame')
Size_nonparametric <- results(size_ttest.nonparametric) %>%
  group_by(N.test, d.test) %>%
  summarise(size=mean(sig))

# Viewing the result in a table
kable(Size_nonparametric)

# Plotting the power under different sitations
Plot.Size.non <- ggplot(Size_nonparametric, aes(x = N.test, y = size, 
                                                group = factor(d.test), colour = factor(d.test))) +
  geom_point() + geom_line() + ggtitle("Wilcoxon t-test") + ylim(c(0, 1)) +
  labs(x='Sample Size', y='Size', colour="Effect size") +
  theme_grey(base_size = 14)
print(Plot.Size.non)


##-------------------------------(Simuating non-normal data)----------------------------------
test_func.nonnormal <- function(simNum, N, d) {
  # Purpose: Calculating the estimated power analytically usung simulation 
  # Simulations repeatedly generate chisquare data set based on the predefined model 
  # It then analyze each dataset and count the proportion of results that are significant 
  # That proportion is the estimated size of the model
  
  # Input:   
  # Creating a function that simulates chisquare distributed data for two groups
  
  x1 <- rchisq(N, mean.Rawpolls.Clinton, Sd.Rawpolls.Trump)
  x2 <- rchisq(N, mean.Rawpolls.Trump, Sd.Rawpolls.Trump)
  
  # Performing parametric t-test on the generated dataset  
  t <- wilcox.test(x1, x2, paired = TRUE)  
  stat <- t$statistic
  p <- t$p.value
  
  # Output: Returning t-statistic and p-value as a named vector
  # Along with a boolean value determining whether the test is significant or not  
  return(c(t=stat, p=p, sig=(p < 0.05)))
  
}

# Calculating the power of the simulated data for non-parametric t-test
power_ttest.nonnormal <- grid_search(test_func.nonnormal, params=list(N=c(100,250,750), 
                                                                      d=c(0.2,0.5,0.8)),
                                     n.iter=1000, output='data.frame')
power.chisq <- results(power_ttest.nonnormal) %>%
  group_by(N.test, d.test) %>%
  summarise(power=mean(sig))

# Viewing the result in a table
kable(power.chisq)

# Plotting the power under different sitations
Plot.Power.nonnormal <- ggplot(power.chisq, aes(x = N.test, y = power, group = factor(d.test), colour = factor(d.test))) +
  geom_point() + geom_line() + ggtitle("t-Test") + ylim(c(0, 1)) +
  labs(x='Sample Size', y='Power', colour="Effect Size") +
  theme_minimal() 
print(Plot.Power.nonnormal)

test_func.para.nonnoraml <- function(simNum, N, d) {
  # Purpose: Calculating the estimated power analytically usung simulation 
  # Simulations repeatedly generate chisquare data set based on the predefined model 
  # It then analyze each dataset and count the proportion of results that are significant 
  # That proportion is the estimated size of the model
  
  # Input:   
  # Creating a function that simulates chisquare distributed data for two groups
  
  x1 <- rchisq(N, mean.Rawpolls.Clinton, Sd.Rawpolls.Trump)
  x2 <- rchisq(N, mean.Rawpolls.Trump, Sd.Rawpolls.Trump)
  
  # Performing parametric t-test on the generated dataset  
  t <- t.test(x1, x2, var.equal = TRUE)  
  stat <- t$statistic
  p <- t$p.value
  
  # Output: Returning t-statistic and p-value as a named vector
  # Along with a boolean value determining whether the test is significant or not  
  return(c(t=stat, p=p, sig=(p < 0.05)))
  
}

# Calculating the power of the simulated data for parametric t-test
power_ttest.para.nonnormal <- grid_search(test_func.para.nonnoraml, params=list(N=c(100,250,750), 
                                                                      d=c(0.2,0.5,0.8)),
                                     n.iter=1000, output='data.frame')
power.chisq.para <- results(power_ttest.para.nonnormal) %>%
  group_by(N.test, d.test) %>%
  summarise(power=mean(sig))

# Viewing the result in a table
kable(power.chisq.para)

# Plotting the power under different sitations
Plot.Power.chisq.para <- ggplot(power.chisq.para, aes(x = N.test, y = power, group = factor(d.test), colour = factor(d.test))) +
  geom_point() + geom_line() + ggtitle("t-Test") + ylim(c(0, 1)) +
  labs(x='Sample Size', y='Power', colour="Effect Size") +
  theme_minimal() 
print(Plot.Power.chisq.para)
#---------------------------------------------------------------------------------------------  
# Graphically showing the Type I and II errors 
x <- seq(-4, 4, length=1000)
hx <- dnorm(x, mean=0, sd=1)
plot(x, hx, type="n", xlim=c(-4, 8), ylim=c(0, 0.5), 
     ylab = "",
     xlab = "",
     main= expression(paste("Type II (", beta, ") error")), axes=FALSE)
axis(1, at = c(-qnorm(0.025), 0, -4), 
     labels = expression("P-value", 0, -infinity ))

shift = qnorm(1-0.025, mean=0, sd=1)*1.7
xfit2 <- x + shift
yfit2 <- dnorm(xfit2, mean=shift, sd=1)

# Printing the null hypothesis area
col_null = "#DDDDDD"
polygon(c(min(x), x,max(x)), c(0,hx,0), col=col_null)
lines(x, hx, lwd=2)

# The alternative hypothesis area
# The red - underpowered area
lb <- min(xfit2)
ub <- round(qnorm(.975),2)
col1 = "#CC2222"

i <- xfit2 >= lb & xfit2 <= ub
polygon(c(lb,xfit2[i],ub), c(0,yfit2[i],0), col=col1)

# The green area shows the power
col2 = "#22CC22"
i <- xfit2 >= ub
polygon(c(ub,xfit2[i],max(xfit2)), c(0,yfit2[i],0), col=col2)

# Outline the alternative hypothesis
lines(xfit2, yfit2, lwd=2)

axis(1, at = (c(ub, max(xfit2))), labels=c("", expression(infinity)), 
     col=col2, lwd=1, lwd.tick=FALSE)

abline(v=ub, lwd=2, col="#000088", lty="dashed")
arrows(ub, 0.45, ub+1, 0.45, lwd=3, col="#008800")
arrows(ub, 0.45, ub-1, 0.45, lwd=3, col="#880000")

# Printing null hypothesis area
col_null = "#AAAAAA"
polygon(c(min(x), x,max(x)), c(0,hx,0), col=col_null, lwd=2, 
        density=c(10, 40), angle=-45, border=0)
lines(x, hx, lwd=2, lty="dashed", col=col_null)
legend("topleft", inset=0.015, title="Color",
       c("Null hypothesis","Type II error", "True"), fill=c(col_null, col1, col2), 
       angle=-45,
       density=c(20, 1000, 1000), horiz=F)

