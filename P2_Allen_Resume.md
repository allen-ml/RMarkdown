---
title: "Problem Set 2 - Business Analytics with R Markdown"
author: "Allen Bolourchi"
date: "2/01/2021"
output: 
  html_document:
    code_folding: hide
    toc: TRUE
    toc_float: TRUE
---

  
# Part 1: Hypothesis Testing and p values  
  
Our null hypothesis, $H_0$, is a hypothesis that a population parameter $\beta$ takes on some specific value $\beta^*$ (often 0). For example, we may hypothesize that the intercept in the market model is 0 ($H_0: \beta_0 = 0$, where $\beta_0^*$ is 0). The alternative hypothesis, $H_A$, is that the population parameter $\beta$ takes on some value other than $\beta^*$. For example, for the case above, our alternative hypothesis would be that the intercept in the market model is something other than 0 ($H_A: \beta_0 \ne 0$, again where $\beta_0^*$ is 0).  
  
We never directly observe population parameters like $\beta_0$, and our sample estimate will never precisely equal the population parameter. But we can get an assessment of how likely we would be to observe a deviation from our null hypothesis (e.g., $b_0-\beta_0^*$) at least as big as the one we actually observed, if the null hypothesis were true. This depends on the sampling error of our estimate, $s_{b_1}$, and is assessed using the t distribution, where $t = (b_0-\beta_0^*)/s_{b_0}$ (in the case of the intercept) or $t = (b_1-\beta_1^*)/s_{b_1}$ (in the case of the slope).  
  
If the null hypothesis ($H_0$) were true, $|t|$ will usually be < 2. If $|t|$ is sufficiently large (larger than the critical value, $t^*_{N-2,\alpha/2}$), this means that we'd be unlikely to observe a discrepancy this large or larger if the null hypothesis were true. Typically we set our statistical significance level ($\alpha$) to be .05. If the probability of observing an estimate at least this extreme if the null hypothesis were true were less than 5%, then we reject the null hypothesis. The logic amounts to saying "if the null hypothesis were true, we probably would not have observed an estimate this extreme, so the null hypothesis must not be true." For example, for the VGENX Vanguard fund, "if the null hypothesis were true that $\beta_1$ were equal to 0, the chance of observing an estimate $b_1$ that is at least this big given $s_{b_1}$ is considerably smaller than 5%, so we reject the null hypothesis." The probability of observing an estimate at least as extreme as the observed estimate if the null hypothesis were true is the p value.  
  
The p value is NOT the probability the null hypothesis is true. The p value is NOT the probability that our results are due to chance. It is the probability that the null hypothesis would generate an estimate at least as extreme as we observed if the null hypothesis were true.  
  
Hypothesis testing can lead to two kinds of error:  
-Type I Errors: Incorrectly rejecting the null hypothesis when the null hypothesis is true. For example, it could be true that $\beta_1 = 0$ for VGENX, and it was just extraordinarily "unlucky" that we observed a value as discrepant as we really did. This would lead us to incorrectly reject the null hypothesis, even though the null hypothesis is true.  
-Type II Errors: Incorrectly failing to reject the null hypothesis when the null hypothesis is true. For example, it could be true $\beta_0 = .004$ for VGENX, indicating that it earns risk-adjusted excess returns. However, our random sample was not sufficiently different from what we'd expect if the null hypothesis that $\beta_0 = 0$ were true, so we incorrectly fail to reject the null hypothesis that $\beta_0 = 0$, despite the fact the null hypothesis is in fact false, because $\beta_0 = .004$.  
  
## Step 1  
  
First, recreate the `simreg()` function from Week 4 code. As a reminder, this function takes `beta0`, `beta1`, `sigma`, and a vector `x` as arguments, and returns a vector of simulated datapoints given those x's and that model. You can copy and paste the function definition in the code chunk below.  
  
#### Your knitted html does not need to include any output for this step.
```{r}
library(tidyverse)
simreg <- function(beta0, beta1, sigma, x){
  err <- rnorm(length(x), sd = sigma)
  beta0 + beta1 * x + err
}
```
  
## Step 2  
  
Second, define a second function modeled after `gen_sample_coef()` from Week 4. (You may find it useful to copy-paste the function definition in the code chunk below, and then make adjustments to it.) However, whereas `gen_sample_coef()` returned a sample **slope**, your function here should return a **vector of length two**, where the first element is the sample **intercept** and the second element is **$s_{b_0}$**. Note that using `coef(m_sim)` won't work here, as it does not return the standard error. Instead, you can use `coef(summary(m_sim))[1, 1:2]`. This will take the coefficients table from the summary output (which does include the standard errors) and returns the first and second columns of the first row, in other words, the estimate and standard error for the intercept.  
  
#### Your knitted html does not need to include any output for this step.
```{r}
gen_sample_coef <- function(beta0_to_sim, beta1_to_sim, sigma_to_sim, xs){
  ys <- simreg(beta0 = beta0_to_sim, beta1 = beta1_to_sim, 
               sigma = sigma_to_sim, x = xs)
  m_sim <- lm(ys ~ xs)
  coef(summary(m_sim))[1, 1:2]
}
```
  
## Step 3  
  
Now, use `replicate()` to generate 1,000 estimates of the intercept and its standard error given $\beta_0 = 0$, $\beta_1 = 1$, $\sigma = .025$, and x given by the 436 `vwretd` values from our usual `join_van` dataset. (Note that you'll need to recreate the `join_van` dataset here from the `Vanguard` and `marketRf` datasets before you can use it, just like we did in class in weeks 4 and 5. If you haven't restarted R recently, your code may appear to work, if `join_van` is in your environment, but then the .rmd file will not knit, as it won't generate `join_van` on its own.)  
  
When `replicate()` uses a function that returns more than a single element, the formatting isn't quite as easy to work with. First, review how we've used replicate in Weeks 4 and 5 in class. Then, I recommend using the following adjustments when you call `replicate()`:  
  
`out_df <- as.data.frame(t(replicate(YOUR REPLICATE ARGUMENTS (N AND FUNCTION) GO HERE)))`  
`colnames(out_df) <- c("b0", "sb0")`  
  
The first line takes the output of replicate, transposes it (using the `t()` function), then makes it into a `data.frame` and assigns it to `out_df`. The second line renames the variables, by renaming the column names.    
  
#### Your knitted html does not need to include any output for this step.  
```{r}
Vanguard <- read_csv("Vanguard.csv")
marketRf <- read_csv("marketRf.csv")
wide_van <- Vanguard %>%
  select(date, ticker, mret) %>%
  pivot_wider(names_from = ticker, values_from = mret)
join_van <- left_join(wide_van, select(marketRf, date, vwretd), by = "date")

num_samples <- 1000
truebeta0 <- 0
truebeta1 <- 1
truesigma <- 0.025

out_df <- as.data.frame(t(replicate(num_samples, 
                   gen_sample_coef(beta0_to_sim = truebeta0, beta1_to_sim = truebeta1, 
                                   sigma_to_sim = truesigma, xs = join_van$vwretd))))
colnames(out_df) <- c("b0", "sb0")
```
  
## Step 4  
  
This should give you a data frame of 1,000 coefficient estimates (as variable `b0`) and their corresponding 1,000 standard errors (as variable `sb0`). You may want to inspect `out_df` at this point to make sure it looks reasonable. Remember that these were generated by simulating data where the true intercept is 0. Taking the null hypothesis that $\beta_0 = 0$, you can use those simulated values to calculate the corresponding 1,000 $t$ statistics as the ratio of $b_0$ to $s_{b_0}$. Store those $t$ statistics in a new variable in the same data frame called `t_stat` and make a histogram of `t_stat` using `qplot()`.  
  
#### Your knitted html should include that histogram in its output.  
```{r}
out_df <- mutate(out_df,t_stat=b0/sb0)
p1<-qplot(out_df$t_stat,xlab="t-statistics")
```

## Step 5  
  
Calculate the critical value of $t$. You can calculate this using the `qt()` function discussed in class in Week 5. For $\alpha = .05$, consider how much mass should be in the right tail and how much in the left tail, and how many degrees of freedom you need. (Remember that your answer for the critical value should be very close to 2. **If it's not near 2, it's wrong.**)  
  
#### Your knitted html should display the critical value of $t$.  
```{r}
qt(.975, df = 434)
qt(.025, df = 434)
ggplot(data = out_df, aes(x = t_stat)) + 
  geom_histogram() + 
  xlab("Simulated t-stat") + 
  ylab("Count") + 
  geom_vline(xintercept = qt(.975, df = 434),color="red")+geom_vline(xintercept = qt(0.025, df = 434),color="red")

#(p2 <- p1+qplot(qt(.975, df = 998)*rep(1, 10000),c(1:10000),color = "red"))
```
  
## Step 6  
  
On what proportion of samples did you reject the null hypothesis? That is, on what proportion of samples did you find $|t|$ to be greater than the absolute value of the critical value calculated in Question 4? If you use the command `abs(x) > qt(p, df)`, you'll get a vector of 1,000 `TRUE` and `FALSE` values where it takes a value of `TRUE` if `abs(x)` is greater than `qt(p, df)` and it takes a value of `FALSE` if `abs(x)` is less than or equal to `qt(p, df)`. Because `TRUE` is treated as 1 and `FALSE` is treated as 0, taking the average of that vector will give you the proportion of cases where the inequality holds. Your answer ought to be close to 0.05.  
  
#### Your knitted html should display that proportion.  
```{r}
beyond_lim<-abs(out_df$t_stat) > qt(.975, df = 434)
mean(beyond_lim)
```

## Step 7  
  
Now repeat that process using `gen_sample_coef()` below to simulate 1,000 new samples of returns for the case where $\beta_0 = .002$ rather than $\beta_0 = 0$, but all other inputs are the same. On what proportion of these new samples do you reject the null hypothesis? Consider what this means for your ability to detect whether a fund manager is in fact generating a small amount of alpha (though you don't need to include that consideration in your answer.)  
  
#### Your knitted html should display that proportion.  
```{r}
num_samples <- 1000
truebeta0 <- 0.002
truebeta1 <- 1
truesigma <- 0.025

out_df2 <- as.data.frame(t(replicate(num_samples, 
                   gen_sample_coef(beta0_to_sim = truebeta0, beta1_to_sim = truebeta1, 
                                   sigma_to_sim = truesigma, xs = join_van$vwretd))))
colnames(out_df2) <- c("b0", "sb0")

out_df2 <- mutate(out_df2,t_stat=b0/sb0)
#qplot(out_df2$t_stat,xlab="t-statistics")

#qt(.975, df = 998)
#qt(.025, df = 998)

beyond_lim<-abs(out_df2$t_stat) > qt(.975, df = 434)
mean(beyond_lim)

ggplot(data = out_df2, aes(x = t_stat)) + 
  geom_histogram() + 
  xlab("Simulated t-stat") + 
  ylab("Count") + 
  geom_vline(xintercept = qt(.975, df = 434),color="red")+geom_vline(xintercept = qt(0.025, df = 434),color="red")
```

## Step 8  
  
Finally, write a new function `gen_sample_confint()` that is like `gen_sample_coef()`, but instead of capturing the estimate and standard error each time, it captures the bounds of the 95% confidence interval for the intercept each time. (Remember that `confint(m)[1, 1]` will give you the lower-bound of the 95% confidence interval for the intercept of `m` and `confint(m)[1, 2]` will give you the upper-bound of the 95% confidence interval for the intercept of `m`.) Simulate 1,000 confidence intervals. What proportion of your 95% confidence intervals include 0 when $\beta_0 = .002$? Consider how this value relates to the likelihood of rejecting the null hypothesis you found above (though you don't need to include that consideration in your answer.)  
  
#### Your knitted html should display that proportion.
```{r}
gen_sample_confint <- function(beta0_to_sim, beta1_to_sim, sigma_to_sim, xs){
  ys <- simreg(beta0 = beta0_to_sim, beta1 = beta1_to_sim, 
               sigma = sigma_to_sim, x = xs)
  m_sim <- lm(ys ~ xs)
  confint(m_sim)[1, 1:2]
}
num_samples <- 1000
truebeta0 <- 0.002
truebeta1 <- 1
truesigma <- 0.025

out_df3 <- as.data.frame(t(replicate(num_samples, 
                   gen_sample_confint(beta0_to_sim = truebeta0, beta1_to_sim = truebeta1, 
                                   sigma_to_sim = truesigma, xs = join_van$vwretd))))
colnames(out_df3) <- c("conf_25", "conf_975")

temp_new<-out_df3$conf_25 < 0 & out_df3$conf_975 > 0
mean(out_df3$conf_25 < 0 & out_df3$conf_975 > 0)
print("This number is apprximately 1-0.35 that we found earlier. Meaning that 0 will be found only 65% in the confidence interval. Therefore, we accept the null hypothesis 65%.")
```

# Part 2: Power Analyses

Statistical power refers to the probability of rejecting the null hypothesis given an effect of a particular size. For example, in Part 1, our answer to Step 7 (our probability of rejecting the null when $\beta_0 = .002$) represents our approximate statistical power to reject the null hypothesis when $\beta_0 = .002$.  
  
Consider the sample size calculation for the Ad experiment in Week 4. We used a back-of-the-envelope calculation that a given effect size won't be statistically significant if it's not at least two standard errors away from 0 because the critical value for $t$ is about 2. But that calculated N does not guarantee we'll reject the null hypothesis. Given the sampling distribution of $b_1$, we'll have some probability of rejecting the null hypothesis and some probability of failing to reject the null hypothesis, as discussed in Week 5. This leads to two questions that we'll address here (these two questions are rhetorical; don't submit answers to them):  
  
1. Given some $\beta_1$, $\sigma$, and N, how likely would we be to reject the null hypothesis?  
2. What N would we need in order to have a strong chance, say 80%, of rejecting the null hypothesis, given $\beta_1$ and $\sigma$?  
  
Let's start by specifying a model for sales, where each observation is log(dollar sales) for one store on one day:  
$log(Sales_i) = \beta_0 + \beta_1 Ad_i + \epsilon_i$  
$Ad_i$ is 1 if a store-day has an ad, 0 if not. Let's take $\beta_0 = 7$ and $\beta_1 = .04$. This means that expected log(sales) in the control group (when $Ad_i = 0$) are 7, so raw sales are $e^7$ or about \$1100. Given $\beta_1 = .04$, we simulate data as though the true effect of advertising is a 4% lift in sales. We'll set $\sigma = 0.85$ to be similar to the detergent data.  
  
First we want to ask the question: "If we had a sample of 1,000 observations, how likely would we be to reject the null hypothesis?" One way to answer this question is through simulation. We can use a similar approach to that we used in Part 1.  
  
## Step 1  
    
Generate a vector `ads` containing 0's and 1's using the following code:  
`num_obs <- 1000`  
`ads <- rep(c(0, 1), each = num_obs / 2)`  
That effectively says "Set `num_obs` (number of observations) equal to 1000. Create a vector `ads` consisting of replicating the vector (0, 1), where each element is repeated 1000 / 2 = 500 times." Consider how that vector looks different from the vector you'd create if you used `times = n/2` instead of `each = n/2`. (I recommend testing it out, but you don't need to report it.)  
  
#### Your knitted html does not need to include any output for this step.  
```{r}
num_obs <- 1000
ads <- rep(c(0, 1), each = num_obs / 2) # each does 500 zeros and 500 ones. Times alternates zeros and ones.
```
  
## Step 2  
  
Following the logic in Part 1, create a function that simulates data (using `simreg()` from Part 1 Step 1) and returns $b_1$ and $s_{b_1}$ (like Part 1 Step 2 did for the intercept), now using our new parameters and our `ads` vector as x. Generate 1,000 estimates of $b_1$ and corresponding $s_{b_1}$s. Calculate the corresponding $t$s. (Throughout this section, pay careful attention to where a value represents **the number of observations in a simulated dataset** and where a value represents **the number of datasets that you are simulating**. This is a distinction that has tripped students up in the past.)  
  
#### Your knitted html does not need to include any output for this step.  
```{r}
simreg <- function(beta0, beta1, sigma, x){
  err <- rnorm(length(x), sd = sigma)
  beta0 + beta1 * x + err
}
gen_sample_coef <- function(beta0_to_sim, beta1_to_sim, sigma_to_sim, xs){
  ys <- simreg(beta0 = beta0_to_sim, beta1 = beta1_to_sim, 
               sigma = sigma_to_sim, x = xs)
  m_sim <- lm(ys ~ xs)
  coef(summary(m_sim))[2, 1:2] # returns b_1 and s_{b_1}
}

num_samples <- 1000
truebeta0 <- 7
truebeta1 <- 0.04
truesigma <- 0.85

out_df4 <- as.data.frame(t(replicate(num_samples, 
                   gen_sample_coef(beta0_to_sim = truebeta0, beta1_to_sim = truebeta1, 
                                   sigma_to_sim = truesigma, xs = ads))))
colnames(out_df4) <- c("b1", "sb1")

out_df4 <- mutate(out_df4,t_stat=(b1-0)/sb1)
#p1<-qplot(out_df$t_stat,xlab="t-statistics")
ggplot(data = out_df4, aes(x = t_stat)) + 
  geom_histogram() + 
  xlab("Simulated t-stat") + 
  ylab("Count") + 
  geom_vline(xintercept = qt(.975, df = 998),color="red")+geom_vline(xintercept = qt(0.025, df = 998),color="red")

```

## Step 3  
  
Report the proportion of samples that reject the null hypothesis that the ad campaign had no effect. This answers the question: "If the true effect of advertising were 4% lift, given $\sigma = 0.85$ and 1,000 store-days, how likely would we be to reject the null hypothesis that advertising had no effect ($H_0:\beta_1=0$)?" This is a measure of statistical power. (Remember that our degrees of freedom are no longer 434)! 
  
#### Your knitted html should include that proportion.
```{r}
print("That is, on what proportion of samples did you find $|t|$ to be greater than the absolute value of the critical value calculated using 95% confidence interval?")
beyond_lim<-abs(out_df4$t_stat) > qt(.975, df = 998)
mean(beyond_lim)
print(paste0("we reject the null hypothesis ",mean(beyond_lim)*100,"% of the times with N=",num_samples))
```

## Step 4  
  
Now vary the number of observations to see how it changes our probability of detecting an effect: try two new values of `num_obs`: 100 and 10,000. What is our statistical power in each case? For each value of N (100 and 10,000), report:  
a. The mean value of $b_1$  
b. The mean value of $s_{b_1}$  
c. The proportion of samples on which we reject the null hypothesis.  
  
#### Your knitted html should include the values for (a), (b), and (c) for N (num_obs) = 100 and N (num_obs) = 10,000.
```{r}
num_obs <- 100
ads <- rep(c(0, 1), each = num_obs / 2) # each does 50 zeros and 50 ones. Times alternates zeros and ones.

num_samples <- 1000
truebeta0 <- 7
truebeta1 <- 0.04
truesigma <- 0.85

out_df5 <- as.data.frame(t(replicate(num_samples, 
                   gen_sample_coef(beta0_to_sim = truebeta0, beta1_to_sim = truebeta1, 
                                   sigma_to_sim = truesigma, xs = ads))))
colnames(out_df5) <- c("b1", "sb1")

out_df5 <- mutate(out_df5,t_stat=(b1-0)/sb1)
#p1<-qplot(out_df$t_stat,xlab="t-statistics")
ggplot(data = out_df5, aes(x = t_stat)) + 
  geom_histogram() + 
  xlab("Simulated t-stat") + 
  ylab("Count") + 
  geom_vline(xintercept = qt(.975, df = num_obs-2),color="red")+geom_vline(xintercept = qt(0.025, df = num_obs-2),color="red")

print("That is, on what proportion of samples did you find $|t|$ to be greater than the absolute value of the critical value calculated using 95% confidence interval?")
beyond_lim<-abs(out_df5$t_stat) > qt(.975, df = num_obs-2)
mean(beyond_lim)
print(paste0("The mean value of b1: ",mean(out_df5$b1)))
print(paste0("The mean value of sb1: ",mean(out_df5$sb1)))
print(paste0("we reject the null hypothesis ",mean(beyond_lim)*100,"% of the times with number of observations N=",num_obs))


num_obs <- 10000
ads <- rep(c(0, 1), each = num_obs / 2) # each does 5000 zeros and 5000 ones. Times alternates zeros and ones.
num_samples <- 1000
truebeta0 <- 7
truebeta1 <- 0.04
truesigma <- 0.85

out_df5 <- as.data.frame(t(replicate(num_samples, 
                   gen_sample_coef(beta0_to_sim = truebeta0, beta1_to_sim = truebeta1, 
                                   sigma_to_sim = truesigma, xs = ads))))
colnames(out_df5) <- c("b1", "sb1")

out_df5 <- mutate(out_df5,t_stat=(b1-0)/sb1)
#p1<-qplot(out_df$t_stat,xlab="t-statistics")
ggplot(data = out_df5, aes(x = t_stat)) + 
  geom_histogram() + 
  xlab("Simulated t-stat") + 
  ylab("Count") + 
  geom_vline(xintercept = qt(.975, df = num_obs-2),color="red")+geom_vline(xintercept = qt(0.025, df = num_obs-2),color="red")

print("That is, on what proportion of samples did you find $|t|$ to be greater than the absolute value of the critical value calculated using 95% confidence interval?")
beyond_lim<-abs(out_df5$t_stat) > qt(.975, df = num_obs-2)
mean(beyond_lim)
print(paste0("The mean value of b1: ",mean(out_df5$b1)))
print(paste0("The mean value of sb1: ",mean(out_df5$sb1)))
print(paste0("we reject the null hypothesis ",mean(beyond_lim)*100,"% of the times with number of observations N=",num_obs))


```

## Step 5  
  
By testing different values of `num_obs`, observe what value you need to have roughly an 80% chance of rejecting the null hypothesis, in other words, 80% power. Only include that final vale of `num_obs` and simulation of 1,000 samples in the code below, though you may need to try several values to "guess and check". Remember that each time you run this code, you'll get a slightly different answer because you're simulating data: if true power is .80, sometimes you'll get .78, sometimes .81, etc., so the goal is to find a value that gives you between .75 and .85 or so, not to get *exactly* .80.  
  
#### Your knitted html should include the value of N (num_obs) and the proportion of simulations on which you reject the null hypothesis given that value of `num_obs`.
```{r}



num_obs <- 14200
ads <- rep(c(0, 1), each = num_obs / 2)
num_samples <- 1000
truebeta0 <- 7
truebeta1 <- 0.04
truesigma <- 0.85

out_df5 <- as.data.frame(t(replicate(num_samples, 
                   gen_sample_coef(beta0_to_sim = truebeta0, beta1_to_sim = truebeta1, 
                                   sigma_to_sim = truesigma, xs = ads))))
colnames(out_df5) <- c("b1", "sb1")

out_df5 <- mutate(out_df5,t_stat=(b1-0)/sb1)
#p1<-qplot(out_df$t_stat,xlab="t-statistics")
 ggplot(data = out_df5, aes(x = t_stat)) + 
   geom_histogram() + 
   xlab("Simulated t-stat") + 
   ylab("Count") + 
   geom_vline(xintercept = qt(.975, df = num_obs-2),color="red")+geom_vline(xintercept = qt(0.025, df = num_obs-2),color="red")

#print("That is, on what proportion of samples did you find $|t|$ to be greater than the absolute value of the critical value calculated using 95% confidence interval?")
beyond_lim<-abs(out_df5$t_stat) > qt(.975, df = num_obs-2)
#mean(beyond_lim)
print(paste0("The mean value of b1: ",mean(out_df5$b1)))
print(paste0("The mean value of sb1: ",mean(out_df5$sb1)))
print(paste0("we reject the null hypothesis ",mean(beyond_lim)*100,"% of the times with number of observations N=",num_obs))

```
