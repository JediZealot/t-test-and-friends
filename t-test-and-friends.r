

####################################################
#t-test and friends
####################################################




#####################################################
### 1. t test from scratch
#####################################################

## In this exercise, we will use the dataset trees (You can see it by typing "trees").
## The dataset contains the height of a sample of black cherry trees. Imagine than in fact 
## some of the cherry trees were inhabited by woodpeckers, while others were not.
## The following line codes which ones had woodpeckers (=1) vs which ones did not (=0)
trees$Woodpecker = as.factor(c(1,1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1))
## Your suspicion is that trees with woodpeckers are shorter than those without.
## To test this hypothesis, you will use the two-sample t-test and calculate it without 
## the t.test function.

## a) What are the Null and alternative Hypotheses here?
  
  # Null Hypothesis: There is no significant difference between average height of 
  #                 trees with woodpeckers and trees without woodpeckers
  # Alternate hypothesis: The average height of trees with woodpeckers is shorter than
  #                trees without woodpeckers.

## b) Why do you need a two-sample t-test here?
  
  # We need to compare two samples in this case 

## c) Do you need a one-tailed or two-tailed test?
  
  # This is a one tailed  test because we are testing if average
  # of one sample is smaller than the other

## We go through it step by step, now. 
## d) What quantities do you need to calculate the t statistic? (assuming equal variance, no Welsh-test)

  # We need number of observations and the mean of both samples

## e) calculate the means of both samples
  
trees_wp <- trees[trees$Woodpecker==1,]
trees_no_wp <- trees[trees$Woodpecker==0,]

wp_mean <- mean(trees_wp$Height)
no_wp_mean <- mean(trees_no_wp$Height)


## f) calculate the variances of the two samples

wp_var <- var(trees_wp$Height)
no_wp_var <- var(trees_no_wp$Height)

## g) calculate the weights of the two subsamples

weight_wp <- nrow(trees_wp) - 1
weight_no_wp <- nrow(trees_no_wp) - 1

## h) calculate the pooled standard deviation

pooled_sd = sqrt(((weight_wp*wp_var)+(weight_no_wp*no_wp_var))/(weight_no_wp+weight_wp))

## i) calculate the standard error of the difference between the two means

std_error <- pooled_sd * sqrt((1/nrow(trees_wp))+(1/nrow(trees_no_wp)))

## j) put it all together to calculate t

t_final <- (wp_mean - no_wp_mean)/(std_error)

## k) What degrees of freedom do you need?

dof <- nrow(trees) - 2


## l) Find the critical value for your test, using an alpha-level of 0.05 

crit_val <- qt(0.05, dof, lower.tail = FALSE)
crit_val

## m) What is your conclusion (in terms of the null hypothesis)?

  # Null hypothesis rejected and the average height of trees with and without 
  # woodpeckers has significant difference. 

## n) Now, run the same t test using the t.test function (assume equal variance!)

t_test_result <- t.test(trees$Height ~ trees$Woodpecker)
t_test_result

