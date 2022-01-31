
# Part 1:
# by default B = 10000
B = 10000
id = 9513004
n = 16 # sample size 
m = 4 # mean of population
s= 2 # standard deviation of population
set.seed(id) # reproducability of results
x = rnorm(n,mean=m,sd=s) # generating a sample


# Part 2:

# When the population standard deviation is known, the formula for a 
# confidence interval (CI) for a population mean is x?? ± z* ??/???n, where x?? is 
# the sample mean, ?? is the population standard deviation, n is the sample size,
# and z* represents the appropriate z*-value from the standard normal 
# distribution for your desired confidence level.

# http://www.stat.yale.edu/Courses/1997-98/101/confint.htm
x_mean = mean(x)
x_sd = s # s = 2
# when level=95 z_value is 1.96 
z = 1.96
delta = z * (x_sd/sqrt(n))
print(" Confidence interval for the mean with known variance :")
lower = x_mean - delta
upper = x_mean + delta
print( c(lower,upper) )

# Part 3:

# I have used this link:
# https://stats.libretexts.org/Courses/Las_Positas_College/
# Math_40%3A_Statistics_and_Probability/07%3A_Confidence_Intervals_and_Sample_Size/
# 7.03%3A_Confidence_Intervals_for_the_Mean_with_Unknown_Standard_Deviation

x_mean = mean(x)
x_sd = sd(x)
# t_value is obtained from t-student distribution
t = 2.14
delta = t * (x_sd/sqrt(n))
print(" Confidence interval for the mean with known variance :")
lower = x_mean - delta
upper = x_mean + delta
print(c(lower,upper))


# Part 4:

# by default B = 10000
B = 10000
id = 9513004
n = 16 # sample size 
m = 4 # mean of population
s= 2 # standard deviation of population
set.seed(id) # reproducability of results
x = rnorm(n,mean=m,sd=s) # generating a sample


scratch_boot <- function(size, B_val=1000, input_sample, estimator = "mean"){
  y = rep(0,B_val);
  for(b in 1:B_val){
    resample <- sample(input_sample, replace = TRUE)
    if(estimator == "mean"){
      y[b]=mean(resample)
    }
    if(estimator =="sd"){
      y[b] = sd(resample)
    }
    if(estimator =="var"){
      y[b] = var(resample)
    }
  }
  return (y)
}

scratch_jack <- function(size, B_val=1000, input_sample, estimator = "mean"){
  y = rep(0,B_val);
  for(b in 1:B_val){
    resample <- sample(input_sample[-b], replace = TRUE)
    if(estimator == "mean"){
      y[b]=mean(resample)
    }
    if(estimator =="sd"){
      y[b] = sd(resample)
    }
    if(estimator =="var"){
      y[b] = var(resample)
    }
  }
  return (y)
}

# calculate confidence interval for a bootstraped estimator array
scratch_confidence <- function(input, level=0.95, method = "percentile"){
  if (method =="percentile"){
    lower_idx = floor(length(input)* ((1-level)/2))
    upper_idx = floor(length(input)* ( 1 - ((1-level)/2)))
    print(lower_idx)
    print(upper_idx)
    print( "lowerand upper bound for Percentile Confidence with Level = 95: ")
    print( input[lower_idx+1])
    print( input[upper_idx])
  }
}


boot_output = scratch_boot(size=n,B_val=B,input_sample = x,estimator  = "mean")
boot_output = sort(boot_output,decreasing = FALSE)
#boot_output
scratch_confidence(boot_output)


jack_output = scratch_jack(size=n,B_val=B,input_sample = x,estimator = "mean")
jack_output = sort(jack_output,decreasing = FALSE)
scratch_confidence(jack_output)

# Part 5

# > scratch_confidence(boot_output)
# > [1] 0
# > [1] 9
# > [1] "lowerand upper bound for Percentile Confidence with Level = 95: "
# > [1] 3.527481
# > [1] 5.081244

# > scratch_confidence(jack_output)
# > [1] 0
# > [1] 9
# > [1] "lowerand upper bound for Percentile Confidence with Level = 95: "
# > [1] 3.471068
# > [1] 4.557175


# *** conclusion: Since the length of confidence interval in Jackknife is less than 
# Bootstrap method, Jackknife is better. ***



# Part 6:

boot_output = scratch_boot(size=n,B_val=B,input_sample = x,estimator  = "var")
boot_output = sort(boot_output,decreasing = FALSE)
scratch_confidence(boot_output)


jack_output = scratch_jack(size=n,B_val=B,input_sample = x,estimator = "var")
jack_output = sort(jack_output,decreasing = FALSE)
scratch_confidence(jack_output)

# > scratch_confidence(boot_output)
# >[1] 250
# >[1] 9750
# >[1] "lowerand upper bound for Percentile Confidence with Level = 95: "
# >[1] 3.163692
# >[1] 5.216696

# > scratch_confidence(jack_output)
# >[1] 250
# >[1] 9750
# >[1] "lowerand upper bound for Percentile Confidence with Level = 95: "
# >[1] 3.177472
# >[1] 5.225808

#**conclusion: Since the length of confidence interval of Bootstrap is less than 
# Jackknife method, Jackknife is better. ***





