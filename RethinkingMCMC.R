## Quick Mixed Chain Monte Carlo (MCMC) from Statistical Rethinking by Richard McElreath, page 45
#install.packages("rethinking")

library(rethinking)



#Take x samples
n_samples <- 1000

#Generate vector n samples long of NA
p <- rep(NA, n_samples)

#Assign 1st index in vector to 0.5
p[1] <- 0.5

#6 Waters in globe toss example from book
W <- 6

#3 Lands in globe toss example from book
L <- 3


##Metropolis-Hastings Algorithm


#loop n samples - 1 times
for (i in 2:n_samples) {
  
  #take single random Gaussian point with mean STARTING at 0.5 from our n_sample vector, and stdev of 0.1. Note this mean will fluctuate at the loop continues 
  p_new <- rnorm(1, p[i-1], 0.1)
  
  #if this value is less than 0, make it positive
  if (p_new < 0) p_new <- abs(p_new)
  #if this value is greater than 1, make it 2 - itself, which logically will bring it to a value below 1 again  
  if (p_new > 1) p_new <- 2 - p_new
  
  #assign q0 as binomial distribution value for the water and land "flips" we've found in the book, STARTING with 50% chance of success. This success rate will fluctuate at the loop iterates 
  q0 <- dbinom(W, W+L, p[i-1])
  
  #do the same for q1, but success rate is based on modified p_new from the lines above - you note we've locked the bounds on it from 0-1 so that it logically makes sense.
  q1 <- dbinom(W, W+L, p_new)
  
  #assign the p-th element to p_new if the quotient of q1 and q0 is less than a random number between 0-1, else "remain" as the previous element
  #Via wiki - this can be thought of as a success or reject phase
  p[i] <- ifelse(runif(1) < q1/q0, p_new, p[i-1])
}


dens(p, xlim=c(0, 1))

curve(dbeta(x, W+1, L+1), lty = 2, add = TRUE)

#The density of our newly modified p via the Metropolis algo and the beta distribution are quite close. More will be explained on this in chapter nine. Pretty cool stuff


dbinom(W, W+L, 0.5)

plot(runif(500))
plot(dbinom(1:25, 25, 0.5))
