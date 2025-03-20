# Load the generated random numbers
lcg_samples <- scan("lcg_samples.txt")

# Generate R's built-in random numbers for comparison
r_samples <- runif(length(lcg_samples))

# Basic statistics
cat("Summary of LCG samples:\n")
print(summary(lcg_samples))
cat("\nSummary of R's runif samples:\n")
print(summary(r_samples))

# Chi-square test for uniformity
# Create 10 equal-width bins for uniform distribution
intervals <- seq(0, 1, by=0.1)
labels <- paste0(intervals[-length(intervals)], "-", intervals[-1])

# Categorize samples
lcg_cat <- cut(lcg_samples, breaks=intervals, labels=labels, include.lowest=TRUE)
r_cat <- cut(r_samples, breaks=intervals, labels=labels, include.lowest=TRUE)

# Count frequencies
lcg_freq <- table(lcg_cat)
r_freq <- table(r_cat)

# Expected frequencies for uniform distribution
n <- length(lcg_samples)
expected_freq <- rep(n/10, 10)

# Chi-square test
cat("\nChi-square test for LCG samples:\n")
lcg_chi_test <- chisq.test(lcg_freq, p=rep(0.1, 10))
print(lcg_chi_test)

cat("\nChi-square test for R's samples:\n")
r_chi_test <- chisq.test(r_freq, p=rep(0.1, 10))
print(r_chi_test)

# Comparison between both generators
cat("\nComparing LCG and R generators:\n")
combined_freq <- data.frame(lcg=as.vector(lcg_freq), r=as.vector(r_freq))
comparison_test <- chisq.test(combined_freq)
print(comparison_test)

# Visualization
par(mfrow=c(2,2))

# Scatter plot for LCG
plot(lcg_samples[1:(length(lcg_samples)-1)], lcg_samples[2:length(lcg_samples)], 
     main="LCG Scatter Plot (n vs n+1)", xlab="n", ylab="n+1", pch=20, cex=0.5)

# Histogram for LCG
hist(lcg_samples, breaks=20, main="LCG Histogram", xlab="Value", col="lightblue", 
     probability=TRUE)
lines(density(lcg_samples), col="red", lwd=2)

# Save plots to PDF
pdf("rng_analysis_plots.pdf", width=10, height=8)

# Basic plots
par(mfrow=c(2,2))
hist(lcg_samples, breaks=20, main="LCG Histogram", xlab="Value", col="lightblue")
plot(lcg_samples[1:(length(lcg_samples)-1)], lcg_samples[2:length(lcg_samples)], 
     main="LCG Scatter Plot (n vs n+1)", xlab="n", ylab="n+1", pch=20)

hist(r_samples, breaks=20, main="R's runif Histogram", xlab="Value", col="lightgreen")
plot(r_samples[1:(length(r_samples)-1)], r_samples[2:length(r_samples)], 
     main="R's runif Scatter Plot (n vs n+1)", xlab="n", ylab="n+1", pch=20)

# 3D Scatter Plot (requires scatterplot3d package)
par(mfrow=c(1,1))
library(scatterplot3d)
scatterplot3d(lcg_samples[1:(length(lcg_samples)-2)], 
              lcg_samples[2:(length(lcg_samples)-1)], 
              lcg_samples[3:length(lcg_samples)],
              main="LCG 3D Scatter Plot (n vs n+1 vs n+2)",
              xlab="n", ylab="n+1", zlab="n+2", pch=20, color="blue")

# Drunk Man's Walk Algorithm
drunk_walk <- function(random_values, steps=NULL) {
  if(is.null(steps)) steps <- length(random_values)
  directions <- cut(random_values[1:steps], breaks=c(0, 0.25, 0.5, 0.75, 1), 
                    labels=c("up", "right", "down", "left"), include.lowest=TRUE)
  
  x <- numeric(steps+1)
  y <- numeric(steps+1)
  
  for(i in 1:steps) {
    dir <- as.character(directions[i])
    if(dir == "up") {
      x[i+1] <- x[i]
      y[i+1] <- y[i] + 1
    } else if(dir == "right") {
      x[i+1] <- x[i] + 1
      y[i+1] <- y[i]
    } else if(dir == "down") {
      x[i+1] <- x[i]
      y[i+1] <- y[i] - 1
    } else if(dir == "left") {
      x[i+1] <- x[i] - 1
      y[i+1] <- y[i]
    }
  }
  
  return(list(x=x, y=y))
}

# Execute drunk walk with LCG samples
lcg_walk <- drunk_walk(lcg_samples, 300)

# Plot drunk walk path
plot(lcg_walk$x, lcg_walk$y, type="l", col="blue", main="Drunk Man's Walk (LCG)",
     xlab="X Position", ylab="Y Position")
points(lcg_walk$x[1], lcg_walk$y[1], col="green", pch=16, cex=2)  # Start
points(lcg_walk$x[length(lcg_walk$x)], lcg_walk$y[length(lcg_walk$y)], 
       col="red", pch=16, cex=2)  # End

# Now let's create a bad RNG with poor randomness for comparison
bad_rng <- function(n) {
  values <- numeric(n)
  values[1] <- 0.1
  for(i in 2:n) {
    # Poor RNG that creates patterns
    values[i] <- (values[i-1] + 0.1) %% 1
  }
  return(values)
}

bad_samples <- bad_rng(300)

# Plot bad RNG scatter plot
plot(bad_samples[1:(length(bad_samples)-1)], bad_samples[2:length(bad_samples)], 
     main="Bad RNG Scatter Plot", xlab="n", ylab="n+1", pch=20)

# Execute drunk walk with bad RNG
bad_walk <- drunk_walk(bad_samples, 300)

# Plot bad drunk walk path
plot(bad_walk$x, bad_walk$y, type="l", col="red", main="Drunk Man's Walk (Bad RNG)",
     xlab="X Position", ylab="Y Position")
points(bad_walk$x[1], bad_walk$y[1], col="green", pch=16, cex=2)  # Start
points(bad_walk$x[length(bad_walk$x)], bad_walk$y[length(bad_walk$y)], 
       col="blue", pch=16, cex=2)  # End

dev.off()