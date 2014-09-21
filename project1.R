
#mean of exponential distribution is 1/lambd
#standard deviation is also also 1/lambda.
#Set lambda = 0.2 for all of the simulations
#you will investigate the distribution of averages of 40 exponential(0.2)s
#Note that you will need to do a thousand or so simulated averages of 40 exponentials.


#data <- replicate(40, mean(rexp(100, 0.2))) # n is the number of random exponentials you choose to generate

data <- data.frame(replicate(1000, mean(rexp(40,0.2))))
names(data) <- c("x")

ggplot(data, aes(x = x)) + geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) +
  stat_function(fun=dnorm,size = 1, args=list(mean=mean(data$x), sd=sd(data$x)))+ labs(title="Distribution of random exponetial numbers",
                                                                                       y="Density") +
  geom_vline(aes(xintercept=median(x, na.rm=T)), color="red", linetype="dashed", size=1)


#calculate the confidence interval
x_bar <- mean(data$x)
x_sd <- sd(data$x)

x_ci <- round((x_bar +( c(-1, 1) * 1.96 * sd(data$x) / sqrt(length(data$x))) ),2)