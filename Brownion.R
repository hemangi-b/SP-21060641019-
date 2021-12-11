library(ggplot2)
library(gganimate)

set.seed(12345)
n = 1000
t = 1
time_sequence = seq(0,1,length.out=1000)
time_sequence
dt = time_sequence[2]-time_sequence[1]
dB = sqrt(dt)*rnorm(n-1,0,1) 
dB
B = c(0,cumsum(dB))

Brownion = data.frame(B,time_sequence)



ggplot(Brownion, aes(x = time_sequence, y = B)) +
  geom_line() +
  ggtitle("1000 Simulations of a Gaussian Random Walk") +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("t") + ylab("X_t") +
  geom_hline(yintercept = 0)+  geom_point(size=2) + transition_reveal(time_sequence)

