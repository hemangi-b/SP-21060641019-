library(data.table)
library(ggplot2)

set.seed(0)

FUN_ruin <- function(p, N, j) {
  
  # loop
  while (TRUE) {
    
    
    # break condition
    if (j[length(j)] == N || j[length(j)] == 0) {
      return(j)
    }
    
    # iterate
    const_play <- rbinom(1, 1, p)
    
    if (const_play == 1) {
      j <- c(j, j[length(j)] + 1)
    }
    
    if (const_play == 0) {
      j <- c(j, j[length(j)] - 1)
    }
  }
}


data <- data.table(A=FUN_ruin(p = 1/3, N = 10, j = 5))
data$t <- 1:nrow(data)
data
ggplot(data, aes(x = t, y = A)) +
  geom_line() +
  ggtitle("Gambler's Ruin") +
  theme_bw() + ylim(c(0, 10)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 1)


#Winning Probability
FUN_ruinprob <- function(N, p, j) {
  
  # edge cases
  if (p == 0) {
    return(0)
  }
  
  if (p == 1) {
    return(1)
  }
  if (p == 1 / 2) {
    return(j / N)
  }
  
  q <- 1 - p
  return((1 - (q / p) ^ j) / (1 - (q / p) ^ N))
}

FUN_ruinprob(10,1/3,5)


#Expected Game Length
set.seed(10)
FUN_ruinlength <- function(N, j, p) {
  
  # p = 1 / 2 case
  if (p == 1 / 2) {
    return(j * (N - j))
  }
  
  # other case
  q <- 1 - p
  c_2 <- N / ((q - p) * (1 - (q / p) ^ N))
  c_1 <- -c_2
  return(c_1 + c_2 * (q / p) ^ j + j / (q - p))
}



results <- replicate(FUN_ruin(N = 10, p = 1/3, j = 5), n = 1000)
results <- unlist(lapply(results, function(x) length(x) - 1))
results

# should match
mean(results); FUN_ruinlength(10, 5, 1/3)
