p <- .70
q <- 1 - p
d <- .03
n1 <- 40
n2 <- 400
se1 <- sqrt((p*q)/n1)
a <- d + p
se2 <- sqrt((a*(1-a))/n2)
p0 <- (p*n1 + a*n2)/(n1 + n2)
se0 <- sqrt((p0 * (1 - p0)) * (1/n1 + 1/n2))
se_p <- sqrt(se1^2  + se2^2)

co_u <- qnorm(.975)*se0 + 0
co_l <- qnorm(.025)*se0 + 0

power <- pnorm(qnorm(.975)*(se0 / se_p) - (d/se_p), lower.tail = F) + pnorm(-qnorm(.975)*(se0 /se_p) - (d/se_p))
s_error <- pnorm(-qnorm(.975)*(se0 /se_p) - (d/se_p)) / power

sim <- 10000
p_vec <- numeric()
p_vec_2 <- numeric()
z_vec <- numeric()
dif_vec <- numeric()

for(i in 1:sim) {
  y1 <- rbinom(n1, size = 1, prob = p)
  y2 <- rbinom(n2, size = 1, prob = a)
  
  mod <- glm(c(y1, y2) ~ rep(c(0,1),c(n1, n2)), family = "binomial")
  
  p_all <- mean(c(y1, y2))
  se_est <- sqrt((p_all*(1-p_all)) * (1/n1 + 1/n2))
  
  z <- (mean(y2) - mean(y1)) / se_est
  
  dif_vec <- c(dif_vec, mean(y2) - mean(y1))
  z_vec <- c(z_vec, z)
  
  p_vec <- c(p_vec, pnorm(abs(z), lower.tail = F)*2)
  p_vec_2 <- c(p_vec_2, prop.test(c(sum(y1), sum(y2)), c(n1, n2))$p.value)
  
}




x <- .7
z <- (sin(.09/2 + asin(sqrt(x))))^2
z - x
2*asin(sqrt(z)) - 2*asin(sqrt(x))
