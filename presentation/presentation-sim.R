##########################################################
# Simulation for Reasoning with Unceratinty Presentation #
##########################################################


# Simulating statistical errors -------------------------------------------

# Set simulation parameters -----------------------------------------------

set.seed(8754) # Set seed for reproducibility
n_sim <- 10000 # Set number of simulation runs

d <- .04 # True effect
p_maj <- .34 # True Hiring rate for majority group
p_min <- p_maj - d # True Hiring rate for minority group

n_maj <- 5000 # Sample size for the number of majority candidates
n_min <- 200 # Sample size for the number of minority candidates

impact_ratio <- p_min / p_maj # Ratio over .80 (4/5 Rules) is taken as evidence of no adverse impact

# Simulation code ---------------------------------------------------------

p_vector <- numeric() # Empty vector to save p-values
effect_vector <- numeric() # Empty vector to save estimated effect
impact_ratio_vector <- numeric() # Empty vector to save estimated impact ratio

for(i in 1:n_sim) {
  
  # Simulate hiring outcomes for majority and minority groups
  # 1 = Hire, 0 = Turndown (Ideal scenario)
  outcome_maj <- rbinom(n_maj, size = 1, prob = p_maj)
  outcome_min <- rbinom(n_min, size = 1, prob = p_min)
  
  p_est_maj <- mean(outcome_maj) # Estimated hiring rate of majority
  p_est_min <- mean(outcome_min) # Estimated hiring rate of minority
  effect_est <- p_est_maj - p_est_min # Estimated effect - hiring rate diff.
  impact_ratio_est <- p_est_min / p_est_maj # Estimated impact ratio
  
  # Calculate test statistic assuming null hypothesis of no difference
  p_pool <- mean(c(outcome_maj, outcome_min)) # Overall hiring rate regardless of group
  se_est <- sqrt((p_pool * (1 - p_pool)) * ((1 / n_maj) + (1 / n_min))) # Standard error given null 
  
  z <- (effect_est - 0) / se_est
  
  # Calculate p-value
  p_value <- pnorm(abs(z), lower.tail = F) * 2
  
  # Save estimates / p-values
  effect_vector <- c(effect_vector, effect_est)
  impact_ratio_vector <- c(impact_ratio_vector, impact_ratio_est)
  p_vector <- c(p_vector, p_value)
  
}

# Calculate statistical errors --------------------------------------------

mean(p_vector >= .05) # Type 2 Error Rate at alpha = .05 

mean(effect_vector < 0 & p_vector < .05) / mean(p_vector < .05) # Type S Error Rate at alpha = .05

ggplot2::ggplot(
  data = tibble(effect = effect_vector, 
                p = p_vector, 
                sig = dplyr::if_else(p < .05, TRUE, FALSE), 
                n = 1:length(effect_vector)),
  ggplot2::aes(
    x = n,
    y = effect,
    color = sig
  )
) + 
  ggplot2::geom_point()

# Type M Errors
mean(abs(effect_vector[p_vector < .05]))
(mean(abs(effect_vector[p_vector < .05]))) / d

ggplot2::ggplot(
  data = tibble(effect = effect_vector,
                p = p_vector,
                sig = dplyr::if_else(p < .05, TRUE, FALSE)
  ),
  ggplot2::aes(
    x = effect,
    fill = sig
  )
) + 
  ggplot2::geom_histogram(
    color = "black"
  )

mean(impact_ratio_vector[p_vector < .05])
(mean(impact_ratio_vector[p_vector < .05]) - impact_ratio) / impact_ratio


