# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caret)
library(reshape2)
library(scales)

# Read the data from the Excel file
data <- read_excel("dataset.xlsx", sheet = "e_Car_Data_for_Case")
data <- data %>% select(FICO, Amount, Rate, Term, Tier, Outcome)

# 1. Logistic Regression Model
model <- glm(Outcome ~ Rate + FICO + Amount + Term + Tier,
             family = binomial(link = "logit"),
             data = data)

# Print model summary
summary(model)

# 2. Decision Tree Model with Max Depth = 3
tree_model <- rpart(Outcome ~ Rate + FICO + Amount + Term + Tier, 
                    data = data, 
                    method = "class", 
                    control = rpart.control(maxdepth = 3))

# PLOT 1: Decision Tree Visualization
# Set larger plot margins
par(mar = c(1, 1, 1, 1))
# Create plot with adjusted dimensions
png("decision_tree.png", width = 800, height = 600)
rpart.plot(tree_model, type = 2, extra = 106, fallen.leaves = TRUE,
           main = "Decision Tree for Outcome (Max Depth = 3)",
           box.palette = "Blues", shadow.col = "gray", nn = TRUE)
dev.off()

# Feature Importance from Decision Tree
tree_importance <- as.data.frame(varImp(tree_model))
tree_importance$Feature <- rownames(tree_importance)
rownames(tree_importance) <- NULL

# PLOT 2: Feature Importance from Decision Tree
png("feature_importance.png", width = 800, height = 600)
ggplot(tree_importance, aes(x = reorder(Feature, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "#1f77b4", color = "black") +
  coord_flip() +
  labs(title = "Feature Importance from Decision Tree",
       x = "Features",
       y = "Importance Score") +
  theme_minimal(base_size = 14)
dev.off()

# 1. Data Preprocessing and Setup
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
# Update the get_conversion_prob function with document coefficients
get_conversion_prob <- function(fico, amount, rate, term, tier, partner_bin = 1) {
  # Coefficients exactly as mentioned in document
  intercept <- 46.260866
  fico_coef <- -0.037471
  amount_coef <- -0.000175
  rate_coef <- -1.456910
  
  # Tier coefficients from document
  tier_coefficients <- c(
    -10.032439,  # Tier-1
    -7.986164,   # Tier-2
    -7.795586    # Tier-3
  )
  
  # Partner bin coefficients from document
  partner_bin_coefficients <- c(
    0.308759,    # Partner Bin-1
    -0.394522    # Partner Bin-2
  )
  
  # Calculate logit using document's equation:
  # Pr (Outcome=1) = 1 / (1 + exp(-(46.260866-0.037471*FICO-0.000175*Amount-1.456910*Rate
  # -10.032439*Tier-1-7.986164*Tier-2-7.795586*Tier-3+0.308759*Partner Bin-1-0.394522*Partner Bin-2)))
  
  logit <- intercept + 
    fico_coef * fico +
    amount_coef * amount +
    rate_coef * rate +
    tier_coefficients[tier] +
    partner_bin_coefficients[partner_bin]
  
  # Convert to probability using logistic function
  prob <- 1/(1 + exp(-logit))
  return(prob)
}

# Rest of the profit calculation remains same
calculate_profit <- function(amount, rate, term, cost_of_funds, prob) {
  # Revenue calculation (Amount × Length × APR%)
  total_revenue <- amount * (term/12) * (rate/100)
  
  # Cost calculation (Amount × Length × Cost of Funds%)
  total_cost <- amount * (term/12) * (cost_of_funds/100)
  
  # Profit calculation
  profit_if_converted <- total_revenue - total_cost
  
  # Expected profit using probability
  expected_profit <- profit_if_converted * prob
  
  return(list(
    expected_profit = expected_profit,
    revenue = total_revenue,
    cost = total_cost,
    margin = profit_if_converted
  ))
}

# Rate scenario analysis
analyze_rate_scenarios <- function(customer, rate_range = seq(3, 8, by = 0.1)) {
  results <- data.frame(
    Rate = rate_range,
    Probability = NA,
    Expected_Profit = NA,
    Revenue = NA,
    Cost = NA,
    Margin = NA
  )
  
  for(i in 1:length(rate_range)) {
    rate <- rate_range[i]
    
    # Calculate conversion probability
    prob <- get_conversion_prob(
      customer$FICO, 
      customer$Amount, 
      rate, 
      customer$Term, 
      customer$Tier,
      customer$Partner_Bin
    )
    
    # Calculate profit metrics
    profit_metrics <- calculate_profit(
      customer$Amount,
      rate,
      customer$Term,
      customer$Cost_of_Funds,
      prob
    )
    
    # Store results
    results$Probability[i] <- prob
    results$Expected_Profit[i] <- profit_metrics$expected_profit
    results$Revenue[i] <- profit_metrics$revenue
    results$Cost[i] <- profit_metrics$cost
    results$Margin[i] <- profit_metrics$margin
  }
  
  return(results)
}

# Test with the same customer examples
customer1 <- list(
  FICO = 705,
  Amount = 18000,
  Term = 60,
  Tier = 1,
  Partner_Bin = 1,
  Cost_of_Funds = 2.13
)

customer2 <- list(
  FICO = 705,
  Amount = 25000,
  Term = 60,
  Tier = 2,
  Partner_Bin = 1,
  Cost_of_Funds = 2.13
)

# Run analysis
results1 <- analyze_rate_scenarios(customer1)
results2 <- analyze_rate_scenarios(customer2)

# Print optimal rates
optimal_rate1 <- results1$Rate[which.max(results1$Expected_Profit)]
optimal_rate2 <- results2$Rate[which.max(results2$Expected_Profit)]

cat("\nOptimal Results:\n")
cat(sprintf("Customer 1 - Optimal Rate: %.1f%%, Maximum Profit: $%.2f\n", 
            optimal_rate1, max(results1$Expected_Profit)))
cat(sprintf("Customer 2 - Optimal Rate: %.1f%%, Maximum Profit: $%.2f\n", 
            optimal_rate2, max(results2$Expected_Profit)))

# Visualization (using the previous plot_analysis function)
p1 <- plot_analysis(results1, "Customer 1")
p2 <- plot_analysis(results2, "Customer 2")