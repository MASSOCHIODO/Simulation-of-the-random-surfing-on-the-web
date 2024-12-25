# Transition Matrix
P <- matrix(c(
   0 ,   0 ,  1,  1/3, 
  1/2,   0 ,  0,  1/3,     
  1/2,  1/2,  0,  1/3,
   0 ,  1/2,  0,   0
), nrow = 4, byrow = TRUE)

# DOMINANT EIGENVECTOR CALCULATION
# Calculation of the dominant eigenvector using linear algebra
eigen_data <- eigen(P)
dominant_vector <- Re(eigen_data$vectors[,1])
r_Perron <- round(dominant_vector / sum(dominant_vector), digits=4)  # Normalize
r_Perron

# Calculation of the dominant eigenvector using the power method
library(fastmatrix)
power_method_result <- power.method(P)
dominant_vector <- power_method_result$vector
r_Perron <- round(dominant_vector / sum(dominant_vector), digits=4)
r_Perron

# PRELIMINARY ELEMENTS
# Create a vector to store the absolute number of visits to each page
n <- 4             # Number of pages in the system
visits <- integer(n)    # Vector to track visits for each page

# Create a table with two columns tracking the first transitions between pages
k_trac <- 20      # Number of tracked transitions
path <- data.frame(time_step = 0:k_trac, visited_page = integer(k_trac+1))  # Data frame to track initial steps

# RANDOM SURFING SIMULATION
# Random initial page
set.seed(123) # Set seed for reproducibility
current_page <- sample(1:n, 1)
current_page
k <- 1000000   # Total number of random surfer transitions

for (step in 1:k) {
  # Increment visit count for the current page
  visits[current_page] <- visits[current_page] + 1
  
  # Record the first k_trac+1 steps in the path table
  if (step <= k_trac+1) {
    path$visited_page[step] <- current_page
  }
  
  # Transition to the next page based on the transition probabilities from P
  current_page <- sample(1:n, 1, prob = P[, current_page])
}

# Display the initial transitions of the random surfer
path

# Absolute frequencies of total visits to each page
visits

# Relative visit frequency for each page (final distribution)
PageRank_prob <- round(visits / sum(visits), digits=4)
PageRank_prob

# VALIDATION WITH DOMINANT EIGENVECTOR
# Compare simulated frequencies with the dominant eigenvector
cbind(PageRank_prob, r_Perron)


