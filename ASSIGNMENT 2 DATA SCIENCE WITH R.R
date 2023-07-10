#SOLUTION A

data(iris)

par(mfrow = c(1, 2))
boxplot(Sepal.Length ~ Species, data = iris, main = "Sepal Length by Species")
boxplot(Petal.Length ~ Species, data = iris, main = "Petal Length by Species")

plot(iris$Sepal.Length, iris$Petal.Length, col = iris$Species, pch = 19, xlab = "Sepal Length", ylab = "Petal Length")
legend("topright", legend = levels(iris$Species), col = 1:3, pch = 19)

#conclusion
#The scatterplot allows us to visualize the relationship between Sepal.Length and Petal.Length. By coloring the points based on species, we can observe any patterns or differences between them. The scatterplot might reveal distinct clusters or trends for each species.





#SOLUTION B
library(imager)

flip <- function(image) {
  # Flip the image vertically
  flipped_image <- image[nrow(image):1, , , drop = FALSE]
  
  return(flipped_image)
}

# Load an image using imager
image <- imager::load.image("C:/Users/Admin/Downloads/dog.jpeg")

# Flip the image using the flip() function
flipped_image <- flip(image)

# Display the original and flipped images side by side
par(mfrow = c(1, 2))
plot(image, main = "Original Image")
plot(flipped_image, main = "Flipped Image")













#SOLUTION C
install.packages("MASS")

library(MASS)

# Load the ships dataset
data(ships)

# Count the number of accidents for each ship type
accidents <- table(ships$type)

# Create a bar plot to compare the number of accidents by ship type
barplot(accidents, main = "Number of Accidents by Ship Type",
        xlab = "Ship Type", ylab = "Number of Accidents")

# Add red color to the bar corresponding to ship type B
bar_col <- rep("gray", length(accidents))
bar_col[which(names(accidents) == "B")] <- "red"
barplot(accidents, main = "Number of Accidents by Ship Type",
        xlab = "Ship Type", ylab = "Number of Accidents", col = bar_col)









#SOLUTION D
library(rvest)

# Specify the URL of the website
url <- "https://stats.stackexchange.com/questions?tab=Votes"

# Scrape the data
page <- read_html(url)

# Extract the relevant information
questions <- page %>% 
  html_nodes(".question-summary") %>% 
  html_nodes(".question-hyperlink") %>% 
  html_text() 

views <- page %>% 
  html_nodes(".question-summary") %>% 
  html_nodes(".views") %>% 
  html_text() %>% 
  gsub(" views", "", .)

answers <- page %>% 
  html_nodes(".question-summary") %>% 
  html_nodes(".status") %>% 
  html_text() %>% 
  gsub(" answers", "", .)

votes <- page %>% 
  html_nodes(".question-summary") %>% 
  html_nodes(".vote-count-post") %>% 
  html_text() %>% 
  gsub("\r\n", "", .) %>% 
  gsub("\\s+", "", .)

# Create a dataframe
df <- data.frame(Title = questions, Views = views, Answers = answers, Votes = votes)

# Display the dataframe
print(df)
View(df)








#SOLUTION E
set.seed(123)  # Set a seed for reproducibility

simulate_pulls <- function() {
  count <- 0  # Initialize count of pulls
  has_half_tablet <- FALSE  # Flag to check if a half-tablet has been pulled
  
  while (!has_half_tablet) {
    count <- count + 1  # Increment the count of pulls
    
    if (count == 1) {
      bottle <- rep(c("whole"), 100)  # Create a fresh bottle with 100 whole tablets
    }
    
    pull <- sample(bottle, 1)  # Randomly pull one tablet from the bottle
    
    if (pull == "half") {
      has_half_tablet <- TRUE  # Found a half-tablet, exit the loop
    } else {
      bottle <- c(bottle, "half")  # Cut the whole tablet in half and put the leftover half back in the bottle
    }
  }
  
  return(count)
}

num_simulations <- 1000  # Number of simulations to perform
pull_counts <- replicate(num_simulations, simulate_pulls())  # Perform the simulations

average_days <- mean(pull_counts)  # Calculate the average number of days

average_days












