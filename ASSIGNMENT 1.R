#SOLUTION OF QUESTION A


# Required libraries
library(rvest)
library(dplyr)

# URL of the webpage
url <- "https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/"

# Read the webpage
page <- read_html(url)

# Extract the table containing the information
table <- html_table(html_nodes(page, "table")[[1]])

# Extract the required columns from the table
df <- table[, c("Company Name (M.Cap)", "CMP", "Price Change", "Market Cap (Cr)", "52 Week High",
                "52 Week Low", "ROE", "P/E", "P/BV", "EV/EBITDA", "5YSales Gr(%)", "5YProfit Gr(%)")]

# Rename the columns
colnames(df) <- c("Company Name (M.Cap)", "CMP", "Price Change", "Market Cap (Cr)", "52 Week High",
                  "52 Week Low", "ROE", "P/E", "P/BV", "EV/EBITDA", "5YSales Gr(%)", "5YProfit Gr(%)")

# Remove unnecessary characters from column names
colnames(df) <- gsub("\\s+|\\(|\\)", "", colnames(df))

# Print the extracted dataset
df
View(df)
library(rvest)
library(dplyr)

# URL of the webpage
url <- "https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/"

# Read the webpage
page <- read_html(url)

# Extract the table containing the information
table <- html_table(html_nodes(page, "table")[[1]])

# Extract the required columns from the table
df <- table[, c("Company Name (M.Cap)", "CMP", "Price Change", "Market Cap (Cr)", "52 Week High",
                "52 Week Low", "ROE", "P/E", "P/BV", "EV/EBITDA", "5YSales Gr(%)", "5YProfit Gr(%)")]

# Rename the columns
colnames(df) <- c("Company Name (M.Cap)", "CMP", "Price Change", "Market Cap (Cr)", "52 Week High",
                  "52 Week Low", "ROE", "P/E", "P/BV", "EV/EBITDA", "5YSales Gr(%)", "5YProfit Gr(%)")

# Remove unnecessary characters from column names
colnames(df) <- gsub("\\s+|\\(|\\)", "", colnames(df))

# Print the extracted dataset
df










#SOLUTION OF QUESTION b

# Required libraries
library(rvest)
library(dplyr)

# Function to extract information for a specific stock
extract_stock_info <- function(url) {
  # Read the webpage
  page <- read_html(url)
  
  # Extract the table containing the information
  table <- html_table(html_nodes(page, "table")[[1]])
  
  # Extract the required columns from the table
  df <- table[, c("Sales", "YoY Gr. Rt.", "Adj EPS", "YoY Gr. Rt.", "BVPS", "Adj Net Profit",
                  "Cash Flow from Ops.", "Debt/CF from Ops", "Return on Equity", "Op. Profit Mgn",
                  "Net Profit Mgn", "Debt to Equity", "Working Cap Days", "Cash Conv. Cycle")]
  
  # Rename the columns
  colnames(df) <- c("Sales", "YoY Gr. Rt.", "Adj EPS", "YoY Gr. Rt.", "BVPS", "Adj Net Profit",
                    "Cash Flow from Ops.", "Debt/CF from Ops", "Return on Equity", "Op. Profit Mgn",
                    "Net Profit Mgn", "Debt to Equity", "Working Cap Days", "Cash Conv. Cycle")
  
  return(df)
}

# URLs of the stock webpages
url_hindustan_unilever <- "https://www.moneyworks4me.com/stocks/nse/hindustan-unilever-limited/info"
url_titan <- "https://www.moneyworks4me.com/stocks/nse/titan-company-limited/info"
url_mahindra_mahindra <- "https://www.moneyworks4me.com/stocks/nse/mahindra-mahindra-limited/info"
url_powergrid <- "https://www.moneyworks4me.com/stocks/nse/power-grid-corporation-of-india-limited/info"
url_infosys <- "https://www.moneyworks4me.com/stocks/nse/infosys-limited/info"

# Extract information for each stock
df_hindustan_unilever <- extract_stock_info(url_hindustan_unilever)
df_titan <- extract_stock_info(url_titan)
df_mahindra_mahindra <- extract_stock_info(url_mahindra_mahindra)
df_powergrid <- extract_stock_info(url_powergrid)
df_infosys <- extract_stock_info(url_infosys)

# Create the final data frame
final_df <- rbind(df_hindustan_unilever, df_titan, df_mahindra_mahindra, df_powergrid, df_infosys)

# Print the final data frame
print(final_df)
View(final_df)







#SOLUTION OF QUESTION c


tennis <- function(p) {
  x <- 0  # Initialize the number of sets played to 0
  
  # Simulate the tennis match
  while (x < 5) {
    # Generate a random number between 0 and 1 to determine the outcome of a set
    outcome <- runif(1)
    
    # If player A wins the set, increment the number of sets played
    if (outcome <= p) {
      x <- x + 1
    } else {
      # If player A loses the set, check if a clear winner is determined
      if (x >= 3) {
        break  # No need to play additional sets if a clear winner is determined
      }
    }
  }
  
  return(x)
}

matches <- numeric(1000)  # Create an empty vector to store the number of matches

# Simulate the matches for p = 0.70
for (i in 1:1000) {
  matches[i] <- tennis(0.70)
}

ans <- mean(matches)  # Calculate the average number of matches

print(ans)  # Print the result










#SOLUTION OF QUESTION d

MontyHall <- function() {
  # Set up the game: 1 door has a car, and the other 2 have goats
  doors <- c("goat", "goat", "car")
  
  # Contestant chooses a random door
  contestant_choice <- sample(1:3, 1)
  
  # Monty opens one of the other doors with a goat
  monty_choice <- sample(setdiff(1:3, contestant_choice)[doors[setdiff(1:3, contestant_choice)] == "goat"], 1)
  
  # Contestant switches to the remaining door
  contestant_switch <- setdiff(1:3, c(contestant_choice, monty_choice))
  
  # Check if the contestant wins or loses
  if (doors[contestant_switch[1]] == "car") {
    return(1)  # Contestant wins
  } else {
    return(0)  # Contestant loses
  }
}

# Simulate the Monty Hall game show 1000 times
num_simulations <- 1000
results <- replicate(num_simulations, MontyHall())

# Calculate the probability of winning if the contestant switches
probability_switch <- mean(results)

# Print the probability of winning if the contestant switches
print(probability_switch)







#SOLUTION OF QUESTION e

library(rvest)
library(dplyr)

# URL of the website
url <- "https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/"

# Read the HTML content of the webpage
page <- read_html(url)

# Scrape the movie data
movie_data <- page %>%
  html_nodes(".countdown-index") %>%
  html_text() %>%
  as.integer() %>%
  setNames("Ranking") %>%
  tibble() %>%
  mutate(
    Movie = page %>% html_nodes(".article_movie_title") %>% html_text(),
    Tomato_Score = page %>% html_nodes(".tMeterScore") %>% html_text() %>% gsub("%", "", .) %>% as.integer(),
    Year = page %>% html_nodes(".start-year") %>% html_text() %>%
      stringr::str_extract("\\d{4}") %>%
      as.integer()
  )

# Print the scraped data
print(movie_data)
View(movie_data)

