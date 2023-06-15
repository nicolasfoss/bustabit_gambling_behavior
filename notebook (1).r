
# Load the tidyverse
library(tidyverse)

# Read in the bustabit gambling data 
bustabit <- read_csv("datasets/bustabit.csv")

# Look at the first five rows of the data
head(bustabit)

# Find the highest multiplier (BustedAt value) achieved in a game
bustabit %>%
    arrange(desc(BustedAt)) %>%
    slice(1)

# These packages need to be loaded in the first @tests cell. 
library(testthat) 
library(IRkernel.testthat)

run_tests({
    test_that("packages are loaded", {
    expect_true("tidyverse" %in% .packages(), info = "Did you load the tidyverse package?")
    })
    
    test_that("bustabit is correct", {
    expect_is(bustabit, "tbl_df", 
        info = "Did you read in the bustabit data with read_csv() (not read.csv())?")
    expect_equal(nrow(bustabit), 50000, 
        info = "Did you read in the bustabit data with read_csv() (not read.csv())?")
    })
})

# Create the new feature variables 
bustabit_features <- bustabit %>% 
  mutate(CashedOut = ifelse(is.na(CashedOut), BustedAt + .01, CashedOut),
         Profit = ifelse(is.na(Profit), 0, Profit),
         Losses = ifelse(Profit == 0, Profit - Bet, 0),
         GameWon = ifelse(Profit == 0, 0, 1),
         GameLost = ifelse(Losses < 0, 1, 0))

# Look at the first five rows of the features data
head(bustabit_features)

bustabit_features_soln <- bustabit %>% 
  mutate(CashedOut = ifelse(is.na(CashedOut), BustedAt + .01, CashedOut),
         Profit = ifelse(is.na(Profit), 0, Profit),
         Losses = ifelse(Profit == 0, -1 * Bet, 0),
         GameWon = ifelse(Profit == 0, 0, 1),
         GameLost = ifelse(Profit == 0, 1, 0)) 

run_tests({
    test_that("bustabit_features is correct", {
    expect_is(bustabit_features, "tbl_df", 
        info = "Did you use tidyverse routines to create bustabit_features?")
    expect_identical(bustabit_features$Losses, bustabit_features_soln$Losses, 
        info = "Did you compute the Losses column correctly by taking the -1 * Bet (for a lost game) or 0 (for a winning game)?")
    expect_identical(bustabit_features$GameWon, bustabit_features_soln$GameWon, 
        info = "Did you compute the GameLost column by using the value 0 for a lost game, and 1 for a winning game?")
    expect_identical(bustabit_features$GameLost, bustabit_features_soln$GameLost, 
        info = "Did you compute the GameLost column by using the value 1 for a lost game, and 0 for a winning game?")
    })
})

# Group by players to create per-player summary statistics
bustabit_clus <- bustabit_features %>%
  group_by(Username) %>%
  summarize(AverageCashedOut = mean(CashedOut), 
            AverageBet = mean(Bet),
            TotalProfit = sum(Profit),
            TotalLosses = sum(Losses), 
            GamesWon = sum(GameWon),
            GamesLost = sum(GameLost),
            .groups = "keep"
           ) %>% 
ungroup()

# View the first five rows of the data
head(bustabit_clus, n = 5)

bustabit_clus_soln <- bustabit_features_soln %>%
  group_by(Username) %>%
  summarize(AverageCashedOut = mean(CashedOut), 
            AverageBet = mean(Bet),
            TotalProfit = sum(Profit),
            TotalLosses = sum(Losses), 
            GamesWon = sum(GameWon),
            GamesLost = sum(GameLost))

run_tests({
    test_that("bustabit_clus is correct", {
    expect_is(bustabit, "tbl_df", 
        info = "Did you use tidyverse routines to create bustabit_clus?")
    expect_equal(nrow(bustabit_clus), 4149, 
        info = "bustabit_clus does not have the correct number of rows. Please make sure you grouped the data by Username.")
    expect_identical(bustabit_clus$AverageBet, bustabit_clus_soln$AverageBet, 
        info = "Did you compute the AverageBet column correctly by averaging all past bets?")
    expect_identical(bustabit_clus$TotalLosses, bustabit_clus_soln$TotalLosses, 
        info = "Did you compute the TotalLosses column correctly by summing all past losses?")
    expect_identical(bustabit_clus$GamesLost, bustabit_clus_soln$GamesLost, 
        info = "Did you compute the GamesLost column by counting all past losing games?")
    })
})

# Create the mean-sd standardization function
mean_sd_standard <- function(x) {
    (x - mean(x)) / sd(x)
}

# Apply this function to each numeric variable in the clustering set
bustabit_standardized <- bustabit_clus %>%
    mutate_if(is.numeric, mean_sd_standard)
              
# Summarize our standardized data
summary(bustabit_standardized)

mean_sd_standard_soln <- function(x) {
    (x - mean(x)) / sd(x)
}

bustabit_standardized_soln <- bustabit_clus_soln %>%
    mutate_if(funs(is.numeric), mean_sd_standard_soln)

run_tests({
    test_that("mean_sd_standard is correct", {
        expect_identical(mean_sd_standard(1:10), mean_sd_standard_soln(1:10), 
            info = "Did you perform mean-sd standardization by subtracting the mean and dividing by the standard deviation?")
    })
    
    test_that("bustabit_standardized is correct", {
    expect_is(bustabit_standardized, "tbl_df", 
        info = "Did you use dplyr routines to apply this function to each numeric column?")
    expect_equal(bustabit_standardized$AverageCashedOut, mean_sd_standard_soln(bustabit_clus$AverageCashedOut), 
        info = "Is your mean_sd_standard function correctly applied to the data columns?")
    })
})

# Choose 20190101 as our random seed
set.seed(20190101)

# Cluster the players using kmeans with five clusters
cluster_solution <- kmeans(bustabit_standardized[,-1], centers = 5)

# Store the cluster assignments back into the clustering data frame object
bustabit_clus$cluster <- factor(cluster_solution$cluster)

# Look at the distribution of cluster assignments
table(bustabit_clus$cluster)

bustabit_clus_soln$cluster <- factor(cluster_solution$cluster)

run_tests({
    test_that("The cluster assignment was performed correctly", {
        expect_is(cluster_solution, "kmeans",
                  info = "Did you use the kmeans() function to create your cluster assignment?")
        expect_equal(length(unique(cluster_solution$cluster)), 5, 
            info = "Did you choose 5 clusters?")
    })
    
        
    test_that("The assignments were stored in the cluster data frame", {
    expect_is(bustabit_clus$cluster, "factor", 
        info = "Did you store the cluster assignment in the bustabit_clus object and explicitly coerce the object to be a factor?")
    })
})

# Group by the cluster assignment and calculate averages
bustabit_clus_avg <- bustabit_clus %>%
    group_by(cluster) %>%
    summarize_if(is.numeric, mean)

# View the resulting table
bustabit_clus_avg

cluster_mean_solution <- bustabit_clus_soln %>%
    group_by(cluster) %>%
    summarize_if(funs(is.numeric), mean)

run_tests({
    test_that("The bustabit_clus_avg was created correctly", {
        expect_is(bustabit_clus_avg, "tbl_df",
                  info = "Did you use tidyverse routines to compute the cluster averages?")
        expect_identical(bustabit_clus_avg, cluster_mean_solution, 
            info = "Did you correctly compute the group averages?")
    })
})

# Create the min-max scaling function
min_max_standard <- function(x) {
    (x - min(x)) / (max(x) - min(x))
}

# Apply this function to each numeric variable in the bustabit_clus_avg object
bustabit_avg_minmax <- bustabit_clus_avg %>%
    mutate_if(is.numeric, min_max_standard)

# Load the GGally package
library(GGally)
              
# Create a parallel coordinate plot of the values
ggparcoord(bustabit_avg_minmax, columns = 2:7, 
           groupColumn = "cluster", scale = "globalminmax", order = "skewness")

min_max_standard_soln <- function(x) {
    (x - min(x)) / (max(x) - min(x))
}

bustabit_avg_minmax_soln <- cluster_mean_solution %>%
    mutate_if(is.numeric, funs(min_max_standard_soln))

student_plot <- last_plot()
solution_plot <- ggparcoord(bustabit_avg_minmax, columns = 2:ncol(bustabit_avg_minmax), 
           groupColumn = "cluster", scale = "globalminmax", order = "skewness")

run_tests({
    test_that("min_max_standard is correct", {
        expect_identical(min_max_standard(1:10), min_max_standard_soln(1:10), 
            info = "Did you perform min-max standardization by subtracting the min and dividing by the range?")
    })
    
    test_that("bustabit_avg_minmax is correct", {
        expect_is(bustabit_avg_minmax, "tbl_df", 
            info = "Did you use dplyr routines to apply bustabit_avg_minmax to each numeric column?")
        expect_identical(bustabit_avg_minmax, bustabit_avg_minmax_soln,
            info = "Did you correctly apply min_max_standard to each numeric column in bustabit_avg_minmax?")
    })
    
    test_that("The plot is drawn correctly", {
        expect_s3_class(student_plot, "ggplot") 
        expect_identical(
            student_plot$data,
            solution_plot$data,
            info = 'The plot data is incorrect. Did you use `bustabit_avg_minmax`?'
        )      
        expect_identical(
            deparse(student_plot$mapping$x),
            deparse(solution_plot$mapping$x),
            info = 'The `x` aesthetic is incorrect. Did you use `ggparcoord` to produce the plot?'
        )      
        expect_identical(
            deparse(student_plot$mapping$y),
            deparse(solution_plot$mapping$y),
            info = 'The `y` aesthetic is incorrect. Did you use `ggparcoord` to produce the plot?'
        )      
        expect_identical(
            deparse(student_plot$mapping$group),
            deparse(solution_plot$mapping$group),
            info = 'The `group` aesthetic is incorrect. Did you specify the `groupColumn` argument for ggparcoord as cluster?'
        )
    })
})

# Calculate the principal components of the standardized data
my_pc <- as.data.frame(prcomp(bustabit_standardized[,-1])$x)

# Store the cluster assignments in the new data frame
my_pc$cluster <- bustabit_clus$cluster

# Use ggplot() to plot PC2 vs PC1, and color by the cluster assignment
p1 <- ggplot(my_pc, aes(PC1, PC2, color = cluster)) + geom_point()


# View the resulting plot
p1

my_pc_soln <- as.data.frame(prcomp(bustabit_standardized_soln[,-1])$x)
my_pc_soln$cluster <- bustabit_clus_soln$cluster

student_plot <- last_plot()
solution_plot <- ggplot(data = my_pc, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point()

run_tests({
    test_that("min_max_standard is correct", {
        expect_is(p1, "ggplot", 
            info = "Did you use ggplot to create the PCP and store it as p1?")
    })
    
    test_that("The plot is drawn correctly", {
        expect_s3_class(student_plot, "ggplot") 
        expect_identical(
            student_plot$data,
            solution_plot$data,
            info = 'The plot data is incorrect. Did you use `my_pc` as the data argument?'
        )      
        expect_identical(
            deparse(student_plot$mapping$x),
            deparse(solution_plot$mapping$x),
            info = 'The `x` variable is incorrect. Did you assign PC1 as the x variable?'
        )      
        expect_identical(
            deparse(student_plot$mapping$y),
            deparse(solution_plot$mapping$y),
            info = 'The `y` variable is incorrect. Did you assign PC2 as the y variable?'
        )      
        expect_identical(
            deparse(student_plot$mapping$color),
            deparse(solution_plot$mapping$color),
            info = 'The `color` variable is incorrect. Did you specify cluster as the color variable?'
        )
    })
})

# Assign cluster names to clusters 1 through 5 in order
cluster_names <- c(
    "Risky Commoners",
    "High Rollers",
    "Risk Takers",
    "Cautious Commoners",
    "Strategic Addicts"
)

# Append the cluster names to the cluster means table
bustabit_clus_avg_named <- bustabit_clus_avg %>%
    cbind(Name = cluster_names)

# View the cluster means table with your appended cluster names
bustabit_clus_avg_named

cluster_names_soln <- c(
    "Risky Commoners",
    "High Rollers",
    "Risk Takers",
    "Cautious Commoners",
    "Strategic Addicts"
)

bustabit_clus_avg_named_soln <- cluster_mean_solution %>%
    cbind(Name = cluster_names_soln)

run_tests({
    test_that("The cluster labels are correctly defined", {
        expect_identical(
            cluster_names,
            cluster_names_soln,
            info = 'Did you correctly identify clusters 1 through 5 with the appropriate name?'
        )
    })
        
    test_that("The labels were stored in the cluster means table correctly", {
        expect_identical(
            bustabit_clus_avg_named,
            bustabit_clus_avg_named_soln,
            info = 'Did you store the correct cluster names as a column in the cluster means table?'
        )
    })
})
