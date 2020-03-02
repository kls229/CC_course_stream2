##%######################################################%##
#                                                          #
####    Efficient data manipulation                     ####
#                                                          #
##%######################################################%##

# Learn to chain multiple lines of code with %>%, use dplyr, and automate advanced tasks like plotting without writing a loop

#  %>% makes the code flow and avoids repitition, it's used to pass output to another function.

library(dplyr)
library(ggplot2)
getwd()

Trees <- trees
head(Trees)

# To find number of trees per species, use group_by() and summarise()

# Count the number of trees for each species

trees.grouped <- group_by(trees, CommonName) # The next function will act on groups of species seperately.

trees.summary <- summarise(trees.grouped, count = length(CommonName)) # Length counts the number of rows (trees) for each group (Species)

# OR....

dplyr_trees <- tally(trees.grouped)

#This is fine but for large datasets, the environment will get cluttered quickly.