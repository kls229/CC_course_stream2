##%######################################################%##
#                                                          #
####           Efficient data manipulation              ####
#                                                          #
##%######################################################%##

# Learn to chain multiple lines of code with %>%, use dplyr, and automate advanced tasks like plotting without writing a loop

#  %>% makes the code flow and avoids repitition, it's used to pass output to another function from left to right.

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

#This is fine but for large datasets, the environment will get cluttered quickly as keep creating objects

# Use %>% instead... %>% = Crtl + shift + m

trees.summary <- trees %>%  # Data to be passed though pipe
  group_by(CommonName) %>%
           tally()

# FInd three species, count number of trees in each species and break them down by age group...

trees.subset <- trees %>% 
  filter(CommonName %in%  c("Common Ash", "Rowan", "Scots Pine")) %>% 
  group_by(CommonName, AgeGroup) %>% 
  tally ()

## Other functions of dplyr ----

# summarise_all() generates a summary dataframe over all columns

summ.all <- summarise_all(trees, mean)

# ifelse() returns true or false statements to conditional statements you pass to it

# case_when() is similar to felse but ets you assugn more than two outcomes. 

# Can use mutate() with case_when() to change the name of factor levels, or create a new variable based on existing.

#grepl function looks for patterns in the data and specify what to return for each genus 

unique(trees$LatinName) # Shows all the species names

trees.genus <- trees %>%
  mutate(Genus = case_when(               # creates the genus column and specifies conditions
    grepl("Acer", LatinName) ~ "Acer",
    grepl("Fraxinus", LatinName) ~ "Fraxinus",
    grepl("Sorbus", LatinName) ~ "Sorbus",
    grepl("Betula", LatinName) ~ "Betula",
    grepl("Populus", LatinName) ~ "Populus",
    grepl("Laburnum", LatinName) ~ "Laburnum",
    grepl("Aesculus", LatinName) ~ "Aesculus", 
    grepl("Fagus", LatinName) ~ "Fagus",
    grepl("Prunus", LatinName) ~ "Prunus",
    grepl("Pinus", LatinName) ~ "Pinus",
    grepl("Sambucus", LatinName) ~ "Sambucus",
    grepl("Crataegus", LatinName) ~ "Crataegus",
    grepl("Ilex", LatinName) ~ "Ilex",
    grepl("Quercus", LatinName) ~ "Quercus",
    grepl("Larix", LatinName) ~ "Larix",
    grepl("Salix", LatinName) ~ "Salix",
    grepl("Alnus", LatinName) ~ "Alnus")
  )

# Or for even faster results, use the seperate() function from tidyr package to split the column into several new columns filled with the words making up the species names

library(tidyr)

trees.genus.2 <- trees %>% 
  tidyr::separate(LatinName, c("Genus", "Species"), sep = " ", remove = FALSE) %>%  
  dplyr::select(-Species)

# We're creating two new columns in a vector (genus name and species name), "sep" refers to the separator, 
# here space between the words, and remove = FALSE means that we want to keep the original column LatinName in the data frame

# Can also reclassify factors e.g number of factor levels in tree height

trees.genus <- trees.genus %>%   # overwriting our data frame 
  mutate(Height.cat =   # creating our new column
           case_when(Height %in% c("Up to 5 meters", "5 to 10 meters") ~ "Short",
                     Height %in% c("10 to 15 meters", "15 to 20 meters") ~ "Medium",
                     Height == "20 to 25 meters" ~ "Tall")
  )

# Re-ording factor levels

str(trees.genus$Height.cat)

(trees.genus$Height.cat <- as.factor(trees.genus$Height.cat))

levels(trees.genus$Height.cat)  # shows the different factor levels in their default order

trees.genus$Height.cat <- factor(trees.genus$Height.cat,
                                 levels = c('Short', 'Medium', 'Tall'),   # whichever order you choose will be reflected in plots etc
                                 labels = c('SHORT', 'MEDIUM', 'TALL')    # Make sure you match the new names to the original levels!
)   

# These new levels will be refected in plots

## Advanced piping for plotting! ----

# Subset trees to fewer genera

trees.five <-trees.genus %>%
  filter(Genus %in% c("Acer", "Fraxinus", "Salix", "Aesculus", "Pinus"))

# Map these 5 trees:

(map.all <- ggplot(trees.five) +
    geom_point(aes(x = Easting, y = Northing, size = Height.cat, colour = Genus), alpha = 0.5) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12))
)

# To create seperate maps for each genus, could filter the data frame and copy the plotting code but this is lonnng


# Could facet, but this doesn't allow you to save things as seperate files/
# The do() function allows you to use any R function within a pipe chain, provided data = is provided

(tree.plots <-  
  trees.five  %>%      # the data frame
  group_by(Genus) %>%  # grouping by genus
  do(plots =           # the plotting call within the do function
       ggplot(data = .) +
       geom_point(aes(x = Easting, y = Northing, size = Height.cat), alpha = 0.5) +
       labs(title = paste("Map of", .$Genus, "at Craigmillar Castle", sep = " ")) +
       theme_bw() +
       theme(panel.grid = element_blank(),
             axis.text = element_text(size = 14),
             legend.text = element_text(size = 12),
             plot.title = element_text(hjust = 0.5),
             legend.position = "bottom")
  ) )

tree.plots$plots # To view the plots

# Saving the plots to file with this code:

tree.plots %>%              # the saving call within the do function
  do(., 
     ggsave(.$plots, filename = paste(getwd(), "/", "map-", .$Genus, ".png", sep = ""), device = "png", height = 12, width = 16, units = "cm"))

# Would look something like this:  ‘C:/Coding_Club/map-Acer.png’.


























