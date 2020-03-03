##%######################################################%##
#                                                          #
####          Data visualisation 2                      ####
#                                                          #
##%######################################################%##


getwd()
library(dplyr)
library(ggplot2)

str(magic_veg)

## Customising histograms ----

#Calculate how many species are in each plot:

species_counts <- magic_veg %>%
  group_by(land, plot) %>%
  summarise(Species_number = length(unique(species)))

(hist <- ggplot(species_counts, aes(x=plot)) + geom_histogram()) # but this looks wierd, need to tell R we already
# know how many species are in each plot

(hist <- ggplot(species_counts, aes(x=plot, y = Species_number)) + geom_histogram(stat = "identity"))

# But species within same land are being grouped together, need to add a colour code to make a stacked bar plot

(hist <- ggplot(species_counts, aes(x=plot, y = Species_number, fill = land)) + geom_histogram(stat = "identity"))

# Or make them appear side by side:


(hist <- ggplot(species_counts, aes(x=plot, y = Species_number, fill = land)) + geom_histogram(stat = "identity", position = "dodge") + scale_x_continuous(breaks = c(1,2,3,4,5,6)) + scale_y_continuous(limits = c(0, 50)))


## Adding titles ----


(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
   geom_histogram(stat = "identity", position = "dodge") +
   scale_x_continuous(breaks = c(1,2,3,4,5,6)) +              # Adds correct number of plots 
   scale_y_continuous(limits = c(0, 50)) +
   labs(title = "Species richness by plot", 
        subtitle = "In the magical lands",                      # Add subtitle as well as title
        caption = "Data from the Ministry of Magic",                  
        x = "\n Plot number", y = "Number of species \n"))     # \n adds space before x and after y axis text

#Control everything with theme() :

# theme(axis.text = element_text(size = 12), 
#axis.title = element_text(size = 12, face = "italic"), 
#plot.title = element_text(size = 14, hjust = 0.5, face = "bold")))


# Adding theme_bw() to our plot removes the grey background and replaces it with a white one.

#### Fix the lgend and customise colours ----

# Scale customises the colour code adn legend at once.

#scale_fill_manual lets you decide the colour of each bar etc.

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
   geom_histogram(stat = "identity", position = "dodge") + 
   scale_x_continuous(breaks = c(1,2,3,4,5,6)) + 
   scale_y_continuous(limits = c(0, 50)) +
   scale_fill_manual(values = c("rosybrown1", "#deebf7"),     # specifying the colours
                     name = "Land of Magic") +                # specifying title of legend
   labs(title = "Species richness by plot", 
        x = "\n Plot number", y = "Number of species \n") + 
   theme_bw() +
   theme(panel.grid = element_blank(), 
         axis.text = element_text(size = 12), 
         axis.title = element_text(size = 12), 
         plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
         plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
         legend.title = element_text(face = "bold"),
         legend.position = "bottom", 
         legend.box.background = element_rect(color = "grey", size = 0.3)))

## Can change name of labels with: labels = c("xx", "xx")

# use levels (dataaframe$factorname) to check the order of the levels, first.


## More here about creating own colour palette - come back to it if needed. 

# Can use scale_colour_gradient and scale_fill_gradient() if variable is continuous.
# Just need to set low = and high = colour values 

# Customise boxplots ----

# Reshape the dataset to take account of year as well.


yearly_counts <- magic_veg %>%
  group_by(land, plot, year) %>%                             # We've added in year here
  summarise(Species_number = length(unique(species))) %>%
  ungroup() %>%
  mutate(plot = as.factor(plot))

(boxplot <- ggplot(yearly_counts, aes(plot, Species_number, fill = land)) + geom_boxplot())

# Make it sexy...

(boxplot <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) +
    scale_fill_manual(values = c("rosybrown1", "#deebf7"),
                      breaks = c("Hogsmeade","Narnia"),
                      name="Land of magic",
                      labels=c("Hogsmeade", "Narnia")) +
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    theme_bw() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

# Dotplots work better for ecological data, as values rarely start at zero like bar plots suggest:


# Create the summarised data
summary <- species_counts %>%  group_by(land) %>% summarise(mean = mean(Species_number),  #This is good code for finding mean and stddev...
                                                            sd = sd(Species_number))

# Make a dot plot
(dot <- ggplot(summary, aes(x = land, y = mean, colour = land)) +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
    geom_point(size = 3) + 
    scale_y_continuous(limits = c(0, 50)) +
    scale_colour_manual(values = c('#CD5C5C', '#6CA6CD'), 
                        labels = c('HOGSMEADE', 'NARNIA'), 
                        name = 'Land of Magic') +                   
    labs(title = 'Average species richness', 
         x = '', y = 'Number of species \n') + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = 'bold'), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , 'cm'), 
          legend.title = element_text(face = 'bold'),
          legend.position = 'bottom', 
          legend.box.background = element_rect(color = 'grey', size = 0.3)))


### Reordering factors:

# Reordering the data
yearly_counts$land <- factor(yearly_counts$land, 
                             levels = c("Narnia", "Hogsmeade"),
                             labels = c("Narnia", "Hogsmeade"))

# Plotting the boxplot 

(boxplot <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) +
    scale_fill_manual(values = c("#deebf7", "rosybrown1"),
                      breaks = c("Narnia","Hogsmeade"),  # Two factors are ordered in new order too.
                      name = "Land of magic",
                      labels = c("Narnia", "Hogsmeade")) +
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    theme_bw() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3)))


## Plot regression lines ----

# Look at how plant height has changed over time.

# Extract the heights

heights <- magic_veg %>%
  filter(!is.na(height)) %>%                    # removing NA values
  group_by(year, land, plot, id) %>%
  summarise(Max_Height = max(height)) %>%       # Calculating max height
  ungroup() %>%                                 # Need to ungroup so that the pipe doesn't get confused
  group_by(year, land, plot) %>%
  summarise(Height = mean(Max_Height))          # Calculating mean max height


# Create a scatterplot

(basic_mm_scatter_line <- ggplot(heights, aes(year, Height, colour = land)) +
    geom_point() +
    theme_bw() +
    stat_smooth(method = "lm"))

# But relationship isn't linear so best to try a different fit, like quadratic:


(improved_mm_scat <- ggplot(heights, aes(year, Height, colour = land)) +
    geom_point() +
    theme_bw() +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2)))


### Creating your own theme ----

theme_coding <- function(){            # creating a new theme function
  theme_bw()+                          # using a predefined theme as a base
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),       # customising lots of things
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14),
          panel.grid = element_blank(),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),
          legend.title = element_blank(),
          legend.position = c(0.9, 0.9))
}

# If certain bits of the theme do not match a graph, you can just overwrite that bit of code e.g theme_coding() + theme(legend.position = "right")      











































