## Library
library(tidyverse)
library(palmerpenguins)
library(ggthemes)
library(ggplot2)

## Data Visualization
# dataset
palmerpenguins::penguins

# See all variables and the first few observations of each variable
glimpse(penguins) 

# Recreating visualization displaying the relationship between flipper lengths 
# and body masses of these penguins, taking into consideration 
# the species of the penguin.
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) + 
  geom_point(mapping = aes(colour = species, shape=species)) +
  geom_smooth(method='lm') +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()

# Rewriting the previous plot more concisely yields:
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point()

# Using pipe %>%
penguins %>%
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point()

# Categorical variables
# Bar plots
ggplot(penguins, aes(x = species)) +
  geom_bar()

ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar()

# Histogram
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)

ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 20)

ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 2000)

# Density plots
ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()

# Numerical + Categorial variables
# Box plots
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot()

# Density plots(cagetorial+numeric)
ggplot(penguins, aes(x = body_mass_g, color = species)) +
  geom_density(linewidth = 0.75)

ggplot(penguins, aes(x = body_mass_g, color = species, fill = species)) +
  geom_density(alpha = 0.5)

# Two categorial variables
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar()

ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")

# Two numerical variables
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

# Three or more variables
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = island))

# Using facet
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island)

# Saving plots
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()
# ggsave(filename = "penguin-plot.png")




