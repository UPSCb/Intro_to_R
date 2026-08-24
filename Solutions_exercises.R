library(tidyverse)
library(palmerpenguins)

penguins <- penguins %>%  drop_na() 
## Exercise1
# How much does the heaviest penguin weight? Which species is it and what island does it come from?
penguins %>% select(body_mass_g) %>% max()
#6300
weight <- penguins %>% select(body_mass_g) %>% max()
penguins %>% filter(body_mass_g==weight) %>% select(species, island)
#Species Gentoo, Biscoe island

## Exercise 2
# How many penguins are females?
penguins %>% filter(sex=='female') %>% rownames() %>% length()
#165

## Exercise 3
# How many penguins of the Gentoo species are males?
penguins %>% filter(sex=='male') %>% filter(species=='Gentoo') %>% dim()
# 61

## Exercise 4
# Which penguin species is most frequent on the Biscoe island?
penguins %>% filter(island=="Biscoe") %>% 
  group_by(species) %>% summarise(count=n())
# Gentoo, 124 penguins (or 119 if you removed NAs)

## Exercise 5
# Group the penguins according to island, sex and species.
# Show the number of penguins for each combination of these traits.
summarised <- penguins %>% group_by(species, sex, island) %>% summarise(count=n())

## Exercise 6
# Using the grouped summary, use pivot_wider to expand the species column in three columns.
# Export the resulting tibble to a file in any format.
summarised <- summarised |> pivot_wider(names_from= species, values_from= count)
write_csv(summarised, "exercise6.csv")

## Exercise 7
# Import the tibble you just exported.
# Use pivot_longer obtain again a single column with all the species information.
summarised  <- read_csv("exercise6.csv")
summarised <- summarised |> pivot_longer(cols= c("Adelie", "Chinstrap", "Gentoo"),names_to= "species", values_to= "count")
