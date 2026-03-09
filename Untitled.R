library(tidyverse)
view(starwars)

nrow(starwars)
ncol(starwars)
str(starwars)

sw_small <- starwars |> 
  select(name, species, sex, height, mass, homeworld)

sw_small |> 
  filter(species=='Human') |> 
  filter(sex=='male') |> 
  summarise(n=n())

sw_small |> 
  mutate(bmi = mass/(height/100)^2)

sw_small |> 
  group_by(species) |> 
  summarise(mean_height=mean(height))
