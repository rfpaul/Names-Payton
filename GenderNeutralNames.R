library(tidyverse)
library(babynames)

# The database has a long tail distribution of uncommon names; let's consider
# names that have been assigned to at least 1500 births for any single year
# and sex
popNames <- babynames[babynames$n > 1500,]

# Get gender-neutral names where a name has been associated with male or female
# births
neutralNames <- popNames %>% 
  group_by(name) %>%
  filter('M' %in% sex & 'F' %in% sex)

neutralNameSubset <- sort(unique(neutralNames$name))

# How many names is this?
paste(length(neutralNameSubset),"common gender neutral-names found.")
# What are these names?
print(neutralNameSubset)

# Get the babynames DB data for these popular gender-neutral names
neutralBabynames <- babynames[babynames$name %in% neutralNameSubset,]

# Group the gender-neutral names by name and year
nameYearGroup <- neutralBabynames %>% group_by(name, year)

# Get the ratio of names with the same sex designation per year
genderRatios <- nameYearGroup %>% mutate(gender_ratio = n / sum(n))

# Get only male-assigned names
maleNames <- genderRatios[genderRatios$sex == 'M',]

# Get gender-neutral names where the ratio between male- and female-assigned
# names has ever fallen below 50%
halfMorLess <- maleNames %>% 
  ungroup() %>%
  group_by(name) %>% 
  filter(min(gender_ratio) <= 0.5)

# Gender-neutral names where the male ratio has always been above 50%
majorityM <- maleNames %>%
  filter(!name %in% halfMorLess$name)

# The names themselves
halfMorLessNames <- sort(unique(halfMorLess$name))
majorityMNames <- sort(unique(majorityM$name))

# Custom color palette for majority female-assigned names
ColorsFMaj <- colorRampPalette(c("yellow", "orange", "red"))
majority.F.colors <- ColorsFMaj(length(halfMorLessNames))
names(majority.F.colors) <- halfMorLessNames

# Custom color palette for majority male-assigned names
ColorsMMaj <- colorRampPalette(c("blue", "darkblue"))
majority.M.colors <- ColorsMMaj(length(majorityMNames))
names(majority.M.colors) <- majorityMNames

# All the custom colors
group.colors = c(majority.F.colors, majority.M.colors)

# Plot the results
ggplot() +
  geom_smooth(data = majorityM, aes(x = year, y = gender_ratio,
              group = name, color = name), se = FALSE) +
  geom_smooth(data = halfMorLess, aes(x = year, y = gender_ratio, group = name, 
              color = name), se = FALSE) +
  scale_color_manual(values = group.colors) +
  ylim(0, 1)
