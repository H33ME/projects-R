# import packages
library(tidyverse)
data('storms', package = 'dplyr')
#convert to categorical var to factor
is_fact_func <- function(dat) {
  listdat <- list()
  for (i in seq_along(dat)) {
    if (is.character(dat[[i]])) {
      listdat[[i]] <- factor(dat[[i]])
    } else{
      listdat[[i]] <- dat[[i]]
    }
  }
  newdat <- data.frame(listdat)
  colnames(newdat) <- colnames(dat)
  return(as_tibble(newdat))
}

# clean the data
new_storms_data <- storms %>% mutate(
  # replace missing values
  tropicalstorm_force_diameter = replace_na(
    tropicalstorm_force_diameter,
    median(tropicalstorm_force_diameter,
           na.rm = TRUE)
  ),
  hurricane_force_diameter = replace_na(
    hurricane_force_diameter,
    median(hurricane_force_diameter,
           na.rm = TRUE)
  )
) %>% # convert to factor
  is_fact_func()

# plot a bar plot
stat_plot <- new_storms_data %>%
  filter(hurricane_force_diameter %in% c(100:300)) %>% 
  ggplot(aes(name, hurricane_force_diameter)) +
  geom_bar(stat = 'identity')+
  labs(
    title = 'A Barplot showing the Distribution of storm names versus Diameter 
    (in nautical miles) of the area experiencing hurricane strength winds (64 knots or above)',
    x = 'Storm Name',
    y = 'Hurricane Force in Diameter'
  )+
  scale_x_discrete(guide = guide_axis(n.dodge = 3))
stat_plot
# make it interactive
library(plotly)
ggplotly(stat_plot)

# update the data
# a function that samples the data
sample_data_function <- function(dat) {
  lis_dat <- list()
  for (var in seq_along(dat)) {
    lis_dat[[var]] <- sample(x = dat[[var]],
                             size = length(dat[[var]]),
                             replace = TRUE)
  }
  new_dat <- data.frame(lis_dat)
  colnames(new_dat) <- colnames(dat)
   return(as_tibble(new_dat))
}
updated_storms_data<- new_storms_data %>% 
  sample_data_function()

# Update the plot  
updated_plot<- updated_storms_data %>% 
  filter(hurricane_force_diameter %in% c(100:300)) %>% 
  ggplot(aes(name, hurricane_force_diameter)) +
  geom_bar(stat = 'identity')+
  labs(
    title = 'A Barplot showing the Distribution of storm names versus Diameter 
    (in nautical miles) of the area experiencing hurricane strength winds (64 knots or above)',
    x = 'Storm Name',
    y = 'Hurricane Force in Diameter',
    caption = 'This plot was made using an updated data.'
  )+
  scale_x_discrete(guide = guide_axis(n.dodge = 3))+
  coord_flip()
updated_plot
  