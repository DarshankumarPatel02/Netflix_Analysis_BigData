library(tidyverse)
library(dplyr)
library(ggplot2)

titles <- read_csv('C:/Users/darsh/Downloads/netflix_dataset.csv', show_col_types = FALSE)
titles <- select(titles, -show_id)

head(titles)

#-----------------------------------------------------------------------------------------------------------------------------------------#

#Distribution of Movies and TV Shows

# Subset the dataset and count the occurrences of each type
type_counts <- titles %>%
  select(type) %>%
  group_by(type) %>%
  summarize(count = n())

# Calculating the percentage
type_counts$percentage <- type_counts$count / sum(type_counts$count) * 100

# Define a vector of colors
my_colors <- c("#FF69B4", "#00FFFF") # Pink and cyan colors

# Create the pie chart with customized colors
pie_chart <- ggplot(type_counts, aes(x = "", y = count, fill = type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Movies and TV Shows") +
  theme_void() +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = my_colors) # Apply the custom colors

# Display the pie chart
print(pie_chart)

#-----------------------------------------------------------------------------------------------------------------------------------------#

#Contry Contribution
plots <- titles %>%
  group_by(type) %>%
  mutate(country = forcats::fct_infreq(country)) %>% #calculate frequency of country and align it.
  split(.$type) %>% 
  lapply(function(df) { 
    ggplot(df, aes(x = country)) +
      geom_histogram(stat = 'count') + 
      theme_bw() + #line applies a black-and-white theme to the plot
      coord_cartesian(xlim = c(1, 10)) + #This line restricts the x-axis limits between 1 and 10
      scale_x_discrete(labels = function(x) { str_wrap(x, 20) }) + #restricts label to 20 character
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) #rotates the x-axis labels by 45 degrees
  })

# Plotting the separate plots
for (i in seq_along(plots)) { #seq_along generate a sequence of integers that corresponds to the indices of a given object
  print(plots[[i]])
}
#-----------------------------------------------------------------------------------------------------------------------------------------#

#Top Ratings on Netflix
# Filter the top 5 ratings
top_ratings <- titles %>%
  group_by(rating) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(5)

# Calculate the percentage for each rating
top_ratings <- top_ratings %>%
  mutate(percentage = count / sum(count) * 100)

# Create a pie chart
pie_chart <- ggplot(top_ratings, aes(x = "", y = percentage, fill = rating)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Top 5 Ratings") +
  theme_void() +
  geom_text(aes(label = paste0(rating, ": ", round(percentage, 2), "%")), position = position_stack(vjust = 0.5))

# Display the pie chart
print(pie_chart)



#-----------------------------------------------------------------------------------------------------------------------------------------#

#Most Popular Categories on Netflix

# Separate rows by comma and rename column
df_show_categories <- titles %>% 
  select(type, listed_in) %>% 
  separate_rows(listed_in, sep = ',') %>%
  rename(Show_Category = listed_in)

# Trim whitespace in Show_Category column
df_show_categories$Show_Category <- trimws(df_show_categories$Show_Category)

# Plot the data using ggplot2 and dplyr
plot <- df_show_categories %>%
  mutate(Show_Category = fct_infreq(Show_Category)) %>%
  ggplot(aes(x = Show_Category)) +
  geom_bar() +
  scale_x_discrete() +
  facet_wrap(~type, scales = 'free_x') + #used to create multiple plot based on type
  theme_bw() +
  coord_cartesian(xlim = c(1, 20)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

plot
#-----------------------------------------------------------------------------------------------------------------------------------------#

#Actor and Director with most of the contents

df_show_people <- titles %>%
  select(cast, director) %>%
  pivot_longer(cols = c(cast, director), names_to = 'role', values_to = 'person') %>%
  filter(person != "") %>%
  separate_rows(person, sep = ',') %>%
  mutate(person = str_trim(person))

df_people_freq <- df_show_people %>%
  group_by(person, role) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

top_people <- df_people_freq %>%
  group_by(role) %>%
  top_n(10, count) %>%
  ungroup()

ggplot(top_people, aes(x = fct_reorder(person, count, .desc = TRUE), y = count, fill = role)) +
  geom_bar(stat = 'identity') +
  scale_x_discrete() +
  facet_wrap(~ role, scales = 'free_x') +
  theme_bw() +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = 'Name of the actor / director')

#-----------------------------------------------------------------------------------------------------------------------------------------#

#Contents Added on Netflix Over Years


# Convert 'date_added' to Date format
titles$date_added <- as.Date(titles$date_added, format = "%B %d, %Y")

# Extract the year from the 'date_added' column
titles$year_added <- year(titles$date_added)

# Group the content by year and type
content_by_year <- titles %>%
  group_by(year_added, type) %>%
  summarise(count = n())

# Plotting using ggplot2
content_plot <- ggplot(content_by_year, aes(x = year_added, y = count, color = type, group = type)) +
  geom_line() +
  labs(x = "Year", y = "Count", title = "Content Added per Year") +
  theme_bw()

# Display the plot
print(content_plot)

#-----------------------------------------------------------------------------------------------------------------------------------------#

#Oldest TV Shows and Movies on Netflix

# Convert release_year to a vector
release_years <- as.vector(titles$release_year)

# Filter and arrange the data for TV shows
oldest_tvshows <- titles %>%
  filter(type == "TV Show") %>%
  arrange(release_year) %>%
  head(3)

# Create a matrix for oldest TV shows
oldest_tvshows_matrix <- as.matrix(oldest_tvshows[, c("title", "release_year")])

# Convert release_year to a factor
release_years_factor <- factor(titles$release_year, levels = unique(titles$release_year))

# Create a list for oldest movies
oldest_movies_list <- list()
oldest_movies_list$movies <- titles %>%
  filter(type == "Movie") %>%
  arrange(release_year) %>%
  head(3)
# Print the list of oldest movies
print(oldest_tvshows_matrix)
print(oldest_movies_list)

#-----------------------------------------------------------------------------------------------------------------------------------------#