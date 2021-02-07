# HW2-part2
library(ggplot2)
library(tidyverse)

head(bikeshare)

#part 1
#  sum up average rent with hour of the day
rent_total = bikeshare %>%
  group_by(hour=hr) %>%
  summarize(average_rent = mean(total))

# Plot A: Plot the resut over time in a line graph
p1 = ggplot(rent_total) + 
  geom_line(aes(x=hour, y=average_rent))+
  scale_x_continuous(breaks = 0:24)

p1

p11 = p1+ labs(title='average bike rentals versus hour of the day',
              x="hour of the day", 
               y='average bike rentals')+
  scale_y_continuous(breaks=seq(0, 600, by=50))

p11


# Part2
#  faceted according to whether it is a working day
bikeshare = bikeshare %>%
  mutate(work_or_not = ifelse(workingday > 0,'work','not work'))

head(bikeshare)

workingday_list = c('work','not work')

combined_bike = bikeshare %>%
  filter(work_or_not %in% workingday_list) %>%
  group_by(hour = hr, work_or_not) %>%
  summarize(average_rent = mean(total))

combined_bike

# Plot all two as line graphs
p2 = ggplot(combined_bike) + 
  geom_line(aes(x=hour, y=average_rent, color=work_or_not)) +
  scale_x_continuous(breaks = 0:24)

p2

p22 = p2 + labs(title='average bike rentals versus hour of the day, faceted according to whether it is a working day',
                x="hour of the day", 
            y="average bike rentals", 
            color='woriking day or not')+
  scale_y_continuous(breaks=seq(0, 600, by=50))

p22

# changing the position of the legend
p22 + theme(legend.position = 'bottom')


# Part 3
weather_bike = bikeshare %>%
  filter(hr==8) %>%
  group_by(weathersit, work_or_not) %>%
  summarize(average_rent = mean(total))

weather_bike

ggplot(weather_bike) + 
  geom_col(mapping = aes(x=weathersit, y=average_rent)) + 
  facet_wrap(~work_or_not)+ 
  labs(title="average ridership during the 8 AM hour by weather situation", 
       y="average ridership",
       x = "weather situation (1: Clear, Few clouds, Partly cloudy, Partly cloudy
2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog)")



