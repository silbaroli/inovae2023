library(echarts4r)
library(dplyr)

mtcars %>% 
  mutate(across(c(cyl, am), as.character)) %>% 
  group_by(cyl, am) %>% 
  summarise(mpg = mean(mpg)) %>% 
  group_by(am) %>% 
  e_charts(cyl) %>% 
  e_bar(mpg, stack = "grupo")
