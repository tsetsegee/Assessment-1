#install.packages("tidyverse")
library(tidyverse)

fastfood <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-09-04/fastfood_calories.csv")
head(fastfood)
head(fastfood, 10)
#install.packages("knitr")
library(knitr)
kable(fastfood[1:10, 1:5])

## Part 2 Filter: Display the observations that has more than 1000 calories

  filter(fastfood,calories>1000)

#Select

fastfood  %>%
  select(total_fat, total_carb) %>%
  filter(total_fat> 40 & total_carb >80) %>%
  arrange(desc(total_carb))

#Arrange observations with more than 40 in total_fat and more than 80 in total_carb in the descending order (PLEASE USE THE VARIABLE OF YOUR CHOICE TO ORGANISE THE DESCENDING ORDER) and save them to a new variable (dataset) called `dont_eat_this`

do_not_eat_this <-  fastfood  %>%
  select(restaurant,item,total_fat, total_carb) %>%
  filter(total_fat> 40 & total_carb >80) %>%
  arrange(desc(total_carb))


#Use case_when()  

fastfood<- fastfood %>%
  mutate(heavy_food = case_when(calories > 500 ~ "heavy", 
                          calories < 250 ~ "low", TRUE ~ "average"))

fastfood %>%
  count(heavy_food)


#Display the types of variables in the dataset using `skimr` package 
library(skimr)
skim(fastfood)

#Present the count observations from each restaurant in a descending order  
#Show the number of distnct items on a menu in the dataset

fastfood %>%
  count(restaurant) %>%
  arrange(desc(n))

#Using groupings (group_by()), summarise and display the average number of calories for each restaurant.
  
 fastfood %>%
  group_by(restaurant) %>%
  summarise(average_numberofcalories = mean(calories)) %>%
  ungroup() 
 
 #Add variables to the dataset, which:
 #calculates the average calories per type of restaurant and call it `average_calories` 
   
 mean_calories <- fastfood %>%
   group_by(restaurant) %>%
   summarise(average_numberofcalories = mean(calories) ) %>%
   ungroup() %>%
   arrange(average_numberofcalories)
 
 fastfood <- fastfood %>%
   group_by(restaurant) %>%
   mutate(average_numberofcalories = mean(calories) ) %>%
   ungroup()
 
 #calculates the maximum total_fat per type of restaurant and call it `max_fat` 
 
 fastfood <- fastfood %>%
   group_by(restaurant) %>%
   mutate(max_fat = max(total_fat) ) %>%
   ungroup()
 
 max_total_fat <- fastfood %>%
   group_by(restaurant) %>%
   summarise(max_fat = max(total_fat) ) %>%
   ungroup() %>%
   arrange(max_fat)
 
# calculates the minimum cholesterol per type of restaurant and call it `min_cholesterol` 

  fastfood <- fastfood %>%
   group_by(restaurant) %>%
   mutate(min_cholestrol = min(cholesterol) ) %>%
   ungroup()
 
  min_cholesterol <- fastfood %>%
    group_by(restaurant) %>%
    summarise(min_cholestrol = min(cholesterol) ) %>%
    ungroup() %>%
    arrange(min_cholestrol)
  
#15_Display the data vis of total fat per each type of restaurant. Write a narration (2-3 sentences) why you believe this type of data viz presents such information best. 

  ggplot(fastfood, aes(total_fat, restaurant)) +
   geom_boxplot()

## Box plot is used to display the data in this section. The box plot is a standardized  method of displaying the distribution of data based on the five number summary: minimum, first quartile, median, third quartile, and maximum. The reason for using box plot to data visualization is a box plots are useful for detecting outliers  and compare distributions.
 
#16_Add a variable to the dataset, which calculates the sum of cholesterol and sodium and call it `cholesterol_sodium`.
  
  fastfood <- fastfood %>%
  mutate(cholesterol_sodium = cholesterol+sodium )
  
#Remove the variable `salad`
  
  fastfood$salad <- NULL
  
  
#17Use observations for Mcdonalds to plot sugar variable against protein with `geom_point()` 
  
  fastfood %>%
    select(restaurant,sugar,protein) %>%
    filter(restaurant=="Mcdonalds") %>%
  ggplot(aes(sugar, protein)) +
    geom_point(colour= "blue") +
    labs(x = "sugar", y = "protein",  
         title = "McDonalds", caption = "Figure 1")
  
  
#18.Identify variable(s) which should be factors and transform their type into a factor variable.
#Change character variable into factor
  install.packages("forcats")
  library(forcats)
  library(dplyr)
  library(ggplot2)
  
  class(fastfood$restaurant)
  
  fastfood$restaurant <- as_factor(fastfood$restaurant)
  
  fastfood %>%
  mutate(restaurant = fct_lump(restaurant, n = 10)) %>%
  count(restaurant)
  
    
 #19. Create two new variables:
  #Read about `cut_number()` function using Help and add a new variable to the dataset `calories_type`. Use `calories` variable for `cut_number()` function to split it into 3 categories `n=3`, add labels `labels=c("low", "med", "high")` and make the dataset ordered by arranging it according to calories. 
    
    calories_type <-fastfood %>%
      group_by(restaurant) %>%
      mutate(calories_type = cut_number(calories, n=3,labels=c("low","med","high"))) %>%
      ungroup() %>%
      arrange(calories)
    
#20. Create a dataviz that shows the distribution of `calories_type` in food items for each type of restaurant. Think carefully about the choice of data viz. Use facets, coordinates and theme layers to make your data viz visually appealing and meaningful. Use factors related data viz functions.
  
    calories_type %>%
      group_by(restaurant) %>%
      ungroup() %>%
      ggplot(aes(restaurant,calories_type)) +
      geom_point(colour="black") +
      facet_wrap(~restaurant)+
      coord_flip() +
      theme_bw()+
      labs(x = "restaurant", y = "calories_type",  
           title = "Calories_type in each restaurant", caption = "Figure 2")
    
#21. The second variable should show the percentage of `trans_fat` in `total_fat`. Add the variable to the dataset and call it `trans_fat_percent`. Do not forget to save the updated dataset.
  
    transfat_percent <- fastfood %>%
      group_by(restaurant) %>%
      mutate(trans_fat_percent=trans_fat/total_fat*100) %>%
      ungroup()
   
    
#22. Create a dataviz that shows the distribution of `trans_fat` in food items for each type of restaurant. Think carefully about the choice of data viz. Use facets, coordinates and theme layers to make your data viz visually appealing and meaningful
    
    
      ggplot(fastfood,aes(restaurant,trans_fat)) +
      geom_point(colour="red") +
      facet_wrap(~restaurant)+
      coord_flip() +
      theme_bw()+
      labs(x = "restaurant", y = "trans_fat",  
           title = "Trans_fat in each restaurant", caption = "Figure 3")
    
#23.  Calculate and show the average (mean)  `total_fat` for each type of restaurant. No need to save it as a variable.
    
    fastfood %>%
      group_by(restaurant) %>%
      summarise(average_total_fat= mean(total_fat)) %>%
      ungroup() %>%
      kable()
    
#24.   24. And create a dataviz that allow to compare different restaurants on this variable (`total_fat`). You can present it on one dataviz (= no facets). Think carefully about the choice of data viz. Use coordinates and theme layers to make your data viz visually appealing and meaningful. 
    
    fastfood %>%
      group_by(restaurant) %>%
      summarise(average_total_fat= mean(total_fat)) %>%
      ungroup() %>%
      ggplot(aes( restaurant, average_total_fat  )) +
      geom_col() +
      coord_flip()
  