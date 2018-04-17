
## Load the packages ##

library(dplyr)
library(ggplot2) 

library(gapminder) %>% invisible() 


## call the gapminder data and view it ##

data("gapminder") %>% View() 



########------------------ Task 1 ------------------#############
## First, we select the countries with gdpPercap > 50000

##### Visualize gdpPercap vs life expect. Results displayed by continent

gapminder %>% filter(gdpPercap < 50000) %>% 
  ggplot(aes(x=gdpPercap, y=lifeExp, col=continent)) + 
  geom_point() # with point geometry



## Improve the plot: logaritmic scale

## for each continent: build a linear model and plot it 

gapminder %>% filter(gdpPercap < 50000) %>% 
  ggplot(aes(x=log(gdpPercap), y=lifeExp, col=continent)) + 
  geom_point(alpha=0.3) + # setup an opacity 
  geom_smooth(method = lm) 

## Split the plots of the 5 continents

gapminder %>% filter(gdpPercap < 50000) %>% 
  ggplot(aes(x=log(gdpPercap), y=lifeExp, col=continent)) + 
  geom_point(alpha=0.3) + 
  geom_smooth(method = lm) + 
  facet_wrap(~continent) # create a new windows for each continent


## Improve the plots: color scale used for the year

gapminder %>% filter(gdpPercap < 50000) %>% 
  ggplot(aes(x=log(gdpPercap), y=lifeExp, col=year)) + 
  geom_point(alpha=0.3) + 
  geom_smooth(method = lm) + 
  facet_wrap(~continent) 



########------------------ Task 2 ------------------#############



#### Plot the evolution of the population in 5 countries

countries <- c('Poland', 'Portugal','Spain','United States', 'Norway')

gapminder %>% filter(country %in% countries) %>% 
  ggplot(aes(x=year, y=pop, col=country)) + 
  geom_point(alpha=0.3)

### In 5 countries randomly chosen

rand.countries <- gapminder %>% pull(country) %>% unique() %>% sample(5)

gapminder %>% filter(country %in% rand.countries) %>% 
  ggplot(aes(x=year, y=pop, col=country)) + 
  geom_point(alpha=0.3)



########------------------ Task 3 ------------------#############




####### Take the 5 countries with most population in 1992 and visualize them

gapminder %>% filter(year == 1992) %>% arrange(desc(pop)) %>% slice(1:5) 

gapminder %>% filter(year == 1992) %>% arrange(desc(pop)) %>% slice(1:5) %>%
  ggplot(aes(x=country, y=pop, col=continent, fill=continent)) +
  geom_bar(stat = "identity")


## In the year 2007

gapminder %>% filter(year == 2007) %>% arrange(desc(pop)) %>% slice(1:5) 

gapminder %>% filter(year == 2007) %>% arrange(desc(pop)) %>% slice(1:5) %>%
  ggplot(aes(x=country, y=pop, col=continent, fill=continent)) +
  geom_bar(stat = "identity")


## In the year 1952

gapminder %>% filter(year == 1952) %>% arrange(desc(pop)) %>% slice(1:5) 

gapminder %>% filter(year == 1952) %>% arrange(desc(pop)) %>% slice(1:5) %>%
  ggplot(aes(x=country, y=pop, col=continent, fill=continent)) +
  geom_bar(stat = "identity")





#######------------------ Task 4 -----------#####

## Visualization of the evolution of the population and life exp in Spain

gapminder %>% filter(country=='Spain') %>% 
  ggplot(aes(x=year, y=pop, col=lifeExp))+
  geom_point(alpha=0.3)+labs(title='Spain: \n Population Growth Starts to decrease')

ep <- c('Spain','Portugal')


gapminder %>% filter(country %in% ep) %>%
  ggplot(aes(x=year, y=lifeExp, col=pop))+
  geom_point(alpha=0.3)+labs(title='Portugal & Spain: \n Evolution of Life Expectancy')
  
