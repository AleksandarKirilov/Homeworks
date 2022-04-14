# Problem 1 
Bet <- 1
Money <- 100
while(Money > 0 ){
  
  cointoss <- sample(c("win","loss"),1,prob = c(0.514,0.486))
  
  if(cointoss == "win"){
  Money <- Money + Bet
  
    } else {
      Money <- Money - Bet 
      Bet <- min(Money, Bet * 2)
    }
  print(Money)
  }


# Problem 2
library(nycflights13)
library(tidyverse)
# Exercise 5.2.4.
# 1

filter(flights, arr_delay >= 120)
filter(flights, dest == "IAH" | dest == "HOU")
filter(flights, dest %in% c("IAH", "HOU"))
airlines
filter(flights, carrier %in% c("AA", "DL", "UA"))
filter(flights, month >= 7, month <= 9)
filter(flights, arr_delay > 120, dep_delay <= 0)
filter(flights, dep_delay >= 60, dep_delay - arr_delay > 30)
summary(flights$dep_time)
filter(flights, dep_time <= 600 | dep_time == 2400)
# 2

filter(flights, between(month, 7, 9))
# 3

filter(flights, is.na(dep_time)
summary(flights)      
# 4

NA ^ 0
NA | TRUE
NA & FALSE
NA | FALSE
NA & TRUE
NA * 0
Inf * 0
-Inf * 0

# Exercise 5.3.1
arrange(flights, desc(is.na(dep_time)), dep_time)
arrange(flights, desc(dep_delay))
head(arrange(flights, desc(distance / air_time)))
arrange(flights, desc(air_time))

# Exercise 5.4.1
select(flights, "dep_time", "dep_delay", "arr_time", "arr_delay")
select(flights, 4, 6, 7, 9)
select(flights, dep_time, dep_delay, arr_time, arr_delay)

select(flights, year, month, day, year, year)

vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, one_of(vars))

select(flights, contains("TIME"))

# Exercise 5.5.2 
flights_airtime <-
  mutate(flights,
         dep_time = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
         arr_time = (arr_time %/% 100 * 60 + arr_time %% 100) %% 1440,
         air_time_diff = air_time - arr_time + dep_time
        
flights_deptime <-
  mutate(flights,
         dep_time_min = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
         sched_dep_time_min = (sched_dep_time %/% 100 * 60 +
         sched_dep_time %% 100) %% 1440,
         dep_delay_diff = dep_delay - dep_time_min + sched_dep_time_min
           )        

rankme <- tibble(
  x = c(10, 5, 1, 5, 5)
)
rankme <- mutate(rankme,
                 x_row_number = row_number(x),
                 x_min_rank = min_rank(x),
                 x_dense_rank = dense_rank(x)
)
arrange(rankme, x)

1:3 + 1:10


