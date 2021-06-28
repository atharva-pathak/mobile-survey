#importing necessary libraries
library(dplyr)
library(gmodels)
library(stringr)


#reading the data
responses = read.csv("cleaned_survey.csv", na.strings = c("", "NA"))
#na.strings argument replaces blank values with NA

#Cleaning the data

#changing "nearest store" and "shopping malls" to offline
responses$outlet = str_replace(responses$outlet, 'Nearest store to me', 'Offline')
responses$outlet = str_replace(responses$outlet, 'Shopping malls', 'Offline')

#merging two columns of online and offline to one
responses = responses %>% mutate(accesories = coalesce(access_ol, access_of)) %>%
  mutate(second_hand = coalesce(second_hand_ol, second_hand_of))



#top 5 phone brands & top mobile operators in the data
top_5 = responses %>% filter(brand_of_phone %in% c('Apple','Samsung', 'Vivo','Xiaomi',
                                                   'OnePlus'),
                         mobile_operator %in% c('Jio', 'Airtel', 'Vi'))


#bivariate table 1 between brand of phone & mobile operator
CrossTable(top_5$brand_of_phone, top_5$mobile_operator, prop.chisq = F,
           dnn = c('brands', 'mobile_operator'), prop.c = F, prop.t = T, prop.r = F)

#chisq test 1
chisq.test(responses$accesories, responses$outlet) #reject h0
table(responses$accesories, responses$outlet) #creating new table to analyze the data

#chisq test 2
chisq.test(responses$second_hand, responses$outlet)

#chisq test 3
chisq.test(responses$payment_method,responses$price_range) #reject h0
table(responses$payment_method, responses$price_range) #creating new table to analyze the data

#chisq test 4
chisq.test(responses$price_range, responses$second_hand)

#chisq test 5
chisq.test(responses$brand_of_phone, responses$outlet)#reject h0
table(responses$brand_of_phone, responses$outlet) #creating new table to analyze the data