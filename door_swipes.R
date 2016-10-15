## R_door_swipes: a quick analysis of door entry information created by 
## whanrott on 9th March 2016

# load the libraryies
library(openxlsx);
library(chron)
library(ggplot2)

# read the data in
swipes <- read.xlsx("2016_01_door_swipes.xlsx", sheet = 1, colNames = F, rowNames = F, detectDates = T, cols = 1:7)#, rows = 1:10)

# tidy the column names
names(swipes) <- c("date1","time1","date2","time2","category","action","card_number")

# convert the times. Dates were converted by the read.xlsx function
swipes$time1  <- times(swipes$time1)
swipes$time2  <- times(swipes$time2)

# category and action columns are categorical variables and so should be factors.
swipes$category <- as.factor(swipes$category)
swipes$action <- as.factor(swipes$action)

# tidy up the card number to a numeric value. This should also be a factor.
swipes$card_number <- as.factor(gsub("([A-z ()])", "\\2", x = swipes$card_number))

# print a table of events (entry and exit) per day 
print(table(swipes$date1))

# plot a bar graph showing door actions by day
print(ggplot(swipes, aes(date1)) + geom_bar() + facet_grid(~ action))

# show number of unique visitors
print(length(unique(swipes$card_number)))

# show how door entry and exit don't match up, meaning duration is hard to calculate
print(head(table(swipes$date1,swipes$action)))

# add a column for day of the week and replot
swipes$day <- weekdays(swipes$date1, abbreviate = T)
print(ggplot(swipes, aes(date1), colour = action) + geom_bar() + facet_wrap(~ day, ncol = 1))

# this plot would be better if it didn't rely on geom_bar to generate the frequencies
# one work around for this is to use as.data.frame(table(...)) first.
# Melt is probably the better way to do it
tmp <- as.data.frame(table(swipes$date1,swipes$action))
names(tmp) <- c("date1","action","Freq")
tmp$date1 <- as.Date(tmp$date1)
print(ggplot(tmp, aes(date1, Freq)) + geom_line() + facet_wrap(~ action, ncol =1))
