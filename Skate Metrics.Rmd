# # 95% Confidence
# qbeta(0.025,47,41)
# qbeta(0.975,47,41)
#
# # 99% Confidence
# qbeta(0.005,47,41)
# qbeta(0.995,47,41)

setwd("~/Documents/")

# Handle NA strings: https://stackoverflow.com/questions/24172111/change-the-blank-cells-to-na#24172277
data <-
  read.csv(file = 'Skate Metrics.csv', na.strings = c("", " ", "NA"))
head(data)

library(tidyr)
library(data.table)
str(data)

# as.Date : https://stackoverflow.com/questions/47230146/error-message-do-not-know-how-to-convert-dataframecol-to-class-date?rq=1
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")

data <- fill(data, c("Date", "Trick"), .direction = "down")

write.csv(data,
          "Skate Metrics - Base Data.csv",
          row.names = FALSE,
          quote = FALSE) # write base data to csv file

df = data.table(data)

# Order: https://stackoverflow.com/questions/12353820/sort-rows-in-data-table-in-decreasing-order-on-string-key-order-x-v-gives-er
data2 <-
  df[, .(sum(Outcome) / length(Outcome), sum(Outcome), length(Outcome)), by =
       c("Trick")][order(-V1)]
colnames(data2) # [1] "Trick" "V1"    "V2"    "V3"

setnames(
  data2,
  colnames(data2),
  c(
    "Trick",
    "Hits to Attempts Ratio",
    "Times Landed",
    "Num of Attempts"
  )
)

# data2[,c(1:3,5)]

data2

write.csv(data2,
          "Trick Hits to Attempts.csv",
          row.names = FALSE,
          quote = FALSE) # write summary to csv file

# Number of days of tricks recorded
length(unique(data$Date))

# Number of attempts in dataset
sum(data2[, 4])

# Nollie attempts before July 20, 2020
first_period_nollie <-
  df[Trick == "Nollie" &
       Date < "20-07-20", .(a = sum(Outcome), b = length(Outcome) - sum(Outcome))]
second_period_nollie <-
  df[Trick == "Nollie" &
       Date >= "20-07-20", .(a = sum(Outcome), b = length(Outcome) - sum(Outcome))]

### Not really sure what all this does
# : https://stackoverflow.com/questions/2564258/plot-two-graphs-in-same-plot-in-r/19039094#19039094

library(ggplot2)
library(reshape2)

# original data in a 'wide' format
x <- seq(0, 1, length = 100)
y1 <- dbeta(x, first_period_nollie$a, first_period_nollie$b)
y2 <- dbeta(x, second_period_nollie$a, second_period_nollie$b)
dff <- data.frame(x, first_period = y1, second_period = y2)

# melt the data to a long format
df2 <- melt(data = dff, id.vars = "x")

# plot, using the aesthetics argument 'colour'
ggplot(data = df2, aes(x = x, y = value, colour = variable)) + geom_line()

# qbeta(0.025,first_period_nollie$a,first_period_nollie$b)
# qbeta(0.975,first_period_nollie$a,first_period_nollie$b)
