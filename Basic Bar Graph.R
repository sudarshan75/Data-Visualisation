## Bar Charts
# Vertical and Horizontal.

data = read.csv("E://DV for Batch 2//odi-batting-analysis.csv")

library(ggplot2)
library(dplyr)

## Grouping by ground and selecting top 10 grounds interms of number of matches.

ground = data %>% group_by(Ground) %>% summarise(Match = n()) %>% arrange(desc(Match))

top_10 = head(ground, 10)

## Ploting a vertical bar chart

P1 = ggplot(top_10, aes(x = Ground, y = Match)) + geom_bar(stat = "Identity", width = .5)
P1

## Arranging the bars in Order and Bar width and giving a common color
P2 = ggplot(top_10, aes(x = reorder(Ground, -Match), y = Match)) + geom_bar(stat = "Identity", width = .5)
P2

## Lets start the make - up of chart

# Removing the background grid
theme_bw()
P3 = P2 + theme_bw()
P3 
# arranging the x - axis test

P4 = P3 + theme(axis.text.x = element_text(angle = 90))

P4

# Renaming x-axis and Y-axis and plot name

P5 = P4 + labs(subtitle="Top 10 cricket grounds", 
               y="Number of Matches", x="Ground Name", title="Bar Plot", 
               caption = "Source: ESPN")
P5

# Adding col and removing x axis

P6 = P5 + geom_bar(stat = "Identity", aes(fill = Ground), width = .6)

P6

# Now I don't need the X - Axis text

P7 = P6 + theme(axis.text.x=element_blank(), axis.title.x=element_blank(), axis.ticks.x = element_blank())

P7

# Removing ticks from X - axis

P8 = P7 + theme(axis.ticks.x=element_blank())

P8

## Adjusting the legends
# Hiding the legend title

P9 = P8 + theme(legend.title=element_blank())

P9

# Changing the position of legends

P10 = P9 + theme(legend.position = "bottom")

P10

# Changing the legends size (text Plus blocks)

P11 = P10 + theme(legend.key.width = unit(.2, "cm"))

P11

# creating the data labels

P12 = P11 + geom_text(aes(label = Match), position = position_dodge(width = .9), vjust = -0.25)
P12

# Removing Y - Axis

P13 = P12 + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y =element_blank())

P13

# Reducing the size of legends

P14 = P13 + theme(legend.text = element_text(colour = 'black', size = 8, hjust = 3, vjust = 3, face = 'bold'))

P14 + coord_flip()

# Using gridExtra
library(gridExtra)
PlotA = grid.arrange(P1, P2,P3,P4,nrow = 2, ncol = 2)
PlotB = grid.arrange(P5,P6,P7,P8, nrow = 2, ncol = 2)
PlotC = grid.arrange(P9,P10,P11, nrow = 2, ncol = 2)

