# Libraries
library(tidyverse)
library("janitor")
library(reshape2)
library(caret) # for multiple box plot
library("PerformanceAnalytics") # Correlation matrix
library(scales)

wineRed <- read_csv('data/winequality-red-kaggle.csv')

head(wineRed)

wineRed <- clean_names(wineRed)

get_dupes(wineRed)
# We can see that we have 230 duplicated observations.
# Let's remove them! 

wineRed <- wineRed[!duplicated(wineRed), ]
# New tibble with only non-duplicate observations.

is.null(wineRed)
# checking for null values 

summary(wineRed)

qualityCat <- cut(wineRed$quality, 
                  breaks=c(0,4,5,6,7,8), 
                  labels=c("undrinkable (2-4)",
                  "pretty bad (5)",
                  "fair, but has noticable flaws (6)",
                  "quaffable (7)",
                  "very good (8)"))

ggplot(data = wineRed, aes(x = qualityCat)) +
  geom_bar(fill='light blue') +
  geom_text(
    aes(label= scales::percent(after_stat(as.double(prop))), group=1),
    stat='count', vjust = -0.3, 
  ) 


cor(wineRed[-1], wineRed$quality)
chart.Correlation(wineRed, histogram=TRUE, pch=19)
# quality has a large positive correlation with alcohol,
# a moderate positive correlation with sulphates 
# and a moderate negative correlation with volatile_acidity.
# It's fair to say that quality, density and pH are normally
# distributed, which means observations have a high frequency near the mean.

featurePlot(x = wineRed[, 1:11], 
            y = qualityCat, 
            plot = "box", 
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),  
            layout = c(4,4), 
            auto.key = list(columns = 2))
# all of the variables affect wine quality excluding chlorides 
# and residual_sugar as they have equal median for all qualities.
