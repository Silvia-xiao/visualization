library(plotly)
data <- read.csv("~/Desktop/5147/assignment/visualization/gdp_happiness_housing_life.csv")
#print(data)
data_2015 <- data[which(data$year == 2015),]
print(data_2015)
fig <- plot_ly(
  labels = c(data_2015$country),
  parents = c(data_2015$region),
  values = c(data_2015$score),
  type = 'sunburst'
)

fig