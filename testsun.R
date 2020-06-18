library(ggplot2)
library(dplyr)
library(magrittr)
library(scales)


pop.eg = read.csv('egypt_pop_2006.csv')
sum_total_pop = sum(pop.eg$population)
print(sun_total_pop)

firstLevel = pop.eg %>% summarize(total_pop=sum(population))

sunburst_0 = ggplot(firstLevel)
sunburst_1 = sunburst_0 + 
  geom_bar(data=firstLevel, aes(x=1, y=total_pop), fill='darkgrey', stat='identity') +
  geom_text(aes(x=1, y=sum_total_pop/2, label=paste('Egypt in 2014  had', comma(total_pop))), color='white')

sunburst_1  + coord_polar('y')

gov_pop = pop.eg %>% group_by(gov) %>% summarize(total_pop=sum(population)) %>% arrange(desc(total_pop))

compute_angle = function(perc){
  angle = -1
  #if(perc < 0.25) # 1st q [90,0]
  #angle = 90 - (perc/0.25) * 90
  #else if(perc < 0.5) # 2nd quarter [0, -90]
  #angle = (perc-0.25) / 0.25 * -90
  #else if(perc < 0.75) # 3rd q [90, 0]
  #angle = 90 - ((perc-0.5) / 0.25 * 90)
  #else if(perc < 1.00) # last q [0, -90]
  #angle = ((perc -0.75)/0.25) * -90
  
  if(perc < 0.5) # 1st half [90, -90]
    angle = (180 - (perc/0.5) * 180) - 90
  else # 2nd half [90, -90]
    angle = (90 - ((perc - 0.5)/0.5) * 180)
  
  return(angle)
}


secondLevel = gov_pop %>%
  mutate(running=cumsum(total_pop), pos=running - total_pop/2) %>% group_by(1:n()) %>% 
  mutate(angle=compute_angle((running - total_pop/2) / sum_total_pop))


sunburst_2 = sunburst_1 + geom_bar(data=secondLevel,
                                   aes(x=2, y=total_pop, fill=total_pop, stroke=3),
                                   color='white', position='stack', stat='identity')

sunburst_3 = sunburst_2 + geom_text(data=secondLevel, aes(label=paste(gov, comma(total_pop)), x=2, y=pos, angle=angle))
sunburst_3 + scale_y_continuous(labels=comma) + scale_fill_continuous(low='white', high='darkred') + coord_polar('y') + theme_minimal()
