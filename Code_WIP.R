# Loading Packages

library(data.table)
library(ggplot2)
library(dplyr)
library(gridExtra)

 # Loading Data

columns_to_keep <- c('EVTYPE', 'FATALITIES', 'INJURIES', 'PROPDMG', 'PROPDMGEXP', 'CROPDMG', 'CROPDMGEXP')
info <- fread(input='https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2', 
              select=columns_to_keep)

unique(info$PROPDMGEXP)
unique(info$CROPDMGEXP)

exponent_list <- list('1'=1,'2'=2,'3'=3,'4'=4,'5'=5,'6'=6,'7'=7,'8'=8,'9'=9,
                      'k'=3,'K'=3, 'm'=6,'M'=6, 'b'=9, 'B'=9,'NA'=0)

exponent_symbol_to_num <- function(exponent_symbol)
  {
    exponent_list[[match(exponent_symbol, names(exponent_list), nomatch=length(exponent_list))]]
  }

info[, `:=`(PROPDMGEXP_NUM = sapply(PROPDMGEXP, exponent_symbol_to_num),
            CROPDMGEXP_NUM = sapply(CROPDMGEXP, exponent_symbol_to_num),
            PROPDMG_VAL = PROPDMG*10^PROPDMGEXP_NUM,
            CROPDMG_VAL = CROPDMG*10^CROPDMGEXP_NUM,
            Sum_Damage = PROPDMG_VAL+CROPDMG_VAL)]


head(info)

head(info)
health_info <- info[, .(Total_Fatalities = sum(FATALITIES), Total_Injuries = sum(INJURIES)), by=EVTYPE]
head(health_info)
economic_info <- info[, .(Total_Damage = sum(Sum_Damage, n.rm=TRUE)/10^9), by=EVTYPE]
head(economic_info)

  
g1 <- ggplot(data=health_info[order(Total_Fatalities, decreasing=TRUE)][1:7], 
             mapping=aes(x=reorder(EVTYPE, Total_Fatalities), y=Total_Fatalities)) +  
  geom_col(fill='coral2') + coord_flip() + theme_bw()+ xlab('Type of Event') + 
  ylab('Total Fatilities') + labs(title='Total Fatilities by Event Type for top 7 Events (1950-2011)')

g2 <- ggplot(data=health_info[order(Total_Injuries, decreasing=TRUE)][1:7], 
             mapping=aes(x=reorder(EVTYPE, Total_Injuries), y=Total_Injuries)) +  
  geom_col(fill='tan2') + coord_flip() + theme_bw()+ xlab('Type of Event') + 
  ylab('Total Injuries') + labs(title='Total Injuries by Event Type for top 7 Events (1950-2011)')

grid.arrange(g1, g2)

ggplot(data=economic_info[order(Total_Damage, decreasing=TRUE)][1:7],
             mapping=aes(x=reorder(EVTYPE, Total_Damage), y=Total_Damage))+ 
  geom_col(fill='wheat3') + coord_flip() + theme_bw()+ xlab('Type of Event') + 
  ylab('Total Economic Damage, Property & Crop (USD Billions) ') + 
  labs(title='Total Economic Damage by Event Type for top 7 Events (1950-2011)')

