top10Losers <- volatility %>% arrange(percent_change_24h) %>% head(10) %>% mutate(status = 'Loser')

top10Winners <- volatility %>% arrange(desc(percent_change_24h)) %>% head(10) %>% mutate(status = 'Winner')

topvolatiles <- rbind(top10Losers, top10Winners)

ggplot(data = topvolatiles, aes(x = reorder(id, -percent_change_24h), y=percent_change_24h, fill=id)) + geom_bar(stat = 'identity')

plotvolatiles <- function(series, header) {
  seriesmap <- list(input=c('day', 'week'), mapto=c('percent_change_24h', 'percent_change_7d'))
  series <- seriesmap$mapto[seriesmap$input==series]
  t7data <- dec6 %>% select(id, percent_change_24h, percent_change_7d)
  top10winners <- head(t7data[order(t7data[,series], decreasing = T),], 10)
  top10winners <- top10winners %>% mutate(status = 'winner')
  top10losers <- head(t7data[order(t7data[,series]),], 10)
  top10losers <- top10losers %>% mutate(status = 'loser')
  topvolatiles <- rbind(top10winners, top10losers)
  ggplot(data = topvolatiles, aes(x = reorder(id, -get(series)), 
                                  y=get(series), fill=id)) + 
    geom_bar(stat = 'identity') + 
    labs(title = header, y = 'change in %', x = 'coin') + 
    theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle=90))
}

capcount <- function(threshhold) {
  nrow(subset(cap, eval(parse(text=ifelse(grepl(' ', threshhold), paste('market_cap_usd', strsplit(threshhold, ' ')[[1]][1], '& market_cap_usd', strsplit(threshhold, ' ')[[1]][2]), paste('market_cap_usd', threshhold))))))
}

