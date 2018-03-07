top10Losers <- volatility %>% arrange(percent_change_24h) %>% head(10) %>% mutate(status = 'Loser')

top10Winners <- volatility %>% arrange(desc(percent_change_24h)) %>% head(10) %>% mutate(status = 'Winner')

topvolatiles <- rbind(top10Losers, top10Winners)

ggplot(data = topvolatiles, aes(x = reorder(id, -percent_change_24h), y=percent_change_24h, fill=id)) + geom_bar(stat = 'identity')

plotvolatiles <- function(series) {
  t7data <- dec6 %>% select(id, percent_change_24h, percent_change_7d)
  top10winners <- head(t7data[order(t7data[,series], decreasing = T),], 10)
  top10winners <- top10winners %>% mutate(status = 'winner')
  top10losers <- head(t7data[order(t7data[,series]),], 10)
  top10losers <- top10losers %>% mutate(status = 'loser')
  topvolatiles <- rbind(top10winners, top10losers)
  ggplot(data = topvolatiles, aes(x = reorder(id, -series), 
                                  y=series, fill=id)) + 
    geom_bar(stat = 'identity') + 
    labs(title = 'Top 10 winners and losers in 24 hrs', y = 'change in %', x = 'coin')
}

# checkfunc <- 