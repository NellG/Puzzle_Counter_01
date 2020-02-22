library(ggplot2)

make_puzz <- function(name, num){
  df <- data.frame(name = rep(name, num), pieces = seq(500, 501-num))
  df$interval = abs(200*runif(num))
  df$cumul = cumsum(df$interval)
  df$time = Sys.time()+df$cumul
  return(df)
}

zupples = rbind(make_puzz('one', 100), make_puzz('two', 150), make_puzz('three', 75))

pplot <- ggplot(zupples, aes(x=cumul/3600, y=pieces, group=name, color=name)) +
  geom_line(size = 1) +
  xlab('Cumulative time, hours') + ylab('Pieces remaining') +
  scale_x_continuous(breaks = seq(0, 100, by=1)) +
  labs(color = 'Puzzle name')
pplot
