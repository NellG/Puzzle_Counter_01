# Script to add new puzzle data into puzzles

library(zoo)

puzzle_name = 'Glacier Bay'
new_data = readRDS(paste('./Puzzle_Counter_01/data/',puzzle_name, '.rds', sep=''))
old_data = readRDS(paste('./Puzzle_Counter_01/data/puzzles.rds'))

combined = merge(new_data, old_data, all = TRUE)
combined = combined[order(combined$name, combined$time),]

ggplot(combined, aes(x=cumul/3600, y=pieces, group=name, color=name)) +
  geom_line(size = 1) +
  xlab('Cumulative time, hours') + ylab('Pieces remaining') +
  scale_x_continuous(breaks = seq(0, 100, by=1)) +
  labs(color = 'Puzzle name') + theme_minimal(base_size = 17) + 
  theme(aspect.ratio = 1)

ggplot(combined, aes(x=interval, y=..density.., group=name, color=name, fill=name)) + 
  #geom_histogram(position='identity', alpha=0.25) +
  geom_density(alpha = 0.25, size = 1) + 
  xlab('Time between pieces') + ylab('Frequency') + 
  scale_x_log10(breaks = c(2, 10, 60, 300), 
                labels = c('2 sec', '10 sec', '1 min', '5 min')) + 
  labs(color = 'Puzzle name', fill = 'Puzzle name') + theme_minimal(base_size = 17) +
  theme(aspect.ratio = 1)

rate = 60/combined$interval

ggplot(combined, aes(x=pieces, y=60/rollmean(interval,100,na.pad=TRUE), group=name, color=name)) +
  geom_line(size = 1) +
  xlab('Cumulative time, hours') + ylab('Pieces remaining') +
  #scale_x_continuous(breaks = seq(0, 100, by=1)) +
  labs(color = 'Puzzle name') + theme_minimal(base_size = 17) + 
  theme(aspect.ratio = 1)

saveRDS(combined, './Puzzle_Counter_01/data/puzzles.rds')
