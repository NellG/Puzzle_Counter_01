# Script to add new puzzle data into puzzles

puzzle_name = 'Glacier Bay'
new_data = readRDS(paste('./Puzzle_Counter_01/data/',puzzle_name, '.rds', sep=''))
old_data = readRDS(paste('./Puzzle_Counter_01/data/puzzles.rds'))

combined = merge(new_data, old_data, all = TRUE)
combined = combined[order(combined$name, combined$time),]
plot(combined$cumul, combined$pieces)

saveRDS(combined, './Puzzle_Counter_01/data/puzzles.rds')
