# Final Workflow

# Read in packages and files that you need
library(data.table)
source('~/Desktop/RTutorial/HelpfulFunctions.R')

# Read in data (e.g., from clipboard or file)
x <- fread(input='~/Desktop/Rtutorial/final.csv')

# Preprocess the results (e.g., calculate replicate means, then normalize data by each experiment)
x2 <- x[, list(rep.mean=mean(Viability)),  by=list(Experiment, Tx, Mouse)] # Calculate mean for each set of technical replicates
x2[, ':='(normalizedViability=normalize(rep.mean)), by=list(Experiment)]   # Normalize the result per experiment
x2 <- x2[order(Tx, Experiment, Mouse)]								# Sort the data for perusing

# Summarize results by Tx
summary <- x2[, list(median=median(normalizedViability), mad=mad(normalizedViability)), by=list(Tx, Experiment)]

# Make and save a plot
pdf(file='~/Desktop/RTutorial/final.pdf', width=5, height=5)
bar(dt=summary, y.column='median', ylab='Normalized Viability', color.column='Tx', group.column='Experiment', xlab='Experiment', error.upper.column='mad', error.lower.column=NULL, ylim=c(0, 1.3), mar=c(4.5,4,2,2), rotate.group.names = T)
dev.off()

# Perform statistics
stats <- wilcox.test.combined(data=x2, replCols='Experiment', condCol='Tx', valCol='normalizedViability', exact=NULL, two.tailed=TRUE)
