source(file.path(tutorial.dir, 'HelpfulFunctions.R'))

# Create some fake data
set.seed(1234) # Just for tutorial purposes, set the random number generator seed so everyone gets same numbers in their table
temp <- data.table(Cell.Type=rep(c('LNCaP','PC3','MCF-7'), times=2), Tx=rep(c('Control','Drug'), each=3), Replicate=rep(c(1,2,3), each=6), Viability=runif(18, 0, 100))


##### oneway.test (anova) #####
# Make a single column that describes all the group descriptions for each row (i.e., concatenate/paste together Cell.Type and Tx)
temp[, ':='(g=paste(temp$Cell.Type, temp$Tx, sep='.'))]
temp # Print
oneway.test(Viability ~ g, temp, var.equal = FALSE)


##### t.test #####
# Simple but gets tedious for comparing each condition
?t.test
t.test(x=temp[Cell.Type=='LNCaP' & Tx=='Drug']$Viability, y = temp[Cell.Type=='LNCaP' & Tx=='Control']$Viability,
	  alternative = c("two.sided", "less", "greater"),
	  mu = 0, paired = FALSE, var.equal = FALSE,
	  conf.level = 0.95)


##### pairwise t.test #####
?pairwise.t.test
pairwise.t.test(x=temp$Viability, g=temp$g, p.adjust.method='none', pool.sd=FALSE, paired=FALSE, alternative='two.sided')


##### kruskal.test (non-parametric one-way anova) #####
?kruskal.test
kruskal.test(x=temp$Viability, g=factor(temp$g)) # Had to make temp$g a factor to make it numeric

##### wilcox.test #####
# Analogous to t.test but non-parametric
# Again, simple but gets tedious for comparing each condition
?wilcox.test
wilcox.test(x=temp[Cell.Type=='LNCaP' & Tx=='Drug']$Viability, y = temp[Cell.Type=='LNCaP' & Tx=='Control']$Viability,
	  alternative = c("two.sided", "less", "greater"),
	  mu = 0, paired = FALSE, exact=NULL, correct=TRUE, conf.level = 0.95)


##### pairwise.wilcox.test #####
?pairwise.wilcox.test
pairwise.t.test(x=temp$Viability, g=temp$g, p.adjust.method='none', paired=FALSE)


##### Combining multiple experiments / biological replicates #####
# Create some more fake data
set.seed(1235) # Just for tutorial purposes, set the random number generator seed so everyone gets same numbers in their table
temp <- data.table(Tx=c('Control','Drug'), Replicate=rep(c(1,2,3,4), each=2), Mouse=rep(c(1,2,3), each=8), Experiment=rep(c(1,2,3), each=24), Viability=runif(72, 0, 100))
temp <- temp[order(Experiment, Mouse, Tx, Replicate)]
# # Save for later
# fwrite(temp, file=file.path(tutorial.dir, 'final.csv'))

# Consider each day an experimental/biological replicate
summary <- wilcox.test.combined(data=temp, replCols='Experiment', condCol='Tx', valCol='Viability', exact=NULL, two.tailed=TRUE)

# Consider each day and mouse as an experimental/biological replicate
summary <- wilcox.test.combined(data=temp, replCols=c('Mouse','Experiment'), condCol='Tx', valCol='Viability', exact=NULL, two.tailed=TRUE)

