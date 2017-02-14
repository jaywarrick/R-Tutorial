source('~/Desktop/RTutorial/HelpfulFunctions.R')
library(data.table)

##### Importing and Exporting Data #####
# Create some fake data
set.seed(1234) # Just for tutorial purposes, set the random number generator seed so everyone gets same numbers in their table
temp <- data.table(Cell.Type=rep(c('LNCaP','PC3','MCF-7'), times=2), Tx=rep(c('Control','Drug'), each=3), Replicate=rep(c(1,2,3), each=6), Viability=runif(18, 0, 100))

# View in RStudio
View(temp)

# Print the table to the console
temp

# Write and read a CSV file
write.csv(x=temp, file='~/Desktop/RTutorial/table.csv', row.names=FALSE)
temp <- data.table(read.csv(file='~/Desktop/RTutorial/table.csv', stringsAsFactors=FALSE))
# OR
fwrite(x=temp, file='~/Desktop/RTutorial/table2.csv', row.names=FALSE)		# Super fast writing of files by data.table
temp <- fread(input='~/Desktop/RTutorial/table2.csv', stringsAsFactors=FALSE)	# Super fast reading of files by data.table

# Write and read a TAB DELIMITTED file
write.table(x=temp, file='~/Desktop/RTutorial/table.txt', sep="\t") 	 		# Notice double quotes around "\t"
temp <- data.table(read.delim(file='~/Desktop/RTutorial/table.txt', stringsAsFactors=FALSE))
# OR
fwrite(x=temp, file='~/Desktop/RTutorial/table2.txt', sep="\t")		 		# Super fast writing of files by data.table
temp <- fread(input='~/Desktop/RTutorial/table2.txt', stringsAsFactors=FALSE)	# Super fast writing of files by data.table

# Write and read an XLSX file
# You might need to run this before hand on Mac 'sudo R CMD javareconf' in the terminal
install.packages("rJava") #,type='source')
install.packages("xlsx")
library(xlsx)
write.xlsx(x=temp, file=path.expand('~/Desktop/RTutorial/table.xlsx'), sheetName='RawData', row.names=FALSE) # Evidently you need to specify the full path, thus the use of 'path.expand' which fills in what is implied by the '~' symbol
temp <- data.table(read.xlsx(file='~/Desktop/RTutorial/table.xlsx', sheetName='RawData'), stringsAsFactors=FALSE)
# Note that two string columns are factors
# See (https://www.stat.berkeley.edu/classes/s133/factors.html) for how to convert to regular columns

# Read from the clipboard
# First open an excel file or tab delimmited text file and copy a table (for now, include the header)
temp <- read.clipboard(os='mac') # This function is defined in the 'HelpfulFunctions.R' file, which is why we sourced it at the beginning of this file (but just needs to be sourced anytime before its use)
# temp <- read.clipboard(os='win')

# Read from online databases (e.g., https://www.ncbi.nlm.nih.gov/geo/browse/)
# Websites will provide lines of R code tht will load RNA seq data etc, directly into R for comparing conditions.

##### Scatter plots #####

# Create a data.table for plotting
x <- seq(0, 2*pi, 2*pi/50)
temp2 <- data.table(x=x, y=sin(x), sd=runif(length(x), 0.05, 0.2))

# Make a basic plot
plot(x=temp2$x, y=temp2$y, main='My Plot', xlab='Angle [radians]', ylab='Signal [au]')

# Change it to a line plot
plot(x=temp2$x, y=temp2$y, main='My Plot', xlab='Angle [radians]', ylab='Signal [au]', type='l')

# Add points to the plot
points(x=temp2$x, y=cos(temp2$x), pch=21, bg=rgb(1,0,0,0.5), cex=1.5)

# Add lines to the plot
lines(x=temp2$x, y=tan(temp2$x), col=rgb(0,0,1,0.7), lwd=3, lty=2)

# Add error bars to points and set width ('length') of error bar tops and bottoms as well as color
error.bar(x=temp2$x, y=cos(temp2$x), upper=temp2$sd, length=0.02, col='darkgreen')


##### Bar Plots #####

# First, calculate some summary statistics to plot
calculateSE <- function(x)
{
	ret <- sd(x)/sqrt(length(x))
	return(ret)
}
summary <- temp[, list(mean=mean(Viability), se=calculateSE(Viability)), by=.(Cell.Type, Tx)]

# Now plot a standard bar chart
bar(dt=summary[Cell.Type=='LNCaP'], y.column='mean', color.column='Tx', group.column=NULL, ylim=c(0, 125), ylab='Viability')

# Specify the x-axis label using 'xlab' and 'ylab'. Change colors using 'color.colors', and add error bars by specifying 'error.upper.column' and/or 'error.lower.column'
bar(dt=summary[Cell.Type=='LNCaP'], y.column='mean', color.column='Tx', group.column=NULL, error.upper.column='se', ylim=c(0, 125), ylab='Viability', color.colors=c(rgb(0,0,0,.5),'black'))

# Change error bar width using args.error.bar=list(length=0.5)
bar(dt=summary[Cell.Type=='LNCaP'], y.column='mean', color.column='Tx', group.column=NULL, error.upper.column='se', ylim=c(0, 125), ylab='Viability', color.colors=c(rgb(0,0,0,.5),'black'), args.error.bar=list(length=0.5))

# Plot a grouped bar chart with error bars (set the y-limits using 'ylim')
bar(dt=summary, y.column='mean', color.column='Tx', group.column='Cell.Type', error.upper.column='se',
    ylim=c(0, 125), xlab='Cell Type', ylab='Viability')

# Change default colors using 'color.colors' and remove lower error bar by setting 'error.lower.column' to NULL
bar(dt=summary, y.column='mean', color.column='Tx', group.column='Cell.Type', error.upper.column='se', error.lower.column=NULL,
    ylim=c(0, 125), xlab='Cell Type', ylab='Viability', color.colors=c('gray','black'))

# Change group.names and color.names, rotate the group.names
bar(dt=summary, y.column='mean', color.column='Tx', group.column='Cell.Type', group.names=c('~LNCaP~','~MCF-7~','~PC3~'), color.names=c('~Control~','~Drug~'), error.upper.column='se', error.lower.column=NULL,
    ylim=c(0, 125), xlab='', ylab='Viability', color.colors=c('gray','black'), rotate.x.lables=T)

# Change font sizes (See ?legend for things you can put into the 'args.legend' parameter to change the legend, such as position, overall width for legend text, border, background, title, lines spacing and text size)
# cex.lab: Changes the size of the x and y labels (i.e., 'Viability', and 'Cell Type')
# cex.axis: Changes the size of the y-axis labels (i.e., 0-120)
# cex.names: Changes the size of the x-axis labels (i.e., '~LNCaP~', '~MCF-7~', and '~PC3~')
# args.legend=list(cex=0.5, bty='o', text.width=3): Changes the size of the text, the legend 'o'utline and overall width for legend text in case it isn't correct by default.
bar(dt=summary, y.column='mean', color.column='Tx', group.column='Cell.Type', group.names=c('~LNCaP~','~MCF-7~','~PC3~'), color.names=c('~Control~','~Drug~'), error.upper.column='se', error.lower.column=NULL,
    ylim=c(0, 125), xlab='Cell Type', ylab='Viability', color.colors=c('gray','black'), cex.lab=2, cex.axis=0.5, cex.names=0.5, args.legend=list(cex=0.5, bty='o', text.width=3))

# Change the margins using the 'mar' parameter
bar(dt=summary, y.column='mean', color.column='Tx', group.column='Cell.Type', group.names=c('~LNCaP~','~MCF-7~','~PC3~'), color.names=c('~Control~','~Drug~'), error.upper.column='se', error.lower.column=NULL,
    ylim=c(0, 125), xlab='Cell Type', ylab='Viability', color.colors=c('gray','black'), cex.lab=2, cex.axis=0.5, cex.names=0.5, args.legend=list(pt.cex=0.5),
    mar=c(5,5,2,2))


##### Other Plots #####
# Visit http://www.statmethods.net/graphs/index.html for basic plots
# Fastest method is to just google "R <plot-type> example" (e.g., 'R grouped bar plot example')
# Look into the ggplot2 and plotrix packages
# Or https://plot.ly/r/ for interactive plots
# Or https://shiny.rstudio.com/gallery/ for complete web user interfaces to your data :-)

##### Exporting Plots #####
# You can always use the lower-right window pane 'Export' button.

# Alternatively, you can automate exporting as well
# The advantage of this approach is that all of you plots can be exported with consistent font sizes, line widths, margins, overall size, and resolution
# If things get weird with this method call...
graphics.off() # This basically resets plotting to files

# To start a file (plot device), choose one of the following commands (see ?pdf and ?jpeg)

# Make pdf with
# With pdfs, you can specify font using the 'family' parameter
# Fonts include "AvantGarde", "Bookman", "Courier", "Helvetica" (defualt), "Helvetica-Narrow", "NewCenturySchoolbook", "Palatino" or "Times"
# I typically create the pdf. Then I can open the pdf and save as any resolution and any type of image needed.
pdf(file='~/Desktop/RTutorial/plot.pdf', width=5, height=4)
bar(dt=summary, y.column='mean', color.column='Tx', group.column='Cell.Type', error.upper.column='se', ylim=c(0, 125), xlab='Cell Type', ylab='Viability')
dev.off()

resolution <- 300 			# ppi or dpi
pixelWidth <- resolution * 5 	# dpi * inches = pixels
pixelHeight <- resolution * 4	# dpi * inches = pixels

bmp(filename = '~/Desktop/RTutorial/plot.bmp',
    width = pixelWidth, height = pixelHeight, units = "px", pointsize = 12,
    bg = "white", res = resolution)
bar(dt=summary, y.column='mean', color.column='Tx', group.column='Cell.Type', error.upper.column='se', ylim=c(0, 125), xlab='Cell Type', ylab='Viability')
dev.off()

jpeg(filename = '~/Desktop/RTutorial/plot.jpeg',
	width = pixelWidth, height = pixelHeight, units = "px", pointsize = 12,
	quality = 75,	bg = "white", res = resolution)
bar(dt=summary, y.column='mean', color.column='Tx', group.column='Cell.Type', error.upper.column='se', ylim=c(0, 125), xlab='Cell Type', ylab='Viability')
dev.off()

png(filename = '~/Desktop/RTutorial/plot.png',
    width = pixelWidth, height = pixelHeight, units = "px", pointsize = 12,
    bg = "white",  res = resolution)
bar(dt=summary, y.column='mean', color.column='Tx', group.column='Cell.Type', error.upper.column='se', ylim=c(0, 125), xlab='Cell Type', ylab='Viability')
dev.off()

tiff(filename = '~/Desktop/RTutorial/plot.tiff',
	width = pixelWidth, height = pixelHeight, units = "px", pointsize = 12,
	compression = c("none", "rle", "lzw", "jpeg", "zip", "lzw+p", "zip+p"),
	bg = "white", res = resolution)
bar(dt=summary, y.column='mean', color.column='Tx', group.column='Cell.Type', error.upper.column='se', ylim=c(0, 125), xlab='Cell Type', ylab='Viability')
dev.off()


