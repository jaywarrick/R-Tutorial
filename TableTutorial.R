# This hashtag symbol allow you to add a comment to the code

# I'll show you how to read a table in, filter it, do stats on it, and plot data
# First though, some quick pointers
# Use RStudio, it is way awesome and is assumed for the rest of the tutorial
# To get it, first download and install R, then download and install RStudio

# When this file is open in RStudio in the text editor, you can put your cursor
# on a line of code and hit Ctrl+Enter to run that line of code (Ctrl == Command on Macs)
# If you have any text selected, Ctrl+Enter will run just the selected text
# A quick way to run all the code is Ctrl+A (to select all) then Ctrl+Enter to run the selection

# For now, just use Ctrl+Enter to run line by line to follow along

# This is how you store a number or a word...
myNumber <- 23
myWord <- 'Hello World'

# This is how you print them
myNumber
myWord
# or
print(myNumber)
print(myWord)

# This is how you create a vector
numbers <- c(1,2,3,4,5)
numbers2 <- 1:5
words <- c('word1','word2','word3','word4','word5')

# This is how you do a for loop
for(word in words)
{
     # This is how you make a bigger word out of multiple words
     biggerWord <- paste0(word, ' more words') # Also look at 'paste' (using ?paste) to see how to separate words with characters easily
     print(biggerWord)
}
# Another quick for loop
for(i in 1:10)
{
     print(i)
}

# Let's store the path to your table to make the tutorial easier to run on your computer
# It's good to put things like this at the top of a file so you can "change" this
# variable throughout the file later by changing once at the top.
# Replace the path here with whereever you save the example data
path <- '/Users/jaywarrick/Public/DropBox/GitHub/R-Tutorial/Data3.txt'

# This is how you read a table
myTable <- read.table(file=path)

# This is how you append one table to another ('rbind' stands for row bind, there is also a 'cbind' for columns)
# I'm just using your same data all over again, but imagine it is a new table
newData <- read.table(file=path)
myTable <- rbind(myTable, newData)

# You can either click the variable name in the upper right of the RStudio window to view
# it as a table or you can type the following command (only works in RStudio)
View(myTable)

# If you want help on ANY function, you can use the help explorer in the lower right of Rstudio
# Or, you can type the name of the function you want help with with a '?' in front
# Scroll to the bottom of the help for examples of how to use the functions
# Find out how to read in .csv, and tab delimitted or .xls files specifically with different options
?read.table
# You'll learn later in this tutorial how to install packages to read almost any file type you want

# This is how you access columns and use them to calculate new values (putting them into a new column)
myTable$IntegratedIntensity <- myTable$Area * myTable$Mean

# Alternatively, you could have done it this way as well using [row,column] notation
# (row and column numbers start at 1)
# You can also use "names" of columns using this approach as well
# a benefit of this approach is that you can pass a variable for the name
myNewColumnName <- 'Integrated Intensity'
myTable[,myNewColumnName] <- myTable[,2] * myTable[,'Mean']

# This is how you can get a subset of the table
# It is essentially specifying rows and columns you want by
# using logical operations
# method 1
subTable <- myTable[myTable$Mean > 50 & myTable$Area < 50, ]
print(subTable)
# The column specifier is left blank because we implicitly want all the columns (add a vector of names to grab certain columns)
subTable <- myTable[myTable$Mean > 50 & myTable$Area < 50, c('Label','IntegratedIntensity')]
# Method 2
subTable <- subset(myTable, Mean > 50 & Area < 50)
print(subTable)
subTable <- subset(myTable, Mean > 50 & Area < 50)[,c('Label','IntegratedIntensity')]
print(subTable)

# If you are reading in a bunch of tables, it is nice to make a master table, but you
# want to add a column (or more) to help you keep track of where the data came from.
# So, instead I probably would have imported the data like this (now that you know
# how to do the things above).
#
# Read in data from condition 1 and add a column for this information
myTable <- read.table(file=path)
myTable$ProteinConcentration <- 100 # Assigns this number to every row in the new column
myTable$Condition <- 'Control'

newData <- read.table(file=path)
newData$ProteinConcentration <- 100
newData$Condition <- 'Experiment'
myTable <- rbind(myTable, newData)

newData <- read.table(file=path)
newData$ProteinConcentration <- 1000
newData$Condition <- 'Control'
myTable <- rbind(myTable, newData)

newData <- read.table(file=path)
newData$ProteinConcentration <- 1000
newData$Condition <- 'Experiment'
myTable <- rbind(myTable, newData)

# You could probably do what is above in a for loop over the files in a folder,
# parsing the names of the files to get the "concentration" and "condition" automatically,
# but this should work for now
#
# Now do your calcs on the master table
myTable$IntegratedIntensity <- myTable$Area * myTable$Mean

# Now you want to do summary stats
# We have been using was is called a 'data.frame' (try ?data.frame).
# They are super useful and easy. However, a more powerful but fairly
# confusing alternative table is called a 'data.table'. But it allows
# you to do stats on subgroups of data super easy with no for loops and crap, I mean stuff :-)
# (try ?data.table after you install the package as shown below...)
#
# Install the data.table package (This only needs to be done the very first time you use a package)
install.packages('data.table')
# Load the package so you can start using data.table's (This needs to be done each time you restart RStudio and want to use the package)
library(data.table)
myFancyTable <- data.table(myTable)
# Put everything you want to calculate for the summary into a list in the middle argument
# Put how you want to group the data in the table using the 'by' argument
summary <- myFancyTable[,list(Mean=mean(IntegratedIntensity), StDev=sd(IntegratedIntensity)), by=.(ProteinConcentration, Condition)]
print(summary)

# Here's the really fast way to do a t.test between to subgroups (use ?t.test to add options for 1-tail or 2-tail etc)
summary2 <- myFancyTable[,list(pValue=t.test(IntegratedIntensity[Condition=='Control'], IntegratedIntensity[Condition=='Experiment'])$p.value), by=.(ProteinConcentration)]
print(summary2)

# Here is how to plot data
tempTable <- data.frame(x=1:10, y=(1:10)^2)
plot(x=tempTable$x, y=tempTable$y, main='My Plot Title', xlab='X Axis Label [units]', ylab='Y Axis Label [units]')
# Use ?plot to see all the things you can do with line weights, colors, point characters, margins etc
# A super helpful, pretty, and simple website on how to do all sorts of plots (http://www.statmethods.net/graphs/index.html)
# Use ?legend to see how to add a legend. It's a pain for small plots but powerful for when you have lots of things
# or want to automate the process.

# You can use the window in the lower right of RStudio to save your plot (pdf, jpeg, png etc)
# Also, here's how you save it to a pdf programmatically
#
# Start a pdf going
# I'm going to create a file right next to where you have the example data stored
plotPath <- file.path(dirname(path),'myPDF.pdf')
print(plotPath)
pdf(file=plotPath, width=6, height=4)
# Add to the file by calling plot commands
plot(x=tempTable$x, y=tempTable$y, main='My Plot Title', xlab='X Axis Label [units]', ylab='Y Axis Label [units]')
# Finish the file
dev.off()
