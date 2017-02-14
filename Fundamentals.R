# This hashtag symbol allows you to add comments to the file. It is not run as code.

# When this file is open in RStudio in the text editor, you can put your cursor
# on a line of code and hit Ctrl+Enter to run that line of code (Ctrl = Command on Macs)
# If you have any text selected, Ctrl+Enter will run just the selected text
# A quick way to run all the code is Ctrl+A (to select all) then Ctrl+Enter to run the selection

# This tutorial assumes you are using RStudio

# For now, use Ctrl+Enter to run line by line to follow along
# Getting through this file is the hardest part. Goal is to get over this hump.


##### Primitive Data types #####

# There are three basic types: numeric, character, or logical value...
# This would be how you create them and store them.
# Notice the top-right window pane filling with variables as you run each line.
myNumericValue <- 23
myCharacterValue <- 'Hello World'	  # Use single quotes if you want your text exactly as is
myCharacterValue <- "Hello \t World" # Use double quotes if you want to include things like tabs and newline characters
myLogicalValue <- TRUE 		# works
myLogicalValue <- T 		# works
myLogicalValue <- FALSE 		# works
myLogicalValue <- F 		# works
myLogicalValue <- True 		# doesn't work (case-sensitive, has to be all caps)

# Print a value to the console (The lower-left window pane... try physically typing one at the console too)
myNumericValue
myCharacterValue
myLogicalValue

# This is how you convert between types
as.character(23)
as.character(TRUE)
as.character(23+5) 		# Things inside parentheses are performed first

as.numeric('1.456')
as.numeric(T) 			# Equals 1
as.numeric(F) 			# Equals 0
as.numeric('Hello World')# Doesn't work (type, 'warnings()' in console to see warning details)
as.numeric('one') 		# Still doesn't work

as.logical('T')
as.logical('TRUE')
as.logical('true')		# Weirdly this does work
as.logical('t') 		# But this doesn't work, producing and NA with no warning
as.logical(0) 			# Only zero evaluates to false
as.logical(0.5) 		# Everything else is TRUE
as.logical(1)			# TRUE
as.logical(-35)		# TRUE


##### Operators #####

# Mathematical operators
1 + 10
1 - 10
1 * 10
T * T 	# 1, TRUE's and FALSE's are treated as 0's and 1's
F * T 	# 0
1 / 10
1^(10)
10 %/% 3  # Modulus (integer division without remainder)
10 %% 3   # Remainder of integer division
(1+1) * (3+3)^3  # Parentheses take precedent, then normal math order of operations (exponent, mult/div, add/sub)

# Logical operators
5 > 2	# Greater than
'a' < 'b'	# Less than
5 == 2	# Equal to
5 >= 5	# Greater than or equal to
5 <= 6	# Less than or equal to
5 != 5	# Not equal to
5 > 2 & 5 < 6	# AND operator
5 > 2 | 5 == 5 # OR operator
!(5 == 2) 	# NOT operator

##### Common Functions #####

# Descriptive stats
temp <- c(1,2,3,4)
min(temp)
max(temp)
mean(temp)
median(temp)
sd(temp)
mad(temp) 	# sd that is robust to outliers
sum(temp)

# Logarithm
exp(1)	# exp is the exponential function, exp(1) = e
log(10)	# Default of log is natural log
log(exp(1))
log(10, base=10)
log10(10)
log2(2)

# Trigonometry
sin(pi)
cos(pi)
tan(pi)
asin(0)
acos(0)
atan(0)


##### Control Statements #####

### If-then statements
a <- 1
if(a > 1)
{
	print('High')
} else
{
	print('Lo')
}

### For loops
a <- c(1,3,5,7,9)
for(thing in a)
{
	print(thing)
}

# Also applies to other types of data
b <- c('a','c','e','g')
for(thing in b)
{
	print(thing)
}

# Use seq_along to automatically loop over the indicies of the thing you have
for(thing in seq_along(a))
{
	print(thing)
}

### While loops
a <- 1
while(a <= 5)
{
	print(a)
	a <- a + 1
}


##### Data Objects #####

#### Vectors ####
# A vector is a list of items of all the same type

# This is how you store a vector of items
myNumericVector1 <- c(1, 3.0, 1, 3.0, 5.5, 1.2)
myNumericVector2 <- 1:10 # Commonly used shortcut for a sequence of numbers
myNumericVector3 <- seq(from=0, to=1, by=0.1)
myCharacterVector <- c('Be', 'my', 'Valentine', 'Be', 'my')
myLogicalVector <- c(T, T, F, F, T)

# Print the values to the console
myNumericVector1
myNumericVector2
myNumericVector3
myCharacterVector
myLogicalVector

# This is how you get values from a vector (inidicies start at 1)
myNumericVector1[1]
myNumericVector1[0] # Doesn't work (there is no 0th index in a vector)
myNumericVector1[1]

# This is how you combine or add to a vector
temp <- c('a','b','c')
temp <- c(temp, 'd')
temp # Print
temp[5] <- 'e'
temp # Print
temp <- c(temp, temp)
temp # Print

# Practically all opterators and functions generally work on vectors
temp <- c(1, 2, 3, 4)
(temp * temp)^3
temp >= 3
temp >= 3 & temp == 3
sin(temp)

##### Lists #####
# A list is like a vector except for three things
# 1) Each item has a name
# 2) A list can hold different types of things instead of just one type like vector
# 3) Access to an individual item is done with double brackets [[]] instead of single []
# Because the elements can be of differen types, most operators and functions don't work on lists
# But, lists are still a great way to keep track of and access data/information

myWeirdList <- list(a=1, b='Hi', c=TRUE, d=c('some','vector','of','things'), e=list(e='other stuff', f=5))
myWeirdList # Print the list contents

# Getting things out of lists
myWeirdList$d 			# This is the typical approach for doing this
myWeirdList[['d']]		# This allows you to get one thing at a time
myWeirdList[c('a','d')]	# This allow you to get more than one thing at a time but RETURNS ANOTHER LIST because you don't know if all the things are of the same type

myWeirdList$d[1] 		# e.g., this returns the first element in the vector of words contained in 'd'
myWeirdList['d'][1]		# This returns your first element OF THE LIST CONTAINED IN A LIST
myWeirdList['d'][[1]]	# This returns your first element OF THE LIST WITHOUT PUTTING IT IN A LIST (i.e., the vector of words)
myWeirdList[['d']][1]	# This returns the first element in the vector of words contained in 'd'

##### data.frame (i.e., table) #####

# A data.frame is a list of vectors
myDF <- data.frame(myCol1=1:6, myCol2=c('a','b','c','d','e','f'), myCol3=T, myGrps=c(1,1,1,2,2,2))
myDF 			# Print the data.frame
myDF$myCol1 		# Get one of the column vectors by name
myDF$'myCol1' 		# Another way to get column vectors by name
myDF[3,'myCol3']	# Get row 3 of the 'myCol3' column
myDF[3, ]			# Get row 3 of all the columns (returns another data.frame)
myDF[3, c('myCol1', 'myCol3')]	# Get row 3 of the specified columns (returns another data.frame)
myDF[c(1,3,5), ]	# Get rows 1, 3, 5 of the data.frame
myDF[c(T,F,T,F,T,F), ]	# Get rows 1, 3, and 5, using T's and F's
myDF$myNewCol1 <- c('a1','b1','c1','d1','e1','f1') # Create new columns by just typing it like it already exists and giving it a value
myDF$myNewCol2 <- c('a1','b1','c1','d1','e1') # Doesn't work, size of vector needs to be the same size
myDF$myNewCol2 <- 6 # Or just a single value that all the rows will get
myDF$myNewCol3 <- c(1,2) # Or a multiple of the number of rows and the vector will be repeated
myDF$myNewCol4 <- (myDF$myCol1 - min(myDF$myCol1))/(max(myDF$myCol1) - min(myDF$myCol1)) # Also perform calcs using existing information
myDF

# "Quiz" What do you think will happen in the following line of code
myDF[myDF$myCol1 > 3, ]	# Thoughts? Hint. Look at line 129
myDF[myDF$myCol1 > 3]	# Doesn't work, you have to have the comma to specify what columns you want


##### Loading Packages #####
# Packages are zip files of R code that provide functions and different types of data objects
# that are useful for specific topics (e.g., plotting, 3D visuals, differential equations, matrix math, statistics)
# To install a package, use the 'Packages' tab in the lower-right window pane

# GO AHEAD AND INSTALL THE 'data.table' package
# Now, load the 'data.table' library you installed as follows
library(data.table) # Now we can create data.table objects and use them
?data.table         # This is how you access help on any function or package

##### How to "Source" a File #####
# Sometimes you have your own code in multiple files
# You can "run" the code using the 'source' command
# Srouce the following file with some helpful functions
source('~/Desktop/RTutorial/HelpfulFunctions.R') # '~' means your 'home' directory like C:/Users/<username>/ on Windows or /Users/<username/ on Mac

##### data.table (i.e., fancy, fast, table) #####
# data.tables ARE data.frames, but with different syntax
# (99%) of functions that take a data.frame can take a data.table
# Can convert from one type to the other with no memory or time cost
# This is what all "big data" folks use when working with R. Needed for speed and efficiency
# Somewhat complicated to get the hang of but, eventually extremely convenient
# If possible, just start by using these instead of data.frames and convert when necessary

# Still just a list of vectors and all the same commands work as before
myDT <- data.table(myCol1=1:6, myCol2=c('a','b','c','d','e','f'), myCol3=T, myGrps=c(1,1,1,2,2,2)) # Same as before
myDT 			# Print the data.table (SAME AS BEFORE)
myDT$myCol1 		# Get one of the column vectors by name (SAME AS BEFORE)
myDT$'myCol1' 		# Another way to get column vectors by name (SAME AS BEFORE)
myDT[3,'myCol2']	# Get row 3 of the 'myCol3' column (SAME AS BEFORE)
myDT[3, ]			# Get row 3 of all the columns (returns another data.frame)
myDT[, c('myCol1', 'myCol3')]	# Get the specified columns (returns another data.frame)
myDT[c(1,3,5), ]	# Get rows 1, 3, 5 of the data.frame
myDT[c(T,F,T,F,T,F), ]	# Get rows 1, 3, and 5, using T's and F's

# How to convert between data.frames and data.tables
myDT <- as.data.table(myDF)
myDF <- as.data.frame(myDT)

# One significant difference of data.table's is the ability to use column names directly (enables auto-complete)
myDT[myCol1 > 3]	# Filter rows (the comma is optional, without the comma, assumes you want all the columns)
myDT[myCol1 > 3, myCol1]
myDT[, myCol3]
myDT[, list(myCol1, myCol3)] 	# Need to use 'list(...)' instead of 'c(...)' when using the names directly
myDT[, c('myCol1', 'myCol3')] # Same result

# This last thing is slightly more complicated when using variables to pass column names
temp1 <- 'myCol1'
temp2 <- 'myCol3'
myDT[, mget(c(temp1, temp2))] # If using variables to specify columns, use 'mget' to 'get' what is inside the passed variables (mget stands for multiple get, i.e., we need to get each one in the vector)

# You also get the ability to CALCULATE things (can't do this with data.frames)
myDT[, list(myCalc1=2*myCol1)]
myDT[, list(myCalc1=2*myCol1, myCalc2=3*myCol1)]

# The two lines above create new data.tables... to edit the existing data table, you do this
myDT[, ':='(myCalc1=2*myCol1)]
myDT[, ':='(myCalc1=2*myCol1, myCalc2=3*myCol1)]

# Now for the other major benefit -- performing calculations per group
myDT[, list(myMean=mean(myCol1), mySD=sd(myCol1)), by=.(myGrps)] # Super easy way to get summary stats per experimental condition
# For example, let's create some fake data
temp <- data.table(CellType=c('LNCaP','PC3'), Tx=rep(c('Control','Drug'), each=2), Replicate=rep(rep(c(1,2,3), each=4)), Viability=runif(12, 0, 100))
temp # Print the table to see it
temp[, list(mean=mean(Viability), sd=sd(Viability)), by=list(CellType, Tx)]

##### Sorting things #####

#sorting and ordering numeric values
temp <- c(1,1,2,3,5,6,7,4)
order(temp)
temp[order(temp)]
sort(temp)
order(temp, decreasing = TRUE) # Reverse order
order(-temp) 				 # Reverse order (shorthand notation for numeric things)

# sorting and ordering character values
temp <- c('a','ab','b','ba','A','B')
sort(temp)
sort(-temp)  # Doesn't work
sort(temp, decreasing = TRUE)

# Use it to sort our data.table
temp <- data.table(CellType=c('LNCaP','PC3'), Tx=rep(c('Control','Drug'), each=2), Replicate=rep(rep(c(1,2,3), each=4)), Viability=runif(12, 0, 100))
temp # Print the original order
temp <- temp[order(CellType, -Tx)] # Sort
temp # Print the new order


##### Special Values #####
NA
NULL
Inf
-Inf
pi

# Logical operators on special values
NA == NA 			# NA
NULL == NULL 		# logical(0) empty logical vector
Inf == Inf 		# TRUE
NA == NULL 		# logical(0) empty logical vector
NA == Inf 		# NA

is.na(NA)			# TRUE
is.finite(NA)		# FALSE
is.infinite(NA)	# FALSE

is.na(Inf)		# FALSE
is.finite(Inf)		# FALSE
is.infinite(Inf)	# TRUE

is.na(NULL)		# logical(0) (with warning message)
is.finite(NULL)	# logical(0)
is.infinite(NULL)	# logical(0)


##### Factors #####
# You can also create 'factors' (generally a tricky data type to work with)
# Stores information as two vectors
# The levels character vector of the unique values you provided
# The provided data converted to integers, representing the index of the value in the levels vectors
# If you need to work with them, read this (https://www.stat.berkeley.edu/classes/s133/factors.html)
# Some plotting functions can use these.
# If for some reason some of your data is a factor, I generally convert to numeric, character, or logical
# and only convert it to factor when necessary for a function
myFactor_Numeric <- factor(myNumericVector1, ordered=T)
myFactor_Numeric
myFactor_Character <- factor(myCharacterVector) # This leaves the factors unordered
myFactor_Character
myFactor_Logical <- factor(myLogicalVector, labels=c('T','F'), ordered=T) # This uses the specified labels to determine how to present the values and their appropriate order
myFactor_Logical

