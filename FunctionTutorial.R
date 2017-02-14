##### Functions ####
# 1) Allows you to reuse code, cleans things up and dramatically reduces bugs and busy work changing code
# 2) Sometimes it is necessary to provide a function as the parameter to another function
#
# Just as you can store a number in a variable (e.g., a <- 5)
# you can store a function in a variable

myFunc <- function(a, b)
{
	return(a + b)
}

myFunc(a=5, b=6)
result <- myFunc(a=5, b=6) # Notice that we use '=' when specifying parameters and '<-' otherwise.
result # Print

# You can set default values
myFunc <- function(a=1, b, c)
{
	# You can do a bunch of things inside these curly brackets

	# In the end, you use 'return(...)' to provide back the answer you want
	return(a + b - c)
}
myFunc(b=2, c=3)		# Default value of 1 for 'a' is used
myFunc(2, 3) 			# Doesn't work. Assumes you are giving arguments in order so it thinks 'c' is missing (better to be explicit)
myFunc(c=3, b=2, a=1)	# Being explicit allows you to not worry about order of parameters
myFunc(a=1, b=2)  		# Doesn't work, we didn't specify 'c'

# Functions are like any other variable, you can pass them to functions or assign them to variables
myFunc2 <- myFunc
myFunc(a=1, b=2, c=3)
myFunc2(a=1, b=2, c=3)

callMyFunc <- function(aFunc, x, y, z)
{
	aFunc(a=x, b=y, c=z)
}
callMyFunc(myFunc, x=1, y=2, z=3)

# Could also use the same names for parameters or change defaults
callMyFunc <- function(aFunc, a, b, c=3)
{
	return(aFunc(a=a, b=b, c=c))
}
callMyFunc(myFunc, a=1, b=2)

# USE FUNCTIONS TO SIMPLIFY YOUR CODE AND PERFORM THE SAME OPERATION ELSEWHERE!!!

##### Create helper functions #####
# Typically I store these in a separate file
# See 'HelpfulFunctions.R'
normalize <- function(x)
{
	minX <- min(x)
	maxX <- max(x)
	newX <- (x-minX)/(maxX - minX)
	return(newX)
}

plotMyData <- function(x)
{
	boxplot(x, ylim=c(0,1), main=paste0('My Awesome Plot, n=', length(x)))
}

##### Analyze Data Using Helper Functions #####
# Now your code can be very simple and descriptive
myData <- 1:10
myData <- normalize(x=myData)
plotMyData(myData)

myOtherData <- 21:35
myOtherData <- normalize(x=myOtherData)
plotMyData(myOtherData)

##### Debugging #####
# What if your script or function is not working?
# You can use RStudio to help you figure it out.

myStupidFunction <- function(a1, b1, c1, d1)
{
	Values <- c(b1, c1, d1)
	return(data.table(a=a1, Values=Values))
}

# Try the following code on the console (copy and paste it and hit enter)
# myStupidFunction(a=1:3, b=1, c=3, d=NULL) # Should produce and error

# To test debugging, put a 'stop' on line 83
# Type a '#' at the beginning of line 26 and 28
# Then 'source' the file
# Then run the same command over again.
