# install.packages("reshape2")
library(data.table)

#' Read table from the clipboard
#'
#' This is a cool way to import data using the clipboard. The clipboard table
#' is typically copied as a tab delimited text 'file' connection
#'
#' @param os - c('mac','win'), string value indicating the platform to use
#' @param header - TRUE or FALSE, whether the header is included in the copied table
#' @param sep - text, defining the separator character used between values (needs to be in double quotes)
#' @param use.data.table - TRUE or FALSE, whether to return a data.table (default)
#' @param ... - additional variables supplied are passed onto the underlying read.table function (e.g., stringsAsFactors, comment.char, col.names)
#'
#'  @export
read.clipboard <- function(os=c('mac','win'), header=T, sep="\t", use.data.table=T, ...)
{
	if(os[1]=='mac')
	{
		ret <- read.table(pipe('pbpaste'), header=header, sep=sep, ...) # Mac
	}
	else
	{
		ret <- read.table('clipboard', header=header, sep=sep, ...) # Windows
	}
	if(use.data.table)
	{
		return(data.table(ret))
	}
	else
	{
		return(ret)
	}
}

#' error.bar
#'
#' Add errorbars on an existing plot
#'
#' @param x - x locations to draw center point of the error bars
#' @param y - y corresponding y locations to draw the center point of the error bars
#' @param upper - the upper distance to draw the error bars
#' @param lower - the lower distance to draw the error bars (by default, drawn the same distance as defined by "upper")
#' @param length - the width/length of the error bar tops and bottoms
#'
#' @export
error.bar <- function(x, y, upper, lower=upper, length=0.1, ...)
{
	require(reshape2)
	# if(length(x) != length(y) | (length(y) != length(lower) | length(lower) != length(upper))
	#      stop("vectors must be same length")
	has.upper <- (!is.null(upper) && !is.na(upper) && length(upper) > 0)
	has.lower <- (!is.null(lower) && !is.na(lower) && length(lower) > 0)
	if(has.upper)
	{
		if(has.lower)
		{
			suppressWarnings(arrows(x, y+upper, x, y-lower, angle=90, code=3, length=length, ...))
		}
		else
		{
			suppressWarnings(arrows(x, y+upper, x, y, angle=90, code=1, length=length, ...))
		}
	}
	else if(has.lower)
	{
		suppressWarnings(arrows(x, y, x, y-lower, angle=90, code=2, length=length, ...))
	}
}

#' Grouped Bar Plots
#'
#' Allows you to plot grouped bar plots based upon a 'grouping' variable or column in the data
#' Requires the error.bar function
#'
#' @param dt - the table with the data
#' @param y.column - name of the columne with the y-values you would like to plot
#' @param color.column - name of the column that should be associated with different bar colors
#' @param group.column - name of the column that should be associated with different groups
#' @param error.upper.column - name of the column with the magnitudes of the upper error bars (use NULL to avoid plotting, default)
#' @param error.lower.column - name of the column with the magnitudes of the lower error bars (default is error.upper.Column, use NULL to avoid plotting)
#' @param main - title for the plot
#' @param ylab - y label
#' @param xlab - x label
#' @param color.names - vector of names to override the color names contained in the table (must be the same length as produced by the table)
#' @param group.names - vector of names to override the group names contained in the table (must be the same length as produced by the table)
#' @param color.color - vector of color values (e.g., c('black', rgb(...), gray(...))) to override the automatically produced colors
#' @param rotate.x.lables - TRUE or FALSE whether to rotate the x labels 90 degrees or not so they fit (default FALSE)
#' @param plot.border - TRUE or FALSE whether to plot a black line around the plot (default TRUE)
#' @param args.error.bar - list with arguments for the error.bar function (default list(length=0.1)) (See error.bar specified in this file)
#' @param legend - TRUE or FALSE, whether to include a legend or not in the graph (only when a group.column is specified and present)
#' @param legend.border - TRUE or FALSE whether to plot a border around the legend
#' @param args.legend - list of parameters to pass to the 'legend' function (overrides automatically determined parameters) (see ?legend)
#' @param mar - numeric vector indicating the margins our the plot border. Units are lines (default c(4.5,4.5,2,2) = c(lower, left, upper, right))
#' @param ... - additional arguments that are passed to the barplot function (see ?barplot)
#'
#' @export
bar <- function(dt, y.column, color.column, group.column=NULL, error.upper.column=NULL, error.lower.column=error.upper.column,
			 main=NULL, ylab=NULL, xlab=NULL, color.names=NULL, group.names=NULL, color.colors=NULL, rotate.x.lables=F, plot.border=T,
			 args.error.bar=list(length=0.1),
			 legend=TRUE, legend.border=F, args.legend=list(),
			 mar=c(4.5,4.5,2,2), ...)
{
	# Store the display names
	color.names.display <- color.names
	group.names.display <- group.names

	# Convert the table to a data.table
	dt <- data.table(dt)

	# Get the y values to plot
	y <- dt[[y.column]]

	# Get check the specified color and group columns
	if(is.null(color.column) || !(color.column %in% names(dt)))
	{
		stop("At least a color.column must be specified and must be present in the provided table of data. Aborting.")
	}
	if(!is.null(group.column) && !(group.column %in% names(dt)))
	{
		stop("The specified group column is not present in the provided table of data")
	}

	# Get the matrix needed for barplot
	if(!is.null(group.column))
	{
		subDT <- dt[, mget(c(color.column, group.column, y.column, error.upper.column, error.lower.column))]
		tempCast <- dcast(subDT, as.formula(paste(color.column, '~', group.column)), value.var=y.column)
		color.names <- tempCast[[1]]
		group.names <- names(tempCast)[2:ncol(tempCast)]
		mat <- as.matrix(tempCast[, 2:ncol(tempCast)])# Get error bar magnitudes if possible
	}
	else
	{
		subDT <- dt[, mget(c(color.column, y.column, error.upper.column, error.lower.column))]
		mat <- y
		color.names <- dt[[color.column]]
	}

	# Copy over group and color names from the table if not specified or if specified incorrectly
	if(is.null(color.names.display))
	{
		color.names.display <- color.names
	}
	else if(length(color.names.display) != length(color.names))
	{
		warning("The number of provided color names does not match the number being plotted. Using the color names in the color.column.")
		color.names.display <- color.names
	}
	if(!is.null(group.column))
	{
		if(is.null(group.names.display))
		{
			group.names.display <- group.names
		}
		else if(length(group.names.display) != length(group.names))
		{
			warning("The number of provided group names does not match the number being plotted. Using the group names in the group.column.")
			group.names.display <- group.names
		}
	}

	# Detect whether or not upper and lower error bars will be plotted
	has.upper <- FALSE
	if(!is.null(error.upper.column) && error.upper.column %in% names(dt))
	{
		has.upper <- TRUE
	}
	has.lower <- FALSE
	if(!is.null(error.lower.column) && error.lower.column %in% names(dt))
	{
		has.lower <- TRUE
	}

	if(legend && !is.null(group.column))
	{
		args.legend.temp <- list(x="topright", bty=if(!legend.border)"n" else "o", inset=c(0,0))

		if(is.list(args.legend))
		{
			args.legend <- modifyList(args.legend.temp, args.legend)
		}
		else
		{
			args.legend <- args.legend.temp
		}
	}
	else
	{
		args.legend <- NULL
		group.names.display <- NULL
	}

	# Determine the extents of the axes to plot
	if(has.upper)
	{
		ymax <- max(y + dt[[error.upper.column]])*21/20
	}
	else
	{
		ymax <- max(y)
	}
	if(has.lower)
	{
		ymin <- min(y - dt[[error.lower.column]])*21/20
	}
	else
	{
		ymin <- min(y)
	}

	# Compile the arguments to give to barplot
	if(is.null(color.colors))
	{
		color.colors <- hcl(h=seq(0,270, 270/(length(color.names)))[-length(color.names)])
	}
	else if(length(color.colors) != length(color.names))
	{
		warning("The number of colors does not match the number of color.names for the table.")
	}

	if(!is.null(group.column))
	{
		args.barplot <- list(beside=TRUE, height=mat, ylim=c(min(0, ymin), max(0,ymax)), main=main, names.arg=group.names.display,
						 col=color.colors,
						 legend.text=color.names.display, args.legend=args.legend, xpd=TRUE,
						 xlab=if(is.null(xlab)) group.column else xlab,
						 ylab=if(is.null(ylab)) y.column else ylab)
	}
	else
	{
		args.barplot <- list(beside=TRUE, height=mat, ylim=c(min(0, ymin), max(0,ymax)), main=main, names.arg=color.names.display,
						 col=color.colors,
						 legend.text=NULL, args.legend=NULL, xpd=TRUE,
						 xlab=if(is.null(xlab)) group.column else xlab,
						 ylab=if(is.null(ylab)) y.column else ylab)
	}

	args.barplot <- modifyList(args.barplot, list(...))

	# Rotate x-axis labels if desired
	if(rotate.x.lables)
	{
		args.barplot <- modifyList(args.barplot, list(las=2))
	}

	# Set the plot margins
	par(mar=mar)

	# If we need to, plot error bars
	if(has.upper || has.lower)
	{
		# Then plot some errobars
		# Sort things appropriately if we have a grouped bar plot, otherwise, no need to
		if(!is.null(group.column))
		{
			# First turn the color and group columns into factors so we can order things 'manually'
			subDT[[color.column]] <- factor( as.character(subDT[[color.column]]), levels=color.names)
			subDT[[group.column]] <- factor( as.character(subDT[[group.column]]), levels=group.names)
			subDT <- subDT[order(subDT[[group.column]], subDT[[color.column]])]
		}

		# Get error bar magnitudes if possible
		upper <- NULL
		if(has.upper)
		{
			upper <- subDT[[error.upper.column]]
		}
		lower <- NULL
		if(has.lower)
		{
			lower <- subDT[[error.lower.column]]
		}

		# Get the xlocations of where to place the error bars
		errloc <- as.vector(do.call(barplot, args.barplot))

		# Compile the error bar arguments
		upper
		args.error.final <- list(x=errloc, y=subDT[[y.column]], upper=subDT[[error.upper.column]], lower=lower)
		args.error.final <- modifyList(args.error.final, args.error.bar)

		# Draw the error bars
		do.call(error.bar, args.error.final)
	}
	else
	{
		# Just plot the bars
		do.call(barplot, args.barplot)
	}

	# If a plot border is desired, draw it
	if(plot.border) box()
}

#' wilcox.test.combined
#'
#' Function for performing wilcox.test's to compare two conditions,
#' producing p-values for each experiment and an overal combined p-value
#' for all experiments using the Lehman procedure.
#'
#' @param data - table of data to be analyzed
#' @param replCols - vector of column names that specify the biological replicate groups of the data
#' @param condCol - name of the column that specifies the conditions to be compared (must be two and only two conditions)
#' @param valCol - name of the column that holds the numeric values to be compared
#' @param exact - TRUE, FALSE, or NULL that is passed to the wilcox.test 'exact' parameter
#' @param two.tailed - TRUE or FALSE whether to have a one-sided or two-sided test
#'
#' @export
wilcox.test.combined <- function(data, replCols, condCol, valCol, exact=NULL, two.tailed=TRUE)
{
	require(data.table)
	x1 <- data.table(data)

	getStats <- function(x, y, cond1, cond2, ...)
	{
		x <- x[is.finite(x)]
		y <- y[is.finite(y)]
		if(length(x) == 0 || length(y) == 0)
		{
			# This results in a missing row in the results details table for the experiment with either missing x or y data
			return(NULL)
		}
		temp <- wilcox.test(x=x, y=y, ...)
		W <- as.numeric(temp$statistic)

		counts <- table(c(x, y))

		n.x <- length(x)
		n.y <- length(y)
		N <- n.x + n.y

		# Taken from R source code for wilcox.test
		z <- W - n.x * n.y / 2
		z <- z - sign(z)*0.5
		SIGMA <- sqrt((n.x * n.y / 12) * ((n.x + n.y + 1) - sum(counts^3 - counts) / ((n.x + n.y) * (n.x + n.y - 1))))
		z <- z/SIGMA

		p1 <- 2*pnorm(-abs(z))

		p.approx <- 2*pnorm(-abs(z))

		return(list(W=W, p.value=temp$p.value, N=N, median.x=median(x), median.y=median(y), n.x=n.x, n.y=n.y, E=n.x * n.y / 2, V=SIGMA^2, z.score=z, p.value.approx=p.approx))
	}

	conds <- unique(x1[[condCol]])
	if(length(conds) != 2)
	{
		stop("Must have 2 and only 2 conditions to compare.")
	}
	if(conds[1] < conds[2])
	{
		conds <- rev(conds)
	}
	x2 <- x1[,getStats(x=.SD[get(condCol)==conds[1]][[valCol]], y=.SD[get(condCol)==conds[2]][[valCol]], exact=exact), by=replCols]
	# x2 <- x1[,getStats(x=.SD[get(condCol)==conds[1]][[valCol]], y=.SD[get(condCol)==conds[2]][[valCol]])$p.value, by=replCols]

	x2[,':='(Wi=W/(N+1), Ei=E/(N+1), Vi=V/((N+1)^2)), by=replCols]

	Wtot <- sum(x2$Wi)
	Etot <- sum(x2$Ei)
	Vtot <- sum(x2$Vi)

	ztot <- (Wtot-Etot)/(sqrt(Vtot))

	if(two.tailed)
	{
		p.overall <- 2*pnorm(-abs((Wtot-Etot)/(sqrt(Vtot))))
	}
	else
	{
		p.overall <- pnorm(-abs((Wtot-Etot)/(sqrt(Vtot))))
	}

	# Gather results
	prettySummary <- getPrettySummary(x2, conds[1], conds[2], includeVals=TRUE)
	x <- list(summary=prettySummary, details=x2, p.overall=p.overall, alpha.prime=1-(1-p.overall)^(nrow(x2)), cond1=conds[1], cond2=conds[2], Wtot=Wtot, Etot=Etot, Vtot=Vtot, z.score=ztot)

	# Print summary
	if(x$p.overall < 1e-3)
	{
		formatted.p.value <- formatC(signif(x$p.overall,digits=3), digits=1, format="e", flag="#")
	}
	else
	{
		formatted.p.value <- formatC(signif(x$p.overall,digits=3), digits=2, format="fg", flag="#")
	}
	cat('%%%% OVERALL STATS %%%%\n')
	cat('% ', x$cond1, ' vs. ', x$cond2, '\n', sep='')
	cat('% Overall p.value =', formatted.p.value, '\n')
	cat('% Overall W =', x$Wtot, '\n')
	cat('% Overall E =', x$Etot, '\n')
	cat('% Overall V =', x$Vtot, '\n')
	cat('% Overall z.score =', x$z.score, '\n\n')
	print(x$summary)

	return(x)
}

#' getPSymbol
#'
#' Simple helper function to produce significance symbols.
#' (i.e., * <= 0.05, ** <= 0.01, *** <= 0.001)
#'
#' @param pval - value or vector of p-values
#'
#' @export
getPSymbol <- function(pval)
{
	ret <- rep('', length(pval))
	ret[pval <= 0.05] <- '*'
	ret[pval <= 0.01] <- '**'
	ret[pval <= 0.001] <- '***'
	return(ret)
}

#' getDeltaSymbol
#'
#' Simple helper function to compare two values and produce a
#' symbol the indicates if V1 is greater than, less than, or
#' equal to V2. The function works with vectors
#'
#' @param V1 - value or vector of numerics
#' @param V2 - value or vector of numerics
#'
#' @export
getDeltaSymbol <- function(V1, V2)
{
	ret <- rep('=', length(V1))
	ret[V1 < V2] <- '<'
	ret[V1 > V2] <- '>'
	return(ret)
}

#' getPrettySummary
#'
#' Get a pretty summary of wilcox.test.combined results list.
#' Call the results list 'ret'
#'
#' @param deets - ret$details
#' @param cond.x - ret$cond1
#' @param cond.y - ret$cond2
#' @param includeVals - TRUE or FALSE whether to include median values of each subgroup in the summary table
#'
#' @export
getPrettySummary <- function(deets, cond.x, cond.y, includeVals=F)
{
	idcols <- names(deets)[!(names(deets) %in% c('W','p.value','N','median.x','median.y','n.x','n.y','E','V','z.score','p.value.approx','Wi','Ei','Vi'))]
	ret <- copy(deets[,c(idcols, 'W','z.score','p.value','n.x','n.y','median.x','median.y'), with=F])
	ret$p.symbol <- getPSymbol(ret$p.value)
	ret$p.value <- sprintf('%1.3f', deets$p.value)
	ret$difference.symbol <- getDeltaSymbol(ret$median.x, ret$median.y)
	ret$log2.ratio <- log2(ret$median.x/ret$median.y)
	setcolorder(ret, c(idcols,'W','n.x','n.y','median.x','difference.symbol','median.y','log2.ratio','z.score','p.value','p.symbol'))
	if(includeVals)
	{
		setnames(ret, c(idcols,'W','n.x','n.y','median.x','median.y','difference.symbol'), c(idcols,'U',paste0('n.',cond.x),paste0('n.',cond.y),paste0('median.',cond.x),paste0('median.',cond.y),'.'))
	}
	else
	{
		ret$median.x <- NULL
		ret$median.y <- NULL
		ret$difference.symbol <- NULL
		setnames(ret, c(idcols,'W','n.x','n.y'), c(idcols,'U',paste0('n.',cond.x),paste0('n.',cond.y)))
	}
	ret$log2.ratio <- sprintf('%1.2f', ret$log2.ratio)
	return(ret)
}

#' normalize
#'
#' Function for normalizing a vector of numbers between its min and max.
#' It is intended to be used with data.table's to normalize subgroups of data.
#'
#' @param x - the vector to be normalized
#'
#' @export
normalize <- function(x)
{
	minX <- min(x)
	maxX <- max(x)
	newX <- (x-minX)/(maxX - minX)
	return(newX)
}
