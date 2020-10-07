##############adding bars to the plot
addBar <- function(vec, col.vec, val.lab = TRUE,
	pos = 1, label = "Total", res, write.file,
	addNet = FALSE, netAdj = .05, digits = 0, vcol = "black") {
	
	x.pos <- 0
	fam <- ifelse( write.file == "pdf", "", "Franklin Gothic Book")
	for (j in 1:length(vec)) {
		## add the bars
		polygon( x = c(x.pos, x.pos + vec[j],
			x.pos + vec[j], x.pos),
			y = c(pos -.4, pos-.4, pos+.4, pos+.4),
			border = NA, col = col.vec[j])
		## write value labels
		if (val.lab[j]) {
			if (digits == 0) mult = 100
			if (digits > 0) mult = 1
			if (round(vec[j]*100) > 0) text(x.pos + vec[j]/2, 
				pos, round(vec[j]*mult, digits),
				family = fam,
				cex = .75, col = vcol[j])
		}
		## increment the x position
		x.pos <- x.pos+vec[j]
		
		## add the NET
		if (j == length(vec) & addNet) {
			fam = ifelse(write.file == "pdf", "", "Franklin Gothic Demi")
			if (digits == 0) mult = 100
			if (digits > 0) mult = 1
			text( x.pos + netAdj, pos, round(sum(vec)*mult, digits),
				family = fam, cex = .75)
		}
	}

	## write the label
	fam = ifelse(write.file == "pdf", "", "Franklin Gothic Book")
			
	##flag for grey sublabels
	flag <- substr(label, 1, 1)
	rcol = "black"
	if (flag == "*") {
		rcol = greys['dark']
		label <- substr(label, 2, 10000)
	}
	text(0, pos, label, pos = 2, 
		family = fam,
		cex = .75, col = rcol)
}

#############Label formatting #############NOT WORKING...
format_label <- function(str) {
	##bold
	stars <- gregexpr("\\*", str)[[1]]

	if (length(stars) == 0) return(str)

	sub <- list()
	sub[[1]] <- substr(str, 1, stars[1]-1)
	for (j in 2:length(stars)) {
		sub[[j]] <- substr(str, stars[j-1]+1, stars[j]-1)
	}
	sub[[j+1]] <- substr(str, stars[j]+1, 10000)

	expr <- 'paste("'
	for (j in 1:length(sub)) {
		if (j %% 2 == 1) expr <- paste(expr, sub[[j]], sep = '"')
		if (j %% 2 == 0) expr <- paste(expr, "bold(", sub[[j]], ")", 
			sep = '"')
	}
	expr <- paste(expr, ")", sep = '')
	lab <- as.expression(expr)
}

###########Positioning labels
getLabelPosition <- function(vec) {
	cs <- cumsum(vec)

	pos <- rep(NA, length(vec))
	pos[1] <- vec[1]/2
	for (j in 2:length(vec)) {
		pos[j] <- vec[j]/2 + cs[j-1]
	}

	return(pos)
}

stackedBar <- function(list, 		##data to plot
	xlim = c(-.18, 1),		##xlim for plot
	col.vec, 			##colors
	val.lab = TRUE, 		##value labels
	res = 1, 			##resolution
	plot.width = 3.2,		##width of plot window
	write.file = "no", 		##write out a file
	bar.width = .5,			##width of bars
	col.lab = NULL, 		##column labels
	col.pos = NULL, 		##position of labels
	n.cats = NULL,			##number of categories to plot
	addNet = FALSE,			##Add NET to bars (only helpful for subsetting)
	digits = 0,			##How many digits to display (0 assumes data is from 0-1 and prints percentages)
	vcol = "black",			##Value label color
	netAdj = .05) {			##horizontal adjustment to bars

	##set up the height of the box based on the number of elements
	##(in reverse order; list should be ordered with total first, etc)
	ymin <- ifelse(is.null(col.lab), .5, 0)
	twoline <- grep("\\n", col.lab)
	if (length(twoline)>0) ymin <- -.75

	ylim <- c(length(list)+.5, ymin)

	##open the plot window (multiplied by the resolution factor)
	dev.new(width=plot.width*res, height=
		length(list)*bar.width*res)

##save to file
if (write.file!="no") {

if (write.file == "jpg") {
	src <- tempfile(fileext = ".jpg")
	jpeg(src, width = plot.width, 
		height = bar.width*length(list),
		units = 'in', res = 1000)
}
if (write.file == "pdf") {
	src <- tempfile(fileext = ".pdf")
	pdf(src, width = plot.width,
		height = bar.width*length(list))
}

}
	##Open plot window
	par(mar = rep(.1, 4))
	plot(0,0, pch = '', xlim = xlim,
		ylim = ylim, axes = FALSE,
		xlab = '', ylab = '')

	##Add data to the plot window
	if (!is.null(col.lab)) {
		fam <- ifelse(write.file == "pdf", "", "Franklin Gothic Demi")
		
	##Add the column labels
	for (j in 1:length(col.lab)) {
		if (ymin == 0) y = 0
		if (ymin != 0) y = ymin/2
		text(col.pos[j], y, col.lab[j], family = fam,
			col = col.vec[j], cex = .75, adj = c(.5,1))
	}
	}
	
	if (length(vcol)<n.cats) vlabcolors <- rep(vcol, n.cats)
	if (length(vcol)==n.cats) vlabcolors <- vcol	
	if (length(val.lab)<n.cats) val.lab = rep(val.lab, n.cats)
	##run through the list to add the bars
	for (j in 1:length(list)) {
		##skip NULL entries; add italicized subheading if applicable
		fam <- ifelse(write.file == "pdf", "", "Franklin Gothic Book")
		if (length(list[[j]]) == 0) {
			text(xlim[1], j, 
				names(list)[[j]], pos = 4, 
				family = fam, offset = -.1,
				font = 3,
				cex = .75)
			next
		}
		##add bars
		if (is.null(n.cats)) N <- length(list[[j]])
		if (!is.null(n.cats)) N <- n.cats

		addBar(vec = list[[j]][1:N], col.vec = col.vec, 
			val.lab = val.lab,
			pos = j, label = names(list)[[j]],
			res = res, write.file = write.file,
			addNet = addNet, netAdj = netAdj, digits = digits,
		      	vcol = vlabcolors)
	}

##Close the plot window
if (write.file!="no") {
	dev.off()
	dev.off()
	return(src)
}
}
