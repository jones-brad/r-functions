##############adding bars to the plot
addVBar <- function(vec, col.vec, val.lab = TRUE,
	pos = 1, label = "Total", res, write.file,
	addNet = FALSE, netAdj = .05, digits = 0, vcol = "black",
	whiteSpace = .2, val.lab.thresh = NULL) {
	
	adj <- .5*(1-whiteSpace)
	y.pos <- 0
	fam <- ifelse( write.file == "pdf", "", "fgb")
	for (j in 1:length(vec)) {
		## add the bars
		polygon( x = c(pos-adj, pos + adj,
			pos + adj, pos - adj),
			y = c(y.pos, y.pos, y.pos + vec[j],
				y.pos + vec[j]),
			border = NA, col = col.vec[j])
		## write value labels
		if (val.lab[j] & vec[j] > val.lab.thresh) {
			if (round(vec[j]*100) > 0) text(pos, 
				vec[j]/2 + y.pos, round(vec[j]*100),
				family = fam,
				cex = .75, col = vcol[j])
		}
		## increment the x position
		y.pos <- y.pos+vec[j]
		

	}

	## write the label
	fam = ifelse(write.file == "pdf", "", "fgb")
			
	text(pos, 0, label, pos = 1, 
		family = fam,
		cex = .75, col = 'black')
}

verticalBar <- function(list, ##data to plot
	ylim = c(-.1, 1),
	col.vec, 			##colors
	val.lab = TRUE, 		##value labels
	val.lab.thresh = .05,	##Threshold for printing value labels
	res = 1, 			##resolution
	plot.width = 4.3,		##width of plot window
	plot.height = 3,		##height of plot window
	write.file = "no", 	##write out a file
	col.lab = NULL, 		##column labels
	col.pos = NULL, 		##position of labels
	n.cats = NULL,		##number of categories to plot
	whiteSpace = 0,		##Add NET to bars (only helpful for subsetting)
	digits = 0,			##How many digits to display (0 assumes data is from 0-1 and prints percentages)
	vcol = "black",		##Value label color
	netAdj = .05) {		##horizontal adjustment to bars

	##open the plot window (multiplied by the resolution factor)
	dev.new(width=plot.width*res, height=
		plot.height*res)

##save to file
if (write.file!="no") {

if (write.file == "jpg") {
	src <- tempfile(fileext = ".jpg")
	jpeg(src, width = plot.width, 
		height = plot.height,
		units = 'in', res = 1000)
}
if (write.file == "pdf") {
	src <- tempfile(fileext = ".pdf")
	pdf(src, width = plot.width,
		height = plot.height)
}

}
	##Open plot window
	par(mar = rep(.1, 4))
	plot(0,0, pch = '', 
		xlim = c(.5, length(to.plot)+.5),
		ylim = ylim, axes = FALSE,
		xlab = '', ylab = '')
	
	if (length(vcol)<n.cats) vlabcolors <- rep(vcol, n.cats)
	if (length(vcol)==n.cats) vlabcolors <- vcol	
	if (length(val.lab)<n.cats) val.lab = rep(val.lab, n.cats)
	##run through the list to add the bars
	for (j in 1:length(list)) {
		##add bars
		if (is.null(n.cats)) N <- length(list[[j]])
		if (!is.null(n.cats)) N <- n.cats

		addVBar(vec = list[[j]][1:N], col.vec = col.vec, 
			val.lab = val.lab, val.lab.thresh = val.lab.thresh,
			pos = j, label = names(list)[[j]],
			res = res, write.file = write.file,
			whiteSpace = whiteSpace)
	}

##Close the plot window
if (write.file!="no") {
	dev.off()
	dev.off()
	return(src)
}
}
