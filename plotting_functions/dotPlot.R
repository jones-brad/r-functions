dotPlot <- function(array, ###########input to plot
	xlim = c(-18, 100),  ###########xlim for plot window
	autoAxis = FALSE,    ###########programatically determine axis points?
	col.vec,             ###########colors for the dots
	row.lab, 		   ###########Labels for the rows (otherwise use rownames of supplied array)
	res = 1,             ###########resolution for the plot
	plot.width = 3.2,    ###########width of plot in inches
	write.file = "no",   ###########write a file out? "pdf" or "jpg"
	dot.spacing = .5,    ###########space between dots
	legend = NULL,	   ###########legend labels 
	leg.pos = NULL,	   ###########legend position
	hollow = TRUE,	   ###########should points be drawn hollow?
	plotAxis = TRUE) {   ###########should horizontal axis be plotted?    

	##set up the height of the box based on the number of elements
	##(in reverse order; list should be ordered with total first, etc)

	ymin <- ifelse(is.null(legend), .5, 0)
	twoline <- grep("\\n", legend)
	if (length(twoline)>0) ymin <- -.5

	ylim <- c(nrow(array)+.5, ymin)


	##open the plot window (multiplied by the resolution factor)
	dev.new(width=plot.width*res, height=nrow(array)*dot.spacing*res)

##save to file
if (write.file != "no") {

if (write.file == "jpg") {
	src <- tempfile(fileext = ".jpg")
	jpeg(src, width = plot.width, height = dot.spacing*nrow(array),
		units = 'in', res = 1000)
}
if (write.file == "pdf") {
	src <- tempfile(fileext = ".pdf")
	pdf(src, width = plot.width, height = dot.spacing*nrow(array))	
}

}

lower <- ifelse(plotAxis, 1.5, .1)
	par(mar = c(lower, rep(.1, 3)))
	plot(0,0, pch = '', xlim = xlim,
		ylim = ylim, axes = FALSE,
		xlab = '', ylab = '')

if (plotAxis) {	
if (!autoAxis) tick.pos <- c(0,20,40,60,80,100)
if (autoAxis) {
	if (xlim[2] %% 10 == 0) tick.pos = seq(from = 0, to = xlim[2], by = 10)
	if (xlim[2] %% 10 != 0) tick.pos <- round(seq(from = 0, to = xlim[2], length = 5))
}
	axis(1, at = tick.pos, labels = NA,
		cex.axis = .75, col = grey(.4), tcl = -.4)

	fam <- ifelse(write.file == "pdf", "", "fgb")

	axis(1, at = tick.pos, family = fam,
		cex.axis = .75, lwd = 0, line = -.5)
}


	##run through the list to add the dots
	if (length(hollow)==1) hollow <- rep(hollow, ncol(array))

	if (!is.null(legend)) {
	for (j in 1:length(legend)) {
		fam <- ifelse(write.file == "pdf", "", "demi")
		addDot(leg.pos[j], 0, col = col.vec[j], hollow = hollow[j])
		text(leg.pos[j], 0, legend[j], col = col.vec[j],
			family = fam, cex = .75, pos = 4)
	}
	}

	for (j in 1:nrow(array)) {
		fam = ifelse(write.file == "pdf", "", "fgb")
		if (sum(is.na(array[j,]))==ncol(array)) {
			 text(xlim[1], j, 
				rownames(array)[j], pos = 4, 
				family = fam, font = 3,
				cex = .75)
			next
		}
	text(0, j, rownames(array)[j], pos = 2,
		family = fam, cex = .75)

	for (k in 1:ncol(array)) {
		##add dots
		addDot(array[j,k]*100, j, col = col.vec[k],
			hollow = hollow[k])
	}
	}

	##add value labels to the first and last in each row
	fam = ifelse(write.file == "pdf", "", "demi")
	for (j in 1:nrow(array)) {
		if (is.na(array[j,1])) next
		##min label
		
		w <- which(array[j,] == min(array[j,]))
		text(array[j,w]*100, j, round(array[j,w]*100),
			pos = 2, family = fam, cex = .75,
			col = col.vec[w])

		##max label
		w <- which(array[j,] == max(array[j,]))
		text(array[j,w]*100, j, round(array[j,w]*100),
			pos = 4, family = fam, cex = .75,
			col = col.vec[w])
	}

if (write.file != "no") {
	dev.off()
	dev.off()
	return(src)
}
}

addDot <- function(x, y, col, hollow = TRUE, cex = 1.67) {
	points(x, y, col= col, pch = 20, cex = cex)
	if (hollow) points(x, y, col = "white", pch = 20, cex = cex/2)
}
