opposingBar <- function(list, 	##input to plot
	xlim = c(-.5, .5),		##xlim for plot
	col.vec, 				##colors to plot
	res = 1, 				##resolution for plot
	plot.width = 3.2,			##width of plot window in inches
	write.file = "no", 		##write the plot to a file
	bar.width = .5,			##width of bars in inches
	n.cats = 4, 			##number of categories to plot
	col.lab = NULL, 			##column labels
	which.neg, 				##which columns should be negative
	lab.pos = NULL,
	text.column = NULL,
	addNets = FALSE,
	val.lab = TRUE,	
	vert.adj = 0,
	addVertical = FALSE) {			##label positioning

	##set up the height of the box based on the number of elements
	##(in reverse order; list should be ordered with total first, etc)
	lines <- grep("\\\n", col.lab)
	ylim <- c(length(list)+.5, ifelse(length(col.lab)>0, -.5-.5*length(lines)+vert.adj, .5))
	
	


	##open the plot window (multiplied by the resolution factor)
	dev.new(width=plot.width*res, height=
		length(list)*bar.width*res)

##save to file
if (write.file != "no") {

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

##put into matrix
mat <- array(NA, c( length(list), n.cats))
for (j in 1:length(list)) {
	if (length(list[[j]]) == 0) next
	for (k in 1:n.cats) {
		mat[j,k] <- list[[j]][k]
	}
}
nmat <- array(NA, dim(mat))
for (j in 1:ncol(mat)) nmat[,j] <- as.numeric(mat[,j])
nmat[,which.neg] <- nmat[,which.neg]*-1
mat <- nmat

if (length(addNets) == 1) {
if (addNets & length(which.neg)>1 & ncol(nmat)-length(which.neg)>1) {
	nets <- array(NA, c( nrow(mat), 2))
	nets[,1] <- rowSums( mat[,which.neg] )
	nets[,2] <- rowSums( mat[,-which.neg] )
}
if (addNets & n.cats == 2) {
	nets <- array(NA, c( nrow(mat), 2))
	nets[,1] <- mat[,which.neg]
	nets[,2] <- mat[,-which.neg]
}
}
	if (length(addNets) == length(to.plot)) {
		nets <- array(NA, c( nrow(mat), 2))
		for (j in 1:nrow(nets)) {
			if (length(addNets[[j]]) != 2) next
			nets[j,1] <- addNets[[j]][1]*-1
			nets[j,2] <- addNets[[j]][2]
		}
	}
	
	###flag for nets
	netFlag = FALSE
	if (class(addNets) == 'logical') if (addNets) netFlag = TRUE
	if (class(addNets) == 'list') netFlag = TRUE

	par(mar = rep(.1, 4))
	exp.x = c(0,0)
	if (netFlag) exp.x = c(-.2, .2)

	plot(0,0, pch = '', xlim = xlim + exp.x,
		ylim = ylim, axes = FALSE,
		xlab = '', ylab = '')


	##Add column labels
	if (length(col.lab)>0) {
		for (j in 1:length(col.lab)) {
			fam = ifelse(write.file == "pdf", "", "Franklin Gothic Demi")
			text(lab.pos[j], -.2-.2*length(lines),
				col.lab[j], family = fam, col = col.vec[j],
				cex = .75)
		}
	}

	##run through the list to add the bars
	for (j in 1:length(list)) {

		##skip NULL entries
		fam = ifelse(write.file == "pdf", "", "Franklin Gothic Book")
		if (length(list[[j]]) == 0) {
			text(xlim[1]+exp.x[1], j, names(list)[[j]], pos = 4, 
				family = fam, offset = -.1,
				font = 3,
				cex = .75)
			next
		}
		
		##add text column if supplied
		if (!is.null(text.column)) {
			fam = ifelse(write.file == "pdf", "", "Franklin Gothic Demi")
			text(xlim[2]-.05, j, text.column[j], family = fam,
				cex = .75, col = tail(col.vec, 1))
		}
		fam = ifelse(write.file == "pdf", "", "Franklin Gothic Book")
		##add label
		if (length(which.neg)>1) lpos <- min(rowSums(mat[,which.neg], 
			na.rm = TRUE), na.rm = TRUE)-.02
		if (length(which.neg)==1) lpos <- min( mat[,which.neg],
			na.rm = TRUE) - .02
		rlab <- names(list)[[j]]
		##flag for grey sublabels
		flag <- substr(rlab, 1, 1)
		rcol = "black"
		if (flag == "*") {
			rcol = greys['dark']
			rlab <- substr(rlab, 2, 10000)
		}
		text(lpos+exp.x[1], j, rlab, pos = 2, 
                family = fam, col = rcol,
                cex = .75)
	x = 0

	##plot positive bars
	cols <- 1:n.cats
	neg <- cols[which.neg]
	pos <- cols[-which.neg]
		
	if (length(val.lab)==1) val.lab <- rep(val.lab, n.cats)
	
	x.pos = 0
	#if ( !is.element(1, neg) ) neg <- rev(neg)
	for (k in neg) {
		 
		addBar2(x = x.pos, val = mat[j,k], 
			col = col.vec[k], 
			val.lab = val.lab[k],
			pos = j, write.file = write.file)
		x.pos = x.pos + mat[j,k]
	}

	x.pos = 0
	if ( !is.element(1, neg) ) pos <- rev(pos)
	for (k in pos) {
		 
		addBar2(x = x.pos, val = mat[j,k], 
			col = col.vec[k], 
			val.lab = val.lab[k] & mat[j,k] >= .05,
			pos = j, write.file = write.file)
		x.pos = x.pos + mat[j,k]
	}
	}

	if (netFlag) {
		fam = ifelse(write.file == "pdf", "", "Franklin Gothic Demi")
		for (j in 1:length(list)) {
			text( x = nets[j,1]-.13, y = j,
				abs( round(nets[j,1]*100) ),
				family = fam, cex = .75) 
			text( x = nets[j,2]+.13, y = j,
				abs( round(nets[j,2]*100) ),
				family = fam, cex = .75) 
		}
	}

	if (addVertical) {
		segments(x0 = 0, x1 = 0,
			y0 = .4, y1 = length(list)+.6,
			col = "grey", lwd = 1.5)
	}

if (write.file != "no") {
	dev.off()
	dev.off()
	return(src)
}
} 
