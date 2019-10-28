plotQuad <- function(tab,
	col.vec, lab = NULL,
	format = 'jpg') {

	

	dev.new(width=20.1, height=
                20.1)

	
	if (format == 'jpg') {
		src <- tempfile(fileext = ".jpg")
		jpeg(src, width = 2.1, height = 2.1, units = "in", res = 1000)
		fam = 'fgb'
	}
	if (format == 'pdf') {
		src <- tempfile(fileext = ".pdf")
		pdf(src, width = 2.1, height = 2.1)
		fam = NULL
	}
	if (format == 'no') {
		dev.off()
		dev.new(width = 2.1, height = 2.1)
		fam = 'fgb'
	}

	par(mar = rep(.1, 4))
	plot(0, 0, pch = '', xlab = '',
		ylab = '', xlim = c(-1, 1),
		ylim = c(-1, 1), axes = FALSE)


	sc.x <- c(-1, 1, -1, 1)
	sc.y <- c(1, 1, -1, -1)

	k <- 1

	for (i in 1:2) {
	for (j in 1:2) {
		len <- sqrt(tab[i,j])
		polygon( x = c(0, sc.x[k]*len, sc.x[k]*len, 0),
			y = c(0, 0, sc.y[k]*len, sc.y[k]*len),
			col = col.vec[k], border = NA)
		text(sc.x[k]*len/2, sc.y[k]*len/2, 
			round(tab[i,j]*100), cex = .75, family = fam)

		if(!is.null(lab[k])) text(sc.x[k]*.5, 
			sc.y[k]-.1*sc.y[k], lab[k],
			cex = .5, family = fam)

		k <- k+1
	}
	}

	abline(h = 0, col = 'grey')
	abline(v = 0, col = 'grey')

	if (format == 'no') return(NULL)

	dev.off()
	dev.off()
	return(src)
}
