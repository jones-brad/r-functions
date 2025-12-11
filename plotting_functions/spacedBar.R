spacedBar <- function(list,	###input list to plot
                      xmin = -.18,		###xmin for plot (xmax is determined from input)
                      col.vec, 			###colors to plot
                      val.lab, 			###value labels
                      val.lab.col = "black",	      
                      res = 1, 			###resolution for plot
                      plot.width = 3.2,		###width of plot window in inches
                      write.file = "no", 	###write out a file? "pdf" or "jpg"
                      bar.width = .5,		###width of bars (determines the height of the plot)
                      n.cats = 4, 		###number of categories to plot (to exclude DK/Refused mostly)
                      col.lab = NULL, 		###column labels
                      col.lab.color = NULL,
                      center.on.which.row = NULL,
                      digits = 0,
                      addVerticalLines = FALSE,
                      shiftSmall = TRUE,
                      small_val = 5,
                      col.lab.adj = .1,
                      spaces = NULL) {		###supply spaces between bars (should be vector of length n.cats-1) (otherwise set to 0.05)
  
  ##set up the height of the box based on the number of elements
  ##(in reverse order; list should be ordered with total first, etc)
  ymax <- ifelse(length(col.lab)>0, -.5, .5)
  if (length(col.lab)>0) {
    i <- grep("\\n", col.lab)
    if (length(i) > 0) ymax = -1.5
  }
  
  ylim <- c(length(list)+.5, ymax)
  
  
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
  
  ##find get the spacers
  mat <- array(NA, c( length(list), n.cats))
  for (j in 1:length(list)) {
    if (length(list[[j]]) == 0) next
    for (k in 1:n.cats) {
      mat[j,k] <- list[[j]][k]
    }
  }
  space <- array(NA, c( length(list), n.cats - 1))
  
  if (is.null(spaces)) {
    for (j in 1:(n.cats - 1)) {
      space[,j] <- max(mat[,j], na.rm = TRUE) - mat[,j] + .05
    }
  }
  
  if (!is.null(spaces)) {
    for (j in 1:(n.cats - 1)) {
      space[,j] <- max(mat[,j], na.rm = TRUE) - mat[,j] + spaces[j]
    }
  }
  
  xmax <- max( rowSums(mat) + rowSums(space), na.rm = TRUE )
  xlim <- c(xmin, xmax)
  par(mar = rep(.1, 4))
  plot(0,0, pch = '', xlim = xlim,
       ylim = ylim, axes = FALSE,
       xlab = '', ylab = '')
  
  
  x.pos <- 0
  pos <- rep(NA, 3)
  if (is.null(col.lab.color)) col.lab.color = col.vec
  ##Add column labels
  if (length(col.lab)>0) {
    for (j in 1:length(col.lab)) {
      pos[j] <- x.pos
      sp = 0
      fam = ifelse(write.file == "pdf", "", "Franklin Gothic Demi")
      if (is.null(center.on.which.row)) { 
        text(x.pos + (max(mat[,j], na.rm = TRUE)/2), ylim[2]/2 - col.lab.adj,
             col.lab[j], family = fam, col = col.lab.color[j],
             cex = .75, adj = c(.5,0))
        x.plus <- (max(mat[,j], na.rm = TRUE))
      }
      if (!is.null(center.on.which.row)) {
        text(x.pos + 
               mat[center.on.which.row,j]/2, ylim[2]/2 - col.lab.adj,
             col.lab[j], family = fam, col = col.lab.color[j],
             cex = .75, adj = c(.5,1))
        x.plus <- mat[center.on.which.row,j]
        if (j < length(col.lab)) sp <- space[center.on.which.row,j]
      }
      
      x.pos <- x.pos + x.plus + sp
    }
  }
  
  ##run through the list to add the bars
  for (j in 1:length(list)) {
    fam = ifelse(write.file == "pdf", "", "Franklin Gothic Book")
    ##skip NULL entries
    if (length(list[[j]]) == 0) {
      
      text(xlim[1], j, names(list)[[j]], pos = 4, 
           family = fam, offset = -.75,
           font = 3,
           cex = .75)
      next
    }
    
    ##add label
    ##flag for grey sublabels
    label <- names(list)[[j]]
    flag <- substr(label, 1, 1)
    rcol = "black"
    if (flag == "*") {
      rcol = greys['dark']
      label <- substr(label, 2, 10000)
    }
    text(0, j, label, pos = 2, 
         family = fam,
         cex = .75, col = rcol)
    
    x = 0
    if (length(val.lab.col) == 1) val.lab.col <- rep(val.lab.col, n.cats)
    for (k in 1:n.cats) {
      
      addBar2(x = x, val = list[[j]][k], 
              col = col.vec[k],
              digits = digits,
              val.lab = list[[j]][k] > 0,
              pos = j, shiftSmall = shiftSmall, write.file = write.file,
              val.lab.col = val.lab.col[k], small_val = small_val)
      x = x + mat[j,k]
      if (k < n.cats) {
        x = x + space[j,k]
      }
    }
  }
  
  ##Add vertical lines
  if (addVerticalLines) {
    x.pos = 0
    for (j in 1:ncol(space)) {
      val = max(mat[,j], na.rm = TRUE) + x.pos
      #abline(v = val + spaces[j]/2)
      segments(x0 = val + spaces[j]/2, x1 = val + spaces[j]/2,
               y0 = .5, y1 = length(list)+.5, lty = c("14"))
      x.pos = val + spaces[j]
    }
  }
  
  if (write.file != "no") {
    dev.off()
    dev.off()
    return(src)
  }
}

addBar2 <- function(x, y, val, col, val.lab = TRUE,
                    pos = 1, shiftSmall = shiftSmall, write.file, val.lab.col = 'black',
                    digits = 0, too_small = TRUE, small_val = small_val) {
  
  fam = ifelse(write.file == "pdf", "", "Franklin Gothic Book")
  polygon( x = c(x, x+val, x+val, x),
           y = c(pos -.4, pos-.4, pos+.4, pos+.4),
           border = NA, col = col)
  
  ## write value labels
  if (!shiftSmall) {
    
    if (val.lab & abs(val*100) > small_val) text(x + val/2, 
                                     pos, format(round(abs(val)*100, digits), nsmall = digits),
                                     family = fam,
                                     cex = .75,
                                     col = val.lab.col)
  }
  
  if (shiftSmall) {
    shift <- ifelse(val*100 < small_val, .05, 0)
    if (shift > 0) val.lab.col = 'black'
    if (val.lab) text(x + val/2 + shift, 
                      pos, abs(round(val*100)),
                      family = fam,
                      cex = .75,
                      col = val.lab.col)
  }
}


########################Grouping sets of bars

spacedBar2 = function(list,  ###input list to plot
         xmin = -.18,            ###xmin for plot (xmax is determined from input)
         col.vec,                        ###colors to plot
         val.lab = TRUE,                        ###value labels
         val.lab.col = 'black',
         res = 1,                        ###resolution for plot
         plot.width = 3.2,               ###width of plot window in inches
         write.file = "no",      ###write out a file? "pdf" or "jpg"
         bar.width = .5,         ###width of bars (determines the height of the plot)
         n.cats = 4,             ###number of categories to plot (to exclude DK/Refused mostly)
         col.lab = NULL,                 ###column labels
         lab.pos = NULL,
         space.labs.evenly = NULL,
         lab.start = .1,
         lab.buff = 0,
         add.net = NULL,         ###add NETs to any grouped bars
         net.buff = 0,           ###add a little extra to the xmax to prevent cutting off net labels
         spaces = NULL,
         groups = NULL) {                ###supply spaces between bars (should be vector of length n.cats-1) (otherwise set to 0.05)
  
  ##set up the height of the box based on the number of elements
  ##(in reverse order; list should be ordered with total first, etc)
  ymax <- ifelse(length(col.lab)>0, -.5, .5)
  if (length(col.lab)>0) {
    i <- grep("\\n", col.lab)
    if (length(i) > 0) ymax = -.75
  }
  
  ylim <- c(length(list)+.5+lab.buff, ymax)
  
  
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
  
  ##find get the spacers
  mat <- array(NA, c( length(list), n.cats))
  for (j in 1:length(list)) {
    if (length(list[[j]]) == 0) next
    for (k in 1:n.cats) {
      mat[j,k] <- list[[j]][k]
    }
  }
  
  mat2 <- array(NA, c(nrow(mat), length(groups)))
  for (j in 1:length(groups)) {
    if (length(groups[[j]])>1) mat2[,j] <- rowSums(mat[,groups[[j]]])
    if (length(groups[[j]])==1) mat2[,j] <- mat[,groups[[j]]]
  }
  
  space <- array(NA, c( length(list), ncol(mat2) - 1))
  sp = .05
  if (!is.null(add.net)) sp = .13
  if (is.null(spaces)) {
    for (j in 1:(length(groups) - 1)) {
      space[,j] <- max(mat2[,j], na.rm = TRUE) - mat2[,j] + sp
    }
  }
  
  if (!is.null(spaces)) {
    for (j in 1:(length(groups) - 1)) {
      space[,j] <- max(mat2[,j], na.rm = TRUE) - mat2[,j] + spaces[j]
    }
  }
  
  space2 <- array(0, c( length(list), n.cats - 1))
  
  for (j in 1:(length(groups)-1)) {
    space2[,max(groups[[j]])] <- space[,j]
  }
  
  xmax <- max( rowSums(mat) + rowSums(space), na.rm = TRUE )
  ##Add a little extra space if there are nets added so they don't get cut off
  if (!is.null(add.net)) {
    if (sum(add.net)>0) xmax = xmax + net.buff
  }
  xlim <- c(xmin, xmax)
  par(mar = rep(.1, 4))
  plot(0,0, pch = '', xlim = xlim,
       ylim = ylim, axes = FALSE,
       xlab = '', ylab = '')
  
  
  x.pos <- 0
  pos <- rep(NA, 3)
  ##Add column labels
  if (length(col.lab)>0 & is.null(lab.pos) & is.null(space.labs.evenly)) {
    for (j in 1:length(col.lab)) {
      pos[j] <- x.pos
      fam = ifelse(write.file == "pdf", "", "Franklin Gothic Demi")
      text(x.pos + (max(mat[,j], na.rm = TRUE)/2), -.25,
           col.lab[j], family = fam, col = col.vec[j],
           cex = .75)
      if (is.null(spaces)) x.pos <- 
        x.pos + max(mat[,j], na.rm = TRUE) +.05
      if (!is.null(spaces)) x.pos <- 
        x.pos + max(mat[,j], na.rm = TRUE) + spaces[j]
    }
  }
  
  if (!is.null(space.labs.evenly) & is.null(lab.pos)) {
    pos = seq(from = lab.start,
              to = xmax - (xmax - xmin)/20,
              length = length(col.lab))
    for (j in 1:length(col.lab)) {
      fam = ifelse(write.file == "pdf", "", "Franklin Gothic Demi")
      text(pos[j], -.25 + lab.buff*.1,
           col.lab[j], family = fam, col = col.vec[j],
           cex = .75)
    }
  }
  
  if (!is.null(lab.pos)) {
    for (j in 1:length(col.lab)) {
      fam = ifelse(write.file == "pdf", "", "Franklin Gothic Demi")
      text(lab.pos[j], -.25 + lab.buff*.1,
           col.lab[j], family = fam, col = col.vec[j],
           cex = .75)
    }
  }
  
  if (length(val.lab.col) == 1) val.lab.col <- rep(val.lab.col, n.cats)
  
  
  ##run through the list to add the bars
  for (j in 1:length(list)) {
    fam = ifelse(write.file == "pdf", "", "Franklin Gothic Book")
    ##skip NULL entries
    if (length(list[[j]]) == 0) {
      
      text(xlim[1], j+lab.buff*.9, names(list)[[j]], pos = 4, 
           family = fam, offset = -.3,
           font = 3,
           cex = .75)
      next
    }
    ##add label
    ##flag for grey sublabels
    label <- names(list)[[j]]
    flag <- substr(label, 1, 1)
    rcol = "black"
    if (flag == "*") {
      rcol = greys['dark']
      label <- substr(label, 2, 10000)
    }
    text(0, j+lab.buff*.9, label, pos = 2, 
         family = fam,
         cex = .75, col = rcol)
    
    x = 0
    if (length(val.lab)==1) val.lab <- rep(val.lab, n.cats)
    for (k in 1:n.cats) {
      
      small = ifelse(is.null(add.net), 100, 3)
      addBar2(x = x, val = list[[j]][k], 
              col = col.vec[k], 
              val.lab.col = val.lab.col[k],
              val.lab = list[[j]][k] > 0 & val.lab[k],
              pos = j+lab.buff*.9, shiftSmall = is.null(add.net), 
              write.file = write.file,
              too_small = small)
      
      x = x + mat[j,k]
      if (k < n.cats) x = x + space2[j,k]
    }
  }
  
  fam = ifelse(write.file == "pdf", "", "Franklin Gothic Demi")
  ###Add NETs
  if (!is.null(add.net)) {
    for (j in 1:length(list)) {
      x = 0
      if (is.null(list[[j]])) next
      for (k in 1:length(groups)) {
        net = sum(list[[j]][groups[[k]]])
        x = x + net
        if (length(groups[[k]]) == 1) next
        if (net < .01) next
        if (k > 1) x.star = x + sum(space[j,(k-1):1])
        if (k == 1) x.star = x
        text(x.star + .05, j, round(net*100), cex = .75,
             family = fam, font = 2)
      }
    }
  }
  
  if (write.file != "no") {
    dev.off()
    dev.off()
    return(src)
  }
}

