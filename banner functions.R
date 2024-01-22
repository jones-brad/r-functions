## First column: value labels; subsequent columns banner point levels

## First rows: header; subsequent rows: banner category, banner label, 
##value labels+NETs
# Header varname+ label, base, survey dates

# Function wants a list of questions (each element of the list should be a column in the dataset, 
# list entries include instructions for how to calcuate the banners (e.g. add nets, etc)

# Also a list of bannerpoints that will serve as the columns for each banner
# 'control' list includes some info for the header, and the weight to use
# outputs a set of .csv files with the results

create_banners = function(questions, bannerpoints,
                          control, pause = FALSE) {
  
  ## set up new directory
  dir.create("banners")
  
  ##calculate the number of columns needed
  nbanner = 0
  for (j in 1:length(bannerpoints)) {
    nbanner = nbanner + length(bannerpoints[[j]]$levels)
  }
  ncol = length(bannerpoints) + nbanner
  
  for (j in 1:length(questions)) {
    
    ##Set up the header
    var = names(questions)[j]
    lab = attr(dat[[var]], 'label')
    base = ifelse(is.null(questions[[j]]$base),
                  control$base, questions[[j]]$basenm)
    header = c(
      var,
      base,
      control$dates, "",
      paste("Question wording", ": ", lab, sep = "")
    )
    
    ##get the base
    inds = 1:nrow(dat)
    if (!is.null(questions[[j]]$base)) inds = questions[[j]]$base
    
    ## standard tables
    if (is.null(questions[[j]]$summary)) {
      ##Calculate the number of rows needed for a standard table
      nrow = length(header) + 1 + length(questions[[j]]$levels) +
        1 + length(questions[[j]]$nets) + 1
      
      ## value labels
      tab = wxtab2(dat$cons[inds],
                   dat[[var]][inds],
                   dat[[control$weight]][inds],
                   round = TRUE,
                   nets = questions[[j]]$nets)
      
      vlab = colnames(tab)[questions[[j]]$levels]
      if (!is.null(questions[[j]]$nets)) vlab = c(vlab,
                                                  names(questions[[j]]$nets))
      vlab = c(vlab, 'n')
      
      ## add NET indicator
      vlab2 = vlab
      if (!is.null(questions[[j]]$nets)) {
        i = (length(questions[[j]]$levels)+1):
          (length(questions[[j]]$levels)+length(questions[[j]]$nets))
        vlab2[i] = paste("NET:", vlab2[i])
      }
      
      ncats = length(questions[[j]]$levels) +
        length(questions[[j]]$nets)
      
    }
    
    ##summary tables
    if (!is.null(questions[[j]]$summary)) {
      svars = names(dat)[grep(questions[[j]]$regex, names(dat))]
      vec = rep(NA, length(svars))
      vlab2 = rep('', length(svars))
      for (r in 1:length(vec)) {
        vec[r] = wmean_bygroup(is.element(dat[[svars[r]]][inds],
                                          questions[[j]]$levels),
                               dat$cons[inds],
                               dat[[control$weight]][inds])
        vlab2[r] = attr(dat[[svars[r]]], 'label')
      }
      summary_order = order(vec, decreasing = TRUE)
      vlab2 = vlab2[summary_order]
      vlab2 = c(vlab2, 'n')
      ncats = length(vlab2)-1
      
      ##add a bit to the header
      header = c(header, paste("Summary table of (",
                 paste(
                   names(attr(dat[[svars[1]]], 'labels')[questions[[j]]$levels]),
                   collapse = ", "), ") responses", sep = ""))
      
      ##Calculate the number of rows needed for a summary table
      nrow = length(header) + 1 + length(svars) + 2
    }
    
    ##container for results
    res = array('', c(nrow, ncol))
    
    ##add the header
    res[1:length(header),1] = header
    
    row_pos = length(header)+3

    res[row_pos:(row_pos+ncats),1] = vlab2
    

    ## counter for column positioning
    col_pos = 2
    for (k in 1:length(bannerpoints)) {
      ##var name for the banner
      x = bannerpoints[[k]]$var
      
      bnames = names(tab2(dat[[x]])[bannerpoints[[k]]$levels])
      
      ##number of banner points
      lev = bannerpoints[[k]]$levels
      nlev = length(lev)
      
      ## add column labels
      res[row_pos-2,col_pos] = names(bannerpoints)[k]
      res[row_pos-1,col_pos:(col_pos+nlev-1)] = bnames
      
      ## Handle summary tables
      if (!is.null(questions[[j]]$summary)) {
        ##container for output
        output = array(NA, c(length(svars), length(bannerpoints[[k]]$levels)))
        colnames(output) = bnames
        ##fill the container
        for (r in 1:nrow(output)) {
          mu = wmean_bygroup(is.element(dat[[svars[r]]][inds],
                                        questions[[j]]$levels),
                             as_factor(dat[[x]][inds]),
                             dat[[control$weight]][inds])
          int = intersect(bnames, names(mu))
          output[r,int] = mu[int]
        }
        output = round(output[summary_order,]*100)
        
        ##add n to the bottom row
        n = table(dat[[x]][inds])[bannerpoints[[k]]$levels]
        if (class(output)[1]=='numeric') output = c(output, n)
        if (class(output)[1]=='matrix') {
          output = rbind(output, n)
        }
        
        ##insert values
        res[row_pos:(row_pos+ncats),
            col_pos:(col_pos+nlev-1)] = output
        
        ## update the column position
        col_pos = col_pos + nlev
        
        next
      }
      
      
      tab = wxtab2(dat[[x]][inds],
                   dat[[var]][inds],
                   dat[[control$weight]][inds],
                   round = TRUE,
                   nets = questions[[j]]$nets)
      
      
      ## fill in data

      res[row_pos:(row_pos+ncats),
          col_pos:(col_pos+nlev-1)] = t(tab[lev,vlab])
      
      ## update the column position
      col_pos = col_pos + nlev
    }
    
    ## write the file
    fnm = paste(names(questions)[j], "csv", sep = ".")
    write.table(res, paste("banners", fnm, sep = "/"),
              row.names = FALSE, col.names = FALSE,
              sep = ",")
    if (pause) Sys.sleep(1)
  }
  
}

test_diff = function(P1, P2, n1, n2, conf_level) {
  delta = P1 - P2
  
  if (delta < 0) return(FALSE)
  
  sd = sqrt((P1*(1 - P1) / n1) + (P2*(1 - P2) / n2) )
  z = abs(delta)/sd
  
  return(z > qnorm(1-(1-conf_level)/2))
}

get_stats = function(xvar, xlev, fnm, q, qlev = 1,
                     decades, conf_level = .95) {
  
  ##results arrays
  mu_array = ess_array = array(NA, c(length(decades), length(xlev)))
  rownames(mu_array) = rownames(ess_array) = decades
  colnames(mu_array) = colnames(ess_array) = levels(adat[[xvar]])[xlev]
  cat_comp = temp_comp = array('', c(length(decades), length(xlev)))
  dimnames(cat_comp) = dimnames(temp_comp) = dimnames(mu_array)
  
  ##fill arrays
  for (j in 1:length(xlev)) {
    inds = which(adat[[xvar]] == levels(adat[[xvar]])[xlev[j]])
    tb = wxtab2(adat$decade[inds], 
                adat[[q]][inds], 
                adat$weight_all[inds])
    
    mu_array[,j] = tb[decades,qlev]
    ess_array[,j] = tb[decades,'ess']
  }
  
  ## make the category comparisons
  
  for (j in 1:nrow(cat_comp)) {
    for (k in 1:ncol(cat_comp)) {
      p1 = mu_array[j,k]
      n1 = ess_array[j,k]
      test = 1:ncol(cat_comp)
      test = test[-j]
      sig = NULL
      for (i in test) {
        p2 = mu_array[j,i]
        n2 = ess_array[j,i]
        if (test_diff(p1, p2, n1, n2, conf_level)) sig = c(sig, LETTERS[i])
      }
      if (length(sig) > 0) cat_comp[j,k] = 
        paste("(", paste(sig, collapse = ","), ")", sep = "")
    }
  }
  
  ## make decade comparisons
  
  for (j in 1:nrow(temp_comp)) {
    for (k in 1:ncol(temp_comp)) {
      p1 = mu_array[j,k]
      n1 = ess_array[j,k]
      test = 1:nrow(temp_comp)
      test = test[-j]
      sig = NULL
      for (i in test) {
        p2 = mu_array[i,k]
        n2 = ess_array[i,k]
        if (test_diff(p1, p2, n1, n2, conf_level)) sig = c(sig, 
                                                           substr(decades[i], 3,4))
      }
      if (length(sig) > 0) temp_comp[j,k] = 
        paste("[", paste(sig, collapse = ","), "]", sep = "")
    }
  }
  
  ## combine into one table
  
  combined = cbind(
    rbind(c("Share who are ...", rep('', length(xlev)-1)),
          colnames(mu_array),
          round(mu_array*100)), '',
    rbind(c("Comparison across categories ...", rep('', length(xlev)-1)),
          colnames(cat_comp), 
          cat_comp), '',
    rbind(c("Comparison across decades ...", rep('', length(xlev)-1)),
          colnames(temp_comp),
          temp_comp))
  
  write.table(combined, paste("comparisons", fnm, sep = "/"),
              col.names = FALSE, sep = ",")
}
