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
        1 + length(questions[[j]]$nets) + 2
      
      ## value labels
      tab = wxtab2(dat$cons[inds],
                   dat[[var]][inds],
                   dat[[control$weight]][inds],
                   round = TRUE,
                   nets = questions[[j]]$nets)
      
      vlab = colnames(tab)[questions[[j]]$levels]
      if (!is.null(questions[[j]]$nets)) vlab = c(vlab,
                                                  names(questions[[j]]$nets))
      vlab = c(vlab, 'n', 'ess')
      
      ## add NET indicator
      vlab2 = vlab
      if (!is.null(questions[[j]]$nets)) {
        i = (length(questions[[j]]$levels)+1):
          (length(questions[[j]]$levels)+length(questions[[j]]$nets))
        vlab2[i] = paste("NET:", vlab2[i])
      }
      vlab2[(length(vlab2)-1):length(vlab2)] = c("Total N", 
                                                 "Effective sample size")
      
      ncats = length(questions[[j]]$levels) +
        length(questions[[j]]$nets)+1
      
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
      vlab2 = c(vlab2, 'Total N', 'Effective sample size')
      ncats = length(vlab2)-1
      
      ##add a bit to the header
      header = c(header, paste("Summary table of (",
                 paste(
                   names(attr(dat[[svars[1]]], 'labels')[questions[[j]]$levels]),
                   collapse = ", "), ") responses", sep = ""))
      
      ##Calculate the number of rows needed for a summary table
      nrow = length(header) + 1 + length(svars) + 3
    }
    
    ##container for results
    res = array('', c(nrow, ncol))
    
    ##add the header
    res[1:length(header),1] = header
    
    row_pos = length(header)+3

    res[row_pos:(row_pos+ncats),1] = vlab2
    
    ## counter for column positioning
    col_pos = 2
    
    ## container for data (cbind for each banner point)
    all_output = NULL
    
    ## container for n
    all_n = NULL
    
    ## container for ess
    all_ess = NULL
    
    for (k in 1:length(bannerpoints)) {
      ##var name for the banner
      x = bannerpoints[[k]]$var
      
      bnames = names(tab2(dat[[x]])[bannerpoints[[k]]$levels])
      
      ##number of banner points
      lev = bannerpoints[[k]]$levels
      nlev = length(lev)
      
      ## add column labels
      #res[row_pos-2,col_pos] = names(bannerpoints)[k]
      #res[row_pos-1,col_pos:(col_pos+nlev-1)] = bnames
      
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
        clean.output = round(output[summary_order,]*100)
        raw.output = output[summary_order,]
        
        ##add n and ess to the bottom row
        n = table(dat[[x]][inds])[bannerpoints[[k]]$levels]
        ess = round(wxtab2(dat[[x]][inds], dat$cons[inds], dat$weight[inds])[
                    bannerpoints[[k]]$levels,'ess'
                    ],1)
        if (class(clean.output)[1]=='numeric') clean.output = c(clean.output, n, ess)
        if (class(clean.output)[1]=='matrix') {
          clean.output = rbind(clean.output, n, ess)
        }
        
        ##insert values
        #res[row_pos:(row_pos+ncats),
        #    col_pos:(col_pos+nlev-1)] = clean.output
        
        ## update the column position
        col_pos = col_pos + nlev
        
        ##append results
        all_output = cbind(all_output, raw.output)
        
        ##append n
        all_n = c(all_n, as.numeric(n))
        
        ##append ess
        all_ess = c(all_ess, ess)
        
        next
      }
      
      
      tab = round(wxtab2(dat[[x]][inds],
                   dat[[var]][inds],
                   dat[[control$weight]][inds],
                   round = TRUE,
                   nets = questions[[j]]$nets),1)
      
      
      ## fill in data

      #res[row_pos:(row_pos+ncats),
      #    col_pos:(col_pos+nlev-1)] = t(tab[lev,vlab])
      
      tab = wxtab2(dat[[x]][inds],
                         dat[[var]][inds],
                         dat[[control$weight]][inds],
                         nets = questions[[j]]$nets)
      
      ## update the column position
      col_pos = col_pos + nlev
      
      ## append output
      vlab_o = vlab[1:(length(vlab)-2)]
      if (length(lev) == 1) all_output = cbind(all_output,
                                               as.numeric(t(tab[lev,vlab_o])))
      if (length(lev) > 1) all_output = cbind(all_output, 
                                              t(tab[lev,vlab_o]))
      
      ## append n
      all_n = c(all_n, tab[lev,'n'])
      
      ## append ess
      all_ess = c(all_ess, tab[lev,'ess'])
    }
    
    
    ## Assemble the results
    
    #header (var label, base, dates, question wording, other notes)
    res = c(header[1], rep("", ncol(all_output)))
    for (h in 2:length(header)) {
      res = rbind(res, c(header[h], rep("", ncol(all_output))))
    }
    
    #banner labels
    blabels = ''
    for (b in 1:length(bannerpoints)) {
      str = c(names(bannerpoints)[b], rep('', 
                                          length(bannerpoints[[b]]$levels) - 1))
      blabels = c(blabels, str)
    }
    res = rbind(res, blabels)
    
    #banner column names
    bnames = ''
    for (n in 1:length(bannerpoints)) {
      tab = tab2(dat[[bannerpoints[[n]]$var]])
      nm = names(tab)[bannerpoints[[n]]$levels]
      bnames = c(bnames, nm)
    }
    
    
    #add stat sig guide if necessary
    sig = control$significance_testing
    
    if (!is.null(sig)) {
      for (s in 1:length(sig)) {
        inds = sig[[s]]+1
        bnames[inds] = paste(bnames[inds], " [", LETTERS[1:length(inds)],
                             "]", sep = "")
      }
    }
    
    res = rbind(res, bnames)
    
    #data (rows of results interweaved with significance indicators)
    
    for (o in 1:nrow(all_output)) {
      
      #get the data
      r = round(all_output[o,]*100)
      res = rbind(res, c(vlab2[o],r))
      
      ## skip any missing cases
      mis = which(is.na(all_output[o,]))
      
      if (!is.null(sig)) {
        # container for significance flags
        sig_res = rep('', ncol(res))
        
        for (s in 1:length(sig)) {
          
          comp = sig[[s]]
          for (c in comp) {
            if (is.element(c, mis)) next
            
            P1 = all_output[o,c]
            n1 = all_ess[c]

            comp.star = 1:length(comp)
            comp.star = comp.star[-which(comp == c)]
            comps = NULL
            for (c2 in comp.star) {
              if (is.element(comp[c2], mis)) next 
              P2 = all_output[o,comp[c2]]
              n2 = all_ess[comp[c2]]
              if (test_diff(P1, P2, n1, n2, conf_level)) comps =
                c(comps, LETTERS[c2])
            }
            if (length(comps)>0) sig_res[c+1] = paste("(",
                      paste(comps, collapse = ","), ")", sep = "")
          }
          
        }
        res = rbind(res, sig_res)
      }
    }
    
    #footer (n, ess)
    res = rbind(res, c("Total N:", all_n),
                c("Effective sample size:", all_ess))
    
    ## write the file
    fnm = paste(names(questions)[j], "csv", sep = ".")
    write.table(res, paste("banners", fnm, sep = "/"),
              row.names = FALSE, col.names = FALSE,
              sep = ",")
    if (pause) Sys.sleep(.5)
  }
  
}

test_diff = function(P1, P2, n1, n2, conf_level) {
  delta = P1 - P2
  if (is.na(delta)) return(NA)
  if (delta == 0) return(FALSE)
  
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
