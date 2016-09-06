require(stringr)

verify.subs <- function(submissions, templates) {
  for (this.team in names(submissions)) {
    for (this.target in names(submissions[[this.team]])) {
    	### load and verify forecast
    	writeLines(paste0(this.team, " forecast for ", this.target))
    	submissions[[this.team]][[this.target]] <- 
          verify.sub(submissions[[this.team]][[this.target]], 
          templates[[str_split(this.target, '\\.')[[1]][1]]])
    }
  }
  return(submissions)
}

verify.sub <- function(sub, templ) {
	if (is.null(templ)) stop("Template is null.")
	
	### match row and column names
	if (any(row.mismatch <- rownames(sub) != rownames(templ))) {
		stop(paste0("Row names don't match: ", 
				paste(which(row.mismatch), collapse=", ")))
	}
	if (any(col.mismatch <- colnames(sub) != colnames(templ))) {
		stop(paste0("Column names don't match: ", 
				paste(which(col.mismatch), collapse=", ")))
	}
	
	select.point <- str_detect(tolower(rownames(sub)), "point")
	select.probs <- !select.point
	
	### check for negative predictions
	# if a point prediction is negative, leave and report
	# if a probability prediction is negative, convert to NA
	if (any(neg.pp <- sub[select.point, ] < 0, na.rm=T)) {
		warning(paste0("!!!Column(s) with a negative point prediction: ", 
				paste(which(neg.pp), collapse=", ")), call.=F, immediate.=T)
	}
	if (any(neg.prob <- apply(sub[select.probs, ] < 0, 2, any, na.rm=T))) {
		sub[select.probs, ][sub[select.probs, ] < 0] <- NA
		warning(paste0("!!!Column(s) with a negative probability prediction: ", 
				paste(which(neg.prob), collapse=", ")), call.=F, immediate.=T)
	}
	
	### check for NAs in the point predictions
	# if a point prediction is NA, leave as NA and report
	if (any(no.pp <- is.na(sub[select.point, ]))) {
		warning(paste0("Column(s) with no point prediction: ", 
				paste(which(no.pp), collapse=", ")), call.=F, immediate.=T)
	}		

	### check for NAs in the probabilities
	# if a column is NA, leave as NA and report
	# if only some entries are NAs, convert them to zeros
	if (any(na.in.probs <- apply(is.na(sub[select.probs, ]), 2, any))) {
		empty.probs <- apply(is.na(sub[select.probs, ]), 2, all)
		if (any(empty.probs)) {
			warning(paste0("Column(s) with no probability predictions: ", 
					paste(which(empty.probs), collapse=", ")), call.=F, immediate.=T)
		}
		if (any(na.in.probs & !empty.probs)) {
			sum.na.in.probs <- sum(is.na(sub[select.probs, !empty.probs]))
			sub[select.probs, !empty.probs][is.na(sub[select.probs, !empty.probs])] <- 0
			writeLines(paste0("NAs replaced with zeros: ", 
					paste(sum.na.in.probs, collapse=", ")))
		}
	}
	
	### check probability column sums
	# if 1.0 < sum(probs) < 1.1 or 0.9 < sum(probs) < 1.0,
	# divide by sum to normalize to 1.0
	# if the sum is less than 0.9 or greater than 1.1, convert all to NAs and report
	col.sums <- apply(sub[select.probs, ], 2, sum)
	if (any(col.sums != 1.0, na.rm=T)) {
		if (any(adj.col <- which((1.0 < col.sums & col.sums < 1.1) | (0.9 < col.sums & col.sums < 1.0)))) {
			for (i in adj.col) sub[select.probs, i] <- sub[select.probs, i]/col.sums[i]
			writeLines(paste(length(adj.col), "column(s) with probability predictions normalized to 1.0."))
		}
		if (any(probs.too.low <- which(col.sums <= 0.9))) {
			sub[select.probs, probs.too.low] <- NA		
			warning(paste("Column(s) with probability predictions summing to <0.9: ", 
					paste(probs.too.low, collapse=", ")), call.=F, immediate.=T)
		}
		if (any(probs.too.high <- which(1.1 <= col.sums))) {
			sub[select.probs, probs.too.high] <- NA		
			warning(paste("Column(s) with probability predictions summing to 1.1+: ", 
					paste(probs.too.high, collapse=", ")), call.=F, immediate.=T)
		}
	}
	
	writeLines("Verification complete.\n")
	return(sub)
}

