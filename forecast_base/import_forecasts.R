require(stringr)

import.forecasts <- function(this.dir) {
  file.names <- list.files(this.dir, recursive=T)
  if (length(file.names) == 0) stop("No files found; check directory.")
  these.files <- list()
  for (this.file.name in file.names) {
	  if (str_detect(this.file.name, "\\.csv")) {
		  this.file <- process.file(this.dir, this.file.name)
		  these.files[[this.file$name]][[this.file$target]] <- this.file$data
	  }
  }
  return(these.files)
}

process.file <- function(this.dir, this.file.name) {
	
  # import file
	this.file <- read.csv(paste(this.dir, this.file.name, sep="/"))
	names(this.file) <- str_replace(names(this.file), "X", "")
	
	# remove any empty rows
	this.file <- this.file[as.character(this.file[ , 1]) != "", ]
	rownames(this.file) <- this.file[ , 1]
	this.file <- this.file[ , -1]
	
	# remove folder from file name if necessary
	if (str_detect(this.file.name, '\\/')) {
	  this.file.name <- str_replace(this.file.name, '.*\\/', '')
	}
	
#	# remove any empty columns
#	this.file <- this.file[ , str_detect(names(this.file), "X20")]
	
	# get team name or "template" label
	this.name <- tolower(str_extract(this.file.name, "[^\\_]*"))
	
	# get target name
	this.target <- tolower(str_replace_all(this.file.name, "_", "."))
	this.target <- paste(str_split(this.target, "\\.")[[1]][2:3], collapse='.')
	
	return(list(
			name=this.name,
			target=this.target,
			data=this.file))
}

import.templates <- function(this.dir) {
  file.names <- list.files(template.dir, recursive=T)
  if (length(file.names) == 0) stop("No files found; check directory.")
  these.files <- list()
  for (this.file.name in file.names) {
	  if (str_detect(this.file.name, "\\.csv")) {
		  this.file <- process.file(this.dir, this.file.name)
		  these.files[[this.file$name]][[this.file$target]] <- this.file$data
	  }
  }
  return(these.files)
}


file.names <- list.files(submission.dir,recursive=T)
this.file.name <- file.names[1]
this.file <- read.csv(paste(submission.dir, this.file.name, sep="/"))

file.names2 <- list.files(template.dir,recursive=T)
this.file.name2 <- file.names2[1]
this.file2 <- read.csv(paste(template.dir, this.file.name2, sep="/"))
