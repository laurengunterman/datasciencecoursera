##R PROGRAMMING WEEK 2 PROGRAMMING ASSIGNMENT 1###

###PART 1
pollutantmean<-function(directory, pollutant, id=1:332){
  directory <- paste(getwd(),"/",directory,"/",sep="")
  files.list<-list.files(directory)
  dat<-data.frame()
  for (i in id){
    dat<-rbind(dat, read.csv(paste(directory,files.list[i],sep="")))
  }
  mean(dat[,pollutant], na.rm=TRUE)
}
#Questions
pollutantmean("specdata", "sulfate", 1:10)

pollutantmean("specdata", "nitrate", 70:72)

pollutantmean("specdata", "sulfate", 34)

pollutantmean("specdata", "nitrate")


###PART 2
complete <- function(directory, id = 1:332) {
  count_complete <- function(fname) sum(complete.cases(read.csv(fname)))
  fnames <- list.files(directory, full.names=TRUE)[id]
  data.frame(id = id, nobs = unlist(lapply(fnames, count_complete)))
}

#Questions
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])


##PART 3
corr <- function(directory, threshold=0){
  
  #List of all csv files
  filelist <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  
  #Vector for values to be input into
  cor_vector <- numeric()
  
  #Loop for each file in list
  for (i in 1:length(filelist)) {
    data <- read.csv(filelist[i])
    
    cc <- sum(complete.cases(data))
    
    if (cc > threshold){
      
      dataN <- data[complete.cases(data[c('sulfate', 'nitrate')]),]
      
      cor_vector <- c(cor_vector, cor(dataN$sulfate, dataN$nitrate))
      
    }
    
  }
  
  cor_vector
  
}

#Questions
cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))