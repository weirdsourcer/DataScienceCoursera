## Calculate the average of value of the polutant of choice
pollutantmean <- function(directory, pollutant, id=1:332, remove = TRUE){
    #retrieve files in the folder as a list
    polfiles <- list.files(directory, full.names = TRUE
                           )[id]
    #select the column you want to work with
    polData <- lapply(polfiles, function(x) read.csv(x)
                      [[pollutant]])
    #merge the data together into a single vector
    polData <- unlist(polData)
    #then...
    mean(polData, na.rm = remove)
} 

#TEST
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)

## finding number of files with complete cases from selected files
complete <- function(directory, id = 1:332){
    polfiles <- list.files(directory, full.names = TRUE
                           )[id]
    nobs <- c()
    for(i in polfiles){
        read <- read.csv(i)
        strip <- complete.cases(read)
        counting <- nrow(subset(read, strip == TRUE))
        nobs <- append(nobs, counting)
    }#paste(nobs, counting)
    data.frame(cbind(id, nobs))
}

#TEST
complete("specdata", 1)
ape <- complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)

#correlation of between pollutants with at least n data points
corr <- function(directory, threshold = 0){
    polfiles <- list.files(directory, full.names = TRUE)
    nobs <- c()
    for(i in polfiles){
        read <- read.csv(i)
        strip <- complete.cases(read)
        strip1 <- subset(read, strip == TRUE)
        
        #calc <- cor(strip1$sulfate, strip1$nitrate)
        #nobs <- append(nobs, calc)
        
        calc <- if(nrow(strip1) > threshold){
            calc <- cor(strip1$sulfate, strip1$nitrate)
            nobs <- append(nobs, calc)
        }
        
    }
    nobs
}

#TEST
cr <- corr("specdata", 150)
head(cr)
summary(cr)
cr <- corr("specdata", 400)
head(cr)
summary(cr)
cr <- corr("specdata", 5000)
summary(cr)
length(cr)
cr <- corr("specdata")
summary(cr)
