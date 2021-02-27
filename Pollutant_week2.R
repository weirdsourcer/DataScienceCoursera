pollutantmean <- function(directory, pollutant, id=1:332){
    #retrieve files in the folder as a list
    polfiles <- list.files(directory, full.names = TRUE
                           )[id]
    #select the column you want to work with
    polData <- lapply(polfiles, function(x) read.csv(x)
                      [[pollutant]])
    #merge the data together into a single vector
    polData <- unlist(polData)
    #then...
    mean(polData, na.rm = TRUE)
} 

#TEST
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)


complete <- function(directory, id = 1:332){
    polfiles <- list.files(directory, full.names = TRUE
                           )[id]
    nobs <- c()
    for(i in polfiles){
        reading <- read.csv(i)
        stripping <- complete.cases(reading)
        counting <- nrow(subset(reading, stripping == TRUE))
        nobs <- append(nobs, counting)
    }#paste(nobs, counting)
    data.frame(cbind(id, nobs))
}

#TEST
complete("specdata", 1)
ape <- complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)

corr <- function(directory, threshold = 0){
    polfiles <- list.files(directory, full.names = TRUE)
    nobs <- c()
    for(i in polfiles){
        reading <- read.csv(i)
        stripping <- complete.cases(reading)
        stripping1 <- subset(reading, stripping == TRUE)
        
        #calc <- cor(stripping1$sulfate, stripping1$nitrate)
        #nobs <- append(nobs, calc)
        
        calc <- if(nrow(stripping1) > threshold){
            calc <- cor(stripping1$sulfate, stripping1$nitrate)
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
