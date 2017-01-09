library(data.table)
library(datasets) 

# constants definition
workingDirectory        = "/Exploratory Data Analysis"
originalDatasetURL      = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
originalDatasetFile     = basename(originalDatasetURL)
extractDirectory        = "/extract"

GetOriginalDataset <- function() {
  # Downloads original dataset from rempte resource into the specially created   
  # sub-directory of current working directory and uzips all the files
  
  # create directory for this exercise 
  targetDirectory <- paste0(getwd(), workingDirectory)
  if (!file.exists(targetDirectory)) {
    dir.create(targetDirectory)
  }
  
  # download original dataset from remote resource
  datasetLocalFile <- paste0(targetDirectory, "/", originalDatasetFile)
  if (!file.exists(datasetLocalFile)) {
    download.file(url      = originalDatasetURL, 
                  destfile = datasetLocalFile, 
                  method   = "libcurl")
  }
  
  # extract downloaded dataset
  unzip (zipfile = datasetLocalFile, exdir = paste0(getwd(), 
                                                    workingDirectory, 
                                                    extractDirectory))
}

ReadObservationsFile <- function () {
  # Reads observation file using first row as a header.
  # Only two dates are taken into consideration - Feb 1, 2007 and Feb 2, 2007. 
  # Observations with at least one missed measurment are filtered out  
  #    
  # Returns: data table as per description above   

  # compose sorce file names
  observationDirectory <- paste0(getwd(), workingDirectory, "/", 
                                 extractDirectory, "/")
  observationsFile <- paste0(observationDirectory, 
                             "household_power_consumption.txt")
  # read file lines and filter out non-required, preserving first row
  fileLines <- readLines(observationsFile)
  fileLines <- c(fileLines[1], 
                 fileLines[grep("(^1/2/2007;)|(^2/2/2007;)", 
                                fileLines, 
                                value = FALSE)])
  # check for records with missed measurments (if any) to exclude them
  rowsToExclude <- c(grep("[?]", fileLines, value = FALSE))
  if (length(rowsToExclude) != 0) {
    fileLines <- fileLines[-c(rowsToExclude)]
  }
  # read selected obseravtion data
  fileData <- read.table(text       = fileLines[],
                         sep        = ";",
                         header     = TRUE,
                         colClasses = c("character", "character", "numeric",
                                        "numeric", "numeric", "numeric", 
                                        "numeric"))
  fileData$Date <- as.Date(x = fileData$Date, format = "%d/%m/%Y")
  #airquality <- transform(airquality, Month = factor(Month)) 
  return(fileData)
}

ProducePlot1 <- function() {
  # Generates output png file into current working directory  
  # according to the requiremetns stated in the assignment for plot 1
  
  # ensure remote data set is downloaded and extracted
  GetOriginalDataset()
  # read observation file so that data becomes available 
  fileData <- ReadObservationsFile()
  # set output png file parameters
  png(filename = "plot1.png", 
      bg       = "white", 
      width    = 480,
      height   = 480)
  # generate required plot
  hist(x    = fileData$Global_active_power, 
       col  = "red", 
       main = "Global active power", 
       xlab = "Global active power (kilowatts)")
  # close output png file and release graphical device (context)
  dev.off()
}
