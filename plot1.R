################################################################################
# plot1.R
#
#                 Data Science Specialization via Coursera
#
#                       Exploratory Data Analysis
#                           Course project 1
#
#                       Author: Vincenc Podobnik
#
################################################################################

# A hidden "gray100" watermark is added to the plot using steganography.

library(dplyr)

# setwd( "G:\\cygwin\\home\\vincenck\\edu\\coursera\\datascience\\proj\\ExData_Plotting1" )


# Clear environment
rm( list = ls() )


# Open the datafile and figure out the range of data to load
# Makes loading much, much, faster and is memory friendly.
#
fhnd = file( "household_power_consumption.txt" )
open( fhnd, "r" )
lnum <- 1
readRange <- c( 0, 0 )


while( length( line <- readLines(fhnd, n = 1, warn = FALSE) ) > 0) {

    if( grepl( '^1/2/2007', line ) & readRange[1] == 0 ){
        readRange[1] <- lnum
    }

    if( grepl( '^3/2/2007', line ) & readRange[2] == 0 ){
        readRange[2] <- lnum - 1
        break
    }

    lnum <- lnum + 1

}
close( fhnd )




# Load a sample of the dataset to auto-learn column classes and names
#
sample <- read.csv(
    "household_power_consumption.txt",
    header = TRUE,
    sep = ";",
    na.strings = "?",
    nrows = 10
)

columnClasses <- sapply( sample, class )
columnNames = colnames( sample )
rm( sample )


# Load only the range I really need with predetermined classes and names.
#
data <- read.csv(
    "household_power_consumption.txt",
    header = TRUE,
    sep = ";",
    na.strings = "?",
    colClasses = columnClasses,
    col.names = columnNames,
    skip = readRange[1] - 2,
    nrows = readRange[2] - readRange[1] + 1
)


# Transform date to actual date as opposed to factor
#
data$Date <- as.Date( strptime( data$Date, "%d/%m/%Y" ) )

# Make absolutely sure only the required dates are whats being looked at.
#
data <- filter( data, Date >= "2007-02-01" & Date <= "2007-02-02" )

# str( data )


png( "plot1.png",  width = 480, height = 480, units = "px")


# Plot histogram 1

par( col.sub = "gray100" )
with( data,
      hist( Global_active_power,
            col="red",
            xlab = 'Global Active Power (kilowatts)',
            main = "Global Active Power",
            sub = "by Vincenc Podobnik"
      )
)



dev.off()
