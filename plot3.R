# read_data.R
#
#
# by Vincenc Podobnik
#
#

library(dplyr)


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




png("plot3.png",  width = 480, height = 480, units = "px")

par( col.sub = "gray" )
with( data,
      plot( Sub_metering_1 ~ as.POSIXct( paste( Date, Time, sep = " " ) ),
            type = "l",
            xlab="",
            ylab="Energy sub metering",
            col = "black",
            sub = "by Vincenc Podobnik"
      )
)

with( data,
      points( Sub_metering_2 ~ as.POSIXct( paste( Date, Time, sep = " " ) ),
              col = "red",
              type = "l"
      )
)

with( data,
      points( Sub_metering_3 ~ as.POSIXct( paste( Date, Time, sep = " " ) ),
              col = "blue",
              type = "l"
      )
)

legend( "topright", c( "Sub_metering_1", "Sub_metering_2", "Sub_metering_3" ), col = c( "black", "red", "blue" ), lty = 1 )



dev.off()