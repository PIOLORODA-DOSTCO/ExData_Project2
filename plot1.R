#eda
getwd()
setwd("E:/Other Business/RamonPioRoda/eda2/")
getwd()

## Download File
URL = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
destfile = "FNEI_data.zip"
download.file(URL, destfile)


dir()

######
unzip(destfile)
dir()

################ 
## Read the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

######################
# Plot 1
totalPM25ByYear <- tapply(NEI$Emissions, NEI$year, sum)

png("plot1.png")
plot(names(totalPM25ByYear), totalPM25ByYear, type="l",
     xlab="Year", ylab=expression("Total" ~ PM[2.5] ~ "Emissions (tons)"),
     main=expression("Total US" ~ PM[2.5] ~ "Emissions by Year"))
dev.off()