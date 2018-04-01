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

############
# Plot 2

BaltimoreCity <- subset(NEI, fips == "24510")

totalPM25ByYear <- tapply(BaltimoreCity$Emissions, BaltimoreCity$year, sum)

png("plot2.png")
plot(names(totalPM25ByYear), totalPM25ByYear, type="l",
     xlab="Year", ylab=expression("Total" ~ PM[2.5] ~ "Emissions (tons)"),
     main=expression("Total Baltimore City" ~ PM[2.5] ~ "Emissions by Year"))
dev.off()