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
# Plot 5
BaltimoreCityMV <- subset(NEI, fips == "24510" & type=="ON-ROAD")

BaltimoreMVPM25ByYear <- ddply(BaltimoreCityMV, .(year), function(x) sum(x$Emissions))
colnames(BaltimoreMVPM25ByYear)[2] <- "Emissions"

png("plot5.png")
qplot(year, Emissions, data=BaltimoreMVPM25ByYear, geom="line") +
  ggtitle(expression("Baltimore City" ~ PM[2.5] ~ "Motor Vehicle Emissions by Year")) +
  xlab("Year") +
  ylab(expression("Total" ~ PM[2.5] ~ "Emissions (tons)"))
dev.off()