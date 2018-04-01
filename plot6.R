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
# Plot 6

MV <- subset(NEI, (fips == "24510" | fips == "06037") & type=="ON-ROAD")

MV <- transform(MV,
                region = ifelse(fips == "24510", "Baltimore City", "Los Angeles County"))

MVPM25ByYearAndRegion <- ddply(MV, .(year, region), function(x) sum(x$Emissions))
colnames(MVPM25ByYearAndRegion)[3] <- "Emissions"


png("plot6a.png")
qplot(year, Emissions, data=MVPM25ByYearAndRegion, geom="line", color=region) +
  ggtitle(expression("Baltimore City and Los Angeles County" ~ PM[2.5] ~ "Motor Vehicle Emissions by Year")) +
  xlab("Year") +
  ylab(expression("Total" ~ PM[2.5] ~ "Emissions (tons)"))
dev.off()


Balt1999Emissions <- subset(MVPM25ByYearAndRegion,
                            year == 1999 & region == "Baltimore City")$Emissions
LAC1999Emissions <- subset(MVPM25ByYearAndRegion,
                           year == 1999 & region == "Los Angeles County")$Emissions
MVPM25ByYearAndRegionNorm <- transform(MVPM25ByYearAndRegion,
                                       EmissionsNorm = ifelse(region == "Baltimore City",
                                                              Emissions / Balt1999Emissions,
                                                              Emissions / LAC1999Emissions))

png("plot6.png", width=600)
qplot(year, EmissionsNorm, data=MVPM25ByYearAndRegionNorm, geom="line", color=region) +
  ggtitle(expression("Total" ~ PM[2.5] ~
                       "Motor Vehicle Emissions Normalized to 1999 Levels")) +
  xlab("Year") +
  ylab(expression("Normalized" ~ PM[2.5] ~ "Emissions"))
dev.off()