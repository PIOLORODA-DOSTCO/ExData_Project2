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
# Plot 4
CoalCombustionSCC <- subset(SCC, EI.Sector %in% c("Fuel Comb - Comm/Institutional - Coal",
                                                  "Fuel Comb - Electric Generation - Coal",
                                                  "Fuel Comb - Industrial Boilers, ICEs - Coal"))

CoalCombustionSCC1 <- subset(SCC, grepl("Comb", Short.Name) & grepl("Coal", Short.Name))

nrow(CoalCombustionSCC)
nrow(CoalCombustionSCC1)

CoalCombustionSCCCodes <- union(CoalCombustionSCC$SCC, CoalCombustionSCC1$SCC)
length(CoalCombustionSCCCodes)

CoalCombustion <- subset(NEI, SCC %in% CoalCombustionSCCCodes)

coalCombustionPM25ByYear <- ddply(CoalCombustion, .(year, type), function(x) sum(x$Emissions))
colnames(coalCombustionPM25ByYear)[3] <- "Emissions"

png("plot4.png")
qplot(year, Emissions, data=coalCombustionPM25ByYear, color=type, geom="line") +
  stat_summary(fun.y = "sum", fun.ymin = "sum", fun.ymax = "sum", 
               color = "black", aes(shape="total"), geom="line") +
  geom_line(aes(size="total", shape = NA)) +
  ggtitle(expression("Coal Combustion" ~ PM[2.5] ~ "Emissions by Source Type and Year")) +
  xlab("Year") +
  ylab(expression("Total" ~ PM[2.5] ~ "Emissions (tons)"))
dev.off()