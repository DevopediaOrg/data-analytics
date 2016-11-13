library(data.table)
library(ggplot2)

readSearchDataFile <- function() {
    d <- fread("data/rio2016/searchInterest.Google.csv", 
               sep = ",", header = FALSE, stringsAsFactors = TRUE, skip = 2,
               colClasses = c("character", "integer", "character"),
               col.names = c("Country", "Interest", "Sport"))
}

readHdiDataFile <- function() {
    # Doesn't give correct classes for the columns
    #hdiInfo <- fread("data/HDI.csv", sep=",", header = TRUE, skip = 1, nrows=0) # to get col names and types
    #hdi <- fread("data/HDI.csv", 
    #           sep = ",", header = TRUE, stringsAsFactors = FALSE, skip = 1,
    #           colClasses = c("integer", "factor", rep("numeric", ncol(hdiInfo)-2)),
    #           col.names = colnames(hdiInfo))

    hdi <- fread("data/HDI.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE, skip = 1)

    hdi$Country <- trimws(hdi$Country)
    hdi$'2014' <- as.numeric(hdi$'2014')
    hdi <- hdi[,.(Country,HDI.2014=hdi$'2014')]

    # Convert to be consistent with other data sets
    hdi$Country[which(hdi$Country=='United Kingdom')] <- "Great Britain"
    hdi$Country <- factor(hdi$Country)

    return(hdi)
}

readMedalsDataFile <- function() {
    m <- fread("data/rio2016/medalsData.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE)
    m[, ts := as.POSIXct(as.character(ts))] # TODO Preserve the time component
    return(m)
}

commonTheme <- function() {
    theme(
        plot.title=element_text(size=12),
        legend.title = element_blank(),
        legend.position = c(.95,.95),
        plot.margin = unit(c(.1,.4,.1,.1), "cm"), # top,right,bottom,left
        panel.grid.major.y = element_line(colour = "#eeeeee"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
}

sportSearchByCountry <- function(fname, countries) {
    d <- readSearchDataFile()
    all <- d[d$C %in% countries,] # ignores items in input not in d$Country
    all$Country <- factor(all$Country)
    all$Sport <- factor(all$Sport)
    all <- data.table(xtabs(Interest ~ Country+Sport, all))
    colnames(all) <- c("Country", "Sport", "Interest")
    all$Country <- factor(all$Country)

    ggp <-
        ggplot(data=all, aes(x=Sport,y=Interest,fill=factor(Country))) +
        geom_bar(position="dodge", stat="identity", width=0.5) +
        ggtitle("Search Interest for Olympic Sport for Period 6-Aug-2016 to 21-Aug-2016\nData Source: Google Trends") +
        coord_flip() +
        commonTheme()

    # We emphasize India if present
    if ('India' %in% all$Country) {
        indiaPos <- which(levels(all$C) == 'India') # levels are in sorted order
        colors <- gray.colors(length(levels(all$C))-1, start=0.6)
        colors <- c(colors[1:indiaPos-1], "orange", colors[indiaPos:length(colors)])
        ggp <- ggp + scale_fill_manual(values=colors)
    }
    
    ggsave(fname, width=10, height=5, units="in", dpi=150)
}

indianSearchCompared <- function() {
    d <- readSearchDataFile()
    
    india <- d[Country=="India"]
    indianSports <- india$Sport
    india$Code <- "INDIA"

    others <- d[Sport %in% indianSports][order(Sport,-Interest,Country)]
    topothers <- others[!duplicated(others$Sport), ]
    topothers$Code <- "TOPPERS"
    all <- rbind(india, topothers)
    
    ggplot(data=all, aes(x=Sport, y=Interest, fill=factor(Code))) +
        geom_bar(position="dodge", stat="identity", width=0.7) +
        ggtitle("Indian Search Interest for Olympic Sport for Period 6-Aug-2016 to 21-Aug-2016\nData Source: Google Trends") +
        coord_flip() +
        commonTheme() +
        scale_fill_manual(values=c("orange","#eeeeee")) +
        scale_y_continuous(expand = c(0, 0)) +
        scale_x_discrete(labels=sub(" ", "\n", indianSports)) +
        geom_text(aes(x=Sport, y=Interest, label=Country, hjust=1, vjust=-0.5, label.size=0.2), color = "#999999", data = all[all$Country!="India",])

    ggsave("plots/indianSearchCompared.png", width=13, height=5, units="in", dpi=150)
}

searchBasicStats <- function() {
    d <- readSearchDataFile()
    
    # Show everything!
    qplot(Country, Sport, data=d, size=Interest, colour=I("red")) +
        theme(axis.text.x = element_text(angle = 90, size=5, hjust=1, vjust=0))
    ggsave("plots/fullSearchInterest.png", width=15, height=18, units="in", dpi=150)

    # Code 1: Countries searching more than a dozen sports
    #mostSports <- table(d$Country)
    #mostSports <- data.table(mostSports[mostSports>12])
    #colnames(mostSports) <- c("Country", "NumOfSports")
    #mostSportsAll <- d[Country %in% mostSports$Country]
    
    # Code 2: Countries searching more than a dozen sports
    mostSportsAll <- d[, .(Interest,Sport,NumOfSports=.N), by=Country][NumOfSports>12]
    mostSportsAll <- mostSportsAll[, .(Sport,NumOfSports,InterestCount=.N), by=.(Country,Interest)]
    mostSportsAll$Country <- factor(mostSportsAll$Country)
    mostSportsAll$Sport <- factor(mostSportsAll$Sport)
    
    ggplot(data=mostSportsAll, aes(x=Country,y=NumOfSports)) +
        geom_bar(position="dodge", stat="identity", fill="steelblue1", width=0.6) +
        ggtitle("Countries Searching More Than a Dozen Sports\nData Source: Google Trends") +
        coord_flip() +
        commonTheme() +
        geom_hline(yintercept = 12, linetype = "longdash")
    
    ggsave("plots/mostSports.Bar.png", width=10, height=5, units="in", dpi=150)

    # TODO Add title to legend and show subset of integers
    ggplot(data=mostSportsAll, aes(x=Country,y=Interest)) +
        #geom_point(aes(size = InterestCount)) + 
        # :factor(InterestCount) can be used to force legend to integers
        # :can simply use geom_count() instead
        geom_count(color='indianred3') +
        ggtitle("Interest Within Countries Searching More Than a Dozen Sports\nData Source: Google Trends") +
        coord_flip() +
        commonTheme()

    ggsave("plots/mostSports.Points.png", width=10, height=5, units="in", dpi=150)
    
    # Sports with most country followers
    mostFollowers <- d[, .(Country,Interest,NumberOfCountries=.N), by=Sport][NumberOfCountries>50]
    ggplot(data=mostFollowers, aes(x=Sport,y=NumberOfCountries)) +
        geom_bar(position="dodge", stat="identity", fill="steelblue1", width=0.6) +
        ggtitle("Sports With More Than 50 Country Followers\nData Source: Google Trends") +
        coord_flip() +
        commonTheme() +
        geom_hline(yintercept = 50, linetype = "longdash")
    
    ggsave("plots/mostCountryFollowers.png", width=10, height=4, units="in", dpi=150)

    # Show a boxplot for sports with most followers
    # TODO Add country name to farthest outlier: d[order(Sport,-Interest)][, head(.SD,1), by=Sport]
    # :geom_text(aes(label = ??), na.rm = TRUE, hjust = -0.1)
    bigFollower <- d[order(Sport,-Interest)][, head(.SD,1), by=Sport][,.(Sport,BigFollower=Country)]
    mostFollowers <- merge(mostFollowers, bigFollower, by=c("Sport"), all.x=T)
    big <- mostFollowers[Country==BigFollower]
    nonbig <- mostFollowers[Country!=BigFollower]
    nonbig$BigFollower <- as.numeric(rep(NA,nrow(nonbig)))
    mostFollowers <- rbind(big, nonbig)
    ggplot(mostFollowers, aes(Sport, Interest)) + 
        geom_boxplot(fill='orange', color="red") +
        ggtitle("Statistical Spread of Interest for Sports With More Than 50 Country Followers\nData Source: Google Trends") +
        commonTheme() +
        geom_text(aes(label = BigFollower), na.rm = TRUE, hjust = -0.1)
    
    ggsave("plots/mostCountryFollowers.stats.png", width=10, height=6, units="in", dpi=150)
    
    # Sports with most total interest
    mostTotalInterest <- d[, .(Interest,NumberOfCountries=.N,TotalInterest=sum(Interest)), by=Sport][, head(.SD, 1), by=Sport][TotalInterest>150]
    ggplot(data=mostTotalInterest, aes(x=Sport,y=TotalInterest)) +
        geom_bar(position="dodge", stat="identity", fill="steelblue1", width=0.6) +
        ggtitle("Sports With Most Total Interest (>150)\nData Source: Google Trends") +
        coord_flip() +
        commonTheme() +
        geom_hline(yintercept = 150, linetype = "longdash")
    
    ggsave("plots/mostTotalInterest.png", width=10, height=4, units="in", dpi=150)
    
    # Countries showing highest interest for particular sport
    mostTotalInterest <- d[, .(Interest,NumberOfCountries=.N,TotalInterest=sum(Interest)), by=Sport][, head(.SD, 1), by=Sport]
    mostInterest <- d[d[, .I[Interest==max(Interest)], by=Sport]$V1]
    mi <- mostInterest[, lapply(.SD, paste0, collapse=", "), by=Sport]
    mostInterest <- mostInterest[, head(.SD, 1), by=Sport]
    mostInterest$Country <- mi$Country
    mostInterest$TotalInterest <- mostTotalInterest$TotalInterest
    mostInterest$NumberOfCountries <- mostTotalInterest$NumberOfCountries
    ggplot(data=mostInterest, aes(x=Sport,y=Interest)) +
        geom_bar(position="dodge", stat="identity", fill="steelblue1", width=0.5) +
        ggtitle("Country Showing Most Interest in a Sport\nData Source: Google Trends") +
        coord_flip() +
        commonTheme() +
        geom_text(aes(x=Sport, y=Interest, label=Country, hjust=-0.05, vjust=0.2, label.size=0.05), color = "#999999", data = mostInterest)
    
    ggsave("plots/mostInterest.png", width=15, height=7, units="in", dpi=150)

    # No. of Countries vs Total Interest showing the toppers by sport
    png(filename="plots/mostInterestCircles1.png", width=12, height=6, units="in", res=150)
    symbols(mostInterest$TotalInterest, mostInterest$NumberOfCountries, circles=sqrt(mostInterest$Interest/pi), inches=1, xlab="Total Interest", ylab="No. of Countries")
    title("Countries Showing Most Interest By Sport\nData Source: Google Trends")
    text(mostInterest$TotalInterest, mostInterest$NumberOfCountries, paste(mostInterest$Sport, mostInterest$Country, sep="\n"), cex=0.8)
    dev.off()
    
    # Sport vs No. of Countries showing the toppers and their interest by sport
    ggplot(mostInterest, aes(x=NumberOfCountries, y=Sport)) +
        geom_point(data=mostInterest,aes(x=NumberOfCountries, y=Sport, size=TotalInterest, colour=Interest), alpha=.5) +
        ggtitle("Countries Showing Interest By Sport\nSize: Total Interest; Colour: Topper Interest\nData Source: Google Trends") +
        commonTheme() +
        geom_text(data=mostInterest, aes(x=NumberOfCountries, y=Sport, label=Country), color="#222222") +
        scale_colour_gradientn(colours=c('orange','red')) +
        scale_size(range=c(1,70), guide=F)

    ggsave("plots/mostInterestCircles2.png", width=12, height=8, units="in", dpi=150)
}

searchHdiCorr <- function() {
    d <- readSearchDataFile()
    hdi <- readHdiDataFile()

    # Scatterplot comparing total interest and HDI
    countryInterest <- d[, .(NumOfSports=.N,TotalInterest=sum(Interest)), by=Country]
    m <- merge(countryInterest, hdi, by.x="Country", by.y="Country", all=F) # ignore countries without HDI info
    ggplot(m, aes(x=HDI.2014, y=TotalInterest)) +
        geom_point(shape=3, size=2) +
        ggtitle("Countrywise Total Interest vs HDI\nData Source: Google Trends") +
        commonTheme() +
        theme(panel.grid.major.x = element_line(colour = "#eeeeee"))
    
    ggsave("plots/interestHdi.png", width=12, height=8, units="in", dpi=150)
    
    # Scatterplot comparing number of sports and HDI
    ggplot(m, aes(x=HDI.2014, y=NumOfSports)) +
        geom_point(shape=3, size=2) +
        geom_smooth(method=lm, se=T) +
        ggtitle("Countrywise Number of Sports Searched vs HDI\nData Source: Google Trends") +
        commonTheme() +
        theme(panel.grid.major.x = element_line(colour = "#eeeeee"))
    
    ggsave("plots/numOfSportsHdi.png", width=12, height=8, units="in", dpi=150)
}

medalsBasicStats <- function() {
    m <- readMedalsDataFile()
    hdi <- readHdiDataFile()
    
    numCountriesWithMedals <- length(levels(m$countrycode))
    numSports <- length(levels(m$sport))
    numEvents <- length(levels(m$event))
    numIndividualMedallists <- length(data.table(table(m$medalist))[!V1 %like% 'team']$V1)

    # Scatterplot total medals by country and HDI
    totalMedals <- m[,.(NumOfMedals=.N), by=country]
    tmhdi <- merge(totalMedals, hdi, by.x="country", by.y="Country", all=F) # ignore countries without HDI info
    ggplot(tmhdi, aes(x=HDI.2014, y=NumOfMedals)) +
        geom_point(shape=3, size=2) +
        geom_smooth(method=lm, se=T) +
        ggtitle("Countrywise Total Medals vs HDI") +
        commonTheme() +
        theme(panel.grid.major.x = element_line(colour = "#eeeeee")) +
        geom_text(data=tmhdi[tmhdi$country %in% c("United States","China","Kenya","Great Britain","Norway")], # outliers seen during exploration
                  aes(x=HDI.2014, y=NumOfMedals, label=country),
                  hjust=0.5, vjust=-0.7, color="darkgreen", size=3) +
        geom_text(data=tmhdi[tmhdi$country=="India",],
                  aes(x=HDI.2014, y=NumOfMedals, label=country),
                  hjust=1.2, vjust=0.3, color="red", size=3)
    
    ggsave("plots/totalMedalsHdi.png", width=8, height=5, units="in", dpi=150)
    
    # Top individual medalists
    toppers <- data.table(table(paste0(m$medalist, "\n[", m$country, "]")))[N>2 & !V1 %like% 'team'][order(-N,V1)]
    colnames(toppers) <- c("Medalist","NumOfMedals")
    toppers$Medalist <- factor(toppers$Medalist, levels=rev(toppers$Medalist)) # so that ggplot doesn't order the factors
    ggplot(data=toppers, aes(x=Medalist,y=NumOfMedals)) +
        geom_bar(position="dodge", stat="identity", fill="steelblue1", width=0.6) +
        ggtitle("Top Individual Medalists") +
        coord_flip() +
        commonTheme()
    ggsave("plots/topIndividualMedalists.png", width=10, height=5, units="in", dpi=150)
}

runAll <- function() {
    dir.create('plots', showWarnings=F)
    sportSearchByCountry("plots/sportSearchIndia.png", "India")
    sportSearchByCountry("plots/sportSearchByCountry.png", c("India", "United States", "Great Britain", "China"))
    indianSearchCompared()
    searchBasicStats()
    searchHdiCorr()
    medalsBasicStats()
}
