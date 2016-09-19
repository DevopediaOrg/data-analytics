library(data.table)
library(ggplot2)

readSearchDataFile <- function() {
    d <- fread("data/rio2016/searchInterest.Google.csv", 
               sep = ",", header = FALSE, stringsAsFactors = TRUE, skip = 2,
               colClasses = c("character", "integer", "character"),
               col.names = c("Country", "Interest", "Sport"))
}

readMedalsDataFile <- function() {
    m <- fread("data/rio2016/medalsData.csv", 
               sep = ",", header = TRUE, stringsAsFactors = TRUE)
}

commonTheme <- function() {
    theme(
        plot.title=element_text(size=12),
        legend.title = element_blank(),
        legend.position = c(.95,.95),
        plot.margin = unit(c(.1,.4,.1,.1), "cm"), # top,right,bottom,left
        panel.grid.major.y = element_line(colour = "lightgray"),
        panel.grid.minor.y = element_line(colour = "lightgray"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
}

sportSearchByCountry <- function(countries) {
    d <- readSearchDataFile()
    all <- d[d$C %in% countries,]
    all$Country <- factor(all$Country)
    all$Sport <- factor(all$Sport)
    all <- data.table(xtabs(Interest ~ Country+Sport, all))
    colnames(all) <- c("Country", "Sport", "Interest")
    
    ggplot(data=all, aes(x=Sport,y=Interest,fill=factor(Country))) +
        geom_bar(position="dodge",stat="identity") +
        ggtitle("Search Interest for Olympic Sport for Period 6-Aug-2016 to 21-Aug-2016\nData Source: Google Trends") +
        coord_flip() +
        commonTheme()
        
    ggsave("sportSearchByCountry.png", width=10, height=5, units="in", dpi=150)
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
        geom_bar(position="dodge", stat="identity") +
        ggtitle("Indian Search Interest for Olympic Sport for Period 6-Aug-2016 to 21-Aug-2016\nData Source: Google Trends") +
        coord_flip() +
        commonTheme() +
        scale_fill_manual(values=c("orange","#eeeeee")) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        scale_x_discrete(labels=sub(" ", "\n", indianSports)) +
        geom_text(aes(x=Sport, y=Interest, label=Country, hjust=1, vjust=-0.5), color = "#aaaaaa", data = all[all$Country!="India",])

    ggsave("indianSearchCompared.png", width=13, height=5, units="in", dpi=150)
}

searchBasicStats <- function() {
    d <- readSearchDataFile()
    
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
        geom_bar(position="dodge", stat="identity", fill="lightblue") +
        ggtitle("Countries Searching More Than a Dozen Sports\nData Source: Google Trends") +
        coord_flip() +
        commonTheme() +
        geom_hline(yintercept = 12, linetype = "longdash")
    
    ggsave("mostSports.Bar.png", width=10, height=5, units="in", dpi=150)

    # TODO Add title to legend and show subset of integers
    ggplot(data=mostSportsAll, aes(x=Country,y=Interest)) +
        #geom_point(aes(size = InterestCount)) + 
        # :factor(InterestCount) can be used to force legend to integers
        # :can simply use geom_count() instead
        geom_count() +
        ggtitle("Interest Within Countries Searching More Than a Dozen Sports\nData Source: Google Trends") +
        coord_flip() +
        commonTheme()

    ggsave("mostSports.Points.png", width=10, height=5, units="in", dpi=150)
    
    # Sports with most country followers
    mostFollowers <- d[, .(Country,Interest,NumberOfCountries=.N), by=Sport][NumberOfCountries>50]
    ggplot(data=mostFollowers, aes(x=Sport,y=NumberOfCountries)) +
        geom_bar(position="dodge", stat="identity", fill="lightblue") +
        ggtitle("Sports With More Than 50 Country Followers\nData Source: Google Trends") +
        coord_flip() +
        commonTheme() +
        geom_hline(yintercept = 50, linetype = "longdash")
    
    ggsave("mostCountryFollowers.png", width=10, height=4, units="in", dpi=150)

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
        geom_boxplot() +
        ggtitle("Statistical Spread of Interest for Sports With More Than 50 Country Followers\nData Source: Google Trends") +
        commonTheme() +
        geom_text(aes(label = BigFollower), na.rm = TRUE, hjust = -0.1)
    
    ggsave("mostCountryFollowers.stats.png", width=10, height=6, units="in", dpi=150)
    
    # Sports with most total interest
    mostTotalInterest <- d[, .(Interest,NumberOfCountries=.N,TotalInterest=sum(Interest)), by=Sport][TotalInterest>150]
    ggplot(data=mostTotalInterest, aes(x=Sport,y=TotalInterest)) +
        geom_bar(position="dodge", stat="identity", fill="lightblue") +
        ggtitle("Sports With Most Total Interest (>150)\nData Source: Google Trends") +
        coord_flip() +
        commonTheme() +
        geom_hline(yintercept = 150, linetype = "longdash")
    
    ggsave("mostTotalInterest.png", width=10, height=4, units="in", dpi=150)
    
    # Country showing highest interest for particular sport
    mostInterest <- d[, .(NumberOfCountries=.N,TotalInterest=sum(Interest)), by=Sport][TotalInterest>150]
    ggplot(data=mostInterest, aes(x=Sport,y=TotalInterest)) +
        geom_bar(position="dodge", stat="identity", fill="lightblue") +
        ggtitle("Sports With Most Interest (>150)\nData Source: Google Trends") +
        coord_flip() +
        commonTheme() +
        geom_hline(yintercept = 150, linetype = "longdash")
    
    ggsave("mostInterest.png", width=10, height=4, units="in", dpi=150)
}

fullSearchInterest <- function() {
    d <- readSearchDataFile()

    # Show everything!
    qplot(Country, Sport, data=d, size=Interest) +
        theme(axis.text.x = element_text(angle = 90, size=5, hjust = 1))
    ggsave("fullSearchInterest.png", width=15, height=18, units="in", dpi=150)
}

medalsBasicStats <- function() {
    m <- readMedalsDataFile()
    
    numCountriesWithMedals <- length(levels(m$countrycode))
    numSports <- length(levels(m$sport))
    numEvents <- length(levels(m$event))
    
    #head(data.table(table(m$medalist))[order(-N,-V1)],30)
}
