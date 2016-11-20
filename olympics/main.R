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

readAthletesDataFile <- function() {
    a <- fread("data/rio2016/athletes.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, drop=c("info"))
    return(a)
}

readCountryCodeFile <- function() {
    c <- fread("data/country.codes.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, select=c("name","IOC"))
    return(c)
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

indianSearchAndParticipation <- function() {
    srch <- readSearchDataFile()
    srchind <- srch[Country=="India"]
    colnames(srchind) <- c("country", "interest", "sport")
    
    ath <- readAthletesDataFile()
    athind <- ath[nationality=="IND"]
    levels(athind$sport)[levels(athind$sport)=="hockey"] <- "Field hockey"
    levels(athind$sport)[levels(athind$sport)=="athletics"] <- "Athletics (Track & Field)"
    levels(athind$sport)[levels(athind$sport)=="gymnastics"] <- "Artistic gymnastics" # TODO Consider search interest (if any) in Rhythmic gymnastics
    levels(athind$sport)[levels(athind$sport)=="aquatics"] <- "Swimming" # TODO Consider search interest (if any) in Diving
    athind$sport <- factor(paste0(toupper(substring(athind$sport,1,1)), substring(athind$sport, 2)))
    athind <- athind[, .(numMedals=sum(gold+silver+bronze), numAthletes=.N), by=sport]

    all <- merge(athind, srchind, by=c("sport"), all.x=T)
    all$interest[is.na(all$interest)] <- 0
    
    ggplot(all, aes(x=numAthletes, y=interest)) +
        geom_point(data=all,aes(size=numMedals), colour="green4") +
        ggtitle("Indian Search Interest Correlated to Partipation & Performance\nSize: No. of Medals\nData Source: Google Trends") +
        commonTheme() +
        coord_cartesian(xlim = c(-5, 40), ylim = c(0, 15)) +
        scale_size(range=c(1,25), guide=F) +
        geom_text(data=all[sport %in% c("Badminton","Wrestling")], aes(label=paste0(sport,'\n',numMedals)), color="white") +
        geom_text(data=all[numAthletes>10], aes(label=sport), color="red", hjust=0.8, vjust=0.8) +
        geom_text(data=all[interest!=0 & numAthletes<6], aes(label=sport), color="blue", hjust=0.8, vjust=0.8)
    
    ggsave("plots/indianSearchAndParticipation.png", width=8, height=6, units="in", dpi=150)
}

searchAndParticipation <- function(srcdata) {
    srch <- readSearchDataFile()
    colnames(srch) <- c("country", "interest", "sport")
    levels(srch$sport)[levels(srch$sport) %in% c("Artistic gymnastics","Rhythmic gymnastics","Trampolining")] <- "Gymnastics"
    levels(srch$sport)[levels(srch$sport) %in% c("Swimming","Diving","Synchronized swimming","Water polo")] <- "Aquatics"
    levels(srch$sport)[levels(srch$sport) %in% c("Beach volleyball","Indoor volleyball")] <- "Volleyball"
    srch <- srch[,.(interest=sum(interest)), by=.(sport,country)] # sum due to data folding above

    ath <- readAthletesDataFile()
    levels(ath$sport)[levels(ath$sport)=="canoe"] <- "Canoeing"
    levels(ath$sport)[levels(ath$sport)=="hockey"] <- "Field hockey"
    levels(ath$sport)[levels(ath$sport)=="athletics"] <- "Athletics (Track & Field)"
    levels(ath$sport)[levels(ath$sport)=="football"] <- "Football (Soccer)"
    ath$sport <- factor(paste0(toupper(substring(ath$sport,1,1)), substring(ath$sport, 2)))
    ath <- ath[, .(numMedals=sum(gold+silver+bronze), numAthletes=.N), by=.(sport,nationality)]

    ccode <- readCountryCodeFile()
    ath <- merge(ath, ccode, by.x="nationality", by.y="IOC", all.x=T)
    levels(ath$name)[levels(ath$name) %in% c("US")] <- "United States"
    levels(ath$name)[levels(ath$name) %in% c("UK")] <- "Great Britain"
    # unique(ath[is.na(name)]$nationality) TODO Country names missing for IOA KOS MHL MNE ROT SSD but that's okay for now
    
    if (srcdata=='perTeam') {
        # Because athletes data does wrong counting of no. of medals for team events, 
        # take tally from another source
        m <- readMedalsDataFile()
        levels(m$sport)[levels(m$sport)=="Athletics"] <- "Athletics (Track & Field)"
        levels(m$sport)[levels(m$sport) %in% c("Beach Volleyball")] <- "Volleyball"
        levels(m$sport)[levels(m$sport) %in% c("Canoe Slalom","Canoe Sprint")] <- "Canoeing"
        levels(m$sport)[levels(m$sport) %in% c("Cycling BMX","Cycling Mountain Bike","Cycling Road","Cycling Track")] <- "Cycling"
        levels(m$sport)[levels(m$sport) %in% c("Swimming","Diving","Marathon Swimming","Synchronised Swimming","Water Polo")] <- "Aquatics"
        levels(m$sport)[levels(m$sport) %in% c("Gymnastics Artistic","Gymnastics Rhythmic","Gymnastics Trampoline")] <- "Gymnastics"
        levels(m$sport)[levels(m$sport) %in% c("Football")] <- "Football (Soccer)"
        levels(m$sport)[levels(m$sport) %in% c("Hockey")] <- "Field hockey"
        levels(m$sport)[levels(m$sport) %in% c("Modern Pentathlon")] <- "Modern pentathlon"
        levels(m$sport)[levels(m$sport) %in% c("Rugby Sevens")] <- "Rugby sevens"
        levels(m$sport)[levels(m$sport) %in% c("Table Tennis")] <- "Table tennis"
        m <- m[, .(numMedals=.N), by=.(sport,country)]

        ath <- merge(ath, m, by.x=c("sport","name"), by.y=c("sport","country"), all.x=T)
        ath$numMedals.x <- NULL
        colnames(ath)[5] <- "numMedals"
        ath$numMedals[is.na(ath$numMedals)] <- 0
    }
    
    all <- merge(ath, srch, by.x=c("sport","name"), by.y=c("sport","country"), all.x=T)
    all$interest[is.na(all$interest)] <- 0
    
    if (srcdata=='perTeam') {
        mi <- max(all$interest)
        ggplot(all, aes(x=numMedals, y=interest)) +
            geom_point(shape=3, size=2, colour="#555555") +
            geom_point(data=all[interest!=0 & numMedals==0], shape=3, size=2, colour="green") +
            geom_point(data=all[numMedals!=0 & interest==0], shape=3, size=2, colour="red") +
            geom_point(data=all[interest>=25 & numMedals<10], shape=3, size=2, colour="blue") +
            geom_point(data=all[numMedals>=10], shape=3, size=2, colour="purple") +
            ggtitle("Search Interest Correlated to Partipation & Performance\nLabels:Country/Sport/No. of Athletes\nData Source: Google Trends") +
            geom_text(data=all[interest>=25 & numMedals<10], aes(label=paste0(nationality,'/',sport,'/',numAthletes), hjust=-0.05, vjust=0.2), color="blue", size=2) +
            geom_text(data=all[numMedals>=10], aes(label=paste0(nationality,'/',sport,'/',numAthletes), hjust=-0.05, vjust=0.2), color="purple", size=2) +
            commonTheme() +
            theme(panel.grid.major.x = element_line(colour = "#eeeeee")) +
            coord_cartesian(xlim = c(0, max(all$numMedals)+2), ylim = c(0, mi)) +
            labs(x="No. of Medals", y="Interest") +
            scale_size(range=c(1,25), guide=F) +
            annotate("text", x=31, y=mi, label="High interest", colour='blue', size=3, hjust=0) +
            annotate("text", x=31, y=mi-4, label="No medals but non-zero interest", colour='green', size=3, hjust=0) +
            annotate("text", x=31, y=mi-8, label="Won medals but no interest", colour='red', size=3, hjust=0) +
            annotate("text", x=31, y=mi-12, label="Specialists: lots of medals", colour='purple', size=3, hjust=0) +
            annotate("text", x=31, y=mi-16, label="The rest", colour='#555555', size=3, hjust=0) +
            annotate("rect", xmin=30.5, xmax=40, ymin=mi+2, ymax=mi-19, alpha=0.3, fill="grey")
            
        ggsave("plots/searchAndParticipationPerTeam.png", width=10, height=6, units="in", dpi=150)
    }
    else {
        ggplot(all, aes(x=numMedals, y=interest)) +
            geom_point(shape=3, size=2, colour="#555555") +
            geom_point(data=all[interest!=0 & numMedals==0], shape=3, size=2, colour="green") +
            geom_point(data=all[numMedals!=0 & interest==0], shape=3, size=2, colour="red") +
            geom_point(data=all[interest>=50], shape=3, size=2, colour="blue") +
            geom_point(data=all[numMedals>=30 & interest<20], shape=3, size=2, colour="purple") +
            ggtitle("Search Interest Correlated to Partipation & Performance\nLabels:Country/Sport/No. of Athletes\nData Source: Google Trends") +
            geom_text(data=all[interest>=50], aes(label=paste0(nationality,'/',sport,'/',numAthletes), hjust=-0.05, vjust=0.2), color="blue", size=2) +
            geom_text(data=all[numMedals>=30 & interest<20], aes(label=paste0(nationality,'/',sport,'/',numAthletes), hjust=0.5, vjust=-0.8), color="purple", size=2) +
            commonTheme() +
            theme(panel.grid.major.x = element_line(colour = "#eeeeee")) +
            coord_cartesian(xlim = c(0, 50), ylim = c(0, 105)) +
            labs(x="No. of Medals", y="Interest") +
            scale_size(range=c(1,25), guide=F) +
            annotate("text", x=42, y=108, label="High interest", colour='blue', size=3, hjust=0) +
            annotate("text", x=42, y=104, label="No medals but non-zero interest", colour='green', size=3, hjust=0) +
            annotate("text", x=42, y=100, label="Won medals but no interest", colour='red', size=3, hjust=0) +
            annotate("text", x=42, y=96, label="Lots of medals but small interest", colour='purple', size=3, hjust=0) +
            annotate("text", x=42, y=92, label="The rest", colour='#555555', size=3, hjust=0) +
            annotate("rect", xmin=41.5, xmax=54, ymin=90, ymax=110, alpha=0.3, fill="grey")
        
        ggsave("plots/searchAndParticipationPerAthlete.png", width=10, height=6, units="in", dpi=150)
    }
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
    symbols(mostInterest$TotalInterest, mostInterest$NumberOfCountries, circles=sqrt(mostInterest$Interest/pi), inches=1, xlab="Total Interest Per Sport", ylab="No. of Countries")
    title("Countries Showing Most Interest By Sport\nSize:Top Country's Interest in that Sport\nData Source: Google Trends")
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
        geom_smooth(method=lm, se=T) +
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
    topnames <- data.table(table(m$medalist))[N>2 & !V1 %like% 'team']
    toppers <- m[medalist %in% topnames$V1]
    toppers$medalist <- factor(paste(toppers$medalist, toppers$sport, toppers$country, sep="\n"))
    toppers <- data.table(table(toppers$medalist, toppers$type))
    colnames(toppers) <- c("Medalist", "Type", "NumOfMedals")
    all <- as.numeric(toppers[Type=='Gold']$NumOfMedals*1000 + toppers[Type=='Silver']$NumOfMedals*100 + toppers[Type=='Bronze']$NumOfMedals*10)
    all <- data.table(cbind(toppers$Medalist, all))
    colnames(all) <- c("Medalist", "all")
    toppers <- merge(toppers, all, by="Medalist", allow.cartesian=T)
    toppers <- toppers[order(-all)]
    toppers$Medalist <- factor(toppers$Medalist, levels=rev(unique(toppers$Medalist))) # so that ggplot doesn't order the factors
    toppers$Type = factor(toppers$Type,c("Bronze","Silver","Gold"))
    toppers[toppers == 0] <- NA # TODO So that zero counts are not displayed, but not working in ggplot
    ggplot(data=toppers, aes(x=Medalist, y=Type)) +
        geom_point(aes(colour=Type), size=15, na.rm=T) +
        geom_text(aes(label=NumOfMedals), na.rm=T) + # TODO Repeat circles/points to represent medals
        ggtitle("Top Individual Medalists") +
        coord_flip(ylim=c(0,5)) +
        commonTheme() +
        scale_color_manual(values=c("#965a38", "grey", "gold"), guide=F)

    ggsave("plots/topIndividualMedalists.png", width=10, height=5, units="in", dpi=150)
}

runAll <- function() {
    dir.create('plots', showWarnings=F)
    sportSearchByCountry("plots/sportSearchIndia.png", "India")
    sportSearchByCountry("plots/sportSearchByCountry.png", c("India", "United States", "Great Britain", "China"))
    indianSearchAndParticipation()
    searchAndParticipation('perAthlete')
    searchAndParticipation('perTeam')
    indianSearchCompared()
    searchBasicStats()
    searchHdiCorr()
    medalsBasicStats()
}
