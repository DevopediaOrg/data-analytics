readDataFile <- function() {
    library(data.table)
    library(ggplot2)
    d <- fread("data/searchInterest.SportsByCountries.Aug2016.csv", 
               sep = ",", header = FALSE, stringsAsFactors = TRUE, skip = 2,
               colClasses = c("character", "integer", "character"),
               col.names = c("Country", "Interest", "Sport"))
}

commonTheme <- function() {
    theme(
        plot.title=element_text(size=12),
        legend.title = element_blank(),
        legend.position = c(.95,.95),
        plot.margin = unit(c(.1,.4,.1,.1), "cm"), # top,right,bottom,left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
}

analyzeGoogleSearches <- function(countries) {
    d <- readDataFile()
    all <- d[d$C %in% countries,]
    all$Country <- factor(all$Country)
    all$Sport <- factor(all$Sport)
    all <- data.table(xtabs(Interest ~ Country+Sport, all))
    colnames(all) <- c("Country", "Sport", "Interest")
    
    ggplot(data=all, aes(x=Sport,y=Interest,fill=factor(Country))) + 
    geom_bar(position="dodge",stat="identity") + 
    coord_flip() +
    commonTheme()
}

analyzeGoogleSearches2 <- function() {
    d <- readDataFile()
    
    india <- d[Country=="India"]
    indianSports <- india$Sport
    india$Code <- "INDIA"

    others <- d[Sport %in% indianSports][order(Sport,-Interest,Country)]
    topothers <- others[!duplicated(others$Sport), ]
    topothers$Code <- "TOPPERS"
    all <- rbind(india, topothers)
    
    ggplot(data=all, aes(x=Sport, y=Interest, fill=factor(Code))) + 
    geom_bar(position="dodge", stat="identity") + 
    ggtitle("Indian Search Interest for Olympic Sport for Period 6-Aug-2016 to 21-Aug-2016\nData Source: Google News Lab") +
    coord_flip() +
    commonTheme() +
    scale_fill_manual(values=c("blue","#eeeeee")) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_discrete(labels=sub(" ", "\n", indianSports)) +
    geom_text(aes(x=Sport, y=Interest, label=Country, hjust=1, vjust=-0.5), color = "#aaaaaa", data = all[all$Country!="India",])
}
