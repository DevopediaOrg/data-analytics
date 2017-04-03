library(data.table)
library(ggplot2)
library(grid)
library(XML)

getOverviewData <- function() {
    # Source: https://www.meetup.com/find/tech/?allMeetups=false&radius=2&userFreeform=Bangalore%2C+India&mcId=z1018093&mcName=Bangalore%2C+IN&sort=recommended&eventFilter=mysugg
    # Filtered to topic Tech
    # Copied manually from web browser and saved into HTML file

    html <- readLines('BangaloreTechMeetups.02Apr2017.htm')
    doc = htmlParse(html, asText=TRUE)
    urls <- xpathSApply(doc, "//li[@class='groupCard tileGrid-tile']/div/a[@itemprop='url']", xmlGetAttr, 'href')
    numMembers <- sub("^\\s*.*We're\\s*([\\d,]+)\\s*(.*)\\s*$", "\\1~\\2", xpathSApply(doc, "//p[@class='small ellipsize']", xmlValue), perl=T)
    salutations <- sub(".*~", "", numMembers)
    numMembers <- sub("~.*", "\\1", numMembers)
    numMembers <- sub(",", "", numMembers)

    od <- data.table(cbind(urls, numMembers, salutations))
    od <- od[!duplicated(od$urls)]
    od <- paste(od$urls,od$numMembers,od$salutations,sep='~')
}

downloadGroupData <- function(od) {
    od <- strsplit(od, '~')[[1]] # url, members, salutation
    fname <- paste0(sub(".*/([^/]+)/$", "\\1", od[1], perl=TRUE), '.htm')
    if (!file.exists(fname))
        download.file(od[1], fname, 'curl', quiet=T)

    html <- readLines(fname)
    doc = htmlParse(html, asText=TRUE)

    name <- xpathSApply(doc, "//div[@id='chapter-banner']/h1/a/span", xmlValue)
    foundingDate <- as.Date(xpathSApply(doc, "//span[@itemprop='foundingDate']", xmlValue), format = "%Y%m%d")

    salutation <- xpathSApply(doc, "//span[@class='unit size5of7 wrapNice']", xmlValue)
    if (length(salutation) == 0) salutation <- od[3]
    
    fields <- c('numOfMembers', xpathSApply(doc, "//span[@class='unit size5of7']", xmlValue))
    values <- xpathSApply(doc, "//span[@class='lastUnit align-right']", xmlValue)
    if (length(values)!=length(fields)) {
        values <- c(od[2], values) # take member data from overview
    }

    df <- data.frame(t(values))
    colnames(df) <- fields
    df$numOfMembers <- sub(",", "", df$numOfMembers)

    if (is.null(df$`Group reviews`)) df$`Group reviews`<- factor(0)
    if (is.null(df$`Upcoming Meetups`)) df$`Upcoming Meetups`<- factor(0)
    if (is.null(df$`Past Meetups`)) df$`Past Meetups`<- factor(0)

    grpdata <- c(df$numOfMembers,
                 levels(df$`Group reviews`), levels(df$`Upcoming Meetups`), levels(df$`Past Meetups`),
                 od[1], fname, name, as.character(foundingDate), salutation)

    return(grpdata)
}

initDerivedData <- function(d) {
    today <- format(Sys.time(), "%Y-%m-%d")
    created <- strptime(all$created, format="%Y-%m-%d")
    d$since <- format(created, "%b %Y")
    d$grpAgeDays <- difftime(today, created, units="days")
    d$evtsPerMonth <- (365.25/12)*d$past/as.numeric(d$grpAgeDays) # approx
    return(d)
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
        axis.line = element_line(colour = "gray"))
}

basicNumbers <- function(d) {
    n <- data.table(X=c(1,4), Y=c(1,1), Colour=c('a','b'), 
                    Value=c(length(d$members), sum(d$members)), 
                    Metric=c('Number of Bangalore\nTech Groups', 'Number of Members'))
    
    ggp <-
        ggplot(n, aes(x=X, y=Y)) +
        geom_point(data=n, aes(x=X, y=Y, size=100, colour=Colour), alpha=.5, show.legend=F) +
        ggtitle(paste("Some Basic Numbers.", 
                      "\nData Source: Meetup.com, 02 Apr 2017.", sep='')) +
        commonTheme() +
        theme(panel.grid.major.y = element_blank(),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) +
        xlim(0, 5) +
        geom_text(data=n, aes(x=X, y=Y, label=paste(Metric,Value,sep='\n')), color="#222222", size=5) +
        scale_size(range=c(1,100), guide=F)
    
    ggsave('1.basicNumbers.png', width=6, height=3, units="in", dpi=150)
}

mostPopular <- function(d, topn) {
    topsum <- sum(tail(sort(d$members), topn[2]))
    n <- data.table(X=c(1,4), Y=c(1,1), Colour=c('b','a'), 
                    Value=as.numeric(format(c(topn[2]*100/length(d$members), topsum*100/sum(d$members)), digits=0)), 
                    Metric=c('Top Percentile', 'Share of Members'))
    
    ggp <-
        ggplot(n, aes(x=X, y=Y)) +
        geom_point(data=n, aes(x=X, y=Y, size=100, colour=Colour), alpha=.5, show.legend=F) +
        ggtitle(paste(topn[2]," Most Popular (", topn[1], "+ members) Bangalore Tech Groups.", 
                      "\nData Source: Meetup.com, 02 Apr 2017.", sep='')) +
        commonTheme() +
        theme(panel.grid.major.y = element_blank(),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) +
        xlim(0, 5) +
        geom_text(data=n, aes(x=X, y=Y, label=paste0(Metric,'\n',Value,'%')), color="#222222", size=5) +
        scale_size(range=c(1,100), guide=F)

    ggsave('2.mostPopular.png', width=6, height=3, units="in", dpi=150)
}

membersPerTopGroups <- function(d, cutoff) {
    top <- d[members>=cutoff]
    numToppers <- length(top$name)

    ggp <-
        ggplot(data=top, aes(x=name,y=members)) +
        geom_bar(position="dodge", stat="identity", width=0.5, fill='chocolate') +
        ggtitle(paste("Number of Members Per Group: Only Popular Bangalore Tech Groups (3000+ Members). ", 
                      "Count: ", numToppers, ".\nData Source: Meetup.com, 02 Apr 2017.", sep='')) +
        coord_flip() +
        commonTheme() +
        theme(panel.grid.major.x = element_line(colour = "#eeeeee")) +
        xlab("Meetup Group Name") +
        ylab("Number of Members") +
        geom_text(data=top, aes(label=members, hjust=-0.05, vjust=0.2), color="#aaaaaa", size=3)

    ggsave('3.membersPerTopGroups.png', width=11, height=5, units="in", dpi=150)
    
    return(c(cutoff,numToppers))
}

membershipHistogram <- function(d, bins, fname) {
    snum <- sort(d$members)
    cnts <- as.data.table(tapply(snum, cut(snum, breaks=bins), length))$V1
    cnts[is.na(cnts)] <- 0
    percent <- as.numeric(format(cnts*100/sum(cnts), digits=0))
    countGrps <- as.data.table(cbind(bins=bins[1:length(bins)-1]+(bins[2]-bins[1])/2, cnts, percent))

    ggp <-
        ggplot(data=countGrps, aes(x=bins, y=cnts)) +
        geom_bar(stat="identity", fill='steelblue1') +
        commonTheme() +
        scale_x_continuous(breaks=bins) +
        ggtitle(paste("Histogram of Bangalore Tech Groups by Size [",bins[1],", ",last(bins),"]", 
                      "\nData Source: Meetup.com, 02 Apr 2017.", sep='')) +
        xlab("Number of Members") +
        ylab("Number of Groups") +
        geom_text(data=countGrps[cnts>0], aes(label=paste0(percent,"%"), hjust=0.5, vjust=-0.2), color="#aaaaaa", size=3)

    ggsave(fname, width=11, height=5, units="in", dpi=150)
}

eventFreq <- function(d, topn) {
    top <- d[members>=topn[1]]$name
    chosen <- d[evtsPerMonth>=2]
    evtsMax <- max(chosen$evtsPerMonth)

    ggp <-
        ggplot(data=chosen, aes(x=name,y=evtsPerMonth)) +
        geom_bar(data=chosen[!name %in% top], position="dodge", stat="identity", width=0.5, fill='steelblue1') +
        geom_bar(data=chosen[name %in% top], position="dodge", stat="identity", width=0.5, fill='olivedrab3') +
        ggtitle(paste("Groups With At Least Two Events Per Month.", 
                      "\nData Source: Meetup.com, 02 Apr 2017.", sep='')) +
        coord_flip() +
        commonTheme() +
        theme(panel.grid.major.x = element_line(colour = "#eeeeee"),
              panel.grid.minor.x = element_line(colour = "#f5f5f5")) +
        xlab("Meetup Group Name") +
        ylab("Number of Events Per Month") +
        annotate("text", x=28, y=evtsMax-2, label="<3000 members", colour='steelblue1', size=4, hjust=0) +
        annotate("text", x=27, y=evtsMax-2, label=">=3000 members", colour='olivedrab3', size=4, hjust=0) +
        geom_text(data=chosen, aes(label=format(evtsPerMonth,digits=2), hjust=-0.05, vjust=0.2), color="#aaaaaa", size=3)
            
    
    ggsave('6.eventFreqActiveGroups.png', width=16, height=8, units="in", dpi=150)

    top <- d[members>=topn[1]]
    ggp <-
        ggplot(top, aes(x=members, y=name)) +
        geom_point(data=top, aes(x=members, y=name, size=evtsPerMonth, colour=evtsPerMonth), alpha=.5) +
        ggtitle(paste("Events Per Month of Most Popular Groups (>=3000 members).", 
                      "\nData Source: Meetup.com, 02 Apr 2017.", sep='')) +
        commonTheme() +
        theme(panel.grid.major.x = element_line(colour = "#eeeeee"),
              panel.grid.minor.x = element_line(colour = "#f5f5f5"),
              legend.direction = "horizontal") +
        xlab("Number of Members") +
        ylab("Meetup Group Name") +
        xlim(0, max(top$members)) +
        geom_text(data=top, aes(x=members, y=name, label=format(evtsPerMonth,digits=2)), color="#222222") +
        scale_colour_gradientn(colours=c('orange','red')) +
        scale_size(range=c(1,70), guide=F)
    
    ggsave('7.eventsMostPopularGroups.png', width=16, height=8, units="in", dpi=150)

    bins <- c(-1, 0, 1/12, 2/12, 4/12, 6/12, 12/12, 24/12, 100)
    binNames <- c('==00/year', '>00/year -\n<=01/year', '>01/year -\n<=02/year', '>02/year -\n<=04/year', 
                  '>04/year -\n<=06/year', '>06/year -\n<=12/year', '>12/year -\n<=24/year', '>24/year')
    cnts <- as.data.table(tapply(d$evtsPerMonth, cut(sort(d$evtsPerMonth), breaks=bins), length))$V1
    cnts[is.na(cnts)] <- 0
    percent <- as.numeric(format(cnts*100/sum(cnts), digits=0))
    countGrps <- as.data.table(cbind(bins=bins[1:length(bins)-1]+(bins[2]-bins[1])/2, cnts, percent))
    
    ggp <-
        ggplot(data=countGrps, aes(x=binNames, y=cnts)) +
        geom_bar(stat="identity", fill='orchid4') +
        commonTheme() +
        scale_x_discrete(breaks=binNames) +
        ggtitle(paste("Histogram of Meetup Events", 
                      "\nData Source: Meetup.com, 02 Apr 2017.", sep='')) +
        xlab("Frequency of Meetup Events") +
        ylab("Number of Groups") +
        geom_text(data=countGrps[cnts>0], aes(label=paste0(percent,"%"), hjust=0.5, vjust=-0.2), color="#aaaaaa", size=3)
    
    ggsave('8.eventFreqHistogram.png', width=6, height=4, units="in", dpi=150)
}

dateCorrelation <- function(d) {
    d1 <- data.table(members=d$members, y=as.integer(d$grpAgeDays), panel='Age of Meetup Group (Days)')
    d2 <- data.table(members=d$members, y=as.integer(d$past), panel='Number of Events')
    both <- rbind(d1, d2)

    ggp <-
        ggplot(both, aes(x=members, y=y)) +
        facet_grid(panel~., scale="free") +
        geom_point(shape=3, size=2) +
        geom_smooth(method=lm, se=T) +
        ggtitle(paste("Correlating With Membership.", 
                      "\nData Source: Meetup.com, 02 Apr 2017.", sep='')) +
        xlab("Number of Members") +
        ylab("") +
        commonTheme() +
        theme(panel.grid.major.x = element_line(colour = "#eeeeee"))
    
    ggsave("9.dateCorrelation.png", width=12, height=8, units="in", dpi=150)
}

topTopicsHistogram <- function(d) {
    kwds <- c('(?i)(design)', '(?i)(develop|program|coding)', '(?i)(devops)', '(?i)(embedded|electronics|raspberry|arduino)', '(?i)(entrepreneurs|startup)', '(?i)(fintech|blockchain|bitcoin)', '(?i)(open\\s*source|oss|foss|linux)', '(?i)(python|pydata|pyladies|numpy)', '(?i)(workshop)', '(?i)\\b(test)', '(?i)(analytics|data)', '(?i)(cloud|bluemix|azure|aws|gcp|google cloud platform)', '(?i)(hadoop|big\\s*data)', '(?i)(iot|internet of )', '(?i)(js|javascript)', '(?i)(machine learning|articifial intelligence|ML|AI|spark|tensorflow)', '(?i)(mobile)', '(?i)(security)', '(?i)(web)')
    topicNames <- c('Design', 'Coding', 'DevOps', 'Electronics', 'Start-up', 'FinTech', 'OpenSource', 'Python', 'Workshop', 'Testing', 'DataAnalytics', 'Cloud', 'BigData', 'IoT', 'JS', 'ML/AI', 'Mobile', 'Security', 'Web')
    numGroups <- sapply(sapply(kwds, grep, d$name, perl=T, USE.NAMES=F), length)
    numGroups[11] <- numGroups[11]-numGroups[13] # exclude "BigData" from "DataAnalytics"
    topicData <- data.table(x=topicNames, y=numGroups)
    
    ggp <-
        ggplot(data=topicData, aes(x=x,y=y)) +
        geom_bar(position="dodge", stat="identity", width=0.5, fill='goldenrod2') +
        ggtitle(paste("Number of Bangalore Tech Groups For Popular Topics.", 
                      "\nData Source: Meetup.com, 02 Apr 2017.", sep='')) +
        coord_flip() +
        commonTheme() +
        theme(panel.grid.major.x = element_line(colour = "#eeeeee")) +
        xlab("Topic") +
        ylab("Number of Groups") +
        geom_text(data=topicData, aes(label=y, hjust=-0.05, vjust=0.2), color="#aaaaaa", size=3)
    
    ggsave('10.topTopicsHistogram.png', width=11, height=5, units="in", dpi=150)
}

main <- function() {
    od <- getOverviewData()
    all <- data.table(t(sapply(od, downloadGroupData, USE.NAMES=F)))
    colnames(all) <- c('members', 'reviews', 'upcoming', 'past', 'url', 'fname', 'name', 'created', 'salutation')
    all <- all[, c(lapply(.(members,reviews,upcoming,past), as.integer), .(url, fname, name, created, salutation))]
    colnames(all) <- c('members', 'reviews', 'upcoming', 'past', 'url', 'fname', 'name', 'created', 'salutation')
    all <- initDerivedData(all)

    basicNumbers(all)
    topn <- membersPerTopGroups(all, 3000)
    mostPopular(all, topn)
    membershipHistogram(all, seq(0,11500,500), '4.membershipHistogram.png')
    membershipHistogram(all, seq(0,500,50), '5.lowMembershipHistogram.png')
    eventFreq(all, topn)
    dateCorrelation(all)
    topTopicsHistogram(all)
}

main()
