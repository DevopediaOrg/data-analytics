library(data.table)
library(ggplot2)
library(grid)
library(XML)

getGroupUrls <- function() {
    # Source: https://www.meetup.com/find/tech/?allMeetups=false&radius=2&userFreeform=Bangalore%2C+India&mcId=z1018093&mcName=Bangalore%2C+IN&sort=recommended&eventFilter=mysugg
    # Filtered to topic Tech
    # Copied manually from web browser and saved into HTML file

    html <- readLines('BangaloreTechMeetups.htm')
    doc = htmlParse(html, asText=TRUE)
    groupUrls <- xpathSApply(doc, "//li[@class='groupCard tileGrid-tile']/div/a[@itemprop='url']", xmlGetAttr, 'href')
    #groupUrls <- unique.default(groupUrls)

    #numMembers <- sub("^\\s*.*We're\\s*([\\d,]+)\\s*(.*)\\s*$", "\\1~\\2", xpathSApply(doc, "//p[@class='small ellipsize']", xmlValue), perl=T)
    #salutations <- sub(".*~", "", numMembers)
    #numMembers <- sub("~.*", "\\1", numMembers)
    #numMembers <- sub(",", "", numMembers)
}

downloadGroupData <- function(url) {
    fname <- paste0(sub(".*/([^/]+)/$", "\\1", url, perl=TRUE), '.htm')
    if (!file.exists(fname))
        download.file(url, fname, 'curl', quiet=T)

    html <- readLines(fname)
    doc = htmlParse(html, asText=TRUE)

    name <- xpathSApply(doc, "//div[@id='chapter-banner']/h1/a/span", xmlValue) 
    foundingDate <- as.Date(xpathSApply(doc, "//span[@itemprop='foundingDate']", xmlValue), format = "%Y%m%d")

    salutation <- xpathSApply(doc, "//span[@class='unit size5of7 wrapNice']", xmlValue)
    if (length(salutation) == 0) salutation <- 'Members'
    
    fields <- c('numOfMembers', xpathSApply(doc, "//span[@class='unit size5of7']", xmlValue))
    values <- xpathSApply(doc, "//span[@class='lastUnit align-right']", xmlValue)
    if (length(values)!=length(fields)) {
        values <- c("0", values) # zero members: update later
    }

    df <- data.frame(t(values))
    colnames(df) <- fields
    df$numOfMembers <- sub(",", "", df$numOfMembers)

    if (is.null(df$`Group reviews`)) df$`Group reviews`<- factor(0)
    if (is.null(df$`Upcoming Meetups`)) df$`Upcoming Meetups`<- factor(0)
    if (is.null(df$`Past Meetups`)) df$`Past Meetups`<- factor(0)
    
    grpdata <- c(df$numOfMembers,
                 levels(df$`Group reviews`), levels(df$`Upcoming Meetups`), levels(df$`Past Meetups`),
                 url, fname, name, as.character(foundingDate), salutation)

    return(grpdata)
}

readMainDataFile <- function() {
    d <- fread("blr.groups.30Mar2017.csv", 
               sep = "~", header = FALSE, stringsAsFactors = FALSE,
               col.names = c("name", "members", "salutation"))

    d$members <- as.numeric(gsub(",", "", d$members))
    d$salutation <- factor(d$salutation)
    
    return(d)
}

readTopicDataFile <- function() {
    d <- fread("blr.topics.30Mar2017.csv", 
               sep = ",", header = FALSE, stringsAsFactors = FALSE,
               col.names = c("topic", "members"))
    
    d$members <- as.numeric(gsub(",", "", d$members))
    d$topic <- factor(d$topic)
    
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
                      "\nData Source: Meetup.com, 30 Mar 2017.", sep='')) +
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
                      "\nData Source: Meetup.com, 30 Mar 2017.", sep='')) +
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
        geom_bar(position="dodge", stat="identity", width=0.5, fill='steelblue1') +
        ggtitle(paste("Number of Members Per Group: Only Popular Bangalore Tech Groups (3000+ Members). ", 
                      "Count: ", numToppers, ".\nData Source: Meetup.com, 30 Mar 2017.", sep='')) +
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
                      "\nData Source: Meetup.com, 30 Mar 2017.", sep='')) +
        xlab("Number of Members") +
        ylab("Number of Groups") +
        geom_text(data=countGrps[cnts>0], aes(label=paste0(percent,"%"), hjust=0.5, vjust=-0.2), color="#aaaaaa", size=3)

    ggsave(fname, width=11, height=5, units="in", dpi=150)
}

topTopicsHistogram <- function(top) {
    ggp <-
        ggplot(data=top, aes(x=topic,y=members)) +
        geom_bar(position="dodge", stat="identity", width=0.5, fill='steelblue1') +
        ggtitle(paste("Number of Bangalore Tech Groups For Popular Topics.", 
                      "\nData Source: Meetup.com, 30 Mar 2017.", sep='')) +
        coord_flip() +
        commonTheme() +
        theme(panel.grid.major.x = element_line(colour = "#eeeeee")) +
        xlab("Topic") +
        ylab("Number of Groups") +
        geom_text(data=top, aes(label=members, hjust=-0.05, vjust=0.2), color="#aaaaaa", size=3)
    
    ggsave('6.topTopicsHistogram.png', width=11, height=5, units="in", dpi=150)
}

main <- function() {
    urls <- getGroupUrls()
    all <- data.table(t(sapply(urls, downloadGroupData, USE.NAMES=F)))
    colnames(all) <- c('members', 'reviews', 'upcoming', 'past', 'url', 'fname', 'name', 'created', 'salutation')
    all <- all[, c(lapply(.(members,reviews,upcoming,past), as.integer), .(url, fname, name, created, salutation))]
    colnames(all) <- c('members', 'reviews', 'upcoming', 'past', 'url', 'fname', 'name', 'created', 'salutation')
    
    basicNumbers(all)
    topn <- membersPerTopGroups(all, 3000)
    mostPopular(all, topn)
    membershipHistogram(all, seq(0,11500,500), '4.membershipHistogram.png')
    membershipHistogram(all, seq(0,500,50), '5.lowMembershipHistogram.png')

    t <- readTopicDataFile()
    topTopicsHistogram(t)
}

#main()
