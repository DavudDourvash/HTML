
rm(list=ls())

setwd(ifelse(Sys.info()['sysname']=="Windows", "D:\\Dropbox\\R-files\\Patient_Survey\\",
             "~/Dropbox/R-files/Patient_Survey")) 

exampletable=read.csv("Categories_comments.csv", stringsAsFactors = FALSE)

mydatafirst = read.csv("FINAL-DATA.csv", na.strings=c("NA", "", 99, 88), stringsAsFactors=FALSE)

counts=read.csv("Counts.csv")

setwd(ifelse(Sys.info()['sysname']=="Windows", "D:\\",
             "~/"))

do.call(file.remove, list(list.files(file.path(getwd(), "temp/",
                                               fsep = .Platform$file.sep), full.names=TRUE)))

file.copy(file.path(ifelse(Sys.info()['sysname']=="Windows", "D:\\Dropbox\\R-files\\HTML\\report.css",
                           "~/Dropbox/R-files/HTML/report.css"),  fsep = .Platform$file.sep),
          file.path(getwd(), "temp/report.css",  fsep = .Platform$file.sep))

file.copy(file.path(ifelse(Sys.info()['sysname']=="Windows", "D:\\Dropbox\\R-files\\HTML\\logo.png",
                           "~/Dropbox/R-files/HTML/logo.png"),  fsep = .Platform$file.sep),
          file.path(getwd(), "temp/logo.png",  fsep = .Platform$file.sep))

setwd(ifelse(Sys.info()['sysname']=="Windows", "D:\\", "~/"))

counts=counts[!duplicated(counts[, c("TeamC", "Time")]),]

counts$Contacts = as.numeric(as.character(counts$Contacts))

Quarter <<- 17 ### Mark current quarter here

if(max(mydatafirst$Time, na.rm=TRUE)> Quarter) warning("Quarter value wrong")

mydatafirst$TeamN=gsub("&", "and", mydatafirst$TeamN)

mydatafirst$Directorate[mydatafirst$Directorate==1 & !is.na(mydatafirst$Directorate)] = 2 

counts$Directorate[counts$Directorate==1 & !is.na(counts$Directorate)] = 2 

mydatafirst$Imp1 = as.numeric(as.character(mydatafirst$Imp1))

mydatafirst$Best1 = as.numeric(as.character(mydatafirst$Best1))

mydatafirst$Imp1[!mydatafirst$Imp1 %in% exampletable$Number] = NA

mydatafirst$Best1[!mydatafirst$Best1 %in% exampletable$Number] = NA

library(tm) 
library(wordcloud)
require(RColorBrewer) 
library(ggplot2) 
library(reshape) 
library(scales) 
library(xtable)
library(plyr)

directorate=c("Adult mental health City", "Adult mental health", "Arnold Lodge",
              "Child and adolescent mental health services", "Low secure + community forensic services",
              "Learning disability", "Mental health services for older people", "High secure LD", "High secure MH",
              "High secure PD", "Peaks", "High secure women's","Substance misuse", "Psychological therapy", "Wathwood",
              "Offender health", rep(NA, 8), "Bassetlaw", "Mansfield and Ashfield", "Newark and Sherwood",
              "Nottingham North and East", "Nottingham West", "Rushcliffe", "Specialist services") 

lappend <- function(lst, obj) {  
    
    lst[[length(lst)+1]] <- obj ; return(lst) }  

Year=floor(Quarter/4)

remainYear=Quarter%%4 

if(remainYear==0) Year=Year-1 

if(remainYear==0) remainYear=4 

barnames=c(paste("Apr - Mar", seq(9, 10+Year-2)), paste(c("Apr - Jun", "Jul - Sep", "Oct - Dec",
                                                          "Jan - Mar")[1:(remainYear)], c(rep((10+ Year-1), 3), 10+Year)[1:(remainYear)])) 

Year=floor(Quarter/4) 

remainYear=Quarter%%4 

if(remainYear==0) Year=Year-1 

if(remainYear==0) remainYear=4 

barnums=list() 

for(x in 1:Year){ 
    
    barnums=lappend(barnums, (4*x-3):(4*x)) 
    
} 

for (x in 1:remainYear){ 
    
    barnums=lappend(barnums, max(unlist(barnums))+1) 
    
} 

mydatafirst[,8:23][mydatafirst[,8:23]>5]=NA 

mydatafirst[,8:23][mydatafirst[,8:23]==0]=NA 

mydatafirst[,"Promoter"][mydatafirst[,"Promoter"]>5]=NA

mydatafirst[,"SU"][mydatafirst[,"SU"]>1]=NA

graph<<-1 

mydatafirst$Staff=apply(mydatafirst[which(names(mydatafirst)=="Doctor"): which(names(mydatafirst )=="Therapist")], 1, mean, na.rm=TRUE) 

mainlist=c("Service quality", "Listening", "Communication", "Respect", "Involved in care",
           "Involved in medication", "Feeling safe", "Achieve goals", "Physical health",
           "Doctor/ psychiatrist", "Nurse", "Social worker", "Psychologist", "Therapist") 

offendermain=c("Service quality", "Staff listening", "Staff communicating", "Respect",
               "Involved in care", "Improving physical health", "Improving Mental Health", rep(NA,7)) 

CHPmain=c("Service quality", "Listening", "Communication", "Respect", "Involved in care",
          "Hygiene and cleanliness", "Information about service", "Help to manage own needs", rep(NA, 6)) 

barvars=list(main=list(first=c("Service", "Listening", "Communication", "Respect", "Safe"),
                       second=c("InvCare", "InvMed", "Goals", "Health", "Staff")),
             HMP=list(first=c("Service", "Listening", "Communication", "Respect"),
                      second=c("InvCare", "InvMed", "Safe")),
             HP=list(first=c("Service", "Listening", "Communication", "Respect", "InvCare", "InvMed"),
                     second=c("Safe", "Goals", "Health", "Doctor", "Nurse", "SW")))

legvars=list(main=list(first=c("Service quality", "Listening", "Communication", "Respect", "Feeling safe"),
                       second=c("Involved in care", "Involved in medication", "Achieve goals", "Physical health", "Staff")),
             HMP=list(first=c("Service quality", "Staff listening", "Staff communicating", "Respect"),
                      second=c("Involved in care", "Improving physical health", "Improving mental health")),
             HP=list(first=c("Service quality", "Listening", "Communication", "Respect", "Involved in care",
                             "Hygiene and cleanliness"),
                     second=c("Information about service", "Help to manage own needs", "Staff available to talk",
                              "Contact if worried", "Privacy", "Informed side effects")))

############################################################ 
############################################################ 
###################### functions ########################### 
############################################################ 
############################################################ 

####################################### 
######## cat helper function ##########  
####################################### 

# write a function which writes to currentPath, sep="" and append = TRUE, then newline

myCat = function(x){
    
    cat(x, "\n", sep="", append=TRUE, file=currentPath)
    
}

########################### 
###### stack function ##### 
########################### 

stack=function(PlaceN, PlaceC, myLabels, Trust = 0){
    
    # PlaceC = c(4, 6, 13, 14) ; PlaceN = "Directorate"; myLabels=mainlist; Trust = 1
    
    graph <<- graph + 1
    
    m=mydatafirst[mydatafirst[,PlaceN] %in% PlaceC & mydatafirst$Time %in% Quarter,]
    
    if(Trust==1){
        
        missnum=apply(m[,c("Service", "Listening", "Communication", "Respect", "InvCare")],
                      2, function(x) sum(!is.na(x))) 
        
    } else {
        
        missnum=apply(m[,c("Service", "Listening", "Communication", "Respect", "InvCare",
                           "InvMed", "Safe", "Goals", "Health", "Doctor", "Nurse", "SW",
                           "Psycho", "Therapist")], 2, function(x) sum(!is.na(x))) 
        
    }
    
    if(sum(missnum>2)<4) return(FALSE)
    
    png(file=paste(file.path(getwd(), "temp", graph, fsep = .Platform$file.sep), ".png", sep=""),
        width = 658, units = "px", pointsize=12)
    
    mygraph=melt(lapply(names(missnum[missnum>2]), function(x) prop.table(table(m[,which(names(m)==x)]))*100)) 
    
    mygraph$L1=factor(mygraph$L1, labels=c(names(missnum[missnum>2]))) 
    
    print(  
        
        ggplot(mygraph, aes(L1, value, fill=factor(Var.1), order = -Var.1)) + geom_bar(position="fill", stat="identity") +
            ylab("Proportion responding") + opts(axis.text.x=theme_text(angle=90, hjust=1)) + xlab("Question") +
            scale_fill_manual(values=rainbow(5), "Response", limits=c(1:5), breaks=c(5:1),
                              labels=c("Excellent", "Good", "Fair", "Poor", "Very poor")) +
            scale_y_continuous(labels =percent_format()) + guides(fill = guide_legend(reverse = TRUE)) +
            scale_x_discrete(labels=myLabels[missnum>2]) 
    )
    
    dev.off()
    
    return(TRUE)
    
} # stack function  

#################################
######## mybar ##################
#################################

mybar=function(PlaceN, PlaceC, GraphN, type){
    
    #    PlaceC = c(4, 6, 13, 14) ; PlaceN = "Directorate"; GraphN = 1; type = "main"
    
    graph <<- graph+1
    
    mydata=mydatafirst[mydatafirst[,PlaceN] %in% PlaceC & mydatafirst$Time %in% 1:Quarter,]
    
    ybar=sapply(1:length(barnums), function(i) apply(subset(mydata, Time %in%
                                                                barnums[[i]])[,unlist(barvars[[type]][GraphN])], 2, mean, na.rm=TRUE))
    
    ### calculate for each cell in matrix number of cases based on
    
    ylength=sapply(1:length(barnums), function(i) apply(subset(mydata, Time %in%
                                                                   barnums[[i]])[,unlist(barvars[[type]][GraphN])], 2, function(x) sum(!is.na(x))))
    
    ybar[which(ylength<3)]=NA # remove all cells based on fewer than 3 cases
    
    namedrop=!apply(ybar, 1, function(x) sum(!is.na(x)))==0 # store variables with no results
    
    timedrop=!apply(ybar, 2, function(x) sum(!is.na(x)))==0 # store time periods with no results
    
    ybar=ybar[namedrop,] # remove NA rows 
    
    ybar=ybar[,timedrop] # remove NA columns
    
    longvec=apply(ylength, 2, max, na.rm=TRUE)[timedrop] # number of responses for mtext command
    
    if(class(ybar)=="matrix"){
        
        png(file=paste(file.path(getwd(), "temp", graph, fsep = .Platform$file.sep), ".png", sep=""),
            width = 658, units = "px", pointsize=12)
        
        par(mar=c(8, 4, 4, 2) + 0.1)
        
        par(cex=.9)
        
        barx=barplot(ybar, beside=T, col=rainbow(nrow(ybar)), ylim=c(1,6),xpd=FALSE, yaxt = "n", names.arg=barnames[timedrop])
        
        axis(2, at = 1:5) 
        
        legend("top", legend=unlist(legvars[[type]][GraphN])[namedrop], fill=rainbow(nrow(ybar)), bty="n", ncol = 3 )
        
        mtext(longvec, side=1, at=sapply(1:ncol(ybar), function(x) (nrow(ybar)+1)*x-(nrow(ybar))/2), line=5, outer=FALSE)
        
        mtext("Number of responses received in each period", side=1, line=3, outer=FALSE)
        
        invisible(dev.off())
        
    }
    
} # matches with function

####################################### 
######## wordcloud function ###########  
####################################### 

# x is local or forensic, y is Improve/ best 

mycloud=function(PlaceN, PlaceC, type, cloudname){
    
    mydata=mydatafirst[mydatafirst[,PlaceN] %in% PlaceC & mydatafirst$Time %in% (Quarter-3):Quarter,]
    
    pal <- brewer.pal(8,"Dark2")
    
    graph <<- graph + 1
    
#     ########################################## 
#     ######### while testing- delete !!!! #####
#     ##########################################
#     
#     png(file=paste(file.path(getwd(), "temp", graph, fsep = .Platform$file.sep), ".png", sep=""),
#         width = 658, units = "px", pointsize=12)
#     
#     plot(runif(10))
#     
#     text(6, .5, "Wordcloud in final version!", cex=3)
#     
#     waste = dev.off()
#     
#     return()
#     
#     ######################################### 
#     ######### end of delete !!!!!!!!!!! #####
#     #########################################
    
    if(!file.exists(paste(file.path(getwd(), "temp", graph, fsep = .Platform$file.sep), ".png", sep=""))){ 
        
        png(file=paste(file.path(getwd(), "temp", graph, fsep = .Platform$file.sep), ".png", sep=""),
           width = 658, units = "px", pointsize=12)
        
        mycorpus=Corpus(DataframeSource(data.frame(mydata[-is.na(mydata[,type]), type]))) 
        
        mycorpus <- tm_map(mycorpus, removePunctuation) 
        mycorpus <- tm_map(mycorpus, tolower) 
        mycorpus <- tm_map(mycorpus, function(m) removeWords(m, c(stopwords("english"), "none", "rampton", "ive", "dont", "etc"))) 
        
        tdm <- TermDocumentMatrix(mycorpus) 
        m <- as.matrix(tdm) 
        v <- sort(rowSums(m),decreasing=TRUE) 
        d <- data.frame(word = names(v),freq=v) 
        
        wordcloud(d$word,d$freq, scale=c(2.5,.5), max.words=250, random.order=TRUE,rot.per=.15,
                  colors=pal, vfont=c("sans serif","plain")) 
        
        mtext(paste(cloudname, "-", ifelse(type=="Improve", "Improve one thing", "Best thing")), side=3, line=0, outer=FALSE) 
        
        mtext("This word cloud represents frequency of words within the Service User and Carer ",
              side=1, line=0, outer=FALSE) 
        
        timetext=paste(rep(c("Apr - Jun", "Jul - Sep", "Oct - Dec", "Jan - Mar"), 10)[-40],
                       c("09", "09", "09", rep(10:18, each=4))) 
        
        paste(timetext[8:11], collapse=" ") 
        
        if(Quarter%%4==3){ 
            
            temp=paste(timetext[(Quarter-3):Quarter], collapse=" ") 
            
            finaltext=paste(substr(temp, 1, 5), substr(temp, nchar(temp)-5, nchar(temp)), collapse=" ") 
        } 
        
        if(Quarter%%4!=3){ 
            
            temp=paste(timetext[(Quarter-3):Quarter], collapse=" ") 
            
            finaltext=paste(substr(temp, 1, 3), substr(temp, 11, 12),
                            substr(temp, nchar(temp)-7, nchar(temp)), collapse=" ") 
            
        } 
        
        mtext(paste("Experience Survey between", finaltext, "based on",
                    length(mydata[,type][!is.na(mydata[,type])]), "comments"),
              side=1, line=1, outer=FALSE)
        
        dev.off()
        
    } # matches with if file.exists
    
} # function end 

####################################### 
######## mytable function #############  
####################################### 

mytable=function(PlaceN, PlaceC, x){
    
    #  PlaceN = "Directorate" ; PlaceC = 6 ; x = 1
    
    mydata=mydatafirst[mydatafirst[,PlaceN] %in% PlaceC,]
    
    if(x==1) { 
        
        resultstable=prop.table(with(subset(mydata, Time==Quarter),
                                     table(factor(Imp1, levels = exampletable$Number)))) * 100
        
        addcomments1=sum(with(subset(mydata, Time==Quarter), table(factor(Imp1, levels=exampletable$Number))))
        
        addcomments2=sum(with(subset(mydata, Time %in% (Quarter-4):(Quarter-1)), table(factor(Imp1, levels=exampletable$Number))))
        
        lasttable=round(prop.table(with(subset(mydata, Time %in% (Quarter-4):(Quarter-1)),
                                        table(factor(Imp1, levels=exampletable$Number)))), 3)*100  
        
    }
    
    if(x==2){
        
        resultstable=prop.table(with(subset(mydata, Time==Quarter),
                                     table(factor(Best1, levels = exampletable$Number)))) * 100
        
        addcomments1=sum(with(subset(mydata, Time==Quarter), table(factor(Best1, levels=exampletable$Number))))
        
        addcomments2=sum(with(subset(mydata, Time %in% (Quarter-4):(Quarter-1)), table(factor(Best1, levels=exampletable$Number))))
        
        lasttable=round(prop.table(with(subset(mydata, Time %in% (Quarter-4):(Quarter-1)),
                                        table(factor(Best1, levels=exampletable$Number)))), 3)*100
        
    }
    
    finaltable=data.frame("resultstable" = round(as.numeric(resultstable), 1),
                          "lasttable" = round(as.numeric(lasttable), 1), exampletable)
    
    finaltable = ddply(finaltable, .(Super, Category), numcolwise(sum))
    
    finaltable$Blank = NA # these two lines just put a blank column in to make the column references correct
    
    finaltable = finaltable[,c(6, 1:4)]
    
    sumtable=tapply(as.numeric(finaltable$resultstable), finaltable$Super, sum)
    
    finaltable$Sumcategory=NA
    
    sumlast=tapply(finaltable$lasttable, finaltable$Super, sum)  
    
    for (i in 1:length(sumtable)) {  
        
        finaltable$Sumcategory[finaltable$Super==names(sumtable[i])]=sumtable[i]  
        
    }  
    
    for (i in 1:length(sumtable)) {  
        
        finaltable$lastcategory[finaltable$Super==names(sumtable[i])]=sumlast[i]  
        
    }  
    
    finaltable2=finaltable[with(finaltable, order(-Sumcategory, -as.numeric(resultstable))),]  
    
    realtable=matrix(data=NA, nrow=0, ncol=7) 
    
    for (i in unique(finaltable2[,2])){
        
        realtable=rbind(realtable, matrix(c(NA, i, "Total", NA, NA, finaltable2[head(which(finaltable2[2]==i), 1),6],
                                            finaltable2[head(which(finaltable2[2]==i), 1),7]), nrow=1))  
        
        realtable=rbind(realtable, as.matrix(finaltable2[which(finaltable2[2]==i),]))
        
    }
    
    temptable=realtable[,c(2,3,6,4,7,5)]
    
    tempvec=as.numeric(as.character(temptable[,4]))>0 
    
    tempvec[is.na(tempvec)]=TRUE 
    
    finaltable3=temptable[tempvec,] 
    
    finaltable3=data.frame(finaltable3[finaltable3[,3]>0 | !is.na(finaltable3[,4]),]) 
    
    finaltable3=finaltable3[!is.na(finaltable3$Super),] 
    
    finaltable3$Sumcategory[finaltable3$Category!="Total"] = NA
    
    finaltable3$lastcategory[finaltable3$Category!="Total"] = NA
    
    # finaltable3 = finaltable2[,c(4, 3, 6, 2, 8, 7)]
    
    names(finaltable3)=c("Main category", "Category", "Total this quarter %", "Category % this quarter", "Total last year %",
                         "Category % last year")  
    
    if(x==1){ 
        
        myCat(paste("<p>The table below shows the percentage of responses from the comments received to the question
           'If you could improve one thing about the care received what would it be?'  We received", addcomments1,
                    "responses to this question this quarter and", addcomments2, "responses in the last four quarters.</p>")) 
        
    } # if x is 1
    
    if(x==2){ 
        
        myCat(paste("<p>The table below shows the percentage of responses from the comments received to the question
           'What was the best thing about the care received?'  We received", addcomments1,
                    "responses to this question this quarter and", addcomments2, "responses in the last four quarters.</p>")) 
        
    } # if x is 2
    
    print(xtable(finaltable3), type="html", append=TRUE, file=currentPath, include.rownames=FALSE) 
    
    myCat("</br>") 
    
} 

####################################### 
######## mycomments function ##########  
####################################### 

mycomments=function(PlaceN, PlaceC, type){ 
    
    # PlaceN = "Directorate"; PlaceC = 2; type = 1
    
    mydata=mydatafirst[mydatafirst[,PlaceN] %in% PlaceC,]
    
    if(type==1){  
        
        commentstable=subset(mydata, Time==Quarter)[,c("Improve", "Imp1")]
        
        names(commentstable)=c("Improve", "Imp1")
        
        # and miscellaneous comments
        
        miscprint = subset(mydata, Time == Quarter)$Improve[is.na(subset(mydata, Time == Quarter)$Imp1)]
        
    }  
    
    if(type==2){  
        
        commentstable=subset(mydata, Time==Quarter)[,c("Best", "Best1")]
        
        names(commentstable)=c("Improve", "Imp1")
        
        miscprint = subset(mydata, Time == Quarter)$Best[is.na(subset(mydata, Time == Quarter)$Best1)]
        
    }
    
    store=list() 
    
    for(x in 1:length(levels(factor(exampletable$Super)))){ 
        
        store=lappend(store, exampletable$Number[which(exampletable$Super==levels(factor(exampletable$Super))[x])])
        
    } 
    
    if(length(levels(factor(exampletable$Super)))>0){ 
        
        for(y in 1:length(levels(factor(exampletable$Super)))){ 
            
            if(sum(commentstable$Imp1 %in% unlist(store[[y]]))>0){ 
                
                myCat(c("<h3> ", levels(factor(exampletable$Super))[y], " </h3>"))
                
                vecprint=as.character(subset(commentstable, Imp1 %in% unlist(store[[y]]))$Improve)
                
                for (c in 1:length(vecprint)){
                    
                    myCat(vecprint[c])
                    
                    myCat("<br>")
                    
                }  
            } # matches with if(sum comments>0)  
        } # matches for for (x in length) 
    } # matches with if () error handling 
    
    # print the misc comments
    
    myCat(c("<h3>Miscellaneous</h3>"))
    
    miscprint = miscprint[!is.na(miscprint)]
    
    for (c in 1:length(miscprint)){
        
        myCat(miscprint[c])
        
        myCat("<br>")
        
    }
    
    if(type==2){ # only if it's best thing, i.e. at the end
        
        # print PO
        
        poPrint = subset(mydata, Time==Quarter)$PO[!is.na(subset(mydata, Time==Quarter)$PO)]
        
        if(length(poPrint)>1){
            
            myCat(c("<h3>Patient Opinion</h3>"))
            
            for (c in 1:length(poPrint)){
                
                myCat(poPrint[c])
                
                myCat("<br>")
                
            }
            
        }
        
        # print PALS
        
        palsPrint = subset(mydata, Time==Quarter)$PALS[!is.na(subset(mydata, Time==Quarter)$PALS)]
        
        if(length(palsPrint)>1){
            
            myCat(c("<h3>PALS</h3>"))
            
            for (c in 1:length(palsPrint)){
                
                myCat(palsPrint[c])
                
                myCat("<br>")
                
            }
            
        }
        
    } # end only if it's best thing
    
} # matches with function

##########################################
##### response rate function #############
##########################################

myResponse = function(PlaceN, PlaceC) {
    
    # PlaceN = "Directorate"; PlaceC = 25
    
    mydata=mydatafirst[mydatafirst[,PlaceN] %in% PlaceC & mydatafirst$TeamC !=1700,]
    
    help=table(mydata$TeamC[mydata$Time == Quarter])
    
    teamnames=names(help)
    
    teamnumbers=help
    
    responseTab = data.frame("TeamC" = as.numeric(teamnames), "Responses" = as.numeric(teamnumbers))
    
    finalTab = merge(responseTab, subset(counts, Time==Quarter), by = "TeamC", all.x=TRUE)[,c("TeamC",
                                                                                              "TeamN", "Responses", "Contacts")]
    
    finalTab$Percent = round(finalTab$Responses / finalTab$Contacts * 100, 1)
    
    finalTab$TeamN = as.character(
        sapply(finalTab$TeamC, function(x)
            tail(as.character(counts$TeamN[which(x==counts$TeamC)]), 1))
    )
    
    finalTab$TeamN[finalTab$TeamN == "character(0)"] = as.character(
        sapply(finalTab$TeamC[finalTab$TeamN == "character(0)"], function(x) as.character(subset(mydatafirst, Time==Quarter)$TeamN
                                                                  [tail(which(x==subset(mydatafirst, Time==Quarter)$TeamC), 1)]))
    )
        
    finalTab$Percent[finalTab$Percent>100] = 100
    
    finalTab = finalTab[order(finalTab$Percent, decreasing = TRUE),]
    
    finalTab[4:5][is.na(finalTab[4:5])] = "NK"
    
    if(PlaceC == 16) finalTab = finalTab[,1:3]
    
    names(finalTab)[2] = c("Team")
    
    print(xtable(finalTab[-1], digits = ifelse(PlaceC==16, c(0, 0, 0), c(0,0,0,0,1))), type="html", append = TRUE, file=currentPath, include.rownames = FALSE)
    
    return(finalTab)
    
}

##################################################
################## SUMMARY #######################
##################################################

# fix file path

currentPath <<- file.path(getwd(), "temp", "trust.html", fsep = .Platform$file.sep)

cat("<html>", "\n", file = currentPath)

myCat("<head>")

myCat("<title>Trust summary</title>")

myCat('<link href="report.css" rel="stylesheet" type="text/css" />')

myCat("</head>")

myCat("<body>")

myCat('<div id="report">') # this tag ends right at the end

myCat('<div id="header">')

myCat('<a href="#"><img src="logo.png" alt="Positive... about change" /></a>')

myCat('<p class="title">')

myCat(c('<strong>Service User and Carer Experience Report for ', tail(barnames, 1), '</strong><br/>'))

myCat(c('<em>Retrieved on:', format(Sys.time(), "%a %b %d %Y"), '</em><br/>'))

myCat('</p>')

myCat('</div>')

myCat('<div id="breadcrumb">')

myCat('<a href="http://feedback.nottinghamshirehealthcare.nhs.uk" title="Back to main site">
      Back to main site</a> &rsaquo;<a href="index.html">Index</a> &rsaquo; Trust summary')

myCat('</div>')

myCat("<h1> Trust summary </h1>")

myCat('<div id="content-left">')

myCat(c("<p>Across the Trust we received ", length(subset(mydatafirst, Time==Quarter)$Service),
        " responses. Of these ", table(subset(mydatafirst, Time==Quarter)$SU)[2],
        " were from carers.</p>"))

myCat(c("<p>Across the whole Trust there was a service quality rating of ",
        round(mean(subset(mydatafirst, Time==Quarter)$Service, na.rm=TRUE)*20),
        "%. This compares with ", round(mean(subset(mydatafirst, Time==Quarter-1)$Service,
                                             na.rm=TRUE)*20), "% in the previous quarter and ", round(mean(subset(mydatafirst, Time %in%
                                                                                                                      (Quarter-4):(Quarter-1))$Service, na.rm=TRUE)*20), " % in the previous four quarters.</p>"))

mydata=mydatafirst

Subset.P=subset(mydatafirst, Time==Quarter)

myCat(c("<p>Net promoter score was ", round((sum(Subset.P$Promoter == 5, na.rm=TRUE)/
                                                 sum(Subset.P$Promoter %in% 1:5, na.rm=TRUE) -
                                                 sum(Subset.P$Promoter %in% 1:3, na.rm=TRUE)/
                                                 sum(Subset.P$Promoter %in% 1:5, na.rm=TRUE))*100, 0),
        ". This compares with ", round((sum(subset(mydatafirst, Time == Quarter-1)$Promoter == 5, na.rm=TRUE)/
                                            sum(subset(mydatafirst, Time == Quarter-1)$Promoter %in% 1:5, na.rm=TRUE) -
                                            sum(subset(mydatafirst, Time == Quarter-1)$Promoter %in% 1:3, na.rm=TRUE)/
                                            sum(subset(mydatafirst, Time == Quarter-1)$Promoter %in% 1:5, na.rm=TRUE))*100, 0)
        , " in the previous quarter and ",
        round((sum(subset(mydatafirst, Time %in% (Quarter-4):(Quarter-1))$Promoter == 5, na.rm=TRUE)/
                   sum(subset(mydatafirst, Time %in% (Quarter-4):(Quarter-1))$Promoter %in% 1:5, na.rm=TRUE) -
                   sum(subset(mydatafirst, Time %in% (Quarter-4):(Quarter-1))$Promoter %in% 1:3, na.rm=TRUE)/
                   sum(subset(mydatafirst, Time %in% (Quarter-4):(Quarter-1))$Promoter %in% 1:5, na.rm=TRUE))*100, 0),
        " in the previous four quarters."))

mycloud("Local", 0:2, "Improve", "Trust") 

myCat(c('<img src= "', graph, '.png" alt = "Improve one thing - word cloud" />'))

mycloud("Local", 0:2, "Best", "Trust") 

myCat(c('<img src= "', graph, '.png" alt = "Best thing - word cloud" />'))

stack(PlaceN = "Local", PlaceC = 0:2, Trust = 1, myLabels = mainlist) 

myCat(c('<img src= "', graph, '.png" alt = "Stacked barchart" />'))

mybar(PlaceN="Local", PlaceC=0:2, GraphN=1, type="main")

if(file.exists(paste(file.path(getwd(), "temp", graph, fsep = .Platform$file.sep), ".png", sep=""))){ 
    
    myCat(c('<img src= "', graph, '.png" alt = "Trend barchart" />'))
    
} 

myCat("<h2> Improve one thing </h2>")

mytable("Local", 0:2, 1) 

myCat("<br>")

myCat("<h2> Best thing </h2>")

mytable("Local", 0:2, 2)

myCat("<br>")

myCat("</div>")

myCat('<div id="content-right">')

myCat('<h2>Navigation</h2>')

myCat('<ol>')

myCat('<li><a href="local.html">Local services</a><br></li>')

myCat('<li><a href="special_local.html">Specialist services directorate</a><br></li>')

myCat('<li><a href="forensic.html">Forensic services</a><br></li>')

check1 = stack(PlaceN = "Directorate", PlaceC = 8:12, myLabels = mainlist)

if(check1) myCat('<li><a href="special_f.html">Special services (Rampton) directorate</a><br></li>')

myCat('<li><a href="hp.html">Health partnerships</a><br></li>')

myCat('</ol>')

myCat("</div>")

myCat("</div>") # report tag

myCat("</body>")

myCat("</html>")

########################################## 
##########################################   
########       Division section  ######### 
########################################## 
########################################## 

bigFunction = function(name, funcPlaceC, funcPlaceN, URL, barType, dirVec, funcLabels) {
    
    # name = "Health partnerships"; funcPlaceC = c(4, 6, 13, 14)
    # funcPlaceN = "Directorate"; URL = "HP" ; barType = "HP"
    # dirVec = c(25:31)
    
    ## only do this if it's a request for a division section
    
    checkDiv = stack(PlaceN = funcPlaceN, PlaceC = funcPlaceC, myLabels = funcLabels)
    
    if(!999 %in% funcPlaceC & checkDiv){
        
        currentPath <<- file.path(getwd(), "temp", paste0(URL, ".html"), fsep = .Platform$file.sep)
        
        mydata=mydatafirst[mydatafirst[,funcPlaceN] %in% funcPlaceC,]
        
        Subset.P=mydatafirst[mydatafirst[,funcPlaceN] %in% funcPlaceC & mydatafirst$Time==Quarter,]
        
        myCat("<html>")
        
        myCat("<head>")
        
        myCat(c("<title>", name, "</title>"))
        
        myCat('<link href="report.css" rel="stylesheet" type="text/css" />')
        
        myCat("</head>")
        
        myCat("<body>")
        
        myCat('<div id="report">') # this tag ends right at the end
        
        myCat('<div id="header">')
        
        myCat('<a href="#"><img src="logo.png" alt="Positive... about change" /></a>')
        
        myCat('<p class="title">')
        
        myCat(c('<strong>Service User and Carer Experience Report for ', tail(barnames, 1), '</strong><br/>'))
        
        myCat(c('<em>Retrieved on:', format(Sys.time(), "%a %b %d %Y"), '</em><br />'))
        
        myCat('</p>')
        
        myCat('</div>')
        
        myCat('<div id="breadcrumb">')
        
        myCat(c('<a href="http://feedback.nottinghamshirehealthcare.nhs.uk" title="Back to main site">
                Back to main site</a> &rsaquo;<a href="index.html">Index</a> &rsaquo; <a href="trust.html">
                Trust summary</a> &rsaquo; ', name))
        
        myCat('</div>')
        
        myCat(c("<h1>", name, "</h1>"))
        
        myCat('<div id="content-left">')
        
        countsSubset = counts[counts[,funcPlaceN] %in% funcPlaceC,]
        
        responseRate = round(length(subset(mydata, Time==Quarter & Directorate!=16)$Service) /
                                 sum(subset(countsSubset, Time==Quarter & Directorate!=16)$Contacts, na.rm=TRUE)*100, 1)
        
        if(URL == "special_f"){
            
            responsePrev = round(length(subset(mydata, Time==(Quarter-2) & Directorate!=16)$Service) /
                                     sum(subset(countsSubset, Time==(Quarter-2) & Directorate!=16)$Contacts, na.rm=TRUE)*100, 1)            
            
        } else {
            
            responsePrev = round(length(subset(mydata, Time==(Quarter-1) & Directorate!=16)$Service) /
                                     sum(subset(countsSubset, Time==(Quarter-1) & Directorate!=16)$Contacts, na.rm=TRUE)*100, 1)
        }
        

        
        responseYear = round(length(subset(mydata, Time %in% (Quarter-4):(Quarter-1) & Directorate!=16)$Service) /
                     sum(subset(countsSubset, Time %in% (Quarter-4):(Quarter-1) & Directorate!=16)$Contacts, na.rm=TRUE)*100, 1)
        
        SQprevious = ifelse(sum(funcPlaceC %in% c(3, 5, 8:12, 15))>0,
                            round(mean(subset(mydata, Time==Quarter-2)$Service, na.rm=TRUE)*20),
                            round(mean(subset(mydata, Time==Quarter-1)$Service, na.rm=TRUE)*20))
        
        myCat(c("<p>In ", name, " we received ", length(subset(mydata, Time==Quarter)$Service),
                " responses. Of these ", as.numeric(table(factor(subset(mydata, Time==Quarter)$SU, levels=0:1))[2]),
                " were from carers.", ifelse(is.na(responseRate), "</p>", paste0("This is a response rate of ",
                         responseRate, "%.", ifelse(is.na(responsePrev), " This compares with ",
                                                    paste0("This compares with ", responsePrev,
                         " % in the previous quarter and ")), responseYear,
                         " % in the previous four quarters.</p>"))))
        
        myCat(c("<p>This quarter ", name, " had a service quality rating of ",
                round(mean(subset(mydata, Time==Quarter)$Service, na.rm=TRUE)*20),
                " %.", ifelse(is.na(SQprevious), "This compares with ", paste0("This compares with ", SQprevious,
                " % in the previous quarter and ")),
                round(mean(subset(mydata, Time %in% (Quarter-4):(Quarter-1))$Service, na.rm=TRUE)*20),
                " % in the previous four quarters.</p>"))
        
        NPScheck = length(Subset.P$Promoter[!is.na(Subset.P$Promoter)]) >=50
        
        NPSprevious = ifelse(sum(funcPlaceC %in% c(3, 5, 8:12, 15))>0,
                             round((sum(subset(mydata, Time==Quarter-2)$Promoter == 5, na.rm=TRUE)/
                                        sum(subset(mydata, Time==Quarter-2)$Promoter %in% 1:5, na.rm=TRUE) -
                                        sum(subset(mydata, Time==Quarter-2)$Promoter %in% 1:3, na.rm=TRUE)/
                                        sum(subset(mydata, Time==Quarter-2)$Promoter %in% 1:5, na.rm=TRUE))*100, 0),
                             round((sum(subset(mydata, Time==Quarter-1)$Promoter == 5, na.rm=TRUE)/
                                        sum(subset(mydata, Time==Quarter-1)$Promoter %in% 1:5, na.rm=TRUE) -
                                        sum(subset(mydata, Time==Quarter-1)$Promoter %in% 1:3, na.rm=TRUE)/
                                        sum(subset(mydata, Time==Quarter-1)$Promoter %in% 1:5, na.rm=TRUE))*100, 0))
        
        NPSyear = round((sum(subset(mydata, Time %in% (Quarter-4):(Quarter-1))$Promoter == 5, na.rm=TRUE)/
                             sum(subset(mydata, Time %in% (Quarter-4):(Quarter-1))$Promoter %in% 1:5, na.rm=TRUE) -
                             sum(subset(mydata, Time %in% (Quarter-4):(Quarter-1))$Promoter %in% 1:3, na.rm=TRUE)/
                             sum(subset(mydata, Time %in% (Quarter-4):(Quarter-1))$Promoter %in% 1:5, na.rm=TRUE))*100, 0)
        
        if(NPScheck){
            
            NPS = round((sum(Subset.P$Promoter == 5, na.rm=TRUE)/
                             sum(Subset.P$Promoter %in% 1:5, na.rm=TRUE) -
                             sum(Subset.P$Promoter %in% 1:3, na.rm=TRUE)/
                             sum(Subset.P$Promoter %in% 1:5, na.rm=TRUE))*100, 0)
            
            if(!is.na(NPS)){
                
                myCat(c("<p>Net promoter score was ", NPS, ifelse(is.na(NPSprevious),
                            ". This compares with ", paste0(". This compares with ", NPSprevious,
                            " in the previous quarter and ")), NPSyear,
                        " in the previous four quarters.</p>"))
                
            }
            
        }
        
        mycloud(PlaceN = funcPlaceN, PlaceC = funcPlaceC, type = "Improve", cloudname = name) 
        
        myCat(c('<img src= "', graph, '.png" alt = "Improve one thing- Word cloud" />'))
        
        mycloud(PlaceN = funcPlaceN, PlaceC = funcPlaceC, type = "Best", cloudname = name)
        
        myCat(c('<img src= "', graph, '.png" alt = "Best thing- word cloud" />'))
        
        stack(PlaceN = funcPlaceN, PlaceC = funcPlaceC, myLabels = funcLabels)
        
        myCat(c('<img src= "', graph, '.png" alt = "Stacked barchart" />'))
        
        mybar(PlaceN=funcPlaceN, PlaceC=funcPlaceC, GraphN=1, type=barType)
        
        if(file.exists(paste(file.path(getwd(), "temp", graph, fsep = .Platform$file.sep), ".png", sep=""))){ 
            
            myCat(c('<img src= "', graph, '.png" alt = "Trend barchart" />'))
            
        }
        
        mybar(PlaceN=funcPlaceN, PlaceC=funcPlaceC, GraphN=2, type=barType)
        
        if(file.exists(paste(file.path(getwd(), "temp", graph, fsep = .Platform$file.sep), ".png", sep=""))){ 
            
            myCat(c('<img src= "', graph, '.png" alt = "Trend barchart - 2nd" />'))
            
        } 
        
        myCat("<h2> Improve one thing </h2>")
        
        mytable(PlaceN=funcPlaceN, PlaceC=funcPlaceC, 1) 
        
        myCat("<br>")
        
        myCat("<h2> Best thing </h2>")
        
        mytable(PlaceN=funcPlaceN, PlaceC=funcPlaceC, 2)
        
        myCat("<br>")
        
        myCat("</div")
        
        myCat('<div id="content-right">')
        
        myCat('<h3>Navigation</h3>')
        
        myCat('<ul>')
        
        for (d in dirVec) {
            
            checkdir = stack(PlaceN = "Directorate", PlaceC = d, myLabels = mainlist)
            
            if(checkdir) {
                myCat(c("<li><a href=", gsub("\\s","_", tolower(directorate[d])), ".html>", directorate[d],
                        "</a><br></li>"))
            }
            
        }
        
        myCat('</ul>')
        
        myCat("</div>")
        
        myCat("</div>") # report
        
        myCat("</body>")
        
        myCat("</html>")
        
    } # end if it's a division request
    
    ###########################################
    ########### Directorate section ###########
    ###########################################
    
    if(URL == "forensic") dirVec = dirVec[dirVec!=16] 
    
    for (d in dirVec){
        
        currentPath <<- file.path(getwd(), "temp", paste0(gsub("\\s","_", tolower(directorate[d])), ".html"),
                                  fsep = .Platform$file.sep)
        
        check1 = stack(PlaceN = "Directorate", PlaceC = d, myLabels = funcLabels)
        
        if(check1){
            
            myCat("<html>")
            
            myCat("<head>")
            
            myCat(c("<title> ", directorate[d], " </title>"))
            
            myCat('<link href="report.css" rel="stylesheet" type="text/css" />')
            
            myCat("</head>")
            
            myCat("<body>")
            
            myCat('<div id="report">') # this tag ends right at the end
            
            myCat('<div id="header">')
            
            myCat('<a href="#"><img src="logo.png" alt="Positive... about change" /></a>')
            
            myCat('<p class="title">')
            
            myCat(c('<strong>Service User and Carer Experience Report for ', tail(barnames, 1), '</strong><br/>'))
            
            myCat(c('<em>Retrieved on:', format(Sys.time(), "%a %b %d %Y"), '</em><br />'))
            
            myCat('</p>')
            
            myCat('</div>')
            
            myCat('<div id="breadcrumb">') # automate
            
            if(!999 %in% funcPlaceC){
                
                myCat(c(paste0('<a href="http://feedback.nottinghamshirehealthcare.nhs.uk" title="Back to main site">
              Back to main site</a> &rsaquo;<a href="index.html">Index</a> &rsaquo;
              <a href="trust.html">Trust summary</a> &rsaquo;
              <a href="', URL, '.html">', name), '</a> &rsaquo;', directorate[d]))
                
            } else {
                
                myCat(c(paste0('<a href="http://feedback.nottinghamshirehealthcare.nhs.uk" title="Back to main site">
              Back to main site</a> &rsaquo;<a href="index.html">Index</a> &rsaquo;
              <a href="trust.html">Trust summary</a> &rsaquo;
              <a href="forensic.html">Forensic services</a> &rsaquo;', directorate[d])))
                
            }
            
            myCat('</div>')
            
            myCat(c("<h1> ", directorate[d], " </h1>"))
            
            myCat('<div id="content-left">')
            
            countsDir = counts[counts$Directorate == d,]
            
            Subset.P=subset(mydatafirst, Time==Quarter&Directorate==d)
            
            mydata=subset(mydatafirst, Directorate==d)
            
            responseDir = round(length(subset(mydata, Time==Quarter & Directorate!=16)$Service) /
                                     sum(subset(countsDir, Time==Quarter & Directorate!=16)$Contacts, na.rm=TRUE)*100, 1)
            
            if(d %in% c(3, 5, 8:12, 15)){
                
                responsePrevDir = round(length(subset(mydata, Time==(Quarter-2) & Directorate!=16)$Service) /
                                         sum(subset(countsDir, Time==(Quarter-2) & Directorate!=16)$Contacts, na.rm=TRUE)*100, 1)            
                
            } else {
                
                responsePrevDir = round(length(subset(mydata, Time==(Quarter-1) & Directorate!=16)$Service) /
                                         sum(subset(countsDir, Time==(Quarter-1) & Directorate!=16)$Contacts, na.rm=TRUE)*100, 1)
            }
            
            
            
            responseYearDir = round(length(subset(mydata, Time %in% (Quarter-4):(Quarter-1) & Directorate!=16)$Service) /
                                     sum(subset(countsDir, Time %in% (Quarter-4):(Quarter-1) & Directorate!=16)$Contacts, na.rm=TRUE)*100, 1)
            
            Subset.P$sumNA = apply(Subset.P[,c("Service", "Listening", "Communication", "Respect", "InvCare")],
                                   1, function(x) sum(!is.na(x), na.rm=TRUE))
            
            Subset.P=subset(Subset.P, sumNA>0)
            
            help=table(Subset.P$TeamC)
            
            teamnames=names(which(help>2))
            
            teamnumbers=help[which(help>2)]
            
            Subset.P = Subset.P[Subset.P$TeamC %in% teamnames,]
            
            myCat(c("<p>In ", directorate[d], " we received ", length(subset(mydata, Time==Quarter)$Service),
                    " responses. Of these ", as.numeric(table(factor(subset(mydata, Time==Quarter)$SU, levels=0:1))[2]),
                    " were from carers.", ifelse(is.na(responseDir), "</p>", paste0("This is a response rate of ",
                        responseDir, "%.", ifelse(is.na(responsePrevDir), "This compares with ",
                        paste0("This compares with ", responsePrevDir, " % in the previous quarter and ")), responseYearDir,
                        " % in the previous four quarters.</p>"))))
            
            SQpreviousdir = ifelse(sum(d %in% c(3, 5, 8:12, 15))>0,
                                round(mean(subset(mydatafirst, Time==Quarter-2 & Directorate==d)$Service, na.rm=TRUE)*20),
                                round(mean(subset(mydatafirst, Time==Quarter-1 & Directorate==d)$Service, na.rm=TRUE)*20))
            
            myCat(c("<p>This quarter the directorate had a service quality rating of ",
                    round(mean(subset(mydatafirst, Time==Quarter & Directorate==d)$Service, na.rm=TRUE)*20),
                    "%.", ifelse(is.na(SQpreviousdir), "This compares with ", paste0("This compares with ", SQpreviousdir,
                    "% in the previous quarter and ")),
                    round(mean(subset(mydatafirst, Time %in% (Quarter-4):(Quarter-1) & Directorate==d)$Service, na.rm=TRUE)*20),
                    "% in the previous four quarters.</p>"))
            
            NPScheckdir = length(subset(mydatafirst, Time==Quarter & Directorate==d)$Promoter[!is.na(subset(mydatafirst,
                                                                    Time==Quarter & Directorate==d)$Promoter)]) >=50
            
            NPSpreviousdir = ifelse(sum(d %in% c(3, 5, 8:12, 15))>0,
                                 round((sum(subset(mydatafirst, Time==Quarter-2 & Directorate==d)$Promoter == 5, na.rm=TRUE)/
                                            sum(subset(mydatafirst, Time==Quarter-2 & Directorate==d)$Promoter %in% 1:5, na.rm=TRUE) -
                                            sum(subset(mydatafirst, Time==Quarter-2 & Directorate==d)$Promoter %in% 1:3, na.rm=TRUE)/
                                            sum(subset(mydata, Time==Quarter-2 & Directorate==d)$Promoter %in% 1:5, na.rm=TRUE))*100, 0),
                                 round((sum(subset(mydatafirst, Time==Quarter-1 & Directorate==d)$Promoter == 5, na.rm=TRUE)/
                                            sum(subset(mydatafirst, Time==Quarter-1 & Directorate==d)$Promoter %in% 1:5, na.rm=TRUE) -
                                            sum(subset(mydatafirst, Time==Quarter-1 & Directorate==d)$Promoter %in% 1:3, na.rm=TRUE)/
                                            sum(subset(mydatafirst, Time==Quarter-1 & Directorate==d)$Promoter %in% 1:5, na.rm=TRUE))*100, 0))
            
            NPSyeardir = round((sum(subset(mydatafirst, Time %in% (Quarter-4):(Quarter-1) & Directorate==d)$Promoter == 5, na.rm=TRUE)/
                                 sum(subset(mydatafirst, Time %in% (Quarter-4):(Quarter-1) & Directorate==d)$Promoter %in% 1:5, na.rm=TRUE) -
                                 sum(subset(mydatafirst, Time %in% (Quarter-4):(Quarter-1) & Directorate==d)$Promoter %in% 1:3, na.rm=TRUE)/
                                 sum(subset(mydatafirst, Time %in% (Quarter-4):(Quarter-1) & Directorate==d)$Promoter %in% 1:5, na.rm=TRUE))*100, 0)
            
            if(NPScheckdir){
                
                NPSdir = round((sum(Subset.P$Promoter == 5, na.rm=TRUE)/
                                 sum(Subset.P$Promoter %in% 1:5, na.rm=TRUE) -
                                 sum(Subset.P$Promoter %in% 1:3, na.rm=TRUE)/
                                 sum(Subset.P$Promoter %in% 1:5, na.rm=TRUE))*100, 0)
                
                if(!is.na(NPSdir)){
                    
                    myCat(c("<p>Net promoter score was ", NPSdir, ifelse(is.na(NPSpreviousdir),
                                        ". This compares with ", paste0(". This compares with ", NPSpreviousdir,
                                        " in the previous quarter and ")), NPSyeardir,
                            " in the previous four quarters.</p>"))
                    
                }
                
            }
            
            myCat(c('<img src= "', graph, '.png" alt = "Stacked barchart" />'))
            
            mybar(PlaceN="Directorate", PlaceC=d, GraphN=1, type=barType)
            
            if(file.exists(paste(file.path(getwd(), "temp", graph, fsep = .Platform$file.sep), ".png", sep=""))){ 
                
                myCat(c('<img src= "', graph, '.png" alt = "Trend barchart" />'))
                
            } 
            
            mybar(PlaceN="Directorate", PlaceC=d, GraphN=2, type=barType)
            
            if(file.exists(paste(file.path(getwd(), "temp", graph, fsep = .Platform$file.sep),
                                 ".png", sep=""))){ 
                
                myCat(c('<img src= "', graph, '.png" alt = "Trend barchart- 2nd" />'))
                
            } 
            
            myCat("<h2> Improve one thing individual comments </h2>")
            
            mycomments(PlaceN="Directorate", PlaceC=d, 1) 
            
            myCat("<h2> Best thing individual comments </h2>")
            
            mycomments(PlaceN="Directorate", PlaceC=d, 2) 
            
            myCat("<h2> Improve one thing summary </h2>")
            
            mytable(PlaceN="Directorate", PlaceC=d, 1) 
            
            myCat("<h2> Best thing summary </h2>")
            
            mytable(PlaceN="Directorate", PlaceC=d, 2) 
            
            myCat("<br>")
            
            dirResponseTable = myResponse(PlaceN="Directorate", PlaceC=d)
            
            myCat("</div")
            
            myCat('<div id="content-right">')
            
            myCat('<h3>Navigation</h3>')
            
            myCat('<ul>')
            
            for (team in teamnames) {
                
                myCat(c("<li><a href=", team, ".html>", as.character(subset(mydatafirst, Time==Quarter)$TeamN
                                                                     [tail(which(team==subset(mydatafirst, Time==Quarter)$TeamC), 1)]), "</a><br></li>"))
                
            }
            
            myCat('</ul>')
            
            myCat("</div>")
            
            myCat("</div>") # report
            
            myCat("</body>")
            
            myCat("</html>")
            
            ##############################################
            ################## team ######################
            ##############################################
            
            for(team in as.numeric(names(table(Subset.P$TeamC)[table(Subset.P$TeamC)>0]))) { 
                
                # team = 26401
                
                currentPath <<- file.path(getwd(), "temp", paste0(team, ".html"), fsep = .Platform$file.sep)
                
                myCat("<html>")
                
                myCat("<head>")
                
                myCat(c("<title> ", as.character(subset(mydatafirst, Time==Quarter)$TeamN[tail(which(team==subset(mydatafirst,
                                                                                                                  Time==Quarter)$TeamC), 1)]), " </title>"))
                
                myCat('<link href="report.css" rel="stylesheet" type="text/css" />')
                
                myCat("</head>")
                
                myCat("<body>")
                
                myCat('<div id="report">') # this tag ends right at the end
                
                myCat('<div id="header">')
                
                myCat('<a href="#"><img src="logo.png" alt="Positive... about change" /></a>')
                
                myCat('<p class="title">')
                
                myCat(c('<strong>Service User and Carer Experience Report for ', tail(barnames, 1), '</strong><br/>'))
                
                myCat(c('<em>Retrieved on:', format(Sys.time(), "%a %b %d %Y"), '</em><br />'))
                
                myCat('</p>')
                
                myCat('</div>')
                
                myCat('<div id="breadcrumb">')
                
                if(!999 %in% funcPlaceC){
                    
                    myCat(c(paste0('<a href="http://feedback.nottinghamshirehealthcare.nhs.uk" title="Back to main site">
                   Back to main site</a> &rsaquo;<a href="index.html">Index</a> &rsaquo;
                   <a href="trust.html">Trust summary</a> &rsaquo; <a href="',
                                   URL, '.html">', name), '</a> &rsaquo; <a href="', gsub("\\s","_", tolower(directorate[d])), '.html">',
                            directorate[d],'</a> &rsaquo; ',
                            as.character(subset(mydatafirst, Time==Quarter)$TeamN[tail(which(team==subset(mydatafirst,
                                                                                                          Time==Quarter)$TeamC), 1)])))
                    
                } else {
                    
                    myCat(c(paste0('<a href="http://feedback.nottinghamshirehealthcare.nhs.uk" title="Back to main site">
                   Back to main site</a> &rsaquo;<a href="index.html">Index</a> &rsaquo;
                   <a href="trust.html">Trust summary</a> &rsaquo;
                       <a href="forensic.html">Forensic services</a>',
                                   '</a> &rsaquo; <a href="', gsub("\\s","_", tolower(directorate[d])), '.html">',
                                   directorate[d],'</a> &rsaquo; ',
                                   as.character(subset(mydatafirst, Time==Quarter)$TeamN[tail(which(team==subset(mydatafirst,
                                                                                                                 Time==Quarter)$TeamC), 1)]))))
                    
                }
                
                myCat('</div>')
                
                myCat(c("<h1> ", as.character(subset(mydatafirst, Time==Quarter)$TeamN[tail(which(team==subset(mydatafirst,
                                                                                                               Time==Quarter)$TeamC), 1)]), " </h1>"))
                
                myCat('<div id="content-left">')
                
                Subset.T = subset(mydatafirst, TeamC==team & Time == Quarter)
                
                stack(PlaceN = "TeamC", PlaceC = team, myLabels = funcLabels)
                
                myCat(c("<p>In ", as.character(subset(mydatafirst, Time==Quarter)$TeamN[tail(which(team==subset(mydatafirst, Time==Quarter)$TeamC), 1)]), " we received ", length(subset(mydatafirst, TeamC==team&Time==Quarter)$Service), " responses. Of these ", length(subset(mydatafirst, TeamC==team&Time==Quarter&SU==1)$Service), " were from carers.</p>"))
                
                if(ncol(dirResponseTable)>4){
                    
                    myCat(c("<p>This is a response rate of ", dirResponseTable$Percent[dirResponseTable$TeamC==team], "%.</p>"))
                    
                }
                
                myCat(c('<img src= "', graph, '.png" alt = "Stacked barchart" />'))
                
                mybar(PlaceN="TeamC", PlaceC=team, GraphN=1, type=barType)
                
                if(file.exists(paste(file.path(getwd(), "temp", graph, fsep = .Platform$file.sep), ".png", sep=""))){ 
                    
                    myCat(c('<img src= "', graph, '.png" alt = "Trend barchart" />'))
                    
                } 
                
                mybar(PlaceN="TeamC", PlaceC=team, GraphN=2, type=barType)
                
                if(file.exists(paste(file.path(getwd(), "temp", graph, fsep = .Platform$file.sep), ".png", sep=""))){ 
                  
                  myCat(c('<img src= "', graph, '.png" alt = "Trend barchart- 2nd" />'))
                  
                }
                
                if(length(Subset.T$Improve[!is.na(Subset.T$Improve)])>0){
                  
                  myCat("<h2> Improve one thing </h2>")
                  
                  for (i in which(!is.na(Subset.T$Improve)==TRUE)){ 
                    
                    vecprint=as.character(Subset.T$Improve[i]) 
                    
                    myCat(c(vecprint, "<br>"))
                    
                  } 
                  
                }
                
                if(length(Subset.T$Best[!is.na(Subset.T$Best)])>0){
                  
                  myCat("<h2> Best thing </h2>")
                  
                  for (i in which(!is.na(Subset.T$Best)==TRUE)){ 
                    
                    vecprint=as.character(Subset.T$Best[i]) 
                    
                    myCat(c(vecprint, "<br>"))
                    
                  }
                  
                }
                
                if(length(Subset.T$PO[!is.na(Subset.T$PO)])>0){
                  
                  myCat("<h2>Patient Opinion</h2>")
                  
                  for (i in which(!is.na(Subset.T$PO)==TRUE)){ 
                    
                    vecprint=as.character(Subset.T$PO[i]) 
                    
                    myCat(c(vecprint, "<br>"))
                    
                  }
                  
                }
                
                if(length(Subset.T$PALS[!is.na(Subset.T$PALS)])>0){
                  
                  myCat("<h2>PALS</h2>")
                  
                  for (i in which(!is.na(Subset.T$PALS)==TRUE)){ 
                    
                    vecprint=as.character(Subset.T$PALS[i]) 
                    
                    myCat(c(vecprint, "<br>"))
                    
                  }
                  
                }
                
                myCat("</div")
                
                myCat('<div id="content-right">')
                
                myCat('<h3>Navigation</h3>')
                
                myCat('<ul>')
                
                for (team in teamnames) {
                    
                    myCat(c("<li><a href=", team, ".html>", as.character(subset(mydatafirst, Time==Quarter)$TeamN
                                                                         [tail(which(team==subset(mydatafirst, Time==Quarter)$TeamC), 1)]), "</a><br></li>"))
                    
                }
                
                myCat('</ul>')
                
                myCat("</div>")
                
                myCat("</div>") # report
                
                myCat("</body>")
                
                myCat("</html>")
                
            } # matches with for (team in...)
            
        } # matches with if.exists(d)
        
    } # matches with for (d in...)
    
} # matches with bigFunction

bigFunction(name = "Rampton hospital", funcPlaceC = 8:12, funcPlaceN = "Directorate",
            URL = "special_f", barType = "main", dirVec = c(8:12), funcLabels = mainlist)

bigFunction(name = "Offender health", funcPlaceC = 999, funcPlaceN = "Directorate",
            URL = "hmp", barType = "HMP", dirVec = c(16), funcLabels = offendermain)

bigFunction(name = "Specialist services (local)", funcPlaceC = c(4, 6, 13, 14), funcPlaceN = "Directorate",
            URL = "special_local", barType = "main", dirVec = c(4, 6, 13, 14), funcLabels=mainlist)

bigFunction(name = "Local services", funcPlaceC = 0, funcPlaceN = "Local", URL = "local", barType = "main",
            dirVec = c(2,7), funcLabels = mainlist)

bigFunction(name = "Forensic services", funcPlaceC = 1, funcPlaceN = "Local", URL = "forensic",
            barType = "main", dirVec = c(3, 5, 15, 16), funcLabels = mainlist)

bigFunction(name = "Health partnerships", funcPlaceC = 2, funcPlaceN = "Local", URL = "hp", barType = "HP",
            dirVec = c(25:31), funcLabels = CHPmain)

### now the index

currentPath <<- file.path(getwd(), "temp", "index.html", fsep = .Platform$file.sep)

cat("<html>", "\n", file = currentPath)

myCat("<head>")

myCat("<title>Index</title>")

myCat('<link href="report.css" rel="stylesheet" type="text/css" />')

myCat("</head>")

myCat("<body>")

myCat('<div id="report">') # this tag ends right at the end

myCat('<div id="header">')

myCat('<a href="#"><img src="logo.png" alt="Positive... about change" /></a>')

myCat('<p class="title">')

myCat(c('<strong>Service User and Carer Experience Report for ', tail(barnames, 1), '</strong><br/>'))

myCat(c('<em>Retrieved on:', format(Sys.time(), "%a %b %d %Y"), '</em><br />'))

myCat('</p>')

myCat('</div>')

myCat('<div id="breadcrumb">')

myCat('<a href="http://feedback.nottinghamshirehealthcare.nhs.uk" title="Back to main site">
                Back to main site</a> &rsaquo;<a href="index.html">Index</a> &rsaquo; ')

myCat('</div>')

myCat("<h1> Index </h1>")

myCat('<ol>')

myCat('<li><a href="trust.html">Trust summary</a><br></li>')

myCat('<li><a href="local.html">Local services</a><br></li>')

myCat('<ol>')

for(d in c(2,7)){
    
    if(file.exists(file.path(getwd(), "temp", paste0(gsub("\\s","_", tolower(directorate[d])), ".html"),
                             fsep = .Platform$file.sep))){
        
        myCat(c('<li><a href="', paste0(gsub("\\s","_", tolower(directorate[d])), ".html"), '">', directorate[d], '</a><br></li>'))
        
    }
    
}

myCat('<li><a href="special_local.html">Specialist services directorate</a><br></li>')

myCat('<ol>')

for(d in c(4, 6, 13, 14)){
    
    if(file.exists(file.path(getwd(), "temp", paste0(gsub("\\s","_", tolower(directorate[d])), ".html"),
                             fsep = .Platform$file.sep))){
        
        myCat(c('<li><a href="', paste0(gsub("\\s","_", tolower(directorate[d])), ".html"), '">', directorate[d], '</a><br></li>'))
        
    }
    
}

myCat('</ol>')

myCat('</ol>')

myCat('<li><a href="forensic.html">Forensic services</a><br></li>')

myCat('<ol>')

for(d in c(3, 5, 15, 16)){
    
    if(file.exists(file.path(getwd(), "temp", paste0(gsub("\\s","_", tolower(directorate[d])), ".html"),
                             fsep = .Platform$file.sep))){
        
        myCat(c('<li><a href="', paste0(gsub("\\s","_", tolower(directorate[d])), ".html"), '">', directorate[d], '</a><br></li>'))
        
    }
    
}

if(file.exists(file.path(getwd(), "temp", "special_f.html",
                         fsep = .Platform$file.sep))){
    
    myCat('<li><a href="special_f.html">Rampton hospital</a><br></li>')
    
    myCat('<ol>')
    
    for(d in 8:12){
        
        if(file.exists(file.path(getwd(), "temp", paste0(gsub("\\s","_", tolower(directorate[d])), ".html"),
                                 fsep = .Platform$file.sep))){
            
            myCat(c('<li><a href="', paste0(gsub("\\s","_", tolower(directorate[d])), ".html"), '">', directorate[d], '</a><br></li>'))
            
        }
        
    }
    
    myCat('</ol>')
    
} # end does special_f.html exist

myCat('</ol>')

myCat('<li><a href="hp.html">Health partnerships</a><br></li>')

myCat('<ol>')

for(d in 25:31){
    
    if(file.exists(file.path(getwd(), "temp", paste0(gsub("\\s","_", tolower(directorate[d])), ".html"),
                             fsep = .Platform$file.sep))){
        
        myCat(c('<li><a href="', paste0(gsub("\\s","_", tolower(directorate[d])), ".html"), '">', directorate[d], '</a><br></li>'))
        
    }
    
}


myCat('</ol>')

myCat('</ol>')

myCat("</div>") # report tag

myCat("</body>")

myCat("</html>")
