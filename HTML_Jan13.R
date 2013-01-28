
rm(list=ls()) 

setwd(ifelse(Sys.info()['sysname']=="Windows", "D:\\Dropbox\\R-files\\HTML\\",
             "~/Dropbox/R-files/HTML")) 

exampletable=read.csv("Categories_comments.csv", stringsAsFactors =FALSE) 

mydatafirst = read.csv("FINAL-DATA.csv", na.strings=c("NA", "", 99, 88), stringsAsFactors=FALSE)

counts=read.csv("Counts.csv")

Quarter=15 ### Mark current quarter here 

if(max(mydatafirst$Time, na.rm=TRUE)> Quarter) stop("Quarter value wrong") 

mydatafirst$TeamN=gsub("&", "and", mydatafirst$TeamN) 

mydatafirst$Directorate[mydatafirst$Directorate==1 & !is.na(mydatafirst$Directorate)]=2 

mydatafirst$Imp1 = as.numeric(as.character(mydatafirst$Imp1))

mydatafirst$Best1 = as.numeric(as.character(mydatafirst$Best1))
  
library(tm) 
library(wordcloud) 
require(RColorBrewer) 
library(ggplot2) 
library(reshape) 
library(scales) 
library(xtable)
  
directorate=c("Adult mental health City", "Adult mental health", "Arnold Lodge", "CAMHS", "Low secure + CFS", "Learning disability", "Mental health services for older people", "High secure LD", "High secure MH", "High secure PD", "Peaks", "High secure women's","Substance misuse", "Psychological therapy", "Wathwood", "Offender health", rep(NA, 8), "Bassetlaw", "Mansfield and Ashfield", "Newark and Sherwood", "Nottingham North and East", "Nottingham West", "Rushcliffe", "Specialist services") 

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

mydatafirst$Time=factor(mydatafirst$Time) 

mydatafirst[,8:23][mydatafirst[,8:23]>5]=NA 

mydatafirst[,8:23][mydatafirst[,8:23]==0]=NA 

mydatafirst[,"Promoter"][mydatafirst[,"Promoter"]>5]=NA 

graph<<-1 

d=0 

mydatafirst$Staff=apply(mydatafirst[which(names(mydatafirst)=="Doctor"): which(names(mydatafirst )=="Therapist")], 1, mean, na.rm=TRUE) 

mydatafirst$TeamC=factor(mydatafirst$TeamC) 
  
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

stack=function(m, Trust=0){ 

  graph <<-graph+1 
  
  if(Trust==1){
    
    missnum=apply(m[,c("Service", "Listening", "Communication", "Respect", "InvCare")],
                  2, function(x) sum(!is.na(x))) 
    
  } else {
  
  missnum=apply(m[,c("Service", "Listening", "Communication", "Respect", "InvCare",
                       "InvMed", "Safe", "Goals", "Health", "Doctor", "Nurse", "SW",
                       "Psycho", "Therapist")], 2, function(x) sum(!is.na(x))) 
  
  }
  
  if(sum(missnum>2)<4) return() 
  
    mygraph=melt(lapply(names(missnum[missnum>2]), function(x) prop.table(table(m[,which(names(m)==x)]))*100)) 
  
    mygraph$L1=factor(mygraph$L1, labels=c(names(missnum[missnum>2]))) 
  
    if(d==16){ 
      labels=offendermain   
    } else  labels=mainlist 

    if(d>16){ 
      labels=CHPmain 
    } 
  
  ggplot(mygraph, aes(L1, value, fill=factor(Var.1), order = -Var.1)) + geom_bar(position="fill", stat="identity") +
    ylab("Proportion responding") + opts(axis.text.x=theme_text(angle=90, hjust=1)) + xlab("Question") +
    scale_fill_manual(values=rainbow(5), "Response", limits=c(1:5), breaks=c(5:1),
                      labels=c("Excellent", "Good", "Fair", "Poor", "Very poor")) +
    scale_y_continuous(labels =percent_format()) + guides(fill = guide_legend(reverse = TRUE)) +
    scale_x_discrete(labels=labels[missnum>2]) 
  
  ggsave(paste(file.path(getwd(), "temp", graph, fsep = .Platform$file.sep), ".png", sep=""), width=7, height=5, units="in") 
  
} # stack function  

#################################
######## mybar ##################
#################################
  
mybar=function(Time=1:Quarter, PlaceN, PlaceC, GraphN= 1, type=NULL, custom=NULL){

  graph <<-graph+1
  
  mydata=mydatafirst[mydatafirst[,PlaceN] %in% PlaceC & mydatafirst$Time %in% Time,]
  
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
      width = 1000, res=100, pointsize=12, height = 700)
  
    par(mar=c(8, 4, 4, 2) + 0.1)
  
    barx=barplot(ybar, beside=T, col=rainbow(nrow(ybar)), ylim=c(1,6),xpd=FALSE, yaxt = "n", names.arg=barnames[timedrop])
  
    axis(2, at = 1:5) 
  
    legend("top", legend=unlist(legvars[[type]][GraphN])[namedrop], fill=rainbow(nrow(ybar)), bty="n", ncol = 3 )
  
    mtext(longvec, side=1, at=sapply(1:length(ybar), function(x) (nrow(ybar)+1)*x-(nrow(ybar))/2), line=5, outer=FALSE)

    mtext("Number of responses received in each period", side=1, line=3, outer=FALSE)
  
    invisible(dev.off())
  
  }
                                                                                  
} # matches with function
  
####################################### 
######## wordcloud function ###########  
####################################### 

 # x is local or forensic, y is Improve/ best 

mycloud=function(x, y, Title){  
  
  pal <- brewer.pal(8,"Dark2") 
  
  graph <<- graph + 1 
    
  ##########################################
  ######### while testing- delete !!!! #####
  ##########################################
  
  png(file=paste(file.path(getwd(), "temp", graph, fsep = .Platform$file.sep), ".png", sep=""),
      width = 1000, res=100, pointsize=12, height = 700)
  
  plot(runif(10))
  
  text(6, .5, "Wordcloud in final version!", cex=3)
  
  waste = dev.off()
  
  return()
  
  ##########################################
  ######### end of delete !!!! #############
  ##########################################
    
  if(!file.exists(paste(file.path(getwd(), "temp", graph, fsep = .Platform$file.sep), ".png", sep=""))){ 
  
    mydata=subset(mydatafirst, Local %in% x & Time %in% (Quarter-3):Quarter) 
  
    png(file=paste(file.path(getwd(), "temp", paste0(x,y, collapse=""), fsep = .Platform$file.sep), ".png", sep=""),
        width = 1000, res=100, pointsize=12, height = 700)
    
    if(y==1){ 
    
      mycorpus=Corpus(DataframeSource(data.frame(mydata[-is.na(mydata$Improve),30]))) 
    
    } 
  
    if(y==2){ 
    
      mycorpus=Corpus(DataframeSource(data.frame(mydata[-is.na(mydata$Best),33]))) 
    
    } 
  
    mycorpus <- tm_map(mycorpus, removePunctuation) 
    mycorpus <- tm_map(mycorpus, tolower) 
    mycorpus <- tm_map(mycorpus, function(m) removeWords(m, c(stopwords("english"), "none", "rampton", "ive", "dont", "etc"))) 
  
    tdm <- TermDocumentMatrix(mycorpus) 
    m <- as.matrix(tdm) 
    v <- sort(rowSums(m),decreasing=TRUE) 
    d <- data.frame(word = names(v),freq=v) 
  
    wordcloud(d$word,d$freq, scale=c(2.5,.5), max.words=250, random.order=TRUE,rot.per=.15,
            colors=pal, vfont=c("sans serif","plain")) 
  
    mtext(paste(Title, "-", ifelse(y==1, "Improve one thing", "Best thing")), side=3, line=0, outer=FALSE) 
  
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
    
    mtext(paste("Experience Survey between", finaltext, "based on", ifelse(y==1,
            length(mydata$Improve[!is.na(mydata$Improve)]),
            length(mydata$Best[!is.na(mydata$Best)])), "comments"),
        side=1, line=1, outer=FALSE) 
  
    invisible(dev.off())

  } # matches with if file.exists
  
} # function end 

####################################### 
######## mytable function #############  
####################################### 

mytable=function(x){ 

  if(x==1) { 

    resultstable=prop.table(with(subset(mydata, Time==Quarter), table(factor(Imp1, levels=1:83)))
                         [-length(with(subset(mydata, Time==Quarter), table(factor(Imp1, levels=1:83))))])*100
 
    finaltable=cbind(round(resultstable, 1), exampletable)
 
    names(finaltable)[1]="resultstable"
 
    sumtable=tapply(finaltable$resultstable, finaltable$Super, sum)
 
    finaltable$Sumcategory=NA  

    finaltable$lasttable=round(prop.table(with(subset(mydata, Time %in% (Quarter-4):(Quarter-1)),
                                            table(factor(Imp1, levels=1:83)))
                                       [-length(with(subset(mydata, Time %in% (Quarter-4):(Quarter-1)),
                                                     table(factor(Imp1, levels=1:83))))]), 3)*100  

    addcomments1=sum(with(subset(mydata, Time==Quarter), table(factor(Imp1, levels=1:83)))
                  [-length(with(subset(mydata, Time==Quarter), table(factor(Imp1, levels=1:83))))]) 

    addcomments2=sum(with(subset(mydata, Time %in% (Quarter-4):(Quarter-1)), table(factor(Imp1, levels=1:83)))
                  [-length(with(subset(mydata, Time %in% (Quarter-4):(Quarter-1)), table(factor(Imp1, levels=1:83))))]) 

    } 

  if(x==2) { 

    resultstable=prop.table(with(subset(mydata, Time==Quarter), table(factor(Best1, levels=1:83)))
                         [-length(with(subset(mydata, Time==Quarter), table(factor(Best1, levels=1:83))))])*100
 
    finaltable=cbind(round(resultstable, 1), exampletable)
 
    names(finaltable)[1]="resultstable"
 
    sumtable=tapply(finaltable$resultstable, finaltable$Super, sum)
 
    finaltable$Sumcategory=NA  

    finaltable$lasttable=round(prop.table(with(subset(mydata, Time %in% (Quarter-4):(Quarter-1)), table(factor(Best1, levels=1:83)))[-length(with(subset(mydata, Time %in% (Quarter-4):(Quarter-1)), table(factor(Best1, levels=1:83))))]), 3)*100  

    addcomments1=sum(with(subset(mydata, Time==Quarter), table(factor(Best1, levels=1:83)))[-length(with(subset(mydata, Time==Quarter), table(factor(Best1, levels=1:83))))]) 

    addcomments2=sum(with(subset(mydata, Time %in% (Quarter-4):(Quarter-1)), table(factor(Best1, levels=1:83)))[-length(with(subset(mydata, Time %in% (Quarter-4):(Quarter-1)), table(factor(Best1, levels=1:83))))]) 

  }

 sumlast=tapply(finaltable$lasttable, finaltable$Super, sum)  

 for (i in 1:length(sumtable)) {  
 
     finaltable$Sumcategory[finaltable$Super==names(sumtable[i])]=sumtable[i]  

 }  

 for (i in 1:length(sumtable)) {  
 
     finaltable$lastcategory[finaltable$Super==names(sumtable[i])]=sumlast[i]  

 }  

  finaltable2=finaltable[with(finaltable, order(-Sumcategory, -resultstable)),]  

  realtable=matrix(data=NA, nrow=0, ncol=7) 

  for (i in unique(finaltable2[,3])){  

    realtable=rbind(realtable, matrix(c(NA, "Total", i, NA, finaltable2[head(which(finaltable2[3]==i), 1),5], NA, finaltable2[head(which(finaltable2[3]==i), 1),7]), nrow=1))  

    realtable=rbind(realtable, as.matrix(finaltable2[which(finaltable2[3]==i),]))  

    names(realtable)=names(finaltable2) 

  }

  temptable=realtable[,c(3,2,5,1,7,6)] 

  tempvec=as.numeric(as.character(temptable[,4]))>0 
  
  tempvec[is.na(tempvec)]=TRUE 
  
  finaltable3=temptable[tempvec,] 
  
  finaltable3=data.frame(finaltable3[finaltable3[,3]>0,]) 
  
  finaltable3=finaltable3[!is.na(finaltable3$Super),] 

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

  print(xtable(finaltable3), type="html", append=TRUE, file=currentPath) 

  myCat("</br>") 

} 

####################################### 
######## mycomments function ##########  
####################################### 

mycomments=function(x){ 

  if(x==1){  

    commentstable=subset(mydata, Time==Quarter)[,c("Improve", "Imp1")]
    
    names(commentstable)=c("Improve", "Imp1")
    
  }  

  if(x==2){  

    commentstable=data.frame(as.character(subset(mydata, Time==Quarter)$Best),
                                   as.numeric(as.character(subset(mydata, Time==Quarter)$Best1)))
    
    names(commentstable)=c("Improve", "Imp1")
    
  }

  store=list() 
  
  for(x in 1:length(levels(factor(exampletable$Super)))){ 
      
    store=lappend(store, which(exampletable$Super==levels(factor(exampletable$Super))[x])) 
  
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
} # matches with function

##########################################
##### response rate function #############
##########################################

myResponse = function() {

    responseTab = data.frame("TeamC" = as.numeric(teamnames), "Responses" = teamnumbers)

    finalTab = merge(responseTab, subset(counts, Time==Quarter), by = "TeamC", all.x=TRUE)[,c("TeamC", "TeamN", "Responses", "Contacts")]

    finalTab$Percent = round(finalTab$Responses / finalTab$Contacts * 100, 1)

    finalTab$TeamN = sapply(as.numeric(teamnames), function(x) as.character(subset(mydatafirst, Time==Quarter)$TeamN
                                                                        [tail(which(x==subset(mydatafirst, Time==Quarter)$TeamC), 1)]))
    
    finalTab$Percent[finalTab$Percent>100] = 100
    
    finalTab = finalTab[order(finalTab$Percent, decreasing = TRUE),]
    
    finalTab[4:5][is.na(finalTab[4:5])] = "NK"
    
    print(xtable(finalTab[-1]), type="html", append=TRUE, file=currentPath, include.rownames = FALSE)
    
    return(finalTab)
    
}

##################################################
################## SUMMARY #######################
##################################################

# fix file path

do.call(file.remove, list(list.files(file.path(getwd(), "temp/",
                     fsep = .Platform$file.sep), full.names=TRUE)))

file.copy(file.path(getwd(), "report.css",  fsep = .Platform$file.sep),
          file.path(getwd(), "temp/report.css",  fsep = .Platform$file.sep))

file.copy(file.path(getwd(), "logo.png",  fsep = .Platform$file.sep),
          file.path(getwd(), "temp/logo.png",  fsep = .Platform$file.sep))

currentPath = file.path(getwd(), "temp", "Index.html", fsep = .Platform$file.sep)

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

  myCat('<strong>Service User and Carer Experience Report</strong><br />')

  myCat('<em>January 2013</em><br />')

  myCat('<span>V1.2</span></p>')
    
  myCat('</div>')
    
myCat('<div id="breadcrumb">')
    
  myCat('<a href="index.html">Trust summary</a>')

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

  Subset.B=mydatafirst 

  mycloud(0:2, 1, "Whole Trust") 

  myCat(c("<img src= ", graph, ".png", " width=500</a>"))

  mycloud(0:2, 2, "Whole Trust") 

  myCat(c("<img src= ", graph, ".png", " width=500</a>"))

  stack(Subset.P, 1) 

  myCat(c("<img src= ", graph, ".png", " width=500</a>"))

  mybar(PlaceN="Local", PlaceC=0:2, GraphN=1, type="main")

  if(file.exists(paste(file.path(getwd(), "temp", graph, fsep = .Platform$file.sep), ".png", sep=""))){ 
    
      myCat(c("<img src= ", graph, ".png", " width=500</a>"))
    
  } 

  myCat("<h2> Improve one thing </h2>")

  mytable(1) 

  myCat("<br>")

  myCat("<h2> Best thing </h2>")

  mytable(2)

  myCat("<br>")

myCat("</div>")

myCat('<div id="content-right">')

  myCat('<h2>Navigation</h2>')
    
  myCat('<ul>')

    myCat('<li><a href="Local.html">Local services</a><br></li>')

    myCat('<li><a href="Special.html">Specialist services directorate</a><br></li>')

    myCat('<li><a href="Forensic.html">Forensic services</a><br></li>')

    myCat('<li><a href="SpecialF.html">Special services (Rampton) directorate</a><br></li>')

    myCat('<li><a href="HP.html">Health partnerships</a><br></li>')

  myCat('</ul>')

myCat("</div>")

myCat("</div>") # report tag

myCat("</body>")

myCat("</html>")

########################################## 
##########################################   
######## Local Division section  ######### 
########################################## 
########################################## 

currentPath = file.path(getwd(), "temp", "Local.html", fsep = .Platform$file.sep)

mydata=subset(mydatafirst, Local==0) 

Subset.P=subset(mydatafirst, Time==Quarter&Local==0)

Subset.B=subset(mydatafirst, Local==0)

myCat("<html>")

myCat("<head>")

myCat("<title>Local services</title>")

myCat('<link href="report.css" rel="stylesheet" type="text/css" />')

myCat("</head>")

myCat("<body>")

myCat('<div id="report">') # this tag ends right at the end

myCat('<div id="header">')

  myCat('<a href="#"><img src="logo.png" alt="Positive... about change" /></a>')

  myCat('<p class="title">')

  myCat('<strong>Service User and Carer Experience Report</strong><br />')

  myCat('<em>January 2013</em><br />')

  myCat('<span>V1.2</span></p>')

myCat('</div>')

myCat('<div id="breadcrumb">')

  myCat('<a href="index.html">Trust summary</a> &rsaquo; Local services')

myCat('</div>')

myCat("<h1> Local services division </h1>")

myCat('<div id="content-left">')

  myCat(c("<p>In local services division we received ", length(subset(mydata, Time==Quarter)$Service),
           " responses. Of these ", table(subset(mydata, Time==Quarter)$SU)[2],
           " were from carers. This is a response rate of ",
          round(length(subset(mydata, Time==Quarter)$Service) / sum(subset(counts, Time==Quarter & Division==0)$Contacts, na.rm=TRUE)*100, 1), " %.</p>"))

  myCat(c("<p>This quarter the division had a service quality rating of ",
           round(mean(subset(mydata, Time==Quarter)$Service, na.rm=TRUE)*20),
           " %. This compares with ", round(mean(subset(mydata, Time==Quarter-1)$Service, na.rm=TRUE)*20),
           " % in the previous quarter and ", round(mean(subset(mydata, Time %in% (Quarter-4):(Quarter-1))$Service, na.rm=TRUE)*20),
           " % in the previous four quarters.</p>"))

  myCat(c("<p>Net promoter score was ", round((sum(Subset.P$Promoter == 5, na.rm=TRUE)/sum(Subset.P$Promoter %in% 1:5, na.rm=TRUE)
                                                - sum(Subset.P$Promoter %in% 1:3, na.rm=TRUE)/sum(Subset.P$Promoter %in% 1:5, na.rm=TRUE))*100, 0), "</p>"))

  mycloud(0, 1, "Local services division") 
  
  myCat(c("<img src= ", graph, ".png", " width=500</a><br>", "\n", sep=""))
  
  mycloud(0, 2, "Local services division") 
  
  myCat(c("<img src= ", graph, ".png", " width=500</a><br>", "\n", sep=""))

  stack(Subset.P) 

  myCat(c("<img src= ", graph, ".png", " width=500</a><br>", "\n", sep=""))

  mybar(PlaceN="Local", PlaceC=0, GraphN=1, type="main")
    
  if(file.exists(paste(file.path(getwd(), "temp", graph, fsep = .Platform$file.sep), ".png", sep=""))){ 

      myCat(c("<img src= ", graph, ".png", " width=500</a><br>", "\n", sep=""))

  }
   
  mybar(PlaceN="Local", PlaceC=0, GraphN=2, type="main")

  if(file.exists(paste(file.path(getwd(), "temp", graph, fsep = .Platform$file.sep), ".png", sep=""))){ 
  
    myCat(c("<img src= ", graph, ".png", " width=500</a><br>", "\n", sep=""))
  
  } 

  myCat("<h2> Improve one thing </h2>")
  
  mytable(1) 

  myCat("<br>")

  myCat("<h2> Best thing </h2>")

  mytable(2)

  myCat("<br>")

myCat("</div")

myCat('<div id="content-right">')

  myCat('<h3>Navigation</h3>')

  myCat('<ul>')

    for (d in c(2, 7)) {
    
      myCat(c("<li><a href=", gsub("\\s","_", directorate[d]), ".html>", directorate[d], "</a><br></li>"))
    
    }

  myCat('</ul>')

myCat("</div>")

myCat("</div>") # report

myCat("</body>")
  
myCat("</html>")

###########################################
########### Directorate section ###########
###########################################
 
for (d in c(2, 7)) {
   
  currentPath = file.path(getwd(), "temp", paste0(gsub("\\s","_", directorate[d]), ".html"), fsep = .Platform$file.sep)
    
  mydata=subset(mydatafirst, Directorate==d) 

  Subset.P=subset(mydatafirst, Time==Quarter&Directorate==d)
  
  Subset.B=subset(mydatafirst, Directorate==d) 

  stack(Subset.P) 

  if(file.exists(paste(file.path(getwd(), "temp", graph, fsep = .Platform$file.sep),
                       ".png", sep=""))){ 
   
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
      
      myCat('<strong>Service User and Carer Experience Report</strong><br />')
      
      myCat('<em>January 2013</em><br />')
      
      myCat('<span>V1.2</span></p>')
      
      myCat('</div>')
      
    myCat('<div id="breadcrumb">') # automate
      
      myCat(c('<a href="index.html">Trust summary</a> &rsaquo; <a href="local.html">Local services</a> &rsaquo;', directorate[d]))
    
    myCat('</div>')
      
      myCat(c("<h1> ", directorate[d], " </h1>"))
      
      myCat('<div id="content-left">')

        mydata$sumNA=as.numeric(!is.na(mydata$Service))+ as.numeric(!is.na(mydata$Listening)) +
          as.numeric(!is.na(mydata$Communication))+ as.numeric(!is.na(mydata$Respect))+ as.numeric(!is.na(mydata$InvCare))   
  
        mydata=subset(mydata, sumNA>0)

        help=table(mydata$TeamC, mydata$Time)[,ncol(table(mydata$TeamC, mydata$Time))]

        teamnames=names(which(help>2))
    
        teamnumbers=help[which(help>2)]

        mydata2=subset(mydata, TeamC %in% teamnames)  

        myCat(c("<p>In ", directorate[d], " we received ", length(subset(mydatafirst, Time==Quarter&Directorate==d)$Service),
                " responses. Of these ", table(factor(subset(mydatafirst, Time==Quarter&Directorate==d)$SU, levels=0:1))[2],
                " were from carers. This is a response rate of ", 
                round(length(subset(mydata, Time==Quarter)$Service) / sum(subset(counts, Time==Quarter & Directorate==d)$Contacts, na.rm=TRUE)*100, 1), " %.</p>"))

        myCat(c("<p>This quarter the directorate had a service quality rating of ",
                round(mean(subset(mydata, Time==Quarter)$Service, na.rm=TRUE)*20),
                ". This compares with ", round(mean(subset(mydata, Time==Quarter)$Service, na.rm=TRUE)*20),
                " in the previous quarter and ",
                round(mean(subset(mydata, Time %in% (Quarter-4):(Quarter-1))$Service, na.rm=TRUE)*20),
                "% in the previous four quarters.</p>"))

        myCat(c("<p>Net promoter score was ", round((sum(Subset.P$Promoter == 5, na.rm=TRUE)/
                                                         sum(Subset.P$Promoter %in% 1:5, na.rm=TRUE) -
                                                         sum(Subset.P$Promoter %in% 1:3, na.rm=TRUE)/
                                                         sum(Subset.P$Promoter %in% 1:5, na.rm=TRUE))*100, 0), "%.</p>"))
  
        myCat(c("<img src= ", graph, ".png", " width=500</a>"))

        mybar(PlaceN="Directorate", PlaceC=d, GraphN=1, type="main")

        if(file.exists(paste(file.path(getwd(), "temp", graph, fsep = .Platform$file.sep), ".png", sep=""))){ 
  
          myCat(c("<img src= ", graph, ".png", " width=500</a>"))
  
        } 

        mybar(PlaceN="Directorate", PlaceC=d, GraphN=2, type="main")

        if(file.exists(paste(file.path(getwd(), "temp", graph, fsep = .Platform$file.sep),
                     ".png", sep=""))){ 
  
          myCat(c("<img src= ", graph, ".png", " width=500</a>"))
  
        } 

        myCat("<h2> Improve one thing individual comments </h2>")

        mycomments(1) 
  
        myCat("<h2> Best thing individual comments </h3>")

        mycomments(2) 
  
        myCat("<h2> Improve one thing summary </h2>")
   
        mytable(1) 

        myCat("<h2> Best thing summary </h2>")
    
        mytable(2) 

        myCat("<br>")
    
        dirResponseTable = myResponse()
    
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
    
  for(team in as.numeric(names(table(droplevels(mydata2$TeamC))))) { 
    
    currentPath = file.path(getwd(), "temp", paste0(team, ".html"), fsep = .Platform$file.sep)
    
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
    
      myCat('<strong>Service User and Carer Experience Report</strong><br />')
    
      myCat('<em>January 2013</em><br />')
    
      myCat('<span>V1.2</span></p>')
    
    myCat('</div>')
    
    myCat('<div id="breadcrumb">')
    
      myCat(c('<a href="index.html">Trust summary</a>&rsaquo; <a href="local.html">Local services</a>
            &rsaquo; <a href="', gsub("\\s","_", directorate[d]), '.html">', directorate[d],'</a> &rsaquo; ',
            as.character(subset(mydatafirst, Time==Quarter)$TeamN[tail(which(team==subset(mydatafirst,
                                                                                          Time==Quarter)$TeamC), 1)])))
    
    myCat('</div>')
    
    myCat(c("<h1> ", as.character(subset(mydatafirst, Time==Quarter)$TeamN[tail(which(team==subset(mydatafirst,
                                        Time==Quarter)$TeamC), 1)]), " </h1>"))
    
    myCat('<div id="content-left">')

      Subset.P=subset(mydata, Time==Quarter&TeamC==team)
 
      Subset.B=subset(mydata, TeamC==team)

      stack(Subset.P) 

      myCat(c("<p>In ", as.character(subset(mydatafirst, Time==Quarter)$TeamN[tail(which(team==subset(mydatafirst, Time==Quarter)$TeamC), 1)]), " we received ", length(subset(mydatafirst, TeamC==team&Time==Quarter)$Service), " responses.  This is a response rate of ", dirResponseTable$Percent[dirResponseTable$TeamC==team], " %. 
              Of these ", length(subset(mydatafirst, TeamC==team&Time==Quarter&SU==1)$Service), " were from carers.</p>"))

      myCat(c("<img src= ", graph, ".png", " width=500</a><br>"))

      mybar(PlaceN="TeamC", PlaceC=team, GraphN=1, type="main")

    if(file.exists(paste(file.path(getwd(), "temp", graph, fsep = .Platform$file.sep), ".png", sep=""))){ 
     
      myCat(c("<img src= ", graph, ".png", " width=500</a><br>"))
     
    } 
   
    mybar(PlaceN="TeamC", PlaceC=team, GraphN=2, type="main")
   
    if(file.exists(paste(file.path(getwd(), "temp", graph, fsep = .Platform$file.sep), ".png", sep=""))){ 
     
      myCat(c("<img src= ", graph, ".png", " width=500</a><br>"))
     
    } 
   
    myCat("<h2> Improve one thing </h2>")
 
    for (i in which(!is.na(Subset.P$Improve)==TRUE)){ 

      vecprint=as.character(Subset.P$Improve[i]) 

      myCat(c(vecprint, "<br>"))

    } 
  
    myCat("<h2> Best thing </h2>")
 
    for (i in which(!is.na(Subset.P$Best)==TRUE)){ 

      vecprint=as.character(Subset.P$Best[i]) 

      myCat(c(vecprint, "<br>"))
    
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
