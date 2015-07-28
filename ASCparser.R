## =============================================================================
## Eyelink II ASCII data file parser
## By Christie Haskell, 28-07-2015 (v.1)
## =============================================================================

#Inputs:
#dirASC - the directory where the ascii files are located
#dirPar - the directory where you want the resulting data frame to be saved
#fileName - the filename of the ascii file you wish to parse

#Outputs:
#eyePos - a dataframe of the eye event data for fixations and saccades

ASCparse<-function(dirASC,dirPar,fileName) {
  fileLines<-readLines(paste(dirASC,fileName,sep="")) #Read in the file lines
  
  #Row number of first TRIALID 1
  rowStart<-which(grepl("TRIALID 1", fileLines))[1]
  
  #Remove all header information
  fileLines.nh<-fileLines[c(rowStart:length(fileLines))]
  
  #Remove all samples
  fileLines.nsIndex<-which(!grepl("^1", fileLines.nh))
  fileLines.ns<-fileLines.nh[fileLines.nsIndex]
  
  #Remove all lines containing:
  fileLines.rmIndex1<-which(!grepl("RECCFG", fileLines.ns))
  fileLines.ns1<-fileLines.ns[fileLines.rmIndex1]
  
  fileLines.rmIndex2<-which(!grepl("GAZE_COORDS", fileLines.ns1))
  fileLines.ns2<-fileLines.ns1[fileLines.rmIndex2]
  
  fileLines.rmIndex3<-which(!grepl("MODE", fileLines.ns2))
  fileLines.ns3<-fileLines.ns2[fileLines.rmIndex3]
  
  fileLines.rmIndex4<-which(!grepl("START", fileLines.ns3))
  fileLines.ns4<-fileLines.ns3[fileLines.rmIndex4]
  
  fileLines.rmIndex5<-which(!grepl("PRESCALER", fileLines.ns4))
  fileLines.ns5<-fileLines.ns4[fileLines.rmIndex5]
  
  fileLines.rmIndex6<-which(!grepl("PUPIL", fileLines.ns5))
  fileLines.ns6<-fileLines.ns5[fileLines.rmIndex6]
  
  fileLines.rmIndex7<-which(!grepl("EVENTS", fileLines.ns6))
  fileLines.ns7<-fileLines.ns6[fileLines.rmIndex7]
  
  fileLines.rmIndex8<-which(!grepl("SAMPLES", fileLines.ns7))
  fileLines.ns8<-fileLines.ns7[fileLines.rmIndex8]
  
  fileLines.rmIndex9<-which(!grepl("TRIAL OK", fileLines.ns8))
  fileLines.ns9<-fileLines.ns8[fileLines.rmIndex9]
  
  #Remove \t
  fileLines8<-gsub("\t", " ", fileLines.ns9)
  
  #Create trial id variable
  trialidRow<-which(grepl("TRIALID", fileLines8))
  lastSample<-length(fileLines8)
  nSamples<-trialidRow[2:length(trialidRow)] - trialidRow[1:length(trialidRow)-1]
  nSamples<-c(nSamples,lastSample-trialidRow[length(trialidRow)])
  trialnum<-rep(c(1:25,1:250,1:250),nSamples)
  blocknum<-rep(c(rep(0,25),rep(1,250),rep(2,250)),nSamples)
  
  #Add trialnum to lines
  fileLines.tn<-paste(trialnum,blocknum,fileLines8,sep=" ")
  
  #Start Saccades
  ssaccLinesIndex<-which(grepl("SSACC", fileLines.tn))
  ssaccLines<-fileLines.tn[ssaccLinesIndex]
  ssacc.df<-read.table(text=ssaccLines,header=FALSE)
  names(ssacc.df)<-c("trialnum","blocknum","eventType","eye","time")
  
  #End Saccades
  esaccLinesIndex<-which(grepl("ESACC", fileLines.tn))
  esaccLines<-fileLines.tn[esaccLinesIndex]
  esacc.df<-read.table(text=esaccLines,header=FALSE)
  names(esacc.df)<-c("trialnum","blocknum","eventType","eye","startTime","time","v6","startX","startY","X","Y","v11","v12")
  esacc.df$pupilDim<-"na"
  
  #Saccades
  ssacc.df$X<-esacc.df$startX
  ssacc.df$Y<-esacc.df$startY
  ssacc.df$pupilDim<-"na"
  esacc.df<-esacc.df[c("trialnum","blocknum","eventType","eye","time","X","Y","pupilDim")]
  sacc<-rbind(ssacc.df,esacc.df)
  
  #Start Fixations
  sfixLinesIndex<-which(grepl("SFIX", fileLines.tn))
  sfixLines<-fileLines.tn[sfixLinesIndex]
  sfix.df<-read.table(text=sfixLines,header=FALSE)
  names(sfix.df)<-c("trialnum","blocknum","eventType","eye","time")
  sfix.df$X<-"na"
  sfix.df$Y<-"na"
  sfix.df$pupilDim<-"na"
  
  #End Fixations
  efixLinesIndex<-which(grepl("EFIX", fileLines.tn))
  efixLines<-fileLines.tn[efixLinesIndex]
  efix.df<-read.table(text=efixLines,header=FALSE)
  names(efix.df)<-c("trialnum","blocknum","eventType","eye","startTime","time","duration","X","Y","pupilDim")
  efix.df<-efix.df[c("trialnum","blocknum","eventType","eye","time","X","Y","pupilDim")]
  
  #All events
  eyePos<-rbind(sacc,sfix.df,efix.df)
  eyePos<-eyePos[with(eyePos, order(time)), ]
  eyePos$eventType<-as.character(eyePos$eventType)
  eyePos$eye<-as.character(eyePos$eye)
  eyePos$time<-as.numeric(eyePos$time)
  eyePos$X<-as.numeric(eyePos$X)
  eyePos$Y<-as.numeric(eyePos$Y)
  eyePos$pupilDim<-as.numeric(eyePos$pupilDim)
  
  #Determine x,y of start fixation
  for (i in 2:(length(eyePos$trialnum)-1)){
    if (eyePos$eventType[i]=="SFIX") {
      eyePos$X[i]<-eyePos$X[i-1]
      eyePos$Y[i]<-eyePos$Y[i-1]
    }
  }
  
  #Add userid
  user<-as.numeric(sub("exp.*", "", fileName))
  eyePos$userid<-user
  
  #Add slide label
  msgLinesIndex<-which(grepl("MSG", fileLines.tn))
  msgLines<-fileLines.tn[msgLinesIndex]
  msgLines<-msgLines[1:(length(msgLines)-1)]
  msgLines.df<-read.table(text=msgLines,header=FALSE,fill=TRUE)
  msgLines.df<-msgLines.df[,c(4,5)]
  names(msgLines.df)<-c("time","slide")
  
  #Since fixation is shown from the moment a response is made until cue/stimulus, 
  #the trialid slides are really fixation slides
  msgLines.df<-droplevels(subset(msgLines.df,msgLines.df$slide!="FIX1"))
  
  #Keep meaningful slides
  msgLines.df<-droplevels(subset(msgLines.df,msgLines.df$slide=="TRIALID" |
                                   msgLines.df$slide=="CUE" |
                                   msgLines.df$slide=="STIMULUS" |
                                   msgLines.df$slide=="FIX2" |
                                   msgLines.df$slide=="RESPONSE" |
                                   msgLines.df$slide=="DRIFTCORRECT"))
  
  #Order by time
  msgLines.df$time<-as.numeric(as.character(msgLines.df$time))
  msgLines.df<-msgLines.df[with(msgLines.df, order(time)), ]
  
  #Change TRIALID to FIX1
  eyePos$slide<-rep("na",length(eyePos$userid))
  for (i in 1:(length(eyePos$userid))) {
    temp<-subset(msgLines.df,msgLines.df$time <= eyePos$time[i])$slide
    eyePos$slide[i]<-as.character(temp[length(temp)])
    if (eyePos$slide[i]=="TRIALID") {
      eyePos$slide[i]<-"FIX1"
    }
  }
  
  #Adjust x and y for center at (0,0)
  eyePos$X<-eyePos$X-1024/2
  eyePos$Y<-768/2-eyePos$Y
  
  #Write the data frame as a .csv
  fn_out<-paste(dirPar,as.character(user),"_ASCparsed.csv",sep="")
  write.csv(eyePos,file=fn_out,row.names=FALSE)
}