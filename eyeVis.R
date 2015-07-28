## =============================================================================
## Eyelink II Eye Data Visualization
## By Christie Haskell, 28-07-2015 (v.1)
## =============================================================================

#Required pacakges
library(ggplot2)
library(grid)
library(gWidgets)
library(gWidgetstcltk)

#Inputs:
#dfori: data frame of beavhaioural data
#dfeye: data frame of eye data

#Outputs
#The GUI and the plot

eyeVisInt<-function(dfori,dfeye) {
  
  #Modified from gaborPatch in the grt package
  gaborPatch2 <- function (sf, theta = 0, rad = (theta * pi)/180, pc = 1, sigma = 1/6, 
                           psi = 0, grating = c("cosine", "sine"), npoints = 100, trim = 0, 
                           trim.col = 0.5, ...) {
    
    if (length(npoints) == 1) 
      npoints <- rep(npoints, 2)
    if (length(sigma) == 1) 
      sigma <- rep(sigma, 2)
    X <- ((1:npoints[1L])/npoints[1L]) - 0.5
    Xm <- matrix(rep(X, npoints[2L]), npoints[2L])
    Ym <- t(Xm)
    Xt <- Xm * cos(rad) + Ym * sin(rad)
    Yt <- -Xm * sin(rad) + Ym * cos(rad)
    grating <- match.arg(grating)
    if (grating == "cosine") {
      wave <- pc * cos(2 * pi * Xt * sf + psi)
    }
    else {
      wave <- pc * sin(2 * pi * Xt * sf + psi)
    }
    gauss <- exp(-0.5 * ((Xt/sigma[1L])^2 + (Yt/sigma[2L])^2))
    gabor <- wave * gauss
    gabor[gauss < trim] <- (trim.col * 2 - 1)
    return(gabor)
  }
  
  #GUI handler function
  p<-function(...) {
    
    #Modified by the GUI
    user<-as.numeric(svalue(user))
    rew<-svalue(rew)
    cueing<-svalue(cueing)
    
    if (rew=="None") {
      rewC<-"none"
    } else if (rew=="Fine") {
      rewC<-"high"
    } else if (rew=="Coarse") {
      rewC<-"low"
    }
    
    if (cueing=="None") {
      cueingC<-"neutral"
    } else if (cueing=="Valid") {
      cueingC<-"true"
    } else if (cueing=="Invalid") {
      cueingC<-"false"
    }
    
    #List of all trials that meet the conditions set on the GUI
    dplot<-subset(dfeye,dfeye$userid==user & dfeye$reward==rewC & dfeye$validCue==cueingC)
    trials<-unique(dplot$trialnum)
    
    #Trial selected in the GUI
    trialIndex<-as.numeric(svalue(trl))
    trial<-trials[trialIndex]
    
    dplot<-subset(dplot,dplot$trialnum==trial)
    block=unique(dplot$blocknum)
    
    #Which slides to display
    if (svalue(slde)=="Fixation 1") {
      dplot<-subset(dplot,dplot$slide=="FIX1")
    }
    if (svalue(slde)=="Fixation 2") {
      dplot<-subset(dplot,dplot$slide=="FIX2")
    }
    if (svalue(slde)=="Stimulus") {
      dplot<-subset(dplot,dplot$slide=="STIMULUS")
    }
    if (cueing!="None") {
      if (svalue(slde)=="Cue") {
        dplot<-subset(dplot,dplot$slide=="CUE")
      }
    }
    if (svalue(slde)=="Response") {
      dplot<-subset(dplot,dplot$slide=="RESPONSE")
    }
    
    #What was the stimulus position?
    stimPosX<-dfori[which(dfori$userid==user & dfori$blocknum==block & dfori$trialnum==trial),]$stimPosXpix
    stimPosY<-dfori[which(dfori$userid==user & dfori$blocknum==block & dfori$trialnum==trial),]$stimPosYpix
    
    #what was the stimlus orientation?
    stimOri<-dfori[which(dfori$userid==user & dfori$blocknum==block & dfori$trialnum==trial),]$orient
    
    #What did participants responsd?
    respOri<-dfori[which(dfori$userid==user & dfori$blocknum==block & dfori$trialnum==trial),]$judgori*pi/180
    df.resp<-data.frame("x"=c(stimPosX+40*cos(respOri),stimPosX-40*cos(respOri)),"y"=c(stimPosY+40*sin(respOri),stimPosY-40*sin(respOri)))
    
    #eventType colours and sizes
    cols<-c("Start Saccade"="red","End Saccade"="red2","Start Fixation"="yellow","End Fixation"="yellow2")
    shapes<-c("Start Saccade"=17,"End Saccade"=15,"Start Fixation"=17,"End Fixation"=15)
    
    #Order of legend
    dplot$eventType <- factor(dplot$eventType, levels = c("Start Saccade","End Saccade","Start Fixation","End Fixation"))
    
    #Stimulus
    gabPat<-gaborPatch2(4,theta = stimOri,pc = 0.6525,npoints = 500,trim=0,trim.col=NA,grating="sine")
    gabAdj<-gabPat-min(gabPat)
    gabAdj2<-ifelse(gabAdj<=1,gabAdj,1)
    gabIm<-as.raster(gabAdj2)
    g<-rasterGrob(gabIm,interpolate=TRUE)
    
    #Theme settings
    ptheme<-theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  panel.background = element_rect(fill =rgb(-min(gabPat),-min(gabPat),-min(gabPat))),
                  legend.position=c(0,1),
                  legend.justification=c(0,1),
                  legend.background = element_rect(fill=rgb(-min(gabPat),-min(gabPat),-min(gabPat))),
                  legend.key = element_rect(fill=rgb(-min(gabPat),-min(gabPat),-min(gabPat))))
    
    #Legend settings
    pguide<-guides(colour = guide_legend(override.aes = list(size=4)))
    
    #Fixation and trial number
    fix<-geom_point(aes(x=0,y=0),colour="white",size=4.55)
    trialNum<-annotate("text", x = 450, y = 375, label = paste("Trial Number: ",unique(as.character(dplot$trialnum)),sep=""))
    
    #Cue
    if (cueing!="None") {
      #What was the cue position?
      cuePosX<-dfori[which(dfori$userid==user & dfori$blocknum==block & dfori$trialnum==trial),]$cuePosXpix
      cuePosY<-dfori[which(dfori$userid==user & dfori$blocknum==block & dfori$trialnum==trial),]$cuePosYpix
      df.cue<-data.frame("x"=cuePosX,"y"=cuePosY)
      
      pcue<-geom_rect(data=df.cue,aes(NULL,NULL,xmin=x-50,xmax=x+50,ymin=y-50,ymax=y+50),colour="white",alpha=0,size=1.5)
    }
    
    #What event types to plot?
    if (svalue(sacc)=="TRUE") {
      dplot<-subset(dplot,dplot$eventType=="Start Saccade" | dplot$eventType=="End Saccade")
    }
    
    if (svalue(fixate)=="TRUE") {
      dplot<-subset(dplot,dplot$eventType=="Start Fixation" | dplot$eventType=="End Fixation" | dplot$eventType=="Fixation Update")
    }
    
    #Stimulus and response
    pstim<-annotation_custom(grob = g,xmin=stimPosX-40,xmax=stimPosX+40,ymin=stimPosY-40,ymax=stimPosY+40)
    presp<-geom_line(data=df.resp,aes(x=x,y=y),colour="black",size=1) 
    
    #Eye data points
    points<-geom_point(aes(colour=eventType,shape=eventType),size=6,alpha=0.5)
    
    #Eye data path
    plines<-geom_line()
    
    #The base plot
    p<-ggplot(dplot,aes(x=X,y=Y))+ylim(c(-768/2,768/2))+xlim(c(-1024/2,1024/2))+scale_colour_manual(name="Event Type",values = cols)+
      scale_shape_manual(name="Event Type",values = shapes)
    
    #Building the plot based on options selcted on the GUI
    if (svalue(slde)=="Fixation 1") {
      plot<-p+ptheme+fix+pguide
    } else if (svalue(slde)=="Fixation 2") {
      plot<-p+ptheme+fix+pguide
    } else if (svalue(slde)=="Stimulus") {
      plot<-p+pstim+ptheme+fix+pguide
    } else if (svalue(slde)=="Cue") {
      plot<-p+pcue+ptheme+fix+pguide
    } else if (svalue(slde)=="Response") {
      plot<-p+presp+ptheme+fix+pguide
    } else if (svalue(slde)=="All") {
      if (cueing!="None") {
        plot<-p+pcue+pstim+presp+ptheme+fix+pguide
      } else {
        plot<-p+pstim+presp+ptheme+fix+pguide
      }
    }
    
    if (svalue(pth)=="TRUE") {
      plot<-plot+plines
    }
    
    if (svalue(pts)=="FALSE") {
      plot<-plot+points
    } else if (svalue(pts)=="TRUE") {
      plot<-plot
    }
    
    #Print the plot
    print(plot+trialNum)
  }
  
  #Set up of the GUI
  win_ctrls <- gwindow("Plot controls")
  tbl = glayout(container=win_ctrls)
  tbl[1,1]<-"Group:"
  tbl[1,2]<- (grp<-gcombobox(unique(dfeye$group), selected = 1, width=70,editable=FALSE, container=tbl))
  tbl[2,1]<-"Participant: "
  tbl[2,2]<- (user<-gcombobox(character(0), editable=FALSE, width=70, container=tbl, handler=p))
  tbl[3,1]<-"Reward: "
  tbl[3,2]<- (rew<-gcombobox(character(0), editable=FALSE, width=70, container=tbl, handler=p))
  tbl[4,1]<-"Cueing: "
  tbl[4,2]<- (cueing<-gcombobox(c("Invalid","None","Valid"), selected = 1, width=70, editable=FALSE, container=tbl, handler=p))
  tbl[5,1]<- (sacc<-gcheckbox("Saccade Only", checked = FALSE, container=tbl,handler=p))
  tbl[5,2]<- (fixate<-gcheckbox("Fixation Only", checked = FALSE, container=tbl,handler=p))
  tbl[6,1]<- (pth<-gcheckbox("Draw Path", checked = FALSE, container=tbl,handler=p))
  tbl[6,2]<- (pts<-gcheckbox("Remove Points", checked = FALSE, container=tbl,handler=p))
  tbl[7,1]<-"Slide: "
  tbl[7,2]<-(slde<-gradio(c("All","Fixation 1","Cue","Stimulus","Fixation 2","Response"), selected=1, container=tbl,handler=p))
  tbl[8,1]<-"Trials"
  tbl[8,2]<- (prev<-gbutton(text = "Prev", border=TRUE, container = tbl))
  tbl[8,3]<- (trl<-gedit(text = "1", width = 5, handler = p, container = tbl))
  tbl[8,4]<- (nex<-gbutton(text = "Next", border=TRUE, container = tbl))
  
  #Fill the userid and reward combo boxes
  addHandlerChanged(grp, handler=function(...)  {
    users <- unique(subset(dfeye,dfeye$group==svalue(grp))$userid)
    user[] <- users
    svalue(user)<-users[1]
    if (svalue(grp)=="NF") {
      rew[]<-c("None","Fine")
      svalue(rew)<-"None"
    } else if (svalue(grp)=="HF") {
      rew[]<-c("Fine","Coarse")
      svalue(rew)<-"Fine"
    } else if (svalue(grp)=="LF")
      rew[]>=c("Coarse","Fine")
    svalue(rew)<-"Coarse"
  })
  
  #Go back one trial
  addHandlerChanged(prev, handler=function(...)  {
    current<-as.numeric(svalue(trl))
    if (current > 1) {
      newTrl<-current - 1
      svalue(trl)<-newTrl
    }
  })
  
  #Go forward one trial
  addHandlerChanged(nex, handler=function(...)  {
    current<-as.numeric(svalue(trl))
    newTrl<-current + 1
    svalue(trl)<-newTrl
  })
}