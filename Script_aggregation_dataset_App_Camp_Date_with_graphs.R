
dstemp <- ds
ds <- dstemp

library(Hmisc); library(ggplot2); library(caret);

# FUNCTIONS


#sum up detailled data per couple (campaign, App)
groupMeasuresPerCampPerApp <- function(data, subcriteria){
      groupingby <- list(cid = data$CampaignID, aaid = data$AppAdvertisedID, aid = data$AppOriginID, ctry = data$Country)
      subsetFrom <- length(groupingby) + 1;
      subsetTo <- subsetFrom;
      if("Date" %in% colnames(data))
      {
            groupingby$date = data$Date
            subsetFrom <- subsetFrom + 1;
            subsetTo <- subsetFrom + 1;
      }
      print(groupingby)
      if(missing(subcriteria)){
            #groupingby <- list(cid = data$CampaignID, aid = data$AppOriginID, ctry = data$Country)
      }
      else{
            if(!is.vector(subcriteria) || (is.vector(subcriteria) && length(subcriteria) == 1)){
                  groupingby$sub = data[[subcriteria]]#list(cid = data$CampaignID, aid = data$AppOriginID, ctry = data$Country, sub = data[[subcriteria]])
                  subsetFrom <- subsetFrom + 1;
                  subsetTo <- subsetFrom + 1;
            }
            else{
                  #groupingby <- list(cid = data$CampaignID, aid = data$AppOriginID, ctry = data$Country)
                  
                  print(length(subcriteria))
                  for(i in 1:length(subcriteria)){
                        print(subcriteria[i])
                        print(i+subsetFrom-1)
                        groupingby <- c(groupingby, sub = data[ subcriteria[i] ]) #subcriteria[i] =
                        names(groupingby)[i+subsetFrom-1] <- subcriteria[i]
                  }
                  print(names(groupingby))
                  #subsetTo = subsetFrom + length(subcriteria) + 1
                  susbsetTo <- subsetFrom + length(subcriteria)
                  print(subsetTo)
                  subsetFrom <- subsetFrom + length(subcriteria) + 1
                  print(subsetFrom)
                  tmp <- subsetFrom
                  subsetFrom <- subsetTo;
                  subsetTo <- tmp
                  subsetFrom <- 9;
                  subsetTo <- 10
                  #print(groupingby)
            }
      }
      list <- list(
            earnings = aggregate(data$earningsGlob, groupingby, sum),
            displays = aggregate(data$displaysGlob, groupingby, sum),
            clicks = aggregate(data$clicksGlob, groupingby, sum),
            installs = aggregate(data$installsGlob, groupingby, sum),
            leads = aggregate(data$leadsGlob, groupingby, sum),
            sessions = aggregate(data$sessionsGlob, groupingby, sum),
            new_users = aggregate(data$newUsersGlob, groupingby, sum)
      )
      print("herte")
      #print(list)
      #as.data.frame(list)
      group_by <- c("cid", "aaid", "aid", "ctry")
      naming <- c("CampaignID", "AppAdvertisedID", "AppOriginID", "Country", "earnings", "displays", "clicks", "installs", "leads", "sessions", "new_users")
      insertPos <- 4;
      if("Date" %in% colnames(data))
      {
            group_by <- c(group_by, "date")
            naming <- c("CampaignID", "AppAdvertisedID", "AppOriginID", "Country", "Date", "earnings", "displays", "clicks", "installs", "leads", "sessions", "new_users")
            subsetFrom <- subsetFrom+1; subsetTo <- subsetTo+1;
            insertPos <- insertPos + 1;
      }
      subsetSel <- c(subsetFrom, subsetTo);
      print(subsetSel)
      #print(group_by)
      
      merger <- function(group_by){
            merge(x=list$earnings, 
                  y=merge(x=list$displays[,-subsetSel], 
                          y=merge(x=list$clicks[,-subsetSel], 
                                  y=merge(x=list$installs[,-subsetSel], 
                                          y=merge(x=list$leads[,-subsetSel], 
                                                  y=merge(x=list$sessions[,-subsetSel], y=list$new_users[,-subsetSel], by=group_by),
                                                  by=group_by),
                                          by=group_by),
                                  by=group_by),
                          by=group_by),
                  by=group_by)
      }
      
      if(missing(subcriteria))
      {
            res <- merger(group_by);
            names(res) <- naming
      }
      else if(!is.vector(subcriteria) || (is.vector(subcriteria) && length(subcriteria) == 1)){
            print("not vector")
            group_by <- c(group_by, c("sub")) # c("cid", "aid", "ctry", "sub")
            print(group_by)
            res <- merger(group_by);
            print(res)
            ###???groupingby <- list(cid = data$CampaignID, aid = data$AppOriginID, ctry = data$Country, sub = data[[subcriteria]])
            names(res) <- append(naming, subcriteria, insertPos)#c("CampaignID", "AppOriginID", "Country", subcriteria, "earnings", "displays", "clicks", "installs", "leads")
      }
      else{
            print("vector")
            print(subcriteria)
            group_by <- c(group_by, subcriteria)#c(group_by, c("sub"))
            print(group_by)
            #print(row.names(list))
            res <- merger(group_by);
            names(res) <- append(naming, subcriteria, insertPos)
            #names(res) <- ins(
            #  c("CampaignID", "AppOriginID", "Country", "earnings", "displays", "clicks", "installs", "leads"), 
            #  list(subcriteria), pos=c(3)
            #)
            print(names(res))
      }
      cpb <- sapply(as.numeric(res$CampaignID), function(x){mean(data[as.numeric(data$CampaignID)==x,]$CostPerBid)})
      #print(cpb)
      res <- cbind(res, costPerBid = cpb)#data[res$CampaignID == data$CampaignID,]$CostPerBid[[1]])
      #print(res$costPerBid)
      res
}
groupKPIsPerCampPerApp <- function(data, subcriteria){
      msrs <- groupMeasuresPerCampPerApp(data, subcriteria);
      #print(msrs)
      ##print(head(data))
      #print(head(data[which(data$CampaignID == msrs$CampaignID),]$CampaignID[1]))
      msrs <- cbind(msrs, 
                    dispPerSess = ifelse(msrs$displays > 0, msrs$displays / msrs$sessions, NaN),
                    CPM = ifelse(msrs$displays > 0, msrs$earnings / msrs$displays * 1000, NaN),
                    CTR = ifelse(msrs$displays > 0, msrs$clicks / msrs$displays, NaN),
                    CVR = ifelse(msrs$clicks > 0, (msrs$installs + msrs$leads) / msrs$clicks, NaN),
                    CPMnorm = ifelse(msrs$displays > 0, (msrs$earnings / msrs$displays * 1000)/msrs$costPerBid, NaN) #(data[which(data$CampaignID == msrs$CampaignID)[1],]$CostPerBid)
      )
      unique(msrs)
}

baysianPredict <- function(){
      #library(e1071)
      #library(caret)
      dsSplitPerSubCriteriaCPMgrouped <- cbind(dsSplitPerSubCriteria, CPMnorm_ = cut(dsSplitPerSubCriteria$CPMnorm, breaks=-1:6), rm.na=T)#(max(dsSplitPerSubCriteria$CPMnorm)+1)
      #cut(dsSplitPerSubCriteria$CPMnorm, breaks=-1:(max(dsSplitPerSubCriteria$CPMnorm)+1))
      datasplit <- createDataPartition(y=dsSplitPerSubCriteriaCPMgrouped$CPMnorm, list=F, p=.7)
      trainset <- dsSplitPerSubCriteriaCPMgrouped[datasplit,]
      testset <- dsSplitPerSubCriteriaCPMgrouped[-datasplit,]
      modelLDA <- train(CPMnorm_ ~ displays, data=trainset, method='lda')
      plda <- predict(modelLDA, newdata=testset)
}

getAverageCPM <- function(data){
      mean <- 0
      for(i in 1:dim(data)[1]){
            mean <- mean + data[i,]$displays * data[i,]$CPMnorm;
      }
      mean <- mean / sum(data$displays)
      mean
}


prepareDS <- function(ds){
      nonFeaturesCol <- c("ConnectionTypeName", "AppOriginID", "CampaignID", "Country", "AppAdvertisedID", "requestsGlob", "displaysGlob", "clicksGlob", "installsGlob", "leadsGlob", "earningsGlob", "spendingsGlob", "CPM", "CTR", "CVR") #, "CostPerBid"
      
      # define the 'ni' operator
      `%ni%` <- Negate(`%in%`)
      dsPrepared <- subset(ds, select=names(ds) %ni% nonFeaturesCol)
      dsPrepared
}
setDS <- function(csvFile){
      if(is.character(csvFile)){
            ds <- read.csv(csvFile)#'StatsForLaunchingPeriodWithConnType.csv') #('CampaignsFirstStats2.csv')#
      }
      #ds <- subset(ds, select=-c(CountryCode))#AppAdvertisedType, AppOriginType,
      #ds <- na.omit(ds)
      
      # we restrict our analysis to the most active object of the platform
      visibility.top30AppOriginIDs <- as.factor(c(2241 ,2272 ,2459, 977, 2300, 2001 ,2334 ,2332, 2284, 2363, 2458, 2256, 2539, 2320, 2495, 2500 ,1341 ,2508, 2468, 2485, 2523, 2237, 2462 ,2497, 2402, 2257 ,2464, 2452, 2514, 2367))
      visibility.top30CampaignIDs<- as.factor(c(2, 2441, 2401,2443,2453,2129,2033,2114,2258,2091,2448, 2452, 2083, 2093,2249, 2260, 2427, 2084, 2388, 2433))
      #print(summary(ds))
      ds <- subset(ds, 
                   displaysGlob >= 0
                   ## & (leadsGlob + installsGlob) < (clicksGlob * 0.2)
                   ##& clicksGlob < (displaysGlob * 0.2)
                   #& AppOriginID %in% visibility.top30AppOriginIDs
                   #& CampaignID %in% visibility.top30CampaignIDs
      )
      
      # setting categorical factors #
      #transform(ds, CampaignID = as.factor(CampaignID))
      ds$CampaignID <- as.factor(ds$CampaignID)
      ds$AppOriginID <- as.factor(ds$AppOriginID)
      ds$IsWebAd <- as.factor(ds$IsWebAd)
      ds$CampaignCostTypeName <- factor(ds$CampaignCostTypeName, levels=c(
            "CPM",
            "CPC (multiple / campaign)",
            "CPI (unique / campaign)",
            "CPL (multiple / campaign / productID) - any kind of goals reached"
      ))
      if("Date" %in% colnames(ds))
      {
            ds$Date <- as.Date(as.POSIXct(ds$Date, 'GMT'))
      }
      ds$AppAdvertisedID <- as.factor(ds$AppAdvertisedID)
      ds$AppOriginOrientation <- as.factor(ds$AppOriginOrientation)
      ds$AppAdvertisedOrientation <- as.factor(ds$AppAdvertisedOrientation)
      #ds$AppAdvertisedType <- as.factor(ds$AppAdvertisedType)
      #normalise CPM per cost#
      ds <- cbind(ds, CPMnorm = ifelse(ds$CostPerBid > 0, (ds$CPM / ds$CostPerBid), NA))
      ds
}

simpleRegression<- function(data, outcome, criteria){
      LM<- lm(eval(as.symbol(outcome))~eval(as.symbol(criteria)), data)
      plot(data[[criteria]], data[[outcome]], ylim=c(quantile(data[[outcome]], .05,na.rm = T),quantile(data[[outcome]], .95,na.rm = T)),cex.lab = 3,cex.axis=0.5,cex.names=0.5)#,  geom="boxplot"), col=colorRamp(c('red', 'blue'))(0.5) 
      #plot(data[[criteria]], data[[outcome]], ylim=c(0,1))#,  geom="boxplot"), col=colorRamp(c('red', 'blue'))(0.5) 
      
      plot(data[[criteria]], LM$fitted, pch=18, cex=3, col=5, add=TRUE,cex.lab = 3,cex.axis=0.5,cex.names=0.5) #col=colorRamp(c('red', 'blue'))(0.5),
      title(main = outcome , sub = criteria)
      for(i in 1:length(unique(data[[criteria]]))){
            currentMod <- unique(data[[criteria]])[i];
            sel <- data[[criteria]]
            print(sel[sel== currentMod])
            print(min(data[[outcome]]))
            #print(head(data[currentMod,criteria]))
            #text(sel[sel== currentMod], mean(data[[outcome]]), paste('M:',median(data[currentMod,criteria]), sep=":"), size=0.7, col=sample(colorRampPalette(c('#175555', '#d76431'))(20))[i])
            text(sel[sel== currentMod], mean(data[[outcome]]), paste('Tot:',length(sel[sel== currentMod]), sep=":"), col=sample(colorRampPalette(c('#175555', '#d76431'))(20))[i])
      }
      print(LM)
}
multipleRegression <- function(data, outcome, criteria){
      
      LM<- lm(eval(as.symbol(outcome))~., data=subset(data, select=names(data) %ni% c(nonFeaturesCol, names(selection))))
      #plot(data[[criteria]], LM$fitted, type="l", lwd=3, col="red")
      qplot(data[[criteria]], LM$fitted)
      
      # compare prediction and mean
      lapply(split(similarCampaigns, similarCampaigns[criteria]),
             function(x){
                   print(x[1, criteria])
                   print(list( pred=getAverageCPM(x), mean = mean(x$CPMnorm), med = median(x$CPMnorm[x$CPMnorm>0])))
             })
}

## END FUNCTIONS


#feature plot#
featurePlot(x=ds[,c('CampaignTargetTypeName', 'AppAdvertisedCategory', 'AppOriginCategory')], y=ds$CPMnorm, plot="pairs")

ds <- setDS('Full[2300]StatsPerDay.csv')#('AppsStatsPerDaySinceFirstJuly.csv')#('StatsOverFullTimeWithConnType.csv')#('StatsForLaunchingPeriodWithConnType.csv')
# we focus on only 1 appOrigin
ds1app <- ds[ds$AppOriginID == '2300',]

# selection of campaigns being exactly the same
selection=list(
      CampaignTypeName= "Full screen interstitial (in-app)"
      ,CampaignSubTypeName = "[PAID] AdDeals user acquisition campaign [quality users]"
      ,PlatformName = "Windows [Phone/Tablets/PCs]"
      ,AdTypeName="AdDeals Interstitials Full Screen (Native)"
      ,AppAdvertisedCategory = "Games"
      ,AppAdvertisedOrientation = "1"
      ,AppOriginCategory = "Games"
      ,AppOriginOrientation = "2"
      #,CampaignCostTypeName = "CPI (unique / campaign)"
      #,CampaignTargetTypeName = "Web mobile"
      #,ConnectionTypeName = "Ethernet"
      #,WeekDay = "Mond./Tues."
      #,grepl("US", CountrySet) == T
)
selectionNames <- names(selection)

#similarCampaigns <- ds; lapply(selectionNames, function(nameindex){similarCampaigns <- subset(similarCampaigns, similarCampaigns[nameindex]==selection[[nameindex]])}) #ds[x.name]=
similarCampaigns <- subset(ds, 
                           CampaignTypeName== "Full screen interstitial (in-app)" 
                           & CampaignSubTypeName == "[PAID] AdDeals user acquisition campaign [quality users]" 
                           & PlatformName == "Windows [Phone/Tablets/PCs]" 
                           #& AppAdvertisedCategory == "Games" 
                           #& AppOriginCategory == "Games" 
                           & AdTypeName=="AdDeals Interstitials Full Screen (Native)"
                           #& AppAdvertisedOrientation == "1"
                           #& AppOriginOrientation == "2"  
                           #& CampaignCostTypeName == "CPI (unique / campaign)" 
                           #& CampaignTargetTypeName == "Web mobile" 
                           
                           #& ConnectionTypeName == "Ethernet" 
                           #& WeekDay == "Satu./Sund."#"Mond./Tues."
                           & (grepl("US", CountrySet) == T)# || grepl("US", CountrySet) == T)
)# settle a dynamic criteria of study

criteria = c("IsWebAd", "CampaignCostTypeName",  "AppAdvertisedID")# AppAdvertisedOrientation","CampaignTargetTypeName")#c("AppAdvertisedOrientation","CampaignTargetTypeName", "CampaignCostTypeName", "AppOriginCategory" )#"CampaignTargetTypeName"#"CostPerBid"#"WeekDay" #"CampaignTargetTypeName"
#criteria = "IsWebAd" AppAdvertisedOrientation"
##dsSplitPerSubCriteria <- subset(groupKPIsPerCampPerApp(similarCampaigns, criteria),displays>1000)

ds1appSplitPerSubCriteria <- subset(groupKPIsPerCampPerApp(ds1app, criteria),displays>1000)

for(crit in selectionNames){
      out = 'CTR'
      ds1appSplitPerSubCriteria <- subset(groupKPIsPerCampPerApp(ds1app, crit),displays>1000)
      print(ds1appSplitPerSubCriteria[as.character(eval(crit))][1])
      if(dim(unique(ds1appSplitPerSubCriteria[as.character(eval(crit))]))[1] > 1){#as.character(eval(as.symbol(crit)))
            simpleRegression(ds1appSplitPerSubCriteria, out, crit) 
            print('ouououoi')
            ggsave(paste(gsub(' ', '_', paste('Daily boxplot', out,'per', crit)), 'jpg', sep="."))
      } 
} 
function(crit){
      out = 'CTR'
      ds1appSplitPerSubCriteria <- subset(groupKPIsPerCampPerApp(ds1app, crit),displays>1000)
      print(ds1appSplitPerSubCriteria[as.character(eval(crit))][1])
      if(dim(unique(ds1appSplitPerSubCriteria[as.character(eval(crit))]))[1] > 1){#as.character(eval(as.symbol(crit)))
            simpleRegression(ds1appSplitPerSubCriteria, out, crit) 
            print('ouououoi')
            ggsave(paste(gsub(' ', '_', paste('Daily boxplot', out,'per', crit)), 'jpg', sep="."))
      }
}


table <- with(ds1appSplitPerSubCriteria, tapply(CTR, list(CampaignID, Date), FUN=mean))

# seeking for CTR per Campaigns..
ggplot(data = ds1appSplitPerSubCriteria, aes(CTR)) + geom_density(aes(color=Country)) + facet_grid(. ~ CampaignID)
ggplot(data = ds1appSplitPerSubCriteria, aes(CTR, AppAdvertisedID)) + geom_point(aes(color=CampaignID))

ggplot(data = ds1appSplitPerSubCriteria, aes(CTR*100)) +
      scale_color_manual(breaks=unique(ds1appSplitPerSubCriteria$CampaignID), values=c(colorRampPalette(c('red', 'blue', 'yellow'))(10),brewer.pal(n = 5, name = "Dark2"))) +#ggplot(data = ds1appSplitPerSubCriteria, aes(displays)) +
      #scale_colour_continuous(guide = FALSE) +
      #scale_colour_hue(guide = FALSE) 
      scale_fill_manual(c("y:displays/1K", "c:clicks"), values=rep("#222222", times=20), guide=guide_legend(override.aes = list(colour=colorRampPalette(c('red', 'blue', 'yellow'))(30)[1:6]))) +
      geom_histogram(aes(fill="ctr", color=CampaignID)) +
      geom_point(aes(fill=cut(clicks, breaks = 5), y=displays/1000, color=cut(clicks, breaks = 5), show.legend = F), alpha=0.4) + #, color=colorRampPalette(c('red', 'blue'))
      facet_grid(. ~ AppAdvertisedID) +
      labs(x='CTR(%)', y='frequency') +
      #scale_color_discrete(breaks=unique(ds1appSplitPerSubCriteria$CampaignID)) +
      ggtitle('Daily CTR per AppAdvertisedID per Campaign') +
      geom_smooth(aes(y=sessions/median(sessions)), colour='#ff9422', se=T) + #/10000
      geom_smooth(aes(y=new_users/median(new_users)), colour='#422ff9', linetype='dotted') #/100
#+ scale_line_manual(labels = c("NewUsers", "Sessions"), values = c("#422ff9", "#ff9422")) 
#ggsave(paste(gsub(' ', '_', 'Daily CTR per Country per Campaign'), 'jpg', sep="."))

#2
criteria = "AppAdvertisedOrientation"
ds1appSplitPerSubCriteria <- subset(groupKPIsPerCampPerApp(ds1app, criteria),displays>1000)

ggplot(data = ds1appSplitPerSubCriteria, aes(CTR*100)) +
      scale_color_manual(breaks=unique(ds1appSplitPerSubCriteria$CampaignID), values=c(colorRampPalette(c('red', 'blue', 'yellow'))(10),brewer.pal(n = 5, name = "Dark2"))) +#ggplot(data = ds1appSplitPerSubCriteria, aes(displays)) +
      #scale_colour_continuous(guide = FALSE) +
      #scale_colour_hue(guide = FALSE) 
      #scale_fill_manual("displays/1000", values=rep("#222222", times=20), guide=guide_legend(override.aes = list(colour=colorRampPalette(c('red', 'blue', 'yellow'))(30)[1:6]))) +
      geom_histogram(aes(fill=CTR)) +
      geom_bar(aes(y=clicks/100), stat="identity") +
      geom_point(aes( y=displays/1000, color=cut(clicks/100, breaks = 5), show.legend = T), alpha=0.4) + #, color=colorRampPalette(c('red', 'blue'))
      #geom_point(aes( y=clicks/100, color=cut(clicks/100, breaks = 5) , show.legend = F), pch=2, alpha=0.4) + #, color=colorRampPalette(c('red', 'blue'))
      ##(method='lm',colour='#889422', se=T) + #/10000
      facet_grid(. ~ AppAdvertisedOrientation) +
      labs(x='CTR(%)', y='frequency') +
      #scale_color_discrete(breaks=unique(ds1appSplitPerSubCriteria$CampaignID)) +
      ggtitle('Daily CTR per AppAdvertisedOrientation per Campaign') +
      geom_smooth(aes(y=sessions/median(sessions)), colour='#ff9422', linetype='dotted', se=T) + #/10000
      geom_smooth(aes(y=new_users/median(new_users)), colour='#422ff9', linetype='dotted') #/100
#+ scale_line_manual(labels = c("NewUsers", "Sessions"), values = c("#422ff9", "#ff9422")) 
#ggsave(paste(gsub(' ', '_', 'Daily CTR per Country per Campaign'), 'jpg', sep="."))

criteria = c("IsWebAd", "CampaignCostTypeName",  "AppAdvertisedID")# AppAdvertisedOrientation","CampaignTargetTypeName")#c("AppAdvertisedOrientation","CampaignTargetTypeName", "CampaignCostTypeName", "AppOriginCategory" )#"CampaignTargetTypeName"#"CostPerBid"#"WeekDay" #"CampaignTargetTypeName"
ds1appSplitPerSubCriteria <- subset(groupKPIsPerCampPerApp(ds1app, criteria),displays>1000)

ggplot(data = ds1appSplitPerSubCriteria, aes(CTR*100)) + 
      geom_point(aes(y=new_users,color=CampaignCostTypeName), pch=1) +
      #scale_colour_manual(labels = c('App', 'Web'),values=colorRampPalette(c('#ff00ff', '#1c54b4'))(2)) +
      #scale_shape_manual(labels = c('App', 'Web')) +
      geom_smooth(aes(y=new_users,color=IsWebAd))+ 
      geom_point(aes(y=sessions/100,color=IsWebAd), pch=2, alpha=0.6) +
      geom_smooth(aes(y=sessions/100,color=IsWebAd))


#3
ggplot(data = ds1appSplitPerSubCriteria, aes(CTR*100)) +
      #scale_color_manual(breaks=unique(ds1appSplitPerSubCriteria$CampaignID), values=c(colorRampPalette(c('red', 'blue', 'yellow'))(10),brewer.pal(n = 5, name = "Dark2"))) +#ggplot(data = ds1appSplitPerSubCriteria, aes(displays)) +
      #scale_colour_continuous(guide = FALSE) +
      #scale_colour_hue(guide = FALSE) 
      #scale_fill_manual("displays/1000", values=rep("#222222", times=20), guide=guide_legend(override.aes = list(colour=colorRampPalette(c('red', 'blue', 'yellow'))(30)[1:6]))) +
      geom_density(aes(color=IsWebAd)) +
      #geom_point(aes(fill=cut(clicks, breaks = 5), y=cut(clicks/1000, breaks = 5), color=cut(clicks, breaks = 5), show.legend = F), alpha=0.4) + #, color=colorRampPalette(c('red', 'blue'))
      #geom_point(aes(fill=cut(displays, breaks = 5), y=cut(displays/1000, breaks = 5), color=cut(displays, breaks = 5) , show.legend = F), alpha=0.4) + #, color=colorRampPalette(c('red', 'blue'))
      #geom_point(aes(y=IsWebAd)) +
      #geom_smooth(method='lm',colour='#889422', se=T) + #/10000
      facet_grid(CampaignCostTypeName ~ AppAdvertisedID) + #AppAdvertisedID
      labs(x='CTR(%)', y='frequency') +
      #scale_color_discrete(breaks=unique(ds1appSplitPerSubCriteria$CampaignID)) +
      ggtitle('Daily CTR per AppAdvertisedID per Campaign') +
      geom_smooth(aes(y=new_users*100/sessions, colour=new_users), se=T)  #/10000

#4
criteria = "ConnectionTypeName"# AppAdvertisedOrientation","CampaignTargetTypeName")#c("AppAdvertisedOrientation","CampaignTargetTypeName", "CampaignCostTypeName", "AppOriginCategory" )#"CampaignTargetTypeName"#"CostPerBid"#"WeekDay" #"CampaignTargetTypeName"
ds1appSplitPerSubCriteria <- subset(groupKPIsPerCampPerApp(ds1app, criteria),displays>1000)

ggplot(data = ds1appSplitPerSubCriteria) +
      #geom_smooth(aes(x=WeekDay, y=CTR*100)) + 
      geom_point(aes(x=ConnectionTypeName, y=displays/100), color='red')

graph2D <- function(i, adj){
      #params = i : indexInSelectionNames list, adj : smoothing degree
      myselectionNames = c('AppAdvertisedCategory','AppAdvertisedOrientation','AppOriginCategory','AppOriginOrientation','CampaignCostTypeName','CampaignTargetTypeName','ConnectionTypeName','WeekDay')
      critdt <- as.list(unique(ds1appSplitPerSubCriteria[criteria]))
      colors <- brewer.pal(n = length(critdt), name = "Dark2")
      names(colors) <- critdt
      criteria = myselectionNames[i]#"ConnectionTypeName"# AppAdvertisedOrientation","CampaignTargetTypeName")#c("AppAdvertisedOrientation","CampaignTargetTypeName", "CampaignCostTypeName", "AppOriginCategory" )#"CampaignTargetTypeName"#"CostPerBid"#"WeekDay" #"CampaignTargetTypeName"
      ds1appSplitPerSubCriteria <- subset(groupKPIsPerCampPerApp(ds1app, criteria),displays>1000)
      
      getPrct <- function(x){
            print(round(table(ds1appSplitPerSubCriteria[,criteria])[x] / dim(ds1appSplitPerSubCriteria)[1],digits=2))
            round(table(ds1appSplitPerSubCriteria[,criteria])[x] / dim(ds1appSplitPerSubCriteria)[1],digits=2)
      }
      
      #get the median of CTR per criteria
      
      groupedCTRmedian <- function(crit, val){
            #ratios <- c()
                  median(lapply(subset(ds1appSplitPerSubCriteria, crit == as.character(val), select = c('clicks','displays')), function(x){
                        print(x)
                        #ratios <- c(ratios,x['clicks']/x['displays'])
                        x['clicks']/x['displays']
                  })
            )
      }
      
      groupedCTRmedian22 <- function(){
            ratios <- c()
            ratios <- sapply(subset(ds1appSplitPerSubCriteria, criteria == as.character(eval(as.symbol(criteria))), select = c('clicks','displays')), function(x){
                  
                  print(head(ds1appSplitPerSubCriteria))
                  ratios <- c(ratios,x['clicks']/x['displays'])
            })
            median(ratios)
      }
      
      tabaap <- table(ds1appSplitPerSubCriteria[criteria])
      print(tabaap)
      count <- function(x){tabaap[x][1]}
      ggplot(data = ds1appSplitPerSubCriteria) + #breaks=unique(ds1appSplitPerSubCriteria[criteria]),  labels=unique(ds1appSplitPerSubCriteria[criteria]), , guide=guide_legend(override.aes = list(colour=brewer.pal(n = 3, name = "Dark2")))
            facet_grid(AppAdvertisedID ~ .) + #AppAdvertisedID + CampaignID ~ .) +
            #color each grid panel according to AppAdvertisedID  
            #geom_rect(data = unique(ds1appSplitPerSubCriteria[,c('AppAdvertisedID','CampaignID')]),aes(fill = AppAdvertisedID),xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf,alpha = 0.3) +
            #geom_text(c(0.5,0.9)) +
            theme(legend.position="bottom",
                  axis.text=element_text(size = 12),
                  legend.text=element_text(colour="blue", size=10, face="bold"),
                  legend.background = element_rect(fill = "#eeee55"),
                  legend.position = c(0.8, 0.2)) +
            geom_density(kernel = "gaussian", adjust = adj, aes(x=CTR, color=eval(as.symbol(criteria)), fill=eval(as.symbol(criteria)), y=..scaled.., alpha=getPrct(which(names(unique(ds1appSplitPerSubCriteria[criteria]))==criteria)[1]) ),show_guide=TRUE) + #fill=eval(as.symbol(criteria)),
            geom_density(aes(y=..scaled..),color='#e557ae') + 
            geom_bar(stat = 'count', color='#aa88ee') +
            stat_summary(aes(y = CTR*100, group=eval(as.symbol(criteria)), colour=eval(as.symbol(criteria))), fun.y=mean, geom="line") +
            geom_histogram(aes(x=CTR, y = ..ndensity..),color='#e557ae',show_guide=FALSE) +
      scale_fill_manual(name = criteria, labels=unique(ds1appSplitPerSubCriteria[criteria]), values=brewer.pal(n = length(critdt), name = "Dark2")) +#ggplot(data = ds1appSplitPerSubCriteria, aes(displays)) +
      scale_color_manual(name = criteria, labels=paste(sapply(unique(ds1appSplitPerSubCriteria[,criteria]), function(x){ 
                                                paste(paste('|', 
                                                      length(unique(ds1appSplitPerSubCriteria[ds1appSplitPerSubCriteria[criteria] == as.character(x),'AppAdvertisedID'])),
                                                      'x',
                                                      length(unique(ds1appSplitPerSubCriteria[ds1appSplitPerSubCriteria[criteria] == as.character(x),'CampaignID'])), '|', sep=''), 
                                                table(ds1appSplitPerSubCriteria[,criteria])[x], '(', getPrct(x)* 100, '%)') })), values=brewer.pal(n = length(critdt), name = "Dark2")) +#  unique(ds1appSplitPerSubCriteria[criteria])[which(names(unique(ds1appSplitPerSubCriteria[criteria]))==criteria)] ggplot(data = ds1appSplitPerSubCriteria, aes(displays)) +#sapply(unique(ds1appSplitPerSubCriteria[,criteria]), function(x){dim(ds1appSplitPerSubCriteria[criteria ==x][1])})), +
            scale_alpha_continuous(guide = FALSE) +
            scale_size_continuous(guide = FALSE) +
            #geom_text(aes(x=mean(CTR),label=count(AppAdvertisedID)) ,y=0.75, angle=45) +
            labs(y="scaled density") +
            geom_vline(aes(xintercept=groupedCTRmedian(criteria,eval(as.symbol(criteria))), color=eval(as.symbol(criteria)), size=sum(clicks)*10/sum(ds1appSplitPerSubCriteria$clicks)), alpha=.4) +
            geom_vline(aes(xintercept=median(clicks/displays)), color='grey', linetype=3, size=3, alpha=.5) +
            #geom_line(aes(color=eval(as.symbol(criteria))), stat = "hline", yintercept = "mean") +
            
            theme(panel.grid.minor = element_blank(), 
                  panel.grid.major = element_line(color = "gray50", size = 0.5),
                  panel.grid.major.x = element_line(color = "gray30", size = 0.8),
                  panel.background = element_rect(fill= "gray40"),
                  axis.ticks.length = unit(.25, "cm")) +
            ggtitle(paste(ifelse(length(unique(ds1appSplitPerSubCriteria$AppOriginID)) > 1, 'ALL', as.character(unique(ds1appSplitPerSubCriteria$AppOriginID)[1])), ': Daily CTR per', criteria, paste(sep='' ,'(', format(min(ds1appSplitPerSubCriteria$Date), format="%m/%d/%Y"), '-', format(max(ds1appSplitPerSubCriteria$Date), format="%m/%d/%Y"),')') ))
            #guide_legend()#title = selectionNames[i])
}

lmPerCountry <- function(){
      usaData <- dsSplitPerSubCriteria[dsSplitPerSubCriteria$Country=='USA', ];
      partCountry <- createDataPartition(p=0.7,y=usaData$CPMnorm, list =F)
      trainsetCountry <- dsSplitPerSubCriteria[partCountry,];
      testsetCountry <- dsSplitPerSubCriteria[-partCountry,]
      lmCountry <- train(CPMnorm ~ AppAdvertisedOrientation + AppOriginCategory + CampaignCostTypeName, data = trainsetCountry, method='lm')
      predictCountry <- predict(lmCountry, newdata = testsetCountry)
      
      summary(lmCountry)
      lmCountry$finalModel$residuals
      
}
predictPerCountry <- lmPerCountry()
predict(lm, newdata = testset)
#reagregate per cai, aid having nbDiqsplays >  1000 
#dsSplitPerSubCriteria <- subset(groupKPIsPerCampPerApp(similarCampaigns, c(criteria, "AppOriginOrientation")),displays>1000)


ggplot(data=ds1appSplitPerSubCriteria, aes(eval(as.symbol(criteria)), CTR)) + #aes(CPM, CPMnorm)) +
      geom_point(aes(color=AppOriginID, shape=eval(as.symbol(criteria)), size=displays), alpha = 0.6) +
      coord_cartesian(xlim=c(0,4), ylim=c(0,7.5)) +
      facet_wrap(~Country)#CampaignTargetTypeName) 
#+ ggtitle("CampaignTypeName: Full screen interstitial (in-app) / CampaignSubTypeName: [PAID] AdDeals user acquisition campaign [quality users] / PlatformName == Windows [Phone/Tablets/PCs] / AppAdvertisedCategory: Games/ AppOriginCategory: Games / AdTypeName: AdDeals Interstitials Full Screen (Native) / AppAdvertisedOrientation: 1 / AppOriginOrientation: 2 / CampaignCostTypeName: CPI (unique / campaign)") 
#+  facet_wrap(~CampaignID)

dsSplitPerCriteria <- subset(groupKPIsPerCampPerApp(similarCampaigns),displays>1000)
ggplot(data=dsSplitPerCriteria, aes(shape=eval(as.symbol(criteria)), CPMnorm)) #+ geom_point(aes(WeekDay,CPMnorm), pch=5, cex=5)
+ geom_point(aes(color=AppOriginID, size=displays), alpha = 0.6) 
+ geom_smooth(method = "lm")
#cut2(similarCampaigns$CPMnorm, g=5)
# returns the value of predicted CPMnorm with various criteria modalities in column
comparePred <- sapply(split(similarCampaigns, similarCampaigns[criteria]), getAverageCPM)
cor(as.data.frame(comparePred))


var(split(similarCampaigns, similarCampaigns[criteria])[1], split(similarCampaigns, similarCampaigns[criteria])[2])

dataHistpresentation <- function(data){
      par(mfrow=c(1,2))
      #attach(dsSplitPerSubCriteria)
      qplot(dsSplitPerSubCriteria$CPMnorm, ylim=c(0,20), xlim=c(0,25), binwidth=0.1, color=dsSplitPerSubCriteria$CampaignCostTypeName)
      qplot(dsSplitPerSubCriteria$CPM, ylim=c(0,30), xlim=c(0,10), binwidth=0.1, color=dsSplitPerSubCriteria$CampaignCostTypeName)
}


isWeb <- list('Web mobile')
#creating subset to split between WebMobile and others #
dsWeb <- subset(ds, ds$CampaignTargetType %in% isWeb)
dsApp <- subset(ds, !(ds$CampaignTargetType %in% isWeb))

#creating subset per campaignCostType#
dsCPI <- subset(dsApp, grepl('CPI',dsApp$CampaignCostType))
boxplot(data=dsCPI, dsCPI$CPMnorm)
dsCPC <- subset(ds, grepl('CPC',ds$CampaignCostType))
boxplot(data=dsCPC, dsCPC$CPMnorm)
dsCPM <- subset(ds, grepl('CPM',ds$CampaignCostType))
dsCPL <- subset(dsWeb, grepl('CPL',dsWeb$CampaignCostType))

#plot CPM along campaignTargetType#
ggplot(data=ds[with(ds,CPMnorm<10),], aes(CampaignTargetTypeName, CPMnorm)) + geom_point(aes(fill=CTR, shape=AppOriginTypeName), alpha=1/2, size=1.5, pch=1) + labs(title='CPM per (Campaign/AppOrigin) since February having nbDisplays > 3000')

# aggregate dipslays ans earnings for every combination#
displaysGlobPerCampPerApp = aggregate(displaysGlob ~ campaignID + appOriginID, FUN = sum, data=ds)
earningsGlobPerCampPerApp = aggregate(earningsGlob ~ campaignID + appOriginID, FUN = sum, data=ds)
calculCPMGlobPerCampPerApp <- function(rawdata){aggregate(earningsGlob ~ campaignID + appOriginID, FUN=function(x){ifelse(x$CostPerBid > 0, (x$CPM / x$CostPerBid), NA)}, data=rawdata)}
cpmsGlob <- calculCPMGlobPerCampPerApp(similarCampaigns)



#CPM per (Campaign/AppOrigin) since February having nbDisplays > 3000#
ggplot(data=ds[with(ds,CPMnorm<10 && ConnectionTypeName != 'Unknown'),], aes(abbreviate(CampaignCostTypeName, 12), CPMnorm)) + geom_point(aes(shape=AppAdvertisedypeName, color=CostPerBid, size=earningsGlob), alpha=1/2, pch=1) + 
      labs(x='CampaignCostType', title='CPM per (Campaign/AppOrigin) since February having nbDisplays > 3000') + geom_smooth(method='lm') + 
      facet_grid(ConnectionTypeName~CampaignTargetTypeName) + coord_cartesian(ylim=c(0,5))

# compute kmodes and assign each combinaison to cluster#
kmodes <- kmodes(data=na.omit(subset(ds, select = -c(apporiginID, campaignID, AppAdvertisedID, CTR, CVR))), modes=4, iter.max=20, weighted=F)
ds <- cbind(ds,kmodes$cluster)
names(ds)[length(ds)] <- 'cluster';
split(ds, ds$cluster)[1]


# display CPM per cluster over various dimensions #
qplot(x=cluster, y=CPM, data=ds, color=AppAdvertisedCategory, shape=CampaignTargetTypeName, ylim=c(0,10))

# tracing the plot of displays = f(AppAdvertisedCategory, AppOriginCategory)#
catVScat <-qplot(data=ds, x=abbreviate(AppAdvertisedCategory_, 7), y=AppOriginCategory_, size=installsGlob, color=displaysGlob, xlab='promotedApp');
trainer <- createDataPartition(y=t$CPM, p=0.75, list=F) #displaysGlob#
trainingset <- t[trainer,]
train <- train(CPM~., data=trainingset, method='glm'); # ~ AppOriginType + AppAdvertisedType#
predict(newdata=t[-trainer, ], train)

library('klaR');

qplot(geom='density', x=CPM, data=ds ,xlim=c(0,4), color=CampaignTargetTypeName);
qplot(x=cluster, y=CPM, data=ds, color=AppOriginCategory ,ylim=c(0,20), shape=CampaignTargetTypeName)

drawnet <- function(dt){
      dsred <- merge(aggregate(dt$clicksGlob, list(#Date=ds$Date,
             From=dt$AppAdvertisedID, To=dt$AppOriginID,Thru=dt$CampaignID), sum),aggregate(dt$displaysGlob, list(#Date=ds$Date, 
                   From=dt$AppAdvertisedID, To=dt$AppOriginID,Thru=dt$CampaignID), sum), c( 'From', 'To', 'Thru')) #'Date',
      names(dsred)[c(4,5)] <- c('clicks', 'displays')
      #net <- graph_from_data_frame(d=data.frame(subset(ds,select=c(AppAdvertisedID,AppOriginID, CampaignID, CountrySet_))), vertices=data.frame(unique(c(levels(ds$AppOriginID), levels(ds$AppAdvertisedID)))), directed=T) 
      net <- graph_from_data_frame(d=data.frame(subset(dsred,select=c(From,To, Thru))), vertices=data.frame(unique(c(levels(dsred$From),levels(dsred$To)))), directed=T) 
      plot(net, layout=layout_with_fr, edge.arrow.size=.4,edge.label=E(net)$Thru, vertex.color="#55442a", vertex.frame.color="#ffffff", edge.color="#8a5b77", edge.label.cex=.4)
      }
net1app <- drawnet(ds1app)
