.libPaths("D:/soft/r/3.6")#this row only for labs
library(rlang)
library(MASS)
library(fitdistrplus)
library(magrittr)
library(dplyr)
library(lazyeval)
library(parallel)
library(e1071)
library(plotly)
library(ggplot2)
library(triangle)
library(sqldf)
#library(readxl)
# library(knitr)
# library(rmarkdown)
library(simmer)
library(simmer.plot)


##----------------------------------------- 1.  all functions ------------------------------------------------
## add service 
addService<- function  (path,sname,timeDist){
  updatedPath <- seize(path, sname)%>%
    timeout(timeDist) %>%
    release(sname)
  
  return(updatedPath)
}

##addServiceRenge

addServiceRenge<- function  (path, sname,time,outpath,timeWait){
  updatedPath <- renege_in(path,t=timeWait,out = outpath)%>%
    seize(sname)%>%
    renege_abort()%>%
    timeout_from_attribute(time) %>%
    set_attribute(key="assignmentDone",value = 1)%>%
    release(sname)
  return(updatedPath)
}

hairCutCategoryFun<- function(hairCutCategory){
  if (hairCutCategory == 1){
    t<-1
  }
  else if (hairCutCategory == 4){
    t<-2
  }
  return (t)
}

extraProducts<- function(paymentService){
  u1 <- runif(1,0,1)
  u2 <- runif(1,0,1)
  totalCost <- 0
  if (u1 <= (1/3)) {
    totalCost = paymentService + ((27*u2)^(1/3))*100
  }
  else {
    totalCost = paymentService + ((9*u2)^(1/2))*100
  }
  return (totalCost)
}


##----------------------------------------- 2.  all simulation parameters ------------------------------------------------

simulationTime <- 540

##----------------------------------------- 3.  Init Simulation and add all resources  ------------------------------------------------

hairSaloon <- simmer("hairSaloon")%>%
  add_resource("mainH",capacity=1,queue_size=Inf)%>%
  add_resource("so-H",capacity=1,queue_size=Inf)%>%
  add_resource("worker",capacity=1,queue_size=Inf)%>%
  add_resource("secretery",capacity=1,queue_size=Inf)%>%
  add_resource("stajer",capacity=1,queue_size=Inf)%>%
  add_resource("shampooStation",capacity=1,queue_size=Inf)%>%
  add_resource("cutStation",capacity=3,queue_size=Inf)%>%
  add_resource("counter",capacity=1,queue_size=Inf)

##----------------------------------------- 4.  All trajectories, start from main trajectory and add sub-trajectories ABOVE IT it . ------------------------------------------------

ByeBye<-trajectory("ByeBye")%>%
  log_("ByeBye")%>%
  set_attribute(key="hairCutDone",value = 0)

late<-trajectory("late")%>%
  log_("i am late")%>%
  timeout(function() runif(1,3,10)) ##check do this timeout

waitingPointTraj <- trajectory("waitingPointTraj")%>%
  log_("waitingPointTraj")%>%
  timeout(function() runif(1,20,25))

paymentTraj <- trajectory("paymentTraj") %>%
  log_("im in paymentTraj")%>%
  seize("counter") %>%
  seize("secretery") %>%
  timeout(function() runif(1,2,5)) %>%
  release("secretery") %>%
  release("counter")%>%
  seize("worker") %>%
  set_attribute("totalCost",function()extraProducts (get_attribute(hairSaloon,"paymentService")))%>%
  release("worker")

cutAfterColorMainTraj <- trajectory("cutAfterColorMainTraj")%>%
  log_("cutAfterColorMainTraj")%>%
  set_attribute ("paymentService", 510)%>%
  seize("cutStation") %>%
  seize("mainH") %>%
  timeout(function() rnorm(1,25,5)) %>%
  release("mainH")%>%
  release("cutStation") %>%
  branch(option= function() 1 ,continue= FALSE, paymentTraj)

cutAfterColorSecTraj <- trajectory("cutAfterColorSecTraj")%>%
  log_("cutAfterColorSecTraj")%>%
  set_attribute ("paymentService", 400)%>%
  seize("so-H") %>%
  seize("cutStation") %>%
  timeout(function() rnorm(1,35,7)) %>%
  release("so-H")%>%
  release("cutStation") %>%
  branch(option= function() 1 ,continue= FALSE, paymentTraj)

colormaterialsTraj <- trajectory("colormaterialsTraj")%>%
  log_("colormaterialsTraj")%>%
  seize("worker") %>%
  timeout(5) %>%
  release("worker")

dryTraj <- trajectory("dry")%>%
  log_("dryTraj")%>%
  timeout(function() rtriangle(1,5,15,8))


shampooStationTraj <- trajectory("shampooStation")%>%
  log_("shampooStationTraj")%>%
  simmer::select(resources=c("stajer","worker"),  policy=c("first-available") )%>%
  seize_selected(amount = 1) %>%
  seize("shampooStation") %>%
  timeout(function() rnorm(1,5,2)) %>%
  release("shampooStation") %>%
  release_selected(amount = 1)


cutTrajSec <- trajectory("cutTrajSec") %>%
  log_("cutTrajSec")%>%
  set_attribute ("paymentService", 170)%>%
  seize("so-H") %>%
  seize("cutStation") %>%
  timeout(function() runif(1,10,15)) %>%
  release("so-H") %>%
  release("cutStation") %>%
  branch(option= function() 1 ,continue= TRUE, shampooStationTraj)%>%
  seize("so-H") %>%
  seize("cutStation") %>%
  timeout(function() rnorm(1,35,7)) %>%
  release("so-H") %>% 
  release("cutStation") %>%
  branch(option= function() 1 ,continue= TRUE, dryTraj)%>%
  log_("gimurim")%>%
  seize("so-H") %>%
  seize("cutStation") %>%
  timeout(function() runif(1,3,4)) %>%
  release("so-H")%>%
  release("cutStation") %>%
  branch(option= function() 1 ,continue= FALSE, paymentTraj)

cutTrajMain <- trajectory("cutTrajMain") %>%
  log_("TalkAndDryHaircut")%>%
  branch(option=function() rdiscrete (1,c(0.6,0.4),c(0,1)) ,continue = c(FALSE),cutTrajSec)%>% 
  seize("cutStation") %>%
  seize("mainH") %>%
  timeout(function() runif(1,10,15)) %>%
  release("mainH") %>%
  release("cutStation") %>%
  branch(option= function() 1 ,continue= TRUE, shampooStationTraj)%>%
  seize("cutStation") %>%
  seize("mainH") %>%
  timeout(function() rnorm(1,25,5)) %>%
  release("mainH") %>% 
  release("cutStation") %>%
  branch(option= function() 1 ,continue= TRUE, dryTraj)%>%
  log_("gimurim")%>%
  seize("cutStation") %>%
  seize("mainH") %>%
  timeout(function() runif(1,3,4)) %>%
  release("mainH")%>%
  release("cutStation") %>%
  branch(option= function() 1 ,continue= FALSE, paymentTraj)

colorTrajSec <- trajectory("colorTrajSec") %>%
  seize("so-H") %>%
  timeout(function() runif(1,1,3)) %>%
  release("so-H") %>%
  branch(option= function() 1 ,continue= TRUE, colormaterialsTraj)%>%
  seize("so-H") %>%
  seize("cutStation") %>%
  timeout(function() rtriangle(1,10,30,23)) %>%
  release("so-H") %>%
  release("cutStation") %>%
  branch(option= function() 1 ,continue= TRUE, waitingPointTraj)%>%
  branch(option= function() 1 ,continue= TRUE, shampooStationTraj)%>%
  branch(option=function() rdiscrete (1,c(0.88,0.12),c(0,1)) , continue = c(TRUE),cutAfterColorSecTraj)%>% 
  branch(option= function() 1 ,continue= TRUE, dryTraj)%>%
  log_("gimurim")%>%
  seize("so-H") %>%
  seize("cutStation") %>%
  timeout(function() runif(1,3,4)) %>%
  release("so-H")%>%
  release("cutStation") %>%
  branch(option= function() 1 ,continue= FALSE, paymentTraj)

colorTrajMain <- trajectory("colorTrajMain") %>%
  branch(option=function() rdiscrete (1,c(0.6,0.4),c(0,1)) ,continue = c(FALSE),colorTrajSec)%>%
  seize("mainH") %>%
  timeout(function() runif(1,1,3)) %>%
  release("mainH") %>%
  branch(option= function() 1 ,continue= TRUE, colormaterialsTraj)%>%
  seize("cutStation") %>%
  seize("mainH") %>%
  timeout(function() rtriangle(1,7,25,18)) %>%
  release("mainH") %>%
  release("cutStation") %>%
  branch(option= function() 1 ,continue= TRUE, waitingPointTraj)%>%
  branch(option= function() 1 ,continue= TRUE, shampooStationTraj)%>%
  branch(option=function() rdiscrete (1,c(0.88,0.12),c(0,1)) , continue = c(TRUE),cutAfterColorMainTraj)%>% 
  branch(option= function() 1 ,continue= TRUE, dryTraj)%>%
  log_("gimurim")%>%
  seize("cutStation") %>%
  seize("mainH") %>%
  timeout(function() runif(1,3,4)) %>%
  release("mainH")%>%
  release("cutStation") %>%
  branch(option= function() 1 ,continue= FALSE, paymentTraj)


umbraTrajMain <- trajectory("umbraTrajMain") %>%
  seize("mainH")%>%
  timeout(function() runif(1,1,3)) %>%
  release("mainH") %>%
  branch(option= function() 1 ,continue= TRUE, colormaterialsTraj)%>%
  seize("cutStation") %>%
  seize("mainH") %>%
  timeout(function() rtriangle(1,30,35,35)) %>%
  release("mainH") %>%
  release("cutStation") %>%
  branch(option= function() 1 ,continue= TRUE, waitingPointTraj)%>%
  branch(option= function() 1 ,continue= TRUE, shampooStationTraj)%>%
  branch(option= function() 1 ,continue= TRUE, dryTraj)%>%
  log_("gimurim")%>%
  seize("cutStation") %>%
  seize("mainH") %>%
  timeout(function() runif(1,3,4)) %>%
  release("mainH")%>%
  release("cutStation") %>%
  branch(option= function() 1 ,continue= FALSE, paymentTraj)


mixTrajSec <- trajectory("mixTrajSec") %>%
  log_("im in so-h")%>%
  seize("so-H") %>%
  seize("cutStation") %>%
  timeout(function() runif(1,10,15)) %>%
  release("cutStation") %>%
  release("so-H") %>%
  branch(option= function() 1 ,continue= TRUE, colormaterialsTraj)%>%
  seize("so-H") %>%
  seize("cutStation") %>%
  timeout(function() rtriangle(1,10,30,23)) %>%
  release("so-H") %>%
  release("cutStation") %>%
  branch(option= function() 1 ,continue= TRUE, waitingPointTraj)%>%
  set_prioritization(c(1,1,FALSE))%>%
  branch(option= function() 1 ,continue= TRUE, shampooStationTraj)%>%
  set_prioritization(c(0,0,FALSE))%>%
  seize("so-H") %>%
  seize("cutStation") %>%
  timeout(function() rnorm(1,35,7)) %>%
  release("so-H") %>%
  release("cutStation") %>%
  branch(option= function() 1 ,continue= TRUE, dryTraj)%>%
  log_("gimurim")%>%
  seize("so-H") %>%
  seize("cutStation") %>%
  timeout(function() runif(1,3,4)) %>%
  release("so-H")%>%
  release("cutStation") %>%
  branch(option= function() 1 ,continue= FALSE, paymentTraj)


mixTrajMain <- trajectory("mixTrajMain") %>%
  log_("TalkAndDryHaircut")%>%
  branch(option=function() rdiscrete (1,c(0.6,0.4),c(0,1)) ,continue = c(FALSE),mixTrajSec)%>%
  seize("cutStation") %>%
  seize("mainH") %>%
  timeout(function() runif(1,10,15)) %>%
  release("mainH") %>%
  release("cutStation") %>%
  branch(option= function() 1 ,continue= TRUE, colormaterialsTraj)%>%
  seize("cutStation") %>%
  seize("mainH") %>%
  timeout(function() rtriangle(1,7,25,18)) %>%
  release("mainH") %>%
  release("cutStation") %>%
  branch(option= function() 1 ,continue= TRUE, waitingPointTraj)%>%
  set_prioritization(c(1,1,FALSE))%>%
  branch(option= function() 1 ,continue= TRUE, shampooStationTraj)%>%
  set_prioritization(c(0,0,FALSE))%>%
  seize("cutStation") %>%
  seize("mainH") %>%
  timeout(function() rnorm(1,25,5)) %>%
  release("mainH") %>%
  release("cutStation") %>%
  branch(option= function() 1 ,continue= TRUE, dryTraj)%>%
  log_("gimurim")%>%
  seize("cutStation") %>%
  seize("mainH") %>%
  timeout(function() runif(1,3,4)) %>%
  release("mainH") %>%
  release("cutStation") %>%
  branch(option= function() 1 ,continue= FALSE, paymentTraj)


mainTraj <- trajectory("mainTraj") %>%
  log_("im in mainTraj")%>%
  branch(option=function() rdiscrete (1,c(0.9,0.1),c(0,1)) , continue = c(TRUE),late)%>% ##10% late
  addService("secretery",function() runif(1,1,3))%>%
  leave(0.07)%>%
  branch (option=function() get_attribute(hairSaloon,"hairCutCategory"),continue=c(FALSE,FALSE,FALSE,FALSE),cutTrajMain,colorTrajMain,umbraTrajMain,mixTrajMain)

cutTrajAttribute<- trajectory("cutTrajAttribute")%>%
  log_("im in cut traj")%>%
  set_attribute(key="hairCutCategory",value = 1)%>%
  set_attribute ("paymentService", 280)%>%
  set_prioritization( function(){
    x <- runif(1,0,1)
    if (x< 0.85)
      return (c (0,0 ,FALSE)) 
    else
      return (c (2,2 ,FALSE))
  } 
  )%>%
  branch(option= function() 1 ,continue= FALSE, mainTraj)


colorTrajAttribute<- trajectory("colorTrajAttribute")%>%
  log_("im in color traj")%>%
  set_attribute(key="hairCutCategory",value = 2)%>%
  set_attribute ("paymentService", 230)%>%
  set_prioritization( function(){
    x <- runif(1,0,1)
    if (x< 0.85)
      return (c (0,0 ,FALSE)) 
    else
      return (c (2,2 ,FALSE))
  } 
  )%>%
  branch(option= function() 1 ,continue= FALSE, mainTraj)



umbraTrajAttribute<- trajectory("umbraTrajAttribute")%>%
  log_("im in umbra traj")%>%
  set_attribute("hairCutCategory",3)%>%
  set_attribute ("paymentService", 610)%>%
  log_("after Attribute")%>%
  set_prioritization( function(){
    x <- runif(1,0,1)
    if (x< 0.85)
      return (c (0,0 ,FALSE)) 
    else
      return (c (2,2 ,FALSE))
  } 
  )%>%
  log_("after prioriti")%>%
  branch(option= function() 1 ,continue= FALSE, mainTraj)%>%
  log_("after branch")

mixTrajAttribute<- trajectory("mixTrajAttribute")%>%
  log_("im in mix traj")%>%
  set_attribute ("hairCutCategory", 4)%>%
  set_attribute ("paymentService", 510)%>%
  set_prioritization( function(){
    x <- runif(1,0,1)
    if (x< 0.85)
      return (c (0,0 ,FALSE)) 
    else
      return (c (2,2 ,FALSE))
  } 
  )%>%
  branch(option= function() 1 ,continue= FALSE, mainTraj)


make2soTraj<- trajectory("make2soTraj")%>%
  log_("changing from 1 so-H to 2 so-H at 12:00")%>%
  set_capacity("so-H",2)

make1soAndBrakeTraj<- trajectory("make1soAndBrakeTraj")%>%
  log_("changing from 2 so-H to 1 so-H at 14:00, and making brake for 30 minutes")%>%
  set_capacity("so-H",1)%>%
  set_prioritization(c(10,10,FALSE))%>%
  seize("mainH")%>%
  seize("so-H")%>%
  seize("worker")%>%
  seize("secretery")%>%
  seize("stajer")%>%
  seize("shampooStation")%>%
  seize("cutStation")%>%
  seize("counter")%>%
  timeout(30)%>%
  release("mainH")%>%
  release("so-H")%>%
  release("worker")%>%
  release("secretery")%>%
  release("stajer")%>%
  release("shampooStation")%>%
  release("cutStation")%>%
  release("counter")

##--------------------------------------- 5.  All Generators, ALWAYS LAST. ------------------------------------------------

hairSaloon%>%
  add_generator("haircut", cutTrajAttribute, distribution = to(7*60,function() rexp(1,0.8*(0.055))),mon=2)%>%
  add_generator("color", colorTrajAttribute, distribution =to(7*60, function() rexp(1,0.8*(0.0218))),mon=2)%>%
  add_generator("umbra", umbraTrajAttribute, distribution =to(7*60,function() rexp(1,0.8*(1/180))),mon=2)%>%
  add_generator("mix", mixTrajAttribute, distribution =to(7*60,function() rexp(1,0.8*(1/60))),mon=2)%>%
  add_generator("make2so", make2soTraj, at(120))%>%
  add_generator("make1soAndBrake", make1soAndBrakeTraj, at(240))

##----------------------------------------- 6.  reset, run, plots, outputs ------------------------------------------------
#set.seed(490)
#reset(hairSaloon)%>%run(until=simulationTime)

#plot(mainTraj)

#resourceData2 <- get_mon_resources(hairSaloon)
#attributeData2 <- get_mon_attributes(hairSaloon)
#arrivalData3 <- get_mon_arrivals(hairSaloon, ongoing = TRUE)



mm <- mclapply(1:384, function(i) {
  set.seed(456+i)
  reset(hairSaloon)%>%run(until=simulationTime) %>%
    wrap()
})

#fullData<-get_mon_arrivals(mm) # the full data of all replications

#FlowMeanData <- sqldf(
# "select replication, avg(end_time - start_time) as meanFlow
# from fullData
# group by replication")


# one-way t-test 
#test<- t.test(x= FlowMeanData$meanFlow,y=NULL, alternative="two.sided",conf.level=0.95)
#print(test)

##ci<-c(test$conf.int[1],test$conf.int[2])%>%print
#meanFlow <- test$estimate%>%print

#plot(mainTraj)

arrivalData2 <- get_mon_arrivals(mm, ongoing = TRUE)
resourceData2 <- get_mon_resources(mm)
attributeData2 <- get_mon_attributes(mm)
arrivalData2all <- get_mon_arrivals(mm, ongoing =TRUE)



#-------------------------------- ממוצע וסטיית תקן ל3 מדדים --------------------------------

#------------------- מדד זמן ממוצע בתור לאדם ביום ----------------------------

arrivalData1withAvgQueue2<-mutate(arrivalData2, waiting = (end_time - start_time - activity_time))

avgWaitingByreplication2 <- sqldf(
  "select replication,avg(waiting)as AW
  from arrivalData1withAvgQueue2
  where finished= true
  group by replication
  ")

avgWaiting2 <- sqldf(
  "select avg(AW)
  from avgWaitingByreplication2
  ")
print(avgWaiting2)
print(sd(avgWaitingByreplication2$AW))

# one-way t-test 
test<- t.test(x= avgWaitingByreplication2$AW,y=, alternative="two.sided",conf.level=0.7)
print(test)


#--------------------------- אורך תור ממוצע לעמדת תספורת --------------------------------

avgQueueByreplication2 <- sqldf(
  "select replication,avg(queue) as AQ
  from resourceData2
  where resource = 'cutStation'
  group by replication
  ")

avgQueue2 <- sqldf(
  "select avg(AQ)
  from avgQueueByreplication2
  ")
print(avgQueue2)
print(sd(avgQueueByreplication2$AQ))

#  ------------------------------------- אחוז מסיימים יומי -------------------------

howManyArrive2 <- sqldf("select count(DISTINCT name) as CA
                               from arrivalData2all
                              group by replication")
paste("total arival",howManyArrive2)


howManyFinish2 <- sqldf("select COUNT(DISTINCT name) as CA
                               from arrivalData2all
                               where finished=TRUE
                       group by replication")
paste("total finish",howManyFinish2)

finishPrecent2<-howManyFinish2/howManyArrive2
avgFinish2<- sqldf("select avg(CA)
                  from finishPrecent2
                    ")
print(avgFinish2)
sd(finishPrecent2$CA)


