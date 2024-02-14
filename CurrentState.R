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
#avg queue per resource
avgQueue <- function(time, queueLength, simTime){
  Lavg = 0;
  L = queueLength[1];
  Tnow = time[1];
  Llast = time[1];
  TL = 0;
  Tmax = simTime;
  if (length(time) == length(queueLength)){
    for (i in 2:length(time)){
      if(queueLength[i] != queueLength[i-1]){
        Tnow = time[i];
        TL = TL+L*(Tnow-Llast);
        L = queueLength[i];
        Llast = Tnow;
      }#if
    }#for
  }#end if
  TL=TL+L*(Tmax-Llast);
  Lavg = TL/Tmax;
  return (Lavg);
}


addService <-	function  (trajectory, sname, timeDist){
  seize(trajectory, sname)
  timeout(trajectory, timeDist)
  release(trajectory, sname)
  return(trajectory)
}


## function that check what is tired level for trainer
tired_level <- function () { ### reverse transform
  
  x <- runif (1,0,1)
  ans <- 0
  if (x>= 0 && x< 1/3){
    ans <- ((3*x)/8)^(1/3)
  }
  
  else if (x>=1/3 && x< 2/3){
    ans <-((1/3) + (x/2))}
  
  else if (x>= 2/3 && x<=1) {
    ans <- ((-6+(12-12*x)^0.5)/(-6))
  }
  return (ans)
}


## function that check if woman training is not tired or  not finish 4 device -- rollback
woman_need_go_back <- function (){
  tired_level <- get_attribute(sport_center,"parsonly_tried_level")
  floor <- get_attribute(sport_center,"floor")
  cascading_parallel <-  get_attribute(sport_center,"cascading_parallel")
  rafter <-  get_attribute(sport_center,"rafter")
  jumping <- get_attribute(sport_center,"jumping")
  use_device <- floor+cascading_parallel+rafter+jumping # counter check many device woman did
  
  ans <- TRUE
  if(use_device == 4 ||tired_level>=2.4 ){  
    ans <- FALSE
  }
  return (ans)
}

## function that check if man training is not tired or  not finish 6 device -- rollback
man_need_go_back <- function (){
  tired_level <- get_attribute(sport_center,"parsonly_tried_level")
  
  parallel <- get_attribute(sport_center,"parallel")
  rings <- get_attribute(sport_center,"rings")
  horizontal_bar <- get_attribute(sport_center,"horizontal_bar")
  pommel_horse <- get_attribute(sport_center,"pommel_horse")
  floor <- get_attribute(sport_center,"floor")
  jumping <- get_attribute(sport_center,"jumping")
  use_device <-  parallel+rings+horizontal_bar+pommel_horse+floor+jumping  # counter check many device man did
  ans <- TRUE
  
  if(use_device == 6 || tired_level>=2.9 ){
    ans <- FALSE
  }
  return (ans)
}


### function that calculate the probability for each countinue trajectory for woman
prob_woman <- function (){
  floor <- get_attribute(sport_center,"floor")
  cascading_parallel <- get_attribute(sport_center,"cascading_parallel")
  rafter <- get_attribute(sport_center,"rafter")
  jumping <- get_attribute(sport_center,"jumping")
  use_device <- floor+cascading_parallel+rafter+jumping 
  
  temp <- c(1,floor,cascading_parallel,rafter,jumping)
  ans <- c()
  for (i in 1:5){
    if (temp[i] == 1){
      ans[i] = 0
    }
    if(temp[i] == 0){
      ans[i] =(1/(4-use_device)) 
    }
  }
  return (ans)
}

### function that calculate the probability for each countinue trajectory for man
prob_man <- function (){
  parallel <- get_attribute(sport_center,"parallel")
  rings <- get_attribute(sport_center,"rings")
  horizontal_bar <- get_attribute(sport_center,"horizontal_bar")
  pommel_horse <- get_attribute(sport_center,"pommel_horse")
  floor <- get_attribute(sport_center,"floor")
  jumping <- get_attribute(sport_center,"jumping")
  use_device <-  parallel+rings+horizontal_bar+pommel_horse+floor+jumping
  
  temp <- c(1,parallel,rings,horizontal_bar,pommel_horse,floor,jumping)
  ans <- c()
  for (i in 1:7){
    if (temp[i] == 1){
      ans[i] = 0
    }
    if(temp[i] == 0){
      ans[i] =(1/(6-use_device)) 
    }
  }
  return (ans)
}

###function that define priority by parsonly_tried_level -- woman
set_priority_woman <-   function(){
  n <- get_attribute(sport_center,"parsonly_tried_level") ## defines a LIFO priority
  if (n>2.4){
    ans <- (c(n,n,FALSE))}
  else {
    ans <- c(n,2.4,FALSE)}
  return (ans)}

###function that define priority by parsonly_tried_level -- man
set_priority_man <-   function(){
  n <- get_attribute(sport_center,"parsonly_tried_level") ## defines a LIFO priority
  if (n>2.9){
    ans <- (c(n,n,FALSE))}
  else {
    ans <- c(n,2.9,FALSE)}
  return (ans)}


##function that define the max time for waiting to the lecture
waiting_time <- function (enve){
  ans <- 0
  time <-now(enve) 
  while(time>60){
    time <- time-60
  }
  ans <- 60-time
  return(ans)
}
## function that check if the trainer has unseen video
check_about_see_video <- function(){
  if (get_attribute(sport_center,"unseen_video")>0){
    ans <-c(0,1)
  }
  else {
    ans <- c(1,0)
  }
  return(ans)
}

#trimmed norm
trimmedNorm<-function(mu,sd){
  while(TRUE){
    sample<-rnorm(1,mu,sd)
    if (sample>0)
      return (sample)
  }
}

##----------------------------------------- 2.  all simulation parameters ------------------------------------------------
number<-30  ## number of replication
simulationTime <- 14*60
video_schedule_capacity <- schedule(c(0,120,420),c(0,2,2), period = Inf)
dietician_schedule_capacity <- schedule(c(0,120,420),c(0,1,1), period = Inf)
physiotherapist_schedule_capacity <- schedule(c(0,120,360,420,600), c(0,2,5,5,3),period = Inf )
schedule_size <- schedule(c(0,120), c(0,Inf), period = Inf)
wait_capacity <- schedule(c(0,120), c(0,Inf), period = Inf)

##----------------------------------------- 3.  Init Simulation and add all resources  ------------------------------------

sport_center<- simmer("sport_center")%>%
  add_resource("video_room_1",capacity=video_schedule_capacity,queue_size=schedule_size)%>%
  add_resource("video_room_2",capacity=video_schedule_capacity,queue_size=schedule_size)%>%
  add_resource("video_room_3",capacity=video_schedule_capacity,queue_size=schedule_size)%>%
  add_resource("video_room_4",capacity=video_schedule_capacity,queue_size=schedule_size)%>%
  add_resource("video_room_5",capacity=video_schedule_capacity,queue_size=schedule_size)%>%
  add_resource("dietician1",capacity=dietician_schedule_capacity,queue_size=schedule_size)%>%
  add_resource("dietician2",capacity=dietician_schedule_capacity,queue_size=schedule_size)%>%
  add_resource("physiotherapist",capacity=physiotherapist_schedule_capacity,queue_size=schedule_size)%>%
  add_resource("man_changing_room",capacity=20,queue_size=Inf)%>%
  add_resource("woman_changing_room",capacity=20,queue_size=Inf)%>%
  add_resource("man_shower",capacity=5,queue_size=Inf)%>%
  add_resource("woman_shower",capacity=5,queue_size=Inf)%>%
  add_resource("rafter1",capacity=1,queue_size=Inf)%>%
  add_resource("rafter2",capacity=1,queue_size=Inf)%>%
  add_resource("floor",capacity=1,queue_size=Inf)%>%
  add_resource("parallel",capacity=1,queue_size=Inf)%>%
  add_resource("cascading_parallel",capacity=1,queue_size=Inf)%>%
  add_resource("rings",capacity=1,queue_size=Inf)%>%
  add_resource("pommel_horse",capacity=1,queue_size=Inf)%>%
  add_resource("horizontal_bar",capacity=1,queue_size=Inf)%>%
  add_resource("jumping1",capacity=1,queue_size=Inf)%>%
  add_resource("jumping2",capacity=1,queue_size=Inf)%>%
  add_resource("wait_for_workers",capacity=wait_capacity,queue_size=Inf)

##--------------------------------------------4.Global Attribute  initialize--------------------------------------------------------------------------------------------------------
  add_global(sport_center,"breaktime",0 ) 
  add_global(sport_center,"last_capactiy_change",180)

##----------------------------------------- 5.  All trajectories, start from main trajectory and add sub-trajectories ABOVE IT it . ------------------------------------------------

#Devices trajectories

floor_line<-trajectory("floor_line")%>%
  set_attribute(keys = "floor", values = 1) %>%
  addService(sname = "floor", function() trimmedNorm (5,1.7))%>%
  set_attribute(keys = "parsonly_tried_level",function()get_attribute(sport_center,"parsonly_tried_level")
                + tired_level() )%>% ## update parsonly tried level
  set_attribute("unseen_video",function()get_attribute(sport_center,"unseen_video") + 1)


cascading_parallel_line<-trajectory("cascading_parallel_line")%>% 
  set_attribute(keys = "cascading_parallel", values = 1) %>%
  addService(sname = "cascading_parallel", function() trimmedNorm (5,1.7))%>%
  set_attribute(keys = "parsonly_tried_level",function() get_attribute(sport_center,"parsonly_tried_level")
                +tired_level() )%>% ## update parsonly tried level
  set_attribute("unseen_video",function()get_attribute(sport_center,"unseen_video") + 1)


rafter_line<-trajectory("rafter_line")%>%
  set_attribute(keys = "rafter", values = 1) %>%
  simmer::select(resources=c("rafter1","rafter2"),policy=c("shortest-queue"))%>% 
  seize_selected(amount = 1)%>%
  timeout (function() trimmedNorm (5,1.7))  %>%
  release_selected(amount = 1)  %>%
  set_attribute( "parsonly_tried_level",function() get_attribute(sport_center,"parsonly_tried_level") 
                 +tired_level() )%>% ## update parsonly tried level
  set_attribute("unseen_video",function()get_attribute(sport_center,"unseen_video") + 1)


jumping_line<-trajectory("jumping_line")%>% 
  set_attribute(keys = "jumping", values = 1) %>%
  simmer::select(resources=c("jumping1","jumping2"),policy=c("shortest-queue"))%>% 
  seize_selected(amount = 1)%>%
  timeout (function() trimmedNorm (5,1.7))  %>%
  release_selected(amount = 1) %>%
  set_attribute(keys = "parsonly_tried_level",function()get_attribute(sport_center,"parsonly_tried_level")
                + tired_level() )%>% ## update parsonly tried level
  set_attribute("unseen_video",function()get_attribute(sport_center,"unseen_video") + 1)


parallel_line<-trajectory("parallel_line")%>% 
  set_attribute(keys = "parallel", values = 1) %>%
  addService(sname = "parallel", function() trimmedNorm (5,1.7))%>%
  set_attribute(keys = "parsonly_tried_level",function() get_attribute(sport_center,"parsonly_tried_level") 
                +tired_level() )%>% ## update parsonly tried level
  set_attribute("unseen_video",function()get_attribute(sport_center,"unseen_video") + 1)


rings_line<-trajectory("rings_line")%>% 
  set_attribute(keys = "rings", values = 1) %>%
  addService(sname = "rings", function() trimmedNorm (5,1.7))%>%
  set_attribute(keys = "parsonly_tried_level",function() get_attribute(sport_center,"parsonly_tried_level") 
                +tired_level() )%>% ## update parsonly tried level
  set_attribute("unseen_video",function()get_attribute(sport_center,"unseen_video") +1 )

horizontal_bar_line<-trajectory("horizontal_bar_line")%>% 
  set_attribute(keys = "horizontal_bar", values = 1) %>%
  addService(sname = "horizontal_bar", function() trimmedNorm (5,1.7))%>%
  set_attribute(keys = "parsonly_tried_level",function() get_attribute(sport_center,"parsonly_tried_level") 
                +tired_level() )%>% ## update parsonly tried level
  set_attribute("unseen_video",function()get_attribute(sport_center,"unseen_video") + 1)


pommel_horse_line<-trajectory("pommel_horse_line")%>% 
  set_attribute(keys = "pommel_horse", values = 1) %>%
  addService(sname = "pommel_horse", function() trimmedNorm (5,1.7))%>%
  set_attribute(keys = "parsonly_tried_level",function() get_attribute(sport_center,"parsonly_tried_level") 
                +tired_level() )%>% ## update parsonly tried level
  set_attribute("unseen_video",function()get_attribute(sport_center,"unseen_video") + 1)


#the traj of professionals

#video room
video_available <- trajectory("video_available")%>%
  timeout(function() trimmedNorm (3*(get_attribute(sport_center,"unseen_video")),0.75*get_attribute(sport_center,"unseen_video")))%>%
  release_selected()%>%
  set_attribute("unseen_video",0)  #when he succeed to get into the video room - change to 0

#traj for the people that come into the video room late- when they finish before 8
late_video_available <- trajectory("late_video_available")%>%
  seize(resource = "wait_for_workers", amount = 1)%>%
  release(resource = "wait_for_workers",amount = 1)%>%
  simmer::select(function()paste0("video_room_",get_attribute(sport_center,"video_room")))%>%
  seize_selected(amount=1,id=0,continue = c(TRUE),post.seize =video_available)

#traj for the people that wait till the video training come 
video_wait_line <- trajectory("video_wait_line")

#dietician 
lecture_line <- trajectory("lecture_line")%>% 
  #people that come to the lecture wait maximum 1 hour, if 10 people didnt come
  batch(n=10, timeout=function()waiting_time(sport_center), permanent = FALSE , name = ' ')%>%
  simmer::select(resources=c("dietician1","dietician2"),policy=c("shortest-queue")) %>%
  ##the trainer go to the lecture that have the shortest queue
  seize_selected()%>%
  timeout(function() runif(1,30,40))%>%
  release_selected()%>%
  separate()


#main traj for women

woman_line<-trajectory("woman_line")%>%
  set_attribute(keys = c("floor","cascading_parallel","rafter","jumping","parsonly_tried_level", "video_room", "lecture", "unseen_video"),
                values = c(0,0,0,0,0,0,0,0))%>%
  set_attribute("video_room" , function () {rdiscrete (1 ,c(0.2,0.2,0.2,0.2,0.2), c(1,2,3,4,5))} )%>%#define the video room for all day
  addService("woman_changing_room", function()runif (1,3,5))%>% #changing room
  
  #branch that send the training to the device, give a 0 probability for device that the trainer did, and calculate equal probability for the other
  branch(option = function () rdiscrete(1, prob_woman(),c(0,1,2,3,4)),continue = c(TRUE, TRUE, TRUE, TRUE),floor_line,cascading_parallel_line,
         rafter_line,jumping_line)%>%
  
  #try to go to video trainer to watch all video he didn't seen yet, if the video trainer didn't in the room he go to 
  #reject traj and come back and continue the main traj
  simmer::select(function()paste0("video_room_",get_attribute(sport_center,"video_room")))%>%
  seize_selected(amount=1,id=0,continue = c(TRUE,TRUE),post.seize=video_available ,reject =video_wait_line )%>%
  
  #return as device number or if the trainer getting tired 
  rollback(amount=6,check =function() woman_need_go_back ())%>%
  
  #all the people that here with positive number of unseen video, come at the morning before the trainer
  # arrive- so they need to wait till he come
  branch(option= function () {rdiscrete (1,check_about_see_video(), c(0,1))}, continue=c(TRUE), late_video_available) %>%
  branch( function () {rdiscrete (1,c(0.32, 0.68), c(0,1))}, continue=c(TRUE), lecture_line) %>% 
  set_prioritization (function () set_priority_woman() )%>%
  seize("physiotherapist",1)%>%
  timeout(function () rtriangle (1,25,40,33))%>%
  release("physiotherapist",1) %>%
  addService("woman_shower", function() runif(1,8,14))## all training go to shower

###################################################################

man_line<-trajectory("man_line")%>%
  set_attribute(keys = c("parallel","rings","horizontal_bar","pommel_horse","floor","jumping","parsonly_tried_level", "video_room", "lecture",
                         "unseen_video"), values = c(0,0,0,0,0,0,0,0,0,0))%>%
  set_attribute("video_room" , function () {rdiscrete (1 ,c(0.2,0.2,0.2,0.2,0.2), c(1,2,3,4,5))} )%>%#define the video room for all day
  addService("man_changing_room", function()runif (1,3,5))%>% #changing room
  
  #branch that send the training to the device, give a 0 probability for device that the trainer did, and calculate equal probability for the other
  branch(option = function () rdiscrete(1, prob_man(),c(0,1,2,3,4,5,6)),continue = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),parallel_line,
         rings_line,horizontal_bar_line,pommel_horse_line,floor_line, jumping_line )%>%
  
  #try to go to video trainer to watch all video he didn't seen yet, if the video trainer didn't in the room he go to reject traj
  #and come back and continue the main traj
  simmer::select(function()paste0("video_room_",get_attribute(sport_center,"video_room")))%>%
  seize_selected(amount=1,id=0,continue = c(TRUE,TRUE),post.seize=video_available ,reject =video_wait_line )%>%
  
  #return as device number  
  rollback(amount=3,check =function() man_need_go_back ())%>%
  ##for the people that arrive here and the video training didn't come yet - they wait till 8:00 and go to see them
  branch(option= function () {rdiscrete (1,check_about_see_video(), c(0,1))}, continue=c(TRUE), late_video_available) %>%
  branch( function () {rdiscrete (1,c(0.32, 0.68), c(0,1))}, continue=c(TRUE), lecture_line) %>% 
  set_prioritization (function () set_priority_man() )%>%
  seize("physiotherapist",1)%>%
  timeout(function () rtriangle (1,25,40,33))%>%
  release("physiotherapist",1) %>%
  addService("man_shower", function() runif(1,8,14))## all training go to shower


#Lunch break traj - entity set the capacity of all resources to 0 according to the break time
break_time <- trajectory("break_time")%>%
  set_global("breaktime",function() trimmedNorm(6,5/6))%>%
  set_global("last_capactiy_change",180 - get_global(sport_center, "breaktime"))%>%
  set_capacity("video_room_1",0)%>%
  set_capacity("video_room_2",0)%>%
  set_capacity("video_room_3",0)%>%
  set_capacity("video_room_4",0)%>%
  set_capacity("video_room_5",0)%>%
  set_capacity("physiotherapist",0)%>%
  set_capacity("dietician1",0)%>%
  set_capacity("dietician2",0)%>%
  timeout_from_global("breaktime")%>%
  set_capacity("video_room_1",2)%>%
  set_capacity("video_room_2",2)%>%
  set_capacity("video_room_3",2)%>%
  set_capacity("video_room_4",2)%>%
  set_capacity("video_room_5",2)%>%
  set_capacity("physiotherapist",5)%>%
  set_capacity("dietician1",1)%>%
  set_capacity("dietician2",1)%>%
  timeout_from_global("last_capactiy_change")%>%
  set_capacity("physiotherapist",3)
  


##----------------------------------------- 5.  All Generators------------------------------------------------

sport_center%>%
  add_generator("woman_training", woman_line, from_to(0,9*60,function () rexp(1,0.7744)) , mon=2, priority=0, preemptible =2.4, restart= FALSE) ### îâéòåú òã äùòä 3
sport_center%>%
  add_generator("man_training", man_line, from_to(0,9*60,function () rexp(1,0.8815) ) , mon=2, priority=0, preemptible =2.9, restart= FALSE) ### îâéòåú òã äùòä 3
sport_center%>%
  add_generator("break_time", break_time, at(420), mon=2,priority=2)


#----------------------------------------6.reset,run,plots----------------------------------------------------
reset(sport_center)%>%run(until=simulationTime)
set.seed(456)
arrivalData <- get_mon_arrivals(sport_center,per_resource=TRUE)%>%
  mutate(waiting_time = end_time - start_time - activity_time)
Resourcedata <- get_mon_resources(sport_center)
AttData <- get_mon_attributes(sport_center)
arrivalData_2 <- get_mon_arrivals(sport_center, ongoing = TRUE)

# mm1envs <- mclapply(1:number, function(i) {
#   set.seed(((i+100)^2)*3-7)
#   reset(sport_center)%>%run(until=simulationTime) %>%
#     wrap()
# })
# 
# 
# arrivalData <- get_mon_arrivals(mm1envs,per_resource=TRUE)%>%
#   mutate(waiting_time = end_time - start_time - activity_time)
# Resourcedata <- get_mon_resources(mm1envs)
# AttData <- get_mon_attributes(mm1envs)
# arrivalData_2 <- get_mon_arrivals(mm1envs, ongoing = TRUE)

# #-----------------------------------------------------------------------------------
# 
# 
# #relative_accuracy
# calc_relative_accuracy <- function(mean,sd){
#   (t*sd/sqrt(n0))/mean
# }
# 
# #replications-number
# calc_needed_replications <- function(relative_accuracy){
#   n0*((relative_accuracy/gamma_check_if_smaller)^2)
# }
# 
# n0 <- number
# num_of_mesured <- 4
# gamma <- 0.10
# a_total <- 0.08
# a_i <- a_total/num_of_mesured
# t <- qt(1-(a_i)/2,n0-1)
# gamma_check_if_smaller <- gamma/(1+gamma)
# 
# 
# 
# #------------------------------------------------------------------------ Tired Percent ----------------------------------
# numcompleted<-sqldf("select name, replication ,sum(value) as sumV
#                  from AttData
#                  where (key ='rings' or key='parallel' or key='pommel_horse' or key='floor' or key='jumping'or key='horizontal_bar' or key = 'cascading_parallel' or key ='rafter')
#                  group by name,replication")
# 
# tiredness<-sqldf("select name ,replication,max(value)as tiredness
#                   from AttData
#                  where key = 'parsonly_tried_level'
#                  group by name,replication")
# 
# TiredFullTable<-sqldf ("select numcompleted.name,numcompleted.replication, tiredness.tiredness,numcompleted.sumV
#                  from numcompleted JOIN tiredness ON numcompleted.name = tiredness.name AND numcompleted.replication = tiredness.replication ")
# 
# 
# mentired<-sqldf ("select replication, count(*) as menleave
#                   from TiredFullTable
#                   where name LIKE 'm%' AND (sumv < 6 AND tiredness >= 2.9)
#                   group by replication")
# 
# womentired<-sqldf("select replication, count(*) as womenleave
#                   from TiredFullTable
#                   where name LIKE 'W%' AND (sumv < 4 AND tiredness >= 2.4)
#                   group by replication")
# leaving<- merge(x = mentired, y = womentired, by = "replication", all.x = TRUE)
# 
# 
# 
# 
# 
# leaving[is.na(leaving)] <- 0
# leaving<-sqldf("select replication, (menleave + womenleave) as leaving
#                   from leaving
#                   group by replication")
# 
# 
# mennottired<-sqldf ("select count(*) as leaving
#                   from TiredFullTable
#                   where name LIKE 'm%' AND (sumv = 6 )
#                    group by replication ")
# 
# Womennottired<-sqldf ("select count(*) as leaving
#                   from TiredFullTable
#                   where name LIKE 'W%' AND (sumv = 4 )
#                   group by replication")
# 
# 
# finshed <-mennottired +Womennottired  ## Number of not tired
# 
# precent_tried_per_run<-leaving$leaving/(leaving$leaving+finshed) # percent of tired
# 
# paste( "The avrage left precent of Trainers is " ,mean(precent_tried_per_run$leaving),",The standard deviation left precent of Trainers is " ,sd(precent_tried_per_run$leaving))
# 
# mean_tierd_left <- mean(precent_tried_per_run$leaving)
# sd_tierd_left <- sd(precent_tried_per_run$leaving)
# 
# 
# relative_accuracy_tired_cu<- calc_relative_accuracy(mean_tierd_left,sd_tierd_left)
# replications_tired_cu <-calc_needed_replications(relative_accuracy_tired_cu)
# 
# 
# #-------------------------------------------------------Average Waiting Time-----------------------------------------------------------
# #Average Waiting time per resource
# avg_per_resource <- sqldf("select replication, resource,avg(waiting_time) as avg 
#                           from arrivalData 
#                           where (resource ='rings' or resource ='parallel' or resource ='pommel_horse' or resource='pommel horse' or resource='floor' or resource='jumping1'or resource='jumping2' or resource='horizontal_bar' or resource = 'cascading_parallel' or resource = 'rafter1' or resource= 'rafter2')
#                           group by replication,resource")
# 
# 
# # Average Waiting time for all devices per replication
# avg_waitingtime_per_run<-sqldf("select resource, replication,avg(avg) as avg_for30 
#                               from avg_per_resource
#                               group by  replication")
# 
# mean_avg_waitingtime <- mean(avg_waitingtime_per_run$avg_for30)
# sd_avg_waitingtime <- sd(avg_waitingtime_per_run$avg_for30)
# 
# print(mean_avg_waitingtime)
# print(sd_avg_waitingtime)
# 
# relative_accuracy_waiting_time_cu <- calc_relative_accuracy(mean_avg_waitingtime,sd_avg_waitingtime)
# replications_waiting_time_cu <-calc_needed_replications(relative_accuracy_waiting_time_cu)
# 
# #----------------------------------------------------- Average queue lenght for floor------------------------------------------------------------
# 
# avg_qforn<-function () {
#   result <-matrix(0:0,nrow=number,ncol=1)
#   for (i in 1:number){
#     test<-filter(Resourcedata <- get_mon_resources(mm1envs), replication == as.character(i) )
#     time <- as.matrix(subset(test, resource == "floor", select = c(time)));
#     queueLength <- as.matrix(subset(test, resource == "floor", select = c(queue)));
#     avgResQueue <- avgQueue(time, queueLength, simulationTime)
#     paste("Average queue len for floor was ",avgResQueue, "people")
#     
#     result[i,1]<-avgResQueue
#   }
#   return (result)
# }
# 
# 
# 
# avg_queue_per_run<-avg_qforn() #  AVRAGE queue length for each replication
# mean_avg_queue <- mean(avg_queue_per_run) # the mean of all replication
# sd_avg_queue <- sd(avg_queue_per_run)
# print(mean_avg_queue)
# print(sd_avg_queue)
# 
# relative_accuracy_avg_queue_cu <- calc_relative_accuracy(mean_avg_queue,sd_avg_queue)
# replications_avg_queue_cu  <-calc_needed_replications(relative_accuracy_avg_queue_cu)
# 
# 
# 
# #-----------------------------------------------------Finish percent---------------------------------------------------------
# finished_name <- sqldf("select replication,name
#                     from arrivalData_2
#                     where arrivalData_2.finished is true
#                     ")
# finished_1 <- sqldf("select replication,count(*) as finished
#                     from finished_name
#                     group by replication
#                     ")
# 
# all_trainer <- sqldf ("select replication, count(*) as all_trainer
#                       from arrivalData_2
#                       group by replication
#                       ")
# 
# precent_finished_per_run <- finished_1/all_trainer #  Finish percent per replication
# finish_precent_mean<-mean(precent_finished_per_run$finished)  
# finish_precent_sd<-sd(precent_finished_per_run$finished)
# paste("The Avrage Finish precent of trainers is ",mean(precent_finished_per_run$finished),"The standard deviation of finish precent is  ",sd(precent_finished_per_run$finished))
# 
# mean_precent_finished_per_run <- mean(precent_finished_per_run$finished)
# sd_precent_finished_per_run <- sd(precent_finished_per_run$finished)
# 
# 
# relative_accuracy_finish_precent_cu <- calc_relative_accuracy(mean_precent_finished_per_run,sd_precent_finished_per_run)
# replications_finish_precent_cu <-calc_needed_replications(relative_accuracy_finish_precent_cu)
# 
# 
# paste("relative accuracy for tired trainers is :",relative_accuracy_tired_cu,"replication number is",replications_tired_cu)
# paste("relative accuracy for avrage waiting time is:",relative_accuracy_waiting_time_cu,"replication number is",replications_waiting_time_cu)
# paste("relative accuracy for avrage queue lenght is :",relative_accuracy_avg_queue_cu ,"replication number is",replications_avg_queue_cu)
# paste("relative accuracy for finish precent is:",relative_accuracy_finish_precent_cu,"replication number is",replications_finish_precent_cu)
# 
# 
# 
# print(mean_tierd_left)
# print(sd_tierd_left)
# print(mean_avg_waitingtime)
# print(sd_avg_waitingtime)
# print(mean_avg_queue)
# print(sd_avg_queue)
# print(finish_precent_mean)
# print(finish_precent_sd)

# write.csv(precent_tried_per_run,"C:\\Users\\Dolev\\CurrentdTiredPrecent.csv", row.names = FALSE)
# write.csv(avg_waitingtime_per_run,"C:\\Users\\Dolev\\CurrentAvrageWaitingTime.csv", row.names = FALSE)
# write.csv(avg_queue_per_run,"C:\\Users\\Dolev\\CurrentAvrageQueueLenght.csv", row.names = FALSE)
# write.csv(precent_finished_per_run,"C:\\Users\\Dolev\\CurrentFisnishPrecent.csv", row.names = FALSE)
# 

#----------------------------------------------------------------------------------------------------




