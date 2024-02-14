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
library(readxl)
library(knitr)
library(rmarkdown)
library(simmer)
library(simmer.plot)


dataset<-read.csv(file.choose(),header = T)
#----------------------------------------------------Average Queue lenght---------------------------------------------------------------

PairedTTestAvgQ1<- t.test(x= dataset$current_avrage_queue,y=dataset$alternative1_avrage_queue, alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=0.99666)
PairedTTestAvgQ2<- t.test(x= dataset$current_avrage_queue,y=dataset$alternative2_avrage_queue, alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=0.99666)
PairedTTestAvgQ3<- t.test(x= dataset$alternative1_avrage_queue,y=dataset$alternative2_avrage_queue, alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=0.99666)
print(PairedTTestAvgQ1)
print(PairedTTestAvgQ2)
print(PairedTTestAvgQ3)


#---------------------------------------------------Finish Precent----------------------------------------------------------------------
#paired t-test
PairedTTestFinishPrecent1<- t.test(x= dataset$current_finished_precent,y=dataset$alternative1_finished_precent, alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=0.99666)
PairedTTestFinishPrecent2<- t.test(x= dataset$current_finished_precent,y=dataset$alternative2_finished_precent, alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=0.99666)
PairedTTestFinishPrecent3<- t.test(x= dataset$alternative1_finished_precent,y=dataset$alternative2_finished_precent, alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=0.99666)
print(PairedTTestFinishPrecent1)
print(PairedTTestFinishPrecent2)
print(PairedTTestFinishPrecent3)


#-----------------------------------------------Percent of Tired Leaving----------------------------------------------------------------

PairedTTestTiredLeaving1<- t.test(x= dataset$current_tired_leaving_precent,y= dataset$alternative1_tired_leaving_precent, alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=0.99666)
PairedTTestTiredLeaving2<- t.test(x= dataset$current_tired_leaving_precent,y=dataset$alternative2_tired_leaving_precent, alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=0.99666)
PairedTTestTiredLeaving3<- t.test(x= dataset$alternative1_tired_leaving_precent,y=dataset$alternative2_tired_leaving_precent, alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=0.99666)
print(PairedTTestTiredLeaving1)
print(PairedTTestTiredLeaving2)
print(PairedTTestTiredLeaving3)


#-------------------------------------------------Average waiting time------------------------------------------------------------------
#paired t-test
PairedTTestWaitingTime1<- t.test(x= dataset$current_avg_waiting_time,y=dataset$alternative1_avg_waiting_time, alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=0.99666)
PairedTTestWaitingTime2<- t.test(x= dataset$current_avg_waiting_time,y=dataset$alternative2_avg_waiting_time, alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=0.99666)
PairedTTestWaitingTime3<- t.test(x= dataset$alternative1_avg_waiting_time,y=dataset$alternative2_avg_waiting_time, alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=0.99666)
print(PairedTTestWaitingTime1)
print(PairedTTestWaitingTime2)
print(PairedTTestWaitingTime3)



