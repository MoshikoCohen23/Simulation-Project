
#relative_accuracy
calc_relative_accuracy <- function(mean,sd){
  (t*sd/sqrt(n0))/mean
}

#replications-number
calc_needed_replications <- function(relative_accuracy){
  n0*((relative_accuracy/gamma_check_if_smaller)^2)
}

n0 <- number
num_of_mesured <- 4
gamma <- 0.10
a_total <- 0.08
a_i <- a_total/num_of_mesured
t <- qt(1-(a_i)/2,n0-1)
gamma_check_if_smaller <- gamma/(1+gamma)

#-------------current state------------------------------

paste("relative accuracy for tired trainers is :",relative_accuracy_tired_cu,"replication number is",replications_tired_cu)
paste("relative accuracy for avrage waiting time is:",relative_accuracy_waiting_time_cu,"replication number is",replications_waiting_time_cu)
paste("relative accuracy for avrage queue lenght is :",relative_accuracy_avg_queue_cu ,"replication number is",replications_avg_queue_cu)
paste("relative accuracy for finish precent is:",relative_accuracy_finish_precent_cu,"replication number is",replications_finish_precent_cu)
max_cur<-max(replications_tired_cu,replications_waiting_time_cu,replications_avg_queue_cu,replications_finish_precent_cu)

#-------------Alternative1 ------------------------------

paste("relative accuracy for tired trainers is :",relative_accuracy_tired_a1,"replication number is",replications_tired_a1)
paste("relative accuracy for avrage waiting time is:",relative_accuracy_waiting_time_a1,"replication number is",replications_waiting_time_a1)
paste("relative accuracy for avrage queue lenght is :",relative_accuracy_avg_queue_a1 ,"replication number is",replications_avg_queue_a1)
paste("relative accuracy for finish precent is:",relative_accuracy_finish_precent_a1,"replication number is",replications_finish_precent_a1)
max_a1<-max(replications_tired_a1,replications_waiting_time_a1,replications_avg_queue_a1,replications_finish_precent_a1)

#-------------Alternative2 ------------------------------

paste("relative accuracy for tired trainers is :",relative_accuracy_tired_a2,"replication number is",replications_tired_a2)
paste("relative accuracy for avrage waiting time is:",relative_accuracy_waiting_time_a2,"replication number is",replications_waiting_time_a2)
paste("relative accuracy for avrage queue lenght is :",relative_accuracy_avg_queue_a2,"replication number is",replications_avg_queue_a2)
paste("relative accuracy for finish precent is:",relative_accuracy_finish_precent_a2,"replication number is",replications_finish_precent_a2)
max_a2<-max(replications_tired_a2,replications_waiting_time_a2,replications_avg_queue_a2,replications_finish_precent_a2)

replication_needed<-max(max_cur,max_a1,max_a2)s