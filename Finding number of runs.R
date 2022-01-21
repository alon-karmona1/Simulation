
#------------------------------------------ בדיקת מספר ריצות ---------------------------------
#parameters

n <- 30
gamma <- 0.12
alfa_total <- 0.09
alfa_i_accuracy <- alfa_total/3
t <- qt(1-(alfa_i_accuracy)/2,n-1)
gamma_tag <- gamma/(1+gamma)
calc_relative_accuracy <- function(mean,sd){
  (t*sd/sqrt(n))/mean
}

##data Current 
avg_Waiting_Time<- 43.783
avg_Line_cutStand<- 4.898
avg_Finish_percent<- 10.19
sd_Waiting_Time<-22.977
sd_Line_cutStand<-1.645
sd_Finish_percent<- 6.29

## data option 1    
avg_Waiting_Time1<- 44.305
avg_Line_cutStand1<- 1.6776
avg_Finish_percent1<- 14.29
sd_Waiting_Time1<-17.199
sd_Line_cutStand1<-1.542
sd_Finish_percent1<- 7.15

## data option 2 
avg_Waiting_Time2<- 36.351
avg_Line_cutStand2<- 2.5886
avg_Finish_percent2<- 16.887
sd_Waiting_Time2<-19.299
sd_Line_cutStand2<-1.311
sd_Finish_percent2<- 8.701

#Relative accuracy for avg_Waiting_Time

#Current 

relative_accuracy_avg_Waiting_Time <-
  calc_relative_accuracy(avg_Waiting_Time,sd_Waiting_Time)

#option 1
relative_accuracy_avg_Waiting_Time_1 <-
  calc_relative_accuracy(avg_Waiting_Time1,sd_Waiting_Time1)

#option 2
relative_accuracy_avg_Waiting_Time_2 <-
  calc_relative_accuracy(avg_Waiting_Time2,sd_Waiting_Time2)

#Relative accuracy for avg_Income_Day

#Current 

relative_accuracy_avg_Line_cutStand <-
  calc_relative_accuracy(avg_Line_cutStand,sd_Line_cutStand)

#option 1
relative_accuracy_avg_Line_cutStand_1 <-
  calc_relative_accuracy(avg_Line_cutStand1,sd_Line_cutStand1)

#option 2
relative_accuracy_avg_Line_cutStand_2 <-
  calc_relative_accuracy(avg_Line_cutStand2, sd_Line_cutStand2)

#Relative accuracy for sd_Finish_Day

#Current   
relative_accuracy_avg_Finish_percent <-
  calc_relative_accuracy(avg_Finish_percent,sd_Finish_percent)

#option 1
relative_accuracy_avg_Finish_percent_1 <-
  calc_relative_accuracy(avg_Finish_percent1,sd_Finish_percent1)

#option 2
relative_accuracy_avg_Finish_percent_2 <-
  calc_relative_accuracy(avg_Finish_percent2,sd_Finish_percent2)




# Function that calculate the number of replication

number_of_replications <- function(relative_accuracy){
  n*(relative_accuracy/gamma_tag)^2
}

#Calculates the number of runs required for each alternative by using the function above

#Current
n1 <- number_of_replications(relative_accuracy_avg_Waiting_Time)

n2 <- number_of_replications(relative_accuracy_avg_Line_cutStand)

n3 <- number_of_replications(relative_accuracy_avg_Finish_percent)


#option 1
n4 <- number_of_replications(relative_accuracy_avg_Waiting_Time_1)

n5 <- number_of_replications(relative_accuracy_avg_Line_cutStand_1)

n6 <- number_of_replications(relative_accuracy_avg_Finish_percent_1)



#option 2
n7 <- number_of_replications(relative_accuracy_avg_Waiting_Time_2)

n8 <- number_of_replications(relative_accuracy_avg_Line_cutStand_2)

n9 <- number_of_replications(relative_accuracy_avg_Finish_percent_2)

n_new <- ceiling(max(n1,n2,n3,n4,n5,n6,n7,n8,n9))

print(n_new)
