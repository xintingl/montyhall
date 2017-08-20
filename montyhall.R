##monty hall problem

nrun <- 10000 #how many times to run sim
n_door_start = 3 #how many doors do we have to begin with

montyhallfx <- function(n_door_start = 3){#how many doors to start w/ - default 3
  
  door_vector <- vector(length=n_door_start)
  door_vector[1] <- TRUE #create a 'winning' door, place it in slot 1 for convenience
  door_vector <- sample(door_vector) #shuffle doors around randomly
  
  door_pick <- sample(length(door_vector),1) #pick a position for your door choice
  
  false_doors <- which(!door_vector)#identify the false doors' locations
  
  true_door <- which(door_vector)
  
  is_original_pick_correct <- door_pick==true_door
  
  is_switch_pick_correct <- door_pick!=true_door
  
  #return door pick answer (T or F) and switch pick answer (T or F) as named DT row
  return(cbind(is_original_pick_correct,is_switch_pick_correct))
}

#create answer DT for n runs
dt <- data.frame("original_pick_correct"=vector(length=nrun),
                 "switching_is_correct"=vector(length=nrun))

#run n timess
for(i in 1:nrun){
  dt[i,] <- rbind(montyhallfx(n_door_start))
}

#stats on dtable

#first: proof it works -- there is a correct answer in every row
which(dt$original_pick_correct) == which(!dt$switching_is_correct)
  #will resolve to true
  #translation: if the original door is correct, then the switch option will be incorrect
    #and implicitly, vice versa

hist(as.numeric(dt[,1]),main=paste0("Was the original door the winner?  \n In an empirical simulation of ",n_door_start," initial doors"),
     xlab='No<-..................................................................->Yes',
     ylab=paste0('Frequency (x/',nrun,')'))


#check against hypothesis!
expected_odds_original <- 1/n_door_start #the expected odds of the original door being right
outcome_original <- length(which(dt$original_pick_correct))/nrun

  