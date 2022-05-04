outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
ncol(outcome) #46 columns
nrow(outcome) #4706 rows

names(outcome) #name of each column

str(outcome)
summary(outcome)

#as.numeric --> only captures numbers; have to do this as we read file colClasses as "character"

hist(as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)) #shows same as below
hist(as.numeric(outcome[, 11])) #shows same as below

best <- function(state, outcome) {
  ## Read outcome data
  
  outcomes <- read.csv("outcome-of-care-measures.csv", 
                       colClasses = "character",
                       header = TRUE)
  
  ## Get data we're interested in
  
  rates <- as.data.frame(cbind(outcomes[, 2],  # hospital 
                               outcomes[, 7],   # state
                               outcomes[, 11],  # heart attack 
                               outcomes[, 17],  # heart failure 
                               outcomes[, 23]), # pneumonia
                         stringsAsFactors = FALSE)
  
  ## Rename columns
  
  colnames(rates) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  
  if(!state %in% rates[,"state"]){
    stop('invalid state')
  }
  
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  }
  
  
  ## Return hospital name in that state with lowest 30-day death rate
  
  ## Get only the hospitals in chosen state
  hRates <- rates[(rates[, "state"] == state), ]
  
  ## Convert outcome rate to numeric
  hRates[, outcome] <- as.numeric(hRates[, outcome])
  
  ## Remove NA values
  hRates <- hRates[!is.na(hRates[, outcome]), ]
  
  ## Order by outcome rate
  hRates <- hRates[order(hRates[, outcome]), ]
  
  ## Get names of hospital with the lowest rate
  hNames <- hRates[hRates[, outcome] == min(hRates[,outcome]),1]
  
  ## Sort by hospital name if tie
  sort(hNames)[1]
  
}

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  
  outcomes <- read.csv("outcome-of-care-measures.csv", 
                       colClasses = "character",
                       header = TRUE)
  
  ## Get data we're interested in
  
  rates <- as.data.frame(cbind(outcomes[, 2],   # hospital
                               outcomes[, 7],   # state
                               outcomes[, 11],  # heart attack
                               outcomes[, 17],  # heart failure
                               outcomes[, 23]), # pneumonia
                         stringsAsFactors = FALSE)
  
  ## Rename columns
  
  colnames(rates) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  
  if(!state %in% rates[,"state"]){
    stop('invalid state')
  }
  
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  }
  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  ## Get only the hospitals in chosen state
  hRates <- rates[(rates[, "state"] == state), ]
  
  ## Convert outcome rate to numberic, gets a warning
  hRates[, outcome] <- as.numeric(hRates[, outcome])
  
  ## Remove NA values
  hRates <- hRates[!is.na(hRates[, outcome]), ]
  
  ## convert num argument to valid rank
  
  if(num == "best") {
    num <- 1 
  }
  
  if (num == "worst") {
    num <- nrow(hRates) 
  }
  
  ## Order by outcome rate
  hRates <- hRates[order(hRates[, outcome], hRates[, "hospital"]), ]
  
  ## Get names of hospital 
  
  hRates[num,1]
  
}

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  
  data <- read.csv("outcome-of-care-measures.csv", 
                      colClasses = "character",
                      header = TRUE)
  
  rankings <- as.data.frame(cbind(data[, 2],   # hospital
                                  data[, 7],   # state
                                  data[, 11],  # heart attack
                                  data[, 17],  # heart failure
                                  data[, 23]), # pneumonia
                         stringsAsFactors = FALSE)
  
  ## Rename columns
  
  colnames(rankings) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")  
  
  
  ## Check that state and outcome are valid
  if (!outcome %in% cbind("heart attack", "heart failure", "pneumonia")){
    stop("invalid outcome!")
  }

  
  ## For each state, find the hospital of the given rank
  
    #make numeric
    rankings[,outcome] <- as.numeric(rankings[, outcome])
    
    #remove na
    rankings <- rankings[!is.na(rankings[,outcome]), ]
    
    split_states <- split(rankings, rankings$state)
    
    ranks <- lapply(split_states, function(x, num){
      x = x[order(x[,outcome], x$hospital),]
      
      if(class(num) == "character"){
        if (num == "best"){
          return (x$hospital[1])
        }
        else if (num == "worst"){
          return (x$hospital[nrow(x)])
        }
      }
      else {
        return(x$hospital[num])
      }
    }, num)
    
  }
  
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name

