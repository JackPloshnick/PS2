data= c(0:100)

leemis <- function(input){
  first.digit <- as.numeric(substr(data, 1, 1)) #takes first digit of each data point as a numeric
  table <- as.data.frame(table(first.digit)) #puts above in a table with frequency 
  proportion <- table$Freq/sum(table$Freq) # number of times each number occurs/ total data points
  out <- numeric() #ensures final result is a numeric
  for(j in 1:length(levels(table$first.digit))){ #from first point in table to total number of points in table
    for(i in 1:9){ # for numbers 1 to 9
      if(as.numeric(levels(table$first.digit)[j])==i){ #if the first digit from table = 1:9, run next line
        out[i] <- proportion[j] - log10(1+1/i)
      }
    }
  }
  return(out)
}

max(leemis(data))

ChoGains <- function(input){
  first.digit <- as.numeric(substr(data, 1, 1)) #takes first digit of each data point as a numeric
  table <- as.data.frame(table(first.digit)) #puts above in a table with frequency 
  proportion <- table$Freq/sum(table$Freq) # number of times each number occurs/ total data points
  out <- numeric() #ensures final result is a numeric
  for(j in 1:length(levels(table$first.digit))){ #from first point in table to total number of points in table
    for(i in 1:9){ # for numbers 1 to 9
      if(as.numeric(levels(table$first.digit)[j])==i){ #if the first digit from table = 1:9, run next line
        out[i] <- sqrt( sum( (proportion[j] - log10(1+1/i))^2))
      }
    }
  }
  return(out)
}

ChoGains(data)