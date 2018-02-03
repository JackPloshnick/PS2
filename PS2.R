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
  return(max(out))
}

leemis(data)


ChoGains <- function(input){
  first.digit <- as.numeric(substr(data, 1, 1)) #takes first digit of each data point as a numeric
  table <- as.data.frame(table(first.digit)) #puts above in a table with frequency 
  proportion <- table$Freq/sum(table$Freq) # number of times each number occurs/ total data points
  out <- numeric() #ensures final result is a numeric
  for(j in 1:length(levels(table$first.digit))){ #from first point in table to total number of points in table
    for(i in 1:9){ # for numbers 1 to 9
      if(as.numeric(levels(table$first.digit)[j])==i){ #if the first digit from table = 1:9, run next line
        out[i] <- (proportion[j] - log10(1+1/i))^2
      }
    }
  }
  return(sqrt(sum(out)))
}

ChoGains(data)


# Return Leemis or Cho-Gains

Leemis.or.ChoGains <- function(input,data){ 
  if(input == "Leemis") {  #If input is "Leemis"
    print(leemis(data)) # Then print the leemis results only
  } else if(input == "ChoGains") { #If input is "ChoGains"
    print(ChoGains(data)) #Then print chogains results only
  } else{ # if input is anything else
      print(c(leemis(data), ChoGains(data))) #Print both Leemis and ChoGains results 
   }
}
  

Leemis.or.ChoGains( "both" ,data)

leemis <-.7

#Question 2 

Critical.Values.Leemis <- function(data){
  if(leemis(data) >= .851 & leemis(data) < .966){
    print("Critical Value is less than or equal to .10 (Leemis)")
   }else if(leemis(data) >= .967 & leemis(data) < 1.211){
    print("Critical Value is less than or equal to .05( Leemis)")
  }else if(leemis(data) >= 1.212) {
    print("Critical Value is less than or equal to .01 (Leemis)")
  }else {
    print("Critical Value is not signifigant (Leemis)")
  }
}

Critical.Values.Leemis(data)

Critical.Values.ChoGains <- function(data){
  if(ChoGains(data) >= 1.212 & ChoGains(data) < 1.329){
    print("Critical Value is less than or equal to .10 (ChoGains)")
  }else if(ChoGains(data) >= 1.330 & ChoGains(data) < 1.568){
    print("Critical Value is less than or equal to .05(ChoGains)")
  }else if(leemis(data) >= 1.569) {
    print("Critical Value is less than or equal to .01(ChoGains)")
  }else {
    print("Critical Value is not signifigant(ChoGains)")
  }
}

Critical.Values.ChoGains(data)
