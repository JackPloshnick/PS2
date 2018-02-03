data= c(0:100)

#returns Leemis' m statistic
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

#Returns Cho-Gains d statistic
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
#input= "Leemis" returns leemis statistic
#input = "Cho-Gains" returns Cho-Gains Statistic
# any other input returns both 
Leemis.or.ChoGains <- function(input,data){ 
  if(input == "Leemis") {  #If input is "Leemis"
    print(leemis(data)) # Then print the leemis results only
  } else if(input == "ChoGains") { #If input is "ChoGains"
    print(ChoGains(data)) #Then print chogains results only
  } else{ # if input is anything else
      print(c(leemis(data), ChoGains(data))) #Print both Leemis and ChoGains results 
   }
}
  

Leemis.or.ChoGains( "both" , data)


#Question 2 

#Tells you Leemis signifigance level
Critical.Values.Leemis <- function(data){
  if(leemis(data) >= .851 & leemis(data) < .966){  #if Leemis stat between .851 and .966, return .10 signifigance
    print("*   (Leemis)")
   }else if(leemis(data) >= .967 & leemis(data) < 1.211){ #if leemis stat between .967 and 1.211, return .05 signifigance
    print("**  (Leemis)")
  }else if(leemis(data) >= 1.212) { # if leemis stat bigger than 1.212, return .01 signifigance
    print("*** (Leemis)")
  }else { #if leemis stat less than ,851, return no signifigance 
    print("    (Leemis)")
  }
}

Critical.Values.Leemis(data)

#Tells you Cho-Gains signifigance value
Critical.Values.ChoGains <- function(data){
  if(ChoGains(data) >= 1.212 & ChoGains(data) < 1.329){
    print("*   (ChoGains)")
  }else if(ChoGains(data) >= 1.330 & ChoGains(data) < 1.568){
    print("**  (ChoGains)")
  }else if(leemis(data) >= 1.569) {
    print("*** (ChoGains)")
  }else {
    print("    (ChoGains)")
  }
}

Critical.Values.ChoGains(data)

#input= "Leemis" returns leemis critical value
#input= "Cho-Gains" returns cho-gains critical value
#any other input returns both

Critical.Values <- function(input,data){ 
  if(input == "Leemis") {  #If input is "Leemis"
    return(Critical.Values.Leemis(data)) # Then print the leemis results only
  } else if(input == "ChoGains") { #If input is "ChoGains"
    return(Critical.Values.ChoGains(data)) #Then print chogains results only
  } else { # if input is anything else
    print(c(Critical.Values.Leemis(data), Critical.Values.ChoGains(data))) #Print both Leemis and ChoGains results 
  }
}

Critical.Values("Leemis" , data)

####Puts it all together 

#input= "Leemis" returns leemis statistic and critical value
#input= "Cho-Gains" returns cho-gains statistic and critical value
#any other input returns both

output <- data.frame()

Master.Function <- function(input,data){
  output = data.frame(substring(Critical.Values(input, data),4) ,Leemis.or.ChoGains(input, data), 
             substring(Critical.Values(input, data),1,3))

} 
output <- Master.Function("Both", data)   ### how to get this stuff in the function???
colnames(output) <- c("Test","Statistic", "Critical Value")

sig.values <- data.frame("describe the shit")

