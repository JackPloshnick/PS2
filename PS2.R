
data <- c(1:100)

# Return Leemis or Cho-Gains
#input= "Leemis" returns leemis statistic, 
#input = "Cho-Gains" returns Cho-Gains Statistic, and full digit distribution 
# any other input returns both, and full digit distribution 
Leemis.or.ChoGains <- function(input,data){ 
  
  ## Gets first digit 
  first.digit <- as.numeric(substr(data, 1, 1)) #takes first digit of each data point as a numeric
  table <- as.data.frame(table(first.digit)) #puts above in a table with frequency 
  proportion <- table$Freq/sum(table$Freq) # number of times each number occurs/ total data points
  
  # calculates Leemis statistic 
  out <- numeric() #ensures final result is a numeric
  for(j in 1:length(levels(table$first.digit))){ #from first point in table to total number of points in table
    for(i in 1:9){ # for numbers 1 to 9
      if(as.numeric(levels(table$first.digit)[j])==i){ #if the first digit from table = 1:9, run next line
        out[i] <- proportion[j] - log10(1+1/i)
      }
    }
  }
  leemis.full = out 
  leemis = max(out)
  
  #Calculates ChoGains statistic 
  out.CG <- numeric()
  for(j in 1:length(levels(table$first.digit))){ #from first point in table to total number of points in table
    for(i in 1:9){ # for numbers 1 to 9
      if(as.numeric(levels(table$first.digit)[j])==i){ #if the first digit from table = 1:9, run next line
        out.CG[i] <- (proportion[j] - log10(1+1/i))^2
      }
    }
  }
  ChoGains.full <- out.CG
  ChoGains = sqrt(sum(out.CG))
  
  #control system 
  if(input == "Leemis") {  #If input is "Leemis"
    print(leemis)
    print(leemis.full )# Then print the leemis results only
  } else if(input == "ChoGains") { #If input is "ChoGains"
    print(ChoGains)
    print(ChoGains.full )#Then print chogains results only
  } else{ # if input is anything else
    print(c(leemis, ChoGains))
    print(c(leemis.full, ChoGains.full))#Print both Leemis and ChoGains results 
  }
}

Leemis.or.ChoGains( "ChoGains" , data)


#Question 2 

#Tells you Leemis signifigance level
Critical.Values.Leemis <- function(data){
  if(leemis(data) >= .851 & leemis(data) < .966){  #if Leemis stat between .851 and .966, return .10 signifigance
    print("*")
   }else if(leemis(data) >= .967 & leemis(data) < 1.211){ #if leemis stat between .967 and 1.211, return .05 signifigance
    print("**")
  }else if(leemis(data) >= 1.212) { # if leemis stat bigger than 1.212, return .01 signifigance
    print("***")
  }else { #if leemis stat less than ,851, return no signifigance 
    print(" ")
  }
}

Critical.Values.Leemis(data)

#Tells you Cho-Gains signifigance value
Critical.Values.ChoGains <- function(data){
  if(ChoGains(data) >= 1.212 & ChoGains(data) < 1.329){
    print("*")
  }else if(ChoGains(data) >= 1.330 & ChoGains(data) < 1.568){
    print("**")
  }else if(leemis(data) >= 1.569) {
    print("***")
  }else {
    print(" ")
  }
}

Critical.Values.ChoGains(data)




#######
print.benfords = function(data){
  
  ### Makes it all a table
  benfords.table = rbind(c(leemis(data),Critical.Values.Leemis(data)),
                          c(ChoGains(data), Critical.Values.ChoGains(data)))
  
  ### adds names
  rownames(benfords.table) = c("Leemis","Cho-gaines'")
  colnames(benfords.table) = c("Statistic", "Significance")
  
  ### print
  print.table(benfords.table)
  ### Displays key to asterisks
  cat("  
      No star indicates a > .10 , * indicates a < .10, ** indicates a < .05, 
      *** indicates a < .01")
}

print.benfords(data)


#### Write CSV
Benfords.CSV <- function(data){
  ## Makes a file
  sink(file = "Benfords_data.csv")
  print.benfords(votes)
  ## sinks file
  sink()
}

Benfords.CSV(data)




