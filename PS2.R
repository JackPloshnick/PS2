
data <- c(1:100)

# Return Leemis or Cho-Gains
# input= "Leemis" returns leemis statistic. input ="Full Leemis" returns full digit distribution 
# input = "Cho-Gains" returns Cho-Gains Statistic. input = "Full ChoGains" returns full digit distribution 
# input = "Full Both" returns the full distribution for Leemis and Cho-Gains
# input = "All" returns a list containing the results, including the full digit distribution
# any other input returns both statistics 
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
    return(as.numeric(leemis))
   # Then print the leemis results only
  } else if(input == "ChoGains") { #If input is "ChoGains"
    return(as.numeric(ChoGains))
    #Then print chogains results only
    
    #Controls for full digit distribution 
   } else if(input == "Full ChoGains") { #If input is "ChoGains"
      return(as.numeric(ChoGains.full))
   } else if(input == "Full Leemis") { 
      return(as.numeric(leemis.full))
   } else if(input == "Full Both") { 
       return(as.numeric(c(leemis.full, ChoGains.full)))
     
  # returns list containing results and distribution     
   } else if(input == "All") { 
     return(as.numeric(c(leemis.full, ChoGains.full, leemis, ChoGains)))
         
  } else{ # if input is anything else
    return(c(leemis, ChoGains))#Print both Leemis and ChoGains results 
  }
}

Leemis.or.ChoGains( "All" , data)


#Question 2 

#######
print.benfords = function(data){
  
  ## Leemis critical values 
  x = data
  a = Leemis.or.ChoGains("Leemis", x)
  
  if(a >= .851 & a < .966){  #if Leemis stat between .851 and .966, return .10 signifigance
    Critical.Value.Leemis = "*"
  }else {
    if(a >= .967 & a < 1.211){ #if leemis stat between .967 and 1.211, return .05 signifigance
      Critical.Values.Leemis = "**"
    }else {
      if(a >= 1.212) { # if leemis stat bigger than 1.212, return .01 signifigance
       Critical.Values.Leemis = "***"
      }else { #if leemis stat less than ,851, return no signifigance 
        Critical.Values.Leemis =" " 
      }
    }
  }
  
  ## ChoGains Critical value 
  if( Leemis.or.ChoGains("ChoGains", data) >= 1.212 & Leemis.or.ChoGains("ChoGains", data) < 1.329){
    Critical.Values.ChoGains ="*"
  }else if(Leemis.or.ChoGains("ChoGains", data) >= 1.330 & Leemis.or.ChoGains("ChoGains", data) < 1.568){
    Critical.Values.ChoGains= "**"
  }else if(Leemis.or.ChoGains("ChoGains", data) >= 1.569) {
    Critical.Values.ChoGains = "***"
  }else {
    Critical.Values.ChoGains = " "
  }
  
  ### Makes it all a table
  benfords.table = rbind(c(Leemis.or.ChoGains("Leemis", data),Critical.Values.Leemis),
                          c(Leemis.or.ChoGains("ChoGains", data), Critical.Values.ChoGains))
  
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
  print.benfords(data)
  ## sinks file
  sink()
}

Benfords.CSV(data)




