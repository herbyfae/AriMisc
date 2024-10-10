missinglevel = function(factor1, factor2){

  tally = NULL; missing = NULL; missingcycle = NULL

  factor1 = as.factor(factor1); factor2 = as.factor(factor2)
  levels = levels(factor1)

  for(i in 1:length(levels)){

    if(sum(factor2 == levels[i]) > 0){
      tally = c(tally, levels[i])
    } else {
      missing = c(missing, levels[i])
      missingcycle = c(missingcycle, which(factor1 == levels[i]))
    }
  }

  missingfactor2 = which(is.na(factor(factor2, levels = tally)))
  test = unique(as.character(factor2[missingfactor2]))

  tally = unique(tally)

  if(is.null(tally)){
    stop("No matches found, are you sure about what you introduced?")
  }

  if(length(tally) < length(unique(c(tally, missing)))/4){
    warning("Less than 25% of possible levels were matched")
  } else if(length(tally) < length(unique(c(tally, missing)))/2){
    warning("Less than 50% of possible levels were matched")
  }

  return(list("matching levels" = tally,
              "non-matching1" = unique(missing),
              "non-matching2" = test,
              "non-matching index1" = missingcycle,
              "non-matching index2" = missingfactor2)
  )
}

NumToFact = function(data = NULL,
                     num = NULL,
                     quantiles = NULL,
                     breaks = NULL,
                     break.names = NULL,
                     return.full = F) {

  if (!is.null(quantiles) & !is.null(breaks)) {
    stop("Either quantiles or breaks, not both")
  }
  if(is.null(num)&is.null(data)){
    stop("How would I convert something you're not giving me")
  }
  if(is.null(num)){
    warning("Data given but numerical column not specified -> All numerical columns converted")
  }
  if(is.null(data) & (is.character(num)|is.factor(num))){
    stop("Given num categorical or factor, check your data's structure")
  }

  if (!is.null(data)) {
    data = as.data.frame(data)

    if (!is.null(num)) {
      # Data given + columns(index or name)
      if (length(num) > length(data)) {
        stop("Given more indexes than there are columns in the data")
      } else{
        if(return.full){
          index = num
        }

        num = data[, num]

        if(sum(!unlist(lapply(num, is.numeric))) > 0){
          warning("Detected columns selected by num that weren't numeric - kept only those that were")
          num = num[,unlist(lapply(num, is.numeric))]
        }

      }
    } else{
      # Data given but no columns specified
      num = data[, unlist(lapply(data, is.numeric))] # Extracts numerical columns
      if(return.full){
        index = colnames(num)
      }
    }
  }

  num = as.data.frame(num)
  cols = length(colnames(num)) # Number of columns -> number of cycles

  # The rest is just recycled from how I subset by everywhere else

  if (!is.null(breaks)) {

    breaks = breaks[order(breaks)]

    for(j in 1:cols){

      str.num = num[j]

      if(!is.null(break.names)){
        if(length(break.names) != (length(breaks) + 1)){
          stop("There should be one name for each new level(length of breaks + 1)")
        }
        quant.levels = break.names
      } else{
        quant.levels = NULL
        quant.levels[1] = paste("<=", as.character(breaks)[1], sep = "")
      }

      str.num[which(num[j] <= breaks[1]),1] = quant.levels[1]

      if(length(breaks) > 1){
        for (i in 2:length(breaks)) {

          if(is.null(break.names)){
          quant.levels[i] = gsub(" ", "", paste(as.character(breaks[i - 1]), "-",
                                                as.character(breaks[i], sep = "")))
          }
          str.num[which(num[j] <= breaks[i] &
                          num[j] > breaks[i - 1]),1] = quant.levels[i]

        }
      }

      if(is.null(break.names)){
      quant.levels[(length(breaks) + 1)] = paste(">", as.character(breaks)[length(breaks)], sep = "")
      }

      str.num[which(num[j] > breaks[length(breaks)]),1] = quant.levels[(length(breaks) + 1)]

      if(sum(is.na(num[j]))>0){
        warning(paste("Given num has NAs - Col", j, sep = "_" ))
        str.num[is.na(num[j])] = "NA"
        quant.levels[(length(breaks) + 2)] = "NA"
      }

      num[j] = factor(str.num[,1], levels = unique(quant.levels))
    }
  } else {
    if (!is.null(quantiles)) {
      quantiles = as.numeric(quantiles)
      quantiles[order(quantiles, decreasing = F)]

      if(sum(quantiles <= 0) > 0|sum(quantiles >= 100) > 0){
        stop("Detected a number under 0 or over 100 in the quantiles - Unable to convert to 0-1 quantiles")
      } else if(sum(quantiles >= 1) > 0){
        quantiles = quantiles/100
        if(sum(quantiles < 0.01) > 0){
          warning("Converted quantiles have at least one element under 1%, check your values")
        }
      }

    } else{
      quantiles = c(0.25,0.5,0.75)
    }

    for(j in 1:cols){

      str.num = num[j]


      if(!is.null(break.names)){
        if(length(break.names) != (length(quantiles) + 1)){
          stop("There should be one name for each new level(length of quantiles + 1)")
        }
        quant.levels = break.names
      } else{
        quant.levels = NULL
        quant.levels[1] = paste("<=Q", quantiles[1], sep = "")
      }

      str.num[which(num[j] <= quantile(num[j], quantiles[1], na.rm = T)),1] = quant.levels[1]

      for (i in 2:length(quantiles)) {

        if (is.null(break.names)) {
          quant.levels[i] =  paste(paste("Q", quantiles[i - 1], sep = ""),
                                   paste("Q", quantiles[i], sep = ""),
                                   sep = "-")

        }
        str.num[which(num[j] <= quantile(num[j], quantiles[i], na.rm = T) &
                        num[j] > quantile(num[j], quantiles[i - 1], na.rm = T)),1] = quant.levels[i]

      }

      if (is.null(break.names)) {
      quant.levels[(length(quantiles) + 1)] = paste(">Q", quantiles[length(quantiles)], sep = "")
      }

      str.num[which(num[j] > quantile(num[j], quantiles[length(quantiles)], na.rm = T)),1] = quant.levels[(length(quantiles) + 1)]

      if(sum(is.na(num[j]))>0){
        warning(paste("Given num has NAs - Col", j, sep = "_" ))
        str.num[is.na(num[j])] = "NA"
        quant.levels[(length(quantiles) + 2)] = "NA"
      }

      num[j] = factor(str.num[,1], levels = unique(quant.levels))

    }
  }

  if(return.full){
    data[,index] = num
    num = data
  }

  return(num)

}







