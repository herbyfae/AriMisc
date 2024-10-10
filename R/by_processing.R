by_processing = function(data = NULL,
                         by = NULL,
                         quantiles = NULL,
                         breaks.num = NULL){

# Just adapt what you've been using at the beggining of just about every function
 # + string to date and floor_date
      # wip


  if (is.null(by)) {
    stop("No by, where am I supposed to be aggregating from?")
  }
  if (is.null(data)) {
    stop("No data")
  }

  data = as.data.frame(data)
  skip = F

# Vector or call for a column

  if (length(by) == 1) {
    if (is.character(by)) {
      by.name = as.character(by)
      by = data[, by.name]

    } else{
      by.name = colnames(data)[by]
      by = data[, by]

    }
    data = data[, grep(paste('\\b', by.name, '\\b', sep = ""), names(data), invert = T)]
  }

# Figuring out the format
  # Character and factors first
  # Just by far the easiest
   # Could've done it so that more than x amount of all numeric "levels"
    # would automatically be converted to a numeric vector

   # However... The point of this entire library is to trust the user
    # no matter how seemingly nonsensical their decisions are

    # Because so are mine

  if (is.factor(by)) {

    if(sum(is.na(by))>0 & skip == F){
      warning("Given by has NAs - converted to a separate subset")
      by = as.character(by)
      by[is.na(by)] = "NA"
      by = as.factor(by)
    }

    levels = levels(by)

  }


    if (is.character(by)) {
      if(sum(is.na(by)) > 0){
        by[is.na(by)] = "NA"
        warning("Given by has NAs - converted to a separate subset")
      }
      by = factor(by, exclude = NULL)
  }


  if (is.numeric(by)) {
    if (!is.null(quantiles) & !is.null(breaks.num)) {
      stop("Either quantiles or breaks, not both")
    }

    if (!is.null(breaks.num)) {

      breaks.num = breaks.num[order(breaks.num)]
      str.by = by
      quant.levels = NULL

      str.by[which(by <= breaks.num[1])] = paste("<=", as.character(breaks.num)[1], sep = "")
      quant.levels[1] = paste("<=", as.character(breaks.num)[1], sep = "")

      if(length(breaks.num) > 1){
        for (i in 2:length(breaks.num)) {
          str.by[which(by <= breaks.num[i] &
                         by > breaks.num[i - 1])] = gsub(" ", "", paste(as.character(breaks.num[i - 1]), "-",
                                                                        as.character(breaks.num[i], sep = "")))
          quant.levels[i] =  gsub(" ", "", paste(as.character(breaks.num[i - 1]), "-",
                                                 as.character(breaks.num[i], sep = "")))

        }
      }

      str.by[which(by > breaks.num[length(breaks.num)])] = paste(">", as.character(breaks.num)[length(breaks.num)], sep = "")
      quant.levels[(length(breaks.num)+1)] = paste(">", as.character(breaks.num)[length(breaks.num)], sep = "")

      if(sum(is.na(by))>0){
        warning("Given by has NAs - converted to a separate subset")
        str.by[is.na(by)] = "NA"
        quant.levels[(length(breaks.num)+2)] = "NA"
      }

      by = factor(str.by, levels = quant.levels, exclude = NULL)
      levels = levels(by)

    } else {
      if (!is.null(quantiles)) {
        quantiles = as.numeric(quantiles)
        quantiles[order(quantiles, decreasing = F)]

        if(sum(quantiles < 0) > 0|sum(quantiles >100) > 0){
          stop("Detected a number under 0 or over 100 in the by quantiles - Unable to convert to 0-1 quantiles")
        } else if(sum(quantiles >= 1) > 0){
          quantiles = quantiles/100
        }

      } else{
        quantiles = c(0.25,0.5,0.75)
      }

      str.by = by
      quant.levels = NULL

      str.by[which(by <= quantile(by, quantiles[1], na.rm = T))] = paste("<=Q", quantiles[1], sep = "")
      quant.levels[1] = paste("<=Q", quantiles[1], sep = "")

      for (i in 2:length(quantiles)) {
        str.by[which(by <= quantile(by, quantiles[i], na.rm = T) &
                       by > quantile(by, quantiles[i - 1], na.rm = T))] = paste(paste("Q", quantiles[i -
                                                                                                       1], sep = ""),
                                                                                paste("Q", quantiles[i], sep = ""),
                                                                                sep = "-")

        quant.levels[i] = paste(paste("Q", quantiles[i -
                                                       1], sep = ""),
                                paste("Q", quantiles[i], sep = ""),
                                sep = "-")

      }

      str.by[which(by > quantile(by, quantiles[length(quantiles)], na.rm = T))] = paste(">Q", quantiles[length(quantiles)], sep = "")
      quant.levels[(length(quantiles) + 1)] = paste(">Q", quantiles[length(quantiles)], sep = "")

      if(sum(is.na(by))>0){
        warning("Given by has NAs - converted to a separate subset")
        str.by[is.na(by)] = "NA"
        quant.levels[(length(quantiles)+2)] = "NA"
      }

      by = factor(str.by, levels = quant.levels, exclude = NULL)
      levels = levels(by)

    }
  }





}
