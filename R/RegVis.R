RegVis = function(model = NULL,
                  target = NULL,
                  data = NULL,
                  by = NULL,
                  quantiles = NULL,
                  breaks.num = NULL
) {

  if(is.null(model)){
    stop("What is this supposed to do without a model?")
  }
  if(is.null(target)){
    stop("Not good enough at this to extract the target from the model")
  }
  if(is.null(by)){
    summ = summary(model)
    return(summ)
  }
  if(is.null(data)){
    stop("How would I create breakpoints in the data without it?")
  }

  if (length(by) == 1) {
    by = as.character(by)
    by = as.factor(data[, by])
  }
  if (is.character(by)) {
    by = as.factor(by)
  }
  if (is.factor(by)) {

    if(sum(is.na(by))>0){
      warning("Given by has NAs - converted to a separate subset")
      by = as.character(by)
      by[is.na(by)] = "NA"
      by = as.factor(by)
    }

    levels = levels(by)
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

      for (i in 2:length(breaks.num)) {
        str.by[which(by <= breaks.num[i] &
                       by > breaks.num[i - 1])] = gsub(" ", "", paste(as.character(breaks.num[i - 1]), "-",
                                                                      as.character(breaks.num[i], sep = "")))
        quant.levels[i] = gsub(" ", "", paste(as.character(breaks.num[i - 1]), "-",
                                              as.character(breaks.num[i], sep = "")))

      }

      str.by[which(by > breaks.num[length(breaks.num)])] = paste(">", as.character(breaks.num)[length(breaks.num)], sep = "")
      quant.levels[(length(breaks.num) + 1)] = paste(">", as.character(breaks.num)[length(breaks.num)], sep = "")

      if(sum(is.na(by))>0){
        warning("Given by has NAs - converted to a separate subset")
        str.by[is.na(by)] = "NA"
        quant.levels[(length(breaks.num) + 2)] = "NA"
      }

      by = factor(str.by, levels = quant.levels)
      levels = levels(by)

    } else {
      if (!is.null(quantiles)) {
        quantiles = as.numeric(quantiles)
        quantiles[order(quantiles, decreasing = F)]
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

        quant.levels[i] =  paste(paste("Q", quantiles[i-1], sep = ""), paste("Q", quantiles[i], sep = ""), sep = "-")
      }
      str.by[which(by > quantile(by, quantiles[length(quantiles)], na.rm = T))] = paste(">Q", quantiles[length(quantiles)], sep = "")
      quant.levels[(length(quantiles) + 1)] = paste(">Q", quantiles[length(quantiles)], sep = "")

      if(sum(is.na(by))>0){
        warning("Given by has NAs - converted to a separate subset")
        str.by[is.na(by)] = "NA"
        quant.levels[(length(quantiles) + 2)] = "NA"
      }

      by = factor(str.by, levels = quant.levels)
      levels = levels(by)

    }
  }


  table = as.data.frame(unique(levels))
  names(table) = "levels"

  data = as.data.frame(data)

  used = NULL

  for (i in 1:(length(unique(levels)))) {

    subset = data[which(by == levels[i] ), ]
    pred = predict(model, subset)
    target.sub = target[which(by == levels[i] )]


    table$n[i] = length(pred)
    table[i, "R^2"] = round(1 - (sum((pred - target.sub)^2)/sum((target.sub - mean(target.sub))^2)), 3)
    if(table$`R^2`[i] < 0){
      table$`R^2`[i] = 0
    }
    table$RMSE[i] = round(sqrt(mean((target.sub - pred) ^ 2)), 3)
    table$real.mean[i] = round(mean(target.sub), 2)
    table$real.median[i] = round(stats::median(target.sub), 2)
    table$real.IQR[i] = round(stats::quantile(target.sub, 0.75) - stats::quantile(target.sub, 0.25), 2)
    table$pred.mean[i] = round(mean(pred), 2)
    table$pred.median[i] = round(stats::median(pred), 2)
    table$pred.IQR[i] = round(stats::quantile(pred, 0.75) - stats::quantile(pred, 0.25), 2)

  }


  return(table)

}
