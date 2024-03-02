#' @importFrom sf st_as_sf
#' @importFrom sp merge
#' @importFrom tmap tm_shape tm_fill

MatrixVis = function(prediction = NULL,
                     target = NULL,
                     model = NULL,
                     data = NULL,
                     by = NULL,
                     quantiles = NULL,
                     breaks.num = NULL,
                     ext.summary = F,
                     detailed = F,
                     map.data = NULL,
                     breaks = NULL,
                     ask = F) {

  if (is.null(by)) {
    if(is.null(target)){
      stop("No reference to compare the predictions to")
    }

    target = as.factor(target)
    View = as.data.frame(table(target, prediction, dnn = c("Class", "Prediction")))

    table = as.data.frame(levels(target))
    colnames(table) = "Target"

    for (i in 1:length(levels(target))) {
      pos = levels(target)[i]
      itr.table = matrix(NA, 2, 2)
      colnames(itr.table) <- rownames(itr.table) <- c("pos", "neg")

      itr.table[1, 1] = sum(View[which(View$Class == pos &
                                         View$Prediction == pos), 3]) # True pos
      itr.table[2, 1] = sum(View[which(View$Class == pos &
                                         View$Prediction != pos), 3]) # False neg
      itr.table[1, 2] = sum(View[which(View$Class != pos &
                                         View$Prediction == pos), 3]) # False pos
      itr.table[2, 2] = sum(View[which(View$Class != pos &
                                         View$Prediction != pos), 3]) # True neg

      TotalPred = sum(itr.table[1, ])
      TrueClass = sum(itr.table[, 1])
      Prevalence = round(TrueClass / length(target), 3)

      Specificity = round(itr.table[2, 2] / sum(itr.table[,2]), 3)
      Recall = ifelse(TrueClass > 0, round(itr.table[1, 1] / TrueClass, 3), NA)
      Precision = ifelse(TotalPred > 0, round(itr.table[1, 1] / TotalPred, 3), NA)
      F1 = ifelse(Recall > 0 |
                    Precision > 0, round((2 * Recall * Precision) / (Recall + Precision), 3), NA)
      Accuracy = round((itr.table[1, 1] + itr.table[2, 2]) / length(target), 3)
      BalAccuracy = round((Recall+Specificity)/2, 3)

      table$CorrectPred[i] = itr.table[1, 1]
      table$FalsePos[i] = itr.table[1, 2]
      table$FalseNeg[i] = itr.table[2, 1]
      table$Prevalence[i] = Prevalence
      table$TotalPred[i] = TotalPred
      table$Accuracy[i] = Accuracy
      table$BalAccuracy[i] = BalAccuracy
      table$Specificity[i] = Specificity
      table$Recall[i] = Recall
      table$Precision[i] = Precision
      table$F1[i] = F1

    }

    BER = sum(1 - table$Accuracy) / nrow(table)

  }
  else{
    target = as.factor(target)
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

        str.by[which(by <= quantile(by, quantiles[1], na.rm  = T))] = paste("<=Q", quantiles[1], sep = "")
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

    table.res = as.data.frame(levels)
    table.list = list(NULL)

    for (j in 1:length(levels)) {
      subset = data[which(by == levels[j]),]
      pred = predict(model, subset)
      target.sub = target[which(by == levels[j])]

      table.sub = as.data.frame(levels(target.sub))
      colnames(table.sub) = "Target"

      View = as.data.frame(table(target.sub, pred, dnn = c("Class", "Prediction")))

      for (i in 1:length(levels(target.sub))) {
        pos = levels(target.sub)[i]
        itr.table = matrix(NA, 2, 2)
        colnames(itr.table) <-
          rownames(itr.table) <- c("pos", "neg")

        itr.table[1, 1] = sum(View[which(View$Class == pos &
                                           View$Prediction == pos), 3]) # True pos
        itr.table[2, 1] = sum(View[which(View$Class == pos &
                                           View$Prediction != pos), 3]) # False neg
        itr.table[1, 2] = sum(View[which(View$Class != pos &
                                           View$Prediction == pos), 3]) # False pos
        itr.table[2, 2] = sum(View[which(View$Class != pos &
                                           View$Prediction != pos), 3]) # True neg

        TotalPred = sum(itr.table[1, ])
        TrueClass = sum(itr.table[, 1])
        Prevalence = round(TrueClass / length(target.sub), 3)

        Specificity = round(itr.table[2, 2] / sum(itr.table[, 2]), 3)
        Recall = ifelse(TrueClass > 0, round(itr.table[1, 1] / TrueClass, 3), NA)
        Precision = ifelse(TotalPred > 0, round(itr.table[1, 1] / TotalPred, 3), NA)
        F1 = ifelse(Recall > 0 |
                      Precision > 0, round((2 * Recall * Precision) / (Recall + Precision), 3), NA)
        Accuracy = round((itr.table[1, 1] + itr.table[2, 2]) / sum(sum(itr.table[1, ]), sum(itr.table[2, ])), 3)
        BalAccuracy = round((Recall+Specificity)/2, 3)

        table.sub$CorrectPred[i] = itr.table[1, 1]
        table.sub$FalsePos[i] = itr.table[1, 2]
        table.sub$FalseNeg[i] = itr.table[2, 1]
        table.sub$Prevalence[i] = Prevalence
        table.sub$TotalPred[i] = TotalPred
        table.sub$Accuracy[i] = Accuracy
        table.sub$BalAccuracy[i] = BalAccuracy
        table.sub$Specificity[i] = Specificity
        table.sub$Recall[i] = Recall
        table.sub$Precision[i] = Precision
        table.sub$F1[i] = F1

        table.sub$by = levels[j]

        table.list[[j]] = table.sub[,-ncol(table.sub)]

      }

      BER = sum(1 - table.sub$Accuracy) / nrow(table.sub)

      table.res$CorrectPredTotal[j] = sum(table.sub$CorrectPred)
      table.res$IncorrectPredTotal[j] = sum(table.sub$FalseNeg)

      table.res$n[j] = sum(table.sub$TotalPred)

      if(ext.summary){

        for(k in 1:length(levels(target.sub))){
          table.res[j, paste("Prevalence", levels(target.sub)[k], sep = "_")] = table.sub$Prevalence[k]
        }

        table.res$MeanRecall[j] = round(mean(table.sub$Recall, na.rm = T), 3)
        table.res$RecallRange[j] = paste(round(min(table.sub$Recall, na.rm = T),2), round(max(table.sub$Recall, na.rm = T),2), sep = "-")

        table.res$MeanPrecision[j] = round(mean(table.sub$Precision, na.rm = T), 3)
        table.res$PrecisionRange[j] = paste(round(min(table.sub$Precision, na.rm = T),2), round(max(table.sub$Precision, na.rm = T),2), sep = "-")

        table.res$MeanSpecificity[j] = round(mean(table.sub$Specificity, na.rm = T), 3)
        table.res$SpecificityRange[j] = paste(round(min(table.sub$Specificity, na.rm = T),2), round(max(table.sub$Specificity, na.rm = T),2), sep = "-")

        table.res$MeanF1[j] = round(mean(table.sub$F1, na.rm = T), 3)
        table.res$F1Range[j] = paste(round(min(table.sub$F1, na.rm = T),2), round(max(table.sub$F1, na.rm = T),2), sep = "-")

        table.res$BER[j] = BER

      }else{

        table.res$MeanRecall[j] = round(mean(table.sub$Recall, na.rm = T), 3)
        table.res$MeanPrecision[j] = round(mean(table.sub$Precision, na.rm = T), 3)
        table.res$MeanSpecificity[j] = round(mean(table.sub$Specificity, na.rm = T), 3)
        table.res$MeanF1[j] = round(mean(table.sub$F1, na.rm = T), 3)

        table.res$BER[j] = BER

      }

    }

    names(table.list) = as.character(levels)

  }

  if (!is.null(map.data)) {

    graphics::par(ask = F)
    if (ask) {
      graphics::par(ask = T)
    }

    map.data = as.data.frame(map.data)

    if (!is.null(by)) {

      colnames(table.res)[1] = colnames(map.data)[1]
      map.data = sp::merge(table.res, map.data)
      map.data = sf::st_as_sf(map.data)

      base = tmap::tm_shape(map.data)
      n = base + tmap::tm_fill(col = "n", breaks = breaks)
      correctpredictions = base + tmap::tm_fill(col = "CorrectPredTotal", breaks = breaks)
      incorrectpredictions = base + tmap::tm_fill(col = "IncorrectPredTotal", breaks = breaks)
      recall = base + tmap::tm_fill(col = "MeanRecall")
      precision = base + tmap::tm_fill(col = "MeanPrecision")
      specificity = base + tmap::tm_fill(col = "MeanSpecificity")
      F1 = base + tmap::tm_fill(col = "MeanF1")
      BER = base + tmap::tm_fill(col = "BER")

    } else{
      colnames(table)[1] = colnames(map.data)[1]
      map.data = sp::merge(table, map.data, by = intersect(rownames(table), map.data[, 1]))
      map.data = sf::st_as_sf(map.data)

      base = tmap::tm_shape(map.data)

      prevalence = base + tmap::tm_fill(col = "Prevalence")
      totalpredictions = base + tmap::tm_fill(col = "TotalPred", breaks = breaks)
      correctpredictions = base + tmap::tm_fill(col = "CorrectPred", breaks = breaks)
      recall = base + tmap::tm_fill(col = "Recall")
      precision = base + tmap::tm_fill(col = "Precision")
      specificity = base + tmap::tm_fill(col = "Specificity")
      accuracy = base + tmap::tm_fill(col = "Accuracy")
      F1 = base + tmap::tm_fill(col = "F1")

    }

    if (!is.null(by)) {
      if (detailed) {
        return(
          list(
            "Tablelist" = table.list,
            "ShortTable" = table.res,
            "MeanBER" = round(mean(table.res$BER), 3),
            "plots" = list(
              "n" = list(n),
              "CorrectPredictions" = list(correctpredictions),
              "IncorrectPredictions" = list(incorrectpredictions),
              "MeanRecall" = list(recall),
              "MeanPrecision" = list(precision),
              "MeanSpecificity" = list(specificity),
              "MeanF1" = list(F1),
              "BER" = list(BER)
            )
          )
        )
      } else{
        return(list(
          "ShortTable" = table.res,
          "MeanBER" = round(mean(table.res$BER), 3),
          "plots" = list(
            "n" = list(n),
            "CorrectPredictions" = list(correctpredictions),
            "IncorrectPredictions" = list(incorrectpredictions),
            "MeanRecall" = list(recall),
            "MeanPrecision" = list(precision),
            "MeanSpecificity" = list(specificity),
            "MeanF1" = list(F1),
            "BER" = list(BER)
          )
        ))
      }

    } else{
      return(list(
        "table" = table,
        "BER" = BER,
        "plots" = list(
          "TrueClass" = list(trueclass),
          "TotalPredictions" = list(totalpredictions),
          "CorrectPredictions" = list(correctpredictions),
          "Recall" = list(recall),
          "Precision" = list(precision),
          "Specificity" = list(specificity),
          "Accuracy" = list(accuracy),
          "F1" = list(F1)
        )
      ))
    }
    graphics::par(ask = F)

  }
  else{
    if (!is.null(by)) {
      if (detailed) {
        return(list(
          "Fulllist" = table.list,
          "ShortTable" = table.res,
          "MeanBER" = round(mean(table.res$BER), 3)
        ))
      } else{
        return(list(
          "ShortTable" = table.res,
          "MeanBER" = round(mean(table.res$BER), 3)
        ))
      }

    } else{
      return(list("table" = table, "BER" = BER))
    }
  }
}
