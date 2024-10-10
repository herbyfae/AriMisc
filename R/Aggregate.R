Aggregate = function(data,
                     by = NULL,
                     quantiles = NULL,
                     breaks.num = NULL,
                     measures = "core",
                     multistr = NULL)
{

  if (is.null(by)) {
    stop("No by, where am I supposed to be aggregating from?")
  }

  data = as.data.frame(data)
  skip = F

  # by processing

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
  if (is.character(by)) {
    if(sum(is.na(suppressWarnings(as.numeric(by))) & !is.na(by)) == 0){
      if(sum(is.na(by)) > 0){
        by[is.na(by)] = "NA"
        by = factor(by, levels = c(unique(by[order(as.numeric(by))])))
        warning("Given by has NAs - converted to a separate subset")
      } else{
        by = factor(by, levels = unique(by[order(as.numeric(by))]))
      }
      skip = T

    }else{
      by = as.factor(by)
    }
  }

  if (is.factor(by)) {

    if(sum(is.na(by))>0 & skip == F){
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

  # frame setup


  buffer = 1
  buffer_temp = buffer

  dim = length(levels)

  frame = as.data.frame(matrix(nrow = 1, ncol = dim+2))

  colnames(frame)[1:2] = c("Column", "Measure")

  for(k in 1:dim){
    length(which(by == levels[k]))
    colnames(frame)[(k+2)] = paste(levels[k], ", n=", length(which(by == levels[k])), sep = "")
  }

  names = colnames(data)

  ## measure formatting

  measures = tolower(measures)

  cat.type = "perline"
  cat.k = 3

  if(sum(grepl("\\bcat", measures))>1){
    stop("Include only one mention of the number of columns/types of processing for categorical variables")
  } else if(sum(grepl("\\bcat", measures)) == 1){

    cat.index = grep("\\bcat", measures)

    if(grepl("all", measures[cat.index])){
      cat.k = "all"
    } else if(gsub("\\D", "", measures[cat.index]) != ""){
      cat.k = as.numeric(gsub("\\D", "", measures[cat.index]))
    }

    if(length(strsplit(measures[cat.index], "_")[[1]]) == 2){
      cat.type = strsplit(measures[cat.index], "_")[[1]][2]
    } else if(length(strsplit(measures[cat.index], "_")[[1]]) > 2){
      stop("Check the structure of you 'cat_' call")
    }

    measures = measures[-cat.index]

  }

  m.quantiles = as.numeric(measures[grep("[0-9]", measures)])

  if(length(m.quantiles) == 0){
    m.quantiles = NULL
  } else if(sum(m.quantiles < 0) > 0|sum(m.quantiles >100) > 0){
    stop("Detected a number under 0 or over 100 in the measures - Unable to convert to quantiles")
  } else if(sum(m.quantiles >= 1) > 0){
    m.quantiles = m.quantiles/100
  }

  # Tally of every possible num call
  # So, anything without cat or bin markers

  num_measures = measures[!(grepl("\\bcat", measures)| grepl("\\bbin", measures))]
  cat_measures = measures[(grepl("\\bcat", measures)| grepl("na", measures))]
  bin_measures = measures[(grepl("\\bbin", measures)| grepl("na", measures))]


  if(sum(grepl("ext", num_measures)) > 0){
    num_measures = num_measures[!(grepl("ext", measures))]
    num_measures = c(num_measures, "min", "max")
  }

  ## Multistr columns

  multistr_ind = NULL

  if(!is.null(multistr)){
    if(is.list(multistr)){
      for(l in 1:length(multistr)){
        multistr[[l]] = tolower(multistr[[l]])
        multistr_ind = c(multistr_ind, multistr[[l]][grep("[^0-9]", multistr[[l]], invert = T)])
      }
    }else{
      multistr = tolower(multistr)
      multistr_ind = multistr[grep("[^0-9]", multistr, invert = T)]
    }
  }

  # Main loop

  for (i in 1:ncol(data)) {
    # Per original column

    buffer = buffer_temp
    # So what is this whole temp thing supposed to do?
    # The main buffer sets a starting line for the block(measures of one original column)
    # The temporary one accounts for and sets the specific lines
    # Alternatively, it sets the line and is always primed for the next operation
    # At the end of a subset, the temporary one resets to the starting line
    # At the final subset, it goes back to this command, setting a new starting line


    # Setting the name of the block of operations as that of the original column
    frame[buffer, 1] = names[i]

    for (k in 1:dim) {
      # Per level of by(new columns 2:length(levels))

      buffer_temp = buffer
      subset = data[which(by == levels[k]), i]

      if (is.numeric(data[, i]) &
          length(table(data[, i])) > 2) {
        # For numeric variables
        # Actually translating the measures

        #Core measures
        if (sum(grepl("core", num_measures)) > 0) {
          frame[buffer_temp, k + 2] = paste(round(mean(subset, na.rm = T), 2), " (", round(sd(subset, na.rm = T), 3), ")", sep = "")
          frame[buffer_temp + 1, k + 2] = paste(round(quantile(subset, 0.25, na.rm = T), 2),
                                                round(median(subset, na.rm = T), 2),
                                                round(quantile(subset, 0.75, na.rm = T), 2),
                                                sep = " - ")

          frame[(buffer_temp + 0:1), 2] = c("Mean (sd)", "25Q - Med - 75Q")

          if (sum(is.na(data[, i])) == 0 &
              (sum(grepl("\\brelev_na\\b", num_measures)) == 1 | sum(grepl("\\bna\\b", num_measures)) == 0)) {
            buffer_temp = buffer_temp + 2
          } else{
            frame[buffer_temp + 2, k + 2] = round(sum(is.na(subset)), 2)
            frame[buffer_temp + 2, 2] = "NA"
            buffer_temp = buffer_temp + 3
          }
        }
        # Loose measures
        if (sum(grepl("mean", num_measures)) > 0) {
          frame[buffer_temp, k + 2] = round(mean(subset, na.rm = T), 2)
          frame[buffer_temp, 2] = "Mean"
          buffer_temp = buffer_temp + 1
        }
        if (sum(grepl("median", num_measures)) > 0) {
          frame[buffer_temp, k + 2] = round(median(subset, na.rm = T), 2)
          frame[buffer_temp, 2] = "Median"
          buffer_temp = buffer_temp + 1
        }
        if (sum(grepl("sd", num_measures)) > 0) {
          frame[buffer_temp, k + 2] = round(sd(subset, na.rm = T), 2)
          frame[buffer_temp, 2] = "SD"
          buffer_temp = buffer_temp + 1
        }
        if (sum(grepl("var", num_measures)) > 0) {
          frame[buffer_temp, k + 2] = round(var(subset, na.rm = T), 2)
          frame[buffer_temp, 2] = "Var"
          buffer_temp = buffer_temp + 1
        }
        if (sum(grepl("mad", num_measures)) > 0) {
          frame[buffer_temp, k + 2] = round(mad(subset, na.rm = T), 2)
          frame[buffer_temp, 2] = "MAD"
          buffer_temp = buffer_temp + 1
        }
        if (sum(grepl("range", num_measures)) > 0) {
          frame[buffer_temp, k + 2] = paste(round(min(subset, na.rm = T), 2), round(max(subset, na.rm = T), 2), sep = " - ")
          frame[buffer_temp, 2] = "Range"
          buffer_temp = buffer_temp + 1
        }
        if (sum(grepl("min", num_measures)) > 0) {
          frame[buffer_temp, k + 2] = round(min(subset, na.rm = T), 2)
          frame[buffer_temp, 2] = "Min"
          buffer_temp = buffer_temp + 1
        }
        if (length(m.quantiles) > 0) {
          for (m in 1:length(m.quantiles)) {
            frame[buffer_temp + m - 1, k + 2] = round(quantile(subset, m.quantiles[m], na.rm = T), 2)
            frame[buffer_temp + m - 1, 2] = paste("Q", m.quantiles[m], sep = "_")
          }
          buffer_temp = buffer_temp + m
        }
        if (sum(grepl("max", num_measures)) > 0) {
          frame[buffer_temp, k + 2] = round(max(subset, na.rm = T), 2)
          frame[buffer_temp, 2] = "Max"
          buffer_temp = buffer_temp + 1
        }
        if (sum(grepl("iqr", num_measures)) > 0) {
          frame[buffer_temp, k + 2] = round(quantile(subset, 0.75, na.rm = T) - quantile(subset, 0.25, na.rm = T), 2)
          frame[buffer_temp, 2] = "IQR"
          buffer_temp = buffer_temp + 1
        }
        if (sum(grepl("relev_na", num_measures)) > 0 &
            sum(is.na(data[, i])) > 0) {
          frame[buffer_temp, k + 2] = sum(is.na(subset))
          frame[buffer_temp, 2] = "NA"
          buffer_temp = buffer_temp + 1
        }
        # Scuffed way to avoid double dipping NAs with core
        if (sum(grepl("\\bna\\b", num_measures)) > 0 & sum(grepl("core", num_measures)) == 0) {
          frame[buffer_temp, k + 2] = sum(is.na(subset))
          frame[buffer_temp, 2] = "NA"
          buffer_temp = buffer_temp + 1
        }


      } else if (length(table(data[, i])) <= 2) {
        ## Binary

        if (sum(grepl("bin_refhigh", bin_measures)) > 0) {
          data[, i] = as.factor(data[, i])
          tag = levels(data[, i])[2]
        } else if (sum(grepl("bin_reflow", bin_measures)) > 0) {
          data[, i] = as.factor(data[, i])
          tag = levels(data[, i])[1]
        } else{
          tag = names(table(data[, i])[order(table(data[, i]), decreasing = T)][1])
        }

        if (sum(grepl("bin_count", bin_measures)) > 0) {
          frame[buffer_temp, k + 2] = sum(subset == as.character(tag), na.rm = T)
          frame[buffer_temp, 2] = as.character(tag)
          buffer_temp = buffer_temp + 1
        } else{
          frame[buffer_temp, k + 2] = round((sum(
            subset == as.character(tag), na.rm = T
          ) / length(subset[!is.na(subset)])) * 100, 2)
          frame[buffer_temp, 2] = paste(as.character(tag), "%", sep = "")
          buffer_temp = buffer_temp + 1
        }

        if (sum(is.na(data[, i])) == 0 &
            (sum(grepl("\\brelev_na\\b", bin_measures)) == 1 | sum(grepl("\\na\\b", bin_measures)) == 0)) {

        } else{
          frame[buffer_temp, k + 2] = round(sum(is.na(subset)), 2)
          frame[buffer_temp, 2] = "NA"
          buffer_temp = buffer_temp + 1
        }
      } else{
        # Categorical/factors and multistring stuff

        ## Multistr setup
        ### Settings
        if (i %in% multistr_ind) {
          # Is the column given in multistr?
          if (is.list(multistr)) {
            # Is multistr a list?
            for (l in 1:length(multistr)) {
              # If it is where is the column?
              if (i %in% multistr[[l]]) {
                settings = multistr[[l]]
                break
              }
            }
            multistr_cols = settings[grep("count|perc", settings)] # Anything containing count or perc
            if (length(multistr_cols) > 1) {
              stop(paste("Multiple settings in multistr:", multistr_cols))
            }

            # Breaking up the string into columns proper and display(count/perc)
            multistr_cols = strsplit(multistr_cols, "_")[[1]]

            multistr_display = multistr_cols[2]
            multistr_cols = multistr_cols[1]

            multistr_breaks = settings[grep("[0-9]|mode", settings, invert = T)] # Anything without numbers
            multistr_mode = "mode" %in% settings

          } else{
            # If multistr wasn't a list
            multistr_cols = multistr[grep("count|perc", multistr)]
            if (length(multistr_cols) > 1) {
              stop(paste("Multiple settings in multistr:", multistr_cols))
            }
            multistr_cols = strsplit(multistr_cols, "_")[[1]]
            multistr_display = multistr_cols[2]
            multistr_cols = multistr_cols[1]
            multistr_breaks = multistr[grep("[0-9]|mode|all", multistr, invert = T)]
            multistr_mode = "mode" %in% multistr
          }
          ### Breaking up the column

          #Total data -> Level order + counts/percs
          # Yes it shouldve been on the beggining of the loop, something for a future version

          multistr_data = NULL
          for (a in 1:length(data[, i])) {
            multistr_data = c(multistr_data, unique(trimws(strsplit(data[a, i], paste(multistr_breaks, collapse = "|"))[[1]])))
          }

          #Subset data -> mode and level matching + counts/percs
          multistr_subset = NULL
          for (b in 1:length(subset)) {
            multistr_subset = c(multistr_subset, unique(trimws(strsplit(subset[b], paste(multistr_breaks, collapse = "|"))[[1]])))
          }


          ### Copying the character arguments with different names

          buffer_temp = buffer

          if (multistr_mode) {
            frame[buffer_temp, 2] = "Mode"
            frame[buffer_temp, k + 2] = names(table(multistr_subset, useNA = "no")[order(table(multistr_subset, useNA = "no"), decreasing = T)][1])
            buffer_temp = buffer_temp + 1

          }

          if (multistr_cols == "all") {

            tag = names(table(multistr_data, useNA = "no")[order(table(multistr_data, useNA = "no"), decreasing = T)])

          } else if (!grepl("[^0-9]", multistr_cols)) { #If there is nothing but numbers there

            multistr_cols = as.numeric(multistr_cols)
            tag = names(table(multistr_data, useNA = "no")[order(table(multistr_data, useNA = "no"), decreasing = T)][1:multistr_cols])
            tag = tag[!is.na(tag)] # To remove excess(e.g. let until 50 but there only were 10 varieties)

          } else{
            stop(
              paste(
                "Given neither a number or \"all\" to set multistr columns. Given:",
                multistr_cols
              )
            )
          }

          if (multistr_display == "count") {
            for (t in 1:length(tag)) {
              frame[buffer_temp + t - 1, 2] = paste(tag[t], sum(multistr_data == tag[t], na.rm = T), sep = ": ") # Total representation
              frame[buffer_temp + t - 1, k + 2] = sum(multistr_subset == tag[t], na.rm = T) # In the subset
            }
            buffer_temp = buffer_temp + t
          }
          if (multistr_display == "perline") {
            for (t in 1:length(tag)) {
              frame[buffer_temp + t - 1, 2] = paste(tag[t], round((sum(multistr_data == tag[t], na.rm = T) /
                                                                     length(data[, i]))*100,2), sep = ": ")
              frame[buffer_temp + t - 1, k + 2] = round((
                sum(multistr_subset == tag[t], na.rm = T) / length(subset)
              ) * 100, 2)
            }
            buffer_temp = buffer_temp + t
          }
          if (multistr_display == "perc") {
            for (t in 1:length(tag)) {
              frame[buffer_temp + t - 1, 2] = paste(tag[t], (
                sum(multistr_data == tag[t], na.rm = T) / length(multistr_data)
              ) * 100, sep = ": ")
              frame[buffer_temp + t - 1, k + 2] = paste(tag[t], (
                sum(multistr_subset == tag[t], na.rm = T) / length(multistr_subset)
              ) * 100, sep = ": ")
            }
            buffer_temp = buffer_temp + t
          }


        } else{
          # Just reformatting the rest

          buffer_temp = buffer

          if (sum(grepl("mode", measures)) == 1) {
            frame[buffer_temp, 2] = "Mode"
            frame[buffer_temp, k+2] = names(table(subset, useNA = "no")[order(table(subset, useNA = "no"), decreasing = T)][1])
            buffer_temp = buffer_temp + 1
          }

          if (is.numeric(cat.k)) {
            if (cat.k == 0) {
              tag = NULL
            } else{
              tag = names(table(data[, i], useNA = "no")[order(table(data[, i], useNA = "no"), decreasing = T)][1:cat.k])
              tag = tag[!is.na(tag)]
            }
          } else if (cat.k == "all") {
            tag = names(table(data[, i], useNA = "no")[order(table(data[, i], useNA = "no"), decreasing = T)])

          } else{
            stop("Invalid number of columns given for categorical variables")
          }

          if(is.null(tag)){
          } else{
            for (m in 1:length(tag)) {
              if (cat.type == "count") {
                frame[buffer_temp + m - 1, 2] = paste(tag[m], sum(data[, i] == as.character(tag[m]), na.rm = T), sep = ": ")
                frame[buffer_temp + m - 1, k + 2] = sum(subset == as.character(tag[m]), na.rm = T)
              }
              if (cat.type == "perline") {
                frame[buffer_temp + m - 1, 2] = paste(tag[m], round(sum(data[, i] == as.character(tag[m]), na.rm = T) /
                                                                      length(data[, i]) * 100, 2), sep = ": ")
                frame[buffer_temp + m - 1, k + 2] = round((sum(subset == as.character(tag[m]), na.rm = T) /
                                                             length(subset)) * 100, 2)
              }
            }
            buffer_temp = buffer_temp + m
          }

          if (sum(is.na(data[, i])) > 0 &
              (sum(grepl("\\bna\\b", measures)) == 0 |
               sum(grepl("relev_na", measures)) == 1)) {
            frame[buffer_temp, 2] = "NA"
            frame[buffer_temp, k+2] = round(sum(is.na(subset)), 2)
            buffer_temp = buffer_temp + 1

          } else if (sum(grepl("\\bna\\b", measures)) > 0) {
            frame[buffer_temp, 2] = "NA"
            frame[buffer_temp, k+2] = round(sum(is.na(subset)), 2)
            buffer_temp = buffer_temp + 1
          }
        }
      }
    }
  }
  return(as.data.frame(frame))
}
