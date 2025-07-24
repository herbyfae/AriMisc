missinglevel = function(factor1, factor2){

  tally = NULL; missing = NULL; missingcycle = NULL

  factor1 = as.factor(factor1); factor2 = as.factor(factor2)
  levels = levels(factor1) # Using the first factor as a base

  for(i in 1:length(levels)){

    if(sum(factor2 == levels[i]) > 0){
      tally = c(tally, levels[i]) # Tallying matches
    } else {
      missing = c(missing, levels[i]) # Compiling levels on the 1st factor not on the 2nd
      missingcycle = c(missingcycle, which(factor1 == levels[i]))
      # If level "i" isn't detected on the 2nd factor -> compile all its instances on the first factor
    }
  }

  if(is.null(tally)){
    stop("No matches found, are you sure about what you introduced?")
  }

  # Looking for unused levels on the 2nd factor
  unusedlevels = levels(factor2)
  for(i in 1:length(tally)){
    unusedlevels = unusedlevels[grep(as.character(tally[i]), unusedlevels, invert=T)]
  }
    # Why don't I just filter off tally?
     # Whatever NAs were present would've been signaled the same as an unused level
     # I'm specifically looking for levels, so NAs are effectively ignored

  missingfactor2 = which(!is.na(factor(as.character(factor2), levels = unusedlevels)))

  tally = unique(tally)

  if(is.null(tally)){
    stop("No matches found, are you sure about what you introduced?")
  }

  # % of levels matched from those in the first factor
   # Could've done globally, but I think using the 1st factor as a baseline makes more sense
   # Not like these warnings are of much interest anyways
  if(length(tally) < length(unique(c(tally, missing)))/4){
    warning("Less than 25% of possible 1st factor levels were matched")
  } else if(length(tally) < length(unique(c(tally, missing)))/2){
    warning("Less than 50% of possible 1st factor levels were matched")
  }

  return(list("matching levels" = tally,
              "non-matching1" = unique(missing),
              "non-matching2" = unusedlevels,
              "non-matching index1" = missingcycle,
              "non-matching index2" = missingfactor2)
  )
}
