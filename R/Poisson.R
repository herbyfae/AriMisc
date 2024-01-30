#' @importFrom MASS glm.nb
#' @importFrom AER dispersiontest

Poisson = function(data = NULL, dependent = NULL){

data = as.data.frame(data)

facts = Filter(is.factor, data)

tab = as.data.frame(matrix(nrow = ncol(facts)))

buffer = 0

colnames(tab) = "Factor"

if (length(dependent) == 1) {
  dependent = data[, dependent]
}

for (i in 1:ncol(facts)) {

  tab[(i + buffer), "Factor"] = colnames(facts)[i]
  tab[(i + buffer), "Level"] = paste("Ref: ", levels(facts[, i])[1])

  subset = dependent[which(facts[,i] == levels(facts[,i])[1])]

  tab[(i + buffer), "Count"] = length(subset)
  tab[(i + buffer), "Mean (sd)"] = paste(round(mean(subset, na.rm = T), 2) ,"(", round(sd(subset, na.rm = T),2), ")")

  if (length(unique(facts[!is.na(facts[, i]), i])) == 1) {

  } else{
    test = glm(dependent ~ facts[, i],
               family = poisson(link = "log"))

    d.test = dispersiontest(test)

    tab[(i + buffer), "Dispersion"] = round(d.test$estimate, 3)
    tab[(i + buffer), "OD test"] = round(d.test$p.value, 3)

    if(tab[(i + buffer), "OD test"] <= 0.05 | tab[(i + buffer), "OD test"] >= 0.95){

      test = glm(dependent ~ facts[, i],
                 family = quasipoisson(link = "log"))

      tab[(i + buffer), "Distribution"] = "QuasiPoisson"

    } else{
      tab[(i + buffer), "Distribution"] = "Poisson"
    }

    test.sum = summary(test)

    test.nb = glm.nb(dependent ~ facts[, i],
                     link = "log")

    test.nb.sum = summary(test.nb)

    for (j in 2:length(levels(facts[, i]))) {
      tab[(i + buffer + j - 1), "Level"] = levels(facts[, i])[j]

      subset = dependent[which(facts[,i] == levels(facts[,i])[j])]

      tab[(i + buffer + j - 1), "Count"] = length(subset)
      tab[(i + buffer + j - 1), "Mean (sd)"] = paste(round(mean(subset, na.rm = T), 2) , "(", round(sd(subset, na.rm = T), 2), ")")

      tab[(i + buffer + j - 1), "Estimate"] = round(test.sum$coefficients[j, 1], 3)
      tab[(i + buffer + j - 1), "p.val"] = round(test.sum$coefficients[j, 4], 3)

      tab[(i + buffer + j - 1), "EstimateNB"] = round(test.nb.sum$coefficients[j, 1], 3)
      tab[(i + buffer + j - 1), "p.valNB"] = round(test.nb.sum$coefficients[j, 4], 3)

      if (tab[(i + buffer + j - 1), "p.val"] < 0.001) {
        tab[(i + buffer + j - 1), "p.val"] = "<0.001"
        tab[(i + buffer + j - 1), "Signif"] = "***"
      } else if (tab[(i + buffer + j - 1), "p.val"] < 0.01) {
        tab[(i + buffer + j - 1), "Signif"] = "**"
      } else if (tab[(i + buffer + j - 1), "p.val"] < 0.05) {
        tab[(i + buffer + j - 1), "Signif"] = "*"
      } else if (tab[(i + buffer + j - 1), "p.val"] < 0.1) {
        tab[(i + buffer + j - 1), "Signif"] = "."
      } else{
        tab[(i + buffer + j - 1), "Signif"] = ""
      }

      if (tab[(i + buffer + j - 1), "p.valNB"] < 0.001) {
        tab[(i + buffer + j - 1), "p.valNB"] = "<0.001"
        tab[(i + buffer + j - 1), "SignifNB"] = "***"
      } else if (tab[(i + buffer + j - 1), "p.valNB"] < 0.01) {
        tab[(i + buffer + j - 1), "SignifNB"] = "**"
      } else if (tab[(i + buffer + j - 1), "p.valNB"] < 0.05) {
        tab[(i + buffer + j - 1), "SignifNB"] = "*"
      } else if (tab[(i + buffer + j - 1), "p.valNB"] < 0.1) {
        tab[(i + buffer + j - 1), "SignifNB"] = "."
      } else{
        tab[(i + buffer + j - 1), "SignifNB"] = ""
      }

    }

    if (any(tab[(i + buffer + 1:(j - 1)), "Signif"] != "")) {
      tab[(i + buffer), "Signif"] = "*"
    } else {
      tab[(i + buffer), "Signif"] = ""
    }

    if (any(tab[(i + buffer + 1:(j - 1)), "SignifNB"] != "")) {
      tab[(i + buffer), "SignifNB"] = "*"
    } else {
      tab[(i + buffer), "SignifNB"] = ""
    }

    buffer = buffer + j - 1

  }
}

return(tab)

}
