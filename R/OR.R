#
# OR = function(data = NULL,
#               dep = NULL,
#               controls = NULL,
#               kable = F){
# }
#
#
#
# TabOR = as.data.frame(matrix(ncol = 4))
# line = 1
# colnames(TabOR) = c("Ref: class2, n = 1253", colnames(Tab3)[c(10,12:13)])
#
# for(i in 1:ncol(Tab4_dat)) {
#   if (colnames(Tab4_dat)[i] == "Classes") {
#   } else{
#     TabOR[line, 1] = colnames(Tab4_dat)[i]
#
#     mnom = multinom(Tab4_dat$Classes ~ Tab4_dat[, i])
#     OR = as.data.frame(tidy(mnom, conf.int = T, exponentiate = T))[, c(3, 6:8)]
#
#     if (is.factor(Tab4_dat[, i]) |
#         length(table(Tab4_dat[, i])) == 2) {
#       Tab4_dat[, i] = as.factor(Tab4_dat[, i])
#       for (c in 2:4) {
#         for (k in 2:length(levels(Tab4_dat[, i]))) {
#           TabOR[line + k - 1, 1] = paste(levels(Tab4_dat[, i])[1], " vs ", levels(Tab4_dat[, i])[k], sep = "")
#
#           TabOR[line + k - 1, c] = paste(
#             round(OR$estimate[k + (c - 2) * length(levels(Tab4_dat[, i]))], 3),
#             " [",
#             round(OR$conf.low[k + (c - 2) * length(levels(Tab4_dat[, i]))], 3),
#             " - ",
#             round(OR$conf.high[k + (c - 2) * length(levels(Tab4_dat[, i]))], 3),
#             "]",
#             sep = ""
#           )
#
#           if(is.nan(OR$p.value[k + (c - 2) * length(levels(Tab4_dat[, i]))])){
#           } else if (OR$p.value[k + (c - 2) * length(levels(Tab4_dat[, i]))] < 0.05) {
#             TabOR[line + k - 1, c] = paste(TabOR[line + k - 1, c], "*")
#           }
#         }
#       }
#     } else{
#       for (c in 2:4) {
#         k = 2
#         TabOR[line, c] = paste(
#           round(OR$estimate[2 + ((c - 2) * 2)], 3),
#           " [",
#           round(OR$conf.low[2 + ((c - 2) * 2)], 3),
#           " - ",
#           round(OR$conf.high[2 + ((c - 2) * 2)], 3),
#           "]",
#           sep = ""
#         )
#         if(is.nan(OR$p.value[2 + ((c - 2) * 2)])){
#         }TabOR = as.data.frame(matrix(ncol = 4))
# line = 1
# colnames(TabOR) = c("Ref: class2, n = 1253", colnames(Tab3)[c(10,12:13)])
#
# for(i in 1:ncol(Tab4_dat)) {
#   if (colnames(Tab4_dat)[i] == "Classes") {
#   } else{
#     TabOR[line, 1] = colnames(Tab4_dat)[i]
#
#     mnom = multinom(Tab4_dat$Classes ~ Tab4_dat[, i])
#     OR = as.data.frame(tidy(mnom, conf.int = T, exponentiate = T))[, c(3, 6:8)]
#
#     if (is.factor(Tab4_dat[, i]) |
#         length(table(Tab4_dat[, i])) == 2) {
#       Tab4_dat[, i] = paste(as.factor(Tab4_dat[, i]), ", ref: ", levels(Tab4_dat[, i])[1])
#       for (c in 2:4) {
#         for (k in 2:length(levels(Tab4_dat[, i]))) {
#           TabOR[line + k - 1, 1] = paste(levels(Tab4_dat[, i])[1], " vs ", levels(Tab4_dat[, i])[k], sep = "")
#
#           TabOR[line + k - 1, c] = paste(
#             round(OR$estimate[k + (c - 2) * length(levels(Tab4_dat[, i]))], 3),
#             " [",
#             round(OR$conf.low[k + (c - 2) * length(levels(Tab4_dat[, i]))], 3),
#             " - ",
#             round(OR$conf.high[k + (c - 2) * length(levels(Tab4_dat[, i]))], 3),
#             "]",
#             sep = ""
#           )
#
#           if(is.nan(OR$p.value[k + (c - 2) * length(levels(Tab4_dat[, i]))])){
#           } else if (OR$p.value[k + (c - 2) * length(levels(Tab4_dat[, i]))] < 0.05) {
#             TabOR[line + k - 1, c] = paste(TabOR[line + k - 1, c], "*")
#           }
#         }
#       }
#     } else{
#       for (c in 2:4) {
#         k = 2
#         TabOR[line, c] = paste(
#           round(OR$estimate[2 + ((c - 2) * 2)], 3),
#           " [",
#           round(OR$conf.low[2 + ((c - 2) * 2)], 3),
#           " - ",
#           round(OR$conf.high[2 + ((c - 2) * 2)], 3),
#           "]",
#           sep = ""
#         )
#           if(is.nan(OR$p.value[2 + ((c - 2) * 2)])){
#           }else if (OR$p.value[2 + ((c - 2) * 2)] < 0.001) {
#             TabOR[line, c] = paste(TabOR[line, c], "***")
#           }else if (OR$p.value[2 + ((c - 2) * 2)] < 0.01) {
#             TabOR[line, c] = paste(TabOR[line, c], "**")
#           }else if (OR$p.value[2 + ((c - 2) * 2)] < 0.05) {
#             TabOR[line, c] = paste(TabOR[line, c], "*")
#           }else if (OR$p.value[2 + ((c - 2) * 2)] < 0.1) {
#             TabOR[line, c] = paste(TabOR[line, c], ".")
#           }
#       }
#       k = 1
#     }
#     line = line + k
#   }
# }
#
# kable(TabOR, align = "lccc")
#       }
#       k = 1
#     }
#     line = line + k
#   }
# }
#
# kable(TabOR, align = "lccc")
