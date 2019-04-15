na_count <-sapply(data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count <- na_count[order(-na_count),,drop = FALSE]
na_count <- na_count[na_count>0,,drop = F]

#Visualization
library(VIM)
# barMiss(data, selection = "all")
# histMiss(data, interactive = TRUE)
# matrixplot(data)
aggr(subset(data, select= c(rownames(na_count))), numbers = TRUE, prop = c(TRUE, FALSE), varheight = F, sortVars = T, cex.lab = 1, cex.axis = 0.7)
