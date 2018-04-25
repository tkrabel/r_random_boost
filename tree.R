##
##											  TREE
##

library(rpart)

#	The tree-building function. max_depth is the crucial parameter that determines
# a tree's depth

tree <- function(formula, data, max_depth) {

	tree <- rpart(formula = formula, method = "anova", data = data,
							  control = list(minsplit = 30, maxsurrogate = 0,
			             					   maxdepth = max_depth, cp = 0))

	tree

}