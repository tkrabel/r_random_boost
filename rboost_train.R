##
##												RBOOST_TRAIN
##

# The workhorse model that carries out the actual boosting
#
# inputs:
#		- formula: a formula
#		- data: the training data on which to train the model
#		- params: a list of parameters
# 			- eta: learning rate
#				- max_depth: the maximum depth a tree can have
#				- n_rounds: number of boosting iterations
#
# output: a boosting object that contains each tree and some more handy info

rboost_train <- function(formula, data, params, randomize = TRUE) {

	# Prepare data
	target <- all.vars(formula)[1]
	predictors <- all.vars(formula)[-1]

	params <- as.list(params)
	eta <- params[["eta"]]
	max_depth <- params[["max_depth"]]
	n_rounds <- params[["n_rounds"]]
	depths <- sample(seq(max_depth), n_rounds, TRUE)
	if (!randomize) {
	  depths <- rep(max_depth, n_rounds)
	}

	# Initiate rboost object
	bst <- list()
	bst[["params"]] <- params
	bst[["params"]][["depth"]] <- vector("numeric", n_rounds)
	bst[["params"]][["n_leaves"]] <- vector("numeric", n_rounds)
	bst[["trees"]] <- list()
	bst[["pred"]] <- matrix(vector("numeric", nrow(data) * n_rounds),
													ncol = n_rounds)
	colnames(bst[["pred"]]) <- paste0("tree_", seq(n_rounds))

	# m = 0 => F_O(x) = 0
	y_model <- 0

	# Boosting
	for (m in seq(params[["n_rounds"]])) {

		# Draw leave number
		depth <- depths[m]
		tree_fit <- tree(formula = formula, data = data, max_depth = depth)

		fit <- eta * predict(tree_fit)

		# Update model prediction
		y_model <- y_model + fit

		# Compute pseuo-residuals
		data[[target]] <- data[[target]] - fit

		# Commit to bst object
		bst[["trees"]][[m]] <- tree_fit
		bst[["params"]][["depth"]][m] <- depth
		bst[["params"]][["n_leaves"]][m] <- sum(tree_fit$frame$var == "<leaf>")
		bst[["pred"]][, m] <- fit

	}

	bst

}