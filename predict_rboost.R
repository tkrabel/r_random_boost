##
##												PREDICT.RBOOST
##

library(magrittr)
library(purrr)

# :arg object:  Object of class "rboost"
# :arg newdata: Data frame containing new cases (optional)

predict.rboost <- function(object, newdata){

	# Extract info
	eta <- object[["params"]][["eta"]]

	pred <- if (missing(newdata)) {
		object[["pred"]] %>% rowSums()
	} else {
		object[["trees"]] %>% map(., ~ eta * predict(.x, newdata = newdata)) %>%
			unlist() %>% matrix(., ncol = length(object[["trees"]])) %>% rowSums()
	}

	pred

}