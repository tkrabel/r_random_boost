##
##												PREDICT_RBOOST
##

library(magrittr)
library(purrr)

# rboost's predict function
#
#

predict_rboost <- function(object, newdata){

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