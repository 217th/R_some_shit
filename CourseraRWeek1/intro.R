myfunction <- function() {
	x <- rnorm(100)
	mean(2*x)
}

second <- function(x) {
	x * rnorm(length(x))
}