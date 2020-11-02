## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  create_vectors_f <- function() {
    if(exists("cached_requests_v") & exists("cached_results_v")) {
      ## do nothing
    } else {
      cached_requests_v <<- vector()
      cached_results_v <<- vector()
    }
  }
  set_request_f <- function(x=matrix()) {
    if(x %in% cached_requests_v) {
      print("instance of request found in cache")
    } else {
      cached_requests_v <<- c(cached_requests_v,x)
    }
  }
  get_results_f <- function(x) {
    if(x %in% cached_requests_v) {
      print("retrieving cached results")
      index <- match(x, cached_requests_v)
      print(cached_results_v[index])
    } else {
      print("new request; solving for inverse")
      results <- solve(x)
      cached_results_v <<- c(cached_results_v, results)
      print(results)
    }
  }
  cache_functions <- list(create_vectors_f, set_request_f, get_results_f)
}


## Write a short comment describing this function

cacheSolve <- function(x=matrix()) {
  makeCacheMatrix()
  create_vectors_f()
  set_request_f(x)
  get_results_f(x)
}

data <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)

cacheSolve(data)
makeCacheMatrix()