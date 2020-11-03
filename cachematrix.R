# The purpose of this script is to compute for the inverse of a matrix and save its results.

# If the function to compute for the inverse of the matrix is ran again with the same matrix,
# the function will retrieve the cached results instead of re-computing the inverse.

# This code only saves the most recent results. This code does not save all of the previous results.


# ------------------------------------

# This function will create the objects that will be used to check for the matrix and store the results.
# This function will not create the objects if it detects that the objects already exists. 

makeCacheMatrix <- function() {
  if(!(exists("cached_result") & exists("cached_request"))){
    print("generating objects")
    cached_result <<- matrix()
    cached_request <<- matrix()
  } else {
    print("objects already created")
  }
}


# This function will check if the inputted matrix is the same as the one in the variable "cached_request"
# If it matches, then the function will retrieve the value of the variable "cached_result"
# If it does not match, then the function will solve for the inverse of the matrix and save the request and result.

cacheSolve <- function(request) {
  if(identical(request, cached_request)) {
    print("retrieving cached results")
    return(cached_result)
  } else {
    print("solving for matrix and saving results")
    cached_result <<- solve(request)
    cached_request <<- request
    return(cached_result)
  }
}


makeCacheMatrix() # generates the objects that will be used to store the last request and its result

makeCacheMatrix() # will not generate new objects, as it will detect that the objects already exist

data <- matrix(c(1,2,3,4), ncol=2, nrow =2) # creates a 2x2 matrix for testing (1st example)

cacheSolve(data) # computes for the inverse of the matrix (due to first run)

cacheSolve(data) # retrieves the cached result (due to second run)

data2 <- matrix(c(5,6,7,8), ncol=2, nrow =2) # creates a 2x2 matrix for testing (2nd example)

cacheSolve(data2) # computes for the inverse of the matrix (as the function will notice that the request is different)

cacheSolve(data2) # retrieves the cached result (due to second run)
