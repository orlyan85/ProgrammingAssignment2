## Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCachematrix <- function(x = matrix()) {
  # set the value of m to NULL(provides a default value)
  m <- NULL 

  # set the value of matrix
  setmatrix <- function (y) {  
    x <<- y 
    m <<- NULL 
  }

  # get original matrix for solve calculation 
  get <- function () x 
  
  
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  
  # name the list of functions
  list(get = get, 
       setmatrix = setmatrix, 
       getmatrix = getmatrix)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x = matrix()){
  # get cached getmatrix
  m <- x$getmatrix()

  #if there is a value for getmatrix, print message and return the value
    if(!is.null(m)) {
    message(
      "getting cached data")
    return(m)
    }
  
  #otherwise, get the value of inverse of the matrix
  data <- x$get()
  m <- solve(data)
  x$setmatrix(m)
  
  #return the inverse
  m 
}

