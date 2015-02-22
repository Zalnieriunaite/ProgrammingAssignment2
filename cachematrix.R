##The functions below together allow to calculate inverse of a given matrix 
##AND do not repeat the same computation if an inverse for the SAME matrix is required

##Below as comments there is a matrix B - which can be used as example

##The first computation should contain of two lines:
##A <-makeCacheMatrix(x) 
##cachesolve(A)
##Afterwards it should only be cacheSolve(A) - otherwhise it recalculates the value

##B = matrix(c(2, 4, 3, 1, 5, 7, 1 , 2 ,3), nrow=3, ncol=3)

##This function is a container of 4 other functions, which can later be called since
##are stored in a list and can be subtracted
##it also sets NULL value to the inverted matrix (inv)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

##This function uses 4 functions from 'makeCacheMatrix' to calculate inverse matrix
##It also evaluates if the inversed matrix has already been evaluated using 'if'condition
##It informs if inverse matrix was calculated previously
cachesolve <- function(x, ...) {
    inv <- x$getsolve()
    if(!is.null(inv)) {
         message("getting cached data")
         return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setsolve(inv)
    inv
}



