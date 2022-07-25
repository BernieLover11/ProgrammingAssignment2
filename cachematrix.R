## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Setting M to NULL, the whole point of this assignment
## Creating 2 new functions to get the inverse & set the inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
 	set <- function(y) {
   	     x <<- y
           m <<- NULL
      }
   get <- function() x
        inverseSet <- function(inverse) m <<- inverse
        inverseGet <- function() m
        list(set = set, get = get,
             inverseSet = inverseSet,
             inverseGet = inverseGet)
}



## Write a short comment describing this function
 ## Return a matrix that is the inverse of 'x'
##This is the solver, it will pulled the cached inverse if it has already been made.
## IF it is not, the inverse will be solved and printed

cacheSolve <- function(x, ...) {
        m <- x$inverseGet()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$inverseSet(m)
        m
       
}
