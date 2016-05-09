## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      im <- NULL ##set initial value of the matrix inverse as NULL
      set <- function(y) {
            x <<- y
            im <<- NULL
      }
      get <- function() x ##get the value of the matrix
      setim <- function(solve) im <<- solve ##set the value of the inverse
      getim <- function() im ##get the value of the inverse
      list(set = set, get = get, setim = setim, getim = getim)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

      im <- x$getim()
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache
      if (!is.null(im)){
            return(im)
      }
      else {
            data <- x$get()
            im<- solve(data, ...)
            x$setim(im)
            
      }
      im 
}
