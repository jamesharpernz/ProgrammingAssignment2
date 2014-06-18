## These functions implement a cached matrix inverse capability.
## As a potentially time-consuming computation, caching the results of
## matrix inversions may provide a performance improvement at the expense
## of greater memory use.
##
## Usage:
## a <-rbind(c(1,2),c(3,4))
## x <- makeCacheMatrix(a)
## cacheSolve(x)
##            [,1]  [,2]
##        [1,] -2.0  1.0
##        [2,]  1.5 -0.5
## cacheSolve(x)
##      getting cached data
##           [,1] [,2]
##      [1,] -2.0  1.0
##      [2,]  1.5 -0.5


## makeCacheMatrix sets up a vector containing a list of functions to get
## and set the matrix inverse and matrix within an environment 'm' that
## persists across calls to these functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL               # m is the persistent environment for the cache
  set <- function(y) {    # set sets the matrix to be operated on 
    x <<- y
    m <<- NULL
  }
  get <- function() x     #get returns the matrix to be operated on
  setinverse <- function(inverse) m <<- inverse  #stores an inverse in the cache
  getinverse <- function() m      #returns a result from the cache if found
  list(set = set, get = get,      #return a list of the 4 functions created
       setinverse = setinverse,   #each of which operate on data in the m
       getinverse = getinverse)   #environment
}


## cacheSolve returns a matrix inverse. If the inverse has been calculated
## before then the cached response is returned. If no cache entry exists then
## the inverse is calculated, cached, and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()    #call getinverse on the matrix in th
  if(!is.null(m)) {      #if getinverse found a cached answer, use it
    message("getting cached data")
    return(m)
  }                      #no cached answer was found
  data <- x$get()        #get the matrix to be inverted from x
  m <- solve(data, ...)  #use solve to calulcate the inverse
  x$setinverse(m)        #store the result in the cache for future use
  m                      #return the result
}
