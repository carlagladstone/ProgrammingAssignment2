## The two functions in this program create and apply matrix "methods" so
## that we can define properties that will 
##    'set' the matrix, (assign it a value)
##    'get' the matrix (return its value to a calling function)
##    'setInverse' of the matrix (assign a persistent value for the inverse)
##    'getInverse' (return the matrix inverse to a calling function)
##
## The first function, 'makeCacheMatrix", establishes the
## "constructor" and "accessor" functions.
##
## The second function "makeCacheMatrix" returns the matrix inverse
## to a calling function. If the inverse is in cache, it returns the
## cached value, otherwise, "makeCacheMatrix" calculates the inverse
## and caches it for later use.
## 

## 'makeCacheMatrix' takes an argument matrix 'x' and
## returns a list of four matrix-management functions:
##
##    'setMatrix' takes an argument 'y', and tests whether
##    'y' is a valid input for a square matrix. If 'y' is valid
##    'x' is set to the matrix 'y' using the "environment search"
##    assingment operator. 'setMatrix' also sets
##    (or re-sets) the internal value of 'xInverse' to NULL also using the 
##    "environment search" assignnment. Therefor 'x' and 'xInverse' 
##    become part of the 'makeCacheMatrix' environment and will retain 
##    their values between function calls.
##
##    getMatrix() simply returns the value 'x'
##
##    setInverse(inverse) sets the internal value 'xInverse' to the 
##    supplied argument 'inverse' (without any tests for validity)
##    using the "environment search" assignment operator so that
##    'xInverse' will retain that value unless 'setMatrix' is called 
##    again for 'x'
##
##    getInverse() returns the value of 'xInverse' which is set to NULL
##    when 'makeCacheMatrix' is first invoked (lexical scoping), and takes
##    the value NULL any time 'x' has been changed by 'setMatrix'.
##    Otherwise, it will return the matrix most recently supplied as
##    an argument to 'setInverse'
##    

makeCacheMatrix <- function(x = matrix()) {
      xInverse <- NULL
      set <- function(y){
            ## test whether y can be made into a square matrix
            ##n <- length(y)
            d <- as.integer(sqrt( length(y)) )
            if( d^2 == length(y) ){
                  x <<- matrix(y, nrow = d, ncol = d)
            } else {
                  message("invalid argument supplied to setMatrix")
                  message("number of elements must be a square")
                  x <<- NULL
            }
            
            xInverse <<- NULL
      }
      get <- function(){x}
      setInv <- function(inverse){xInverse <<- inverse}
      getInv <- function(){xInverse}
      list(setMatrix = set, getMatrix = get, setInverse = setInv, getInverse = getInv)
}## end definition 'makeCacheMatrix'


## Write a short comment describing this function
## 'cacheSolve' uses the list of functions defined in
## 'makeCacheMatrix' to return a matrix inverse.
## It takes argument x, assumed to be created by 'setMatrix'.
## Its first attempt to obtain the inverse is to look for 
## a cached value by calling 'getInverse'.
## If 'getInverse' returns a NULL value, then the inverse
## has not been calculated yet.
## A call to 'getMatrix' obtains the value of the matrix,
## which can then be supplied as an argument to the
## 'solve' function to produce the inverse as the value of a 
## local variable. 
## The inverse is supplied as an argument to 'setInverse'
## so it will be preserved for later use.
## Finally the inverse matrix is returned to the calling function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse <- x$getInverse()
      if( ! is.null(inverse) ){
            message("getting cached inverse")
            return(inverse)
      }
      mat <- x$getMatrix()
      inverse <- solve(mat) ## calculate matrix inverse
      x$setInverse(inverse) ## cache 'inverse' for further use
      inverse
} ## end definition 'cacheSolve'
