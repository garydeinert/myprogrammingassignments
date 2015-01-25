
## makeCacheMatrix is designed to do the following:
##    set creates the matrix in current environment
##    get will retrieve the x matrix
##    setSolve will compute the inverse of x
##    getSolve will retrieve the computed inverse

## x is local matrix, y is matrix in parent environment, xinv is inversion(x)

makeCacheMatrix <- function(x = matrix()) {
      xinv <-NULL                   ## initialize inverse 
      set<-function(y) {
            x <<- y                 ## reset inverse in case matrix changed                                    
            xinv <- NULL
      }                                                                                        
      
      get <- function() x           ## retrieve original matrix
          
      setSolve <- function(solve) xinv <<- solve      ## calculate inverse
                                          
      getSolve <- function() xinv                     ## get the inverse
                                                
      list(set=set, get=get, 
           setSolve=setSolve, getSolve=getSolve)      ## pass values from makeCacheMatrix
                                                
}

## cacheSolve wil find the existing inverse if existing, or compute new one
cacheSolve <- function(x = matrix(), ...) {
      xinv <- x$getSolve()                            ## get existing inverse(x)
      if(!is.null(xinv)) {
            message("getting cached data")
            return(xinv)
      }
      matrx <- x$get()                               ## calculate missing inverse(x)
      xinv <- solve(matrx, ...)
      x$setSolve(xinv)
      xinv
}
