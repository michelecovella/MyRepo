## Function makeCacheMatrix includes 4 functions: set, get, setinverse, getinverse
## It takes a matrix as its argument and stores it. If the inverse of the matrix (inv)
## is calculated through cacheSolve function, inv is stored as well

makeCacheMatrix <- function(x = matrix()) {    ## x is the matrix argument
      inv <- NULL                   ## inv is the inverse of the matrix, set to NULL
      set <- function(y){           ## set function caches the matrix x 
            x <<- y     
            inv <<- NULL            ## every time set is called, inv is changed to NULL 
      }
      get <- function() {           ## get function simply returns the matrix cached
            x                       ## by the set function
      }      
      setinverse <- function(s) {   ## setinverse function caches inv, the variable
            inv <<- s               ## whose value is the inverse of the matrix 
      }      
      getinverse <- function() {    ## getinverse returns the inverse of the matrix
            inv                     ## or a NULL value if no value is cached for inv 
      }      
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}                                   ## the 4 functions are assigned to a list


## Function cacheSolve returns the inverse of a matrix. If the inv variable is not 
## yet assigned a value, then its value is calculated by using function solve.
## If the inv variable already has a value, this is retrieved and returned

cacheSolve <- function(x, ...) {    
      inv<- x$getinverse()          ## inv value is initially retrieved from getinverse
      if(is.null(inv)) {            ## if no value is assigned to inv
            data <- x$get()         ## get passes the matrix as the argument to
            inv <- solve(data, ...) ## the solve function
            x$setinverse(inv)       ## setinverse is called to cache the value of inv
            inv                     ## and the inverse is returned as output
      } else {    
            message("getting cached data")
            return(inv)             ## if a value for inv can be retrieved 
      }                             ## this is returned without further computation
      
}

