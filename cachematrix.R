## Put comments here that give an overall description of what your
## functions do

## The output of this function is the inverse of a given matrix

makeCacheMatrix <- function(x = matrix()) { ## the matrix is defined
  m <- NULL                                 ## initializing the variable
  set <- function(y){                       ## defining the set function
    x <<- y                                 ## parent environment´s value
    m <<- NULL
    
  }
  get <- function()x
  set_inv <- function(inverse) m <<- inverse     ## parent environment´s value
  get_inv <- function()m                         ## get the value of m
  
  list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)     ## needed to use $ operator and avoid an error
  
  
}


## The output is the inverse of a matrix from makeCacheMatrix. If that matrix has not changed
## then this function will retrieve the inverse from the cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_inv()
  if(!is.null(m)){                             ## if the inverse is already calculated 
    message("getting cached data")             ## it will be taken from the cache
    return(m)
  }
  data <- x$get_inv()
  m <- solve(data,...)
  x$set_inv(m)
  m
  
}
