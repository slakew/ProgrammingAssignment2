## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL ##reassigns m to NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x ##gets the value
  setinverse <- function(z) inverse <<- z ##sets the inverse
  getinverse <- function() inverse ##gets the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## Write a short comment describing this function

cacheSolve <- function(x,..) {
    ## Return a matrix that is the inverse of 'x'
    ##x <-  makeCacheMatrix()
    z <- x$getinverse() ## calling the mean function
    
    if(!is.null(z)) { ## checking is m is NOT (!) null
      print("getting cached data")
      return(z) ## printed the saved inverse matrix
    }
    data <- x$get()
    z <- solve(data)
    x$setinverse(z)
    z
  }
  

