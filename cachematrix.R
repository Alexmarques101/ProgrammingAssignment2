## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix creates a special "matrix" object that can cache its inverse. This function contains a function to:
##a. set the value of the vector
##b. get the value of the vector
##c. set the value of the mean
##d. get the value of the mean

##The following function calculates the inverse of the special "matrix" created with the above function. 
##It checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.
makeCacheMatrix <- function(x = matrix()) {
invmatrix <- NULL
  set <- function(y) {
  x <<- y
  invmatrix <<- NULL
}

  get <- function() x
  setinverse <- function(inverse) invmatrix <<- inverse
  getinverse <- function() invmatrix
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix created above

##It assumes that the matrix inputed is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
invmatrix <- x$getinverse()
if(!is.null(invmatrix)) { ##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
  message("getting cached data")
  return(invmatrix)
}
data <- x$get()
invmatrix <- solve(data)
x$setinverse(invmatrix)
invmatrix
}

}

