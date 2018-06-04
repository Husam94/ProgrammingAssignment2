## Here I provide a function to (1) make and cache the inverse of the matrix 
## and (2) reverse the inverse of that cahced matrix with a solve function 

## makeCacheMatrix makes and stores the inverse of an inputted matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { ## set the value of the matrix 
    x <<- y
    inv <<- NULL
  }
  get <- function() x ## get the value of the matrix 
  setinverse <- function(inverse) inv <<- inverse ## set the matrix of the inverse  
  getinverse <- function() inv ## get the matrix of the inverse 
  list(set = set, get = get, ## display 
       setmean = setmean,
       getmean = getmean)
}


## cacheSolve takes the matrix from makeCacheMatrix (now a inverse) and reverts it back using 
## the solve function 

cacheSolve <- function(x, ...) {
       inv <- x$getinverse() ## retreives inverse matrix from previous cache operation 
  if(!is.null(inv)) { ## searches cache 
    message("getting cached data")
    return(inv)
  }
  data <- x$get() ## stores matrix into "data" 
  inv <- solve(data, ...) ## solves matrix 
  x$setinverse(inv) 
  inv
}
