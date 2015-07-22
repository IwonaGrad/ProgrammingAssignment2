# makeCachematrix creates a special matrix, but also stores functions within

makeCacheMatrix <- function(x = matrix()) { #definition of the function
    
    inv <- NULL # sets the inv parameter to NULL
    set <- function(y) { # this function sets a new value (user defined) for main function
    x <<- y               
    inv <<- NULL          # resets the inv value
  }
  get <- function() x #returns the input matrix
  setinverse <- function(solve) inv <<- solve #stores the inversed value
  getinverse <- function() inv #returns the inverse value
  list(set = set, get = get, #makes a list of all functions within makeCacheMatrix
       setinverse = setinverse,
       getinverse = getinverse)
}

# this function calculates the inverse of the matrix from the first function
    
cacheSolve <- function(x,...) {
  
  inv <- x$getinverse()   #gets the inverse of the matrix from within makeCacheMatrix
  if(!is.null(inv)) {     #checks for the value of inverse, if not null, returns the matrix
    message("getting cached data")
    return(inv)
  }
  data <- x$get()         #if the value of the matrix (inv) within makeCacheMatrix was null, gets the value from the main function 
  inv <- solve(data)      #makes the inverse of it
  x$setinverse(inv)       #sets the value of the inverse as a new value of the matrix within makeCacheMatrix
  inv
}
## Return a matrix that is the inverse of 'x'
