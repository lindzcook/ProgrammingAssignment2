## functions work together to create a special matrix that can cache its inverse in order to save computing resources

##makeCacheMatrix creates the special matrix object with getters and setters for the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  ##starts mean out as null  
  inv <- NULL
  ##setter sets value of x and resest m
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
  ##getter returns x
    get <- function() x
  ##takes the inverse and sets inv to it
    setinverse <- function(inverse) inv <<- inverse
  ##returns the inverse
    getinverse <- function() inv
  ##function list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }


##cacheSolve returns the inverse, either by using the get function from the matrix object or by calculating the inverse if it has already been created

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ##gets the inverse and stores it
    inv <- x$getinverse()
  ##if inverse is not null, print message and return it
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
  ##else get x, calculate the inverse and set it, then returns inverse
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
  }
  

