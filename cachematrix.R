## Usage:
## > cacheSolve(cx)
## > cacheSolve(cx)
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

  inv  <- NULL
  
  # set the matix
  set  <- function(y){
    x <<- y
    inv <<- NULL 
  }
  
  # get the matrix
  get  <- function() x
  
  # set the inverse
  setinverse  <- function(inverse) inv  <<- inverse
  
  # get the inverse
  getinverse  <- function() inv
  
  # return the matrix
  return(list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse))
}


## cacheSolve compute's the inverse of the matrix. If the inverse has
## already been caculated, returns the cached inverse

cacheSolve <- function(x, ...) {
  
  inv  <- x$getinverse()
  
  # if the inverse has already been calculated return the cached inverse
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # the inverse has not been calculated yet
  data  <- x$get()
  inv  <- solve(data, ...)
  
  # cache the new value
  x$setinverse(inv)
  
  return(inv)
}
