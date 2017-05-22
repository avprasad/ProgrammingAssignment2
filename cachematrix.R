## The objective of the assignment is to create a cache to hold the inverse of the matrix 

##This function is to create a  Matrix object which can cache the inverse 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## Initialize the inv of the matrix var
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  #initialize set inverse
  setinverse <- function(inverse) inv <<- inverse
  #initialize get inverse 
  getinverse <- function() inv
  ## Use the list function 
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse) 

}


## This function is to handle the Cache 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  # Check if the inverse is NULL which means that the value has to come from Cache
  if (!is.null(i)) {
    #Informative message
    message("The Cached data is")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
   #Informative message
  message("The first time data is")
  i
}
