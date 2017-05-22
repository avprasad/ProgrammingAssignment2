## The objective of the assignment is to create a cache to hold the inverse of the matrix 
## This function is to create a  Matrix object which can cache the inverse 
## This assignment creates two functions the makeCacheMatrix and cacheSolve

## Make Cached Matrix performs the following activities 
## Set the value of the Matrix and its inverse 
## get the value of the Matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
## Initialize variable for the inverse of the matrix
  inv <- NULL 
## The base init function which sets the value 
  init <- function(y) {
    x <<- y
    inv <<- NULL
  } 
## The base get function which gets the value 
  get <- function() x
  
##set the inverse matrix value 
  setinverse <- function(inverse) inv <<- inverse
##get the inverse matrix value 
  getinverse <- function() inv
## Use the list to set the value appropriately 
  list(set = init,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse) 

}


## This function is to handle the Cache 
## This function checks if the inverse matrix is already present
## if so it displays it else it inverses it and displays it 

cacheSolve <- function(x, ...) {
## The inverse of matrix is computed and stored in invm
## The invm variable in the current environment will hold this value till cache is cleared
  invm <- x$getinverse()
  # Check if the inverse is already computed which means that the value has to come from Cache
  if (is.null(invm))
  {
   data <- x$get()
  invm<- solve(data, ...)
  x$setinverse(invm)
   #Informative message
  message("The initial/first time data is")
  invm 
  ## Return the inverted matrix after computation     
  }
  else
  {
  #Informative message
    message("The cached data is")
    return(invm) 
  }
}
