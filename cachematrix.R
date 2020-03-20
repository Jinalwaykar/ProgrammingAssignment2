## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  mtrx <- NULL #Set as null so can be used within function later
  
  # Take the matrix passed into makeCacheMatrix and assign a value to an object in an environment
  # that is different from the current environment through the <<- operator
  # This object will be referenced in the cacheSolve function below through the functions defined further below
  
  set <- function(y) {
    x <<- y
    mtrx <<- NULL
  }
  
  # The three functions below enables us to get the value of the matrix passed as an argument above
  # or provide the code to set and get the inverse values
  get <- function() x
  
  setinverse <- function(inverse) mtrx <<- inverse
  
  getinverse <- function() mtrx
  
  
  #Declaration of functions to make them availabe as objects in cacheSolve()
  #The first element is the method name that is used to access the function
  list(setmatrix=set, getmatrix=get, setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  
  mtrx <- x$getinverse()
  
  # Check if matrix was previously inversed and return if not null
  if(!is.null(mtrx)) {
    
    message("...getting cached data.")
    
    return(mtrx)
  }
  
  # If matrix not inversed, get the matrix
  data <- x$getmatrix()
  
  # Invert the matrix via Solve
  mtrx <- solve(data)
  
  # Call setinvese from makeCacheMatrix
  x$setinverse(mtrx)
  
  # Return the result to the console
  mtrx
}


