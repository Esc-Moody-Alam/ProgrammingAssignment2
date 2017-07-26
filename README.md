### Introduction

I have made this repository for the second programming assignment of the R Programming course at Coursera. It contains a single file that has methods to create a 'list' object that contains a matrix and its cached inverse.

### Caching the Inverse of a Matrix

The first function, `makeCacheMatrix` creates a special "list", which is
really a list containing a function to

1.  set the value of the Matrix
2.  get the value of the Matrix
3.  set the value of the Inverse of the Matrix
4.  get the value of the Inverse of the Matrix

<!-- -->

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL

  #set and get methods for x
  get <- function() x
  set <- function(y){
    #Note that we cached the data and initialise the inverse with null
    x <<- y
    inverse <<-NULL
  }


  #set and get methods for inverse
  setInverse <-function(y) inverse <<- y
  getInverse <- function() inverse

  list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}

The following function calculates the inverse of the Matrix but before doing so ensures that the inverse has not already been computed.

cacheSolve <- function(x,...) {

  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()

  #If the inverse is already computed (i.e., its not NULL) then simply return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }   
  #We are here which means the inverse matrix is not computed - let's do it.
  inverse <- solve(x$get(),...)
  x$setInverse(inverse)
  return(inverse)

}

Finally the Test it function makes it easy to test the above two functions.


#This functions tests the above two functions.

testit <- function()
{
  m<- matrix(c(3,2,2,3),2,2)
  mc <- makeCacheMatrix(m)
  print(cacheSolve(mc))
  print(cacheSolve(mc))
}
