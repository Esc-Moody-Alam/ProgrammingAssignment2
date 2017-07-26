## This file has three function (1) makeCacheMatrix which creates a vector that contains a matrix and its 'cached' inverse. 
#(2) cacheSolve which computes the inverse of a matrix and caches it inside the passed 'vector' object
#(3) testit function which makes it easy to test the above two functions

## makeCacheMatrix creates a special 'vector' object that contains a matrix and its inverse

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

##This function checks if the given matrix (inside the list x) a cached inverse - if not it computes it 

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

#This functions tests the above two functions.

testit <- function()
{
  m<- matrix(c(3,2,2,3),2,2)
  mc <- makeCacheMatrix(m)
  print(cacheSolve(mc))
  print(cacheSolve(mc))
}

