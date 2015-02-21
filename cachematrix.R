## Put comments here that give an overall description of what your functions do
#
#  makeCacheMatrix defines a data structure that can both store and recover the 
#  inverse of a matrix. This function does not do the actual matrix inversion!
#  (above the function is a more step by step explanation)
#
#  cacheSolve tries to recover the inverse matrix of the data structure defined
#  by the former function. If no inverse is stored, it will do the computation. 
#  (above the function is a more step by step explanation)


## Write a short comment describing this function
#  makeCacheMatrix takes as input any matrix, but for this assignment we were 
#  allowed to just assume it has an inverse. So I'm not checking for this. 
#  Firstly, the 'inverse'-object in this structure is set to NULL. Secondly,
#  a function called set is defined that stores both the regular matrix as
#  well as the inverse in the "inverse'-object in the parental scope. 
#  Get and getinverse recover both these values respectively. 
#  The function body ends with a list of all these function, so they can be
#  called by typing $getmean() etc.


makeCacheMatrix <- function(x = matrix()){
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#  Instead of directly inverting the matrix, this function first searches
#  for an existing definition in the datastructure we defined above. If getinverse 
#  fails to recover any existing data, it will return NULL. If if a stored 
#  inverse matrix was recovered, it will print a message that cached data is being
#  used. If no data was found in cache, it will instead call solve() to calculate
#  the inverse matrix, store it in the data structure using setinverse(), and of
#  course returning the inverse matrix.


cacheSolve <- function(x, ...) {
  inverse<- x$getinverse()
  if(!is.null(inverse)) {
    message("Getting cached inverse!")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
  ## Return a matrix that is the inverse of 'x'
}