#Coursera Data Science R Programming course,
#Assignment 2,  Sep, 2017
#prepared by Ragavan R
#Requirement:
#Review the README.md in github
#Focus: Lexical Scoping Rules
# using <<- operator (assingment in different environemt)
#Demonstrate using caching the inverse of matrix. It avoids repeating the inverse
#operation on a matrix

# function makeCacheMatrix 
# This function will take a matrix and create a special object with matrix,
# and realted operations like get/set, set_inv/get_inv in the list for that
# matrix instance/object
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #set function assigns to x from the objects environment
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #get function return x from the objects environment
  get <- function() x
  #call solve() to do the matrix inversion
  set_inv <- function(solve) inv <<- solve
  #return the inversion value
  get_inv <- function() inv
  #list of names of getters/setters associated with the object, can be called with $name
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}

# function cacheSolve
# Takes a matrix that was created by makeCacheMatrix as input
# and returns inverse of the matrix
# inverse of the matrix will be looked up in the object and if it doesn't 
# exist int will be computed using solve() and set in the matrix object
# If the inverse value is found, message will be printed saying cached data
# is being returned

cacheSolve <- function(x, ...) {
  #get the inverse matrix value fromt the matrix object
  invMatrix <- x$get_inv()
  #check if the value was found (cache)
  #if found, return with the message saying so
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  #if not found in cache, get the matrix data to do the invesrse
  data <- x$get()
  #do the inversion using solve()
  invMatrix <- solve(data, ...)
  #store the calculated value with the object
  x$set_inv(invMatrix)
  #function returns inverse of the matrix, 
  #this is calculated when the value doesn't exist in the cache
  invMatrix
}
#clear variables to avoid any preexisting values
rm(list = setdiff(ls(), lsf.str()))
#some tests to validate the functions
xx <- matrix(1:4,2,2)
yy=makeCacheMatrix(xx)
zz=cacheSolve(yy)
#results should be Identity matrix (diagnol 1s, rest 0)
print(xx %*% zz)

