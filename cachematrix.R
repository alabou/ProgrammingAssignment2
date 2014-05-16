# The following code implements mutable objects in a way similar to other programming languages
# using an environment to store the members of an instance of an object. The special matrix
# object has a 'matrix' member for storing the regular matrix when the object is created and
# an 'inverse' member for storing the inverse. Initailly the inverse is set to NULL to flag
# that the inverse has not been calculated yet.


## makeCacheMatrix
#
#  A special matrix object constructor. Calling this function creates such an object from the
#  matrix it receives as a parameter or from a default 1x1 matrix. The function stops with an
#  error if the specified parameter is not a matrix. The function returns a special matrix 
#  object to pass to the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  
  if (!is.matrix(x)) stop('error: makeCacheMatrix expects a matrix parameter')
  
  this <- new.env(parent=emptyenv())  # new empty object

  this$matrix  <- x                   # adding members
  this$inverse <- NULL
  
  this$solve   <- function() {        # local function has access to local 'this' object
    
    if (is.null(this$inverse)) {
    
      this$inverse <<- solve(this$matrix)
    }
    else {
      
      this$inverse
    }
  }

  this$get <- function() {            # bonus functio to get the matrix from the special matrix object
    
    return (this$matrix)
  }

  this$set <- function(x) {           # bonus function to change the matrix in the special matrix object
    
    if (!is.matrix(x)) stop('error: makeCacheMatrix expects a matrix parameter')
    
    this$matrix  <<- x                   # adding members
    this$inverse <<- NULL
    
    return (this$matrix)
  }
  
  return(this)                        # return the constructed object
}

## cacheSolve
#
#  Invert the matrix of a special matrix object. The inversion of the matrix is performed
#  once on the first invocation and the result cached for the subsequent invocations. This
#  function call the 'solve' member funciton of the special matrix object to retrieve the
#  matrix inverse and return the result to the caller. The function stops with an error if
#  the function parameter is not an environment.

cacheSolve <- function(this, ...) {
  
  if (!is.environment(this)) stop('error: cacheSolve expects a special matrix parameter')
  
  return(this$solve(...))
}

# setMatrix
#
# Bonus function to change the matrix associated with the special matrix object. It 
# invalidate the previously calculated inverse. The function returns the matrix
# recieved as a parameter if it succeeds. Otherwise it returns an error if the 
# function parameter is not a matrix.

setMatrix <- function(this, m) {
  
  if (!is.environment(this)) stop('error: setMatrix expects a special matrix parameter')
  
  return(this$set(m))
}

# getMatrix
#
# Bonus function to retrieve the matrix associated with the special matrix object. It
# returns an error if the function parameter is not a matrix.

getMatrix <- function(this) {
  
  if (!is.environment(this)) stop('error: cacheSolve expects a special matrix parameter')
  
  return(this$get())
}
