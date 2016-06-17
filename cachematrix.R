# Submitter:    Steve Morin
# Date:         June 17th, 2016
#
# For this programming assignment we are required to write an R
# function that is able to cache potentially time-consuming computations.
# 
# In this case we will cache the results of matrix inversion so that the 
# results can be retrieved/looked up rather than recomputed. This will reduce
# the time it takes for the function to be performed when it is executed 
# repeatedly. This assignment takes advantage of the scoping rules of the R language. 

# This program uses `<<-` operator which is used to assign a value to an object 
# in an environment that is different from the current environment. 

# The first function, `makeVector` creates a special "vector", which is
# really a list containing a function to

# 1.  set the value of the vector
# 2.  get the value of the vector
# 3.  set the value of the inverse
# 4.  get the value of the inverse

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# This function, `cacheSolve` returns the inverted matrix. It first checks
# the cache to see if it has previously done the calculation. If the result is in
# the cache it returns that matrix. Otherwise it calculates the inverse and caches the
# result so that it can be retrieved in the future.
#
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
