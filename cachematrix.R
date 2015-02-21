## Raghu812 - Coursera Programming Assignment2 - 2015-02-21
## There are two functions in this code block that helps store the inverse of a 
## matrix in cache and return the value from cache instead of calculating everytime
## In case the matrix is defined for the first time of redefined then the inverse is
## calculated and stored in cache
## 

## Returns the set of functions that would be needed to be used to either set the 
## Inverse ofa matrix passed as argument or return the cached inverse value
## 

makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL # initializes the value of m
  set <- function(y){ 
      x <<- y # sets the value of x to the argument passed
      m <<- NULL # resets the value of m to null outside of function scope as well
  }
  get <- function() x # returns the stored value of x
  setInverse <- function(Inverse) m <<- Inverse # sets the value of m to the argument passed
  getInverse <- function() m # returns the stored value of m
  list(set=set, get = get, setInverse = setInverse, getInverse = getInverse) # returns the functions to perform the above operations
}


## Accepts a matrix and evaluates if the inverse already exists in cache and 
## returns the cached Inverse else calculates the Inverse, stores the value in
## cache and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)){ #checks if m is already cached or not
      message("getting cached inverse")
      return (m) # if cached returns the value
  }
  message("getting calculated inverse")
  mat <- x$get() # gets the matrix to be inversed
  m <- solve(mat,...) #invokes solve function of R to calculate the inverse and store in m
  x$setInverse(m) #stores the calculated inverse value
  m # returns the inverse matrix
}
