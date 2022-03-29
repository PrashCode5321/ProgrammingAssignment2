
## Function to create a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Function to calculate the inverse of the matrix in the special matrix

cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()       #Getting inverse matrix if it was previously set
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)              #If inverse was set in makeCacheMatrix(), it is returned.
  }
  
  data <- x$get()          #If inverse was not found using previous function then retrieving matrix data to calculate inverse
  m <- solve(data)         #solve method used to calculate inverse of the matrix
  x$setInverse(m)          #Caching the inverse matrix to the special matrix
  m
        ## Return a matrix that is the inverse of 'x'
}
