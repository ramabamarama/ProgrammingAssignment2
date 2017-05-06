makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversed_matrix <- function(solve) m <<- solve
  getinversed_matrix <- function() m
  list(set = set, get = get,
       setinversed_matrix = setinversed_matrix,
       getinversed_matrix = getinversed_matrix)
}

cacheSolve <- function(x, ...) {
  m <- x$getinversed_matrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinversed_matrix(m)
  m
}

# sample_matrix <- matrix(c(3, 1, 1, 1, 1, 0, 1, 0, 0), nrow=3, ncol=3)
# sample_matrix
# inverted <- solve(sample_matrix)
# inverted
# myMatrix <- makeCacheMatrix(sample_matrix)
# aResult <- cacheSolve(myMatrix)
# aResult
