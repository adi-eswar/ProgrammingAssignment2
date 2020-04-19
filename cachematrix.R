
## makeCacheMatrix creates a list containing a function to set and get value of the matrix plus set and get the value of its invers. 
## repurposing the given mean template 
makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinvererse <- function(invererse) inver <<- invererse
  getinvererse <- function() inver
  list(set=set, get=get, setinvererse=setinvererse, getinvererse=getinvererse)
}


## Function to return the inver of a matrix. If the inver is stored it will retreive it.

cacheSolve <- function(x, ...) {
  inver <- x$getinvererse()
  if(!is.null(inver)) {
    message("getting cached data.")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data)
  x$setinvererse(inver)
  inver
}
