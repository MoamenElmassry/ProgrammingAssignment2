# return the inverse of a matrix and save it to the cache
makeCacheMatrix <- function(val = matrix()) {
  cache <- NULL
  set <- function(new) {
    val <<- new
    cache <<- NULL
  }
  get <- function() val
  setinverse <- function(inverse) cache <<- inverse
  getinverse <- function() cache
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse) 
}
## check if the inverse in the cache or not, if not this function computes it
cacheSolve <- function(val, ...) {
  cache<-val$getinverse()
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  data <- val$get()
  cache<-solve(data)
  val$setinverse(cache)
  cache
}