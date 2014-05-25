## The two functions allow calculation of the inverse of an inverible
## square matrix and caching its value;and on further calls for 
## inverting the same matrix, to retrieve it from the cache, rather than 
## recompute it.

## makeCacheMatrix takes an invertible square matrix 'x' as its lone argument. 
## It returns a list of four sub-functions:
## a) set - alters the variable 'x' within the environment of makeCacheMAtrix 
## itself, thus 'caching' it. Also caches the inverse of 'x'('I') as NULL. 
## b) get - returns the matrix cached as 'x'
## c) setInv - alters the inverse 'I', assigning to 'I' a matrix passed on as 
## its argument, within the environment of makeCacheMAtrix itself, thus 
## caching the new inverse
## d) getInv - returns the matrix cached as the inverse 'I' 

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x<<- y
    I<<- NULL
  }
  get <- function() x
  setInv <- function(Inv) I<<-Inv
  getInv <- function() I
  list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## cacheMatrix takes the list of sub-functions returned by makeCacheMatrix as
## an argument and returns the inverse of the matrix in question. 
## It calls on getInv from makeCacheMatrix. If the inverse retrieved is NULL 
## (as in the first call with a specific matrix), it calculates the inverse 
## using solve() and stores it within the environment of makeCacheMatrix by 
## calling setInv, before returning it. 
## On subsequent calls, the cached inverse is returned. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  I <- x$getInv()
  if (!is.null(I)) {
    message("getting cached Inverse")
    return (I)
  }
  data <- x$get()
  I <- solve(data,...)
  x$setInv(I)
  I
}
