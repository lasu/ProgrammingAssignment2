## This is assignment2 in class of R.
## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.

## On assignment2's instructions, 
## this function will creates a special "matrix" object that can cache its inverse.
## It can ...
## 1-set the value of the matrix (line 14-16)
## 2-get the value of the matrix (line 18)
## 3-set the value of the inverse of the matrix (line 19)
## 4-get the value of the inverse of the matrix (line 20)

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL  
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x  
	setinverse <- function(solve) i <<- solve
	getinverse <- function() i
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. (line 32,37-40)
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache. (line 33-36)

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
