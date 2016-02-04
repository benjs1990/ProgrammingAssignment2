##Caching the inverse of a square matrix##

##The first funtion creates a special "matrix" which is really a list containing a function to:
# 1. Set the values of the matrix
# 2. Get the values of the matrix
# 3. Set the values of the inverse
# 4. Get the values of the the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list (set=set, 
          get=get,
          setinverse = setinverse,
          getinverse = getinverse)
}


#The second function calculates the inverse of the matrix. 
#If the inverse is already cached from the previous function then it will display that result plus a message. 
#Otherwise it will compute the inverse in this function and display only the result

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)){
        message ("getting cached data")
        return (inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
