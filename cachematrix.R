
#These functions will first create a matrix, which can then store (cache) its inverse in a list. The section function can double-check whether the relevant matrix is already cached, and if so make use of this data. If not it will run the same process afresh. 

#part 1: defining the inverse of the matrix and stroing to cache
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get= get, setinverse = setinverse, getinverse = getinverse)
        
}


## Write a short comment describing this function
#part 2: checking the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message('getting cached data')
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$getinverse(m)
        m
        }




