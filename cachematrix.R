## calculate and cache the inverse of a matrix with first function (result not returned),
## then get the inverse of the matrix by either getting it 
## from the cache (parent environment) if calculated before, or by calculating it now

## makeCacheMatrix function with x=matrix defined in same environment as makeCacheMatrix (not within this function itself)(empty matrix as default to avoid error)
## s initialized as object within same environment as makeCacheMatrix, used later
## define 'getters and setters' with set: assign the input y to x in parent environment, and NULL to s (to clear cache in case of earlier use of cacheSolve)
## get (with x defined in and got from parent environment), setsolve (applies solve function and assigns input to s in parent environment),
## and getsolve (similar as 'get' for x) and save it as list with names (names needed for second function cacheSolve)

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## return inverse of matrix x (by function solve).
## if value stored in parent environment (cached), then get it from there and return it
## if not (s is NULL), then calculate it new, get data of matrix x,
## apply the function solve()to the data, assign the result to s and print s

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)){
                message ("getting cached data")
                return (s)
        }
        data <- x$get()
        s <- solve (data,...)
        x$setsolve(s)
        s
}
