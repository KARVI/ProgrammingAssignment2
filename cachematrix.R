## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

# Part 1: makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # sets x equal to an empty matrix
        Inv <- NULL
        # Set the inverse equal to NULL
        
        set <- function(y){
                x <<- y
                # set function assigns the argument to x
                Inv <<- NULL
                # Once the set function is called, Inverse is re-set to NULL (this is important if you redefine the matrix, x)
        }
        get <- function() x
        # get function returns the matrix
        
        setInverse <- function(solve) Inv <<- solve
        # setInverse overrides the previous value of Inv and assigns the argument to Inverse (which is supposed to be the inverse of matrix x)
        
        getInverse <- function() Inv
        # getInverse returns the Inverse
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        # creates a list of the functions
        
}

# Part 2: cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        Inv <- x$getInverse()
        # Retrives the most recent value for the inverse
        
        if(!is.null(Inv)){
                message("getting cached data")
                return(Inv)
                # If the value of Inverse is NOT null (was previously calculated), cacheSolve returns that value        
        }
        # If the value of Inverse is NULL, then you retrive matrix x and calculate the inverse with the solve() function
        message("newly calculating data")
        data <- x$get()
        Inv <- solve(data, ...)
        x$setInverse(Inv)
        # Sets Inverse to the newly calculated value   
        Inv #Returns the new Inverse value
}
