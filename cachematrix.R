# makeCacheMatrix in combination with cacheSolve allow to save computation capacity by
#caching some results for inverse matrix calculation

# This function creates 4 object functions that are passed as element
# of a list to the parent environment.
# These function will then be used by the function "cacheSolve" whenever it receives
# a value of "makeCacheMatrix" as argument.
# "cashSolve" function will be able to use all the 6 objects of "makeCacheMatrix". 

makeCacheMatrix <- function(x = matrix()) {#define generic function with generic matrix argument
        inv <- NULL # set up a default value for the inverse matrix
        set <-function(y) {
                x<<-y # assign the value of the matrix
                inv <<- NULL # clear the value of the inverse matrix whenever new matrix is provided
        }        
        get <- function() x # allow to get x
        setinverse <- function(inverse) inv <<- solve #mutator function
        getinverse <- function() inv        # Return a list with the above four functions
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse) 
}


## Solve Inverse matrix of X
## if the inverse of X was already calculated for a specific 
##value of X it will return the cached vallue of the inverse X

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <-x$getinverse() # check if there is already a result stored for
        #the value of x in the cache
        #note it is possible to filter a matrix x over a function getinverse()
        # becouse matrix and function are part of the same object.
        if(!is.null(inv)) { #the value is NULL if X sha changed and has never been input previously
                message("getting cached data")
                return(inv) #return cached value
        }        data <- x$get()  #getter of x
        inv <-solve(data) # compute inverse
        x$setinverse(data) # set the inverse data as inv
        inv
}
