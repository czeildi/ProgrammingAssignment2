## These functions allow caching inverse of a matrix so that there is no
## need to recompute the inverse of the same matrix every time it is used.
## This is useful because matrix inverse computation can be costly.

## This function returns an object of 4 functions for a matrix.
## These 4 functions provide the functionality for setting and using 
## the cached inverse.

makeCacheMatrix <- function(my_matrix = matrix()) {
        stored_inverse <- NULL
        set <- function(new_matrix) {
            my_matrix <<- new_matrix
            stored_inverse <<- NULL
        }
        get <- function() {
            return (my_matrix)
        }
        setInverse <- function(new_inverse) {
            stored_inverse <<- new_inverse
        }
        getInverse <- function() {
            return(stored_inverse)
        }
        
        return (list(set = set, get = get,
            setInverse = setInverse,
            getInverse = getInverse))
    }
}


## This function provides functionality of getting the 
## inverse of a matrix, but computing it only if it has not been cached.

cacheSolve <- function(matrix_cached_object, ...) {
    stored_inverse <- matrix_cached_object$getInverse()
    if(!is.null(stored_inverse)) {
        message("fetching cached data")
        return(stored_inverse)
    }else {
        local_matrix <- matrix_cached_object$get()
        local_inverse <- solve(local_matrix, ...)
        matrix_cached_object$setInverse(local_inverse)
        return (local_inverse)
    }
    
}
