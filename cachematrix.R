## cachematrix contains two functions. makeCacheMatrix takes a matrix input
## and computes four functions related to the matrix and its inverse, which 
## it caches

## makeCacheMatrix takes a matrix as an input, sets its initial inverse as NULL,
## and stores four functions related to the matrix: set (which caches the 
## matrix and caches its inverse as NULL), get (which prints the matrix), 
## setinverse (which stores the inverse of the matrix), and getinverse 
## (which prints the inverse).

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL   #sets the locally defined inverse as NULL
    set <- function(y) {
        x <<- y  #sets the matrix for the function, when invoked
        I <<- NULL #sets matrix inverse as NULL when the matrix is changed
    }
    get <- function() x #prints original matrix
    setinverse <- function(solve) I <<- solve #stores input matrix as inverse
    getinverse <- function() I #prints inverse matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes the output of makeCacheMatrix as an input, since it relies 
## on its variables and functions. cacheSolve first calls getinverse() to 
## determine whether the inverse has already been calculated. If it has, it 
## returns the cached value. Otherwise, cacheSolve calls setinverse() to cache
## the inverse.

cacheSolve <- function(x, ...) { 
    I <- x$getinverse()
    if(!is.null(I)) {
        message("getting cached data")
        return(I)
    }
    matrix <- x$get()
    I <- solve(matrix, ...) #stores the inverse matrix in variable I
    x$setinverse(I)  
    #passes inverse matrix to setinverse function from makeCacheMatrix
    I
}
