## Put comments here that give an overall description of what your
## functions do
## first: sorry for my bad inglish
## when i know the function makeVector and Cachemean its a point to go forward and try 
## to analize the structure and adapt to keep me doing the same thing, that principal
## its make a copy of the calculus that make the function in the cache to save time
## in other times that i can need re calculus the inverse of a given matrix

## Write a short comment describing this function


## this function its the most important in this case because she have the responsability
## to make a list of the inverse matrix in the parent enviroment and prevent that when i
## run the function the return stays in the memory and not in the garbage

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## Write a short comment describing this function
## this function first check if the inverse matrix oof the argument was before calculate
## by access to objects define in makecachematrix. Chachesolve can calculate
## and store the inverse of the original argument (x) 
## but if before was execute the inverse, only access to the result for saving time
## with the extract operator to data store in the list of makeCahematrix

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
        ## Return a matrix that is the inverse of 'x'
}

the_matrix1 <- makeCacheMatrix(matrix(c(6,9,12,6), 2, 2))
the_matrix1$get()
the_matrix1$getinverse()
cacheSolve(the_matrix1)
cacheSolve(the_matrix1)
the_matrix1$getinverse()
