
## This function creates a special "matrix" object that can CACHE its INVERSE

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
      x <<- y
      inv <<- NULL
    }
    get <- function () x
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv
    ##matrix(c(set = set, get = get, setInverse = setInverse, getInverse = getInverse), 2, 2)
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
} 

##CALL TO THIS MATRIX USING THIS EXAMPLE
##source('C:/Users/TB/Documents/ProgrammingAssignment2/cachematrix.R')
##mat<-matrix(sample.int(15, size = 3*3, replace = TRUE), 3, 3)
##mMat<-makeCacheMatrix()
##mMat$set(mat) 
##mMat$get()
##mMat$setInverse() ##PUT A MATRIX IN HERE IF YOU WANT. NEXT FN COMPARES CALC AND COMPARES
##mMat$getInverse()


##------------------------------------------------------##

## This FN computes inverse of the spcial "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated(and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
    inv<-x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data, ...)
    x$setInverse(inv)
    inv  ## Returns a matrix that is the inverse of 'x'
        
}

##CALL THIS VECTOR USING EXAMPLE
##source('C:/Users/TB/Documents/ProgrammingAssignment2/cachematrix.R')
##cacheSolve(mMat)
