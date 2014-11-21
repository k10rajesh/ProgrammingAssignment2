## A pair of functions to cache the inverse of an invertible matrix
##
## How to use: Pass a matrix to makeCacheMatrix and assign to x.
## x<- makeCacheMatrix(matrix(c(38,7928,32457,425,125,64678,76,118,94), nrow =3, ncol =3, byrow=TRUE))
## cacheSolve(x), now pass x to cacheSolve. This will return the inverse of x. 
## Now re-run cacheSolve(x). This step will give cache inverse data. This implies that the calculation was not done instead ## the value was picked from the previous run that was stored in object retuned by makeCacheMatrix
##
## Function makeCacheMatrix will cache the inverse of a given invertible matrix and will return an object list
makeCacheMatrix <- function(m=matrix()){
        inverse <- NULL
        setMatrix <- function(n){                           ## A function to set a different matrix value to 'm'
                m <<- n
                inverse<<- NULL                
        }
        getMatrix <- function() {m}                         ## A function to get the original matrix 'm'
        setInverse <- function(solve) {inverse <<- solve}   ## Assign value to 'inverse' that is inverse of matrix
        getInverse <- function() {inverse}                  ## to retrieve the value of 'inverse'
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

## Function cacheSolve will return a matrix that is inverse of the 'p'. It is assumed that p is an invertible matrix.
## Function cacheSolve will take the object returned by makeCacheMatrix and first check if inverse is availble in the object ## list for the given matrix, if not it will calculate the inverse and return the matrix, additionally it will also put the ## inverse value in makeCacheMatrix object
cacheSolve <- function(p, ...){                             ## takes the object retuned by makeCacheMatrix
        i <- p$getInverse()                                 ## gets value of 'inverse' using getInverse function
        if (!is.null(i)) {                                  ## if value is not null then retuns the 'inverse'
                message('Getting cached inverse data')
                return (i)
        }
        data<- p$getMatrix()                                ## gets the matrix value in 'p'
        inverse <- solve(data, ...)                         ## calculates inverse
        p$setInverse(inverse)                               ## sets the value of inverse in 'p'
        inverse                                             ## retuns the inverse of 'p'
}
