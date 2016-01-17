## Functions for the programming asignment 2 of the Coursera course "R programming"


# create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    mat<-NULL
    set<-function(f){
        x<<-f
        mat<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) mat<<- solve
    getmatrix<-function() mat
    list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}

# compute the inverse of the special "matrix" returned by makeCacheMatrix()
cacheSolve <- function(x=matrix(), ...) {
    mat<-x$getmatrix()
    if(!is.null(mat)){
        message("retrieving cached data")
        return(mat)
    }
    matrix<-x$get()
    mat<-solve(matrix, ...)
    x$setmatrix(mat)
    mat
}

