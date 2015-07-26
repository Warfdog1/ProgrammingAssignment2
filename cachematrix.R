
#The purpose of the makecacheMatrix is to create a special object that creates
#and contains a matrix, its inverse and the functions to get and set those.

makeCacheMatrix <- function(x = matrix()) {
            Inv_X<-NULL
            set<-function(y){
                        x<<-y
                        Inv_X<-NULL
            }
            get<-function() x
            setmatrix<-function(solve) Inv_X<<- solve
            getmatrix<-function() Inv_X
            list(set=set, get=get,
                 setmatrix=setmatrix,
                 getmatrix=getmatrix)
}



#In cacheSolve, the purpose is to calculate the inverse of the matrix for an
#object created by "makeCacheMatrix", if it has not previously done so. If cacheSolve
#calculates the inverse of the matrix, it will cache the inverse via setmatrix
#and returns the inverse using Inv_X.

cacheSolve <- function(x, ...) {
      
            Inv_X<-x$getmatrix()
            if(!is.null(Inv_X)){
                        message("getting cached data")
                        return(Inv_X)
            }
            matrix<-x$get()
            Inv_X<-solve(matrix, ...)
            x$setmatrix(Inv_X)
            Inv_X
             
}





