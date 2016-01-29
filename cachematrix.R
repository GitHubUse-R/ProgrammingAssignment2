## Dear fellow student,
## 
## I am delighted to study in collaboration with you.
##
## Many thanks for reading and marking my work. 
##
## Best regards and best of luck with your studies.

## The recommended coding standards are used in this file:
## 1) encoding = ASCII
## 2) indent = 4 spaces
## 3) width = 80 columns
## 4) length of function <= one page

## The description of the code: The following two functions, makeCacheMatrix and
## cacheSolve, are meant to create and update a special object that can cache a 
## matrix and its inverse. It can be beneficial to keep the matrix and its 
## inverse together in one special object so as not to repeat the computation of
## the same inverse (matrix inversion is usually a costly computation) and so as
## not to forget to recalculate the inverse when there are changes in the 
## matrix.

## The function makeCacheMatrix creates a special object for a matrix (the 
## argument mtrx). The matrix supplied (mtrx) is supposed to be invertible.
## The function returns a list of methods which can be used to set/get the 
## matrix/inverse.

makeCacheMatrix <- function(mtrx = matrix()) {
    
    invrs <- NULL
    
    set_matrix <- function(m) {
        ## the operator '<<-' allows to change variables at higher levels in the
        ## environment hierarchy.
        mtrx <<- m
        invrs <<- NULL
    }
    
    get_matrix <- function() mtrx
    set_inverse <- function(i) invrs <<- i
    get_inverse <- function() invrs
    
    special_object <- list(set_matrix = set_matrix, get_matrix = get_matrix,
                           set_inverse = set_inverse, get_inverse = get_inverse)
    
    special_object
}

## The function cacheSolve computes the inverse of the matrix contained in the 
## special object (the argument special_object). The object is returned by the 
## function makeCacheMatrix. If the inverse has already been calculated (and the
## matrix has not changed), then the function cacheSolve retrieves the inverse 
## from the cache.

cacheSolve <- function(special_object, ...){
    
    invrs <- special_object$get_inverse()
    
    if(!is.null(invrs)) {
        message("Retrieving the inverse from the cache")
        return(invrs)
    }
    
    message("Calculating the inverse")
    invrs <- solve(special_object$get_matrix(), ...)
    special_object$set_inverse(invrs)
    
    invrs
}