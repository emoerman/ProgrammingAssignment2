##
## These functions are involved in the calculation of inverse
## matrices. Combined they will make sure that a calculation
## is only made when there is no inverse matrix cached for the
## most recently provided matrix.
##
## Use:
## 1. Define a matrix for which you want to calculate the inverse.
## 2. Assign a cached matrix by calling makeCacheMatrix with your
##		own matrix as an argument and assign the result to a variable.
##	    For example:
##		xCached <- makeCacheMatrix(x)
##		This assumes that your own matrix is x.
## 3. The call to the function cacheSolve with the variable assigned 
##		to in step 2) as an argument will return the inverse matrix 
##		of the matrix you provided in step 1).
##		For example:
##		y <- cacheSolve(xCached)
##		Taking the example data of step 2).
##		Now y is your inverse matrix and 
##			x %*% y should equal the unity matrix
##			y %*% x should also equal the unity matrix
##		In both cases rounding errors can occur.
##
## Author: Etienne Moerman
## Date:   2015-06-18



## The function makeCacheMatrix takes a matrix as an argument and
## embeds a cached version of the matrix argument and it embeds
## the associated inverse matrix.
## If the inverse matrix has not been calculated before it is NULL.
## If this function is called again or if the set method of this 
## function is called than the inverse matrix is reset to NULL.
## 
## Parameters:
## x 	= matrix that will be embedded in the resulting class
##
## Return:
## a list containing this function's 'methods' 
##		- set (to set a new matrix as embedded value)
##		- get (to get the embedded matrix)
##		- setInv (to set a new inverse matrix as embedded value)
##		- getInv (to get the embedded inverse matrix) 
makeCacheMatrix <- function(x = matrix()) {

		## inv is the variable we use to store the inverse matrix.
		inv <- NULL

		set <- function(y) {
				## Sets the value of x in the parent function, so of
				## x in makeCacheMatrix to the passed parameter y.
				x <<- y

				## Passing a potentialy new matrix invalidates
				## the embedded inverse matrix, so we set it to NULL.
				inv <<- NULL
		}
		get <- function() { x }

		## Sets the value of inv in the parent function, so of
		## inv in makeCacheMatrix to the passed parameter invMatrix.
		setInv <- function(invMatrix) { inv <<- invMatrix }
		getInv <- function() { inv }

		## Passes a list with all the values we are interested in.
		list(set=set, get=get, 
				setInv=setInv,
				getInv=getInv)
}


## The function cacheSolve returns the inverse matrix of the 
## matrix embedded in x. When x contains an embedded value of the 
## inverse matrix that will be returned, otherwise the 
## inverse matrix will be calculated and embedded in x.
##
## Parameters:
## 	x 	= a 'object' generated using the makeCacheMatrix function
##			the parameter used to call makeCacheMatrix must
##			be a square and invertible matrix 
##
## Return:
## The inverse of the matrix embedded in parameter x.
## Side effect:
## If the inverse of the matrix embedded in parameter x
##	is calculated then the inverse matrix is embedded in x.
cacheSolve <- function(x, ...) {

		## Get the embedded value of the inverse matrix.
		inv <- x$getInv()
		
		## If the retreaved value of the inverse matrix is
		## not NULL then we found a value calculated earlier ...
		if (!is.null(inv)) {
				message("Getting cached inverse matrix")

				## ... and we return that cached (earlier calculated) value.
				return(inv)
		}

		message("Calculating inverse matrix. Please be patient.")

		## If an when we get to this point there was no cached value
		## of the inverse matrix and we retreave the matrix for 
		## which we must determine the inverse ...
		xMatrix <- x$get()

        ## ... and we calculate the inverse of this matrix by 
		## using the solve function ...
		inv <- solve(xMatrix)
		## ...  and we cache this inverse matrix. That is we
		## set the value of the inverse matrix in the parameter
		## of this function (cacheSolve).
		x$setInv(inv)

		## And finally we return the calculated inverse matrix.
		inv
}
