## The first function creates four posible operations to run on a matrix (these operations are detailed on the next comment block).
## The second function will check if a result is already stored in cache before attempting to calculate it again, in order to save resources.

## This function defines 4 parameters to call on a matrix:
# set - creates the matrix
# get - outputs the matrix we created
# setInv - to apply the solve() function on the matrix (solve() outputs the inverse of our matrix)
# getInv - outputs the result of setInv

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function will get the above specified variable x and check if the inverse has already been calculated.
## If true, returns the stored value. If not, calculates the inverse and outputs it using the solve() function.

cacheSolve <- function(x) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("Getting cached data, no need to reprocess!")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInv(m)
  m
}

## Let's try it!
# 1. Add the first function to a variable
matrixFunction <- makeCacheMatrix()

# 2. Create the matrix
matrixFunction$set(matrix(1:4,2,2))

# 3. Call the second function on this variable
cacheSolve(matrixFunction)
# Output:
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

# 4. If we call it again, we should get the cached version:
cacheSolve(matrixFunction)
# Output:
# Getting cached data, no need to reprocess!
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5