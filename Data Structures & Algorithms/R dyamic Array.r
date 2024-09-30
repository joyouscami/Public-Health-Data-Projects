dynamic_array <- function() {
  arr <- list(size = 0, capacity = 1, array = vector("list", 1))
  append <- function(element) {
    if (arr$size == arr$capacity) {
      resize(2 * arr$capacity)
    }
    arr$array[[arr$size + 1]] <- arr$size <- arr$size + 1
  }
  pop <- function() {
    if (arr$size == 0) {
      stop("Array Is Empty")
    }
    value <- arr$array[[arr$size]]
    arr$array[[arr$size]] <- NULL
    arr$size <- arr$size - 1
    return(value)
  }
  resize <- function(new_capacity) {
    new_array <- vector("list", new_capacity)
    for (i in seq_len(arr$size)){
      new_array[[i]] <- arr$array[[i]]
    }
    arr$array <- new_array
    arr$capacity <- new_capacity
  }
  return(list(append = append, pop = pop, size = function() arr$size))
}
#Example usage
arr <- dynamic_array()
arr$append(10)
arr$append(20)
arr$append(30)
print(arr$size())
print(arr$pop())