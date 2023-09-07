SUB___Empty.Structure <- function(obj) {
  if (is.array(obj)) {

    dims <- dim(obj)
    # dims[1] <- 0
    return(array(0, dim = dims))

  } else if (is.vector(obj)) {

    return(rep(0, length(obj)))

  } else if (is.list(obj)) {

    return(vector("list", 0))

  } else if (is.data.frame(obj)) {

    return(data.frame(matrix(ncol = ncol(obj), nrow = 0)))

  } else if (is.matrix(obj)) {

    return(matrix(nrow = 0, ncol = ncol(obj)))

  } else {

    stop("Unsupported class: ", class(obj))

  }
}

