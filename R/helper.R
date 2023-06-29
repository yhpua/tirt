

#' used within `tirt()` to compute the expected score
#' modified from mirt::expected.test() to account for different category values
#'
#' @title modified expected.test()
#' @param x
#' @param Theta
#' @param which.items
#' @param mydata dataframe from which category values of individual items are extracted
#'

expected.test_mod <- function (x, Theta, which.items, mydata){


  (K <- extract.mirt(x, "K"))
  pars <- x@ParObjects$pars
  pick <- c(which.items, length(pars)); pick
  pars <- pars [pick]
  itemloc <- c(1L, 1L + cumsum(K[which.items]))

  trace <- mirt:::computeItemtrace(pars, Theta, itemloc = itemloc,
                                   CUSTOM.IND = x@Internals$CUSTOM.IND)

  mydat1 <- mydata[, which.items]
  mydat <-  as.data.frame(mydat1)
  score1 <- sapply(mydat, function(x) sort(unique((x))))
  score <-  unlist(as.vector(score1))
  # print(score1)
  ret <- as.numeric(score %*% t(trace))

  return(ret)

}



#' Used within `tirt()`
#' checks whether items have 2 or more response options
#' important to perform checks on low-prevalence items during the bootstrapping process
#'
#'
#' @title item response option checks
#' @param mydata
#' @param n_unique

yhcheck <- function (mydata, n_unique = 1){

  #' return vector of vars with <=`nunique` values

  # data <- na.omit(mydata)
  nam <- names(mydata)
  p <- length(nam)

  myvec <- NULL

  for(i in 1 : p) {
    # evaluate each var one at a time
    values <- NULL
    x <- mydata[[i]]  ## each vector

    y <- x[!is.na(x)]
    n <- length(y)
    if(n < 2)
      stop(paste("fewer than 2 non-missing observations for", nam[i]))

    values <- sort(unique(y))
    names(values) <- NULL
    nunique <- length(values)
    if(nunique <= n_unique) {
      warning(paste(nam[i],"has <=", n_unique, "unique value(s)"))

      myvec <- c(myvec, nam[i])
    }


  }
  return(myvec)
}
