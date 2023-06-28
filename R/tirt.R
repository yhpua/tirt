#'
#'
#'
#'
#' @title Threshold based on IRT method (tirt)
#'
#'
#' @param mydata a dataframe containing all items of a multi-item questionnaire (or Frailty Index) and a binary anchor item
#' @param B   number of bootstrap resamples to do to get confidence intervals for IRT-based thresholds
#' @param rform computes expected summed score (`sum`) or express summed score as a proportion of number of items (`prop` default)
#'
#' @return a list of results comprising the expected scores (expressed as a proportion or sum) and bootstrapped CIs if B > 0
#' @export
#'
#' @examples
#'\dontrun{
#' dat <- data("fidf_example")
#' tirt(dat)
#' }


tirt <- function (mydata, B = 0, rform = c("prop", "sum")){

  #' ensure mirt package is installed
  if (!requireNamespace("mirt", quietly = TRUE))
    stop("This function requires the 'mirt' package")

  rform <- match.arg(rform)

  #' Ensure dataset has no missing values
  if (anyNA(mydata)) {
    warning("NA responses in mydata")
    mydata <- mydata[complete.cases(mydata), ]  ## remove NAs (if any)
  }

  if (length(yhcheck(mydata, 1)) >=1 ) stop ("dataset contains items with only 1 level")

  mydata_no_NA <- mydata
  nam <- names(mydata)
  p <- length(nam)


  is.wholenumber <- function(x) x %% 1 == 0

  #' identify all vars with non-integer values
  #' ensure binary anchor item
  for(i in 1 : p) {
    values <- NULL
    x <- mydata[[i]]  ## each vector

    values <- sort(unique(x))
    names(values) <- NULL
    nunique      <- length(values)
    svar_length <- length (unique(mydata[[p]]))

    if ( !all(is.wholenumber(values)) ) warning (paste( nam[i], " has non-integer values"))
    if(  svar_length >= 3) stop (paste( nam[p],"has > 2 unique values \n tirt() works on binary state variable"))

  }

  #' convert non-integers to integers
  all_integer <- all(is.wholenumber(mydata))

  if(!all_integer){
    warning (paste( "dataset includes non-integer values"))
    mydata <- lapply(mydata_no_NA, function(x) as.numeric(as.factor(x)))
    mydata <- data.frame(mydata)
  }

  # Step 1: IRT GRM model
  nitems <- ncol(mydata) - 1
  mod <- suppressMessages(
    mirt(data = mydata, model=1, itemtype="graded", verbose = FALSE, TOL=.0001)
  )

  # Step 2: anchor difficulty parameter
  cf <- coef(mod, simplify=TRUE, IRTpars=TRUE)$items
  thr.thet <- as.matrix( cf[nitems+1, 2] )


  # Step 3: expected total score
  if(all_integer){
    ret <- expected.test_mod (x = mod, Theta = thr.thet, which.items = 1:nitems, mydata)
  } else {
    ret <- expected.test_mod (x = mod, Theta = thr.thet, which.items = 1:nitems, mydata_no_NA)
  }

  # Express summed score as a proportion of number of items
  if(rform == "prop") ret <- ret/ nitems

  #' B > 0, bootstrap IRT-based threshold
  nboot <- 0
  boot  <- array(NA, c(B, 1),dimnames=list(NULL, c('threshold')))

  if(B > 0) {
    for(i in 1 : B) {

      if(i %% 200 == 0) cat(i, '')

      j   <- sample(nrow(mydata_no_NA), nrow(mydata_no_NA), replace=TRUE)
      mydat <- mydata_no_NA[j, ]

      if (length(yhcheck(mydat, 1)) >=1) next ## skip if sample contains zero-deficit items


      #' convert non-integers to integers
      all_integer <- all(is.wholenumber(mydat))

      if(!all_integer){
        # warning (paste( "dataset includes non-integer values"))
        mydat_no_NA_b <- mydat
        mydat <- lapply(mydat_no_NA_b, function(x) as.numeric(as.factor(x)))
        mydat <- data.frame (mydat)
      }


      nitems <- ncol(mydat) - 1

      # fit bootstrap models
      bmod <-   suppressMessages(mirt(data = mydat, model=1, itemtype="graded", verbose = FALSE, TOL=.0001))
      b_cf <- coef(bmod, simplify=TRUE, IRTpars=TRUE)$items
      b_thr.thet <- as.matrix( b_cf[nitems+1, 2] )

      if(all_integer){
        b_ret <- expected.test_mod (x = bmod, Theta = b_thr.thet, which.items = 1:nitems, mydat)

      } else {
        b_ret <- expected.test_mod (x = bmod, Theta = b_thr.thet, which.items = 1:nitems, mydat_no_NA_b)
      }



      if(rform == "prop") b_ret <- b_ret/ nitems



      nboot          <- nboot + 1
      boot[nboot, ]  <- b_ret
    }

    if(nboot < B) boot <- boot[1 : nboot,  , drop=FALSE]

  }


  cl <- function(x) {
    qu <- unname(quantile(x, c(0.025, 0.975)))
    c(Lower=qu[1], Upper=qu[2]) }

  tci_wide <- round (apply(boot, 2, function(x) cl(x) ) , 3)

  aa <-
    list(
      irt  = ret,
      boot = if(B > 0) t(tci_wide)
    )

  return(aa)

}
