#'
#'
#'
#'
#' @title Threshold based on IRT method (tirt)
#'
#'
#' @param mydata a dataframe containing all items of a multi-item questionnaire (or Frailty Index) and a binary anchor item (last column)
#' @param B   number of bootstrap resamples to do to get confidence intervals for IRT-based thresholds
#' @param clvar   a character vector specifying names of items that are highly similar to (or collinear with) the anchor item
#' @param rform computes expected summed score (`sum`) or express summed score as a proportion of number of items (`prop` default)
#'
#' @return a list of results comprising the expected scores (expressed as a proportion or sum) and bootstrapped CIs if B > 0
#' @export
#'
#' @examples
#'\dontrun{
#' library(tirt)
#' data(fi_dat)
#' tirt(fi_dat, B = 1000)  ## threshold value with 95% bootstrapped CI
#' }


tirt <- function (mydata, B = 0, clvar = NULL, rform = c("prop", "sum")){


  #' ensure mirt package is installed
  if (!requireNamespace("mirt", quietly = TRUE))
    stop("This function requires the 'mirt' package")

  rform <- match.arg(rform)

  #' ensure dataset has no missing values
  if (anyNA(mydata)) {
    warning("NA responses in mydata")
    mydata <- mydata[complete.cases(mydata), ]  ## remove NAs (if any)
  }

  if (length(yhcheck(mydata, 1)) >=1 ) stop ("dataset contains items with only 1 level")

  mydata_no_NA <- mydata    # supplied `mydata` with no NAs
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



  #' collinear variables with anchor var
  #' create cleaned dataframe subsets
  if(!is.null(clvar)) {

    #' ensure valid names of collinear items
    valid_clvar <- clvar %in% names(mydata)
    if(length(clvar[!valid_clvar])){
      warning (
        clvar[!valid_clvar], " : not valid item names \n")
    }
    mydata_anchor_excluded <- mydata [ , head(names(mydata),-1)]
    mydata <- mydata_clvar_excluded  <- mydata [ , setdiff(names(mydata), clvar)]
  }


  # Step 1: IRT GRM model
  nitems <- ncol(mydata) - 1
  mod <- suppressMessages(
    mirt(data = mydata, model=1, itemtype="graded", verbose = FALSE, TOL=.0001)
  )


  # Step 2: anchor difficulty parameter
  cf <- coef(mod, simplify=TRUE, IRTpars=TRUE)$items
  thr.thet <- as.matrix( cf[nitems+1, 2] )


  # Step 2.5: with collinear vars, re-fit an IRT model on all test items (excluding anchor)
  if(!is.null(clvar)) {
    # refit IRT model
    mod <- suppressMessages(
      mirt(data = mydata_anchor_excluded, model=1, itemtype="graded", verbose = FALSE, TOL=.0001)
    )
    nitems <- ncol(mydata_anchor_excluded) ## using all items
    mydata <- mydata_anchor_excluded

  }


  # Step 3: compute expected total score
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


      #' collinear variables with anchor var
      #' create cleaned dataframe subsets
      if(!is.null(clvar)) {

        mydat_anchor_excluded <- mydat [ , head(names(mydat),-1)]
        mydat <- mydat_clvar_excluded  <- mydat [ , setdiff(names(mydat), clvar)]
      }



      # Step 1: fit bootstrap IRT models
      nitems <- ncol(mydat) - 1
      bmod <-   suppressMessages(mirt(data = mydat, model=1, itemtype="graded", verbose = FALSE, TOL=.0001))
      # Step 2: anchor difficulty parameter
      b_cf <- coef(bmod, simplify=TRUE, IRTpars=TRUE)$items
      b_thr.thet <- as.matrix( b_cf[nitems+1, 2] )

      # Step 2.5: with collinear vars, re-fit an IRT model on all test items (excluding anchor)
      if(!is.null(clvar)) {
        # refit IRT model on all test items (excluding anchor)
        bmod <- suppressMessages(
          mirt(data = mydat_anchor_excluded, model=1, itemtype="graded", verbose = FALSE, TOL=.0001)
        )
        nitems <- ncol(mydat_anchor_excluded)
        mydat  <- mydat_anchor_excluded

      }



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
