# https://github.com/aaronpeikert/lavaan/commit/a528028973c9b9ffc4afbde3c4a663a9e9a9be23
standardizedSolution <-
  standardizedsolution <- function(object,
                                   type = "std.all",
                                   se = TRUE,
                                   zstat = TRUE,
                                   pvalue = TRUE,
                                   ci = TRUE,
                                   level = 0.95,
                                   cov.std = TRUE,
                                   remove.eq = TRUE,
                                   remove.ineq = TRUE,
                                   remove.def = FALSE,
                                   partable = NULL,
                                   GLIST = NULL,
                                   est   = NULL,
                                   output = "data.frame") {
    
    stopifnot(type %in% c("std.all", "std.lv", "std.nox"))
    
    # check output= argument
    output <- tolower(output)
    if(output %in% c("data.frame", "table")) {
      output <- "data.frame"
    } else if(output %in% c("text", "pretty")) {
      output <- "text"
    } else {
      stop("lavaan ERROR: output must be ", sQuote("data.frame"),
           " or ", sQuote("text"))
    }
    
    # no zstat + pvalue if estimator is Bayes
    if(object@Options$estimator == "Bayes") {
      zstat <- pvalue <- FALSE
    }
    
    # no se if class is not lavaan
    # using class() -- can't use inherits(), as this includes blavaan
    if(class(object)[1L] != "lavaan") {
      if(missing(se) || !se) {
        se <- FALSE
        zstat <- FALSE
        pvalue <- FALSE
      }
    }
    
    if(is.null(partable)) {
      PARTABLE <- inspect(object, "list")
    } else {
      PARTABLE <- partable
    }
    LIST <- PARTABLE[,c("lhs", "op", "rhs", "exo")]
    if(!is.null(PARTABLE$group)) {
      LIST$group <- PARTABLE$group
    }
    if(!is.null(PARTABLE$block)) {
      LIST$block <- PARTABLE$block
    }
    if (sum(nchar(PARTABLE$label)) != 0L) {
      LIST$label <- PARTABLE$label
    }
    
    # add std and std.all columns
    if(type == "std.lv") {
      LIST$est.std     <- lavaan:::lav_standardize_lv(object, est = est, GLIST = GLIST,
                                             partable = partable, cov.std = cov.std)
    } else if(type == "std.all") {
      LIST$est.std <- lavaan:::lav_standardize_all(object, est = est, GLIST = GLIST,
                                          partable = partable, cov.std = cov.std)
    } else if(type == "std.nox") {
      LIST$est.std <- lavaan:::lav_standardize_all_nox(object, est = est, GLIST = GLIST,
                                              partable = partable, cov.std = cov.std)
    }
    
    if(object@Options$se != "none" && se) {
      # add 'se' for standardized parameters
      VCOV <- try(lavaan:::lav_object_inspect_vcov(object, standardized = TRUE,
                                          type = type, free.only = FALSE,
                                          add.labels = FALSE,
                                          add.class = FALSE))
      if(inherits(VCOV, "try-error")) {
        LIST$se <- rep(NA, length(LIST$lhs))
        if(zstat) {
          LIST$z  <- rep(NA, length(LIST$lhs))
        }
        if(pvalue) {
          LIST$pvalue <- rep(NA, length(LIST$lhs))
        }
      } else {
        tmp <- diag(VCOV)
        # catch negative values
        min.idx <- which(tmp < 0)
        if(length(min.idx) > 0L) {
          tmp[min.idx] <- as.numeric(NA)
        }
        # now, we can safely take the square root
        tmp <- sqrt(tmp)
        
        # catch near-zero SEs
        zero.idx <- which(tmp < .Machine$double.eps^(1/3)) # was 1/2 < 0.6
        if(length(zero.idx) > 0L) {
          tmp[zero.idx] <- 0.0
        }
        LIST$se <- tmp
        
        # add 'z' column
        if(zstat) {
          tmp.se <- ifelse( LIST$se == 0.0, NA, LIST$se)
          LIST$z <- LIST$est.std / tmp.se
        }
        if(zstat && pvalue) {
          LIST$pvalue <- 2 * (1 - pnorm( abs(LIST$z) ))
        }
      }
    }
    
    # simple symmetric confidence interval
    if(se && object@Options$se != "none" && ci) {
      # next three lines based on confint.lm
      a <- (1 - level)/2; a <- c(a, 1 - a)
      fac <- qnorm(a)
      #if(object@Options$se != "bootstrap") {
      ci <- LIST$est.std + LIST$se %o% fac
      #} else {
      #    ci <- rep(as.numeric(NA), length(LIST$est.std)) + LIST$se %o% fac
      #}
      
      LIST$ci.lower <- ci[,1]; LIST$ci.upper <- ci[,2]
    }
    
    
    # if single group, remove group column
    if(object@Data@ngroups == 1L) LIST$group <- NULL
    
    # remove == rows?
    if(remove.eq) {
      eq.idx <- which(LIST$op == "==")
      if(length(eq.idx) > 0L) {
        LIST <- LIST[-eq.idx,]
      }
    }
    # remove <> rows?
    if(remove.ineq) {
      ineq.idx <- which(LIST$op %in% c("<",">"))
      if(length(ineq.idx) > 0L) {
        LIST <- LIST[-ineq.idx,]
      }
    }
    # remove := rows?
    if(remove.def) {
      def.idx <- which(LIST$op == ":=")
      if(length(def.idx) > 0L) {
        LIST <- LIST[-def.idx,]
      }
    }
    
    if(output == "text") {
      class(LIST) <- c("lavaan.parameterEstimates", "lavaan.data.frame",
                       "data.frame")
      # LIST$exo is needed for printing, don't remove it
      attr(LIST, "group.label") <- object@Data@group.label
      attr(LIST, "level.label") <- object@Data@level.label
      #attr(LIST, "header") <- FALSE
    } else {
      LIST$exo <- NULL
      LIST$block <- NULL
      class(LIST) <- c("lavaan.data.frame", "data.frame")
    }
    
    LIST
  }
