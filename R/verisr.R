#' Read in all the JSON files in directory
#'
#' This function will iterate through all the JSON files (regex pattern of "json$") in
#' the given directory and parse it as a VERIS object.
#' This will return a verisr object.
#'
#' @param dir the directory to list through
#' @keywords json
#' @export
#' @import rjson
#' @examples
#' \dontrun{
#' veris <- json2veris(dir="~/vcdb")
#' }
json2veris <- function(dir=".") {
  # create listing of files
  jfiles <- unlist(sapply(dir, list.files, pattern = "json$", full.names=T))
  # now read them all
  veris <- lapply(jfiles, function(jfile) {
    fromJSON(file=jfile, method='C')
  })    
  # set my class
  class(veris) <- "verisr"
  veris
}

#' Count the existance of a field in all records
#'
#' This function will count where the field is not null
#'
#' @param veris a verisr object
#' @param field the field to count
#' @export
#' @examples
#' \dontrun{
#' hacking <- count(veris, "action.hacking")
#' external <- count(veris, "actor.external")
#' }
count <- function(veris, field) {
  sapply(field, function(f) {
    vars <- unlist(strsplit(f, ".", fixed=T))
    sum(unlist(lapply(veris, function(x) ifelse(is.null(x[[vars]]), 0, 1))))
  })
}

#' Get a vector of values from an enumeration
#'
#' This will collect the values for an enumation 
#'
#' @param veris a verisr object
#' @param enum the field to count
#' @param add.n include a total count of variables found (denominator)
#' @param add.freq include a percentage (x/n)
#' @export
#' @examples
#' \dontrun{
#' hacking <- getenum(veris, "action.hacking.variety")
#' external <- getenum(veris, "actor.external.motive")
#' }
getenum <- function(veris, enum, filter=NULL, add.n=F, add.freq=F) {
  # get the internal list for the enumeration
  int.enum <- getintenum(veris, enum)
  # and apply the filter, it one was passed in
  if (!is.null(filter)) {
    int.enum <- ifelse(filter, int.enum, NA)
  }
  # count and aggreagte it
  count.enum <- table(unlist(int.enum))
  if (any(dim(count.enum) == 0)) {
    warning(paste("No values found for enum \"", enum, "\"", sep=""))
    return(data.frame())
  }
  # convert to data.frame
  enum.df <- data.frame(enum=names(count.enum), x=as.vector(count.enum))
  # order it
  enum.df <- enum.df[with(enum.df, order(x)), ]
  # reset the row names
  row.names(enum.df) <- 1:nrow(enum.df)
  # make the enum a factor
  enum.df$enum <- factor(enum.df$enum, levels=rev(enum.df$enum), ordered=T)
  if (add.n | add.freq) {
    # get the count of non-null values
    n <- sum(sapply(int.enum, function(x) {
      # it has length and is.na, return 0, else 1
      ifelse(length(x)==1, ifelse(is.na(x), 0, 1) ,1) })
    )
  }
  if (add.n) {
    enum.df$n <- n
  }
  if (add.freq) {
    enum.df$freq <- round(enum.df$x/n, 3)
  }
  enum.df
}

#' This will return a verisr filter object.
#' 
#' Get a filter given a list of "and" combinations and/or a list 
#' of "or" combinations.
#' Note: industry can be industryN
#' values can be "$exists" for that field being present
#' 
#' @param veris a verisr object
#' @param or list of criteria matching "or"
#' @param and list of criteria matching "and"
#' @export
#' @examples
#' \dontrun{
#' #tbd
#' hacking <- getenum(veris, "action.hacking.variety")
#' external <- getenum(veris, "actor.external.motive")
#' }
getfilter <- function(veris, and=NULL, or=NULL, or.not=NULL, and.not=NULL) {
  # this will return a matrix, one row for each
  # element in the list passed in, with a match
  # of the value in that list
  simple.match <- function(either) {
    sapply(names(either), function(x) {
      unlist(sapply(getintenum(veris, x), function(y) {
        if (either[[x]]=="$exists") {
          match.list <- as.logical(sum(!is.na(y)))
        } else {
          # note this means any vector is treated as an "or"
          match.list <- as.logical(sum(ifelse(either[[x]] %in% y, TRUE, FALSE)))
        }
        match.list
      }))
    })
  }
  retval <- NULL
  if (!is.null(or)) {
    or.retval <- as.logical(apply(simple.match(or), 1, sum))
  } else {
    or.retval <- rep(T, length(veris))
  }
  if (!is.null(and)) {
    #if all match, include the record
    and.retval <- ifelse(apply(simple.match(and), 1, sum)==length(and), TRUE, FALSE)
  } else {
    and.retval <- rep(T, length(veris))
  }
  if (!is.null(and.not)) {
    # if all match, exclude the record
    and.not.retval <- ifelse(apply(simple.match(and.not), 1, sum)==length(and.not), FALSE, TRUE)
  } else {
    and.not.retval <- rep(T, length(veris))    
  }
  if (!is.null(or.not)) {
    #if any are true, set to false (!) 
    or.not.retval <- !as.logical(apply(simple.match(or.not), 1, sum))
  } else {
    or.not.retval <- rep(T, length(veris))
  }
  # defaulting to an AND match between them
  sendlist <- (or.retval & and.retval & or.not.retval & and.not.retval)
  class(sendlist) <- "verisr.filter"
  sendlist
}  


#' Get a count of enumerations values by some other enumeration
#'
#' This will collect an enumation and count it.
#'
#' @param veris a verisr object
#' @param enum the field to count
#' @param by the second enumeration to sort by
#' @export
#' @examples
#' \dontrun{
#' hacking <- getenum(veris, "action.hacking.variety")
#' external <- getenum(veris, "actor.external.motive")
#' }
getenumby <- function(veris, enum, by) {
  # get the internal list for the enumeration
  primary.enum <- getintenum(veris, enum)
  #secondary.df <- getenum(veris, by)
  if(by=="industry2") {
    secondary.enum <- getindustry(veris, 2)
  } else {
    secondary.enum <- getintenum(veris, by)
  }
  by.list <- unique(unlist(secondary.enum))
  by.list <- by.list[!is.na(by.list)]
  rez <- do.call(rbind, lapply(by.list, function(x) {
    local.filter <- getsimfilter(secondary.enum, x)
    pri.count <- table(unlist(primary.enum[local.filter]))
    if (length(pri.count)) {
      int.df <- data.frame(enum=names(pri.count), 
                           x=as.vector(pri.count), 
                           primary=x)      
    } else {
      int.df <- NULL
    }
    int.df
  }) )
  if(by=="industry2") {
    rez$primary <- merge(rez, industry2, by.x="primary", by.y="code")$title
  }
  rez
}  
#foo <-getenumby(vcdb, "action.hacking.variety", "actor.external.variety")
#foo <-getenumby(vcdb, "action.hacking.variety", "industry2")
#ggplot(foo, aes(enum, x)) + geom_bar(stat="identity") + facet_wrap( ~ primary, ncol=2) + coord_flip() + theme_bw()

getindustry <- function(veris, len=2) {
  int.enum <- getintenum(veris, "victim.industry")
  sapply(int.enum, function(x) {
    sapply(x, function(y) {
      ret.val <- NA
      if (nchar(y) > len) {
        ret.val <- substr(y, 1, len)
        if (ret.val==rep("0", len)) {
          ret.val <- NA
        }
      } 
      ret.val      
    })
  })  
}

#' Internal Function.
#' This will iterate through the veris object and return
#' a list of matches.  This is intented to maintain the orginal
#' indexes of the veris object so further manipulation can be done.
#' 
#' Note: Can do a special "industryN" request and it will chop
#' off the industry at the N value or return same length of zeros
#' if it isn't long enough.
#' 
#'
#' @param veris a verisr object
#' @param enum the field to count
getintenum <- function(veris, enum) {
  # if the veris object has null names and yet length
  # it is an array, and we simply want to step into
  # and through it.  The top level veris object
  # is an array, as is things like victim and assets
  # and data variety
  if(is.null(names(veris)) & length(veris)) {
    return(lapply(veris, getintenum, enum))
  }
  # now we are in the meat of the function
  # and we should have either a full slice
  # or a partial slice of a veris incident
  
  # look at the enum passed in, want to 
  # grab the first ("tag") and concatenate the rest
  vars <- unlist(strsplit(enum, "[.]"))
  tag <- vars[1]
  therest <- paste(vars[-1], collapse='.')
  # if the veris object is null at "tag", return NA
  if (is.null(veris[[tag]])) {
    retval <- NA
  } else if (therest == "") {
    # else if we are at the end of our enum, return the value?
    if (length(veris[[tag]])==0) {
      retval <- NA
    }
    # if we have names return those
    # it's an easy way to count actions, actors, etc.
    these.names <- names(veris[[tag]])
#    cat("the rest is blank, names:", these.names, "null:", is.null(these.names), "\n")
    if (tag=="assets") {
      assetmap <- c("S"="Server", "N"="Network", "U"="User Dev", "M"="Media", 
                    "P"="Person", "T"="Kiosk/Term", "Unknown"="Unknown")
      retval <- sapply(veris[[tag]], function(asset) {
        myasset <- ifelse(asset$variety=="Unknown", "Unknown", substr(asset$variety, 1, 1))
        myamount <- ifelse(is.null(asset$amount), 1, asset$amount)
        rep(assetmap[[myasset]], myamount)
      })
    } else if (!is.null(these.names)) {
      retval <- these.names
    } else {
      retval <- veris[[tag]]
    }
  } else {
    # else we need to continue to "drill down" into the veris object
    # with the rest of the enum being quieried
    # passing it back to self so it can parse through arrays
    # and use the same logic to continue parsing
    #
    # but before we do, let's check for some unique variables
    # like "industry*" where * is a length to chop
    if (grepl("^industry\\d$", therest, perl=T)) {
      # figure out the length of industry to return
      ind.len <- substr(therest, 9, 9)
      retval <- getintenum(veris[[tag]], "industry")
      retval <- lapply(retval, function(x) {
        i <- substr(x, 1, ind.len)
        ifelse(nchar(i)==ind.len, i, paste(rep("0", ind.len), collapse=""))
      })
    } else {
      retval <- getintenum(veris[[tag]], therest)      
    }
  }
  retval
}

#' Internal Function.
#' This will create a mask of indexes based on filter
#' a list of matches.  This is intented to maintain the orginal
#' indexes of the veris object so further manipulation can be done.
#'
#' @param veris a verisr object
#' @param enum the field to count
#' @param value 
# getfilter <- function(veris, enum, value) {
#   # test if value is regex!
#   int.enum <- getintenum(veris, enum)
#   sapply(int.enum, function(x) { ifelse(value %in% x, TRUE, FALSE)})
# }
#   
# getsimfilter <- function(int.enum, value) {
#   sapply(int.enum, function(x) { ifelse(value %in% x, TRUE, FALSE)})
# }  



#dir <- "~/Documents/github/VCDB/incidents"
#dir <- "~/Documents/json/newfinal/uscert"
# dir <- c("~/Documents/github/VCDB/incidents", "~/Documents/json/newfinal/vzint")
#vcdb <- json2veris(dir)

#' Displays a useful description of a ggplot object
#' 
#' @param object veris object to summarise
#' @param ... other arguments ignored (for compatibility with generic)
#' @keywords internal
#' @method summary verisr
#' @export
summary.verisr <- function(object, ...) {
  x <- object
  cat(paste(length(x), "incidents in this object.\n"))
  actor <- c("ext"="actor.external", 
            "int"="actor.internal",
            "prt"="actor.partner")
  cat("\n")
  print(count(x, actor))
  action <- c("mal"="action.malware",
              "hak"="action.hacking",
              "soc"="action.social",
              "mis"="action.misuse",
              "err"="action.error",
              "phy"="action.physical",
              "env"="action.environmental")
  cat("\n")
  print(count(x, action))
}

