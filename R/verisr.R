#' Read in all the VERIS incidents (JSON files) in a given directory.
#'
#' This function will iterate through all the JSON files (regex pattern of "json$") in
#' the given directory and parse it as an encoded VERIS record.  This function
#' requires that a JSON schema be available for the VERIS data.  If the variable is 
#' not specified, it will attempt to grab the "verisc-merged.json" schema from
#' https://raw.githubusercontent.com/vz-risk/veris/master/verisc-merged.json.
#' 
#' This will return a verisr object, which is a data.table object and can be 
#' directly accesses as such.
#' 
#' Couple of unique things...  The returned object will have additional fields 
#' for convenience: 
#'   * *actor* will return top level actor categories
#'   * *action* will return top level action categories
#'   * *asset.variety* will return top level asset categories
#'   * *attribute* will return top level asset categories
#'   * *victim.industry2* will return the first 2 digits of the NAICS code
#'   * *victim.industry3* same, first 3 digits
#'   * *victim.orgsize* returns "Large" and "Small" enumerations
#' 
#' The victim.secondary.victim_id, external.actor.region, and any other free
#' text field that can be repeated is being collapsed into a single string 
#' seperated by a comma at the moment.  If that poses a challnge, open an issue
#' on it.
#'
#' @param dir the directory to list through.  This may be a vector of 
#' directorites, in which case each all the matching files in each 
#' directory will be laoded.
#' @param schema a full veris schema with enumerations included.
#' @keywords json
#' @import rjson
#' @import data.table
#' @import RCurl
#' @export
#' @examples
#' \dontrun{
#' # load up all the veris files in the "vcdb" directory
#' # grab the schema off of github.
#' veris <- json2veris(dir="~/vcdb")
#' 
#' # specify a local schema with localized plus section.
#' veris <- json2veris(dir="~/vcdb", 
#'                     schema="~/veris/verisc-local.json")
#' }
json2veris <- function(dir=".", schema=NULL) {
  # if no schema, try to load it from github
  if (missing(schema)) {
    x <- getURL("https://raw.githubusercontent.com/vz-risk/veris/master/verisc-merged.json")
    lschema <- fromJSON(json_str=x)
  } else {
    lschema <- fromJSON(file=schema)
  }  
  # create listing of files
  jfiles <- unlist(sapply(dir, list.files, pattern = "json$", full.names=T))
  numfil <- length(jfiles)
  # need to pull these before we loop, used over and over in loop
  a4 <- geta4names()
  vtype <- parseProps(lschema)
  # get a named vector of field and types
  vft <- getverisdf(lschema, a4)
  # now create a data table with the specific blank types
  # we just pulled from getverisdf()
  veris <- as.data.table(lapply(seq_along(vft), function(i) {
    if (vft[i]=="character") rep(NA_character_, numfil)
    else if (vft[i]=="logical") rep(FALSE, numfil)
    else if (vft[i]=="integer") rep(NA_real_, numfil)
    else if (vft[i]=="double") rep(NA_real_, numfil)
  }))
  setnames(veris, names(vft))
  # get a text progress bar going
  pb <- txtProgressBar(min = 0, max = length(jfiles), style = 3)
  # in each file, pull out the values and fill in the data table
  for(i in seq_along(jfiles)) {
    json <- fromJSON(file=jfiles[i], method='C')
    nfield <- nameveris(json, a4, vtype)
    if (length(nfield)==0) warning(paste("empty json file parsed from", jfiles[i]))
    for(x in names(nfield)) {
      tt <- tryCatch(set(veris, i=i, j=x, value=paste(nfield[[x]], collapse=",")),
                     error=function(e) e, warning=function(w) w)
      if(is(tt,"warning")) {
        cat(paste0("Warning found trying to set ", i, ", \"", x, "\" for \"", nfield[[x]], "\"\n"))
        cat("  length of assignment:", length(nfield[[x]]), "\n")
        cat("  in", i, jfiles[i], "\n")
        print(tt)
        cat("\n")
      }
      
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  veris <- post.proc(veris)
  class(veris) <- c("verisr", class(veris))
  veris
}

#' Post process the veris object to add convenience fields.
#' 
#' Given a veris object this will populate several convenience fields
#' like the victim.industry2 and industry3, 
#' 
#' @param veris the verisr object
post.proc <- function(veris) {
  # orgsize
  small <- c("victim.employee_count.1 to 10", "victim.employee_count.11 to 100", 
             "victim.employee_count.101 to 1000", "victim.employee_count.Small")
  large <- c("victim.employee_count.1001 to 10000", "victim.employee_count.10001 to 25000", 
             "victim.employee_count.25001 to 50000", "victim.employee_count.50001 to 100000", 
             "victim.employee_count.Over 100000", "victim.employee_count.Large")
  veris[ , victim.orgsize.Small := rowSums(veris[ ,small, with=F]) > 0]
  veris[ , victim.orgsize.Large := rowSums(veris[ ,large, with=F]) > 0]
  # victim.industry
  veris[ , victim.industry2 := substring(unlist(veris[ ,"victim.industry", with=F], 
                                                use.names=F), 1L, 2L)]
  veris[ , victim.industry3 := substring(unlist(veris[ ,"victim.industry", with=F], 
                                                use.names=F), 1L, 3L)]
  # actor.partner.industry
  veris[ , victim.industry2 := substring(unlist(veris[ ,"actor.partner.industry", with=F], 
                                                use.names=F), 1L, 2L)]
  veris[ , victim.industry3 := substring(unlist(veris[ ,"victim.industry", with=F], 
                                                use.names=F), 1L, 3L)]
  veris
}

#' Map VERIS fields to data type.
#'
#' Given a json schema for VERIS, this function will return a named vector
#' where the name is the field and the value is the R mode string or "enum".
#'
#' @param schema the merged veris schema in json
#' @param cur the current name (internal)
#' @param outvec the current output vector (internal)
#' @keywords json
parseProps <- function(schema, cur="", outvec=NULL) {
  if ('items' %in% names(schema)) {
    if ('enum' %in% names(schema[['items']])) {
      vnames <- c(names(outvec), cur)
      outvec <- c(outvec, "enum")
      outvec <- setNames(outvec, vnames)
    } else {
      outvec <- parseProps(schema[['items']], cur, outvec)
    }
  } else if ('type' %in% names(schema)) {
    if(schema[['type']]=='object') {
      outvec <- parseProps(schema[['properties']], cur, outvec)
    } else if ('enum' %in% names(schema)) {
      vnames <- c(names(outvec), cur)
      outvec <- c(outvec, "enum")
      outvec <- setNames(outvec, vnames)
    } else {
      setto <- "character"
      if(schema[['type']] == 'number') {
        setto <- "double"
      } else if (schema[['type']] == 'integer') {
        setto <- "integer"
      }
      vnames <- c(names(outvec), cur)
      outvec <- c(outvec, setto)
      outvec <- setNames(outvec, vnames)
    }
  } else {
    for(x in names(schema)) {
      newcur <- ifelse(nchar(cur), paste(cur, x, sep='.'), x)
      outvec <- parseProps(schema[[x]], newcur, outvec)
    }
  }
  outvec
}

#' Create a list of all column names expected from JSON schema.
#'
#' Given a json schema for VERIS, this function will return a vector
#' of columns names to be set in the verisr object.  This is a wrapper 
#' around mkenums() which is a recursive function.  This will clean up
#' some of the one-off things we do after reading the schema.
#'
#' @param schema the merged veris schema in json
#' @keywords json
veriscol <- function(schema) {
  rawfields <- mkenums(schema)
  gfields <- c("^ioc", "^impact", 
               "attribute.confidentiality.data.amount", 
               "asset.assets.amount")
  clean <- rawfields[grep(paste(gfields, collapse="|"), rawfields, invert=T)]
  wonkyvariety <- clean[grep('asset.assets.variety|attribute.confidentiality.data.variety', clean)]
  wonkyamount <- sapply(strsplit(wonkyvariety, "[.]"), function(x) {
    x[x=="variety"] <- "amount"
    paste(x, collapse=".")
  })
  sort(c(wonkyamount, clean))
}

#' Create a raw list of all column names expected from JSON schema.
#'
#' Given a json schema for VERIS, this function will return a vector
#' of columns names to be set in the verisr object. 
#'
#' @param schema the merged veris schema in json
#' @param cur the current named value
#' @param outvec the vector passed around building the output
#' @keywords json
mkenums <- function(schema, cur="", outvec=NULL) {
  if ('items' %in% names(schema)) {
    if ('enum' %in% names(schema[['items']])) {
      outvec <- c(outvec, paste(cur, schema[['items']][['enum']], sep='.'))
    } else {
      outvec <- mkenums(schema[['items']], cur, outvec)
    }
  } else if ('type' %in% names(schema)) {
    if(schema[['type']]=='object') {
      outvec <- mkenums(schema[['properties']], cur, outvec)
    } else if ('enum' %in% names(schema)) {
      outvec <- c(outvec, paste(cur, schema[['enum']], sep='.'))
    } else {
      outvec <- c(outvec, cur)
    }
  } else {
    for(x in names(schema)) {
      newcur <- ifelse(nchar(cur), paste(cur, x, sep='.'), x)
      outvec <- mkenums(schema[[x]], newcur, outvec)
    }
  }
  outvec
}

#' Merges veriscol and parseProps in a single object.
#'
#' Given the schema object, will identify each column type to be 
#' created into a data.table.
#' 
#' TODO: need to add the convenience fields in here.
#'
#' @param lschema the json schema object for VERIS
#' @param a4 the a4 named vector for convenience fields
getverisdf <- function(lschema, a4) {
  # get a named vector of VERIS objects
  # e.g. c("action.hacking.variety" = "enum")
  vtype <- parseProps(lschema)
  # get a vector of veris columns
  # e.g. c("action.hacking.variety.Brute force", "action.hacking.variety.SQLi"...)
  vfield <- veriscol(lschema)
  out <- sapply(vfield, function(x) {
    ret <- vtype[x]
    if (is.na(ret)) {
      temp <- unlist(strsplit(x, '[.]'))
      base <- paste(temp[1:(length(temp)-1)], collapse='.')
      ret <- vtype[base]
    }
    ifelse(ret=="enum", "logical", ret)
  }, USE.NAMES=F)
  out <- c(out, rep("logical", length(a4)))
  setNames(out, c(vfield, names(a4)))
}

#' Return a dense list of only the VERIS variables and values in the JSON.
#' 
#' Given an incident in json format, it will process the file and 
#' return a simple list object where each element is named with 
#' the field name and the value is the value read in, or True if the
#' field is an enumeration.
#' 
#' @param json the json object from reading in a file.
#' @param vtype the vtype ojbect from parseProps()
#' @param cur the current field name as it is being built
#' @param outlist the return value being passed internally
nameveris.recurs <- function(json, vtype, cur=NULL, outlist=list()) {
  # Three options:
  #   named values (fields to loop through)
  #   looped variety object (asset.assets or data variety)
  #   a value itself
  
  # if named values, loop through each of the children and recurse to myself
  if (length(names(json))>0) {
    for(x in names(json)) {
      curname <- ifelse(is.null(cur), x, paste(cur, x, sep='.'))
      outlist <- nameveris.recurs(json[[x]], vtype, cur=curname, outlist)
    }
  # if one of the repeated values, handle uniquely
  } else if (cur %in% c('asset.assets', 'attribute.confidentiality.data')) {
    for(x in json) {
      if ('variety' %in% names(x)) {
        curname = paste(cur, 'variety', x[['variety']], sep='.')
        outlist[curname] <- TRUE
        if ('amount' %in% names(x)) {
          curname = paste(cur, 'amount', x[['variety']], sep='.')
          outlist[curname] <- x[['amount']]
        }
      }
    }
  # else this is field to assign, assign it
  } else {
    if (cur %in% names(vtype)) {
      if (vtype[cur] == "enum") {
        for(x in json) {
          curname = paste(cur, x, sep='.')
          outlist[[curname]] = TRUE
        }
      } else {
        if (!mode(json) %in% c("character", "numeric")) {
          cat('mode of', cur, "is", mode(json), "\n")
        }
        outlist[[cur]] = json
      }
    } else if (cur %in% c("actor.unknown", "action.unknown")) {
      outlist[[sub('u', 'U', cur)]] <- TRUE
    # } else {
      # warning(paste("Invalid data in JSON, dropping value of", cur))
    }
  }
  outlist
}

#' Return a dense and complete list of VERIS variables and values
#' 
#' Given an incident in json format, it will process the file and 
#' return a simple list object where each element is named with 
#' the field name and the value is the value read in, or True if the
#' field is an enumeration.
#' 
#' @param json the json object from reading in a file.
#' @param a4 the a4 object from geta4names()
#' @param vtype the vtype object from parseProps()
nameveris <- function(json, a4, vtype) {
  olist <- nameveris.recurs(json, vtype)
  # start simple, with the actor, action, asset and attribute fields
  for(a4name in names(a4)) {
    if (any(grepl(paste0('^', a4[a4name]), names(olist)))) {
      olist[[a4name]] = TRUE
    }
  }  
  olist
}

#' Convenience function for the a4 names and values
#' 
#' This returns a named vector where the names are the column names 
#' in the final verisr data table and the valus are suitable for using
#' in a regex in the existing column names.  
geta4names <- function() {
  convenience <- function(nm) {
    out <- tolower(nm)
    setNames(tolower(nm), nm)
  }
  actor <- convenience(paste('actor', 
                             c('External', 'Internal', 'Partner', 'Unknown'), 
                             sep="."))
  action <- convenience(paste('action', 
                              c('Malware', 'Hacking', 'Social', 'Physical', 
                                'Misuse', 'Error', 'Environmental', 'Unknown'), 
                              sep="."))
  attribute <- convenience(paste('attribute', 
                                 c('Confidentiality', 'Integrity', 'Availability'), 
                                 sep="."))
  assetmap <- c("S "="Server", "N "="Network", "U "="User Dev", "M "="Media", 
                "P "="Person", "T "="Kiosk/Term", "Un"="Unknown")
  asset <- setNames(paste('asset.assets.variety', names(assetmap), sep='.'),
                    paste('asset.variety', assetmap, sep='.'))
  c(actor, action, asset, attribute)  
}


counto <- function(olist) {
  cnm <- names(olist)
  found.enum <- unlist(lapply(c('action', 'actor', 'attribute'), function(x) {
    paste(x, unique(getnth(cnm[grep(paste0("^", x), cnm)])), sep=".")
  }))
  for(x in found.enum) {
    olist[[x]] <- TRUE
  }
  assetmap <- c("S "="Server", "N "="Network", "U "="User Dev", "M "="Media", 
                "P "="Person", "T "="Kiosk/Term", "Un"="Unknown")
  outs <- cnm[grep('^asset.assets.variety', cnm)]
  if (length(outs) > 0) {
    found.enum <- paste('asset.assets', assetmap[substr(getlast(outs), 1,2)], sep=".")
    for(x in found.enum) {
      olist[[x]] <- TRUE
    }
  }
  # TODO : need to add more fields for convenience things like:
  # victim.industry2
  # victim.industry3
  # victim.orgsize, (Small/Large)
  # victim.region
  # victim.subregion
  olist
}

#' Get the last element from a column name
#' 
#' Givn a vector with one or more column names (veris fields), this will
#' return the last string in the name, as it is seperated by [.].
#' 
#' @param nm the vector of column names
getlast <- function(nm) {
  sapply(nm, function(x) {
    temp <- unlist(strsplit(x, '[.]'))
    temp[length(temp)]
  })
}

#' Get the nth element from a column name
#' 
#' Givn a vector with one or more column names (veris fields), this will
#' return the nth string in the name, as it is seperated by [.].
#' 
#' @param nm the vector of column names
#' @param which the nth vlue to return
getnth <- function(nm, which=2) {
  sapply(nm, function(x) {
    temp <- unlist(strsplit(x, '[.]'))
    temp[2]
  })
}

#' Get a vector of values from an enumeration
#'
#' This will collect the values for an enumation 
#' 
#' Note there are some special values that can be set as the enumeration
#' that are not obvious. :
#' * actor, action, attribute: will all return the next level down.  For example, just querying for "action" will return "malware", "hacking", and so on.
#' * asset.assets: will return the type of assets, "Server", "Network, "User Dev" and so on
#' * victim.industry2: will return a short label of industries based on 2 values of NAICS code.
#' * victim.industry3: will return a short label of industries based on 3 values of NAICS code.
#'
#' Change: the "add.n" and "add.freq" options are now TRUE by default.
#' Change: the "primary" and "secondary" arguments were dropped.
#' 
#' @param veris a verisr object
#' @param enum the field to count
#' @param filter limit what records are searched (optional)
#' @param add.n include a total count of variables found (denominator)
#' @param add.freq include a percentage (x/n)
#' @export
#' @examples
#' \dontrun{
#' hacking <- getenum(veris, "action.hacking.variety")
#' external <- getenum(veris, "actor.external.motive")
#' }
getenum <- function(veris, enum, filter=NULL, add.n=T, add.freq=T) {
  if (missing(filter)) {
    filter <- rep(T, nrow(veris))
  } else if (length(filter) != nrow(veris)) {
    warning(paste0("filter is not same length (", length(filter),
                   ") as object (", nrow(veris), ")."))
    return(NULL)
  }
  
  # get names from the veris object
  cnames <- colnames(veris)
  # extract by the enumeration
  # if field name exists as is, return it, else search.
  if(any(grepl(paste0('^', enum, "$"), cnames))) {
    warning("need to return single field here")
  } else {
    # only match where there are one level of enumerations 
    # after the requested enum
    gkey <- paste0("^", enum, ".[^.]+$")
    thisn <- cnames[grep(gkey, cnames)]
    ret <- setNames(colSums(veris[filter ,thisn, with=F]), getlast(thisn))
    ret <- ret[ret>0]
    outdf <- data.table(enum=names(ret), x=ret)
    n <- sum(rowSums(veris[filter ,thisn, with=F]) > 0)
    if (add.n) outdf$n <- n
    if (add.freq) outdf$freq <- outdf$x/n
    outdf <- outdf[order(rank(x), enum)]
    outdf$enum <- factor(outdf$enum, levels=outdf$enum, ordered=T)
    outdf
  }
}

#' Get a count of enumerations values by some other enumeration
#'
#' This will collect an enumation and count it.
#'
#' Change: the "add.n" and "add.freq" options are now TRUE by default.
#' 
#' @param veris a verisr object
#' @param enum the main enumeration field 
#' @param primary the primary enumeration to filter on
#' @param secondary the (optional) secondary enumeration to filter on
#' @param filter limit what records are searched (optional)
#' @param add.n include a total count of variables found (denominator)
#' @param add.freq include a percentage (x/n)
#' @param fillzero fill in missing matches with zeros
#' @export
#' @examples
#' \dontrun{
#' hacking <- getenumby(veris, "action", "asset.variety", fillzero=T)
#' }
getenumby <- function(veris, enum, primary=NULL, secondary=NULL, filter=NULL, 
                      add.n=T, add.freq=T, fillzero=T) {
  if (missing(filter)) {
    filter <- rep(T, nrow(veris))
  } else if (length(filter) != nrow(veris)) {
    warning(paste0("filter is not same length (", length(filter),
                   ") as object (", nrow(veris), ")."))
    return(NULL)
  }
  cnames <- colnames(veris)
  enum <- c(enum, primary, secondary)
  gkey <- paste0("^", enum, ".[^.]+$")
  savethisn <- thisn <- lapply(gkey, function(x) cnames[grep(x, cnames)])
  thisn$x <- 0
  outdf <- as.data.table(expand.grid(thisn))
  cnm <- colnames(outdf)[1:(ncol(outdf)-1)]
  for(i in seq(nrow(outdf))) {
    this.comp <- as.character(unlist(outdf[i, cnm, with = F]))
    count <- sum(rowSums(veris[filter, this.comp, with=F]) == length(enum))
    # cat("comparing:", paste(this.comp, collapse=" > "), "=", count, "\n")
    outdf[i, x:=count]
  }
  for(column in cnm) {
    tempcol <- getlast(as.character(unlist(outdf[ , column, with=F])))
    outdf[ , column:=tempcol, with=F]
  }
  extra.names <- NULL
  if (length(enum)>1) extra.names <- paste0('enum', seq((length(enum)-1)))
  setnames(outdf, c('enum', extra.names, 'x'))
  n <- sum(rowSums(veris[filter ,unlist(savethisn), with=F]) > 0)
  if (add.n) outdf$n <- n
  if (add.freq) outdf$freq <- outdf$x/n
  # name the columns... enum enum1 enum2 (?)
  # print(outdf)
  #outdf <- outdf[order(-rank(x), enum)]
  #outdf$enum <- factor(outdf$enum, levels=outdf$enum, ordered=T)
  outdf
}

#' Displays a useful description of a verisr object
#' 
#' @param object veris object to summarise
#' @param ... other arguments ignored (for compatibility with generic)
#' @keywords internal
#' @method summary verisr
#' @export
summary.verisr <- function(object, ...) {
  veris <- object
  cat(paste(nrow(veris), "incidents in this object.\n"))
  actor <- getenum(veris, "actor", add.freq=T)
  action <- getenum(veris, "action", add.freq=T)
  asset <- getenum(veris, "asset.variety", add.freq=T)
  attribute <- getenum(veris, "attribute", add.freq=T)
  outlist <- list(actor=actor, action=action, asset=asset, attribute=attribute)
  full <- lapply(names(outlist), function(n) {
    thisdf <- outlist[[n]]
    ret <- unlist(lapply(seq(nrow(thisdf)), function(i) {
      rep(thisdf$enum[i], thisdf$x[i])
    }))
    ret
  })
  maxline <- max(sapply(full, length))
  temp.out <- as.data.frame(lapply(full, function(x) {
    c(as.character(x), rep(NA, maxline-length(x)))
  }))
  names(temp.out) <- names(outlist)
  outs <- summary(temp.out, maxsum=100)
  outs[grep("^NA\'s", outs)] <- ""
  outs
}

#' Displays a four panel barplot of a verisr object
#' 
#' @param object veris object to summarise
#' @param ... other arguments ignored (for compatibility with generic)
#' @keywords internal
#' @import ggplot2
#' @import grid
#' @import gridExtra
#' @method plot verisr
#' @export
plot.verisr <- function(x, y, ...) {
  # @importFrom ggplot2 ggplotGrob
  veris <- x
  actor <- getenum(veris, "actor", add.freq=T)
  action <- getenum(veris, "action", add.freq=T)
  asset <- getenum(veris, "asset.variety", add.freq=T)
  attribute <- getenum(veris, "attribute", add.freq=T)
  a4 <- list(actor=actor, action=action,
             asset=asset,
             attribute=attribute)
  ht_mult <- c(0.22)  # multiplier for each row
  highest <- (max(sapply(a4, nrow))*ht_mult)
  
  plots <- lapply(names(a4), function(x) {
    this.ht <- (nrow(a4[[x]])*ht_mult)
    ht.diff <- (highest - this.ht) * 0.5
    simplebar(a4[[x]], x, plot.margin=unit(c(0,0,ht.diff,0), "npc"))
  })
#   foo <- arrangeGrob(plots[[1]], plots[[2]], plots[[3]], plots[[4]], nrow=2)
#   print(foo)

#   ggplotGrob <- ggplot2::ggplotGrob
  require(ggplot2)
   print(do.call(arrangeGrob, c(plots, nrow=2)))
#   print(do.call(grid.arrange, c(plots, nrow=2)))
}


#' Metadata for 2-digit NAICS industry classification
#'
#' This data allows a mapping between two digit NAICS code and the 
#' full definition provided by the NAICS specification and a shorter
#' version of the title for compact visuals.
#' @name industry2
#' @docType data
#' @references \url{www.census.gov/naics/}
#' @keywords data
NULL

#' Metadata for 3-digit NAICS industry classification
#'
#' This data allows a mapping between three digit NAICS code and the 
#' full definition provided by the NAICS specification.
#' @name industry3
#' @docType data
#' @references \url{www.census.gov/naics/}
#' @keywords data
NULL