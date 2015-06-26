lbl <- lower <- upper <- percent <- code <- short <- title <- 
  spread <- enum1 <- Large <- Small <- Unknown <- Total <- 
  pct <- labx <- laby <- amount <- type <- ..density.. <- logic <-
  math_format <- .x <- timeline.incident.year <- primary <- NULL
 contains <- starts_with <- function(x) NULL

#' Sets up an object of multiple enumerations
#' 
#' Given a verisr object and one or more enumerations to query, it will set up an
#' object that enables plotting by \code{\link{plotjchunk}}.
#' The object also contains information that will help set the optimal height 
#' of the object (for example, in a knitr document) and information about the 
#' data queried.
#' 
#' @param veris the verisr data 
#' @param enums one or more enumeration to query (e.g. c("victim.orgsize", "victim.industry2"))
#' @param trim maximum values to return for any single enumeration, set to NULL to not trim
#' @param unknowns vector of strings to treat as "Unknown" values to not be counted in "n"
#' @import dplyr
#' @export
#' @seealso plotjchunk
#' @seealso simplebar
#' @seealso simplejbar
#' 
#' @return a list with the following objects:
#'   * *ucount* : count of unknown in enumeration
#'   * *df* : the data frame(s) returned by getenum()
#'   * *title* : vector of suitable titles for plotting
#'   * *enum* : the specific enum for this chunk
#'   * *height* : vector with the height of each plot
#'   * *ptall* : the max height the entire plot, useful for knitr calls
setjenum <- function(veris, enums, trim=10, unknowns=c("Unknown", " - Other")) { 
  ## cycle through each enum passed in
  chunk <- lapply(seq_along(enums), function(i) {
    thisenum <- getenum(veris, enums[i], add.freq=F, add.n=T, fillzero=F, exclusive=T)
    if(nrow(thisenum)) {
      ## look for unknowns
      unknowns <- paste(unknowns, collapse="|", sep="")
      # save the index of the unknowns locations
      umatches <- grepl(unknowns, thisenum$enum)
      # assume no Unknowns are found
      ucount <- 0
      if(any(umatches)) {
        # set ucount for unknowns if they are found
        ucount <- sum(thisenum$x[umatches])
        # remove the matched unknowns from the enumdf
        thisenum <- filter(thisenum, !umatches)
      }
      # modify the n and set freq now that it's clear of unknowns
      thisenum <- thisenum %>% mutate(n=n-ucount, freq=x/n) %>% arrange(freq)
      
      # reduce length of enumdf it if trim is set
      if(!is.null(trim) && trim>0) thisenum <- tail(thisenum, trim)
      thisenum$enum <- factor(thisenum$enum, levels=thisenum$enum, ordered=T)
      # get a count of total records found
      thisn <- thisenum$n[1]
      # set the title
      title <- paste0(enums[i], " (n=", format(thisn, big.mark=",", scientific=F), ")\n",
                      "Unknowns=", format(ucount, big.mark=",", scientific=F), "/", 
                      format((thisn+ucount), big.mark=",", scientific=F),
                      " (", round(ucount/(thisn+ucount), 2)*100, "%)")
      ## get the height of 
      hts <- 0.85 + nrow(thisenum)/4 
    } else {  # empty df
      ucount <- 0
      title <- "not found"
      hts <- 1
    }
    list(ucount=ucount, enum=enums[i], df=thisenum, title=title, height=hts)
  })
  chunk$ptall <- max(sapply(chunk, function(x) x$height))
  chunk
}

#' Plot the object returned from setjenum
#' 
#' This will plot the data returned from a call to \code{\link{setjenum}}.  
#' Internally, it will call \code{\link{simplebar}} or \code{\link{simplejbar}} 
#' depending if \code{final} is TRUE or FALSE respectively.
#' 
#' @param chunk the object returned by \code{\link{setjenum}}
#' @param final boolean, whether we want to make it pretty (TRUE) or detailed (FALSE)
#' @import gridExtra
#' @export
#' @seealso setjenum
plotjchunk <- function(chunk, final=F) {
  maxht <- max(sapply(chunk, function(x) if(is.list(x)) { x$height } else { 0 }))
  plots <- lapply(chunk[sapply(chunk, is.list)], function(x) {
    if (nrow(x$df)==0) {
      textGrob(paste("No records with", x$enum, "defined.", sep="\n"))
    } else {
      rdif <- maxht-x$height
      if (final) {
        simplebar(x$df, x$title, plot.margin=unit(c(0,0,rdif,0), "inches"))
      } else {
        simplejbar(x$df, x$title, plot.margin=unit(c(0,0,rdif,0), "inches"))
      }
    }
  })
  if(length(plots)==1) {
    if (final) {
      ret <- plots[[1]] 
    } else {
      blank <- textGrob(" ")
      wth <- list(widths=c(0.4, 1, 0.4))
      plots <- list(blank, plots[[1]], blank)
      ret <- do.call(arrangeGrob, c(plots, nrow=1, wth))
    }
  } else {
    ret <- do.call(arrangeGrob, c(plots, nrow=1))    
  }
  ret
}

#' Plot a "simple" bar chart from an enumeration data.frame without details
#' 
#' This will return a ggplot render of an enumeration stored in a data.frame.  
#' The enumeration could be from the basic \code{\link{getenum}} call, or 
#' the \code{df} entry in the object returned by \code{\link{setjenum}}. 
#' 
#' "simplebar" will return a plot that is simplified and polished.  
#' It is just showing the percentage next to the 
#' end of a solid bar for the value.
#' 
#' "simplejbar" is intended for an analyst to review as it showing counts and 
#' percentages next to a solid bar with an error bar included. 
#' 
#' @param enumdf the data.frame containing the enumeration, expected to have the 
#' text label in "enum" and "freq" with the frequecy variaable.
#' @param title the title for the plot
#' @param ... optional variables passed into \code{ggplot::theme}
#' @export
#' @import ggplot2
#' @aliases simplejbar
simplebar <- function(enumdf, title, ...) {
  enumdf$lbl <- paste0(enumdf$x, ' (', round(enumdf$freq*100, 1), "%)")
  enumdf$lbl <- paste0(round(enumdf$freq*100, 1), "%")
  yexp <- max(enumdf$freq)*1.22
  gg <- ggplot(enumdf, aes(x=enum, y=freq, label=lbl))
  gg <- gg + ggtitle(title)
  gg <- gg + geom_bar(width=0.90, stat="identity", fill="steelblue")
  gg <- gg + geom_hline(yintercept=0)
  #  gg <- gg + geom_errorbar(aes(ymin=lower, ymax=upper), color="lightsteelblue", alpha=2/3, size=5, width=0)
  gg <- gg + geom_text(hjust=-0.1,color="black", size=4) + coord_flip()
  gg <- gg + scale_y_continuous(expand = c(0, 0), limits=c(0, yexp), labels=percent)
  gg <- gg + xlab("") + ylab("") + theme(axis.text.x=element_blank(), 
                                         axis.line.x=element_line(color="red", size=15),
                                         ...)
  gg    
}

#' @export
#' @import ggplot2
#' @import binom
simplejbar <- function(enumdf, title, ...) {
  enumdf <- cbind(enumdf, binom.wilson(enumdf$x, enumdf$n))
  enumdf$lbl <- paste0(enumdf$x, ' (', round(enumdf$freq*100, 1), "%)")
  yexp <- max(enumdf$upper)*1.42
  gg <- ggplot(enumdf, aes(x=enum, y=freq, label=lbl))
  gg <- gg + ggtitle(title)
  gg <- gg + geom_bar(width=0.90, stat="identity", fill="steelblue")
  gg <- gg + geom_errorbar(aes(ymin=lower, ymax=upper), color="lightsteelblue", alpha=2/3, size=5, width=0)
  gg <- gg + geom_text(hjust=-0.1,color="black", size=3) + coord_flip()
  gg <- gg + scale_y_continuous(expand = c(0, 0), limits=c(0, yexp), labels=percent)
  gg <- gg + xlab("") + ylab("") + theme(...)
  gg    
}

#' Convert 2 or 3 digit naics codes to matching industry title.
#' 
#' Given a vector of industry codes, these functions will return a matching
#' vector of the industry labels.
#' 
#' *industry2label* will not merge top level industries together (like manufacturing
#' or retail, which span 3 and 2 top level codes respectively). 
#' 
#' *industry2label.final* will merge the top level categories.
#' 
#' @param industry a vector of industry codes, 2 or 3 digit
#' @param inc.code boolean if the codes should be included in label
#' @aliases industry2label.final
#' @export
industry2label <- function(industry, inc.code=T) {
  data(industry2, envir = environment())
  industry2$shorter <- substring(industry2$short, 1, nchar(industry2$short)-5)
  data(industry3, envir = environment())
  industry3$shorter <- substring(industry3$short, 1, nchar(industry3$short)-6)
  ind <- rbind(industry2, industry3)
  if(inc.code) {
    mcol <- ind$short
  } else {
    mcol <- ind$shorter
  }
  out <- sapply(industry, function(x) {
    ifelse(x %in% ind$code, as.character(mcol[which(ind$code==x)]), "Unknown")
  })
  factor(out, levels=c(mcol, "Unknown"), ordered=T)
}

#' @export
industry2label.final <- function(industry) {
  data(industry2, envir = environment())
  ind <- industry2 %>%
    select(code, short) %>%
    mutate(short=ifelse(short=="Manufacturing (31)", "Manufacturing (31-33)", short),
           short=ifelse(short=="Manufacturing (32)", "Manufacturing (31-33)", short),
           short=ifelse(short=="Manufacturing (33)", "Manufacturing (31-33)", short),
           short=ifelse(short=="Retail (44)", "Retail (44-45)", short),
           short=ifelse(short=="Retail (45)", "Retail (44-45)", short),
           short=ifelse(short=="Transportation (48)", "Transportation (48-49)", short),
           short=ifelse(short=="Transportation (49)", "Transportation (48-49)", short))
  out <- sapply(industry, function(x) {
    ifelse(x %in% ind$code, as.character(ind$short[which(ind$code==x)]), "Unknown")
  })
  factor(out, levels=c(sort(unique(ind$short)), "Unknown"), ordered=T)
}

#' Specific view of victim industry and employee count
#' 
#' Given a verisr object, these functions will query for the top level (2-digit)
#' industry codes, convert to a string and correlate those with the employee count 
#' enumerations for the victims.
#' 
#' *getindemp* will get the information and plot it using the \code{\link{showtile}} function.
#' 
#' *getindemp.final* will not plot the information, but will return a data.frame 
#' of the data and it will compress the employee count down to Small, Large and Unknown
#' 
#' @export
#' @param veris a verisr object
#' @aliases getindemp.final
getindemp <- function(veris) {
  enumdf <- getenum(veris, "victim.industry2", "victim.employee_count", fillzero=T, add.freq = F, add.n=F)
  enumdf$enum <- industry2label(as.character(enumdf$enum))
  
  enumdf2 <- aggregate(x ~ enum + enum1, data=enumdf, FUN=sum)
  empcount <- c("1 to 10", "11 to 100", "101 to 1000", "Small", "Large", 
                "1001 to 10000", "10001 to 25000", "25001 to 50000", 
                "50001 to 100000", "Over 100000", "Unknown")
  enumdf$enum1 <- factor(enumdf$enum1, levels=rev(empcount), ordered=T)
  ylab <- paste0("Employees: ", round(sum(enumdf$x[enumdf$enum1!="Unknown"])/sum(enumdf$x), 3)*100, "% Known")
  xlab <- paste0("Industry: ", round(sum(enumdf$x[enumdf$enum!="Unknown"])/sum(enumdf$x), 3)*100, "% Known")
  showtile(enumdf, xlab=xlab, ylab=ylab, title="Victim Demographics")    
}

#' @import dplyr
#' @import tidyr
#' @export
getindemp.final <- function(veris) {
  enumdf <- getenum(veris, "victim.industry2", "victim.employee_count", fillzero=T, add.freq = F, add.n=F)
  enumdf$enum <- industry2label.final(as.character(enumdf$enum))
  
  empmap <- data.frame(src=c("1 to 10", "11 to 100", "101 to 1000", "Small", "Large", 
                             "1001 to 10000", "10001 to 25000", "25001 to 50000", 
                             "50001 to 100000", "Over 100000", "Unknown"),
                       dst=c("Small", "Small", "Small", "Small", "Large", 
                             "Large", "Large", "Large",
                             "Large", "Large", "Unknown"))
  enumdf$enum1 <- sapply(enumdf$enum1, function(x) empmap$dst[which(empmap$src==x)], USE.NAMES=F)
  enumdf2 <- aggregate(x ~ enum + enum1, data=enumdf, FUN=sum)
  
  tbl_df(enumdf2) %>% 
    spread(enum1, x) %>% 
    group_by(enum) %>% 
    mutate(Total=sum(Large, Small, Unknown)) %>%
    select(Industry=enum, Total, Small, Large, Unknown)
}

#' Produces a heat map from 2 enumerations
#' 
#' Given an enumeration data frame with "enum" and "enum1", it will return a ggplot
#' object of a heatmap comparing the two enumerations.
#' 
#' @param enumdf the data frame containing two enumerations
#' @param xlab the label for the x axis ("enum")
#' @param ylab the label for the y axis ("emum1")
#' @param title the title for the plot
#' @param heatlow the color value for the low end of values
#' @param heathigh the color value for the high end of values
#' @import ggplot2
#' @export
showtile <- function(enumdf, xlab, ylab, title="", heatlow="#FAFAFA", heathigh="#2166AC") {
  enum.levels <- c(levels(enumdf$enum), "Total")
  enum1.levels <- c("Total", levels(enumdf$enum1))
  enum.total <- aggregate(x ~ enum, data=enumdf, FUN=sum)
  enum.total$enum1 <- "Total"
  enum1.total <- aggregate(x ~ enum1, data=enumdf, FUN=sum)
  enum1.total$enum <- "Total"
  enumdf <- rbind(enumdf, enum.total, enum1.total)
  enumdf$enum1 <- factor(enumdf$enum1, levels=enum1.levels, ordered=T)
  enumdf$enum <- factor(enumdf$enum, levels=enum.levels, ordered=T)
  #special <- enumdf[(enumdf$enum=="Unknown" | enumdf$enum1=="Unknown" | enumdf$enum=="Total" | enumdf$enum1=="Total"), ]
  
  enumdf.fill <- enumdf[(enumdf$enum!="Unknown" & enumdf$enum1!="Unknown" & enumdf$enum!="Total" & enumdf$enum1!="Total"), ]
  enumdf.fill <- enumdf.fill[enumdf.fill$x>0, ]
  enumdf.text <- enumdf[enumdf$x>0, ]
  euniq <- unique(as.character(enumdf$enum))
  puniq <- unique(as.character(enumdf$enum1))
  off.x <- ifelse('Unknown' %in% euniq, 1, 0) + ifelse('Total' %in% euniq, 1, 0) + 0.5
  off.y <- ifelse('Unknown' %in% puniq, 1, 0) + ifelse('Total' %in% puniq, 1, 0) + 0.5
  gg <- ggplot(enumdf, aes(enum, enum1))  # , group=count)) +
  gg <- gg + geom_tile(fill="white", color="gray50")
  #gg <- gg + geom_tile(data=special, fill="white", color="red")
  gg <- gg + geom_tile(data=enumdf.fill, aes(fill=x), color="gray50")
  gg <- gg + geom_text(data=enumdf.text, aes(label=x), size=2)
  gg <- gg + geom_segment(x=length(euniq)+1-off.x, xend=length(euniq)+1-off.x, y=off.y, yend=length(puniq)+0.5)
  gg <- gg + geom_segment(y=off.y, yend=off.y, x=-0.5, xend=length(euniq)+1-off.x)
  gg <- gg + ggtitle(title)
  gg <- gg + xlab(xlab) + ylab(ylab)
  gg <- gg + scale_fill_gradient(low = heatlow, high = heathigh, guide=F)
  gg <- gg + scale_x_discrete(expand=c(0,0)) + scale_y_discrete(expand=c(0,0))
  gg <- gg + theme(panel.background = element_blank(),
                   panel.border = element_rect(color="black", size=1, fill=NA),
                   #axis.title = element_blank(),
                   axis.ticks = element_blank(),
                   axis.text = element_text(color="black", size=10),
                   axis.text.x = element_text(angle=55, hjust=1, vjust=1.05),
                   plot.title = element_text(color="black", size=16),
                   axis.title = element_text(color="black", size=12))
  gg  
}

#' work with timeline data from veris 
#' 
#' Munges and returns either data or a ggplot object (depending on the \code{justdata} argument)
#' from one or more categories for timeline measurements.
#' 
#' @param mat the verisr object
#' @param timelines vector of one or more of the following: c("compromise", "exfiltration", "discovery", "containment")
#' @param justdata boolean, if TRUE it will return a dataframe of the data, else a ggplot2 object
#' @export
#' @import ggplot2
gettimeline <- function(mat, timelines=c("compromise", "exfiltration", "discovery", "containment"), justdata=F) {
  timeline.unit=c( "Seconds", "Minutes", "Hours", "Days",
                   "Weeks", "Months", "Years", "Never")
  #timelines <- c("compromise", "exfiltration", "discovery", "containment")
  matname <- colnames(mat)
  timeline.names <- matname[grep('timeline.*.unit', matname)]
  temp.names <- unlist(sapply(timelines, function(x) {
    timeline.names[grep(x, timeline.names)]
  }, USE.NAMES=F))
  timeline.names <- temp.names
  # unknown and NA should be gone, but make sure
  timeline.names <- timeline.names[grep('NA|Unknown', timeline.names, invert=T)]
  tots <- as.matrix(colSums(mat[, timeline.names, with=F]))
  
  tsums <- sapply(timelines, function(enum) {
    colSums(mat[, timeline.names[grep(enum, timeline.names)], with=F])
  })
  if(all(tsums==0)) {
    blank <- textGrob(" ")
    gg <- textGrob(paste("No records with", "a timeline", "defined.", sep="\n"))
    plot <- arrangeGrob(blank, gg, blank, nrow=1, widths=c(0.5, 1, 0.5))
    return(plot)
  }
  out <- do.call(rbind, lapply(timeline.names, function(x) {
    ups <- unlist(strsplit(x, '[.]'))
    ct <- tots[x, ]
    data.frame(enum=ups[2], primary=ups[4], x=ct)
  })) %>% group_by(enum) %>% mutate(n=sum(x), freq=x/n)
  
  rownames(out) <- NULL
  
  # catch empty rows
  out <- out %>% filter(n>0)
  if (!nrow(out)>0) {
    blank <- textGrob(" ")
    gg <- textGrob(paste("No records with", "a timeline", "defined.", sep="\n"))
    plot <- arrangeGrob(blank, gg, blank, nrow=1, widths=c(0.5, 1, 0.5))
    return(plot)
  }
  lb <- paste0(out$enum, " n=", out$n)
  tfactor <- c(lb[grep(timelines[1], lb)][1],
               lb[grep(timelines[2], lb)][1],
               lb[grep(timelines[3], lb)][1],
               lb[grep(timelines[4], lb)][1])
  out$enum <- factor(lb, levels=tfactor, ordered=T)
  out$primary <- factor(out$primary, levels=timeline.unit, ordered=T)
  if (justdata) {
    gg <- out
  } else {
    gg <- ggplot(out, aes(primary, freq, label=paste0(round(freq,3)*100,"%")))
    gg <- gg + facet_grid(enum ~ .)
    gg <- gg + geom_bar(width=0.9, stat="identity", fill="steelblue")
    gg <- gg + geom_hline(data=data.frame(x=0), aes(x=x), colour="black", size=0.3)
    gg <- gg + geom_text(vjust=-0.2, color="black", size=3)
    gg <- gg + scale_y_continuous(expand = c(0, 0), limits=c(0, max(out$freq)*1.2))
    gg <- gg + scale_x_discrete(expand = c(0, 0)) + xlab("") + ylab("")
    gg <- gg + theme(strip.background = element_rect(fill=NA, color=NA),
                     axis.text.y = element_blank(),
                     axis.text.x = element_text(angle = -90, hjust=-0.1)) #, hjust = -1))
  }
  gg  
}

#' internal function, used in gettimeline.dbir
#' @import stringr
#' @param timelabel label to slice up
slicetime <- function(timelabel) {
  sapply(stringr::str_split(timelabel, "\\."), function(z) {
    z[4]
  })
}

#' Gets the data for a "detection deficit" plot
#' 
#' Given a verisr object, it will return a data.frame for the percentage of 
#' compromise and detection events within days or less and over time.
#' 
#' @param veris verisr object
#' @export
#' @import dplyr
#' @import tidyr
gettimeline.dbir <- function(veris) {
  compromise <- 
    tbl_df(as.data.frame(veris)) %>% select(year=timeline.incident.year, starts_with("timeline.compromise.unit")) %>% 
    select(-contains("NA"), -contains("Unknown")) %>% 
    filter(year>=2004) %>% 
    gather(slice, logic, -year) %>% 
    filter(logic==TRUE) %>% 
    count(year,slice) %>%
    mutate(slice=slicetime(slice)) %>%
    rename(x=n) %>%
    group_by(year) %>% mutate(n=sum(x)) %>%
    filter(slice %in% c("Days", "Hours", "Minutes", "Seconds")) %>%
    ungroup %>% group_by(year, n) %>% summarize(x=sum(x)) %>%
    mutate(pct=x/n) %>% mutate(category="compromise")
  discovery <- 
    tbl_df(as.data.frame(veris)) %>% select(year=timeline.incident.year, starts_with("timeline.discovery.unit")) %>% 
    select(-contains("NA"), -contains("Unknown")) %>% 
    filter(year>=2004) %>% 
    gather(slice, logic, -year) %>% 
    filter(logic==TRUE) %>% 
    count(year,slice) %>%
    mutate(slice=slicetime(slice)) %>%
    rename(x=n) %>%
    group_by(year) %>% mutate(n=sum(x)) %>%
    filter(slice %in% c("Days", "Hours", "Minutes", "Seconds")) %>%
    ungroup %>% group_by(year, n) %>% summarize(x=sum(x)) %>%
    mutate(pct=x/n) %>% mutate(category="discovery")
  tbl_df(rbind(compromise, discovery))  
}

#' Get the data for discovery methods from a verisr object
#' 
#' Given a verisr object, this will return the discovery methods and values 
#' for "All External" and "All Internal" summaries
#' 
#' @param veris a verisr object
#' @export
getall_discovery <- function(veris) {
  chunk <- setjenum(veris, c("discovery_method"))
  temp <- getenum(veris, "discovery_method")
  if (nrow(temp)==0) {
    return(NA)
  }
  temp$enum <- substr(temp$enum, 1, 3)
  pie <- aggregate(x ~ enum, data=temp, FUN=sum)
  pie <- pie[pie$enum %in% c("Int", "Ext"), ]
  pie$freq <- round(pie$x/sum(pie$x), 3)
  pie$n <- sum(pie$x)
  pie$enum[pie$enum=="Ext"] <- "All External"
  pie$enum[pie$enum=="Int"] <- "All Internal"
  gum <- rbind(chunk[[1]]$df, pie)
  gum$enum <- factor(gum$enum, levels=gum$enum, ordered=T)
  gum
}

#' Special for Wade Baker, get a pie chart of the internal/external discovery methods
#' 
#' Given a verisr object, this will return a ggplot object of a pie chart showing
#' the proportion of internal versus external discovery methods.
#' 
#' @param veris a verisr object 
#' @export
#' @import ggplot2
discovery_pie <- function(veris) {
  temp <- getenum(veris, "discovery_method")
  temp$enum <- substr(temp$enum, 1, 3)
  pie <- aggregate(x ~ enum, data=temp, FUN=sum)
  pie <- pie[pie$enum %in% c("Int", "Ext"), ]
  #slices <- table(substr(temp$enum, 1,3))[c("Int", "Ext")]
  #pie <- data.frame(label=names(slices), x=as.vector(slices))
  pie$label <- paste0(pie$enum, "\n", round(pie$x/sum(pie$x)*100), "%")
  pie$pct <- pie$x/sum(pie$x)
  pie$laby <- pie$pct/2
  pie$laby[2] <- pie$pct[1] + pie$laby[2]
  pie$labx <- 1.6
  gg <- ggplot(pie,aes(x=1, y = pct, label=label, fill = label))
  gg <- gg + geom_bar(width = 1, stat="identity")
  gg <- gg + scale_fill_manual(values=c("steelblue", "lightsteelblue"), guide=F)
  gg <- gg + coord_polar(theta = "y")
  gg <- gg + geom_text(aes(x=labx, y=laby))
  gg <- gg + ggtitle(paste0("Discovery Methods n=", sum(pie$x)))
  gg <- gg + theme(
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.margin=unit(c(0,0,0,0), "inches"))
  gg
}

#' get variety of data compromised
#' 
#' given a verisr object, this will return a ggplot2 object showing the 
#' amount of records lost for various data types over time. 
#' 
#' @param veris a verisr object
#' @import ggplot2
#' @import gridExtra
#' @import scales
#' @export
getdatavariety <- function(veris) {
  datatype <- getenum(veris, "attribute.confidentiality.data.variety")
  datatype <- as.character(datatype$enum)
  rcount <- rbindlist(lapply(datatype, function(x) {
    thistype <- paste0("attribute.confidentiality.data.amount.", x)
    amounts <- veris[[thistype]]
    amounts <- amounts[!is.na(amounts)]
    amounts <- amounts[amounts > 1]
    out <- NULL
    if (length(amounts) && amounts > 0) 
      out <- data.frame(
        type=paste0(x, " (", 
                    format(length(amounts), big.mark=",", scientific=F), ")"),
        amount=amounts)
    out
  }))
  if (nrow(rcount)) {
    lbrk <- 10^c(1,2,3,4,5,6,7,8)
    ggplot(rcount, aes(amount, color=type, fill=type)) + facet_wrap(~type, ncol=4, scales="free_y") +
      geom_histogram(aes(y=..density..), color="white", fill="black", alpha=1/2) + 
      geom_density(alpha=1/2) + 
      scale_x_log10("Number of Records", 
                    breaks = lbrk,
                    labels = trans_format("log10", math_format(10^.x))) +
      theme(legend.position="None")
  } else {
    grid.arrange(textGrob(paste("No records with", "data amount defined", sep="\n")))
  }
}

#' get data for the top N variety of actions across all action categories
#' 
#' Given a verisr object, this will return the \code{topn} most frequent action
#' varieties across all of the action categories.
#' 
#' @param veris a verisr object
#' @param topn how many entries to return 
#' @export
#' @import dplyr
actionvariety <- function(veris, topn=20) {
  vcol <- grep('action.*.variety', colnames(veris))
  v1 <- veris %>% select(vcol) 
  v2 <- select(v1, which(colSums(v1)>0))
  v3 <- filter(v2, rowSums(v2)>0)
  unk <- grep('Unknown', colnames(v3))
  v4 <- filter(v3, rowSums(v3) > rowSums(v3[ ,unk, with=FALSE]))
  numrow <- nrow(v4)
  v5 <- colSums(v4)
  base <- data.frame(enum=names(v5), x=as.vector(v5), stringsAsFactors=FALSE) %>% 
    tbl_df %>% 
    mutate(cat=substring(sapply(strsplit(enum, '[.]'), function(x) x[2]), 1, 3),
           enum=sapply(strsplit(enum, '[.]'), function(x) x[4]),
           enum=paste0(enum, " (", cat, ")"),
           n=nrow(v4),
           freq=x/n) %>%
    arrange(desc(x)) %>% 
    filter(row_number() <= topn) %>%
    select(-cat) %>%
    arrange(x)
  base$enum <- factor(base$enum, levels=base$enum, ordered=T) 
  base
}

#` internal function, used to average across categories of data for CCC calculation
enum.avg <- function(mat, fields) {
  present <- do.call(cbind, lapply(fields, FUN=function(x) {
    smat <- mat[ ,grep(x, colnames(mat)), with=F]
    if (is.data.table(smat)) {
      if(ncol(smat)>0) {
        smat <- do.call(cbind, lapply(colnames(smat), function(y) {
          if (is.character(smat[[y]])) {
            mout <- data.frame(y=!is.na(smat[[y]]) && length(smat[[y]]))
          } else if (is.numeric(smat[[y]])) {
            mout <- data.frame(y=!is.na(smat[[y]]) & smat[[y]] > 0)
          } else if (is.factor(smat[[y]])) {
            mout <- data.frame(y=!is.na(smat[[y]]))
          } else if (is.logical(smat[[y]])) {
            mout <- data.frame(y=smat[[y]])
          }
          colnames(mout) <- y
          mout
        }))
      } else {
        smat <- rep(F, nrow(mat))
      }
    }
    smat <- smat[ ,grep('Unknown', colnames(smat), ignore.case=T, invert=T)]
    if (! is.vector(smat)) {
      smat <- rowSums(smat)
    } else {
      smat <- smat * 1
    }
    outs <- data.frame(some=smat)
    colnames(outs) <- x
    outs
  }))
  #   if (! is.vector(present)) {
  #     present <- rowMeans(present)
  #   }
  present
}

#' get the CCC values for each entry in a verisr object
#' 
#' Given a verisr object, calculate the scores for the complexity, completeness and context.
#' 
#' *Completeness* looks at the actor, action, asset and attributes, the core components 
#' of the veris framework and produces a value between 0 and 1 for each of the categories.
#' Unknowns are treated as a zero and defined values are treated as a 1, the specific fields
#' in that categories are then averaged to produce the final value.
#' 
#' *Context* is computed the same way at completeness, but over the victim, timeline, 
#' discvoery method and targeted data point.  Again this produces a value between 0 and 1.
#' 
#' *Complexity* is a simple count of fields that are defined and entered.  The theory 
#' here being that lower scores may indicate that less overall data is known.
#'
#' @param veris a verisr object
#' @export 
getccc <- function(veris) {
  # actors <- mapply(fields, FUN=enum.avg, MoreArgs = list("mat"=veris))
  fields <- list(actor.external=c("actor.external.variety", "actor.external.motive"),
                 actor.internal=c("actor.internal.variety", "actor.internal.motive"),
                 actor.partner=c("actor.partner.motive", "actor.partner.industry2",
                                 "actor.partner.country"),
                 action.malware=c("action.malware.variety", "action.malware.vector"),
                 action.hacking=c("action.hacking.variety", "action.hacking.vector"),
                 action.social=c("action.social.variety", "action.social.vector", "action.social.target"),
                 action.misuse=c("action.misuse.variety", "action.misuse.vector"),
                 action.physical=c("action.physical.variety", "action.physical.vector"),
                 action.error=c("action.error.variety", "action.error.vector"),
                 action.environmental=c("action.environmental.variety", "action.environmental.variety"),
                 assets=c("asset.assets.variety"),
                 attribute.confidentiality=c("attribute.confidentiality.data_total", 
                                             "attribute.confidentiality.data.variety"),
                 attribute.integrity=c("attribute.integrity.variety"),
                 attribute.availability=c("attribute.availability.variety", 
                                          "attribute.availability.duration.unit"),
                 victim=c("victim.employee_count", "victim.industry",
                          "victim.country"),
                 timeline=c("timeline.compromise.unit",
                            "timeline.exfiltration.unit",
                            "timeline.containment.unit",
                            "timeline.discovery.unit"),
                 dicovery_method=c("discovery_method"),
                 targeted=c("targeted"))
  
  a4 <- mapply(fields, FUN=enum.avg, MoreArgs = list("mat"=veris))
  a4.sum <- do.call(cbind, lapply(names(a4), function(nm) {
    data.table(rowMeans(a4[[nm]]>0))
  }))
  names(a4.sum) <- names(a4)
  category <- unique(sapply(strsplit(names(a4.sum), '[.]'), function(x) x[1]))
  a4sum <- do.call(cbind, lapply(category, function(ct) {
    icol <- grep(paste0("^", ct), colnames(a4.sum))
    out <- data.frame(y=apply(a4.sum[ ,icol, with=F], 1, max))
    colnames(out) <- ct
    out
  }))
  a4sum$completeness <- rowMeans(a4sum[ ,1:4])
  a4sum$context <- rowMeans(a4sum[ ,5:8])
  a4sum$complexity <- rowSums(mapply(a4, FUN=rowSums))
  a4sum
}

