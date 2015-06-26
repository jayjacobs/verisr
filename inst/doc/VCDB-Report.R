## ----echo=FALSE, warning=FALSE-------------------------------------------
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(binom))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(verisr))
suppressPackageStartupMessages(library(pbapply))
suppressPackageStartupMessages(library(rjson))
opts_chunk$set(echo=TRUE, warning=FALSE, message=FALSE,
               results="markdown", prompt=FALSE, error=FALSE,
               #fig.width=8, fig.height=5, 
               cache=FALSE)

theme_set(theme_minimal() + 
            theme(panel.background=element_rect(fill="floralwhite", color="gray75"),
                  panel.grid.major=element_line(color="gray75", size=0.1),
                  axis.ticks=element_blank(),
                  title = element_text(face="italic", size=10)
          ))


## ------------------------------------------------------------------------
vz <- json2veris("~/Documents/github/VCDB/data/json/")

## ----eval=FALSE----------------------------------------------------------
#  vz <- vz %>% filter(attribute.confidentiality.data.variety.Credentials)

## ------------------------------------------------------------------------
fullrows <- format(nrow(vz), big.mark=",", scientific=F)
vrows <- format(nrow(vz), big.mark=",", scientific=F)
ddyes <- format(sum(vz$attribute.confidentiality.data_disclosure.Yes), big.mark=",", scientific=F)
ddpot <- format(sum(vz$attribute.confidentiality.data_disclosure.Potentially), big.mark=",", scientific=F)

## ----fig.height=3, fig.width=10------------------------------------------
ccc <- getccc(vz)
simpled <- tbl_df(ccc[ ,1:8]) %>% gather(type, score) 
avgs <- simpled %>% group_by(type) %>% summarize(score=mean(score)) %>%
  mutate(label=paste0(round(score, 3)*100, "%"), clabel="Some")
cuts <- simpled %>% 
  mutate(clabel=cut(score, breaks=c(-0.1,0.4,0.99, 1), 
                    labels=c("Poor", "Some", "Perfect"))) %>%
  group_by(type, clabel) %>% tally %>% 
  group_by(type) %>% mutate(sums=sum(n), freq=n/sums)
  
cuts$clabel <- factor(cuts$clabel, levels=rev(c("Poor", "Some", "Perfect")), ordered=T)
ggplot(cuts, aes(clabel, freq, fill=clabel)) + facet_wrap(~type, nrow=1) + 
  geom_bar(stat="identity")  + 
  scale_y_continuous("Percentage of Incidents", breaks=seq(0,1,by=0.1),
                     label=percent, expand=c(0,0), limits=c(0,1)) +
  scale_fill_manual(values = rev(c("#fbb4ae", "#b3cde3", "#ccebc5"))) +
  geom_text(data=avgs, aes(clabel, label=label), y=0.92, size=4) +
  xlab("") + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.3),
                   legend.position="None")

## ----fig.height=1.5, fig.width=5-----------------------------------------
# this will calculate 1.5 * IQR to filter off "outliers" to calculate a
# trimmed standard deviation
thec3 <- tbl_df(ccc) %>% 
  select(completeness, context, complexity)  %>% 
  mutate(iqr=1.5*IQR(complexity), 
         lwr=quantile(complexity, probs=c(.25))-iqr, 
         upr=quantile(complexity, probs=c(.75))+iqr) %>%
  filter(complexity >= lwr, complexity <= upr) %>%
  summarize(mean=round(mean(complexity), 2), 
            sd=round(sd(complexity), 2)) %>%
  mutate(Score=round((sqrt(mean*sd))/12, 2)) %>%
  select(Score, mean, sd) %>%
  gather(wh, score) %>% 
  mutate(x=1, y=1)

  ggplot(thec3, aes(x, y, label=score)) + geom_text(size=8) + 
  facet_wrap(~wh) + theme(axis.text=element_blank(),
                          axis.title=element_blank(),
                          panel.margin=unit(c(10), "points"))

## ----warning=F-----------------------------------------------------------

chunk <- setjenum(vz, c("actor", "action", "asset.variety")) # , ht_mult=0.22, ht_pad=2)

## ----fig.width=8.5, fig.height=chunk$ptall-------------------------------
print(plotjchunk(chunk))

## ------------------------------------------------------------------------
chunk <- setjenum(vz, c("pattern"))

## ----fig.width=8.5, fig.height=chunk$ptall-------------------------------
print(plotjchunk(chunk))

## ----fig.width=8.5, fig.height=chunk$ptall-------------------------------
print(plotjchunk(chunk, final = TRUE))

## ---- fig.width=8, fig.height=6------------------------------------------
plota4(vz, "A4 Grid")

## ----fig.width=8.5, fig.height=4-----------------------------------------
print(getindemp(vz))

## ----warning=F-----------------------------------------------------------
chunk <- setjenum(vz, c("victim.country"))

## ----fig.width=8.5, fig.height=chunk$ptall-------------------------------
print(plotjchunk(chunk))

## ------------------------------------------------------------------------
ncount <- format(nrow(vz), big.mark=",", scientific=F)
secondary <- paste0("Out of the ", ncount, 
                    " incidents, there were no secondary victims recorded.")
scount <- sum(!is.na(vz$victim.secondary.amount))

if (scount>0) {
  secondary <- paste0("Out of the ", ncount, 
                    " incidents, there were *", 
                      format(scount, big.mark=",", scientific=F), 
                      " incidents* with secondary victims.")
}

## ----warning=F-----------------------------------------------------------
chunk <- setjenum(vz, c("actor.external.variety", "actor.external.motive"))

## ----fig.width=8.5, fig.height=chunk$ptall-------------------------------
print(plotjchunk(chunk))

## ----warning=F-----------------------------------------------------------
chunk <- setjenum(vz, c("actor.external.country"))

## ----fig.width=8.5, fig.height=chunk$ptall-------------------------------
print(plotjchunk(chunk))

## ----warning=F-----------------------------------------------------------
chunk <- setjenum(vz, c("actor.internal.variety", "actor.internal.motive"))

## ----fig.width=8.5, fig.height=chunk$ptall-------------------------------
print(plotjchunk(chunk))

## ----warning=F-----------------------------------------------------------
chunk <- setjenum(vz, c("actor.partner.motive", "actor.partner.country"))

## ----fig.width=8.5, fig.height=chunk$ptall-------------------------------
print(plotjchunk(chunk))

## ----warning=F-----------------------------------------------------------
chunk <- setjenum(vz, c("action.malware.variety", "action.malware.vector"))

## ----fig.width=8.5, fig.height=chunk$ptall-------------------------------
print(plotjchunk(chunk))

## ----warning=F-----------------------------------------------------------
chunk <- setjenum(vz, c("action.hacking.variety", "action.hacking.vector"))

## ----fig.width=8.5, fig.height=chunk$ptall-------------------------------
print(plotjchunk(chunk))

## ----warning=F-----------------------------------------------------------
chunk <- setjenum(vz, c("action.social.variety", "action.social.vector"))

## ----fig.width=8.5, fig.height=chunk$ptall-------------------------------
print(plotjchunk(chunk))

## ----warning=F-----------------------------------------------------------
chunk <- setjenum(vz, c("action.misuse.variety", "action.misuse.vector"))

## ----fig.width=8.5, fig.height=chunk$ptall-------------------------------
print(plotjchunk(chunk))

## ----warning=F-----------------------------------------------------------
chunk <- setjenum(vz, c("action.physical.variety", "action.physical.vector"))

## ----fig.width=8.5, fig.height=chunk$ptall-------------------------------
print(plotjchunk(chunk))

## ----warning=F-----------------------------------------------------------
chunk <- setjenum(vz, c("action.error.variety", "action.error.vector"))

## ----fig.width=8.5, fig.height=chunk$ptall-------------------------------
print(plotjchunk(chunk))

## ----warning=F-----------------------------------------------------------
#chunk <- setjenum(vz, c("action.environmental.variety"))

## ----fig.width=8.5, fig.height=chunk$ptall-------------------------------
#print(plotjchunk(chunk))

## ----warning=F-----------------------------------------------------------
chunk <- setjenum(vz, c("asset.assets.variety"))

## ----fig.width=8.5, fig.height=chunk$ptall-------------------------------
print(plotjchunk(chunk))

## ----warning=F-----------------------------------------------------------
try(chunk <- setjenum(vz, c("asset.governance", "asset.country", "asset.cloud")))

## ----fig.width=8.5, fig.height=chunk$ptall-------------------------------
try(print(plotjchunk(chunk)))

## ----echo=F--------------------------------------------------------------
att <- getenum(vz, 'attribute')
confidentiality <- format(att$x[att$enum=='Confidentiality'], big.mark=",",scientific=F)
integrity <- format(att$x[att$enum=='Integrity'], big.mark=",",scientific=F)
availability <- format(att$x[att$enum=='Availability'], big.mark=",",scientific=F)

## ----warning=F-----------------------------------------------------------
chunk <- setjenum(vz, c("attribute.confidentiality.data_disclosure", "attribute.confidentiality.data.variety"))

## ----fig.width=8.5, fig.height=chunk$ptall-------------------------------
print(plotjchunk(chunk))

## ----warning=F-----------------------------------------------------------
chunk <- setjenum(vz, c("attribute.confidentiality.state", "attribute.confidentiality.data_victim"))

## ----fig.width=8.5, fig.height=chunk$ptall-------------------------------
print(plotjchunk(chunk))

## ------------------------------------------------------------------------
getdatavariety(vz)

## ----warning=F-----------------------------------------------------------
chunk <- setjenum(vz, c("attribute.integrity.variety"))

## ----fig.width=8.5, fig.height=chunk$ptall-------------------------------
print(plotjchunk(chunk))

## ----warning=F-----------------------------------------------------------
chunk <- setjenum(vz, c("attribute.availability.variety"))

## ----fig.width=8.5, fig.height=chunk$ptall-------------------------------
print(plotjchunk(chunk))

## ----fig.width=8.5, fig.height=7-----------------------------------------
gettimeline(vz)


## ----fig.width=8.5, fig.height=4-----------------------------------------
# gad <- function(veris) {
#   chunk <- setjenum(veris, c("discovery_method"))
#   temp <- getenum(veris, "discovery_method")
#   if (nrow(temp)==0) {
#     return(NA)
#   }
#   temp$enum <- substr(temp$enum, 1, 3)
#   pie <- aggregate(x ~ enum, data=temp, FUN=sum)
#   pie <- pie[pie$enum %in% c("Int", "Ext"), ]
#   pie$freq <- round(pie$x/sum(pie$x), 3)
#   pie$n <- sum(pie$x)
#   pie$enum[pie$enum=="Ext"] <- "All External"
#   pie$enum[pie$enum=="Int"] <- "All Internal"
#   gum <- data.table::rbindlist(list(chunk[[1]]$df, pie), fill=TRUE)
#   # gum <- rbind(chunk[[1]]$df, pie)
#   gum$enum <- factor(gum$enum, levels=gum$enum, ordered=T)
#   gum
# }
gum <- getall_discovery(vz)
# gum <- gad(vz)
if (all(is.na(gum))) {
  blank <- textGrob(" ")
  gg <- textGrob(paste("No records with", "discovery_method", "defined.", sep="\n"))
  plot <- arrangeGrob(blank, gg, blank, nrow=1, widths=c(0.5, 1, 0.5))
  print(plot)
} else {
  gg <- simplejbar(gum, paste0("Discovery methods, n=", max(gum$n)), plot.margin=unit(c(0,0,0.1,0), "inches"))
  two <- discovery_pie(vz)
  print(arrangeGrob(gg, two, nrow=1))
#   print(arrangeGrob(gg, nrow=1))
}

## ----warning=F-----------------------------------------------------------
chunk <- setjenum(vz, c("targeted"), unknowns = c("NA", "Unknown", "Not Applicable"))

## ----fig.width=8.5, fig.height=chunk$ptall-------------------------------
print(plotjchunk(chunk))

