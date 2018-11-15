library(biocViews)
library(igraph)
library(XML)
library(stringr)
library(jsonlite)
library(ontologyIndex)
# get_OWL got depreciated, therefore load function from version 2.2
# library(ontoCAT)

setwd("/home/veit/devel/Bioinformatics/ELIXIR_EDAM/biotoolsExtract/")
source("ontologyIndex_get_OWL.R")

# ############### get recent EDAM ontology for mapping of terms
# system("rm EDAM.owl")
# system("wget http://edamontology.org/EDAM.owl")
# EDAM <- get_OWL("EDAM.owl")

# 
# ## Remove obsolete terms
# FullEDAM <- EDAM
# EDAM$id <- EDAM$id[!EDAM$obsolete]
# EDAM$name <- EDAM$name[!EDAM$obsolete]
# EDAM$parents <- EDAM$parents[!EDAM$obsolete]
# EDAM$children <- EDAM$children[!EDAM$obsolete]
# EDAM$ancestors <- EDAM$ancestors[!EDAM$obsolete]
# EDAM$obsolete <- EDAM$obsolete[!EDAM$obsolete]
# 

############### ELIXIR REGISTRY -> MSUTILS.ORG 

FullReg <- list()
for (i in 1:1300) {

  # request <- paste("wget 'https://bio.tools/api/tool?page=",i, "&format=xml' -O FullElixir.xml", sep="")
  # url <- system(request)
  request <- paste("wget 'https://bio.tools/api/tool?page=",i, "&format=json' -O FullElixir.json", sep="")
  url <- system(request)
  FullReg[[i]] <- fromJSON("FullElixir.json")$list
  
#   FullRegistry <- xmlTreeParse("FullElixir.xml")
# FullReg[[i]] <- xmlToList(FullRegistry)$list

}
save(FullReg,file="FullRegJSON.RData")
load("FullRegJSON.RData")

AllTools <- NULL
for (i in 1:length(FullReg)) {
  # AllTools <- c(AllTools, FullReg[[i]])
  AllTools <- rbind(AllTools, FullReg[[i]][,c("collectionID","accessibility","topic","owner",
                                              "download","validated","homepage_status","maturity",
                                              "canonicalID","availability","version","homepage",
                                              "function","lastUpdate","description")])
}

## Functions!
output <- lapply(AllTools$'function',function(x) lapply(x$output,function(y) 
  c(grep("format_",unlist(y),value=T),grep("data_",unlist(y),value=T))))
input <- lapply(AllTools$'function',function(x) lapply(x$input,function(y) 
  c(grep("format_",unlist(y),value=T),grep("data_",unlist(y),value=T))))


fdmaps <- NULL
for (i in 1:length(input))  {
  for (j in input[[i]]) {
    tformats <- grep("format_",j,value=T)
    tdata <- grep("data_",j,value=T)
    # restricting to case of only one data to avoid wrong matches
    if (length(tformats)>0 & length(tdata)==1)
      for (f in tformats)
      fdmaps <- rbind(fdmaps, c(f,tdata))
  }
}
for (i in 1:length(output))  {
  for (j in output[[i]]) {
    tformats <- grep("format_",j,value=T)
    tdata <- grep("data_",j,value=T)
    # restricting to case of only one data to avoid wrong matches
    if (length(tformats)>0 & length(tdata)==1)
      for (f in tformats)
        fdmaps <- rbind(fdmaps, c(f,tdata))
  }
}
fdmaps <- unique(fdmaps)
length(unique(fdmaps[,1]))
length(unique(fdmaps[,2]))


## extracting relationship from EDAM
EDAM <- read.csv("http://edamontology.org/EDAM.csv")
names(EDAM)
FullEDAM <- EDAM
EDAM <- EDAM[!EDAM$Obsolete,]
EDAMformats <- EDAM[grep("format_",EDAM$Class.ID),]
EDAMformats$Parents

# generic file formats to be removed
fdmaps3 <- NULL
EDAMformats$Parents <- sub("http://edamontology.org/format_2333","",EDAMformats$Parents)
EDAMformats$Parents <- sub("http://edamontology.org/format_2331","",EDAMformats$Parents)
EDAMformats$Parents <- sub("http://edamontology.org/format_3464","",EDAMformats$Parents)
EDAMformats$Parents <- sub("http://edamontology.org/format_2376","",EDAMformats$Parents)
EDAMformats$Parents <- sub("http://edamontology.org/format_2330","",EDAMformats$Parents)
EDAMformats$Parents <- sub("http://edamontology.org/format_2332","",EDAMformats$Parents)
EDAMformats$Parents <- sub("http://edamontology.org/format_3750","",EDAMformats$Parents)
fparents <- sapply(EDAMformats$Parents, function(x) str_extract_all(x,"format_[0-9]*"))
for (i in 1:nrow(EDAMformats)) {
  ttt <- unlist(fparents[i])
  for (j in 1:length(ttt)) {
    fdmaps3 <- rbind(fdmaps3, c(as.character(EDAMformats$Class.ID[i]),
                                paste("http://edamontology.org/",ttt[j],sep="")))
  }
}


## get is_format_of 
fdmaps2 <- NULL
system('grep \'<owl:someValuesFrom rdf:resource="http://edamontology.org/data_\\|<owl:onProperty rdf:resource="http://edamontology.org/is_format_of"/>\\|<!-- http://edamontology.org/format_\' /tmp/EDAM_1.21.owl > t')
t_is_format_of <- readLines("t")
for (i in 1:(length(t_is_format_of)-2)) {
  if (grepl('<!-- http://edamontology.org/format_',t_is_format_of[i])) {
    if (grepl('<owl:onProperty rdf:resource=', t_is_format_of[i+1])) {
      if (grepl('<owl:someValuesFrom rdf:resource=',t_is_format_of[i+2])) {
        
        j <- 1
        while(grepl('<owl:someValuesFrom rdf:resource=',t_is_format_of[i+j+1]) & 
              grepl('<owl:onProperty rdf:resource=', t_is_format_of[i+j])) {
          fdmaps2 <- rbind(fdmaps2, c(
          paste("http://edamontology.org/",str_extract(t_is_format_of[i], "format_[0-9]*"),sep=""),
          paste("http://edamontology.org/",
                str_extract(t_is_format_of[i+j+1], "data_[0-9]*"),sep="")))
          j <- j+2
        }
      }
    }
  }
}

rownames(EDAM) <- EDAM$Class.ID

EDAM[fdmaps[,1],]$Preferred.Label

## put everything into table and think about how to validate
fdmaps <- cbind(fdmaps[,1], as.character(EDAM[fdmaps[,1], "Preferred.Label"]), 
                fdmaps[,2], as.character(EDAM[fdmaps[,2], "Preferred.Label"]))
fdmaps2 <- cbind(fdmaps2[,1], as.character(EDAM[fdmaps2[,1], "Preferred.Label"]), 
                 fdmaps2[,2], as.character(EDAM[fdmaps2[,2], "Preferred.Label"]))
fdmaps3 <- cbind(fdmaps3[,1], as.character(EDAM[fdmaps3[,1], "Preferred.Label"]), 
                 fdmaps3[,2], as.character(EDAM[fdmaps3[,2], "Preferred.Label"]))

fullFormats <- NULL
for (f in EDAMformats$Class.ID) {
  matches <- fdmaps[,1] == f
  entries <- rep(NA,6)
  if (sum(matches)> 0) 
    entries[1:2] <- c(paste(fdmaps[matches,3],collapse=""), paste(fdmaps[matches,4],collapse=""))
  matches <- fdmaps2[,1] == f
  if (sum(matches)> 0) 
    entries[3:4] <- c(paste(fdmaps2[matches,3],collapse=""), paste(fdmaps2[matches,4],collapse=""))
  matches <- fdmaps3[,1] == f
  if (sum(matches)> 0) 
    entries[5:6] <- c(paste(fdmaps3[matches,3],collapse=""), paste(fdmaps3[matches,4],collapse=""))
  fullFormats <- rbind(fullFormats, as.vector(c(f, as.character(EDAM[f,"Preferred.Label"]), entries )))
}
colnames(fullFormats) <- c("Format", "Format label", "from bio.tools","from bio.tools label", 
                           "is_format_of", "is_format_of label", "parent", "parent label")
write.csv2(fullFormats, "FormatDataMappings.csv")


