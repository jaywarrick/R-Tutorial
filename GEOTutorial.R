https://www.ncbi.nlm.nih.gov/geo/browse/


# Version info: R 3.2.3, Biobase 2.30.0, GEOquery 2.40.0, limma 3.26.8
# R scripts generated  Fri Feb 10 14:51:34 EST 2017

## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("Biobase")
biocLite("GEOquery")

##### THIS SECTION IS COPIED FROM THE WEBSITE #####

#Server: www.ncbi.nlm.nih.gov
#Query: acc=GSE86796&platform=GPL16570&type=txt&groups=&colors=&selection=XXXXXXXX&padj=fdr&logtransform=auto&columns=ID&columns=adj.P.Val&columns=P.Value&columns=F&columns=SPOT_ID&num=250&annot=submitter

# Unable to generate script analyzing differential expression.
#      Invalid input: at least two groups of samples should be selected.

################################################################
#   Boxplot for selected GEO samples
library(Biobase)
library(GEOquery)

# load series and platform data from GEO

gset <- getGEO("GSE86796", GSEMatrix =TRUE, getGPL=FALSE)
if (length(gset) > 1) idx <- grep("GPL16570", attr(gset, "names")) else idx <- 1
gset <- gset[[idx]]

# set parameters and draw the plot

dev.new(width=4+dim(gset)[[2]]/5, height=6)
par(mar=c(2+round(max(nchar(sampleNames(gset)))/2),4,2,1))
title <- paste ("GSE86796", '/', annotation(gset), " selected samples", sep ='')
boxplot(exprs(gset), boxwex=0.7, notch=T, main=title, outline=FALSE, las=2)


# Now do something with the information
expressionLevels <- as.data.table(exprs(gset), keep.rownames = T) # get expression levels
setnames(expressionLevels, 'rn', 'PROBEID') # Change head of probe id from 'rn' to 'PROBEID'
expressionLevels
protocolData <- pData(gset) # get information on the different samples
View(protocolData)

# Convert probe ids to gene symbols and names
# First, get the probe set information (i.e., what probe number is associated with this genes) for the particular assay (e.g., affymetrix mogene 2.0)
# by going to https://www.bioconductor.org/packages/release/data/annotation/, searching and downloading the appropriate .db file
# Run the following (inserting your .db file for the one used here)
source("https://bioconductor.org/biocLite.R")
biocLite("annotate") # installs this package
biocLite("mogene20stprobeset.db") # installs this package
library("annotate") # loads this package
library(mogene20stprobeset.db) # loads this package

columns(mogene20stprobeset.db) # shows the columns of information that can be accessed in mogene20stprobeset.db
# [1] "ACCNUM"       "ALIAS"        "ENSEMBL"      "ENSEMBLPROT"  "ENSEMBLTRANS" "ENTREZID"     "ENZYME"      
# [8] "EVIDENCE"     "EVIDENCEALL"  "GENENAME"     "GO"           "GOALL"        "IPI"          "MGI"         
# [15] "ONTOLOGY"     "ONTOLOGYALL"  "PATH"         "PFAM"         "PMID"         "PROBEID"      "PROSITE"     
# [22] "REFSEQ"       "SYMBOL"       "UNIGENE"      "UNIPROT"     

keys(mogene20stprobeset.db) # Shows all the PROBEIDs in the mogene20stprobeset.db
lookupTable <- data.table(select(mogene20stprobeset.db, keys(mogene20stprobeset.db), c('SYMBOL','GENENAME')))
print(unique(lookupTable$SYMBOL)) # Lots of probes don't have info associated with them. Just showing you some that show up in the table

# Add the gene symbol and name to the expression table.
duh <- merge(lookupTable, expressionLevels, by='PROBEID')
geneRows <- which(!is.na(duh$SYMBOL))
View(duh[geneRows])



