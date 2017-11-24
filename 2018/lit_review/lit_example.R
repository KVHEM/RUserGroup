#devtools::install_github("massimoaria/bibliometrix")
library(bibliometrix)

D <- readFiles("./2018/lit_review/savedrecs.bib")
M <- convert2df(D, dbsource = "isi", format = "bibtex")

results <- biblioAnalysis(M, sep = ";")
S=summary(object = results, k = 30, pause = T)
S$MostCitedPapers

CR <- citations(M, field = "author", sep = ".  ")
CR$Cited[1:20]

DF <- dominance(results, k = 20)
DF

authors=gsub(","," ", names(results$Authors)[1:30])
indices <- Hindex(M, authors, sep = ";",years=50)
indices$H

A <- cocMatrix(M, Field = "SO", sep = ";")
sort(Matrix::colSums(A), decreasing = TRUE)[1:5]

#Bibliographic coupling analyses the citing documents:
#Current research front and connection between groups
NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "authors", sep = ";")
S <- normalizeSimilarity(NetMatrix, type="jaccard")
net <- networkPlot(S, n = 50, Title = "Authors' Coupling", type = "kamada", labelsize = 0.6, size=FALSE,remove.multiple=TRUE)

#Co-citation analyses the cited documents: 
#Mapping older papers and detect schools of thought/shift in abstractsdigms
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ".  ")
net=networkPlot(NetMatrix, n = 30, halo=F,
                Title = "Co-Citation Network", type = "kamada", 
                size=T, labelsize = 0.8, remove.multiple=T)

#Scientific colalboration/co-authorship
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "authors", sep = ";")
S <- normalizeSimilarity(NetMatrix, type="jaccard")
net <- networkPlot(S, n = 130, Title = "Authors' Collaboration", 
                   type = "fruchterman", labelsize = 0.8, 
                   size=FALSE,remove.multiple=TRUE)

#Keyword co-occurence
histResults <- histNetwork(M, n = 120, sep = ".  ")
net <- histPlot(histResults, size = FALSE)

#Find specific keywords in abstract
abstracts <- M$AB
abstracts <- gsub("   ", " ", abstracts)
names(abstracts) <- paste(gsub("([A-Za-z]+).*", "\\1", M$AU), M$PY, "|", M$JI,"|", M$TC)

toMatch <- c("UNCERTAINTY")
matched_sentences = tolower(unlist(strsplit(abstracts, split = "\\."))[grep(paste(toMatch, collapse="|"), unlist(strsplit(abstracts, split = "\\.")))])
matched_sentences = paste0(matched_sentences, " [", substr(names(matched_sentences), 1, nchar(names(matched_sentences))-1), "]") 

write(matched_sentences, file = "sample_abs.txt")
