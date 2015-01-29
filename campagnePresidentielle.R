#///////////////////////////////////////////////////////////////////////////////
# projet CampagnePresidentielle
#///////////////////////////////////////////////////////////////////////////////
#
# L'objectif de ce script est de décrire et comparer les programmes des 
# candidats à l'élection présidentielle 2012.
#
# Données
# ///////
# Les programmes ont été scannés et analysés avec un logiciel d'OCR, puis
# vérifiés manuellement. Les scans et les fichiers textes obtenus se trouvent 
# dans le dossier "programmes".
# 
#
#///////////////////////////////////////////////////////////////////////////////

library(tm)
library(FactoMineR)

# Importation et traitement du corpus ##########################################

programmes <- Corpus(DirSource("programmes"))

# Mettre en minuscules
programmes <- tm_map(programmes, content_transformer(tolower))

# Remplacer les ponctuations par des espaces. tm fournit la fonction
# removePunctuation pour ça, mais elle gère mal les apostrophes.
programmes <- tm_map(programmes, 
                     content_transformer(function(x) gsub("\\W", " ", x)))

# Supprimer les mots courants
programmes <- tm_map(programmes, removeWords, words = stopwords("french"))

# Supprimer les terminaisons des mots (ex : e, es, ent...)
programmes <- tm_map(programmes, stemDocument, language = "french")

# Supprimer les nombres
programmes <- tm_map(programmes, removeNumbers)

# Supprimer les espaces superflus
programmes <- tm_map(programmes, stripWhitespace)

# Programmes premier et second tour
programmesT1 <- programmes[-c(4,11)]
programmesT2 <- programmes


# Analyse des programmes 1er tour ##############################################

dtm <- DocumentTermMatrix(programmesT1, control = list(weighting = weightTfIdf))
dimnames(dtm)[[1]] <- gsub(".txt", "", dimnames(dtm)[[1]])

# Supprimer les mots trop rares. Si on en garde trop, on obtient des résultats
# anecdotiques, mais si on n'en conserve pas assez, on a des résultats très 
# faibles.
dtm <- removeSparseTerms(dtm, 0.8)
dim(dtm)

# Clustering hiérarchique
d <- dist(dtm)
h <- hclust(d, "ward.D")

pdf("CAH T1.pdf", 6, 4.5)
par(mar=c(1,4,1,1))
plot(h, xlab = "", ylab = "distance", main = NA, sub = NA)
dev.off()

# Analyse en composante principale
pca <- PCA((as.matrix(dtm)), scale.unit = F, graph=F)

pdf("PCA T1.pdf", 6, 4.5)
plot(pca, title = "Analyse en composantes principales", axes=c(1,2))
dev.off()

# Graphique avec les mots ayant les plus fortes pondérations
nwords <- 50
coord <- pca$var$coord
d <- coord[,1]^2 + coord[,2]^2
idx <- which(d > quantile(d, 1 - nwords/nrow(coord))) # On veut environ 50 mots

pdf("PCA words T1.pdf", 6, 4.5)
plot(coord[idx, 1:2], type = "n", xaxt = "n", yaxt = "n", xlab = "Composante 1", ylab = "Composante 2")
text(coord[idx, 1:2], names(idx), cex=0.7)
abline(h = 0, lty = 2, col = gray(0.5))
abline(v = 0, lty = 2, col = gray(0.5))
dev.off()

# Ajout des programmes reçus pour le second tour ###############################

dtm2 <- DocumentTermMatrix(programmesT2, control = list(weighting = weightTfIdf))
dimnames(dtm2)[[1]] <- gsub(".txt", "", dimnames(dtm2)[[1]])

# On conserve les mêmes mots que dans la partie précédente, pour comparaison
dtm2 <- dtm2[, Terms(dtm)]
dim(dtm2)

# Clustering hiérarchique
d <- dist(dtm2)
h <- hclust(d, "ward.D")

pdf("CAH T2.pdf", 6, 4.5)
par(mar=c(1,4,1,1))
plot(h, xlab = "", ylab = "distance", main = NA, sub = NA)
dev.off()

# Analyse en composante principale
pca <- PCA((as.matrix(dtm2)), scale.unit = F, graph=F)

pdf("PCA T2.pdf", 6, 4.5)
plot(pca, title = "Analyse en composantes principales", axes=c(1,2))
dev.off()

# Graphique avec les mots ayant les plus fortes pondérations
nwords <- 50
coord <- pca$var$coord
d <- coord[,1]^2 + coord[,2]^2
idx <- which(d > quantile(d, 1 - nwords/nrow(coord))) # On veut environ 50 mots

pdf("PCA words T2.pdf", 6, 4.5)
plot(coord[idx, 1:2], type = "n", xaxt = "n", yaxt = "n", xlab = "Composante 1", ylab = "Composante 2")
text(coord[idx, 1:2], names(idx), cex=0.7)
abline(h = 0, lty = 2, col = gray(0.5))
abline(v = 0, lty = 2, col = gray(0.5))
dev.off()