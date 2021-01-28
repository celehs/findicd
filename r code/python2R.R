rm(list = ls())
setwd("D:/xxiong/program/network/findicd/find_icd_latest")

library(data.table)
library(stringr)
library(lsa)
library(Matrix)
library(dplyr)

mapping <- fread("files/UMLS_NER_full_mapping.csv")
cui_str <- fread("files/CUI_STR_CLEANED.csv")
emb <- fread("files/dodo_emb_new.csv")

mapping_s <- mapping
cui_str_s <- cui_str[1:8000]

t <- proc.time()
mapping_s$possible_cui_1 <- paste0(
  mapping_s$UMLS_ICDSTR_CUI, ",",
  mapping_s$UMLS_PHESTR_CUI
)
mapping_s$possible_cui_1 <- str_replace(mapping_s$possible_cui_1, ",$", "")
mapping_s$possible_cui_1 <- str_replace(mapping_s$possible_cui_1, "^,", "")
mapping_s$possible_cui_1 <- str_replace_all(mapping_s$possible_cui_1, "^,$", "")

mapping_s$possible_cui_1 <- sapply(mapping_s$possible_cui_1, function(x) {
  y <- str_split(x, ",", simplify = TRUE)
  y <- y[!duplicated(y)]
  y <- y[!duplicated(y)]
  y <- paste(y, collapse = ",")
  return(y)
})




mapping_s$possible_cui_2 <- paste0(
  mapping_s$UMLS_ICDSTR_CUI, ",",
  mapping_s$UMLS_PHESTR_CUI, ",",
  mapping_s$ICD_CUI, ",",
  mapping_s$PHECODE_CUI
)
mapping_s$possible_cui_2 <- str_replace(mapping_s$possible_cui_2, ",$", "")
mapping_s$possible_cui_2 <- str_replace(mapping_s$possible_cui_2, "^,", "")
mapping_s$possible_cui_2 <- str_replace_all(mapping_s$possible_cui_2, ",,", ",")
mapping_s$possible_cui_2 <- str_replace_all(mapping_s$possible_cui_2, "^,,,$", "")

mapping_s$possible_cui_2 <- sapply(mapping_s$possible_cui_2, function(x) {
  y <- str_split(x, ",", simplify = TRUE)
  y <- y[!duplicated(y)]
  y <- y[!duplicated(y)]
  y <- paste(y, collapse = ",")
  return(y)
})
proc.time() - t


cosine_matrix <- lsa::cosine(t(as.matrix(emb[, -c(1, 2)])))
rownames(cosine_matrix) <- colnames(cosine_matrix) <- emb$cui

cosine_matrix_top100 <- t(apply(cosine_matrix, 2, function(x) {
  c(
    x[order(x, decreasing = TRUE)[1:100]],
    emb$terms[order(x, decreasing = TRUE)[1:100]]
  )
}))
cosine_matrix_top100 <- cbind(emb$terms, cosine_matrix_top100)


all_cui_1 <- unique(unlist(str_split(paste(mapping_s$possible_cui_1, collapse = ","), ",")))
all_cui_2 <- unique(unlist(str_split(paste(mapping_s$possible_cui_2, collapse = ","), ",")))

all_cui_1_matrix = full_join(data.frame(cui=all_cui_1),data.frame(cui=emb$cui),by="cui")
all_cui_2_matrix = full_join(data.frame(cui=all_cui_2),data.frame(cui=emb$cui),by="cui")

cui_str_s_1 = inner_join(rbind(all_cui_1_matrix,emb$cui),cui_str_s,by="cui")
cui_str_s_2 = inner_join(rbind(all_cui_2_matrix,emb$cui),cui_str_s,by="cui")


cui_str_s_1$loc.map <- apply(cui_str_s_1, 1, function(x) {
  paste(which(is.na(str_match(mapping_s$possible_cui_1, x[1])) != 1), collapse = ",")
})
cui_str_s_1$loc.emb <- apply(cui_str_s_1, 1, function(x) {
  paste(which(is.na(str_match(emb$cui, x[1])) != 1), collapse = ",")
})


cui_str_s_2$loc.map <- apply(cui_str_s_2, 1, function(x) {
  paste(which(is.na(str_match(mapping_s$possible_cui_2, x[1])) != 1), collapse = ",")
})
cui_str_s_2$loc.emb <- apply(cui_str_s_2, 1, function(x) {
  paste(which(is.na(str_match(emb$cui, x[1])) != 1), collapse = ",")
})


cui_uni_1 <- unique(cui_str_s_1$cui)
cui_uni_2 <- unique(cui_str_s_2$cui)
cui_dict_1 <- sapply(cui_uni_1, function(x) {
  cui_str_s_1$str[which(cui_str_s_1$cui == x)]
})
cui_dict_2 <- sapply(cui_uni_2, function(x) {
  cui_str_s_2$str[which(cui_str_s_2$cui == x)]
})


cui_str_s_1$loc.dict <- apply(cui_str_s_1, 1, function(x) {
  which(x[1] == names(cui_dict_1))
})
cui_str_s_2$loc.dict <- apply(cui_str_s_2, 1, function(x) {
  which(x[1] == names(cui_dict_2))
})

a <- ls()


rm(list = a[which(a != "cosine_matrix_top100" & a != "mapping_s" &
  a != "cui_str_s_1" & a != "emb" & a != "cui_str_s_2" &
  a != "cui_dict_1" & a != "cui_dict_2")])
rm(a)


save.image("D:/xxiong/program/network/findicd/find_icd_latest/r code/shiny/data/raw.RData")
