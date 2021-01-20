rm(list = ls())
setwd("D:/xxiong/program/network/findicd/find_icd_latest")

library(data.table)
library(stringr)
library(lsa)
library(Matrix)
library(wordcloud)
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

# string_all_1 = sapply(mapping_s$possible_cui_1, function(x){
#   cui_i_list = str_split(x,",",simplify = TRUE)
#   a = sapply(cui_i_list,function(y){
#     cui_str_s$str[which(cui_str_s$cui==y)]
#   })
#   a = as.vector(unlist(a))
#   if(length(a)==0){
#     b = NA
#   }else{
#     b = a
#   }
#   return(b)
# })
#
# num_all_1 = sapply(mapping_s$possible_cui_1, function(x){
#   cui_i_list = str_split(x,",",simplify = TRUE)
#   a = sapply(cui_i_list,function(y){
#     cui_str_s$cui[which(cui_str_s$cui==y)]
#   })
#   a = as.vector(unlist(a))
#   if(length(a)==0){
#     b = NA
#   }else{
#     b = a
#   }
#   return(b)
# })
#
# string_all_2 = sapply(mapping_s$possible_cui_2, function(x){
#   cui_i_list = str_split(x,",",simplify = TRUE)
#   a = unlist(sapply(cui_i_list,function(y){
#     cui_str_s$str[which(cui_str_s$cui==y)]
#   }))
#   a = a[!duplicated(a)]
#   if(length(a)==0){
#     b = NA
#   }else{
#     b = a
#   }
#   return(b)
# })
#
# num_all_2 = sapply(mapping_s$possible_cui_2, function(x){
#   cui_i_list = str_split(x,",",simplify = TRUE)
#   a = sapply(cui_i_list,function(y){
#     cui_str_s$cui[which(cui_str_s$cui==y)]
#   })
#   a = as.vector(unlist(a))
#   if(length(a)==0){
#     b = NA
#   }else{
#     b = a
#   }
#   return(b)
# })
#
# mapping_s$possible_str_1= string_all_1
# mapping_s$possible_str_2 = string_all_2
# mapping_s$all_num_1= num_all_1
# mapping_s$all_num_2 = num_all_2
#
#
# proc.time()-t


# input_str = "cholera"
# string_all = string_all_1
# id_1 = sapply(string_all, function(x){
#   input_str %in% x
# },simplify = TRUE)
# selected_s = mapping_s[id_1,]
# if(nrow(selected_s)==0){
#   print("Please specify other input string!")
# }
#
# selected_cui_emb = c(selected_s$ICD_CUI,
#                      unlist(str_split(selected_s$UMLS_ICDSTR_CUI,",")))
# selected_cui_emb = selected_cui_emb[!duplicated(selected_cui_emb)]
# selected_emb_id = sapply(selected_cui_emb, function(x){
#    if(sum(emb$cui == x)==0){
#      return(NA)
#    }else{
#      return(which(emb$cui == x))
#    }
# })
# selected_emb_id = na.omit(selected_emb_id)
# if(length(selected_emb_id)==0){
#   print("No related cui based on current embeddings.")
# }

cosine_matrix <- cosine(t(as.matrix(emb[, -c(1, 2)])))
rownames(cosine_matrix) <- colnames(cosine_matrix) <- emb$cui

cosine_matrix_top100 <- t(apply(cosine_matrix, 2, function(x) {
  c(
    x[order(x, decreasing = TRUE)[1:100]],
    emb$terms[order(x, decreasing = TRUE)[1:100]]
  )
}))
cosine_matrix_top100 <- cbind(emb$terms, cosine_matrix_top100)
# pdf(file=paste0("wordcloud.pdf"),width=15,height=18)
#
# wordcloud(cosine_matrix_top100[1,c(101:200)],freq = round(abs(as.numeric(cosine_matrix_top100[1,c(1:100)])),3)*1000,
#           random.order = FALSE, colors = c(brewer.pal(9, "YlGnBu")),
#           min.freq = .2,max.words = 40,rot.per = 0,scale = c(4,.2))
# dev.off()

#
# selected_emb = emb$terms[selected_emb_id]
# selected_cosine = cosine_matrix_top100[selected_emb,]
# wordcloud(selected_cosine[101:200],freq = round(abs(as.numeric(selected_cosine[1:100])),3)*1000,
#           random.order = FALSE, colors = c(brewer.pal(9, "YlGnBu")),
#           min.freq = .2,max.words = 40,rot.per = 0,scale = c(4,.2))
#
#
#
# string_all = as.vector(unlist(mapping_s$possible_str_1))
# num_all = as.vector(unlist(mapping_s$all_num_1))
# string_all_1 = string_all[!duplicated(string_all)]
# num_all_1 = num_all[!duplicated(string_all)]
#
# string_all = as.vector(unlist(mapping_s$possible_str_2))
# num_all = as.vector(unlist(mapping_s$all_num_2))
# string_all_2 = string_all[!duplicated(string_all)]
# num_all_2 = num_all[!duplicated(string_all)]

# a = str_length(string_all_1)
# string_all_1[which.max(a)]

all_cui_1 <- unique(unlist(str_split(paste(mapping_s$possible_cui_1, collapse = ","), ",")))
all_cui_2 <- unique(unlist(str_split(paste(mapping_s$possible_cui_2, collapse = ","), ",")))

t <- proc.time()
cui_str_s_1 <- apply(cui_str_s, 1, function(x) {
  if ((x[1] %in% all_cui_1) | (x[1] %in% emb$cui)) {
    x
  } else {
    c(NA, NA)
  }
})

cui_str_s_1 <- as.data.frame(t(cui_str_s_1))
cui_str_s_1 <- na.omit(cui_str_s_1)
colnames(cui_str_s_1) <- c("cui", "str")
cui_str_s_1$loc.map <- apply(cui_str_s_1, 1, function(x) {
  paste(which(is.na(str_match(mapping_s$possible_cui_1, x[1])) != 1), collapse = ",")
})
cui_str_s_1$loc.emb <- apply(cui_str_s_1, 1, function(x) {
  paste(which(is.na(str_match(emb$cui, x[1])) != 1), collapse = ",")
})

proc.time() - t


t <- proc.time()
cui_str_s_2 <- apply(cui_str_s, 1, function(x) {
  if ((x[1] %in% all_cui_2) | (x[1] %in% emb$cui)) {
    x
  } else {
    c(NA, NA)
  }
})

cui_str_s_2 <- as.data.frame(t(cui_str_s_2))
cui_str_s_2 <- na.omit(cui_str_s_2)
colnames(cui_str_s_2) <- c("cui", "str")
cui_str_s_2$loc.map <- apply(cui_str_s_2, 1, function(x) {
  paste(which(is.na(str_match(mapping_s$possible_cui_2, x[1])) != 1), collapse = ",")
})
cui_str_s_2$loc.emb <- apply(cui_str_s_2, 1, function(x) {
  paste(which(is.na(str_match(emb$cui, x[1])) != 1), collapse = ",")
})
proc.time() - t


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
