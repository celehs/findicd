rm(list = ls())
setwd("~/program/findicd")
#setwd("D:/xxiong/program/network/findicd/find_icd_latest")

x = c("data.table","stringr","dplyr","lsa","Matrix","snowfall","parallel")
for (i in 1:length(x)) {
  if (!require(x[i],character.only = TRUE)){
    install.packages(x[i],dep=TRUE, repos='https://mirrors.tuna.tsinghua.edu.cn/CRAN/')
  }
}

mapping <- fread("UMLS_NER_full_mapping.csv")
cui_str <- fread("CUI_STR_CLEANED.csv")
emb <- fread("dodo_emb_new.csv")

mapping_s <- mapping
cui_str_s <- cui_str

tt <- proc.time()
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
cui_str_s_1 = cui_str_s_1[order(cui_str_s_1$str),]
cui_str_s_2 = cui_str_s_2[order(cui_str_s_2$str),]

# 
# ##======
# library(parallel)
# 
# # Calculate the number of cores
# no_cores <- detectCores() - 2
# 
# # Initiate cluster
# cl <- makeCluster(no_cores)
# clusterExport(cl, c("mapping_s","cui_str_s_1","emb"))
# clusterEvalQ(cl, library(stringr))
# system.time(
# cui_str_s_1$loc.map <- 
#   parSapply(cl, 1:dim(cui_str_s_1)[1], 
#             function(exponent){ 
#               paste(which(is.na(
#                 str_match(mapping_s$possible_cui_1, 
#                           cui_str_s_1[exponent,1])) != 1), collapse = ",")
#               })
# )
# cui_str_s_1$loc.emb <- 
#   parSapply(cl, 1:dim(cui_str_s_1)[1], 
#             function(exponent){
#               paste(which(is.na(
#                 str_match(emb$cui, 
#                           cui_str_s_1[exponent,1])) != 1), collapse = ",")
#               })
# 
# 
# 
# 
# stopCluster(cl)
# 

#===========


sfInit(parallel = TRUE, cpus = detectCores())
sfExport("mapping_s","cui_str_s_1", "cui_str_s_2", "emb")
sfLibrary(stringr)


cui_str_s_1$loc.map <- sfApply(cui_str_s_1, 1, function(x) {
  paste(which(is.na(str_match(mapping_s$possible_cui_1, x[1])) != 1), collapse = ",")
})


cui_str_s_1$loc.emb <- sfApply(cui_str_s_1, 1, function(x) {
  paste(which(is.na(str_match(emb$cui, x[1])) != 1), collapse = ",")
})


cui_str_s_2$loc.map <- sfApply(cui_str_s_2, 1, function(x) {
  paste(which(is.na(str_match(mapping_s$possible_cui_2, x[1])) != 1), collapse = ",")
})
cui_str_s_2$loc.emb <- sfApply(cui_str_s_2, 1, function(x) {
  paste(which(is.na(str_match(emb$cui, x[1])) != 1), collapse = ",")
})


cui_uni_1 <- unique(cui_str_s_1$cui)
cui_uni_2 <- unique(cui_str_s_2$cui)

cui_dict_1 <- sfSapply(cui_uni_1, function(x) {
  cui_str_s_1$str[which(cui_str_s_1$cui == x)]
})
cui_dict_2 <- sfSapply(cui_uni_2, function(x) {
  cui_str_s_2$str[which(cui_str_s_2$cui == x)]
})


cui_str_s_1$loc.dict <- sfApply(cui_str_s_1, 1, function(x,cui_dict_1) {
  which(x[1] == names(cui_dict_1))
},cui_dict_1)
cui_str_s_2$loc.dict <- sfApply(cui_str_s_2, 1, function(x,cui_dict_2) {
  which(x[1] == names(cui_dict_2))
},cui_dict_2)


sfStop()

cosine_matrix_top20 = cbind(cosine_matrix_top100[,1],
                            matrix(round(as.numeric(cosine_matrix_top100[,c(2:22)]),4),nrow(cosine_matrix_top100),21,),
                            cosine_matrix_top100[,102:122])
mapping_1 = cbind(mapping_s$ICD_CODE,mapping_s$icd_string,mapping_s$UMLS_ICDSTR_CUI,mapping_s$ICD_CUI,
                  mapping_s$Phecode,mapping_s$Phecode_String,mapping_s$UMLS_PHESTR_CUI,mapping_s$PHECODE_CUI)
mapping_1 = as.data.frame(mapping_1)
colnames(mapping_1) = c("ICD_CODE","icd_string","UMLS_ICDSTR_CUI","ICD_CUI",
                        "Phecode","Phecode_String","UMLS_PHESTR_CUI","PHECODE_CUI")
mapping_s = mapping_1
# 
# cui_str_s_1 = cui_str_s_1[!duplicated(cui_str_s_1$cui),-2]
# cui_str_s_2 = cui_str_s_2[!duplicated(cui_str_s_2$cui),-2]
# 
# dict_name_1 = names(cui_dict_1)
# cui_dict_1_new = sapply(order(names(cui_dict_1)), function(i){
#   y = list(rep(i,length(cui_dict_1[[i]])))
#   names(y) = sort(cui_dict_1[[i]])
#   return(y)
# })
# cui_dict_2_new = sapply(order(names(cui_dict_2)), function(i){
#   y = list(rep(i,length(cui_dict_2[[i]])))
#   names(y) = sort(cui_dict_2[[i]])
#   return(y)
# })
# names(cui_dict_1_new) = sort(names(cui_dict_1))
# names(cui_dict_2_new) = sort(names(cui_dict_2))
# cui_dict_1 = cui_dict_1_new
# cui_dict_2 = cui_dict_2_new

cui_str_s_2 = cui_str_s_2[-seq(nrow(cui_str_s_2)-6,nrow(cui_str_s_2),by=1),]

a <- ls()


rm(list = a[which(a != "cosine_matrix_top20" & a != "mapping_s" &
                  a != "cui_str_s_1" & a != "cui_str_s_2" &
                  #a != "cui_dict_1" & a != "cui_dict_2" & 
                  a != "emb")])
rm(a)


save.image("cutdata.RData")

