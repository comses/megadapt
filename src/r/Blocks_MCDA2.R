#modelo multi-criteria
#create a function taht take arguments the
# csv with the unweighted supermatrix
# and the block to modify
#and the values to within the block


modify_block_row=function(path_td, unwaited_supermatrix, block, col_to_modify, values){
  if(sum(values)!=1)stop("values must sum to 1")
  UWM=read.csv(paste(path_td),sep=",",skip = 1,header = T)
  y_cors=which(UWM$X!="")
  x_cors=2+y_cors
  index = 1
  category_coors <- list()
  for(i in y_cors){
    index <- index + 1
    if (is.na(y_cors[index])){
      category_coors[[paste(index-1)]] <- c(i,nrow(UWM))
    }else{
      category_coors[[paste(index-1)]] <- c(i,y_cors[index]-1)
    }
  }
  a <- category_coors[[paste(block[1])]][1]
  b <- category_coors[[paste(block[1])]][2]
  cc <- category_coors[[paste(block[2])]][1] + col_to_modify - 1
  if(length(values)!=length(a:b))stop("number of rows to change must be the same length as the length of the col_to_modify column in block")

  unwaited_supermatrix[a:b, cc] <- values
  return(unwaited_supermatrix)
}

get_matrix <- function(path_td){
  df=read.csv(paste(path_td),sep=",",skip = 1,header = T)
  matrix <- as.matrix(df[c(-1, -2)])
  colnames(matrix) <- NULL
  return(matrix)
}


x = get_matrix("../../data/supermatrices/SACMEX_unweighted_SESMO.csv")
xm = modify_block_row(path_td ="../../data/supermatrices/SACMEX_unweighted_SESMO.csv", x, block =c(1,1), col_to_modify =1,values =  c(0.2,0.8))
xm = modify_block_row("../../data/I072816_OTR.weighted.csv", xm, c(1,1),2,c(0.2,0.2,0.2,0.2,0.2))
xm = modify_block_row("../../data/I072816_OTR.weighted.csv", xm, c(1,1),3,c(0.2,0.2,0.2,0.2,0.2))
xm = modify_block_row("../../data/I072816_OTR.weighted.csv", xm, c(1,2),1,c(0.1,0.1,0.2,0.3,0.3))
#hola vic
xm = modify_block_row("../../data/I072816_OTR.weighted.csv", xm, c(1,2),3,c(0.1,0.1,0.2,0.3,0.3))
###############################################################
#new weighted super matrix
weighted_supermatrix=sweep(xm,MARGIN = 2,colSums(xm),FUN = "/")
###################################################################
