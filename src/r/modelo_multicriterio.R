#modelo multi-criteria
#create a function taht take arguments the 
# csv with the unweighted supermatrix
# and the block to modify
#and the values to within the block


SM=function(x,Block,col_to_modify,arg_par,rows_in_block){
  UWM=read.csv(paste(path_td,x,sep=""),skip = 1,header = T)
y_cors=which(UWM$X!="")
x_cors=2+y_cors

num_blocks=length(y_cors)*length(x_cors)
if(Block>num_blocks){
  stop('No enough blocks in the supermatrix')
}

####################################################################
###create the blocks from SP
#row 1 super matrix


W11=UWM[y_cors[1]:(y_cors[2]-1),x_cors[1]:(x_cors[2]-1)] ###Block 1
W12=UWM[y_cors[1]:(y_cors[2]-1),x_cors[2]:(x_cors[3]-1)] ###Block 2
W13=UWM[y_cors[1]:(y_cors[2]-1),x_cors[3]:(x_cors[4]-1)] ###Block 3
W14=UWM[y_cors[1]:(y_cors[2]-1),x_cors[4]:(x_cors[5]-1)] ###Block 4
W15=UWM[y_cors[1]:(y_cors[2]-1),x_cors[5]:length(UWM$X)] ###Block 5
#row2
W21=UWM[y_cors[2]:(y_cors[3]-1),x_cors[1]:(x_cors[2]-1)] ###Block 6
W22=UWM[y_cors[2]:(y_cors[3]-1),x_cors[2]:(x_cors[3]-1)] ###Block 7
W23=UWM[y_cors[2]:(y_cors[3]-1),x_cors[3]:(x_cors[4]-1)] ###Block 8
W24=UWM[y_cors[2]:(y_cors[3]-1),x_cors[4]:(x_cors[5]-1)] ###Block 9
W25=UWM[y_cors[2]:(y_cors[3]-1),x_cors[5]:length(UWM$X)] ###Block 10

#row3
W31=UWM[y_cors[3]:(y_cors[4]-1),x_cors[1]:(x_cors[2]-1)] ###Block 11
W32=UWM[y_cors[3]:(y_cors[4]-1),x_cors[2]:(x_cors[3]-1)] ###Block 12
W33=UWM[y_cors[3]:(y_cors[4]-1),x_cors[3]:(x_cors[4]-1)] ###Block 13
W34=UWM[y_cors[3]:(y_cors[4]-1),x_cors[4]:(x_cors[5]-1)] ###Block 14
W35=UWM[y_cors[3]:(y_cors[4]-1),x_cors[5]:length(UWM$X)] ###Block 15

#row4
W41=UWM[y_cors[4]:(y_cors[5]-1),x_cors[1]:(x_cors[2]-1)] ###Block 16
W42=UWM[y_cors[4]:(y_cors[5]-1),x_cors[2]:(x_cors[3]-1)] ###Block 17
W43=UWM[y_cors[4]:(y_cors[5]-1),x_cors[3]:(x_cors[4]-1)] ###Block 18
W44=UWM[y_cors[4]:(y_cors[5]-1),x_cors[4]:(x_cors[5]-1)] ###Block 19
W45=UWM[y_cors[4]:(y_cors[5]-1),x_cors[5]:length(UWM$X)] ###Block 20


#row5
W51=UWM[y_cors[5]:length(UWM$X),x_cors[1]:(x_cors[2]-1)] ###Block 21
W52=UWM[y_cors[5]:length(UWM$X),x_cors[2]:(x_cors[3]-1)] ###Block 22
W53=UWM[y_cors[5]:length(UWM$X),x_cors[3]:(x_cors[4]-1)] ###Block 23
W54=UWM[y_cors[5]:length(UWM$X),x_cors[4]:(x_cors[5]-1)] ###Block 24
W55=UWM[y_cors[5]:length(UWM$X),x_cors[5]:length(UWM$X)] ###Block 25
##############################################################################

if(Block==1){
  block_modify=UWM[y_cors[1]:(y_cors[2]-1),x_cors[1]:(x_cors[2]-1)] ###Block 1
  #check that the block has more columns than the "col_to_modify" argument
  if(length(block_modify[1,])<col_to_modify){stop("Not enough columns in the block")}
  
  #ckeck that the column sum to 1
  if(colSums(block_modify)[col_to_modify]!=1){stop("pick another column")}
  if(sum(arg_par)!=1)stop("arguments for new values must sum to 1")
  UWM[y_cors[1]:(y_cors[2]-1),x_cors[1]:(x_cors[2]-1)][rows_in_block,col_to_modify]=arg_par
}


if(Block==2){block_modify=W12}
if(Block==3){block_modify=W13}
if(Block==4){block_modify=W14}
if(Block==5){block_modify=W15}
if(Block==6){block_modify=W21}
if(Block==7){block_modify=W22}
if(Block==8){block_modify=W23}
if(Block==9){block_modify=W24}
if(Block==10){block_modify=W25}
if(Block==11){block_modify=W31}
if(Block==12){block_modify=W32}
if(Block==13){block_modify=W33}
if(Block==14){block_modify=W34}
if(Block==15){block_modify=W35}
if(Block==16){block_modify=W41}
if(Block==17){block_modify=W42}
if(Block==18){block_modify=W43}
if(Block==19){block_modify=W44}
if(Block==20){block_modify=W45}
if(Block==21){block_modify=W51}
if(Block==22){block_modify=W52}
if(Block==23){block_modify=W53}
if(Block==24){
#  block_modify=W54
  block_modify=UWM[y_cors[5]:length(UWM$X),x_cors[4]:(x_cors[5]-1)] ###Block 24
  #check that the block has more columns than the "col_to_modify" argument
  if(length(block_modify[1,])<col_to_modify){stop("Not enough columns in the block")}
  
  #ckeck that the column sum to 1
  if(colSums(block_modify)[col_to_modify]!=1){stop("pick another column")}
  if(sum(arg_par)!=1)stop("arguments for new values must sum to 1")
  browser()
  UWM[y_cors[5]:length(UWM$X),x_cors[4]:(x_cors[5]-1)][rows_in_block,col_to_modify]=arg_par
  UWM_modified<-UWM
  }
if(Block==25){block_modify=W55}
return(UWM_modified)
}

######check for block 24 sacmex matrix######
UWM_initial=read.csv(paste(path_td,'supermatrices/SACMEX_Drenaje modificada febrero 2017.unweighted.csv',sep=""),skip = 1,header = T)
#print matrix before the tranformation for block 24
####call function####
SM(x='supermatrices/SACMEX_Drenaje modificada febrero 2017.unweighted.csv',Block=24,col_to_modify=1,rows_in_block=c(1,2),arg_par=c(0.7,0.3))
#print matrix after the tranformation
print(UWM)
