############ MAPPING RESULTS

## Fabricio Villalobos

## Function to convert the IDs of a random_range into a matrix of the same characteristics than the domain used (matrix of continent cells, 1 on land, 0 on sea)
## using the matrix with cell ID's of the domain (e.g. matrix_cell_ids.txt) to check for the ID's in the individual ranges
## so it can be map with the function MyImagePlot() by adding matrices (e.g. continent_matrix + spX_matrix)
## arguments: matriz=continent matrix; sp_range=the size of the species' range to map
## NOTE: exclusive to Fabricio's data. Change the dimensions according to your data (e.g. 175 for XXX)

spp_matrix_mapping<-function(matriz, sp_range){


#cont_matrix <- read.table(matriz, header=F)

#cont_matrix <- as.matrix(cont_matrix)

sp_range <- as.matrix(sp_range)

sp_matrix <- matrix(0,nrow=175,ncol=175)

for (a in 1:length(sp_range)){

  for (i in 1:175){
  
    for (j in 1:175){
      
      if (matriz[i,j] == sp_range[a,]){
      
		sp_matrix[i,j] <- 1
        

      }
      
    }
  }

}
  
  assign("sp_matrix",sp_matrix,env=.GlobalEnv)
  write.table(sp_matrix, paste('sp_matrix.txt',sep=""), sep="\t", row.names=F, col.names=F)
  
}
