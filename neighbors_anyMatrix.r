## Fabricio Villalobos

## Function to identify the 8 nearest neighbors (cells) of each individual cell
## The original matrix has to have the first and last columns full of zeros, as well as the first and last rows. If it is not like this, add such rows and columns.
## It is also needed to know the number of cells with actual values (IDs), to use them as the "ided_cells"

neighbors<-function(data_file, rows, columns, ided_cells){

niche_matrix<-read.table(data_file, header=F)

cells<-matrix(0,nrow=ided_cells,ncol=9)
      row_index<-1
      col_index<-1

  for (i in 1:rows){
  
    for (j in 1:columns){
      
      if (niche_matrix[i,j]!=0){
      
        cells[row_index,col_index]<-niche_matrix[i,j]
        cells[row_index,col_index+1]<-niche_matrix[i-1,j-1]
        cells[row_index,col_index+2]<-niche_matrix[i-1,j]
        cells[row_index,col_index+3]<-niche_matrix[i-1,j+1]
        cells[row_index,col_index+4]<-niche_matrix[i,j-1]
        cells[row_index,col_index+5]<-niche_matrix[i,j+1]
        cells[row_index,col_index+6]<-niche_matrix[i+1,j-1]
        cells[row_index,col_index+7]<-niche_matrix[i+1,j]
        cells[row_index,col_index+8]<-niche_matrix[i+1,j+1]
        
        row_index<- row_index + 1

      }
      
    }
  }
  
  # assign("neighbor_ids",cells,env=.GlobalEnv)
  write.table(cells, paste('neighbor_ids_andres1.txt',sep=""), sep="\t")
  
}
