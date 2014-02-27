###### Fabricio Villalobos

## function to iterate the creation of random ranges with environmental control, using a different vector of random range sizes for each iteration
## arguments are the same as the nicheRange and nicheManyRanges functions
## the function saves (writes) the 
## NOTE: again, this is for Fabricio's data. In this case, the total number of cells with environmental information (of interest) within the geographic domain (6489)

nicheRangesVectors <- function(vecinos, ambientales, spp, cells, sims){

  for (j in 1:sims){
  
    print(j)

	rangesizes <- round(runif(spp)*6489)
  
    one.niche.pam <- nicheManyRanges(vecinos,ambientales,rangesizes,cells)  
   
    write.table(one.niche.pam, paste('niche_table_', j, '.txt', sep=""), sep="\t")
  }

}