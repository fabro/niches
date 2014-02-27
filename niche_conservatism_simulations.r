##### Iterate the NICHE CONSERVATISM functions

## Fabricio Villalobos

## Function to generate a random presence-absence matrix based on ranges generated by the "nicheConserv" function
## It uses "nicheConserv" & "nicheFirst" functions (need to be loaded in the R workspace already)
## a different vector of random ranges is used for each simulation (i.e. shuffling the ranges/species)

## NOTE: remember that this is for Fabricio's data (6489 cells with environmental info). You should change this according to your data.

nicheConservSims <- function(vecinos, ambientales, spp, cells, sims){
	     
    for (i in 1:sims){
	
	print (i)
	
	ranges_vector <- round(runif(spp)*6489)
     
     one_random_niche_table <- nicheConserv(vecinos, ambientales, ranges_vector, cells)
     
     write.table(one_random_niche_table, paste('nicheconserv_table_', i, '.txt', sep=""), sep="\t", row.names=F, col.names=F)
     
     }
             
}