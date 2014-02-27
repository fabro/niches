######### Fabricio Villalobos

### function to apply the "nicheRange" (Villalobos & Soberón) function for a set of ranges (vector of range size; argument: 'ranges')
## input arguments are the same as in the nicheRange function
## NOTE: the matrix created within the function corresponds to Fabricio's data dimensions (175*175). You'll have to change it accordingly to your own data (remember that it comes from a square (NxN) matrix/grid)

nicheManyRanges <- function(vecinos, ambientales, rangesizes, cells){

niche_table <- matrix(0, nrow=175*175, ncol=length(rangesizes))

    
   for (i in 1:length(rangesizes)){
    
    one.random.range <- nicheRange(vecinos, ambientales, rangesizes[i], cells)
    
    niche_table[one.random.range,i]=1
    
             }
             
    niche_table
}