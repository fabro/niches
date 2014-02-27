######### NICHE CONSERVATISM ##############

## Fabricio Villalobos

## Function to generate random ranges within previously generated ranges ( for the "nicheConserv" function)
## arguments: the size of the first range (range_size); the niche table produced by the "nicheManyRanges" function

nicheFirst <- function(niche_table, range_size, n, d)
{

#n=as.matrix(read.table(vecinos, header = TRUE))
#d=as.matrix(read.table(ambientales, header=T))
d2=d[,-1]

#n1 sin la primera columna
n1=n[,-1]
# n2 es la matriz de elegibles
n2=n1
# n2 para ir borrando repetidos
sps=1

celds = range_size

# recover the ID's (niche_table row numbers) of random_generated_ranges to choose the next random_range's "seed"
ids_list <- which(rowSums(niche_table)!=0)

#celds = sample(ranges_vector, size=1)
	#rango <- FALSE
	#while (rango == FALSE){
		#if (celds == length(random_range) )
		
		#	celds = sample(ranges_vector, size=1) else {
				#rango <- TRUE
				
		#	}										
#	} 
	

contador=1
manchas2=matrix(0, nrow=sps, ncol=celds)
center=sample(ids_list, size=1)
#centro <- FALSE
#while (centro == FALSE){
 # if(sum(n1[center,]) == 0)
  #  center=sample(1:cells,size=1) else {
#Primer punto de la mancha
   manchas2[1,1]=center
    #centro <- TRUE
  #}
#}    


## Sacar las probabilidades de cada sitio con base en el centro
    # Set the initial point as the seed for the Mahalanobis distances (centre)
      seed <- matrix(d2[center,], nrow=1, ncol=ncol(d2))
      
      # Calculate the covariance matrix excluding NA observations: sites with no environmental data
      cov_matrix<-cov(d2, use="complete.obs")
      
      # Calculate the Mahalanobis distances between the seed_cell and the rest of sites/cells
      maha_dist<-mahalanobis(d2, seed, cov_matrix)
      
      # Calculate the table of probabilities according to the distances (this will be used as the "friction layer" for the spreading of ranges
      max_dist<-max(na.exclude(maha_dist))    
      site_probs_nas<-as.data.frame(1-(maha_dist/max_dist)) 
      site_probs_nas[is.na(site_probs_nas)] <- 0
      site_probs<-site_probs_nas 
      site_probs<-as.data.frame(site_probs)

 
# Remuevo center de los elegibles
# primero, busco los indices de las celdas colindantes con center
j=which(n1[center,]>0)
# Luego, sacos lo numeros de esas celdas
jj=n1[center,j]
#matrifico jj, no se por que fregaos, pero hay que hacerlo
jj=as.matrix(jj)
for(k in 1:length(jj)){
                      h=which(n2[jj[k],]==center);
                      n2[jj[k],h]=0
                      }
#Lo anterior me deja una matriz de elegibles, n2, ya sin center
#E inicio el conjunto de los elegibles
j=which(n2[center,]>0)
elegibles=as.matrix(n2[center,j])
#Empiezo a iterar
#
while(contador < celds)
      {
      contador=contador+1
   # Cuento los elegibles
      l=length(elegibles)
   #Saco un indice al azar de los l elegibles
      #i=sample(1:l,size=1)
   
      ## Checar que i de los elegibles tenga la mayor probabilidad
      proba_elegibles <- matrix(0, nrow=1, ncol=length(elegibles))
      for (m in 1:length(elegibles)){
        proba_elegibles[,m]<-site_probs[elegibles[m],1]
        
        if (proba_elegibles[1,m]==max(proba_elegibles)){
        nuevo=elegibles[m]
        }
      
      }
      
           
   # Aniado la nueva celda a manchas
      #nuevo=elegibles[i]
      manchas2[1,contador]=nuevo
   
                                                 
      
   # Y borro la nueva de entre la matriz de elegibles, buscando primero los colindantes con nuevo
      j=which(n1[nuevo,]>0)
      jj=n1[nuevo,j]
      jj=as.matrix(jj)
      for(k in 1:length(jj)){
                            h=which(n2[jj[k],]==nuevo);
                            n2[jj[k],h]=0
                            }
      a1=as.matrix(elegibles)
      a2=as.matrix(n2[nuevo,n2[nuevo,]>0])
      a3=union(a1,a2)
      a=setdiff(a3,nuevo)
      l=length(a)
      elegibles=matrix(0,ncol=l,nrow=1)
      elegibles=a
      }
     
     random_range2 <- t(manchas2)
	 random_range2
 
   }