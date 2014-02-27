######### NICHE CONSERVATISM ##############

## Fabricio Villalobos

### Function to generate random ranges with niche conservatism, using the "nicheFirst" function to create ranges once an initial one has been created
## arguments as in the nicheManyRanges function
## this function returns the niche matrix with the IDs of the generated ranges

nicheConserv <- function(vecinos, ambientales, ranges_vector, cells)
{

n=as.matrix(vecinos)
d=as.matrix(ambientales)
d2=d[,-1]

niche_table <- matrix(0, nrow = cells, ncol = length(ranges_vector))

#n1 sin la primera columna
n1=n[,-1]
# n2 es la matriz de elegibles
n2=n1
# n2 para ir borrando repetidos
sps=1
celds = sample(ranges_vector, size=1)
contador=1
manchas=matrix(0,nrow=sps,ncol=celds)
center=sample(1:cells,size=1)
centro <- FALSE
while (centro == FALSE){
  if(sum(n1[center,]) == 0)
    center=sample(1:cells,size=1) else {
#Primer punto de la mancha
    manchas[1,1]=center
    centro <- TRUE
  }
}    


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
while(contador<celds)
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
      manchas[1,contador]=nuevo
   
                                                 
      
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
     
     random_range <- t(manchas)
		
		niche_table[random_range, 1] = 1
		
		ranges_vector <- as.matrix(ranges_vector)
		
		ranges_vector <- ranges_vector[-(which(ranges_vector==length(random_range))),]
		
		ranges_vector <- sample(ranges_vector)
		
			for (j in 1:length(ranges_vector)){
				
				random_range <- niche_chosen(niche_table, ranges_vector[j], n, d)
				
				niche_table[random_range, j+1] = 1														
		
				
			}

	assign("niche_table", niche_table, env=.GlobalEnv)

   }