######### Random ranges (cohesive) with Environmental control
## authors: Fabricio Villalobos & Jorge Soberón (for Villalobos et al. 2014. BAAE. http://dx.doi.org/10.1016/j.baae.2013.11.001)


# function to generate a random range with environmental restrictions (spatially and environmentally cohesive)
# this particular function is exclusive to Fabricio's data (30625 cells in total)
# It gives back a vector with the cell's IDs of the random range.

# vecinos: matrix of neighbors (1st column with the individual site and the rest of columns with its neighbors)
# ambientales: matrix of environmental values for each site (1st column with the site and the rest of columns with e-values)
# tam: size of the desire random range
# cells: maximum value of IDs or maximum number of cells

nicheRange <- function(vecinos, ambientales, tam, cells)
{

n=as.matrix(vecinos)
d=as.matrix(ambientales)
d2=d[,-1]

#n1 sin la primare acolumna
n1=n[,-1]
# n2 es la matriz de elegibles
n2=n1
# n2 para ir borrando repetidos
sps=1
celds=tam
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
while(contador<tam)
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
     random_range
   }
