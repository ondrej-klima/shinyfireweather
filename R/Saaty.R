# function
"%^%"<-function(A,n){ 
  if(n==1) A else {B<-A; for(i in (2:n)){A<-A%*%B}}; A 
}


Saaty <- function() {
  ### krok c)
  # matice parovych srovnani
  A<-matrix(c(1,8,3,2,
              1/8,1,1/3,1/4,
              1/3,3,1,1/2,
              1/2,4,2,1),4,4)
  colnames(A)<-rownames(A)<-c("dostupnost dat","věrohodnost dostupných dat",
                              "aktuálnost dostupných dat",
                              "odhadovaná míra předpokládatného ovlivnění hodnot vysvětlované proměnné")
  # ty nazvy i pocty promennych jsou predvolene podle metodiky, ale u obojiho
  # bych jim dal moznost to upravit (ale defaultne by to bylo takto)
  
  ### krok d)
  # eigenvalue
  eigen(A)
  if (Im(eigen(A)$values[1])==0){
    lambda<-Re(eigen(A)$values[1])
  } else {
    print("warning> complex eigenvalue") 
  } 
  # myslim, ze to posledni tu byt nemusi, podle me nemuze byt prvni vlastni cislo
  # komplexni, ale nevidim to linearni algebry natolik, abych si tim byl jisty
  
  CI<-(lambda-dim(A)[1])/(dim(A)[1]-1)
  
  ### krok e)
  RI<-c(0,0,0.52,0.89,1.11,1.24,1.35,1.4,1.45,1.49)
  CR<-round(CI/RI[dim(A)[1]],2)
  
  ### krok f)
  # eigenvector
  for (k in 1:1000){
    if (k!=1){
      w0<-w
    }
    w<-rowSums(A%^%k)/sum(rowSums(A%^%k))
    if (k!=1){
      if (all((w-w0)<0.0001)){
        break
      }
    }
  }
  rm(w0,k)
  names(w)<-colnames(A)
  return(w)
}