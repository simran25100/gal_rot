calculate_oort_constants <- function(Vrot, R, vrad, dvdrot, dvdrad) {
  # Calculate Oort constants
  A <- 0.5 * ((Vrot / R) - dvdrot - (dvdrad / R))
  B <- 0.5 * ((-Vrot / R) - dvdrot + (dvdrad / R))
  C <- 0.5 * ((-vrad / R) + dvdrad - (dvdrot / R))
  K <- 0.5 * ((vrad / R) + dvdrad + (dvdrot / R))
  
  # Return all constants as a list
  return(list(A = A, B = B, C = C, K = K))
}