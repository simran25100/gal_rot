calculate_proper_motion_l <- function(A, B, C, K, l, b, pi, u0, v0, w0) {
  library(REdaS)
  # Convert degrees to radians for calculations
  l_rad <- deg2rad(l)
  b_rad <- deg2rad(b)
  
  # Calculate proper motion
  mul <- (A * cos(2 * l_rad) - C * sin(2 * l_rad) + B) * cos(b_rad) + pi * (u0 * sin(l_rad) - v0 * cos(l_rad))
  mub <- -(A * sin(2 * l_rad) + C * cos(2 * l_rad) + K) * sin(b_rad) * cos(b_rad) + pi * ((u0 * cos(l_rad) + v0 * sin(l_rad)) * sin(b_rad) - w0 * cos(b_rad))
  
  # Derivatives of the proper motions
  dmul <- (mul - pi * (u0 * sin(l_rad) + v0 * cos(l_rad))) / cos(b_rad)
  dmub <- -(mub - pi * ((u0 * cos(l_rad) + v0 * sin(l_rad)) * sin(b_rad) - w0 * cos(b_rad))) / (sin(b_rad) * cos(b_rad))
  
  # Return all values as a list
  return(list(mul = mul, mub = mub, dmul = dmul, dmub = dmub))
}
