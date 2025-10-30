# Local galactic rotational features from Oort constants
R0 <- 8178/1000
R0_err1 <- 13/1000
R0_err2 <- 22/1000
   
A     <- 15.46
A_err <- 0.22

B     <- -14.26
B_err <- 0.17

C     <- 2.85
C_err <- 2.85

K     <- 6.97
K_err <- 6.97

u0     <- 9.44
u0_err <- 0.28

v0     <- 11.64
v0_err <- 0.29

w0     <- 7.06
w0_err <- 0.58

s_l     <- 4.59
s_l_err <- 0.15

s_b     <- 3.12
s_b_err <- 0.42

nu_l     <- 1.96
nu_l_err <- 0.13

nu_b     <- 2.29
nu_b_err <- 0.56


A_c     <- 15.42
A_c_err <- 0.36

B_c     <- -14.4
B_c_err <- 0.23

u0_c     <- 9.8
u0_c_err <- 0.85

v0_c     <- 11.52
v0_c_err <- 0.58

w0_c     <- 6.87
w0_c_err <- 0.79

s_l_c     <- 4.78
s_l_c_err <- 0.38

s_b_c     <- 3.16
s_b_c_err <- 0.17

nu_l_c     <- 2.01
nu_l_c_err <- 0.23

nu_b_c     <- 2.31
nu_b_c_err <- 0.3

################## 
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
##### Non - Circular rotation
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
################## 

# Local angular velocity Ω0 = A - B km/s/kpc
LAV     <- A - B
LAV_err <- sqrt(A_err^2 + B_err^2)

# Local gradient of rotation curve = -(A + B) km/s/kpc
# when negative: incr rad ~> decr vel (expected)
LGRC     <- -(A + B)
LGRC_err <- sqrt(A_err^2 + B_err^2)

# Local radial velocity gradient = K + C km/s/kpc
LGRV     <- K + C
LGRV_err <- sqrt(K_err^2 + C_err^2)

# Rot speed of LSR km/s
CGR     <- R0*(A - B)
CGR_err <- sqrt(A_err^2 + B_err^2 + R0_err1^2 + R0_err2^2)

cat("Local angular velocity NC  =", round(LAV, 2),  "±", round(LAV_err, 2),  "\n")
cat("Local gradient of rotation curve NC =", round(LGRC, 2), "±", round(LGRC_err, 2), "\n")
cat("Local radial velocity gradient NC =", round(LGRV, 2), "±", round(LGRV_err, 2), "\n")
cat("Circular Galactic Rotation NC =", round(CGR, 2),  "±", round(CGR_err, 2),  "\n")

################## 
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
##### Circular rotation
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
################## 

LAV_c    <- A_c- B_c
LAV_c_err <- sqrt(A_c_err^2 + B_c_err^2)

LGRC_c     <- -(A_c+ B_c)
LGRC_c_err <- sqrt(A_c_err^2 + B_c_err^2)

CGR_c     <- R0*(A_c- B_c)
CGR_c_err <- sqrt(A_c_err^2 + B_c_err^2 + R0_err1^2 + R0_err2^2)

cat("Local angular velocity C =", round(LAV_c, 2),  "±", round(LAV_c_err, 2),  "\n")
cat("Local gradient of rotation curve C =", round(LGRC_c, 2), "±", round(LGRC_c_err, 2), "\n")
cat("Circular Galactic Rotation C =", round(CGR_c, 2),  "±", round(CGR_c_err, 2),  "\n")


# finding significance
# Li paper values
LAVli<- 28.5
LAVlierr<-0.1

LAVbovy<-27.1
LAVbovyerr<-0.5

# LAVreid<- 30.3
# LAVreiderr<-0.9

LAVreid<- 30.57
LAVreiderr<-0.43

# Values of paper 2 for comparison
LAVpaper2<-LAVreid
LAVpaper2err<-LAVreiderr

val1 <- LAVpaper2
val2 <-LAV

v1err<-LAVpaper2err
v2err<-LAV_err

diff<- val2 - val1

combsig<-sqrt(v1err^2 + v2err^2)

sig<-abs(diff)/combsig

cat("Significance LAV  =",round(sig,2),"σ","\n")



# Function to calculate sigma significance
sigma_significance <- function(val1, err1, val2, err2) {
  diff <- val2 - val1
  combsig <- sqrt(err1^2 + err2^2)
  sig <- abs(diff) / combsig
  return(round(sig, 2))
}

# Example usage:
LAV_sig <- sigma_significance(val1 = 13.8, err1 = 0.4, val2 = 11.3, err2 = 0.5)
cat("Significance =", LAV_sig, "σ\n")

# Example usage:
LGRC_sig <- sigma_significance(val1 = -1.7, err1 = 0.1, val2 = LGRC, err2 = LGRC_err)


# Local radial velocity gradient = K + C
LGRV     <- K + C
LGRV_err <- sqrt(K_err^2 + C_err^2)

# Example usage:
LGRV_sig <- sigma_significance(val1 = -6.6, err1 = 0.7, val2 = LGRV, err2 = LGRV_err)
cat("LGRV Significance Bovy =", LGRV_sig, "σ\n")

LGRV_sig <- sigma_significance(val1 = -4.4, err1 = 0.2, val2 = LGRV, err2 = LGRV_err)
cat("LGRV Significance Li =", LGRV_sig, "σ\n")


LCGR_sig <- sigma_significance(val1 = 254, err1 =  16, val2 = CGR, err2 = CGR_err)
cat("CGR Significance R09 =", LCGR_sig, "σ\n")

LCGR_sig <- sigma_significance(val1 = 240, err1 =  8, val2 = CGR, err2 = CGR_err)
cat("CGR Significance R14 =", LCGR_sig, "σ\n")
