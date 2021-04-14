mod <- swmmr::read_inp("inst/extdata/sensitivity/models/zone1/1.inp")
mod$options

mod$lid_usage

mod$lid_controls

q_drain <- function(C, h, n = 0.5) {

  C * h ^ n
}

C <- seq(2,150, 0.1)
h <- 300

C <- 1
h <- seq(0,650,50)
plot(h, q_drain(C, h, n = 0.5))

### Abflussspendenwert (l/s/ha)

# area_roof <- 65 #m2
# area_rain.barrel <-
# rain_mm.per.hour <-




suction.head_mm <- function(kf_mm.per.hour) {

  #Source: Rawls, W.J. et al., (1983). J. Hyd. Engr., 109:1316.
  #Note: The following relation between wilting_point_mm and kf_mm.per.hour ca
  # n be derived from this table:
  # Ψ = 3.237K^-0.328  (R2 = 0.9)
inch_to_mm <- 25.1

kf_inch.per.hour <- kf_mm.per.hour / inch_to_mm

(3.237 * kf_inch.per.hour^-0.328) * inch_to_mm

}

suction.head_mm (kf_mm.per.hour = 30)



