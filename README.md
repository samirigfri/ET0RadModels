# ET0RadModels

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/ET0RadModels)](https://CRAN.R-project.org/package=ET0RadModels)
[![R-CMD-check](https://github.com/samirigfri/ET0RadModels/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/samirigfri/ET0RadModels/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**ET0RadModels** provides 10 radiation-based empirical models for estimating
daily reference evapotranspiration (ET0), with the FAO Penman-Monteith
standard method as the reference, statistical evaluation metrics, and
visualization tools.

## Installation

Install from CRAN:

```r
install.packages("ET0RadModels")
```

Or install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("samirigfri/ET0RadModels")
```

## Quick Start

```r
library(ET0RadModels)

# Compute ET0 using all 10 radiation-based models for a single day
result <- et0_rad_all(
  Tmin = 15, Tmax = 35,
  RH_morning = 90, RH_evening = 40,
  u2 = 1.5, n = 9, J = 172,
  lat = 25.43, z = 216
)

# Evaluate model performance against FAO-PM
data(jhansi_weather)
full <- et0_rad_all(
  Tmin       = jhansi_weather$Tmin,
  Tmax       = jhansi_weather$Tmax,
  RH_morning = jhansi_weather$RH_morning,
  RH_evening = jhansi_weather$RH_evening,
  u2         = jhansi_weather$WS,
  n          = jhansi_weather$SSH,
  J          = jhansi_weather$J,
  lat = 25.43, z = 216
)

metrics <- evaluate_models(full$FAO_PM, full[, -1])
print(metrics)
```

## Models Included

| # | Model | Function |
|---|-------|----------|
| Reference | FAO Penman-Monteith | `et0_fao_pm()` |
| 1 | Jensen-Haise (1963) | `et0_jensen_haise()` |
| 2 | McGuinness-Bordne (1972) | `et0_mcguinness_bordne()` |
| 3 | Irmak (2003) | `et0_irmak()` |
| 4 | Makkink (1957) | `et0_makkink()` |
| 5 | Turc (1961) | `et0_turc()` |
| 6 | Caprio (1974) | `et0_caprio()` |
| 7 | Abtew (1996) | `et0_abtew()` |
| 8 | Priestley-Taylor (1972) | `et0_priestley_taylor()` |
| 9 | Jones-Ritchie (1990) | `et0_jones_ritchie()` |
| 10 | Hargreaves (1975) | `et0_hargreaves()` |

## Citation

```r
citation("ET0RadModels")
```

Satpute, A.N., Barman, S., Singh, A.K., Ghosh, A., & Gupta, G. (2026).
A multi-scale evaluation of 30 empirical models for reference evapotranspiration
estimation in a data-scarce semi-arid region: A case study of Jhansi, India.
*Journal of Hydrology: Regional Studies*.
https://doi.org/10.1016/j.ejrh.2026.103925

## License

GPL (>= 3)
