# CropWaterBalance
Calculates daily climate water balance for irrigation purposes.

# Basic Description
The CropWaterBalance is an R package designed to assist users in irrigation scheduling based on the Water Balance Approach.
The package is capable of calculating reference evapotranspiration (ET0) through various methods and conducting crop water balance accounting. Additionally, CropWaterBalance includes auxiliary functions for comparing different ET0 estimation methods, calculating descriptive statistics for ET0 and rainfall series, and estimating soil heat flux and water stress coefficient. 
The functions ET0_HS(), ET0_PT(), and ET0_PM() are used to estimate daily ET0 amounts using the methods of Hargreaves-Samani, Priestley-Taylor, and FAO-56 Penman-Monteith, respectively.
The Descriptive() function is specifically designed to calculate descriptive statistics for ET0 and rainfall series, including sample mean, median, standard deviation, standard error, maximum value, minimum value, and frequency of zeros. Additionally, the Compare() function may be used to calculate measures of accuracy and agreement between two ET0 or rainfall series.
The Soil_Heat_Flux() function uses average air temperature data to estimate the soil heat flux, and the Water_Stress_Coef() function calculates the water stress coefficient for a crop.
The package depends on R (>= 2.10) and imports functions from the R packages {[PowerSDI]( https://CRAN.R-project.org/package=PowerSDI/)} and {[lubridate]( https://CRAN.R-project.org/package=lubridate)}.

# Installation

```r
devtools::install_github("gabrielblain/CropWaterBalance")
```

# Basic Instructions

## Function ET0_PM()
Calculates daily reference evapotranspiration amounts using the Penman and Monteith method.

## Usage
```r
ET0_PM(Tavg,
Tmax,
Tmin,
Rn,
RH,
WS,
G = NULL)
```

## Arguments

* Tavg: A vector, 1-column matrix or data frame with daily average air temperature.
* Tmax: A vector, 1-column matrix or data frame with daily maximum air temperature
in Celsius degrees.
* Tmin: A vector, 1-column matrix or data frame with daily minimum air temperature in
Celsius degrees.
* Rn: A vector, 1-column matrix or data frame with daily net radiation in MJ M-2 DAY-1.
* RH: A vector, 1-column matrix or data frame with daily relative Humidity in %.
* WS: A vector, 1-column matrix or data frame with daily wind speed in M S-1.
* G: Optional. A vector, 1-column matrix or data frame with daily soil heat flux in
MJ M-2 DAY-1. Default is `NULL` and if `NULL` it is assumed to be zero.
May be provided by Soil_Heat_Flux
## Value

Daily reference evapotranspiration amounts in millimetres.
## Examples

```r
data(DataForCWB)
Tavg <- DataForCWB[,2]
Tmax <- DataForCWB[,3]
Tmin <- DataForCWB[,4]
Rn <- DataForCWB[,6]
WS <- DataForCWB[,7]
RH <- DataForCWB[,8]
G <- DataForCWB[,9]
ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
```

## Function ET0_PT()
Calculates daily reference evapotranspiration amounts using the Priestley-Taylor method.

## Usage
```r
ET0_PT(
Tavg,
Rn,
G = NULL, 
Coeff = 1.26) 
```

## Arguments

* Tavg: A vector, 1-column matrix or data frame with daily average air temperature.
* Rn: A vector, 1-column matrix or data frame with daily net radiation in MJ M-2 DAY-1.
* G: Optional. A vector, 1-column matrix or data frame with daily soil heat flux in
MJ M-2 DAY-1. Default is `NULL` and if `NULL` it is assumed to be zero.
May be provided by Soil_Heat_Flux
* Coeff: Single number defining the Priestley-Taylor coefficient. Default is 1.26
## Value

Daily reference evapotranspiration amounts in millimetres.
## Examples

```r
data(DataForCWB)
Tavg <- DataForCWB[,2]
Rn <- DataForCWB[,6]
G <- DataForCWB[,9]
ET0_PT(Tavg, Rn,G) 
```

## Function ET0_HS()
Calculates daily reference evapotranspiration amounts using the Hargreaves-Samani method.

## Usage
```r
ET0_HS(
Ra,
Tavg,
Tmax,
Tmin
) 
```

## Arguments

* Ra: A vector, 1-column matrix or data frame with daily net radiation in MJ M-2 DAY-1.
* Tavg: A vector, 1-column matrix or data frame with daily average air temperature.
* Tmax: A vector, 1-column matrix or data frame with daily maximum air temperature
in Celsius degrees.
* Tmin: A vector, 1-column matrix or data frame with daily minimum air temperature in
Celsius degrees.
## Value

Daily reference evapotranspiration amounts in millimetres.
## Examples

```r
data(DataForCWB)
Tavg <- DataForCWB[,2]
Tmax <- DataForCWB[,3]
Tmin <- DataForCWB[,4]
Ra <- DataForCWB[,5]
ET0_HS(Ra=Ra, Tavg=Tavg, Tmax=Tmax, Tmin=Tmin) 
```

## Function Soil_Heat_Flux()
Calculates the daily amounts of Soil Heat Flux.

## Usage
```r
Soil_Heat_Flux(Tavg) 
```

## Arguments

* Tavg: A vector, 1-column matrix or data frame with daily average air temperature.
## Value

Daily amounts of soil Heat flux in MJ m-2 day-1.
## Examples

```r
data(DataForCWB)
Tavg <- DataForCWB[,2]
Soil_Heat_Flux(Tavg) 
```

## Function Descriptive()
Calculates descriptive statistics for rainfall, evapotranspiration, or other variable.

## Usage
```r
Descriptive(Sample)
```

## Arguments

* Sample: A vector, 1-column matrix or data frame with rainfall, evapotranspiration, or other variable.
## Value

* sample mean (Avg), sample median (Med), sample standard variation (SD), sample standard Error (SE), maximum value (MaxValue), minimum value (MinValue), and frequency of zeros (FreqZero%)
## Examples

```r
data(DataForCWB)
Rain <- DataForCWB[,10]
Descriptive(Sample = Rain)
```

## Function Compare()
Calculates measures of accuracy and agreement.

## Usage
```r
Compare(Method1, Method2)
```

## Arguments

* Method1: A vector, 1-column matrix or data frame with evapotranspiration or other variable.
* Method2: A vector, 1-column matrix or data frame with evapotranspiration or other variable.
## Value

* Absolute mean error (AME), Square root of the mean squared error (RMSE), Willmott's indices of agreement: original (dorig), Modified (dmod) and refined (dref), Pearson determination coefficient (R2).
## Examples

```r
data(DataForCWB)
Tavg <- DataForCWB[,2]
Tmax <- DataForCWB[,3]
Tmin <- DataForCWB[,4]
Rn <- DataForCWB[,6]
WS <- DataForCWB[,7]
RH <- DataForCWB[,8]
G <- DataForCWB[,9]
Method1 <- ET0_PM(Tavg=Tavg, Tmax=Tmax, Tmin=Tmin, Rn=Rn, RH=RH, WS=WS,G=G)
Method2 <- ET0_PT(Tavg=Tavg, Rn=Rn,G=G)
Compare(Method1=Method1, Method2=Method2)
```

## Function CWB()
Calculates several parameters of the crop water balance. It also suggests when irrigate.

## Usage

```r
CWB(
Rain,
ET0,
AWC,
Drz,
Kc = NULL,
Ks = NULL,
Irrig = NULL,
MAD = NULL,
InitialSoilWater = NULL,
InitialD = 0,
start.date = "2011-11-23"
)
```

## Arguments

* Rain: Vector, 1-column matrix or data frame with daily rainfall totals in millimeters.
* ET0: Vector, 1-column matrix or data frame with daily reference evapotranspiration in millimeters.
* AWC: Vector, 1-column matrix or data frame with the available water capacity of the soil, that is: the amount of water between field capacity and permanent wilting point in millimeter of water per centimeter of soil.
* Drz: Vector, 1-column matrix or data frame defining the root zone depth in centimeters.
* Kc: Vector, 1-column matrix or data frame defining the crop coefficient. If NULL its values are assumed to be 1.
* Ks: Vector, 1-column matrix or data frame defining the water stress coefficient. If NULL its values are assumed to be 1.
* Irrig: Vector, 1-column matrix or data frame with net irrigation amount infiltrated into the soil for the current day in millimeters.
* MAD: Vector, 1-column matrix or data frame defining the management allowed depletion. Varies between 0 and 1.
* InitialSoilWater: Single number defining in millimeter the initial amount of soil water. It is used to start the water balance accounting. If NULL it is assumed to be at the field capacity.
* InitialD Single number defining in millimeter, the initial soil water deficit. It is used to start the water balance accounting. Default value is 0, which assumes the root zone is at the field capacity.
* start.date: Date at which the accounting should start. Formats: “YYYY-MM-DD”, “YYYY/MM/DD”.
## Value

* Water balance accounting, including the soil water deficit.
## Examples

```r
data(DataForCWB)
Tavg <- DataForCWB[,2]
Tmax <- DataForCWB[,3]
Tmin <- DataForCWB[,4]
Rn <- DataForCWB[,6]
WS <- DataForCWB[,7]
RH <- DataForCWB[,8]
G <- DataForCWB[,9]
ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
Rain <- DataForCWB[,10]
Drz <- DataForCWB[,11]
AWC <- DataForCWB[,12]
MAD <- DataForCWB[,13]
Kc <- DataForCWB[,14]
Ks <- DataForCWB[,15]
Irrig <- DataForCWB[,16]
CWB(Rain=Rain, ET0=ET0, AWC=AWC, Drz=Drz,
Kc=Kc, Ks=Ks, Irrig=Irrig, MAD=MAD, start.date = "2023-11-23")
```

## Function Water_Stress_Coef()

Calculates the water stress coefficient (Ks).

## Usage 

```r
Water_Stress_Coef(
AWC, 
MAD, 
Drz, 
SoilWaterDeficit
)
```

## Arguments

* AWC: Vector, 1-column matrix or data frame defining the available water capacity of the soil, that is: the amount of water between field capacity and permanent wilting point in millimeters of water per centimeter of soil.
*MAD: Vector, 1-column matrix or data frame defining the management allowed depletion. Varies between 0 and 1, default is 0.3.
* Drz: Vector, 1-column matrix or data frame defining the root zone depth in centimeters.
* SoilWaterDeficit: Vector, 1-column matrix or data frame with soil water deficit in millimeters. It may be provided by the CWB() function.
## Value

Water stress coefficient (Ks).

## Examples

```r
data(DataForCWB)
Tavg <- DataForCWB[,2]
Tmax <- DataForCWB[,3]
Tmin <- DataForCWB[,4]
Rn <- DataForCWB[,6]
WS <- DataForCWB[,7]
RH <- DataForCWB[,8]
G <- DataForCWB[,9]
ET0 <- ET0_PM(Tavg, Tmax, Tmin, Rn, RH, WS,G)
Rain <- DataForCWB[,10]
Drz <- DataForCWB[,11]
AWC <- DataForCWB[,12]
MAD <- DataForCWB[,13]
Kc <- DataForCWB[,14]
Ks <- DataForCWB[,15]
Irrig <- DataForCWB[,16]
CWB.been <- CWB(Rain=Rain, ET0=ET0, AWC=AWC, Drz=Drz,
Kc=Kc, Ks=Ks, Irrig=Irrig, MAD=MAD, start.date = "2023-11-23")
D <- CWB.been$SoilWaterDeficit
Water_Stress_Coef(AWC = AWC,MAD = MAD, Drz = Drz, SoilWaterDeficit = D) 
```

## DataForAWC: Soil texture and plant available water capacity (AWC).

AWC is the amount of water between field capacity and permanent wilting point. Given in millimetre of water per centimeter of soil. Extracted from: Irrigation Scheduling: The Water Balance Approach Fact Sheet No. 4.707 by A. A. Andales, J. L. Chávez, T. A. Bauder..

## Usage

```r
DataForAWC
```

## Format
* Soil Texture Soil Texture
* AWC Low Available water capacity in millimeter of water per centimeter of soil
* AWC High Available water capacity in millimeter of water per centimeter of soil
* AWC Average Available water capacity in millimeter of water per centimeter of soil

## Source

https://extension.colostate.edu/topic-areas/agriculture/.

## Examples

```r
data(DataForAWC)
```

## DataForCWB: Data for Water Balance Accounting.

Daily meteorological data from a weather station in Campinas, Brazil and other parameters required for calculating the crop water balance. The meteorological data belongs to the Agronomic Institute of the state of Sao Paulo.

## Usage 

DataForCWB

## Format

* A data frame with 129 rows and 16 columns.
* date date
* tmed Average air temperature in Celsius degrees
* tmax Maximum air temperature in Celsius degrees
* tmin Minimum air temperature in Celsius degrees
* Ra Extraterrestrial solar radiation in MJ M-2 DAY-1
* Rn Net radiation in MJ M-2 DAY-1
* W Wind speed in M S-1
* RH Relative Humidity in %
* G Soil Heat Flux in MJ M-2 DAY-1
* Rain Rain in millimeters
* Drz Depth of the root zone in centimeters
* AWC available water capacity (amount of water between field capacity and permanent wilting point) in millimeter of water per centimeter of soil
* MAD management allowed depletion (between 0 and 1)
* Kc Crop coefficient (between 0 and 1)
* Ks Water stress coefficient (between 0 and 1)
* Irrig Applied net irrigation in millimeters

## Source

http://www.ciiagro.org.br/.

## Examples

```r
data(DataForCWB)
```

## BugReports: 
<https://github.com/gabrielblain/CropWaterBalance/issues >

## License:

MIT

## Authors: 
Gabriel Constantino Blain, Graciela da Rocha Sobierajski, Regina Célia Matos Pires
Maintainer: Gabriel Constantino Blain, <gabriel.blain@sp.gov.br>

## Acknowledgments: Falar do trabalho da Irrigação.
The package uses data from the Fact Sheet number 4707 Irrigation Scheduling: The Water Balance Approach, by A. A. Andales, J. L. Chávez, and T. A. Bauder.
The authors greatly appreciate this initiative.

## References
Allen, R.G.; Pereira, L.S.; Raes, D.; Smith, M. Crop evapotranspiration. In Guidelines for Computing Crop Water Requirements. Irrigation and Drainage Paper 56; FAO: Rome, Italy, 1998; p. 300.

Andales, A.A.; Chávez, J.L.;Bauder, T.A. Irrigation Scheduling: The Water Balance Approach. Fact Sheet number 4707, crop series | irrigation. <https://extension.colostate.edu/docs/pubs/crops/04707.pdf>

Hargreaves, G.H.; Samani, Z.A. 1985.Reference crop evapotranspiration from temperature. Appl. Eng. Agric,1, 96–99.

Package ‘lubridate', Version 1.9.3, Author Vitalie Spinu et al., https://CRAN.R-project.org/package=lubridate

Package ‘PowerSDI', Version 1.0. 0, Author Gabriel C. Blain et al., https://CRAN.R-project.org/package=PowerSDI

Priestley, C.H.B., Taylor, R.J., 1972. On the Assessment of Surface Heat Flux and Evaporation Using Large-Scale Parameters. Monthly Weather Review, 100 (2), 81–92. https://doi.org/10.1175/1520-0493(1972)100<0081:OTAOSH>2.3.CO;2
