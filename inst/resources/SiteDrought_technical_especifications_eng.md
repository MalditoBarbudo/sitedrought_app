---
title: "Technical specifications"
author: "Miquel De Cáceres"
date: "24/10/2022"
output: html_document
---

### Introduction

The application **SiteDrought** provides daily estimates of drought impacts and fire risk in forest and shrubland stands in Catalonia over the last year. This service is designed to provide information that is useful for estimating the daily fire danger in areas of vegetation of interest. The application updates soil moisture and vegetation stress daily from daily meteorological data obtained by interpolation from data from automatic station networks provided by the [*Agencia Estatal de Meteorologia (AEMET)*](http://www.aemet.es) and the [*Servei Meteorologic de Catalunya (SMC)*](http://www.meteo.cat). In order to perform the calculations, daily meteorological data is used as input to a forest water balance model parameterized with edaphic data (derived from global databases) and vegetation.

**SiteDroughtApp** currently provides information for four sets of vegetation plots:

  (a) Plots of the fourth National Forest Inventory (IFN4) in Catalonia.
  (b) Shrubland monitoring plots from the [MatoSeg](https://matoseg.ctfc.es) project, supported by *Fundación Biodiversidad* from *Ministerio para la Transición Ecológica y el Reto Demográfico*.
  (c) Permanent forest plots of the *Aigüestortes i Estany de Sant Maurici* National Park.
  (d) Plots of the National Forest Inventory (IFN3) of the *Ordesa y Monteperdido* National Park.

It is important to remember that the data provided includes calculations based on models, so the resulting values may contain significant differences from actual measurements.

### Data sources

  + **Topography** - The elevation, slope and orientation come from a digital model of elevations at 30 m resolution.
  + **Meteorology** - The interpolations of daily meteorology are made from measurements obtained in meteorological stations of the [*Agencia Estatal de Meteorologia (AEMET)*](http://www.aemet.es) and the [*Servei Meteorologic de Catalunya* (SMC)](http://www.meteo.cat). Since these are automatic stations and the data are downloaded at the end of the current day, these have not passed all the desirable quality controls, which may generate artifacts in the water balance estimates for some areas.
  + **Soils** - Soil texture, bulk density, and organic matter are from the SoilGrids global database at 250 m (Hengl et al. 2017), while bedrock depth is from Shangguan et al. (2017), also at 250 m resolution.
  + **Vegetation** - The sources of vegetation plot data mentioned in the introduction.

### Water balance model

The forest water balance model described in De Cáceres et al. (2015) and De Cáceres et al. (2021). The model updates the soil water content daily based on the structure and composition of the vegetation, the weather and the properties of the soil. The water balance in the soil is the result of the difference between the processes that determine the entry of water (precipitation) and the exit of water (canopy interception, transpiration, evaporation from the soil surface, surface runoff and deep percolation). The model also incorporates routines that allow the potential fire behavior to be calculated given atmospheric and drought conditions (Sánchez-Pinillos et al. 2021; Miezite et al. 2022). The model is implemented in the R language, in the [medfate](https://emf-creaf.github.io/medfate/) package, and detailed documentation on the design and formulation of model equations can be found in the [medfatebook](https://emf-creaf.github.io/medfatebook/)

### Parameterization

Soils are divided into 4 layers \[0-30 cm; 30-100 cm; 100 - SD i SD - 400 cm\] where SD is the depth of the R horizon indicated in Shangguan et al. (2017). The texture, organic matter and apparent density of the different soil layers are obtained from SoilGrids (Hengl et al. 2017). Taking advantage of the fact that the National Forestry Inventory collects estimates of surface rockiness in the plots, this is used to estimate the percentage of rocks in the different soil layers. The forest inventory provides the measured tree diameters and heights, as well as their associated density factors. The leaf biomass of each species is estimated from allometric relationships depending on the diameter and level of competition in the plot (Ameztegui, Cabon et al. 2017), see **Allometr App**. The estimation of leaf biomass in the case of shrubs derives from the heights and coverage percentages measured in the field and also follows the application of allometric relationships (De Cáceres et al. 2019). The specific leaf area of each species allows the leaf biomass to be translated into leaf surface and hence the leaf area index can be estimated. The vertical distribution of the roots in the soil is estimated using the empirical equations obtained by Cabon et al. (2018).

### Daily operations

The calculation engine of the **SiteDrought** is fed by meteorological data downloaded from the SMC and AEMET APIs. This data is downloaded and used to interpolate the current day's weather over the target locality, as described in De Cáceres et al. (2018), and similarly to how the **Meteoland App** works. Once meteorological data is available for a locality, the engine runs the water balance model, taking into account the state of soil moisture from the previous day. The model generates the water flows that make up the water balance in the soil and updates its moisture status. The calculation engine then estimates the drought stress of the vegetation and the moisture content of the live fuel based on the soil moisture status and the hydraulic properties of the different species. The engine also estimates the humidity of the dead fuel, following the approach of Resco De Dios et al. (2015). Finally, all these parameters - as well as the estimates of the surface and overhead fuel load, and the daily wind, temperature and humidity conditions - allow us to estimate two indices of potential fire behavior, which follow the *Fuel Characteristics Classification System* (Prichard et al. 2013).


### Output variables

The application provides information on different variables, grouped by category:

**Climatic variables** - The most relevant meteorological variables for the water balance model are precipitation (whether in liquid or solid form) and potential evapotranspiration:

Variable 	| Definition               | Units 
--------- | ----------------------- | ----- 
Potential evapotranspiration | Daily potential evapotranspiration, calculated from the Penman equation (1956)	| $mm \cdot d^{-1}$
Precipitation	| Daily precipitation (includes rain and snow) | $mm \cdot d^{-1}$

**Drought indicators** - Two variables provide information on edaphic and vegetation drought. On the one hand, the application provides a standardized indicator of the amount of water available in the soil. For the same soil moisture, some species suffer more from drought stress than others. The relative conductance of the plant is a relative measure of reduction in transpiration due to drought stress. The drought stress intensity is defined in the model as the complement of the relative conductance of the plant. 

Variable 	| Definition               | Units 
--------- | ----------------------- | ----- 
Available soil water | Moisture available in the soil, normalized between field capacity (which would correspond to a value of 100%) and the wilting point at -1.5 MPa (which would correspond to a value of 0%). | [0-1] 
Vegetation stress | Weighted average of the daily stress intensity of the different species, using LAI values as weights. | [0-1] 

**Fire risk indicators** -  Another way to express vegetation stress is through live fuel moisture, which depends on leaf structure and the relative water content of plant tissues.

Variable 	| Definition               | Units 
--------- | ----------------------- | ----- 
Live fuel moisture | Moisture content of live fine fuel in relation to dry weight. Weighted average of the different species, using LAI values ​​as weights. | % 
Dead fuel moisture | Moisture content of dead fine fuel in relation to dry weight, estimated following Resco De Dios (2015). | % 
Surface fire potential | Index of surface fire potential behavior, according to a modification of the *Fuel Characteristics Classification System* (FCCS; Prichard et al. 2013). | [0-9]
Crown fire potential | Index of crown fire potential behavior, according to a modification of the *Fuel Characteristics Classification System* (FCCS; Prichard et al. 2013)). | [0-9] 

**Percentiles** - Finally, the application offers the possibility to express some of the above variables in the form of percentiles with respect to a distribution of historical values, in order to represent to what extent given levels of drought or fire risk are extreme. The historical reference period is fixed and has been obtained through simulations for the period 1981-2020 for each plot. The 50% percentile corresponds to the median of the historical distribution; percentiles > 50% would indicate that the variable has higher values than historical ones; similarly, percentiles < 50% would indicate lower values than historical ones. Percentiles are currently provided for the following variables:

Variable 	| Definition               | Unitats 
--------- | ----------------------- | ----- 
Percentile of available soil water | Percentile of the value of water available in the soil in relation to the distribution of values of the historical series (1981-2020) | %
Percentile of vegetation stress | Percentile of the vegetation stress value in relation to the distribution of values of the historical series (1981-2020) | %
Percentile of live fuel moisture | Percentile of the live fuel moisture value relative to the distribution of historical series values (1981-2020)  | %

### Authorship and acknowledgements

The following people have worked in the development of **SiteDrought**:

 + Calculation engine - **Miquel De Cáceres**
 + Database design and development of shiny app - **Oleguer Amorós** i **Victor Granda**
 
The authors express their gratitude towards the [*Agencia Estatal de Meteorologia* (AEMET)](http://www.aemet.es) and the [*Servei Meteorologic de Catalunya (SMC)*](http://www.meteo.cat) for providing daily meteorological data necessary for this application; and to *Servei de Prevenció d'Incendis Forestals* from *Generalitat de Catalunya* for their advice on its development.

### Funding

**MODOSIN** (*Monitoreo y modelización integrados en un sistema de alerta de la vulnerabilidad climática en bosques de montaña*) project has been conducted with support from *Ministerio de Agricultura, Alimentación y Medio Ambiente (Organismo Autónomo de Parques Nacionales)*.

**MatoSeg**  (*Red de detección, predicción y seguimiento de los efectos de la sequía sobre la estructura y biodiversidad de formaciones arbustivas mediterráneas del este peninsular*) project has been conducted thanks to the funding provided by *Fundación Biodiversidad*, from *Ministerio para la Transición Ecológica y el Reto Demográfico* and an agreement with *Servei de Prevenció d’Incendis Forestals* from *Generalitat de Catalunya*.

### References

+ Ameztegui, A., Cabon, A., De Cáceres, M. & Coll, L. (2017). Managing stand density to enhance the adaptability of Scots pine stands to climate change: A modelling approach. Ecol. Modell., 356, 141–150. https://doi.org/10.1016/j.ecolmodel.2017.04.006
+ Cabon A, Martínez-Vilalta J, Martínez-de-Aragón J, De Cáceres M (2018) Applying the eco-hydrological equilibrium hypothesis to model root distribution in water-limited forests. Ecohydrology  11: e2015.  https://doi.org/10.1002/eco.2015
+ De Cáceres, M., Martinez-Vilalta, J., Coll, L., Llorens, P., Casals, P., Poyatos, R., et al. (2015). Coupling a water balance model with forest inventory data to predict drought stress: the role of forest structural changes vs. climate changes. Agricultural and Forest Meteorology, 213, 77–90. https://doi.org/10.1016/j.agrformet.2015.06.012
+ De Cáceres, M., Martin-StPaul, N., Turco, M., Cabon, A., Granda, V., 2018. Estimating daily meteorological data and downscaling climate models over landscapes. Environ. Model. Softw. 108, 186–196. https://doi.org/10.1016/j.envsoft.2018.08.003
+ De Cáceres, M., Casals, P., Gabriel, E., Castro, X., 2019. Scaling-up individual-level allometric equations to predict stand-level fuel loading in Mediterranean shrublands. Ann. For. Sci. 76, 87. https://doi.org/10.1007/s13595-019-0873-4
+ De Cáceres M, Mencuccini M, Martin-StPaul N, Limousin JM, Coll L, Poyatos R, Cabon A, Granda
V, Forner A, Valladares F, Martínez-Vilalta J (2021) Unravelling the effect of species mixing on water
use and drought stress in Mediterranean forests: a modelling approach. Agricultural and Forest
Meteorology 296: 108233. https://doi.org/10.1016/j.agrformet.2020.108233 
+ Hengl, T., Mendes De Jesus, J., Heuvelink, G.B.M., Gonzalez, M.R., Kilibarda, M., Blagotí, A., Shangguan, W., Wright, M.N., Geng, X., Bauer-Marschallinger, B., Guevara, M.A., Vargas, R., Macmillan, R.A., Batjes, N.H., Leenaars, J.G.B., Ribeiro, E., Wheeler, I., Mantel, S., Kempen, B., 2017. SoilGrids250m: Global Gridded Soil Information Based on Machine Learning. PLoS One 12, e0169748. https://doi.org/10.1371/journal.pone.0169748
+ Miezite LE, Ameztegui A, De Cáceres M, Coll L, Morán-Ordóñez A, Vega-García C, Rodrigues M
(2022). Trajectories of wildfire behavior under climate change. Can forest management mitigate the
increasing hazard? Journal of Environmental Management 322: 116134. https://doi.org/10.1016/j.jenvman.2022.116134
+ Penman, H. L. 1956. Evaporation: An introductory survey. Netherlands Journal of Agricultural Science, 4, 9-29.
+ Prichard, S. J., D. V Sandberg, R. D. Ottmar, E. Eberhardt, A. Andreu, P. Eagle, and K. Swedin. (2013). Classification System Version 3.0: Technical Documentation.
+ Resco de Dios, V., A. W. Fellows, R. H. Nolan, M. M. Boer, R. A. Bradstock, F. Domingo, and M. L. Goulden. (2015). A semi-mechanistic model for predicting the moisture content of fine litter. Agricultural and Forest Meteorology 203:64–73. https://doi.org/10.1016/j.agrformet.2015.01.002
+ Sánchez-Pinillos M, De Cáceres M, Casals P, Alvarez A, Beltrán M, Pausas JG, Vayreda J, Coll L.
(2021). Spatial and temporal variations of overstory and understory fuels in Mediterranean landscapes.
Forest Ecology and Management 490: 119094. https://doi.org/10.1016/j.foreco.2021.119094
+ Shangguan, W., Hengl, T., Mendes de Jesus, J., Yuan, H., Dai, Y., 2017. Mapping the global depth to bedrock for land surface modeling. J. Adv. Model. Earth Syst. 9, 65–88. https://doi.org/10.1002/2016MS000686
