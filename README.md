
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mapspam2globiom

<!-- badges: start -->

<!-- badges: end -->

The aim of the mapspam2globiom R package is to facilitate the creation
of country level crop distribution maps, which can be used as input by
the IIASA’s [Global Biosphere Management Model
(GLOBIOM)](https://www.globiom.org/). GLOBIOM is a spatially explicit
partial equilibrium model that is used to analyze the competition for
land use between agriculture, forestry, and bioenergy. The model can be
used for global and national level analysis
(<span class="citeproc-not-found" data-reference-id="ADD">**???**</span>).
In the latter case, model output can be greatly improved by
incorporating regionally specific information that is often provided by
local stakeholders or can be taken from national statistics. Information
on crop cover and the location of crops are a key driver of the model
and it is therefore desirable to base these as much as possible on
national sources of information.

mapspam2globiom provides the necessary infrastructure to run the Spatial
Production Allocation Model for Country level studies (SPAMc). The model
builds on the global version of SPAM (You and Wood 2006; You, Wood, and
Wood-Sichra 2009; You et al. 2014; Yu et al. 2020), which uses an
cross-entropy optimization approach to ‘pixelate’ national and
subnational crop statistics on a spatial grid at a 5 arcmin resolution.
SPAMc was specifically developed to support country level analysis and
makes it possible incorporate national sources of information and create
maps at a higher resolution of 30 arcsec.
(<span class="citeproc-not-found" data-reference-id="VanDijk2020">**???**</span>)
provides an in-depth discussion of SPAMc including an example for
Southern Africa.\[1\]

Apart from implementing SPAMc, mapspam2globiom includes functions to
aggregate the SPAMc output to the spatial (i.e. simulation units) and
crop-level (18 major crops) format that is used by GLOBIOM.

## Installation

To install mapspam2globiom:

``` r
 install.packages("remotes")
remotes::install_github("iiasa/globiomvis")
```

## References

<div id="refs" class="references">

<div id="ref-You2006">

You, Liangzhi, and Stanley Wood. 2006. “An entropy approach to spatial
disaggregation of agricultural production.” *Agricultural Systems* 90
(1): 329–47. <https://doi.org/10.1016/j.agsy.2006.01.008>.

</div>

<div id="ref-You2009">

You, Liangzhi, Stanley Wood, and Ulrike Wood-Sichra. 2009. “Generating
plausible crop distribution maps for Sub-Saharan Africa using a
spatially disaggregated data fusion and optimization approach.”
*Agricultural Systems* 99 (2): 126–40.
<https://doi.org/10.1016/j.agsy.2008.11.003>.

</div>

<div id="ref-You2014a">

You, Liangzhi, Stanley Wood, Ulrike Wood-Sichra, and Wenbin Wu. 2014.
“Generating global crop distribution maps: From census to grid.”
*Agricultural Systems* 127: 53–60.
<https://doi.org/10.1016/j.agsy.2014.01.002>.

</div>

<div id="ref-Yu2020">

Yu, Qiangyi, Liangzhi You, Ulrike Wood-Sichra, Yating Ru, Alison K. B.
Joglekar, Steffen Fritz, Wei Xiong, Wenbin Wu, and Peng Yang. 2020. “A
cultivated planet in 2010: 2. the global gridded agricultural production
maps.” *Earth System Science Data*.
<https://doi.org/https://doi.org/10.5194/essd-2020-11>.

</div>

</div>

1.  Note that the SPAMc version in mapspam2globiom is simpler version
    than the one described in
    (<span class="citeproc-not-found" data-reference-id="VanDijk2020">**???**</span>)
    as it is not possible to blend in detailed information on the
    location of crops (e.g. OpenStreetMap information, large-farm
    surveys and machine learning products). It also does not include the
    approach to backcast crop distribution maps to earlier periods.
    These features might be added later.
