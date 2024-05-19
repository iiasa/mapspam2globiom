
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mapspam2globiom

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The aim of the [mapspam2globiom R
package](https://iiasa.github.io/mapspam2globiom) is to facilitate the
creation of country level crop distribution maps, which can be used as
input by the IIASA’s \[Global Biosphere Management Model (GLOBIOM)\].
GLOBIOM is a spatially explicit partial equilibrium model that is used
to analyze the competition for land use between agriculture, forestry,
and bioenergy. The model can be used for global and national level
analysis (Valin et al. 2013; Leclère et al. 2014; Havlik et al. 2014).
In the latter case, model output can be greatly improved by
incorporating regionally specific information that is often provided by
local stakeholders or can be taken from national statistics. Information
on crop cover and the location of crops are a key driver of the model
and it is therefore desirable to base these as much as possible on
national sources of information.

mapspam2globiom includes a single function to aggregate crop
distribution maps that were created with [mapspamc
package](https://michielvandijk.github.io/mapspamc/) to the GLOBIOM
input format. mapspamc (Dijk et al. 2023) was specifically developed to
create national crop distribution maps using the Spatial Production
Allocation Model (You and Wood 2006; You, Wood, and Wood-Sichra 2009;
You et al. 2014; Yu et al. 2020). mapspam2globiom aggregates mapspamc
output along two dimensions: (a) it aggregates raster cells to GLOBIOM
simulation units (simu), which are clusters of grid cells with similar
bio-physical characteristics and (b) it aggregates the 40+ mapspamc
crops to the 18 crop groups in GLOBIOM.

## Installation

To install mapspam2globiom:

``` r
install.packages("remotes")
remotes::install_github("iiasa/mapspam2globiom")
```

Apart from the mapspam2globiom package, several other pieces of software
are essential to run `mapspam2globiom`, which are described in the
[Installation](articles/installation.html) section.

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-VanDijk2023" class="csl-entry">

Dijk, Michiel van, Ulrike Wood-Sichra, Yating Ru, Zhe Guo, and Liangzhi
You. 2023. “<span class="nocase">mapspamc: An R package to create crop
distribution maps for country studies using a downscaling
approach</span>.” Preprint.
<https://doi.org/10.21203/rs.3.rs-2497136/v1>.

</div>

<div id="ref-Havlik2014" class="csl-entry">

Havlik, Petr, Hugo Valin, Mario Herrero, Michael Obersteiner, Erwin
Schmid, Mariana C Rufino, Aline Mosnier, et al. 2014.
“<span class="nocase">Climate change mitigation through livestock system
transitions.</span>” *Proceedings of the National Academy of Sciences of
the United States of America* 111 (10): 3709–14.
<https://doi.org/10.1073/pnas.1308044111>.

</div>

<div id="ref-Leclere2014" class="csl-entry">

Leclère, D, Petr Havlik, S Fuss, E Schmid, A Mosnier, B Walsh, H Valin,
Mario Herrero, N Khabarov, and M Obersteiner. 2014.
“<span class="nocase">Climate change induced transformations of
agricultural systems: insights from a global model</span>.”
*Environmental Research Letters* 9 (12): 124018.
<https://doi.org/10.1088/1748-9326/9/12/124018>.

</div>

<div id="ref-Valin2013b" class="csl-entry">

Valin, H, Petr Havlik, A Mosnier, Mario Herrero, E Schmid, and M
Obersteiner. 2013. “<span class="nocase">Agricultural productivity and
greenhouse gas emissions: Trade-offs or synergies between mitigation and
food security?</span>” *Environmental Research Letters* 8 (3): 1–9.
<https://doi.org/10.1088/1748-9326/8/3/035019>.

</div>

<div id="ref-You2006" class="csl-entry">

You, Liangzhi, and Stanley Wood. 2006. “<span class="nocase">An entropy
approach to spatial disaggregation of agricultural production</span>.”
*Agricultural Systems* 90 (1): 329–47.
<https://doi.org/10.1016/j.agsy.2006.01.008>.

</div>

<div id="ref-You2009" class="csl-entry">

You, Liangzhi, Stanley Wood, and Ulrike Wood-Sichra. 2009.
“<span class="nocase">Generating plausible crop distribution maps for
Sub-Saharan Africa using a spatially disaggregated data fusion and
optimization approach</span>.” *Agricultural Systems* 99 (2): 126–40.
<https://doi.org/10.1016/j.agsy.2008.11.003>.

</div>

<div id="ref-You2014a" class="csl-entry">

You, Liangzhi, Stanley Wood, Ulrike Wood-Sichra, and Wenbin Wu. 2014.
“<span class="nocase">Generating global crop distribution maps: From
census to grid</span>.” *Agricultural Systems* 127: 53–60.
<https://doi.org/10.1016/j.agsy.2014.01.002>.

</div>

<div id="ref-Yu2020" class="csl-entry">

Yu, Qiangyi, Liangzhi You, Ulrike Wood-Sichra, Yating Ru, Alison K. B.
Joglekar, Steffen Fritz, Wei Xiong, Miao Lu, Wenbin Wu, and Peng Yang.
2020. “<span class="nocase">A cultivated planet in 2010 – Part 2: The
global gridded agricultural-production maps</span>.” *Earth System
Science Data* 12 (4): 3545–72.
<https://doi.org/10.5194/essd-12-3545-2020>.

</div>

</div>
