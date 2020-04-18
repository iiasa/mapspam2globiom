
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
used for global and national level analysis (Valin et al. 2013; Leclère
et al. 2014; Havlik et al. 2014; Soterroni et al. 2018). In the latter
case, model output can be greatly improved by incorporating regionally
specific information that is often provided by local stakeholders or can
be taken from national statistics. Information on crop cover and the
location of crops are a key driver of the model and it is therefore
desirable to base these as much as possible on national sources of
information.

mapspam2globiom provides the necessary infrastructure to run the Spatial
Production Allocation Model for Country level studies (SPAMc). The model
builds on the global version of SPAM (You and Wood 2006; You, Wood, and
Wood-Sichra 2009; You et al. 2014; Yu et al. 2020), which use an
cross-entropy optimization approach to ‘pixelate’ national and
subnational crop statistics on a spatial grid at a 5 arcmin resolution.
SPAMc was specifically developed to support country level analysis and
makes it possible to incorporate national sources of information and
create maps at a higher resolution of 30 arcsec (Van Dijk et al. 2020).
The articles in the Background section provide more information on the
model, including [model structure](articles/model_structure.html),
overview of [input data](articles/data.html) and an
[appendix](articles/appendix.html) with the equations. Apart from
implementing SPAMc, mapspam2globiom includes functions to aggregate the
SPAMc output to the spatial (i.e. simulation units) and crop-level (18
major crops) format that is used by GLOBIOM.

## Installation

To install mapspam2globiom:

``` r
install.packages("remotes")
remotes::install_github("iiasa/mapspam2globiom")
```

Apart from the mapspam2globiom package, several other pieces of software
are essential to run SPAMc, which are described in the
[Installation](articles/software.html) section.

## Preparation

It takes some preparation before SPAMc can be run. Most important and
propbably most time consuming is the collection of input data. SPAMc is
a data-driven model and therefore requires a large variety of input
data, which can be grouped under two headers: (1) (Sub)national
agricultural statistics and (2) spatial information. The availability of
data strongly affects the structure of the model and how can be solved.
Hence, we highly recommend to start collecting input data before running
the model. The articles in the Preparation section give an overview of
all the information that is requited by SPAMc and shows were to download
templates/examples that can be used as a framework to apply SPAMc to
other countries:

  - [Input data collection](articles/collect_input_data.html).
  - [Download templates](articles/template.html)

## Run SPAMc

Running SPAMc can be divided into seven steps, which are described in
the articles in the Run SPAMc section:

  - [Model setup](articles/model_structure.html)
  - [Processing of subnational
    statistics](articles/process_subnat_stat.html)
  - [Processing of spatial data](articles/process_spatial_data.html)
  - Create cropland extent
  - Create irrigated area extend
  - [Run model](articles/run_model.html)
  - [Post-processing](articles/post_process.html)

## References

<div id="refs" class="references">

<div id="ref-Havlik2014">

Havlik, Petr, Hugo Valin, Mario Herrero, Michael Obersteiner, Erwin
Schmid, Mariana C Rufino, Aline Mosnier, et al. 2014. “Climate change
mitigation through livestock system transitions.” *Proceedings of the
National Academy of Sciences of the United States of America* 111 (10):
3709–14. <https://doi.org/10.1073/pnas.1308044111>.

</div>

<div id="ref-Leclere2014">

Leclère, D, Petr Havlik, S Fuss, E Schmid, A Mosnier, B Walsh, H Valin,
Mario Herrero, N Khabarov, and M Obersteiner. 2014. “Climate change
induced transformations of agricultural systems: insights from a global
model.” *Environmental Research Letters* 9 (12): 124018.
<https://doi.org/10.1088/1748-9326/9/12/124018>.

</div>

<div id="ref-Soterroni2018">

Soterroni, Aline C, Aline Mosnier, Alexandre X Y Carvalho, Gilberto
Câmara, Michael Obersteiner, Pedro R Andrade, Ricardo C Souza, et al.
2018. “Future environmental and agricultural impacts of Brazil’s Forest
Code.” *Environmental Research Letters* 13 (7): 074021.
<https://doi.org/10.1088/1748-9326/aaccbb>.

</div>

<div id="ref-Valin2013b">

Valin, H, Petr Havlik, A Mosnier, Mario Herrero, E Schmid, and M
Obersteiner. 2013. “Agricultural productivity and greenhouse gas
emissions: Trade-offs or synergies between mitigation and food
security?” *Environmental Research Letters* 8 (3): 1–9.
<https://doi.org/10.1088/1748-9326/8/3/035019>.

</div>

<div id="ref-VanDijk2020">

Van Dijk, Michiel, Ulrike Wood-Sichra, Yating Ru, Amanda Palazzo, Petr
Havlik, and Liangzhi You. 2020. “Mapping the change in crop distribution
over time using a data fusion approach.”

</div>

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
