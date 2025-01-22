---
output:
  pdf_document: default
  html_document: default
---
# osm_coyote_glmm
This repository contains data, R scripts/outputs and other things needed for the manuscript *Energy infrastructure clears the way for coyotes in Alberta’s oil sands*.

<hr>

### GENERAL INFORMATION

**Project Information:** Information on the Oil Sands Monitoring Program can be found [here](https://open.alberta.ca/publications/9781460151341). Additional information on the Applied Conservation Macro Ecology (ACME) Lab's oil sands monitoring work can be found [here](http://www.acmelab.ca/dataportal.html#Boreal).

**Author Information (data):**  
    Principal Investigator: Dr. Jason T Fisher  
		Institution: University of Victoria  
		Address: 3800 Finnerty Rd, Victoria, BC V8P 5C2  
		Email: [fisherj@uvic.ca](mailto::email)

**Author Information (code):** 		
		Data Analysis Contact: Jamie Clarke  
		Institution: University of Victoria  
		Address: 3800 Finnerty Rd, Victoria, BC V8P 5C2  
		Email: [jamiefclarke@gmail.com](mailto::email)


**Date of Data Collection:** 2021-2023

**Location of Data Collection:** Alberta, Canada

<hr>

### SHARING/ACCESS INFORMATION

**Recommended Citation for Manuscript:** Clarke, J., M. A. Dyck, L. Bron, M. Carlson, S. Labiy, Z. Penno, H. Webster, and J. T. Fisher. In prep. *Energy infrastructure clears the way for coyotes in Alberta’s oil sands*.

**Project Funders:** This research was funded by the Oil Sands Monitoring Program. It was a product of the program, but does not necessarily represent its views.

<hr>

### DATA + FILE OVERVIEW

**Files in 'osm_coyote_glmm' Folder:**
		
* <span style = "color: #7B0F17;">**LICENSE**</span>; MIT licensing information for data and code  

* <span style = "color: #7B0F17;">**osm_coyote.Rproj**</span>; R project to run code for data formatting, analysis and visualization

* <span style = "color: #7B0F17;">**README**</span>; this README file with extensions for viewing (.html) and editing (.md)  

**Files in 'data' Folder:**  

*/raw*: contains data aggregated by Marissa Dyck for other OSM projects (see [here](https://github.com/ACMElabUvic/OSM_2022-2023) for more details)

* <span style = "color: #7B0F17;">**OSM_2022_total_detections.csv**</span>; total independent detections for all species and landscape units sampled in 2022-2023

* <span style = "color: #7B0F17;">**OSM_covariates_grouped_2021_2022.csv**</span>; cleaned human footprint index and landcover data grouped based on feature type from landscape units sampled in 2021-2022 and 2022-2023

* <span style = "color: #7B0F17;">**OSM_proportional_detections_merged_2021_2022.csv**</span>; proportional monthly presence/absence data for a subset of mammals detected on cameras from landscape units sampled in 2021-2022 and 2022-2023

* <span style = "color: #7B0F17;">**OSM_total_detections_2021.csv**</span>; total independent detections for all species and landscape units sampled in 2021-2022

*/processed*: contains cleaned and reformatted data, created using scripts in this repository

* <span style = "color: #7B0F17;">**coyote_data.csv**</span>; formatted covariate (landcover and linear feature proportions, prey/competitor species total detections) and coyote monthly occurrence data

*/spatial*: contains spatial data needed for mapping in R

* <span style = "color: #7B0F17;">**ALL_LUs**</span>; .cpg, .dbf, .prj, .shp and .shx files with spatial information for all landscape units
* <span style = "color: #7B0F17;">**city_points**</span>; .dbf, .prj, .sbn, .sbx, .shp and .shx files with spatial information on Albertan cites - including locations, geometries and names
* <span style = "color: #7B0F17;">**lpr_000b16a_e**</span>; .dbf, .prj, .shp and .shx files with spatial information on Canadian provincial boundaries
* <span style = "color: #7B0F17;">**NRN_AB_14_0_ROADSEG**</span>; .cpg, .dbf, .prj, .sbn, .sbx, .shp and .shx files with spatial information on Albertan road networks
* <span style = "color: #7B0F17;">**OSM_2021_LU2**</span>; .cpg, .dbf, .prj, .shp and .shx files with spatial information on camera trap deployment locations in LU2
* <span style = "color: #7B0F17;">**OSM_2021_LU3**</span>; .cpg, .dbf, .prj, .shp and .shx files with spatial information on camera trap deployment locations in LU3, active from 2021-2022
* <span style = "color: #7B0F17;">**OSM2022_SITES_ALL**</span>; .dbf, .prj, .shp and .shx files with spatial information on camera trap deployment locations for LUs 1, 13, 15 and 21, active from 2022-2023

**Files in 'scripts' Folder:**  

* <span style = "color: #7B0F17;">**coyote_analysis**</span>; .R, .Rmd and .html files for GLMMs and data summaries   
* <span style = "color: #7B0F17;">**coyote_formatting**</span>; .R, .Rmd and .html files for data exploration, coyote_data dataframe creation and correlation testing 
* <span style = "color: #7B0F17;">**coyote_figures**</span>; .R, .Rmd and .html files for creating odds ratio, predicted probabilities and variance inflation factor plots, and an inset study area map

**Files in 'figures' Folder:**  

* <span style = "color: #7B0F17;">**odds_ratio_plot.tiff**</span>; plot of odds ratios for all top model covariates  
* <span style = "color: #7B0F17;">**predicted_probabilities_panel.png**</span>; muti-panel plot showing predicted coyote occurrence given top model covariates   
* <span style = "color: #7B0F17;">**re_predicted_probability.png**</span>; plot of predicted coyote occurrence per landscape unit (i.e., random effect), given an example covariate   
* <span style = "color: #7B0F17;">**study_area_map.png**</span>; map of landscape units and camera trap deployment locations in the oil sands region, with inset map of study area within Canada  
* <span style = "color: #7B0F17;">**vif_plot.png**</span>; map of variance inflation factor values for all top model covariates    

<hr>

### METHODOLOGICAL INFORMATION

**Description of Methods:**
A description of data collection and processing methods can be found in the manuscript.  

**Software Needed:**
Data was formatted, analyzed and plotted in R version 4.3.2. 

* Download R <a href = "https://cran.r-project.org/bin/windows/" target = "_blank">[Windows link]</a> <a href = "https://cran.r-project.org/bin/macosx/" target = "_blank">[Mac link]</a>
* Downlad R Studio <a href = "https://www.rstudio.com/products/rstudio/" target = "_blank">[link]</a> 

**Data Collection and Processing:**
Many people were involved in the collection and processing of these camera trap images. We are grateful to Sandra Frey, Macgregor Aubertin-Young, Laura Eliuk, Andrew Barnas, Millicent Gaston, Rebecca Smith, Shay Marks, Emerald Arthurs and Megan Braun for their dedication, field prowess and keen image-tagging eyes.
 
<hr>

### RAW DATA  

#### DATA-SPECIFIC INFORMATION FOR: [<span style = "color: #7B0F17;">OSM_2022_total_detections.csv</span>]  
Total independent detections of mammal species for 2022-2023.

* **Number of variables/columns:** 40
*  **Number of observations/rows:** 155

Variable list:

* <span style = "color: #002747;">**site**</span>, composite of LU number and camera station number (e.g., LU3_50); character   
* <span style = "color: #002747;">**Black bear**</span>, total number of independent black bear detections at a camera station; numeric  
* <span style = "color: #002747;">**Coyote**</span>, total number of independent coyote detections at a camera station; numeric  
* ...  


#### DATA-SPECIFIC INFORMATION FOR: [<span style = "color: #7B0F17;">OSM_covariates_grouped_2021_2022.csv</span>]  
Proportional landcover and human footprint index information, binned into general categories.

* **Number of variables/columns:** 19
*  **Number of observations/rows:** 4,660

Variable list:

* <span style = "color: #002747;">**array**</span>, landscape unit (LU) number; character  
* <span style = "color: #002747;">**site**</span>, composite of LU number and camera station number (e.g., LU3_50); character   
* <span style = "color: #002747;">**buff_dist**</span>, measurement of the buffer radius around a camera trap - for which the proportion of associated human factors variables were calculated - ranging from 250 to 5,000 m; numeric  
* <span style = "color: #002747;">**harvest**</span>, proportion of land harvested for timber (e.g., clear-cut, selective harvest, salvage logging) within the buffer radius; numeric
* <span style = "color: #002747;">**pipeline**</span>, proportion of pipelines within the buffer radius; numeric  
* <span style = "color: #002747;">**roads**</span>, proportion of roads (paved and unpaved) within the buffer radius; numeric  
* <span style = "color: #002747;">**seismic_lines**</span>, proportion of conventional seismic lines within the buffer radius; numeric  
* <span style = "color: #002747;">**seismic_lines_3D**</span>, proportion of 3D (or 'low-impact') seismic lines within the buffer radius; numeric  
* <span style = "color: #002747;">**trails**</span>, proportion of human and vehicle trails within the buffer radius; numeric  
* <span style = "color: #002747;">**transmission_lines**</span>, proportion of power transmission line infrastructure within the buffer radius; numeric  
* <span style = "color: #002747;">**veg_edges**</span>, proportion of disturbed vegetatin along roads, railways and other industrial features within the buffer radius; numeric  
* <span style = "color: #002747;">**wells**</span>, proportion of land cleared for oil and gas wells within the buffer radius; numeric  
* <span style = "color: #002747;">**lc_grassland**</span>, proportion of grassland within the buffer radius; numeric  
* <span style = "color: #002747;">**lc_coniferous**</span>, proportion of coniferous forest within the buffer radius; numeric  
* <span style = "color: #002747;">**lc_broadleaf**</span>, proportion of broadleaf forest within the buffer radius; numeric  
* <span style = "color: #002747;">**lc_mixed**</span>, proportion of mixed forest within the buffer radius; numeric  
* <span style = "color: #002747;">**lc_developed**</span>, proportion of developed land within the buffer radius; numeric  
* <span style = "color: #002747;">**lc_shrub**</span>, proportion of shrubland within the buffer radius; numeric  
* <span style = "color: #002747;">**osm_industrial**</span>, proportion of industrially-developed land within the buffer radius; numeric  

#### DATA-SPECIFIC INFORMATION FOR: [<span style = "color: #7B0F17;">OSM_proportional_detections_merged_2021_2022.csv</span>]
Monthly proportional detection data for mammal species of interest for all deployment years (2021-2022 and 2022-2023).

* **Number of variables/columns:** 25
*  **Number of observations/rows:** 232

Variable list:

* <span style = "color: #002747;">**site**</span>, composite of LU number and camera station number (e.g., LU3_50); character   
* <span style = "color: #002747;">**black_bear**</span>, number of months black bears were present (i.e., detected) at a camera station; numeric  
* <span style = "color: #002747;">**coyote**</span>, number of months coyotes were present (i.e., detected) at a camera station; numeric  
* ...  
* <span style = "color: #002747;">**absent_black_bear**</span>, number of months black bears were absent (i.e., not detected) at a camera station; numeric
* <span style = "color: #002747;">**absent_coyote**</span>, number of months coyotes were absent (i.e., not detected) at a camera station; numeric
* ...  

#### DATA-SPECIFIC INFORMATION FOR: [<span style = "color: #7B0F17;">OSM_total_detections_2021.csv</span>]
Total independent detections of mammal species for 2021-2022.

* **Number of variables/columns:** 36
*  **Number of observations/rows:** 78

Variable list:

* <span style = "color: #002747;">**site**</span>, composite of LU number and camera station number (e.g., LU3_50); character   
* <span style = "color: #002747;">**Black bear**</span>, total number of independent black bear detections at a camera station; numeric  
* <span style = "color: #002747;">**Coyote**</span>, total number of independent coyote detections at a camera station; numeric  
* ...  

<hr>

### PROCESSED DATA  

#### DATA-SPECIFIC INFORMATION FOR: [<span style = "color: #7B0F17;">coyote_data.csv</span>]  
Merged, cleaned and formatted dataset for analyses. All landcover and human footprint index proportions are for a buffer radius of 4,750 m. Only landcover/human footprint indices and mammal species of interest are included.

* **Number of variables/columns:** 20
*  **Number of observations/rows:** 233

This .csv contains combined variables from all raw files. Variable descriptions are the same as above, except:

* all variable names are in lowercase
* coyote total independent detections were renamed coyote_tot (from Coyote) for clarity
* coyote monthly presence was renamed coyote_pres (from coyote), and absence renamed coyote_abs (from absent_coyote) for clarity
* natural landcover was grouped into a single variable (called nat_land)
* a new column combining wide linear into a single variable (called wide_linear) was created
