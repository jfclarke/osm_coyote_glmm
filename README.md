# osm_coyote_glmm
This repository contains data, R scripts/outputs and other things needed for the manuscript *Energy infrastructure clears the way for coyotes in Alberta’s oil sands*.

<hr>

### GENERAL INFORMATION

**Project Information:** Information on the Oil Sands Monitoring Program can be found [here](https://open.alberta.ca/publications/9781460151341). Additional information on the Applied Conservation Macro Ecology (ACME) Lab's oil sands monitoring work can be found [here](http://www.acmelab.ca/dataportal.html#Boreal).

**Author Information (data):**  
    Principal Investigators: Dr. Jason T Fisher  
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

**Recommended Citation for Manuscript:** Clarke, J. F., L. Bron, M. Carlson, S. Labiy, Z. Penno, H. Webster, J. T. Fisher, and M. A. Dyck. In prep. *Energy infrastructure clears the way for coyotes in Alberta’s oil sands*.

**Project Funders:** This research was funded by the Oil Sands Monitoring Program. It is a product of the program, but does not necessarily represent its views.

<hr>

### DATA + FILE OVERVIEW

**Files in 'osm_coyote_glmm' Folder:**
		
* <span style = "color: #7B0F17;">**LICENSE**</span>; MIT licensing information for data and code  

* <span style = "color: #7B0F17;">**osm_coyote.Rproj**</span>; R project to run code for data formatting, analysis and visualization

* <span style = "color: #7B0F17;">**README**</span>; this README file with extensions for viewing (.html) and editing (.md)  

**Files in 'data' Folder:**  

*/raw*: contains data aggregated by Marissa Dyck for other OSM projects (see [here](https://github.com/ACMElabUvic/OSM_2022-2023) for more details)

* <span style = "color: #7B0F17;">**OSM_2021_2022_Deployment_Data.csv**</span>; deployment data for all camera sites deployed during 2021-2022 and 2022-2023 sampling periods

* <span style = "color: #7B0F17;">**OSM_2022_total_detections.csv**</span>; total independent detections for all species and landscape units sampled in 2022-2023

* <span style = "color: #7B0F17;">**OSM_covariates_grouped_2021_2022.csv**</span>; cleaned human footprint index and landcover data grouped based on feature type from landscape units sampled in 2021-2022 and 2022-2023

* <span style = "color: #7B0F17;">**OSM_proportional_detections_merged_2021_2022.csv**</span>; proportional monthly presence/absence data for a subset of mammals detected on cameras from landscape units sampled in 2021-2022 and 2022-2023

* <span style = "color: #7B0F17;">**OSM_total_detections_2021.csv**</span>; total independent detections for all species and landscape units sampled in 2021-2022

*/processed*: contains cleaned and reformatted data, created using scripts in this repository

* <span style = "color: #7B0F17;">**coyote_data.csv**</span>; formatted covariate (landcover and linear feature proportions, prey/competitor species total detections) and coyote monthly occurrence data

* <span style = "color: #7B0F17;">**simulation_output.csv**</span>; results from 1,000 iterations of simulations (of top, global model and model selection)

*/spatial*: contains spatial data needed for mapping in R

* <span style = "color: #7B0F17;">**ALL_LUs**</span>; .cpg, .dbf, .prj, .shp and .shx files with spatial information for all landscape units
* <span style = "color: #7B0F17;">**city_points**</span>; .dbf, .prj, .sbn, .sbx, .shp and .shx files with spatial information on Albertan cites - including locations, geometries and names
* <span style = "color: #7B0F17;">**lpr_000b16a_e**</span>; .dbf, .prj, .shp and .shx files with spatial information on Canadian provincial boundaries
* <span style = "color: #7B0F17;">**NRN_AB_14_0_ROADSEG**</span>; .cpg, .dbf, .prj, .sbn, .sbx, .shp and .shx files with spatial information on Albertan road networks
* <span style = "color: #7B0F17;">**OSM_2021_LU2**</span>; .cpg, .dbf, .prj, .shp and .shx files with spatial information on camera trap deployment locations in LU2
* <span style = "color: #7B0F17;">**OSM_2021_LU3**</span>; .cpg, .dbf, .prj, .shp and .shx files with spatial information on camera trap deployment locations in LU3, active from 2021-2022
* <span style = "color: #7B0F17;">**OSM2022_SITES_ALL**</span>; .dbf, .prj, .shp and .shx files with spatial information on camera trap deployment locations for LUs 1, 13, 15 and 21, active from 2022-2023

**Files in 'scripts' Folder:**  

* <span style = "color: #7B0F17;">**coyote_analysis**</span>; .R, .Rmd and .pdf files for GLMMs, data summaries and simulations   
* <span style = "color: #7B0F17;">**coyote_figures**</span>; .R, .Rmd and .pdf files for creating odds ratio, predicted probabilities, variance inflation factor and simulation results plots, an inset study area map
* <span style = "color: #7B0F17;">**coyote_formatting**</span>; .R, .Rmd and .pdf files for data exploration, coyote_data dataframe creation and correlation testing 

**Files in 'figures' Folder:**  

* <span style = "color: #7B0F17;">**covariate_distribution_panel.png**</span>; plot of covariate spread across camera trap sites and LUs
* <span style = "color: #7B0F17;">**ct_op_plot.tiff**</span>; plot of camera trap operability  
* <span style = "color: #7B0F17;">**naive_occupancy.png**</span>; plot of naive coyote occupancy in all six LUs
* <span style = "color: #7B0F17;">**odds_ratio_h.tiff**</span>; plot of odds ratios for global model covariates  
* <span style = "color: #7B0F17;">**odds_ratio_lf.tiff**</span>; plot of odds ratios for wide linear feature model covariates  
* <span style = "color: #7B0F17;">**predicted_probabilities_panel.png**</span>; multi-panel plot of predicted coyote occurrence given global model covariates   
* <span style = "color: #7B0F17;">**re_predicted_probability.png**</span>; plot of predicted coyote occurrence per landscape unit (i.e., random effect), given an example covariate (NOTE: not used in manuscript, not in .Rmd or .pdf files)
* <span style = "color: #7B0F17;">**simulated_parameters_panel.png**</span>; multi-panel plot of simulated beta coefficient values for global model covariates after 1,000 iterations
* <span style = "color: #7B0F17;">**simulated_top_models**</span>; plot of best-performing simulated models after 1,000 iterations
* <span style = "color: #7B0F17;">**study_area_box.png**</span>; map of Canada with Alberta highlighted and a box around the study area
* <span style = "color: #7B0F17;">**study_area.png**</span>; map of LUs and camera trap deployments
* <span style = "color: #7B0F17;">**vif_plot.png**</span>; map of variance inflation factor values for global model covariates    

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

#### DATA-SPECIFIC INFORMATION FOR: [<span style = "color: #7B0F17;">OSM_2021_2022_Deployment_Data.csv</span>]  
Camera trap deployment information for 2021-2023.

* **Number of variables/columns:** 7
* **Number of observations/rows:** 237

Variable list:

* <span style = "color: #002747;">**Project.ID**</span>, landscape unit preceded by 'OSM_'; character
* <span style = "color: #002747;">**Deployment.Location.ID**</span>, composite of LU number and camera station number (e.g., LU3_50); character   
* <span style = "color: #002747;">**Camera.Deployment.Begin.Date**</span>, date camera was deployed; character  
* <span style = "color: #002747;">**Camera.Deployment.End.Date**</span>, date camera failed/died; character
* <span style = "color: #002747;">**Deployment.ID**</span>, unknown; character
* <span style = "color: #002747;">**Months.Deployed**</span>, number of months camera was active; character
* <span style = "color: #002747;">**Camera.Failure.Details**</span>, information about/description of camera failures; character

#### DATA-SPECIFIC INFORMATION FOR: [<span style = "color: #7B0F17;">OSM_2022_total_detections.csv</span>]  
Total independent detections of mammal species for 2022-2023.

* **Number of variables/columns:** 40
* **Number of observations/rows:** 155

Variable list:

* <span style = "color: #002747;">**site**</span>, composite of LU number and camera station number (e.g., LU3_50); character   
* <span style = "color: #002747;">**Black bear**</span>, total number of independent black bear detections at a camera station; numeric  
* <span style = "color: #002747;">**Coyote**</span>, total number of independent coyote detections at a camera station; numeric  
* ...  


#### DATA-SPECIFIC INFORMATION FOR: [<span style = "color: #7B0F17;">OSM_covariates_grouped_2021_2022.csv</span>]  
Proportional landcover and human footprint index information, binned into general categories. More information on the human footprint index - as well as links to download data - can be found [here](https://abmi.ca/abmi-home/data-resources/data-portal-main/data-portal.html?rootPath=/&itemPerPage=6).

* **Number of variables/columns:** 19
* **Number of observations/rows:** 4,660

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
* **Number of observations/rows:** 232

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
* **Number of observations/rows:** 78

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
* **Number of observations/rows:** 233

This .csv contains combined variables from all raw files. Variable descriptions are the same as above, except:

* all variable names are in lowercase
* coyote total independent detections were renamed coyote_tot (from Coyote) for clarity
* coyote monthly presence was renamed coyote_pres (from coyote), and absence renamed coyote_abs (from absent_coyote) for clarity
* natural landcover was grouped into a single variable (called nat_land)
* a new column combining wide linear into a single variable (called wide_linear) was created

#### DATA-SPECIFIC INFORMATION FOR: [<span style = "color: #7B0F17;">simulation_output.csv</span>]  
Results from 1,000 simulation iterations of the global model and model selection.

* **Number of variables/columns:** 4
* **Number of observations/rows:** 11,000

* <span style = "color: #002747;">**simulation**</span>, simulation iteration (number between 1-1,000); numeric
* <span style = "color: #002747;">**Parameter**</span>, name of simulated global model intercept and covariates, and NA associated with top model outcome; character   
* <span style = "color: #002747;">**Estimate**</span>, estimate of simulated global model intercept and covariates, and NA associated with top model outcome; numeric
* <span style = "color: #002747;">**model**</span>, top-performing model from model selection, and NAs associated with intercept/covariate names/estimates; character