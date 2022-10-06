# R-to-FVS
This code takes any tree list from R and creates the FVS-ready input database. It produces an Excel file with FVS_StandInit, FVS_PlotInit, and FVS_TreeInit tabs in the same format as the [Blank FVS Database found here](https://www.fs.usda.gov/fvs/software/data.shtml), which can be read into FVS. 

At a minimum you need to supply a tree list file with these variables:

* **STAND**, a unique stand identifier code,
* **PLOT**, a unique plot identifier code,
* **TREE**, a unique tree identifier code,
* **YEAR**, year the tree was inventoried,
* **SPP**, FVS Species code, according to the [variant being used](https://www.fs.usda.gov/fvs/software/variantkey.shtml),
* **DBH**, tree diameter at breast height (inches)

You will also need to specify general information about the model variant and forest:

* **VARIANT**, Two-letter FVSvariant code: [see here](https://www.fs.usda.gov/fvs/software/variantkey.shtml)	
* **LOCATION**, Three-digit forest locaton code: [see here](https://www.fs.usda.gov/fvs/software/variantkey.shtml)	
* **SITE_SPECIES**, the FVS species code for site index species
* **BASAL_AREA_FACTOR**, for a positive value = basal area factor. Negative value = inverse of a large-tree fixed area plot. 
* *or* 
**INV_PLOT_SIZE**, Fixed-radius plot expansion factor