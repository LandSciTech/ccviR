
*Running this app is only necessary if you want to run the CCVI app with custom climate data. To use the pre-prepared climate data download a [zip file](https://drive.google.com/drive/folders/18mO5GDUmwi-nswhIAC36bmtYsvmqNQkH?usp=share_link) unzip it and save the folder in a convenient location.*

 <span style='color:red'>*</span> Indicates Required Data and Inputs</span>

#### Step 1: Acquire climate data

**Historical data**: 
  - Mean annual temperature (MAT) <span style='color:red'>*</span>
  - Climate moisture deficit (CMD) <span style='color:red'>*</span>
  - Mean annual precipitation (MAP)
  - Minimum coldest and maximum warmest month temperatures (MCMT, MWMT)
  - Keep notes on the sources and year range of the historical period
  
**Future scenario data**:
  - Mean annual temperature (MAT) <span style='color:red'>*</span>
  - Climate moisture deficit (CMD) <span style='color:red'>*</span>
  - Keep notes on the sources, scenarios, and year range of the future period
  - Pick a short and descriptive title for a 'Scenario Name'

See the [tutorial](https://landscitech.github.io/ccviR/articles/data_prep_vignette.html) for recommended data sources.
  
**Supporting data**: 
To calculate a migratory exposure index you'll need a Climate Change Exposure Index (CCEI) raster for the migratory region. You can obtain one from [NatureServe](https://www.natureserve.org/ccvi-species) for Central and South America or use the [one included with ccviR](https://drive.google.com/drive/folders/175eBmxN1KIcqVvHLC3PcJIvvUP67BzPG) which covers all the Americas. 

Save all downloaded data in a folder you can easily find.


#### Step 2: Prepare the climate data for use in the app

Climate data can be added by selecting file paths for each file. 
For the output folder make sure to choose a location that is easy to find again 
because you will use the prepared climate data to calculate the index.
