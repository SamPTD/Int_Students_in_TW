**Project Title**

Exploring the landscape of international students in Taiwan's higher-education

**Project Description**

Over the last decade, Taiwan has welcomed students from almost all countries and territories in the world (177 out of 195). There have been more than 4,000 courses for international students taught in over 170 universities all across the island. A deeper look into the picture will reveal nuanced clarity such as heavy focus on particular regions and countries, the impact and disproportionate distribution of the Industry-School Cooperation program, and the gender ratio of trending disciplines.

Insights about the international students' choices can help policy makers assess the efficiency of policies and measures for attracting foreign students, in particular ones that are related to the Industry-School Cooperation program. It can help policy makers develop better policies to attract students from specific regions and countries, as well as to improve gender ratio for targeted fields.

The project analyzes data downloaded from the Directorate-General of Budget, Accounting and Statistics (DGBAS) of Taiwan. The range is from the year 2015 to 2023. The format is CSV. Each file contains information including schools, departments, countries of origins, gender.

There are three different datasets from the DGBAS website: (D1) for foreign students, (D2) for overseas Chinese and Hong Kong, Macau students, and (D3) for mainland Chinese students.

Since there were changes in the academic disciplines, we use the Ministry of Education's 4th and 5th Edition Classification Standards for Academic Disciplines to reclassify the disciplines.

**Getting Started**

There are three steps for data processing: clean data, merge data and map disciplines. The process goes as follow:



* Align column names and ensure consistent columns for sources D1, D2, and D3.
* Merge graduates and current students for source D3.
* Merge data from sources D1 and D2.
* Merge the combined dataset with source D3.
* Create a "Discipline" column and use the 4th and 5th Edition standards to map major codes.
* Map 2015 majors (4th edition) to 2016-2023 disciplines (5th edition) after verifying changes.

The project is run on R Studio which can be downloaded here: [https://posit.co/downloads/](https://posit.co/downloads/).

The following R packages are required to run analysis: dplyr, readr, ggplot2, stringr, showtext, cowplot, tidyr, forcats, sf, rnaturalearth, rnaturalearthdata, ggrepel.

**File Structure**

There are 4 folders 

[Describe the file structure of your project, including how the files are organized and what each file contains. Be sure to explain the purpose of each file and how they are related to one another.]

**Analysis**



<p id="gdcalert1" ><span style="color: red; font-weight: bold">>>>>>  gd2md-html alert: inline image link here (to images/image1.png). Store image on your image server and adjust path/filename/extension if necessary. </span><br>(<a href="#">Back to top</a>)(<a href="#gdcalert2">Next alert</a>)<br><span style="color: red; font-weight: bold">>>>>> </span></p>


![alt_text](images/image1.png "image_tooltip")


[Describe your analysis methods and include any visualizations or graphics that you used to present your findings. Explain the insights that you gained from your analysis and how they relate to your research question or problem statement.]

**Results**

[Provide a summary of your findings and conclusions, including any recommendations or implications for future research. Be sure to explain how your results address your research question or problem statement.]

**Contributors**

[List the contributors to your project and describe their roles and responsibilities.]

**Acknowledgments**

[Thank any individuals or organizations who provided support or assistance during your project, including funding sources or data providers.]

**References**

[List any references or resources that you used during your project, including data sources, analytical methods, and tools.]

 

From &lt;[https://github.com/advapplab/ici_template](https://github.com/advapplab/ici_template)> 
