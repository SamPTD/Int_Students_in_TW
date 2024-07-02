# **Project Title**

Exploring the landscape of international students in Taiwan's higher-education


# **Project Description**

Over the last decade, Taiwan has welcomed students from almost all countries and territories in the world (177 out of 195). There have been more than 4,000 courses for international students taught in over 170 universities all across the island. A deeper look into the picture will reveal nuanced clarity such as heavy focus on particular regions and countries, the impact and disproportionate distribution of the Industry-School Cooperation program, and the gender ratio of trending disciplines.

Insights about the international students' choices can help policy makers assess the efficiency of policies and measures for attracting foreign students, in particular ones that are related to the Industry-School Cooperation program. It can help policy makers develop better policies to attract students from specific regions and countries, as well as to improve gender ratio for targeted fields.

The project analyzes data downloaded from the Directorate-General of Budget, Accounting and Statistics (DGBAS) of Taiwan. The range is from the year 2015 to 2023. The format is CSV. Each file contains information including schools, departments, countries of origins, gender.

There are three different datasets from the DGBAS website: (D1) for foreign students, (D2) for overseas Chinese and Hong Kong, Macau students, and (D3) for mainland Chinese students.

Since there were changes in the academic disciplines, we use Taiwan’s Ministry of Education's 4th and 5th Edition Classification Standards for Academic Disciplines to reclassify the disciplines.


# **Getting Started**

There are three steps for data processing: clean data, merge data and map disciplines. The process goes as follow:



* Align column names and ensure consistent columns for sources D1, D2, and D3.
* Merge graduates and current students for source D3.
* Merge data from sources D1 and D2.
* Merge the combined dataset with source D3.
* Create a "Discipline" column and use the 4th and 5th Edition standards to map major codes.
* Map 2015 majors (4th edition) to 2016-2023 disciplines (5th edition) after verifying changes.

The project is run on R Studio which can be downloaded here: [https://posit.co/downloads/](https://posit.co/downloads/).

The following R packages are required to run analysis: dplyr, readr, ggplot2, stringr, showtext, cowplot, tidyr, forcats, sf, rnaturalearth, rnaturalearthdata, ggrepel.


# **File Structure**

There are 5 files in the project.



1. Dataset.rar: compressed file containing original data set downloaded in csv format to be used in the R analysis. (Note: For overseas Chinese and Hong Kong, Macau students (D2), the header rows in the raw data were removed to prevent errors in R analyzing.)
2. Plots.rar: compressed file containing plots created from the R program illustrating the findings.
3. References.rar: compressed file containing additional information used in the analysis (namely the changes in disciplines based on the Ministry of Education's 4th and 5th Edition Classification Standards for Academic Disciplines.)
4. Complete_codes.R: the coding used for this project.
5. readme.md: file explaining the project and its findings.


# **Analysis**

The first part of the project starts with cleaning and merging the original dataset to create one compiled dataset for analyzing. This requires seven steps as shown in the “Complete_codes.R” file.



1. Load and Process International Student Data
2. Load and Process Overseas Chinese Data
3. Merge International and Overseas Chinese Data
4. Load and Process Chinese Current Students
5. Load and Process Chinese Graduates
6. Merge Chinese Current Students and Graduates
7. Merge All Data

After having the compiled data, the second part of the project goes on with analysis of three aspects of international students in Taiwan: (1) origins and the impact of the Industry-School Program (ISP), (2) top disciplines by students, and (3) origins and gender ratios for Information and Communication Technology (ICT) and Engineering.

The findings of each aspect is summarized as followed:


## **Origins of international students in Taiwan:**

![alt text](https://github.com/SamPTD/Plots/blob/main/1.1%20Current_Students_2015.PNG)

* Top 10 are all Asian, making up almost 90% of total students.
* In 2023, six ASEAN countries accounted for nearly 70% of total students.
* From nearly 3,000 in 2015, the number of Vietnamese and Indonesian have increased more than 6 and 3 times respectively in 2023.
* The main driver for the case of Vietnam: the Industry-School Program (ISP).
* As of 2023, Vietnam accounts for almost 90% of the ISP students. Half of the Vietnamese students enrolling in ISP.


## **Top disciplines chosen disciplines:**



* Over the last decade, there are five disciplines which continuously attract the most international students.
* Business and Management stays top throughout the years.
* The most trendy are Hospitality, Engineering (double over the period) and Information Technology (increases 5 times).
* Students from Vietnam, Indonesia and Malaysia make up the top countries.
* Engineering is likely to surpass Business and Management as the top discipline.
* Language and Arts are in the top 5 and stay stable during the period.
* In the group “Others”,  Medical and Health, Social and Behavioral Sciences and Journalism routinely make up the top 3.


## **Breakdown of ICT and Engineering:**



* The top 3 ASEAN countries Vietnam, Indonesia and Malaysia account for most students in these tech fields.
* From 3 to 1, the ratio of male and female in IT is now reduced to less than 2 to 1. Gender gap in Engineering is also narrowing.
* Vietnam and the Philippines have had more female than male students in the IT department in the last couple of years.


# **Results**

As shown from the findings, the landscape of international students in Taiwan is heavily focused on the Asian region, with Southeast Asian countries accounting for the majority. Vietnamese students have the biggest jump in numbers over the years and are the top country of origin as of 2023. The main driver for the case of Vietnam is the Industry-School Program (ISP) with half of the Vietnamese students in Taiwan enrolling via this program. As of 2023, Vietnamese accounted for nearly 90% of the ISP students. It needs further analysis and more data to determine what kind of impact this region-bias and country-dominance create for school programs in general and ISP in particular.

Information and Communication Technology (ICT) and Engineering are among the most trendy and have the biggest increase during the studied period. Gender gaps in these disciplines are narrowing over the years. Targeted policies can help faster improve the gender ratios.


# **Contributors**

Group BD-07 (“Big Data for Social Analysis” class):



* Charlotte/李琬婷 LI, Yuen-Ting (112ZM1031) - Project Leader, Coder, Writer
* Claire/蔡欣妤 TSAI, Hsin-Yu (112ZM1020) - Coder, Writer
* Ivan/余彥澤 IU, In-Chak (112ZM1033) - Coder, Writer
* Samuel/馮朝陽 PHONG, Trieu Duong (112ZM1042) - Coder, Writer


# **Acknowledgments**



* We would like to thank Prof. Chung-Pei Pien of ICI/NCCU for his guidance and encouragement to finish this project. His “Big Data for Social Analysis” class is a mind-opening pathway to learn using R for data analyzing.
* We have great help from AI language models like ChatGPT and Copilot. They play the roles of personal assistants, helping with questions, testing solutions and fixing all kinds of mistakes.
* We are grateful for the online community who have struggled with the same problems and provided great support through their expertise.


# **References**

Data sources:



* Links:

Taiwan’s Ministry of Education's 4th and 5th Edition Classification Standards for Academic Disciplines:



* Link:
