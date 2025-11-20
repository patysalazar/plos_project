# Pedestrian Level-of-Service (PLOS) and Walkability Equity in Saskatoon: A Gender-Based and Health Analysis 

Patricia Salazar-Ramirez, Meisam Ghasedi, Daniel Fuller<br>

## Abstract
Pedestrian infrastructure plays an important role in shaping mobility and health. This study developed a Pedestrian Level-of-Service (PLOS) measure for Saskatoon and examined its relationship with walking to work, gender composition, and diabetes prevalence at the dissemination-area (DA) level. Results show strong spatial clustering of high PLOS and walking, weak overall associations between gender and walk rates, and geographically varying local effects. Low PLOS areas with lower walking and higher diabetes prevalence indicate potential equity gaps

## I. Research Questions
1.	How does pedestrian infrastructure quality, measured by the Pedestrian Level-of-Service (PLOS), relate to walking behavior across Saskatoon?<br>
    a) Do these patterns differ by gender?<br>
2.	How do PLOS and walking rate relate to diabetes in each dissemination area (DA)? <br>

## II. Rationale and Significance
### Development of Ideas
Rationale and Significance
Walking is a core determinant of health and the foundation of sustainable cities. However, disparities in pedestrian infrastructure often reflect social inequalities, especially differences in mobility, safety, and comfort1. Pedestrian Level-of-Service (PLOS) is one of the most widely used metrics for assessing the overall quality of the walking environment.2 

There are many ways to calculate PLOS, but key components include a mix of environmental, traffic-related, and physical factors. Within these categories, sidewalk width, surface quality, pedestrian flow, and presence of obstructions are frequently examined aspects1,3,4. These features frequently vary within cities, with suburbs and peripheral zones typically scoring lower than inner-city and downtown areas, which tend to display higher walkability5. Traditional PLOS models prioritize infrastructure measurements but rarely account for who benefits from improved pedestrian environments or how these experiences differ across populations4,6. For example, walkable streets have been shown to attract more women than men7, which could be associated with safety perception.

The degree to which the surroundings are pleasant and encourage walking is known as walkability8. Research has consistently demonstrated the impact that walkability features and infrastructure improvement have on increasing physical activity behaviours9. This is especially important considering the rising prevalence of non-communicable diseases (NCD), like Type 2 Diabetes. Physical inactivity is among the most important modifiable risk factors for this condition10, and moderate physical activity is associated with a significant reduction in Type 2 Diabetes risk11. 

This research will evaluate a Pedestrian Level-of Service measurement by dissemination area (DA) in Saskatoon, using three variables: sidewalk width, class, and material. It will also address gaps in existing literature by applying a sex- and gender-based analysis (GBA+) to the spatial assessment of pedestrian environments, identifying potential inequities in infrastructure quality and walking behavior. Finally, the study will compare PLOS results with administrative health data from 1995 to 2010, evaluating the association between walk rate and diabetes.  

By integrating infrastructure data, gender-stratified walking behavior, and administrative data, this research will contribute to Healthy and Smart Cities paradigms, using geographic data to guide equitable urban design.

### Objectives
1.	To develop a PLOS model using municipal spatial data on sidewalk characteristics in Saskatoon.
2.	To validate the PLOS measure using walking to work rates.
3.	To assess the spatial relationship between PLOS scores, walking rate, gender, and diabetes within a Healthy Cities and Smart Cities framework.
 
## III. Methods and Approach
### Methodological Framework
This research design combines several methods from the course:
•	Socio-Spatial Epidemiology: to identify geographic disparities in pedestrian infrastructure and walking behavior using spatial analysis at the DA level.
•	Gender-Based Analysis (GBA+): to examine how gender influences use of pedestrian infrastructure and walking behaviors.
•	Data Science and Methods Modeling: to develop and validate a PLOS scoring system using spatial data and computational analysis in R.
•	Healthy Cities and Smart Cities Frameworks: to frame PLOS as an equity indicator for sustainable, health-promoting urban environments.

### Data 
1)	Study area: Saskatoon, Saskatchewan.
2)	Data sources:
a)	City of Saskatoon open data (sidewalks, roads, paths).
b)	Statistics Canada 2021 Census (walking mode share by gender and DA).
c)	Administrative health data (diabetic patients in Saskatoon from 1995 to 2010).

All spatial layers were projected to Mercator, and analyses were performed using R.

Data Preparation and Mapping
The analysis began by developing a Pedestrian Level-of-Service (PLOS) scoring framework that incorporated sidewalk width, class, and material. Each sidewalk segment was assigned a score for each of these features. For width, segments wider than 3 meters were scored 4; 2.5 to 3 meters received a score of 3; 1.5 to 2.5 meters received 2; 1 to 1.5 meters received 1; and those narrower than 1 meter received 0. For type, “walkway” or “separate” sidewalks were scored 3, “pathways” were scored 2, and “combined” or “other” types were scored 1. For material, concrete, asphalt, or overlay were assigned a score of 3; brick or pavers were assigned 2; crusher dust or shale were assigned 1; and dirt received a score of 0.

These component scores were summed to create a quality index ranging from 0 to 10. To reflect the relative contribution of each segment within its dissemination area (DA), scores were multiplied by segment length. The weighted scores were then summed for each DA and normalized by DA area (points/km²). Based on these results, DAs were classified into four categories: low (<80 points/km²), medium (80-160 points/km²), high (160-240 points/km²), and very high (>240 points/km²). These scores were then joined to DA polygons and mapped, with colour gradients assigned to each category.

Walk rate validation used the 2021 Census of Population variable representing the percentage of commuters whose primary mode of travel to work was walking. It was mapped by DA for visual comparison with PLOS scores to assess spatial alignment between infrastructure quality and active commuting behaviour.

For the Gender-Based Analysis Plus (GBA+) component, two layers were included. First, walk to work rate by gender was examined at the Census Metropolitan Area (CMA) level. Vectors for the employed labour force by gender were obtained from the 2021 Census, and gender-specific walk rates were computed descriptively, although they were not included in DA-level regressions. Second, gender composition was constructed at the DA level using age-by-gender counts from the 2021 Census. Although gender identity categories beyond men and women exist in the Census, these data are suppressed at the DA level due to low counts, limiting the ability to conduct analyses beyond binary gender categories. Men and women populations were summed across three age groups (0-14, 15-64, and ≥65 years), and proportions of men and women were computed by dividing each by the total DA population. These proportions were then joined to Saskatoon DA polygons and mapped.

Health outcome data was focused on diabetes hospitalizations at the DA level, both overall and by gender. Using the “diabetes” tab from the hospitalization dataset, the DA identifier and gender columns were used to count cases by DA. Denominators were derived from Census 2021 totals: overall DA population for total prevalence, and men and women populations for gender-specific prevalence. Prevalence was calculated as cases divided by population, and maps were produced using shared colour limits across genders for comparability.
Analysis
An analysis table was constructed by joining all required variables using the DAUID identifier. This included PLOS density and walk rate (with PLOS density also standardized for comparability), gender composition (proportion of women and total population), and diabetes prevalence (overall and gender-specific).

Statistical analyses were conducted at the DA level. Bivariate correlations (Pearson and Spearman as appropriate) were used to test relationships between PLOS and walk rate, proportion of women and walk rate, and between PLOS or walk rate and overall diabetes prevalence. Additionally, ordinary least squares (OLS) regressions were performed using walk rate as the dependent variable and proportion of women as the predictor, estimated both unweighted and weighted by DA population.

To explore whether the relationship between gender composition and walking behavior varied across Saskatoon DAs, a Geographically Weighted Regression (GWR) model was applied. GWR does not estimate a single global coefficient for the association between the proportion of women and walk rate, instead, it allows the strength and direction of the relationship to differ across geographic space. First, DA-level variables (walk rate and proportion of women) were joined to spatial polygons, and this was converted to a spatial object. Then, a bandwidth was selected to determine the spatial extend over which local regressions were estimated. The GWR model generated a local coefficient for each DA, indicating how strongly the proportion of women predicted walk rate in that specific area. These coefficients were mapped to visualize the heterogenitiy of the relationship.
 
## IV. Results and Discussion
### PLOS Scores
The PLOS map of pedestrian segments shows a clear disparity in sidewalk quality, width, and surface material between central and peripheral neighborhoods (Figure 1). 
 
![segment_level_plos](https://github.com/walkabillylab/plos_saskatoon/blob/main/maps/PLOS_Saskatoon_segments_highres.jpg)<br>
Figure 1: Segment-level Pedestrian Level-of-Service (PLOS) in Saskatoon. [(View Code)](https://github.com/walkabillylab/plos_saskatoon/blob/main/code/plos_final_code_segments.Rmd).<br>

The city's downtown has a concentration of high and very high PLOS scores. Pedestrian paths are more continuous, comfortable, and accessible in these places because of their higher densities and more developed infrastructure. This is also reflected in the continuous PLOS map (Figure 2). 
 
![continuous_plos](https://github.com/walkabillylab/plos_saskatoon/blob/main/maps/PLOS_Saskatoon_continuous.png)<br>
Figure 2: Continuous Pedestrian Level-of-Service (PLOS) in Saskatoon. [(View Code)](https://github.com/walkabillylab/plos_saskatoon/blob/main/code/plos_final_code.Rmd).<br>

Newer districts outside the city’s denser central areas tend to have medium PLOS scores. These neighborhoods present adequate pedestrian infrastructure but may also exhibit gaps in connectivity or quality. Bigger, less dense distribution regions have lower PLOS scores overall, reflecting car-centered layouts and limited supportive features for pedestrians. 

In general, the findings show a distinct pedestrian infrastructure difference between the city centre and its periphery, with central neighborhoods providing areas that encourage walking more than the suburbs and industrial areas, which appear to be designed as mostly car oriented. These results highlight the need for targeted infrastructure in lower PLOS regions to enhance equity, safety, and accessibility across Saskatoon.

### Walk rate validation
The central neighborhoods of Saskatoon exhibit a higher proportion of walking rates, which coincide with regions with higher PLOS. On the other hand, walking mode shares and PLOS are lower in suburban and industrial areas (Figure 3). 

 
![walking_rate_saskatoon](https://github.com/walkabillylab/plos_saskatoon/blob/main/maps/PLOS_Saskatoon_walk-rate.png)<br>
Figure 3. Walking rates (%) by Dissemination Area in Saskatoon. [(View Code)](https://github.com/walkabillylab/plos_saskatoon/blob/main/code/walk_rate_scatterplot.Rmd).<br>

The scatterplot demonstrates a weak-to-moderate positive correlation: walking rate tends to increase as PLOS scores increase. This implies that while improved pedestrian infrastructure is associated with increased walking, other factors, such land use and built environment factors, are equally important in creating walkable communities (Figure 4). 

![scatterplot_plos_walkingrate](https://github.com/walkabillylab/plos_saskatoon/blob/main/maps/PLOS_walk-rate_scatterplot.png)<br>
Figure 4: Scatterplot of Density vs Walking Rate, Saskatoon Dissemination Areas. [(View Code)](https://github.com/walkabillylab/plos_saskatoon/blob/main/code/walk_rate_scatterplot.Rmd).<br>

### GBA+ Analysis
1)	Walk rate by gender
At the CMA level, women walk to work slightly more than men (4.5% vs 3.2%) (Figure 5), reflecting small gender differences in active commuting. However, this comparison is at a bigger geographic level than other analyses and could be concealing variation across neighborhoods.

![walking_rate_cma_gender](https://github.com/patysalazar/plos_project/blob/main/code/walk_rate_by_sex_files/figure-html/unnamed-chunk-10-1.png)<br>
Figure 5: Walk share by gender proportion at CMA level (Saskatoon). [(View Code)](https://github.com/patysalazar/plos_project/blob/main/code/walk_rate_by_gender.Rmd).<br>

2)	Gender composition at DA-level
At the DA level, the proportion of women varies between 40 and 60%, with more balanced distributions in central areas and higher male proportions in peripheral or industrial DAs (Figure 6).

![women_proportion](https://github.com/patysalazar/plos_project/blob/main/code/plos_gba_files/figure-html/unnamed-chunk-17-1.png)<br>
Figure 6: Proportion of women by DA in Saskatoon. [(View Code)](https://github.com/patysalazar/plos_project/blob/main/code/walk_rate_by_gender.Rmd).<br>

These patterns suggest that the gender composition of neighborhoods may align with differences in land use and employment type. 

### Health outcome: Diabetes at DA-level

Overall diabetes prevalence (Figure 7) ranged from below 1% to over 10%. According to Statistics Canada12, the prevalence of diabetes in 2021 was 9.4%. In Saskatchewan, the diabetes prevalence was 9% in 202413. Although the administrative data source is from 1995 to 2010 and the overall prevalence has increased since then14, it accurately reflects diabetes prevalence in Saskatoon, which is lower than provincial average15. 

![diabetes_overall_prevalence](https://github.com/patysalazar/plos_project/blob/main/code/plos_gba_files/figure-html/unnamed-chunk-26-1.png)<br>
Figure 7: Diabetes prevalence by DA in Saskatoon. [(View Code)](https://github.com/patysalazar/plos_project/blob/main/code/plos_gba.Rmd).<br>

When disaggregating by gender, prevalence in men was slightly higher than prevalence in women, which is consistent with national and provincial surveillance trends16, reflected by more lighter shaded areas in the men prevalence map (Figure 8).

a)	
  
![diabetes_women_prevalence](https://github.com/patysalazar/plos_project/blob/main/code/plos_gba_files/figure-html/unnamed-chunk-29-1.png)<br>

b)	
  
![diabetes_men_prevalence](https://github.com/patysalazar/plos_project/blob/main/code/plos_gba_files/figure-html/unnamed-chunk-28-1.png)<br>

Figure 8: Diabetes prevalence by DA in Saskatoon in women (a) and men (b). [(View Code)](https://github.com/patysalazar/plos_project/blob/main/code/plos_gba.Rmd).<br>

When visually comparing to the PLOS and walk rate maps (Figures 2 and 3), there is a somewhat inverse spatial pattern. The lowest prevalence occurs in the same central neighborhoods characterized by higher walk rates and better pedestrian infrastructure, while the highest prevalence appears in outer suburban and industrial zones. This supports the expected relationship between active mobility and chronic disease risk17, although at DA level the correlation may be modest. 
Among women (Figure 8a), diabetes prevalence is slightly lower and more evenly distributed across the city, while men (Figure 8b) exhibit higher prevalence areas in the northwest and eastern periphery of the city, coinciding with more automobile-dependent neighborhoods. There are also some missing areas in both maps, likely due to suppressed data or small counts. 	
Overall, these patterns reinforce that walkability inequities align with health inequities. Although these patterns cannot establish causation, they do point to structural, location-based health disparities linked to transportation environments.

### Correlation and Regression
	Pairwise correlations showed a weak positive association between PLOS density and walk rate (r ≈ 0.15), confirming that better pedestrian infrastructure corresponds to slightly higher walking activity. Correlations between walk to work rate and diabetes prevalence (r ≈ -0.08) were weak, and the negative PLOS and diabetes prevalence correlation coefficient is consistent with expectations that areas with more walking have lower diabetes prevalence.  

The coefficient for women proportion per DA in the linear regression model between proportion of women and walk-to-work rate was 0.149 can be interpreted as: for every 1.0 (100%) increase in the proportion of women, the average walk rate increases by about 15% However, this relationship is not statistically significant (p = 0.131). The R² (0.006) is extremely small, meaning the proportion of women explains less than 1% of the variation in walk rates. The weighted model by DA population resulted in a nearly identical coefficient (0.151) and a p-value not reaching statistical significance at the 0.05 level (p = 0.077). This indicates that the relationship becomes clearer when considering population size, and larger DAs could show a slightly stronger gender-walking association. However, the R² (0.0085) remains very low, so it is not a strong predictor. 

After removing DAs with walk rate equal to zero and restricting the analysis to valid proportions, the association between gender composition and walk rate became even weaker. In the dataset without these values, the unweighted regression coefficient for the proportion of women dropped to 0.020, and the association was not statistically significant (p = 0.899). The population-weighted model showed a slightly larger coefficient (0.106), but the relationship remained non-significant (p = 0.474) and weak (R2 = 0.003) (Figure 9). The results confirm that, after removing outliers, gender composition shows no meaningful linear relationship with walk share at the DA level. 

![scaterplot_walk-rate_gender](https://github.com/patysalazar/plos_project/blob/main/code/plos_gba_files/figure-html/unnamed-chunk-40-1.png)<br>
Figure 9: Scatterplot of Walk Rate vs Gender, Saskatoon Dissemination Areas. [(View Code)](https://github.com/patysalazar/plos_project/blob/main/code/plos_gba.Rmd).<br>

	Even though the global linear model showed only a very weak relationship, the geographical weighted regression (GWR) revealed spatial variation in the coefficient linking the proportion of women to walk rate. Local coefficients ranged from approximately –1.0 to +1.4, with a median around 0.15, indicating that the direction and strength of the association differed markedly by DA (Figure 10). 

![gwr_coefficients_map](https://github.com/patysalazar/plos_project/blob/main/code/plos_gba_files/figure-html/unnamed-chunk-49-1.png)<br>
Figure 10: Geographically Weighted Regression Coefficients of Proportion of Women vs. Walk Rate by DA in Saskatoon. [(View Code)](https://github.com/patysalazar/plos_project/blob/main/code/plos_gba.Rmd).<br>

In several walkable areas, particularly around the University of Saskatchewan campus, the GWR coefficients were among the most positive (shown in yellow), indicating that DAs with a higher proportion of women also had higher walk-to-work rates. In contrast, the downtown area displayed some of the strongest negative coefficients (dark purple), suggesting that women were less likely to walk to work in these central neighbourhoods despite their walkability. Most other areas, especially suburban DAs, showed coefficients close to zero. These contrasts highlight that gender walking patterns in Saskatoon are strongly context-dependent, emphasizing the need for neighbourhood-specific planning that considers gendered mobility.

Together, findings suggest that PLOS and walk rates are aligned spatially but not perfectly correlated, indicating that infrastructure alone does not drive walking behaviour. Furthermore, gender composition appears to influence walkability at the DA level. Diabetes prevalence shows spatial patterns similar to those of low-PLOS and low-walk-rate areas, highlighting potential health inequities in built environment and transportation infrastructure. 

## V. Conclusions
### Summary
This project developed a Pedestrian Level-of-Service (PLOS) measure for Saskatoon, integrating elements of socio-spatial epidemiology, Gender-Based Analysis Plus (GBA+), and data science to examine equity in walkability and health. The analysis revealed clear spatial inequalities in sidewalk quality and walking rates, with central neighborhoods showing higher walkability and slightly lower diabetes prevalence compared to peripheral, car-oriented areas. Although global statistical associations were weak, geographically weighted regression indicated strong local variation in how gender composition relates to walking. These findings demonstrate that walkability and gendered mobility are shaped by neighbourhood context and highlight opportunities for equity-oriented pedestrian planning in Saskatoon.

### Expected Contributions 
The findings are being shared with City of Saskatoon planners and public health stakeholders to inform local infrastructure decisions. City planners are being consulted in refining the PLOS scoring framework and interpreting spatial patterns, ensuring that the analytical approach reflects real-world planning needs and aiding in targeting infrastructure changes. Future iterations of this project will continue to engage planners and public health stakeholders earlier in the process, incorporating their feedback into the refinement of PLOS measurement and data visualization tools. 

The overarching goal is to translate the analysis into practical insights that can guide investments directed at improving sidewalk quality, connectivity, and accessibility, ultimately promoting more active, healthy and inclusive communities. 

### Limitations and Future Directions
The analysis is cross-sectional, meaning causation cannot be inferred between pedestrian environment and health outcomes. In addition, temporal mismatches between administrative health data and 2021 Census information may reduce precision. The PLOS index focuses on physical sidewalk features and does not yet incorporate broader walkability dimensions such as traffic safety.

Future work should incorporate time-series data to track changes in infrastructure and health outcomes, include individual-level mobility data for validation, and expand GBA+ to explore intersections with income, age, and disability, providing a more comprehensive view of walkability equity. 

## References
1.	Raad N, Burke MI. What Are the Most Important Factors for Pedestrian Level-of-Service Estimation? A Systematic Review of the Literature. Transp Res Rec. 2018;2672(35):101-117. doi:10.1177/0361198118790623
2.	Landis BW, Vattikuti VR, Ottenberg RM, McLeod DS, Guttenplan M. Modeling the Roadside Walking Environment: Pedestrian Level of Service. Transp Res Rec. 2001;1773(1):82-88. doi:10.3141/1773-10
3.	Kadali BR, Vedagiri P. Review of Pedestrian Level of Service: Perspective in Developing Countries. Transp Res Rec. 2016;2581(1):37-47. doi:10.3141/2581-05
4.	Nag D, Goswami AK, Gupta A, Sen J. Assessing urban sidewalk networks based on three constructs: a synthesis of pedestrian level of service literature. Transp Rev. 2020;40(2):204-240. doi:10.1080/01441647.2019.1703841
5.	Conderino SE, Feldman JM, Spoer B, Gourevitch MN, Thorpe LE. Social and Economic Differences in Neighborhood Walkability Across 500 U.S. Cities. Am J Prev Med. 2021;61(3):394-401. doi:10.1016/j.amepre.2021.03.014
6.	Rahul TM, Manoj M. Categorization of pedestrian level of service perceptions and accounting its response heterogeneity and latent correlation on travel decisions. Transp Res Part Policy Pract. 2020;142:40-55. doi:10.1016/j.tra.2020.10.011
7.	Jensen WA, Stump TK, Brown BB, Werner CM, Smith KR. Walkability, Complete Streets, and Gender: Who Benefits Most? Health Place. 2017;48:80-89. doi:10.1016/j.healthplace.2017.09.007
8.	Knapskog M, Hagen OH, Tennøy A, Rynning MK. Exploring ways of measuring walkability. Transp Res Procedia. 2019;41:264-282. doi:10.1016/j.trpro.2019.09.047
9.	Smith M, Hosking J, Woodward A, et al. Systematic literature review of built environment effects on physical activity and active transport - an update and new findings on health equity. Int J Behav Nutr Phys Act. 2017;14(1):158. doi:10.1186/s12966-017-0613-9
10.	Katzmarzyk PT, Friedenreich C, Shiroma E, Lee IM. Physical Inactivity and Non-Communicable Disease Burden in Low-, Middle-, and High-Income Countries. Br J Sports Med. 2022;56(2):101-106. doi:10.1136/bjsports-2020-103640
11.	Kyu HH, Bachman VF, Alexander LT, et al. Physical activity and risk of breast cancer, colon cancer, diabetes, ischemic heart disease, and ischemic stroke events: systematic review and dose-response meta-analysis for the Global Burden of Disease Study 2013. The BMJ. 2016;354:i3857. doi:10.1136/bmj.i3857
12.	Canada PHA of. Snapshot of Diabetes in Canada, 2023. December 6, 2023. Accessed November 11, 2025. https://www.canada.ca/en/public-health/services/publications/diseases-conditions/snapshot-diabetes-canada-2023.html
13.	Saskatchewan 2024 Backgrounder One Pager. Accessed November 11, 2025. https://www.diabetes.ca/getmedia/43083884-1d47-4ef9-adf1-045dcf63cae9/DC_2024_Backgrounder_OnePager_SK_Nov2024.pdf
14.	Canada PHA of. ARCHIVED Diabetes in Canada. November 9, 2017. Accessed November 11, 2025. https://www.canada.ca/en/public-health/services/publications/diseases-conditions/diabetes-canada-highlights-chronic-disease-surveillance-system.html
15.	Government of Canada SC. Census metropolitan area of Saskatoon, Saskatchewan - Quick facts by census metropolitan area, 2013-2014. June 24, 2015. Accessed November 11, 2025. https://www150.statcan.gc.ca/n1/pub/82-625-x/2015001/article/14197/cma-rmr28-eng.htm
16.	Alam MS, Dyck R, Janzen B, Karunanayake C, Dosman J, Pahwa P. Risk factors, incidence, and prevalence of diabetes among rural farm and non-farm residents of Saskatchewan, Canada; a population-based longitudinal cohort study. J Diabetes Metab Disord. 2020;19(2):1563-1582. doi:10.1007/s40200-020-00693-z
17.	Katzmarzyk PT, Friedenreich C, Shiroma E, Lee IM. Physical Inactivity and Non-Communicable Disease Burden in Low-, Middle-, and High-Income Countries. Br J Sports Med. 2022;56(2):101-106. doi:10.1136/bjsports-2020-103640

