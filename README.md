# Happiness-Quality_of_Life
Dataset Description:

1. Suicide Rate
   - Source https://worldpopulationreview.com/country-rankings/suicide-rate-by-country
   - Year: 2021
   - Number of Countries: 183
  
2. Quality of life
   - Source https://www.worlddata.info/cost-of-living.php
   - Year: 2020
   - Number of Countries: 192

3. Happiness Score
   - Source https://worldhappiness.report/ed/2020/
   - Year: 2020
   - Number of Countries: 153
  
4. Quality of Living 
   - Source https://www.kaggle.com/orhankaramancode/city-quality-of-life-dataset
   - Column definition: https://developers.teleport.org/api/
   - Year: 2020
   - Number of Cities: 266


Cleaning data:
In Excel
Problem in file
1. Country name in "Quality of life by city" has state names instead of United States
   To change all state name to be United State. The alldata and newcountryname excel file have been created.
   1.Search the states name on Wikipedia. [In the slide shows excel from "alldata"]
   2.Use formula in the excel to correct country name (Sheet: Clean country) [In the slide shows excel from "alldata"]
   3.Then save new file as "newcountryname". [In the slide show excel from "newcountryname"]

In R
1.Happiness score > 1.Change colummn's name (easier to read)
                    2.Create new cateforical variable (High/Medium/Low)
                 
2.Quality of life by country > 1.Change the country name (to match with Happiness score)

3.Quality of life by cities > 1.Change column names
                              2.rename country by merge "newcountryname" that we've prepare before to correc the country name.
                              3.
Merge all files : 
Merge all files with country name in R (*if there are more than one country, we use the average amount to aggregate)
