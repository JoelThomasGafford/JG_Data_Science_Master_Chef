JG_Data_Science_Master_Chef

This project is a portfolio piece for my data science resume. I used RStudio to scrape data from wikipedia on the popular "Master Chef" show, hosted by Gordon Ramsay.

## Project Summary

Youtube video explaining the project:
https://www.youtube.com/watch?v=wKWZW08O-EY

In the "Master Chef" show, contestants compete in multiple rounds and are graded by the quality of their dishes. 

![Graph 1](output/Master_Chef_00_Elim_A_001.png)

I used Rvest to bring the Wikipedia table into a dataframe in RStudio:

![Graph 1](output/Master_Chef_01_Elim_A_001.png)

I converted these grades into numbers ranging from -2 for being eliminated, up to 3 for creating the best dish in that round. 

![Graph 1](output/Master_Chef_03_Number_A_001.png)

I then created a new table in RStudio to calculate the cumulative sums for each contestant over time. 

![Graph 1](output/Master_Chef_04_Cumsum_A_001.png)

This gives us data that we can convert into a line graph.

![Graph 1](output/Master_Chef_Cont_S01_A_001.png)

Now we can see that in some seasons, sometimes people won despite not being as high in the graph as others.

![Graph 1](output/Master_Chef_Cont_S01-S13_A_001.png)

Here is a point graph showing all contestants from seasons 1-13, colored by their final standing.

![Graph 1](output/Master_Chef_Cont_FInal_A_001.png)




Type of project: Data analysis / visualization

Tools used: R, ggplot2, tidyverse, reshape2, rvest, tibble

Key skills demonstrated: Web scraping, function creation, data wrangling, data visualization