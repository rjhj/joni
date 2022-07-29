### Joni
#### A data wrangling exercise with Joni Mitchell albums using R's data.table library.

##### OVERVIEW

This program scrapes some HTML tables from Wikipedia, such as these two: 

![Studio albums, Wikipedia](/files/image_for_read_me/studio_wikipedia.png)
![Singles, Wikipedia](/files/image_for_read_me/single_wikipedia.png)

And turns and combines them to data.tables such as these two:

![Studio data.table, Wikipedia](/files/image_for_read_me/studio.png)
![Singles data.table, Wikipedia](/files/image_for_read_me/singles_per_album.png)

##### DETAILS

##### 1) Description
This program transforms four HTML tables from Wikipedia into usable and tidy data.tables. Also creates a new table that combines albums with singles.

##### 2) Libraries
```
library(data.table) # Enhanced data.frame
library(rvest) # For web scraping and HTML element manipulations
library(stringr) # For string manipulation
```
##### 3) How to use
Make sure you have installed the packages mentioned in libraries. After that you can just copy joni.R and run it. It should work as long as the Wikipedia page doesn't change too much.

You can also just read joni.R, because I have added lost of comments.

#### 4) What it does?
1) Downloads all 12 tables from en.wikipedia.org/wiki/Joni_Mitchell_discography
2) Only selects four of them: studio, live, compilation and single and saves them to a list called albums
3) Turns albums to data.tables
4) Removes the Wikipedia references, e.g. [1]
5) Replaces newlines with spaces
6) Fixes the headers
7) Removes junk lines
8) Turns "Album details" column to separate columns Year, Month and Label
9) Modifies single to be more similar with others
10) Turns Year and Month columns to numeric
11) Sets keys and indices
12) Adds a column for number of singles per album to studio
13) Creates a new data.table that contains all the singles as columns for each album
