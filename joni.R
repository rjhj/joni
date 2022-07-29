# DESCRIPTION ------------------------------------------------------------------
#
# JONI by rjhj, https://github.com/rjhj/joni
#
# A data wrangling exercise with Joni Mitchell albums using R's data.table.
#
# The original HTML tables from Wikipedia are transformed to tidy and usable
# data.tables. See README.md for screenshots and more information.

# 1a. LIBRARIES ----------------------------------------------------------------

library(data.table) # Enhanced data.frame
library(rvest) # For web scraping and HTML element manipulations
library(stringr) # For string manipulation

# 1b. CONSTANTS (ALL CONSTANTS ARE CAPITALIZED FOR CLARITY)---------------------

# Website where to find the tables. Backup html is in the files folder
URL = "https://en.wikipedia.org/wiki/Joni_Mitchell_discography"

# There are 12 tables on the website:
#     1. Info box
#     2. Studio albums,  
#     3. Live albums
#     4. Compilation albums
#     5. Extended plays
#     6. Singles
#     7. Guest singles
#     8. Video albums
#     9. Music videos
#    10. Navigation box
#    11. Archives series
#    12. Collaborations
# I only want to use 2. Studio, 3. Live, 4. Compilation and 6. Singles
ALBUM_POSITIONS = c(
  'studio'= 2L,
  'live' = 3L,
  'compilation' = 4L,
  'single' = 6L
)
## CONCLUSION PART 1
# Now libraries and constants have been defined.

# 2. TURN URL TO RELEVANT HTML TABLES ------------------------------------------

html_tables <- read_html(URL) |> # Read html from URL
  html_elements("table") # Find all <table> elements

html_tables <- html_tables[ALBUM_POSITIONS] # Choose only relevant tables

# Name the list using names given in ALBUM_POSITIONS dictionary
names(html_tables) <- names(ALBUM_POSITIONS)

## CONCLUSION PART 2
# html_tables list now contains all the relevant tables: studio, live,
# compilation and single

# 3. CONVERT THE HTML TABLES TO DATA.TABLES-------------------------------------

html_table_to_dt <- function(html_table){
  # Converts the html table into a data.frame
  #
  # Args:
  #   html_table: HTML containing <table...>..</tbody>
  #
  # Returns:
  #   data.table
  return(
    html_table |> # Variable containing html table 
      html_table() |> # Parses the html table in a data frame
      as.data.table() # Coerces the data.frame to data.table
  )
}

# Apply the html to data.table conversion to all the html tables
albums = lapply(html_tables, html_table_to_dt)

# Albums are fairly uniform, but the last columns of "studio" is junk,
# so let's remove it
albums[["studio"]][, ncol(albums[["studio"]]) := NULL]

## CONCLUSION PART 3
# albums list now contains the four data.tables. You can access them using:
# albums[["live"]] for live albums, etc
# class(albums[["live"]]) returns [1] "data.table" "data.frame" as it should
# View(albums[["live"]]) to see the data.table in RStudio
# However currently all the data.tables need a lot of tidying.

# 4. REMOVE WIKIPEDIA REFERENCES -----------------------------------------------
# Tables are from Wikipedia so all the reference links (such as [12] or [A])
# need to be removed from the data.

remove_references <- function(dt){
  # Takes a data.table and removes every pattern of
  # data where any number (>=1) of alphanumerical characters
  # are inside of brackets, for example, [8].
  #
  # Args:
  #   dt: data.table from where the citation links are removed
  cols <- c(1:ncol(dt)) # Apply to all columns
  dt[,(cols) := # Assign by reference
       lapply(.SD, # Apply function to all columns
       str_remove_all, # Removes all patterns of:
       "\\[[:alnum:]+\\]")] # [ALPHANUM+]
}

lapply_invis <- function(dts, fun, ...){
  # lapply wrapped inside invisible() to hide printing.
  #
  # Args:
  #   dts: list of data.tables
  #   fun: function to be used
  #   ... : additional parameters
  dts |> # Use all the data.tables
    lapply(fun, ...) |> # Use function and other arguments to all
    invisible() # Hide printing
}

#Remove Wikipedia references from all cells
lapply_invis(albums, remove_references)

## CONCLUSION PART 4
# Now all the Wikipedia references have been removed.

# X. REPLACE NEWLINES WITH SPACES -----------------------------------------
# Certification columns has newlines. Replace those with spaces.

replace_all_str_for_cols <- function(dt, cols, s1, s2){
  # Replaces all occurrences of s1 with s2 in specified columns
  #
  # Args:
  #   dt: data.table
  #   col: column name as a string
  #   s1: string to be replaced
  #   s2: string to replace with
  dt[,(cols) := # Assign by reference
       lapply(.SD, # Using subset of dt
              str_replace_all, s1, s2), # Replace all s1 with s2
     .SDcols = cols] # Use these columns
}

# Replace all newlines with spaces in Certifications for all albums
lapply_invis(albums, replace_all_str_for_cols, c("Certifications"), '\\n', ' ')  

# 5. FIXING THE HEADERS --------------------------------------------------------
# Because all the original HTML tables have a partial double header, the more
# useful part of header is currently the first row of the table. That works
# well as the actual header.

first_row_to_header <- function(dt){
  # Replaces the headers with the first row, since
  # original table used double header.
  #
  # Args:
  #   dt: data.table
  setnames(dt, # data.table to be used
           as.character( # Convert data.table to a character vector
             dt[1])) # First row will be the new names
}

# Sets first row as headers
lapply_invis(albums, first_row_to_header)

## CONCLUSION PART 5
# Headers have been fixed

# 6.REMOVE LAST AND FIRST ROW --------------------------------------------------
# First and last line for all the data.tables are junk, so they need to be
# removed. Currently first lines are just copies of the headers and the last
# lines say: "—" denotes a recording that did not chart or was not released
# in that territory.

remove_first_and_last_row <- function(dt){
  # Removes the first and last row from a data.table.
  #
  # Args:
  #   dt: data.table
  #
  # Returns:
  #   dt: data.table
  dt <- dt[-.N] # Remove the last row
  dt <- dt[c(-1)] # Remove the first row  
  return(dt)
}

# Removes first and last row for each data.table.
albums = lapply(albums, remove_first_and_last_row)

## CONCLUSION PART 6
# "Junk rows" have been removed

# 7a. EXTRACTING FROM ONE COLUMN TO MAKE THREE-----------------------------------
# All the data.tables (except single) has a column "Album details", which might
# contain rows such as: "Released: March 1968\nLabel: Reprise".
# We need to extract columns "Year", "Month" and "Label" from "Album details"

album_details_to_three_columns <- function(dt){
  # Removes "Album details" by turning it to three columns: "Year", "Month"
  # and "Label". Changes months to numbers. Puts these new columns after "Title"
  #
  # Args:
  #   dt: data.table
  setnames(dt, "Album details", "Details") # Name changed for easier processing
  
  # Create new columns using assignment by reference and string extracting. 
  dt[,':='( # Use assignment by reference function. Extract following patterns:
    Year = str_extract(Details, "[:digit:]{4}"), # Any exactly four digits 
    Month = str_extract(Details, "(?<=Released: )\\w+"), # Word after "Released: "
    Label = str_extract(Details, "(?<=Label: )\\w+") # Word after "Label: "
  )]
  
  # Change months to numbers, for example, "May" to 5. Match() return NA
  # when it doesn't find a valid month.
  dt[, Month := match(Month, month.name)]
  
  dt[, Details := NULL] # Drop column "Details"

  setcolorder(dt, c("Year", "Month", "Label"), #Move these three columns..
              after = "Title") # ..to be after the "Title".
}

# Removes and turns column "Album details" to "Month", "Year" and "Label" columns
# for data.tables "studio,  "live" and "compilation".
lapply_invis(albums[c("studio", "live", "compilation")],
             album_details_to_three_columns)

# 7b. UPDATE SINGLES TO BE MORE SIMILAR WITH OTHERS ----------------------------
# Currently single doesn't have a title column, so let's change that.

# Create a new column Title from Single but with double quotes removed
albums[["single"]][, "Title" := str_remove_all(Single, '\\"')]

# Drop Single
albums[["single"]][,"Single" := NULL]

# Move Title to the first position.
setcolorder(albums[["single"]], "Title", before = "Year")

## CONCLUSION PART 7
# Now "Album details" columns is removed and replaced with "Year", "Month"
# and "Label". In 7b changed single to have a Title columns like the others.

# 8. YEAR AND MONTH TO NUMERIC -------------------------------------------------

update_to_valid_cols <- function(dt, cols){
  # Returns only columns that exist in the data.table.
  # For example, "single" doesn't have "Month", so from
  # c("Year", "Month") only c("Year") would be returned.
  # Args:
  #   dt: data.table  
  #   cols: list of columns
  #
  # Returns:
  #   a character vector of existing column names
  return(cols[cols %in% names(dt)])
}

change_column_types <- function(dt, cols, fun){
  # Changes column types of data.table based on fun. Updates only those cols
  # that are found in the data.table.
  #
  # Args:
  #   dt: data.table  
  #   cols: columns we want to coerce to a different type
  #   fun: coercing function (such as as.numeric)
  
  # Rebuild cols to only have the columns that exist in the data.table
  cols <- update_to_valid_cols(dt, cols)
  
  # Coerce all the columns in the cols to the type defined in fun.
  dt[, (cols) := lapply(.SD, fun), .SDcols = cols]
}

# Change the data types of "Year" and "Month" to numeric
lapply_invis(albums, change_column_types, c("Year", "Month"), as.numeric)

## CONCLUSION PART 8
# Now the data types for Year and Month have been changed to numeric.

# 9a. SET KEYS AND INDICES ------------------------------------------------------
# To speed up future queries, we set keys and indices.

set_keys <- function(dt, cols){
  # Sets keys to the data.table. Only sets those keys that exist as columns
  # in the data.table
  # Args:
  #   dt: data.table  
  #   cols: character vector of columns to use as keys
  cols <- update_to_valid_cols(dt, cols) # Update cols to only valid columns
  setkeyv(dt, cols) # Sets keys
}

# Tries to set the first key as "Year" and second as "Month",
# but only does this for valid keys
lapply_invis(albums, set_keys, c("Year", "Month"))  

# Set Title as a secondary index for all the data.tables
lapply(albums, setindex, "Title")

# 9b. CREATE SHORTCUTS ---------------------------------------------------------

# Now all the tables have been turned to a usable form
# Let's name the data.tables for easier use
studio =  albums[["studio"]]
live = albums[["live"]]
compilation = albums[["compilation"]]
single = albums[["single"]]

## CONCLUSION PART 9
# All data.tables now have at least one key column (Year) and most have 
# Month as the second key. They all have Title as a secondary index.
# Also in 9b. created a variable for each data.table in the albums list
# for convenience.

# 10a. ADD NUMBER OF SINGLES PER STUDIO ALBUM ----------------------------------
# Let's calculate the number of singles from single and add it to studio.
# There are many ways to do this. I'm going to create a temporary data.table
# called album_and_singles just for the sake of clarity and join it with studio.

# .EACHI groups by i, .N contains the number of rows in each group
album_and_singles <- single[studio, .("N_of_singles" = .N),
               by=.EACHI, on = c(Album = "Title")]

# Join the tables
studio <- studio[album_and_singles, on = c(Title = "Album")]

# This would be the same thing without creating the extra data.table:
# studio <- studio[single[studio, .("N_of_singles" = .N),
# by=.EACHI, on = c(Album = "Title")], on = c(Title = "Album")]

# 10b. Singles as columns for each album ---------------------------------------
# We want to create a data.table which contains singles as columns
# (Single_1, etc) for their albums. Each album has 1-3 singles.

# Overwriting album_and_singles, which has used in 10a. as temporary data.table
album_and_singles <- single[, .(Album, Title)] # Get Album and Title columns

album_and_singles[, Single := #Create a new column with the walrus operator
                    paste0("Single_", # Create a string starting with "Single_"
                           1:.N), # String ends with a sequential ID.
                  by = Album] # Group by album to match .N with N of singles

# Using dcast to achieve "long to wide" operation. 
album_and_singles <- dcast(album_and_singles, Album ~ Single,
                           value.var = c("Title"))

# Currently album_and_singles isn't ordered. Let's add Year and Month for each
# album. Since some albums are studio and some live, we'll row bind them first.
studio_and_live <- rbind(studio[,.(Title, Year, Month)],
                         live[,.(Title, Year, Month)])

# Add Year and Month columns from studio_and_live
album_and_singles[studio_and_live, ':='(Year = i.Year,
                                        Month = i.Month),
                  on = c(Album = "Title")]

# Sets Year and Month to be located after Album
setcolorder(album_and_singles, c("Year", "Month"), after = c("Album"))

# Sets keys Year and Month to order the data and allow fast searches
setkey(album_and_singles, Year, Month)

## CONCLUSION PART 10
# a. studio now includes number of singles. b. new table was created to include
# the albums and their singles as new columns (long to wide).