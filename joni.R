# DESCRIPTION -----------------------------------------------------------------
#
# JONI by rjhj, https://github.com/rjhj/joni
#
# A data wrangling exercise with Joni Mitchell albums using R's data.table library.
#
# URL: https://en.wikipedia.org/wiki/Joni_Mitchell_discography
#
# These original HTML tables from Wikipedia need to be transformed heavily,
# since they have partial double headers, some combined cells, columns
# containing different pieces of information and other issues.
#
# The goal is to transform the HTML tables to usable data.tables.
# 
# See README.md for screenshots and more information

# LIBRARIES ---------------------------------------------------------------

library(data.table) # for data wrangling
library(rvest) # for web scraping.
library(stringr) # for string manipulation

# CONSTANTS (ALL CONSTANTS ARE CAPITALIZED FOR CLARITY)------------------------

# Website where to find the tables
URL = "https://en.wikipedia.org/wiki/Joni_Mitchell_discography"

# There are 12 tables on the website, but I only want these four
ALBUM_POSITIONS = c(
  'studio'= 2L,
  'live' = 3L,
  'compilation' = 4L,
  'single' = 6L
)

# 1. URL TO HTML TABLES -------------------------------------------------------

# Get table elements from URL
html_tables_all <- URL |>
  read_html() |> # Read URL
  html_elements("table") # Takes only <table>'s

## CONCLUSION PART 1
# Now html_tables_all contains a list of all the 12 html tables
# from Joni Mitchell's discography page:
#   1. Info box
#   2. Studio albums,  
#   3. Live albums
#   4. Compilation albums
#   5. Extended plays
#   6. Singles
#   7. Guest singles
#   8. Video albums
#   9. Music videos
#  10. Navigation box
#  11. Archives series
#  12. Collaborations

# 2. CHOOSE ONLY THE RELEVANT TABLES-------------------------------------------

# Create an empty object to store only the relevant tables
html_tables = NULL

# Save the four relevant tables (defined in ALBUM_POSITIONS)
# to html_tables using a for-loop
for (name in names(ALBUM_POSITIONS)) {
  html_tables <- c(html_tables, # Add to list with every loop using concatenate
                   list( # Make a new list object
                     html_tables_all[[ # Find from all the tables
                       ALBUM_POSITIONS[name]]])) # Gives the position as integer
}

# Name the list using names given in ALBUM_POSITIONS dictionary
names(html_tables) <- names(ALBUM_POSITIONS)

## CONCLUSION PART 2
# html_tables list now contains all the relevant tables: studio, live,
# compilation and single

# 3. CONVERT THE HTML TABLES TO DATA.TABLES------------------------------------

# Create an object where to store all the data.tables
albums = NULL

html_table_to_dt <- function(html_table){
  # Converts the html table into a data.frame
  #
  # Args:
  #   html_table: HTML containing <table...>..</tbody>
  #
  # Returns:
  #   data.table
  return(
    html_table |> # Variable containing HTML table 
      html_table() |> # Parses the html table in a data frame
      as.data.table() # Coerces the data.frame to data.table
  )
}

# Apply the html to data.table conversion to all the HTML tables
albums = lapply(html_tables, html_table_to_dt)

# Albums are fairly uniform, but the last columns of "studio" is junk,
# so let's remove it
albums[["studio"]][,ncol(albums[["studio"]]) := NULL]

## CONCLUSION PART 3
# albums list now contains the four data.tables. You can access them using:
# albums[["live"]] for live albums, etc
# class(albums[["live"]]) returns [1] "data.table" "data.frame" as it should
# View(albums[["live"]]) to see the data.table in RStudio
# However currently all the data.tables need a lot of tidying.

# 4. REMOVE WIKIPEDIA REFERENCES ----------------------------------------------
# Since the tables are from Wikipedia there are 
# plenty of links to references, for example,
# [12] or [A]. They need to be removed from the data.

remove_references <- function(dt){
  # Takes a data.table and removes every pattern from
  # data where any number (>=1) of alphanumerical characters
  # are inside of brackets, for example, [8].
  #
  # Args:
  #   dt: data.table from where the citation links are removed
  #
  # Returns:
  #   data.table, but not explicitly
  cols <- c(1:ncol(dt)) # Apply to all columns
  dt[,(cols) := # Assign by reference
       lapply(.SD, # Apply function to all columns
       str_remove_all, # Removes all patterns of:
       "\\[[:alnum:]+\\]"), # [ALPHANUM+]
       .SDcols = cols] # Use all the columns
}

clean_up <- function(dts, fun) {
  # Used to apply different cleaning functions
  # to all the data.tables and hide the printing.
  #
  # Args:
  #   dts: list of data.tables
  #   fun: function to be used
  #
  # Returns:
  #   data.table, but not explicitly
  dts |> # Use all the data.tables
    lapply(fun) |> # Use function to all of them
    invisible() # Hide printing
  
}

#Remove Wikipedia references from all cells
clean_up(albums, remove_references)
  
## CONCLUSION PART 4
# Now all the Wikipedia references/citations
# have been removed.

# 5. FIXING THE HEADERS -------------------------------------------------------
# Because all the original HTML tables have a partial double header, the more
# useful part of header is currently the first row of the table. That works
# well as the actual header.

first_row_to_header <- function(dt){
  # Replaces the headers with the first row, since
  # original table used double header.
  #
  # Args:
  #   dt: data.table
  #
  # Returns:
  #   data.table, but not explicitly
  setnames(dt, # data.table to be used
           as.character( # Convert data.table to a character vector
             dt[1])) # First row will be the new names
}

# Sets first row as headers
clean_up(albums, first_row_to_header)

## CONCLUSION PART 5
# Headers have been fixed

# 6.REMOVE LAST AND FIRST ROW -------------------------------------------------



#Helpers, to be removed:
View(albums[["studio"]])
View(albums[["live"]])
View(albums[["compilation"]])
View(albums[["single"]])
