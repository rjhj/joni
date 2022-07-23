#
# Data wrangling exercise with Joni Mitchell albums
# using R's data.table library.
# 


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


