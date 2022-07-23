#
# Data wrangling exercise with Joni Mitchell albums
# using R's data.table library.
# 


# Libraries ---------------------------------------------------------------

library(data.table) # for data wrangling
library(rvest) # for web scraping.
library(stringr) # for string manipulation


# CONSTANTS (all CONSTANTS are CAPITALIZED for clarity)------------------------

URL = "https://en.wikipedia.org/wiki/Joni_Mitchell_discography"

# There are 12 tables on the website, but I only want these four
ALBUM_POSITIONS = c(
  'studio'= 2,
  'live' = 3,
  'compilation' = 4,
  'single' = 6
)

# 1. URL to HTML tables -------------------------------------------------------

# Get table elements from URL
html_tables <- URL |>
  read_html() |> # Read URL
  html_elements("table") # Takes only <table>'s

# After this phase html_tables contains a list of all the 12 html tables
# from Joni Mitchell's discography page:
# 1. Info box
# 2. Studio albums,  
# 3. Live albums
# 4. Compilation albums
# 5. Extended plays
# 6. Singles
# 7. Guest singles
# 8. Video albums
# 9. Music videos
# 10. Navigation box
# 11. Archives series
# 12. Collaborations

