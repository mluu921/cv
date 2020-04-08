library(tidyverse)
library(RefManageR)
library(googlesheets4)


pubs <- ReadPubMed('Michael Luu', retmax = 100) %>%
  as_tibble()

dat <- pubs %>%
  separate(
    author, c(paste0('author', 1:20)), sep = 'and'
  )

dat <- dat %>%
  mutate(
    across(author1:author20, ~ str_trim(.x))
  )


abbreviate_names <- function(x) {
  paste0(
    str_extract(x, '^.'),
    str_extract(x, '\\s\\w\\s') %>% str_trim() %>% str_replace_na(., ''),
    ' ',
    str_extract(x, '\\w+$')
  )
}

dat <- dat %>%
  mutate(
    across(author1:author20, ~ abbreviate_names(.x)),
    across(author1:author20, ~ ifelse(.x == 'NA NA', NA, .x))
  )

dat <- dat %>%
  unite('author', author1:author20, sep = ', ', na.rm = T) %>% 
  mutate(author = str_remove_all(author, ', NA, NA'))



dat <- dat %>%
  filter(
    !str_detect(author, 'MH Luu')
  )

out <- tibble(
  'section'	= 'academic_articles',
  'title'	= dat$title,
  'loc'	= dat$journal,
  'institution'	= '',
  'start'	= dat$year,
  'end'	= dat$year,
  'description_1'	= paste0('AUTHORS: ', dat$author),
  'description_2'	= paste0('DOI: ', dat$doi),
  'description_3'	= '',
  'in_resume' = ''
)

sheets_auth(
  email = 'mluu921@gmail.com'
)

googlesheets4::write_sheet(
  out, "https://docs.google.com/spreadsheets/d/1TX1PmIAqO0bjKlo9L_YbjTHFMqN4dDD9pmS0Ib1aG2c",
  sheet = 'publications'
)
