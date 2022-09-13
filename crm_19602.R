# CRM-19602
# reshape data from wide to long
# for Greek Officer contact info
# 2022-23-08



# read in CSV
greek_officers <- read.csv(file = "greek_officer_contact_update_form.csv")
greek_officers1 <- greek_officers%>% 
  select(-c("Serial", "Draft", "IP.Address", "Username"))

# # all phone numbers
# phone <- greek_officers %>% 
#   select(cols = starts_with("Phone"))

# remove punctuations from the phone numbers
greek_officers$Phone.Number <- gsub('[[:punct:] ]+',' ',greek_officers$Phone.Number)
greek_officers$Phone.Number.1 <-gsub('[[:punct:] ]+',' ',greek_officers$Phone.Number.1)
greek_officers$Phone.Number.2 <-gsub('[[:punct:] ]+',' ',greek_officers$Phone.Number.2)
greek_officers$Phone.Number.2 <-gsub('[[:punct:] ]+',' ',greek_officers$Phone.Number.3)
greek_officers$Phone.Number.4 <-gsub('[[:punct:] ]+',' ',greek_officers$Phone.Number.4)
greek_officers$Phone.Number.5 <-gsub('[[:punct:] ]+',' ',greek_officers$Phone.Number.5)
greek_officers$Phone.Number.6 <-gsub('[[:punct:] ]+',' ',greek_officers$Phone.Number.6)
greek_officers$Phone.Number.7 <-gsub('[[:punct:] ]+',' ',greek_officers$Phone.Number.7)
greek_officers$Phone.Number.8 <-gsub('[[:punct:] ]+',' ',greek_officers$Phone.Number.8)
greek_officers$Phone.Number.9 <-gsub('[[:punct:] ]+',' ',greek_officers$Phone.Number.9)
greek_officers$Phone.Number.10 <-gsub('[[:punct:] ]+',' ',greek_officers$Phone.Number.10)
greek_officers$Phone.Number.11 <-gsub('[[:punct:] ]+',' ',greek_officers$Phone.Number.11)
greek_officers$Phone.Number.12 <-gsub('[[:punct:] ]+',' ',greek_officers$Phone.Number.12)
greek_officers$Phone.Number.13 <-gsub('[[:punct:] ]+',' ',greek_officers$Phone.Number.13)
greek_officers$Phone.Number.14 <-gsub('[[:punct:] ]+',' ',greek_officers$Phone.Number.14)
greek_officers$Phone.Number.15 <-gsub('[[:punct:] ]+',' ',greek_officers$Phone.Number.15)
greek_officers$Phone.Number.16 <-gsub('[[:punct:] ]+',' ',greek_officers$Phone.Number.16)
greek_officers$Phone.Number.17 <-gsub('[[:punct:] ]+',' ',greek_officers$Phone.Number.17)

# remove white spaces
greek_officers$Phone.Number <- gsub(" ", "", greek_officers$Phone.Number)
greek_officers$Phone.Number.1 <-gsub(" ", "",greek_officers$Phone.Number.1)
greek_officers$Phone.Number.2 <-gsub(" ", "",greek_officers$Phone.Number.2)
greek_officers$Phone.Number.2 <-gsub(" ", "",greek_officers$Phone.Number.3)
greek_officers$Phone.Number.4 <-gsub(" ", "",greek_officers$Phone.Number.4)
greek_officers$Phone.Number.5 <-gsub(" ", "",greek_officers$Phone.Number.5)
greek_officers$Phone.Number.6 <-gsub(" ", "",greek_officers$Phone.Number.6)
greek_officers$Phone.Number.7 <-gsub(" ", "",greek_officers$Phone.Number.7)
greek_officers$Phone.Number.8 <-gsub(" ", "",greek_officers$Phone.Number.8)
greek_officers$Phone.Number.9 <-gsub(" ", "",greek_officers$Phone.Number.9)
greek_officers$Phone.Number.10 <-gsub(" ", "",greek_officers$Phone.Number.10)
greek_officers$Phone.Number.11 <-gsub(" ", "",greek_officers$Phone.Number.11)
greek_officers$Phone.Number.12 <-gsub(" ", "",greek_officers$Phone.Number.12)
greek_officers$Phone.Number.13 <-gsub(" ", "",greek_officers$Phone.Number.13)
greek_officers$Phone.Number.14 <-gsub(" ", "",greek_officers$Phone.Number.14)
greek_officers$Phone.Number.15 <-gsub(" ", "",greek_officers$Phone.Number.15)
greek_officers$Phone.Number.16 <-gsub(" ", "",greek_officers$Phone.Number.16)
greek_officers$Phone.Number.17 <-gsub(" ", "",greek_officers$Phone.Number.17)

# # keep phone numbers as numeric
# more elegant

phone_it_in <- function(phone, invalid = NA)
{
  phone <- gsub("[[:punct:]]", "", phone)          # remove punctuation
  phone <- trimws(phone)                           # remove whitespace
  phone[!nchar(phone) %in% c(7, 10)] <- invalid    # keep only 7 or 10 digit numbers
  phone[nchar(phone) %in% 7] <- gsub("(^\\d{3})(\\d{4}$)", 
                                     "\\1-\\2", 
                                     phone[nchar(phone) %in% 7])
  phone[nchar(phone) %in% 10] <- gsub("(^\\d{3})(\\d{3})(\\d{4}$)", 
                                      "\\1-\\2-\\3",
                                      phone[nchar(phone) %in% 10])
  phone
}

phone <- c("(123)-456-7890", "1234567890", "456890", "456-7890", 
          "+1 302-285-8022", "520-999-8888/332-189-1740")
phone_it_in(phone)

# more elegant
greek_officers$Phone.Number <- trimws(gsub("[[:punct:]]", "", greek_officers$Phone.Number))
# 
# greek_officers$Phone.Number[!nchar(greek_officers$Phone.Number) %in% c(10)] <- NA

# greek_officers$Phone.Number.17 <-str_extract_all(greek_officers$Phone.Number.17,"[0-9]")
# Email <- greek_officers %>%  
#   pivot_longer(-c(Serial, SID, Submitted.Time, Completed.Time, Modified.Time, Draft, IP.Address, UID))  %>%  
#   mutate(Email = sub('.*(Email).*', '\\1', name))

# greek_officers %>% 
#   pivot_longer(-c(Serial, SID, Submitted.Time, Completed.Time, Modified.Time, Draft, IP.Address, UID, Username), 
#                names_pattern = "(Email.*)", names_to = c("Email")) %>% 
#   pivot_wider()

# try with unite
# greek_officers %>% 
#   unite(First.Name, First.Name.1, First.Name.3, First.Name.4, First.Name.5, First.Name.6, First.Name.7, ...) %>% 
#   separate_rows(2:4, sep = "_")

# greek_officers %>% 
#   rownames_to_column("Emails") %>%
#   gather(Emails, values, -id)

# Email <- greek_officers %>%
#   pivot_longer(
#     cols = starts_with("Email"),
#     names_to = "Emails",
#     names_prefix = "Email",
#     values_to = "Email",
#     values_drop_na = TRUE
#   )
# 
# Phone <- Email %>% 
#   pivot_longer(
#     cols = starts_with("Phone"),
#     names_to = "Phone",
#     names_prefix = "Phone",
#     values_to = "Phone_number",
#     values_drop_na = TRUE
#   )
# 
# First <- Phone %>% 
#   pivot_longer(
#     cols = contains("First"),
#     names_to = "First",
#     names_prefix = "First",
#     values_to = "First_name",
#     values_drop_na = TRUE
#   )
# 
# Last <- First %>% 
#   pivot_longer(
#     cols = contains("Last"),
#     names_to = "Last",
#     names_prefix = "Last",
#     values_to = "Last_name",
#     values_drop_na = TRUE
#   ) %>% 
#   distinct()

# https://stackoverflow.com/questions/73150008/pivot-longer-with-multiple-new-columns

# df_raw %>% 
#   pivot_longer(cols = c(contains('lat'), contains('long'))) %>% 
#   pivot_longer(cols = contains('loc'), names_to = 'loc', values_to = 'loc_val') %>% 
#   filter(str_remove(name, '_(lat|long)') == str_remove(loc, '_(loc)')) %>% 
#   mutate(name = str_remove(name, '(off|pt)_')) %>% 
#   spread(name, value) %>% 
#   select(-loc)
# 
# 
# pivot_greek <- greek_officers %>% 
#   pivot_longer(cols = c(contains('Email'), contains('Phone'), contains('First'), contains('Last')))

# longer<-pivot_longer(greek_officers, cols=-c(SID, UID), 
#                      names_pattern = "(.*)(..)$", 
#                      names_to = c("First_name", "Last_name", "Phone_Number", "Email")) %>% 
#   mutate(limit=ifelse(limit=="", "value", limit))
# 
# answer <-pivot_wider(longer, id_cols = c(group, name), names_from = limit, values_from = value, names_repair = "check_unique")

library(dplyr)
library(tidyr)
# 
# longer1<- greek_officers %>% 
#   rename_with(~sub("^(First\\.Name|Last\\.Name|Phone\\.Number|Email)$", "values\\1", .)) %>%     # add prefix values
#   pivot_longer(greek_officers, cols=-c(1:16),
#                names_pattern = "(.*)(First\\.Name|Last\\.Name|Phone\\.Number|Email)$",
#                names_to = c(".value", "names")) 

# onger<-pivot_longer(greek_officers, 
# cols=-c(1:15), 
# names_pattern = "(.*)(...)", names_to = c("limit", "name")) %>% 
# mutate(limit=ifelse(limit=="", "value", limit))

# move email forward
greek_officers1 <- greek_officers1 %>% 
  relocate(Email, .after = UID)

longer <- pivot_longer(greek_officers1, 
                        cols=-c(1:6), 
                        names_pattern = "(.*)", names_to = c("question")) %>% 
  mutate(question=ifelse(question=="", "value", question))

write.csv(longer, "longer_greek_officers_form.csv")

###

longer1 <- pivot_longer(greek_officers1, 
                     cols=-c(1:11), 
                     names_pattern = "(.*)", names_to = c("question")) %>% 
  mutate(question=ifelse(question=="", "value", question))

write.csv(longer1, "longer1_greek_officers_form.csv")

# answer <-pivot_wider(longer, id_cols = c(group, name), names_from = limit, values_from = value, names_repair = "check_unique")

### from pivot advanced


# df_long <- greek_officers %>%
#   tidyr::pivot_longer(cols = -c(1:16), 
#                       names_to = "field", 
#                       values_to = "value")
# 
# df_long2 <- df_long %>% 
#   select(Submitted.Time, Completed.Time, Modified.Time, UID, 
#          Fraternity.or.Sorority, Fraternity, Sorority, field, value)

#### jeff's suggestion for cleaning up the data
longer1a <- greek_officers1 %>% 
  pivot_longer(cols = -c(1:12)) %>%
  mutate(record = substr(x = First, start = nchar(First), stop = nchar(First))) %>%
  mutate(record = if_else(record %in% c("e", "l", "r"),
                          true = "0",
                          false = record)) %>%
  mutate(record = as.numeric(record))

# Get rid of the record number suffixes from the name column
longer2a <- longer1a %>%
  mutate(name = gsub(pattern = "\\.[0-9]+$", 
                     replacement = "",
                     x = name))

longer3a <- longer2a %>%
  pivot_wider(id_cols = c(SID, record))

###
