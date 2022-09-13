######## R snippet lesson for percentages 
# https://github.com/MCMaurer/R_snippets/tree/main/ggplot

test1 %>% 
  count(Biomarker, Communication) %>% 
  groupby(Biomarker) %>% 
  mutate(sum_n = sum(n),
         prop = n/sum_n)

# prop is a column in our data
# regex parenthesis does something
phone_num %>%  
  mutate(clean_num = str_remove_all(phone_num, "\\(|\\)|\\+1|-|") %>%
   str_trim(), 
   area_code = str_sub(clean_num, 1,3),
   extension = str_sub(clean_num, 4,6),
   last4 = str_sub(clean_num, 7, 10)) # gets the first 3 to get the area code 
   

# multiple phone numbers
phone_nums %>% 
  row = row_number() %>% 
  pivot_longer = cols(-everything) %>% 
  mutate(across(contains("phone"), .fns = list(
                clean = 
                  ~str_remove_all(.x,  "\\(|\\)|\\+1|-|") %>% 
                str_trim()
                )))


#### maria's query
# https://github.com/MCMaurer/R_snippets

invalid_to_NA <- function (col, valid_vals = 1:7){
  col[!(col %in% valid_vals)] <- NA
  return(col)
}

rescale <- function(x, min = 1, max = 7) {
  (x-min)/(max-min)
}

rescale(c(1,1,3,4,2,3,4,7,6,5))
rescale(c(1,1,3,4,2,3,4), max =4)

###
d <- tibble(response = c(1, 4, 3, 7, -9, 10, NA, 5),
            response2 = c(1, NA, -20, 6, -9, 10, NA, 3),
            response3 = c(1, 24, 2, 2, -9, 11, NA, 1),
            rowID = 1:8)

invalid_to_NA <- function(col, valid_vals = 1:7){
  
  col[!(col %in% valid_vals)] <- NA
  
  return(col)
}

invalid_to_NA(d$response)
###
d <- data.frame(response = c(1, 4, 3, 7, -9, 10, NA, 5),
                response2 = c(1, NA, -20, 6, -9, 10, NA, 3),
                response3 = c(1, 24, 2, 2, -9, 11, NA, 1),
                rowID = 1:8)

invalid_to_NA <- function(col, valid_vals = 1:7){
  
  col[!(col %in% valid_vals)] <- NA
  
  return(col)
}

invalid_to_NA(d$response)
####
rescale <- function(x, min = 1, max = 7){
  
  (x - min) / (max - min)
  
}

rescale(c(1,1,1,2,4,5,1,7,3,5))

rescale(c(1,1,3,4,1,1,2,2,1,3,4), max = 4)
####
c %>% 
  mutate(response = ifelse(response %in% 1:7, response, NA))

#### does the same thing in tidyverse
c %>% 
  mutate(across(starts_with("response"),
                list(clean = ~ ifelse(.x %in% 1:7, .x, NA)))) 
# wrap the function as a list to get the new columns underscore clean

d[match_cols] <- sapply(d[match_cols], invalid_to_NA)
d[paste0(colnames(d)[match_cols], "_clean")] <- sapply(d[match_cols], invalid_to_NA)
