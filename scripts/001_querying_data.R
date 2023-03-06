library(httr)
library(jsonlite)
library(tibble)
library(tidyverse)
library(ggtext) #add and modify text to ggpplot
library(showtext) #fonts
font_add_google("Lato")
showtext_auto()

#URLs
urls <- c(
  "https://www.data.brisbane.qld.gov.au/data/api/3/action/datastore_search?resource_id=8bbb460e-702a-443e-8b1f-52c52f31c4b1",
  "https://www.data.brisbane.qld.gov.au/data/api/3/action/datastore_search?resource_id=572bcc68-ad89-4cb8-99bf-0c267be6ff53",
  "https://www.data.brisbane.qld.gov.au/data/api/3/action/datastore_search?resource_id=f7c1b5ef-d862-4e29-a457-62ee5e0e529e",
  "https://www.data.brisbane.qld.gov.au/data/api/3/action/datastore_search?resource_id=17fc09cb-3609-4755-b7a3-1946d057d7e2",
  "https://www.data.brisbane.qld.gov.au/data/api/3/action/datastore_search?resource_id=4623b24d-ca40-47c0-b0d1-a1b84c51c27a",
  "https://www.data.brisbane.qld.gov.au/data/api/3/action/datastore_search?resource_id=365ca3c5-0a83-4fad-ba5c-bf4920682613",
  "https://www.data.brisbane.qld.gov.au/data/api/3/action/datastore_search?resource_id=22f660e8-febc-4446-a3e4-17c4bae30afb",
  "https://www.data.brisbane.qld.gov.au/data/api/3/action/datastore_search?resource_id=6c99f6ab-12e6-477b-a101-be3289ee1805",
  "https://www.data.brisbane.qld.gov.au/data/api/3/action/datastore_search?resource_id=95cde981-31b2-46bd-b66e-51bec39d07aa",
  "https://www.data.brisbane.qld.gov.au/data/api/3/action/datastore_search?resource_id=7afbc244-5c73-4d85-a566-4f2462757690",
  "https://www.data.brisbane.qld.gov.au/data/api/3/action/datastore_search?resource_id=b60fbe2e-f76d-474d-a49f-dfa35ef2aff1",
  "https://www.data.brisbane.qld.gov.au/data/api/3/action/datastore_search?resource_id=71d3f444-ed0b-4b7b-8c02-062489e5c687"
)

#Month names
month_names <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

#Function to extract data from each URL
extract_data <- function(url) {
  response <- GET(url)
  data <- fromJSON(content(response, "text"))
  
  #Extract data from the API
  records <- data$result$records
  
  return(records)
}

#Loops through each URL
data_list <- list()
for (i in seq_along(urls)) {
  data <- extract_data(urls[i])
  data_list[[month_names[i]]] <- data
}

#Data combination
all_data <- do.call(rbind, data_list)

#Convert row names to a new column called "Month"
df <- rownames_to_column(all_data, var = "Month")
df$Month <- gsub("\\..*", "", df$Month)

df_cleaned <- df %>%
  rename(am_peak = `AM Peak (7-9AM) seconds`,
        pm_peak = `PM Peak (4-7PM) seconds`,
        corridor = `Corridor Name`)


#save dataset
write.csv(df_cleaned, 'assets/df_cleaned.csv')


  
ggplot(data = df_cleaned, aes(x = factor(Month, levels = month_names), y = am_peak, group = corridor, color=corridor)) +
  geom_line()+
  #scale_color_manual(values = c("red", "blue")) +
  labs(title = "AM and PM Peak Variation Across Logan Road",
       x = "Month",
       y = "Peak Volume")
