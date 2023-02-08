library(tidyverse)
library(lubridate)
library(readxl)

sheets_file <- "Quarterly median rents by local government area - June quarter 2019.xlsx"
  
sheet_list <- excel_sheets(path = sheets_file)

importer <- function(sheet) {
  

data <- readxl::read_excel(sheets_file,sheet) 

months <- names(data %>% rename(`Dec 2002` = `Dec 2003...31`,
                                `Dec 2003` = `Dec 2003...39`) %>%
                  select(-contains("..."))) %>% parse_date(format = "%b %Y")

months_duplicated <- c("area","lga",sort(c(paste0(months,"_count"), paste0(months,"_median"))))

colnames(data) <- months_duplicated
data <- data[-1,]

data <- data %>% fill(area) %>% 
  pivot_longer(-c(area,lga), names_to = "a",values_to = "value") %>% 
  separate(a, into = c("year","metric"),sep = "_") %>% 
  mutate(series = tolower(sheet),
         property_type = case_when(str_detect(series,"flat") ~ "flat",
                   str_detect(series,"house") ~ "house",
                   str_detect(series,"all") ~ 'all'),
         bedrooms = parse_number(series),
         value = parse_number(value),
         year = parse_date(year),
         bedrooms_string = replace_na(bedrooms,"all"),
         bedrooms_string = paste(bedrooms_string, "bedrooms"))

}

data <- map_df(sheet_list,.f = importer)


data %>% 
  mutate() %>% 
  filter(lga =="Yarra",
         metric == "median") %>% 
  ggplot(aes(x = year, 
             y = value,
             colour = property_type,
             group = property_type)) +
  geom_line()+
  facet_wrap(~bedrooms_string)+
  theme_minimal()+
  scale_y_continuous(labels = scales::dollar_format())+
  labs(title = "Weekly median advertised rent for an apartment in Yarra",
       x = element_blank(),
       colour = "Type",
       caption = "Source: Vic Government")

data %>% 
  mutate() %>% 
  filter(lga  %in% c("Yarra","Melbourne"),
         metric == "median",
         property_type  == "flat")  %>% 
  arrange(year) %>% 
  group_by(series,lga) %>% 
  mutate(value = value/first(value)) %>% 
  ggplot(aes(x = year, 
             y = value,
             colour = lga)) +
  geom_line()+
  facet_wrap(~bedrooms_string)+
  theme_minimal()+
  labs(title = "Weekly median advertised rent for flats in Melbourne and Yarra",
       subtitle = "ratio from first data",
       x = element_blank(),
       colour = "Type",
       caption = "Source: Vic Government",
       y = element_blank())
data %>% 
  mutate() %>% 
  filter(lga  %in% c("Yarra","Melbourne","Darebin","Stonnington","Maribyrnong","Moreland"),
         metric == "median",
         property_type  == "flat")  %>% 
  arrange(year) %>% 
  ggplot(aes(x = year, 
             y = value,
             colour = lga)) +
  geom_line()+
  facet_wrap(~bedrooms_string)+
  theme_minimal()+
  scale_y_continuous(labels = scales::dollar_format())+
  labs(title = "Weekly median advertised rent for an apartment",
       x = element_blank(),
       colour = "Type",
       caption = "Source: Vic Government",
       y = element_blank())
