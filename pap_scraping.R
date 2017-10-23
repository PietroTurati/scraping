## Web Scraping Recipes from Websit
## Author: P.Turati
## Scrape Site: https://www.pap.fr/
#


rm(list = ls())

# install.packages("rvest")

library(rvest)
library(stringr)
# Scrape the list appartement references first ------------------------------------------------------------------


# url = "https://www.pap.fr/annonce/vente-appartements-paris-15e-g37782-du-2-pieces-au-3-pieces"
# Prepare the automation of the extraction process
# Part of the url change with the page

# Combine the different url
list.p2 <- c("vente-appartements",
             "appartement-a-vendre",
             "ventes-appartements",
             "vente-appartement-particulier",
             "immobilier-vente-appartement",
             "appartement-en-vente-paris",
             "achat-vente-appartement",
             "vente-d-appartement",
             "ventes-appart",
             "ventes-appartement-sans-agence"
)
list.page.nb <- c("", "-2","-3","-4","-5","-6","-7","-8","-9", "-10")
url.p1 <- "https://www.pap.fr/annonce/"
# url.p3 <- "-paris-15e-g37782-du-2-pieces-au-3-pieces" # Paris15
url.p3 <- "-paris-15e-g37782g43270-jusqu-a-800000-euros-a-partir-de-40-m2" #Paris15 + Issy

list.url <- paste0(url.p1,list.p2,url.p3,list.page.nb)

# Extract the list of references
ref_list <- NULL
for(j in 1:length(list.url)){
  # Read the page and select the box including the information of interest
  page = read_html(list.url[j])
  page.box = page %>% html_nodes(".box.search-results-item")
  
  # Extract the information in a raw format
  page.box.ref.date = page.box %>% html_nodes(".date") %>% html_text()
  
  ref_temp <- substr(page.box.ref.date,8,15)
  ref_list <- c(ref_list, ref_temp)
}

# Translate the ref code for url
url_ref <- ref_list %>%
          str_replace_all("/", "0") %>%
          str_replace("B", "r41")

url_root <- "https://www.pap.fr/annonce/vente-immobiliere-"
list_url_ref <- paste0(url_root, url_ref)

# Get information from an apartment page ----------------------------------

for(i in 1:length(list_url_ref)){
  page <- read_html(list_url_ref[i])
  page.box <- page %>% html_nodes(".box.box-details-annonce")
  
  
  # Ref
  ref <- ref_list[i]
  
  # Price
  prix <- page.box %>% html_nodes(".price") %>% html_text()
  
  # Date
  date <- page.box %>% html_nodes(".date") %>% html_text() %>% 
    str_replace_all("\t", "") %>% 
    str_split("  ")
  date <- date[[1]][2]
  # Add a function to translate the string in a real date
  
  df_page <- data.frame(ref = ref, date = date, pieces = NA, rooms = NA, surface = NA, metro = NA, prix = prix, description = NA)
  
  # Item-summary
  item_summary <- page.box %>% html_nodes(".item-summary") %>%
    html_text() %>% 
    str_replace_all("\t", "") %>% 
    str_split("\n")
  item_summary <- item_summary[[1]]
  item_summary <- item_summary[str_detect(item_summary, "")]
  
  for(j in 1:length(item_summary)){
    if(grepl("Pièce", item_summary[j]) | grepl("Piece", item_summary[j])){
      len <- nchar(item_summary[j])
      df_page$pieces <- substr(item_summary[j], len, len)
    }
    if(grepl("Chambre", item_summary[j])){
      len <- nchar(item_summary[j])
      df_page$rooms <- substr(item_summary[j], len, len)
    }
    if(grepl("Surf", item_summary[j])){
      df_page$surface <- substr(item_summary[j], 8, 10)
    }
    
  }
  
  if(i==1){
    df_ref <- df_page
  }else{
    df_ref <- rbind(df_ref, df_page)
  }
    
  
  
}


# Scrape all the information from the research page ------------------------------------------------------------------


# url = "https://www.pap.fr/annonce/vente-appartements-paris-15e-g37782-du-2-pieces-au-3-pieces"
# Prepare the automation of the extraction process
# Part of the url change with the page

# Combine the different url
list.p2 <- c("vente-appartements",
             "appartement-a-vendre",
             "ventes-appartements",
             "vente-appartement-particulier",
             "immobilier-vente-appartement",
             "appartement-en-vente-paris",
             "achat-vente-appartement",
             "vente-d-appartement",
             "ventes-appart",
             "ventes-appartement-sans-agence"
             )
list.page.nb <- c("", "-2","-3","-4","-5","-6","-7","-8","-9", "-10")
url.p1 <- "https://www.pap.fr/annonce/"
# url.p3 <- "-paris-15e-g37782-du-2-pieces-au-3-pieces"
url.p3 <- "-paris-15e-g37782g43270-jusqu-a-800000-euros-a-partir-de-40-m2"

list.url <- paste0(url.p1,list.p2,url.p3,list.page.nb)

# Extract the list of references
data.recherche <- NULL
for(j in 1:length(list.url)){
  # Read the page and select the box including the information of interest
  page = read_html(list.url[j])
  page.box = page %>% html_nodes(".box.search-results-item")
  
  # Extract the information in a raw format
  page.box.ref.date = page.box %>% html_nodes(".date") %>% html_text()
  
  page.box.price = page.box %>% html_nodes(".price") %>% html_text()
  
  page.box.piece.chambre.surface = page.box %>% html_nodes(".clearfix li") %>% html_text()
  
  page.box.transport = page.box %>% html_nodes(".item-transports") %>% html_text()
  
  page.box.explication = page.box %>% html_nodes(".item-description") %>% html_text()
  
  # page.box.link = page.box %>% html_nodes(".clearfix div a") %>% html_attr("href")
  
  # Reformat the data
  
  # number of annonces per page
  n <- length(page.box.price)
  page.box.ref.date
  
  # Remove appartement en construction
  idx.app.en.construction <-  page.box.price == ""
  
  page.box.price <- page.box.price[!idx.app.en.construction]
  page.box.explication <- page.box.explication[!idx.app.en.construction] 
  
  # Reformat details
  df.details <- data.frame(piece = seq(n)*NA, chambre = seq(n)*NA, surf = seq(n)*NA)
  idx <- 0
  for(i in 1:length(page.box.piece.chambre.surface)){
    if(grepl("Pièce", page.box.piece.chambre.surface[i]) | grepl("Piece", page.box.piece.chambre.surface[i])){
      idx <- idx + 1
      len <- nchar(page.box.piece.chambre.surface[i])
      df.details$piece[idx] <- substr(page.box.piece.chambre.surface[i], len, len)
    }
    if(grepl("Chambre", page.box.piece.chambre.surface[i])){
      len <- nchar(page.box.piece.chambre.surface[i])
      df.details$chambre[idx] <- substr(page.box.piece.chambre.surface[i], len, len)
    }
    if(grepl("Surf", page.box.piece.chambre.surface[i])){
      df.details$surf[idx] <- substr(page.box.piece.chambre.surface[i], 8, 9)
    }
    
  }
  df.details <- df.details[!idx.app.en.construction,]
  
  data.page <- data.frame(ref = substr(page.box.ref.date,8,15), date = substr(page.box.ref.date,20, 50),  df.details, px =  page.box.price, info = page.box.explication)
  data.recherche <- rbind(data.recherche, data.page)
}


# Data preparation --------------------------------------------------------
data.recherche$piece <- as.numeric(data.recherche$piece)
data.recherche$chambre <- as.numeric(data.recherche$chambre)
data.recherche$surf <- as.numeric(data.recherche$surf)
data.recherche$px <- as.numeric(data.recherche$px)


# Analyzed data -----------------------------------------------------------
data.analysis <- data.recherche[,1:6]

summary(data.analysis)
# Les annonce neuf ont une format different pieces vs piece


# Perfect game ------------------------------------------------------------


library(rvest)

url       <-"http://www.perfectgame.org/"   ## page to spider
pgsession <-html_session(url)               ## create session
pgform    <-html_form(pgsession)[[1]]       ## pull form from session

# Note the new variable assignment 

filled_form <- set_values(pgform,
                          `prix[min]` = "500000", 
                          `geo_objets_ids` = "439")

test = submit_form(pgsession,filled_form,'Actualiser')

page = read_html(pgsession)

page.box = page %>% html_nodes(".box.search-results-item")

page.box.price = page.box %>% html_nodes(".price") %>% html_text()
