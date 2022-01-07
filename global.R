# Reading in OS Shared Storefront Owners ----
library(httr)
library(DT)
library(jsonlite)

url <- "https://api.opensea.io/api/v1/assets"

get_owners <- function(asset_contract_address, collection_name, collection_size, url = url){ 
  
  n = collection_size
  
  owners_list <- list()
  
  # Pull Data via REST API w/o API Key - rate limited 
  for(i in 0:ceiling(n/50)){
  queryString <- list(
    asset_contract_address = asset_contract_address,
    order_direction = "desc",
    offset = as.character(i*50),
    collection = collection_name,
    limit = 50
  )
  
  response <- VERB("GET", url, 
                   query = queryString, 
                   content_type("application/octet-stream"))
  raw <- content(response, "text", encoding = "UTF-8")
  raw_content <- jsonlite::fromJSON(raw)
  
  owners_list <- c(owners_list, list(raw_content$assets))
  }
  
  # Drop extras in case collection size was estimated 
  drop_index <- unlist(lapply(owners_list, length)) == 0
  owners_list <- owners_list[!drop_index]
  
  # due to embedded lists within DF, need to pull only subset of columns 
  owners_tbl <- NULL
  for(i in 1:length(owners_list)){ 
    temp = owners_list[[i]][, c("name","id","token_id","num_sales")]
    rownames(temp) <- NULL
    owners_tbl <- rbind(owners_tbl, temp)
    rownames(owners_tbl) <- NULL
  }
  
  owners_tbl$owner_name <- NA
  owners_tbl$owner_address <- NA
  
  for(i in 1:nrow(owners_tbl)){
    temp_url <- paste0(
      "https://api.opensea.io/api/v1/asset/",
      asset_contract_address,"/",
      owners_tbl$token_id[i]
    )
    
    temp_response <- VERB("GET", temp_url, content_type("application/octet-stream"))
    temp_raw <- content(temp_response, "text", encoding = "UTF-8")
    temp_raw_content <- jsonlite::fromJSON(temp_raw)
    
    owners_tbl$owner_name[i] <- temp_raw_content$top_ownerships$owner$user$username
    owners_tbl$owner_address[i] <- temp_raw_content$top_ownerships$owner$address
  }
  
return(owners_tbl)
}

get_dt <- function(the_data){
  DT::datatable(data = the_data, extensions = 'Buttons', 
                options = list(dom = 'lrtip',
                               scrollX = TRUE, 
                               autoWidth = TRUE))
}

top_collectors <- function(the_data){ 
  temp <- as.data.frame(table(the_data$owner_address))
  colnames(temp) <- c("owner_address","amount_owned")
  return(temp[order(temp$amount_owned, decreasing = TRUE), ])
  }

