# Pull Random Streets -----------------------------------------------------
segs <- read_csv("Street_Segments.csv")
names(segs) <- tolower(names(segs))

segs <- segs %>% 
  select(registeredname, streettype, quadrant, fromaddresslefttheo, 
         toaddresslefttheo)


se <- subset(segs, quadrant == "SE")
nw <- subset(segs, quadrant == "NW")
ne <- subset(segs, quadrant == "NE")
sw <- subset(segs, quadrant == "SW")

set.seed(1)
nw_sample <- sample_n(nw, 40)
se_sample <- sample_n(se, 20)
ne_sample <- sample_n(ne, 25)
sw_sample <- sample_n(sw, 15)

sample <- rbind(nw_sample, se_sample, ne_sample, sw_sample)

sample$select <- round(((sample$fromaddresslefttheo + 
                       sample$toaddresslefttheo) / 2), digits = 0)

address <- data.frame(street_num = as.character(sample$select),
                      street_name1 = as.character(sample$registeredname),
                      street_name2 = as.character(sample$streettype),
                      quadrant = as.character(sample$quadrant))

address$street_num[is.na(address$street_num)] <- 0

street.url.frame <- "http://citizenatlas.dc.gov/newwebservices/locationverifier.asmx/findLocation?str=AAA+BBB+CCC+DDD"

street.url.frame1 <- str_replace_all(street.url.frame, 
                                     pattern = "AAA", 
                                     replacement = address$street_num)

street.url.frame2 <- str_replace_all(street.url.frame1, 
                                     pattern = "BBB", 
                                     replacement = address$street_name1)

street.url.frame3 <- str_replace_all(street.url.frame2, 
                                     pattern = "CCC", 
                                     replacement = address$street_name2)

street.url <- str_replace_all(street.url.frame3, 
                              pattern = "DDD", 
                              replacement = address$quadrant)

street.datalist <- list()

for(i in 1:3){
  street.raw <- read_xml(street.url[i])
  street.raw.list <- as_list(street.raw)
  street.data <- as.data.frame(unlist(street.raw.list))
  
  street.datalist[[i]] <- 
    data.frame(address_id = street.data[2,],
             street_num = street.data[6,], 
             street_name = str_c(street.data[7,], " ", street.data[8,]), 
             quadrant = street.data[9,], 
             anc = street.data[15,],
             police_service_area = street.data[16,], 
             ward = street.data[17,], 
             neighborhood_cluster = street.data[19,], 
             police_district = street.data[20,], 
             police_sector = street.data[21,], 
             census_tract = street.data[22,], 
             voter_precinct = street.data[23,], 
             single_member_district = street.data[24,], 
             zip_code = street.data[25,], 
             roadway_id = street.data[27,], 
             lat = street.data[33,], 
             lon = street.data[34,], 
             residential_type = street.data[36,]) 
}

street <- do.call(rbind, street.datalist)

write_csv(street, "street.csv")


