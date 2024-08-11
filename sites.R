# The following manual steps have to be performed ONCE:
# 0. Make sure you open R by clicking on one of the three files. That way the working directory is set correctly.
# 1. Install the required packages.
# 2. Download the official list of sites (http://whc.unesco.org/en/syndication) xls file and save it in the 
#    same folder as the three R files (the working directory)
# 3. Run code in this file up to line 60 (i.e. run only Section 1.1)
# 4. Unquote and run code in section 1.2 to copy only the following columns in a new csv file called unesco_data.csv:
#    unique_nb	id_no	name	description	justification	date_inscribed	secondary_dates	danger_list	longitude	latitude	category	state	region	iso_code	udnp_code	transboundary
#    (if you're from the US, replace write.csv2 with write.csv)
# 5. Run Section 2
# Afterwards, this file is only needed after updates to the official UNESCO list. Just run the entire file 
# (without Section 1.1).


# nice to know: https://en.wikipedia.org/wiki/List_of_World_Heritage_Sites_by_year_of_inscription


library(dplyr); library(reshape2); library(readxl); library(stringr)

### ------ read in an clean up data (for 2023) -----
dat = data.frame(read_excel("orig/whc-sites-2023.xls"))

dat = transmute(dat, unique_nb=unique_number, id_no=id_no, name=name_en, 
             description=short_description_en, justification=justification_en,
             date_inscribed=date_inscribed, secondary_dates=secondary_dates,
             danger_list=danger_list, longitude=longitude, latitude=latitude,
             category=category, state=states_name_en, region=region_en, 
             iso_code=iso_code, udnp_code=as.character(udnp_code), 
             transboundary=transboundary)
for (x in c("<p>","</p>","\n","<em>","</em>","<I>","</I>","<i>","</i>")){
  dat$description   = gsub(x,"",dat$description)
  dat$justification = gsub(x,"",dat$justification)
  dat$name          = gsub(x,"",dat$name)
}; rm(x)


### --- Section 1.1: Manual correction of missing/ wrong location values (based on my own research) ----
dat[grep("Bauhaus and its Sites",dat$name),c("longitude","latitude")] = c(12.14,51.5)
dat[grep("Primeval Beech Forests",dat$name),c("longitude","latitude")] = c(8.58,51.8)
dat[grep("Frontiers of the Roman",dat$name),c("longitude","latitude")]=c(8.5,50.27)
dat[grep("Venetian Works of Defence",dat$name),c("longitude","latitude")]=c(18.771043,42.424884)
dat[grep("Prehistoric Pile Dwellings around the Alps",dat$name),c("longitude","latitude")]=c(9.22831,47.72634)
dat[grep("Melaka and George Town",dat$name),c("longitude","latitude")]=c(102.240143, 2.200844)
dat[grep(1613,dat$id_no),c("longitude","latitude")]=c(7.71924, 50.3340619)

x=NULL
for(i in 1:nrow(dat)){
  if(length(which(round(dat[,"latitude"],3) == round(dat[i,"latitude"],3) &
      round(dat[,"longitude"],3) == round(dat[i,"longitude"],3))) > 1){
    x = rbind(x, dat[i,c("name","longitude","latitude")])
    x = rbind(x, dat[which(round(dat[,"latitude"],3) == round(dat[i,"latitude"],3) &
                           round(dat[,"longitude"],3) == round(dat[i,"longitude"],3)),
                     c("name","longitude","latitude")])
  }
};

dat[grep("Jongmyo",dat$name),c("longitude","latitude")] = c(126.9941, 37.5746)
dat[grep("Changdeokgung",dat$name),c("longitude","latitude")] = c(126.9927, 37.5823)

rm(i,x)

### ------- Section 1.2: Run only upon first usage (see readme)

# dat %>%
#   mutate(user1 = "",
#          user2 = "",
#          color = "",
#          color2= "") %>%
#   write.csv2("unesco_data.csv", row.names=F, na="")



##### ------ Section 2. Merging with personalized information (visiting dates) ---

history = read.csv2("unesco_data.csv", stringsAsFactors=F) %>%
  select(id_no, user1, user2) 

unesco_data = full_join(dat, history, by="id_no")

if(nrow(unesco_data) == nrow(dat)){
  if("name.x" %in% colnames(unesco_data)){
    unesco_data = unesco_data %>% 
      rename(name = name.x) %>%
      select(-name.y)
  }
  unesco_data = unesco_data %>%
    mutate(color = ifelse(category=="Cultural", "yellow",
                   ifelse(category=="Natural", "green", "greenyellow")),
           color2 = ifelse(is.na(danger_list), color,
                    ifelse(substr(danger_list,1,1) == "Y", "red", color)))
  
  write.csv2(unesco_data, "unesco_data.csv", row.names = F, na="")
  
  }else{
  print("Attention: Differing number of rows")
}

# ### If the number of rows differs, the following code can be used for debugging
# 
# err = unesco_data$id_no[which(!(unesco_data$id_no %in% dat$id_no))]
# 
# unesco_data = unesco_data %>%
#   filter(id_no != err)
# 
# ### after running the code again, the row number should be equal
