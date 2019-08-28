####This R Script will take the Oregon APACD from start to finished tables and figures####

# qsub -I -l nodes=1:ppn=1 -l mem=150gb -l walltime=100:00:00
# R

###################################################################################################################################################################################
####Section 0: Load the Libraries####
###################################################################################################################################################################################
#these packages need some special attention
# download the tar.gz package from CRAN and move it to /home/gwg/TMP. 
# Sys.setenv(TMPDIR = "/home/gwg/TMP")
# install.packages("lfe")
# Sys.setenv(TMPDIR = "/var/tmp")

library(data.table);library(ggplot2);library(zoo);library(lmtest);library(lfe);library(sandwich);library(gridExtra);library(grid);library(multiwayvcov)
library(matrixStats);library(reshape2);library(zipcode);library(geosphere);library(scales); library(xtable); library(stargazer); #library(mnlogit); library(mlogit)
library(foreach)
library(doMC)
#supress scientific notation
options(scipen=99)

###################################################################################################################################################################################
####Section 1: Load and clean the raw data and create basic variables ####
###################################################################################################################################################################################

#### OAPCD load ####
# rx_2011 = fread( "~/current/pharmacy_deserts/data/rx/mahoney_rx_2011.txt", header=T, sep="|",colClasses=list(character=c(27,28,32)), nThread = 5)
# rx_2012 = fread( "~/current/pharmacy_deserts/data/rx/mahoney_rx_2012.txt", header=T, sep="|",colClasses=list(character=c(27,28,32)), nThread = 5)
# rx_2013 = fread( "~/current/pharmacy_deserts/data/rx/mahoney_rx_2013.txt", header=T, sep="|",colClasses=list(character=c(27,28,32)), nThread = 5)
# 
# #append the three years of data
# rx = rbindlist(list(rx_2011, rx_2012, rx_2013)) 
# 
# #remove the year data
# rm(rx_2011, rx_2012, rx_2013)
# gc()
# 
# #### fix time vars ####
# #create a variable for the date each pharmacy first appears in the data
# rx[, pick_up_date := as.Date(fromdate)]
# rx[, pick_up_month := month(pick_up_date)]
# rx[, pick_up_year := year(pick_up_date)]
# rx[, pick_up_yrqtr := as.yearqtr(pick_up_date)]
# 
# #### pick up yrmon ####
# rx[, puym := paste(pick_up_year,pick_up_month, sep="/")]
# rx[, pick_up_yrmon := as.yearmon(puym, format = "%Y/%m")]
# rx[, c("puym","fromdate") := NULL]
# gc()
# 
# #### load the npi data ####
# npi = fread("~/current/pharmacy_deserts/data/npidata_20050523-20150607.csv")
# npi[, NPI := as.character(NPI)]
# #### name change ####
# rxn = rx
# rm(rx)
# gc()
# 
# 
# #### merge on the needed variables from the npi data ####
# #add the state and zip code of the pharmacy from the npi data
# setkey(rxn, npi)
# setkey(npi, NPI)
# npi[, enum_date := get("Provider Enumeration Date")]
# npi[, deact_date := as.Date(get("NPI Deactivation Date"), format = "%m/%d/%Y")]
# npi[, react_date := as.Date(get("NPI Reactivation Date"), format = "%m/%d/%Y")]
# npi[, deact_reason := get("NPI Deactivation Reason Code")]
# npi[, pharm_state := get("Provider Business Practice Location Address State Name")]
# npi[, pharm_zip := ifelse(nchar(get("Provider Business Practice Location Address Postal Code")) > 5,
#                           substr(get("Provider Business Practice Location Address Postal Code"),
#                                  1,
#                                  nchar(get("Provider Business Practice Location Address Postal Code")) - 4),
#                           get("Provider Business Practice Location Address Postal Code"))]
# npi[, pharm_address := get("Provider First Line Business Practice Location Address")]
# 
# rxn[npi, pharm_state := pharm_state]
# rxn[npi, pharm_zip :=  pharm_zip]
# rxn[npi, pharm_address :=  pharm_address]
# rxn[npi, enum_date :=  enum_date]
# rxn[npi, deact_date :=  deact_date]
# rxn[npi, deact_reason :=  deact_reason]
# rxn[npi, react_date :=  react_date]
# rxn[, enum_year := year(as.Date(enum_date, format = "%m/%d/%Y"))]
# rxn[, enum_month := month(as.Date(enum_date, format = "%m/%d/%Y"))]
# rm(npi)
# gc()
# 
# ####npi is too narrow a definition. I want to label pharmacies by their building address####
# rxn[, pharm_add_full := paste(pharm_address, pharm_zip, pharm_state,sep=",")]
# rxn[, pharm_add_full := ifelse(pharm_add_full == ",,", npi, pharm_add_full)]
# 
# #### get empirical open date and closing date ####
# rxn[, open_date := min(pick_up_date, na.rm=T), by = c("pharm_add_full")]
# rxn[, open_year := year(open_date)]
# rxn[, open_month := month(open_date)]
# #the empirical opening month
# rxn[, opening_year_mon := as.yearmon(open_date)]
# #closing
# rxn[, last_date := max(pick_up_date, na.rm=T), by = c("pharm_add_full")]
# rxn[, last_year := year(last_date)]
# rxn[, last_month := month(last_date)]
# #the empirical closing month
# rxn[, last_year_mon := as.yearmon(last_date)]
# 
# #### subset to remove patients who EVER have a missing zip code ####
# rxn[, unknown_zip_flag := ifelse(is.na(ZIP), 1, 0)]
# rxn[, unknown_zip_flag_max := max(unknown_zip_flag), by = personkey]
# # length(unique(rxn[unknown_zip_flag_max == 1 & (STATE == "OR" | STATE == "") ,]$personkey)) #2311
# # nrow(rxn[unknown_zip_flag_max == 1 & (STATE == "OR" | STATE == "") ,]) #52,474
# rxnk = rxn[unknown_zip_flag_max == 0, ] #101,971,766 obs
# rm(rxn)
# rxnk[, c("unknown_zip_flag_max", "unknown_zip_flag") := NULL]
# gc()
# 
# ####months since variables####
# ####pharmacy type####
# rxnk[, entity_broad := ifelse(grepl("WAL-MART", entity) | grepl("WALMART",entity), "walmart",
#                               ifelse(grepl("TARGET",entity), "target",
#                                      ifelse(grepl("FRED MEYER", entity), "fred_meyer",
#                                             ifelse(grepl("CVS",entity), "cvs",
#                                                    ifelse(grepl("SAFEWAY", entity), "safeway",
#                                                           ifelse(grepl("WALGREENS",entity), "walgreens",
#                                                                  ifelse(grepl("BI-MART",entity) | grepl("BIMART",entity), "bimart",
#                                                                         "other")))))))]
# 
# 
# ####oym_zip####
# #find the months since opening in each zip code
# #a dummy for if the pharm is a new pharm (appeared after jan 2011 and is located in Oregon)
# rxnk[, new_pharm := ifelse(opening_year_mon > as.yearmon("Jan 2011", format="%b %Y") & pharm_state == "OR", 1,0)]
# 
# #index the visits to each pharmacy
# setkey(rxnk, ZIP, pharm_add_full, pick_up_date)
# gc()
# rxnk[, zip_pharm_visit_index := seq_len(.N), by = c("ZIP","pharm_add_full")] #==1 means first date by patient in ZIP to pharm_add_full
# 
# #index the openings within each ZIP
# setorder(rxnk, ZIP, opening_year_mon, pharm_add_full) 
# rxnk[, zip_open_index_nf := NULL]
# rxnk[new_pharm == 1 #this pharmacy is a new pharmacy
#      & zip_pharm_visit_index == 1 #this is the first time someone from the zip visited the pharmacy
#      & ZIP == pharm_zip, #the pharmacy is located in the zip code where the patients live #this also drops out all the pharms w/o addresses
#      zip_open_index_nf := seq_len(.N), #data is ordered by pick up date so earlier means earlier open date
#      by = c("ZIP")]
# rxnk[, zip_open_index := NULL]
# rxnk[new_pharm == 1 #this pharmacy is a new pharmacy
#      & ZIP == pharm_zip, #the pharmacy is located in the zip code where the patients live #this also drops out all the pharms w/o addresses 
#      zip_open_index := na.locf(zip_open_index_nf),
#      by = pharm_add_full]
# #now each of the new pharmacies has an index ranking it by opendate in the zip code
# 
# #What is the max number of openings in a zip?
# max(rxnk$zip_open_index, na.rm=T) #4
# gc()
# 
# ####opening month for each of the 4 openings ####
# for (X in c(seq(1,max(rxnk$zip_open_index, na.rm=T)))) {
#   
#   nf = paste("open_ym_zip_",X,"_nf",sep="")
#   full = paste("open_ym_zip_",X,sep="")
#   
#   rxnk[, nf := NULL, with=F]
#   rxnk[zip_open_index == X,
#        nf := opening_year_mon,
#        with=F]
#   
#   rxnk[, full := NULL, with=F]
#   rxnk[, full := na.locf(get(nf)),
#        by = ZIP,
#        with=F]
#   gc()
#   
# }
# #now the patients who live in an opening zip code have opening dates for up to 4 pharamceis in their zip code
# 
# ####get the address and entity type of the new pharmacy####
# for (X in c(seq(1,max(rxnk$zip_open_index, na.rm=T)))) {
#   new_pharm_add_nf = paste("new_pharm_add_",X,"_nf",sep="")
#   new_pharm_add = paste("new_pharm_add_",X,sep="")
#   entity_nf = paste("entity_broad_",X,"_nf",sep="")
#   entity = paste("opening_entity_broad_",X,sep="")
#   
#   
#   rxnk[, new_pharm_add_nf := NULL, with=F]
#   rxnk[, new_pharm_add := NULL, with=F]
#   rxnk[, entity_nf := NULL, with=F]
#   rxnk[, entity := NULL, with=F]
#   rxnk[zip_open_index == X,
#        new_pharm_add_nf := pharm_add_full,
#        with=F]
#   rxnk[, new_pharm_add := na.locf(get(new_pharm_add_nf)),
#        by = ZIP,
#        with=F]
#   
#   rxnk[zip_open_index == X,
#        entity_nf := entity_broad,
#        with=F]
#   rxnk[, entity := na.locf(get(entity_nf)),
#        by = ZIP,
#        with=F]
#   
#   rxnk[, entity_nf := NULL, with=F]
#   rxnk[, new_pharm_add_nf := NULL, with=F]
#   gc()
# }
# 
# 
# ####ms_zip####
# rxnk[, ms_zip_1 := round((pick_up_yrmon - open_ym_zip_1) * 12)]
# rxnk[, ms_zip_2 := round((pick_up_yrmon - open_ym_zip_2) * 12)]
# rxnk[, ms_zip_3 := round((pick_up_yrmon - open_ym_zip_3) * 12)]
# rxnk[, ms_zip_4 := round((pick_up_yrmon - open_ym_zip_4) * 12)]
# 
# gc()
# 
# #check number of events
# nrow(unique(rxnk[new_pharm == 1 
#                  & last_year_mon >= opening_year_mon + .5 # I want the openings to last
#                  & !is.na(zip_open_index) #takes out the obs where other zip codes shopped at the pharm
#                  & opening_year_mon > "Jun 2011" & opening_year_mon < "Jul 2013" #this is so I have 6 months before and 6 after
#                  ,
#                  .(opening_year_mon, pharm_zip)]) ) #36
# 
# 
# 
# ####cym_zip####
# #Find the date of the closing in each zip code if there was one
# #find the months since closing in each zip code
# 
# #a dummy for if the pharm is a new pharm (appeared after jan 2011 and is located in Oregon)
# rxnk[, closing_pharm := ifelse(last_year_mon < as.yearmon("Dec 2013", format="%b %Y") & pharm_state == "OR", 1,0)]
# 
# 
# #index the closings within each ZIP
# setkey(rxnk, ZIP, last_year_mon,pharm_add_full) #sorted within zip by closing date
# 
# rxnk[, zip_close_index_nf := NULL]
# rxnk[closing_pharm == 1 #this pharmacy is a closing pharmacy
#      & zip_pharm_visit_index == 1 #this is the first time someone from the zip visited the pharmacy
#      & ZIP == pharm_zip, #the pharmacy is located in the zip code where the patients live #this also drops out all the pharms w/o addresses
#      zip_close_index_nf := seq_len(.N), #data is ordered by pick up date so lower means earlier close date
#      by = c("ZIP")]
# rxnk[, zip_close_index := NULL]
# rxnk[closing_pharm == 1
#      & ZIP == pharm_zip,
#      zip_close_index := na.locf(zip_close_index_nf),
#      by = pharm_add_full]
# #now each of the new pharmacies has an index ranking it by closedate in the zip code
# 
# #What is the max number of closings in a zip?
# max(rxnk$zip_close_index, na.rm=T) #2
# 
# ####closing month for each of the 2 closings ####
# for (X in c(seq(1,max(rxnk$zip_close_index, na.rm=T)))) {
#   nf = paste("close_ym_zip_",X,"_nf",sep="")
#   full = paste("close_ym_zip_",X,sep="")
#   
#   rxnk[, (nf) := NULL]
#   rxnk[zip_close_index == X,
#        (nf) := last_year_mon]
#   rxnk[, (full) := NULL]
#   rxnk[, (full) := na.locf(get(nf)),
#        by = ZIP]
#   gc()
#   
# }
# 
# ####get the address and entity type of the closing pharamcy####
# for (X in c(seq(1,max(rxnk$zip_close_index, na.rm=T)))) {
#   closing_pharm_add_nf = paste("closing_pharm_add_",X,"_nf",sep="")
#   closing_pharm_add = paste("closing_pharm_add_",X,sep="")
#   entity_nf = paste("entity_broad_",X,"_nf",sep="")
#   entity = paste("closing_entity_broad_",X,sep="")
#   
#   
#   rxnk[, (closing_pharm_add_nf) := NULL]
#   rxnk[, (closing_pharm_add) := NULL]
#   rxnk[, (entity_nf) := NULL]
#   rxnk[, (entity) := NULL]
#   rxnk[zip_close_index == X,
#        (closing_pharm_add_nf) := pharm_add_full]
#   rxnk[, (closing_pharm_add) := na.locf(get(closing_pharm_add_nf)),
#        by = ZIP]
#   
#   rxnk[zip_close_index == X,
#        (entity_nf) := entity_broad]
#   rxnk[, (entity) := na.locf(get(entity_nf)),
#        by = ZIP]
#   gc()
# }
# 
# 
# ####msc_zip####
# rxnk[, msc_zip_1 := round((pick_up_yrmon - close_ym_zip_1) * 12)]
# rxnk[, msc_zip_2 := round((pick_up_yrmon - close_ym_zip_2) * 12)]
# 
# 
# gc()
# 
# #check number of events
# nrow(unique(rxnk[closing_pharm == 1 
#                  & last_year_mon >= opening_year_mon + .5 # I want the closing to not be of flukes
#                  & !is.na(zip_close_index) #takes out the obs where other zip codes shopped at the pharm
#                  & last_year_mon > "Jun 2011" & last_year_mon < "Jul 2013" #this is so I have 6 months before and 6 after
#                  ,
#                  .(last_year_mon, pharm_zip)]) ) #17?
# 
# 
# #### drop out the adjustments with negative copays####
# rxnk[, adjust_flag := NULL]
# rxnk[, adjust_flag := ifelse((copay < 0 & !is.na(copay)) 
#                              | (rxdays <= 0 & !is.na(rxdays))
#                              | (qtydisp <= 0 & !is.na(qtydisp))
#                              | rxclass == "*NULL*" 
#                              | (paid < 0 & !is.na(paid))
#                              | (oop < 0 & !is.na(oop))
#                              | (coins < 0 & !is.na(coins)) #97,020,752 obs
#                              , 1, 0)]
# gc()
# #subsets added feb 8 2017: oop < 0, coins < 0 
# rxnka = rxnk[adjust_flag == 0, ] #97,020,752 obs
# 
# rxnka[,adjust_flag := NULL]
# rm(rxnk)
# gc()
# #why is there a negative value for the rxdays and the quantity dispensed?
# #  Sometimes submitters submit claims that are “reversals” or “adjustments” 
# # to the original claim.  Perhaps the original claim tried to bill $1000 for a drug,
# # but then after arbitration the provider agreed to accept $700. 
# # A second claim that adjusted the original claim would show up with 
# # negative values for cost and utilization measures (scripts, days, etc).
# 
# ####Same personkey, rxclass, ndc, pick up date, pharmacy = duplicate####
# rxnka[, duplicate_counter := seq_len(.N),
#       by = c("personkey","rxclass","ndc","pick_up_date","pharm_add_full","qtydisp","copay")]
# 
# rxnka = rxnka[duplicate_counter == 1,] #drops about 10 million obs
# #87,355,325 observations
# rxnka[, duplicate_counter := NULL]
# gc()
# 
# 
# ####remove vars we don't need ####
# names_to_rm = c("line" ,"clmstatus","medflag", "rxflag","MSA","paydate","pos","fromdate",
#                 "todate","rxcompound" ,"puym","year","shop_at_bad_npi_flag","shop_at_bad_npi_flag_max",
#                 "enum_date","days_btw_last_deact","days_btw_enum_open","open_ym_zip_1_nf",
#                 "open_ym_zip_2_nf","open_ym_zip_3_nf","open_ym_zip_4_nf" ,"new_pharm_add_1_nf" ,
#                 "entity_broad_1_nf","new_pharm_add_2_nf","entity_broad_2_nf","new_pharm_add_3_nf",
#                 "entity_broad_3_nf", "new_pharm_add_4_nf", "entity_broad_4_nf")
# rxnka[, (names_to_rm) := NULL]
# gc()
# 
# 
# ####Use ym_rx_z to specify index####
# 
# ####remove the outliers that are probably miscoded####
# # sort(unique(rxnka$qtydisp)) #some have over 1 million pills dispensed! (the measure must be in mg?)
# rxnka[, qtydisp_freq := .N, by=qtydisp]
# rxnka[, qtydisp_index := seq_len(.N), by=qtydisp]
# gc()
# rxnka[is.na(qtydisp), qtydisp := 0]
# vec = sort(quantile(rxnka$qtydisp,probs = seq(0,1,0.0001)))
# rxnka[, qtydisp_pctile := findInterval(qtydisp, vec)]
# 
# gc()
# #pills per day expected to take
# rxnka[, pills_per_day := qtydisp / rxdays]
# rxnka[, pills_per_day_pctile := findInterval(pills_per_day, 
#                                              sort(quantile(pills_per_day,
#                                                            probs = seq(0,1,0.001),
#                                                            na.rm=T)))]
# rxnka[, pills_per_day_index := seq_len(.N), by = pills_per_day]
# gc()
# 
# rxnka[, rxdays_index := seq_len(.N), by=rxdays]
# gc()
# 
# #days covered
# rxnka[is.na(rxdays), rxdays := 0]
# rxnka[, rxdays_pctile := findInterval(rxdays, 
#                                       quantile(rxdays,probs = seq(0,1,0.0001)),
#                                       rightmost.closed=TRUE)]
# 
# 
# ####drop the 0.9% of the data that is 10 or more pills per day####
# rxnkaw = rxnka[pills_per_day_pctile <= 991 & qtydisp_pctile < 9976 & rxdays_pctile <= 9992, ] 
# rxnkaw[,c("pills_per_day_pctile", "qtydisp_pctile", "rxdays_pctile", 
#           "rxdays_index","pills_per_day_index", "qtydisp_index") := NULL]
# rm(rxnka)
# gc()
# 
# #If I make the balance condition = 3 months, I get 46 events#####
# opening_zips = unique(rxnkaw[new_pharm == 1 
#                              & last_year_mon >= opening_year_mon + .5 # I want the openings to last
#                              & !is.na(zip_open_index) #takes out the obs where other zip codes shopped at the pharm
#                              & opening_year_mon > "Jun 2011" & opening_year_mon < "Jul 2013" #this is so I have 6 months before and 6 after
#                              ,
#                              ]$pharm_zip)
# 
# rxnkaw[pharm_zip %in% opening_zips, num_pharms_ym_pz := length(unique(pharm_add_full)), by = c("pharm_zip","pick_up_yrmon")]
# 
# rxnkaw[, ym_pz_index := seq_len(.N), by = c("pharm_zip","pick_up_yrmon")]
# 
# # pdf(file = "~/current/pharmacy_deserts/entry_events_variation.pdf", width = 10)
# plot = ggplot(data = rxnkaw[pharm_zip %in% opening_zips & ym_pz_index == 1]) +
#   geom_point(aes(x=pick_up_yrmon, y=num_pharms_ym_pz, color = as.factor(pharm_zip)), size =.5) +
#   geom_line(aes(x=pick_up_yrmon, y=num_pharms_ym_pz, color = as.factor(pharm_zip))) +
#   theme_bw() +
#   facet_wrap(~pharm_zip) +
#   theme(legend.position = "none",
#         text = element_text(size = 20),
#         axis.text.x = element_text(angle = 45, vjust = .5)) +
#   ylab("Number of pharmacies") + xlab("")
# print(plot)
# # dev.off()
# 
# gc()
# 
# 
# 
# ####create an rx2 field that is the first two digits of the rxclass####
# rxnkaw[, rx2 := substr(as.character(rxclass), 1, 2)]
# rxnkaw[, rx4 := substr(as.character(rxclass), 1, 4)]
# rxnkaw[, rx6 := substr(as.character(rxclass), 1, 6)]
# 
# 
# ####index the observations####
# rxnkaw[, ym_rx_z_index := seq_len(.N), 
#        by = c("pick_up_yrmon","rx2","ZIP")]
# 
# rxnkaw[, ym_z_index := seq_len(.N), 
#        by = c("pick_up_yrmon","ZIP")]
# 
# 
# 
# 
# #### CHECKPOINT ####
# rxnkaw[, c("medflag","rxflag","pos", "fromdate","todate","puym","shop_at_bad_npi_flag","shop_at_bad_npi_flag_max",
#            "pharm_address", "qtydisp_freq","pills_per_day") := NULL]
# save(rxnkaw, file="~/current/pharmacy_deserts/data/rxnkaw_master.RData") 
# gc()#############
a = Sys.time()
load("~/current/pharmacy_deserts/data/rxnkaw_master.RData", verbose =2) #92 vars #max 114gb used
b = Sys.time()
print(b - a) #Time difference of 12.4781 mins
gc()

####mail order####
mail_order_npi_list_1 = c(1003036435, 1003047507, 1003053174, 1003114968, 1003175266, 1003183401, 1003229550, 1003276429, 1003826694, 1003832056, 1003841842, 1003954223, 1013076520, 1013189422, 1013190487, 1013226687, 1013245562, 1013272095, 1013359785, 1013359785, 1013389535, 1013919240, 1013927177, 1013927557, 1013958990, 1013965953, 1013966993, 1013998913, 1023017647, 1023029618, 1023039088, 1023047826, 1023048790, 1023053261, 1023184660, 1023339454, 1023387701, 1023410149, 1023474889, 1033109319, 1033154257, 1033155494, 1033213327, 1033218615, 1033264130, 1033285796, 1033297288, 1033346564, 1033360342, 1033360342, 1033384003, 1033414842, 1033426234, 1033426424, 1033521109, 1033533591, 1043221633, 1043232044, 1043359854, 1043375975, 1043394612, 1043399272, 1043434939, 1043447204, 1043472145, 1043484009, 1043577935, 1043582968, 1043608268, 1043608268, 1043614217, 1043619604, 1043640550, 1043652654, 1043677834, 1053322693, 1053340893, 1053365544, 1053459206, 1053585257, 1053585927, 1053602664, 1053641787, 1053737015, 1053747915, 1063410389, 1063432672, 1063448280, 1063452829, 1063456374, 1063510097, 1063645026, 1063675684, 1063714111, 1063714434, 1063808962, 1063816601, 1063879096, 1063879096, 1063881399, 1063892032, 1073528626, 1073617189, 1073623633, 1073653762, 1073659637, 1073671608, 1073692745, 1073715140, 1073775169, 1073847463,
                          1073863312, 1073882387, 1073892014, 1073953873, 1073965729, 1083051353, 1083057038, 1083629620, 1083629695, 1083935233, 1083946123, 1083975486, 1093032468, 1093083206, 1093108805, 1093139875, 1093143570, 1093755423, 1093829079, 1093829335, 1093897456, 1093927675, 1104015395, 1104076694, 1104103290, 1104206382, 1104910363, 1104934967, 1104938992, 1104953942, 1104967066, 1104987312, 1114017019, 1114036944, 1114189206, 1114192358, 1114290582, 1114305307, 1114311818, 1114938628, 1114943776, 1114946274, 1114948171, 1114962438, 1114974797, 1124011226, 1124029442, 1124029442, 1124043963, 1124060793, 1124069943, 1124102256, 1124122841, 1124123500, 1124228176, 1124267117, 1124280227, 1124379425, 1124387337, 1134100134, 1134128838, 1134130842, 1134131352, 1134145295, 1134151699, 1134226178, 1134228745, 1134237126, 1134413560, 1134431638, 1134463425, 1134493810, 1134501059, 1144212408, 1144231788, 1144248568, 1144251943, 1144263310, 1144317330, 1144323403, 1144409095, 1144424177, 1144557828, 1144559592, 1144576463, 1144641655, 1144654245, 1144688318, 1154421493, 1154429181, 1154433167, 1154438877, 1154469146, 1154596682, 1154623023, 1154635050, 1154635050, 1154661320, 1164425336, 1164437406, 1164443628, 1164471405, 1164518163, 1164526125, 1164616215, 1164756813, 1164779617, 1164899696, 1174077408, 1174584544, 1174584544, 1174637144, 1174693105, 1174709349, 1174723241, 1174825103, 1174921803, 1174931943, 1174967269, 1184605099, 1184635310, 1184636359, 1184640294, 1184649782, 1184653388, 1184656621, 1184659286, 1184673428, 1184723884, 1184724395, 1184752388, 1184782955, 1184790149, 1184917429, 1194027177, 1194027177, 1194048918, 1194049494, 1194141176, 1194186056, 1194725705, 1194736769, 1194740902, 1194761585, 1194837047, 1194862359, 1194870824, 1194876581, 1194878223, 1205035011, 1205061538, 1205077401, 1205120706, 1205203312, 1205203312, 1205219789, 1205227519, 1205865508, 1205871522, 1205913597, 1205930260, 1215020805, 1215073820, 1215074414, 1215143490, 1215176201, 1215179189, 1215338058, 1215903182, 1215955802, 1215963913, 1225046956, 1225056567, 1225082472, 1225176431, 1225176530, 1225201932, 1225239940, 1225290323, 1225291875, 1225342405, 1225424070, 1235153826, 1235156027, 1235172404, 1235184078, 1235184086, 1235211467, 1235212564, 1235233289, 1235261405, 123537153)
mail_order_npi_list_1.5 = c(1235391335, 1235505272, 1245251214, 1245259282, 1245260934, 1245275452, 1245286756, 1245368448, 1245375377, 1245379411, 1245399161, 1245537430, 1245601061, 1255351607, 1255359865, 1255377610, 1255384426, 1255387767, 1255396842, 1255434205, 1255462156, 1255495438, 1255514287, 1255665907, 1255769881, 1255790978, 1265418891, 1265466155, 1265501472, 1265508360, 1265576391, 1265588446, 1265597447, 1265695852, 1265725808, 1265788707, 1265788707, 1265793012, 1265811665, 1265866727, 1265888630, 1275602484, 1275740474, 1275800781, 1275931404, 1285045302, 1285189076, 1285643171, 1285670679, 1285716647, 1285737411, 1285748178, 1285751651, 1285778357, 1285793653, 1285852939, 1285927178, 1285944744, 1285990887, 1295018794, 1295032639, 1295038438, 1295095875, 1295115335, 1295192565, 1295746709, 1295751147, 1295758688, 1295804920, 1295829190, 1295839843, 1295845147, 1295863884, 1295874246, 1295901197, 1306175823, 1306216866, 1306274949, 1306882774, 1306888037, 1306888755, 1306933270, 1306972708, 1306983564, 1316026164, 1316046915, 1316132723, 1316180193, 1316212731, 1316257264, 1316377443, 1316389950, 1316958952, 1316963267, 1316982531, 1326029232, 1326042342, 1326093899, 1326095852, 1326124389, 1326187543, 1326212291, 1326242470, 1326435603, 1326489071, 1336217249, 1336263029, 1336287630, 1336292283, 1336316736, 1336326784,
                            1336396480, 1336445360, 1336448182, 1336449511, 1336523745, 1336529551, 1336598218, 1346250610, 1346255965, 1346274693, 1346279015, 1346283520, 1346322575, 1346342607, 1346352267, 1346354727, 1346402435, 1346402609, 1346438124, 1346507514, 1356361745, 1356401582, 1356410567, 1356456701, 1356489348, 1356544258, 1356585368, 1356636401, 1356693436, 1356778336, 1356790364, 1366445892, 1366453953, 1366475022, 1366499881, 1366502841, 1366552960, 1366554206, 1366568628, 1366585424, 1366598575, 1366708935, 1366729451, 1366777492, 1366854788, 1376552851, 1376554907, 1376569939, 1376574558, 1376589911, 1376625731, 1376627091, 1376639195, 1376658740, 1376670109, 1376860866, 1386057990, 1386072197, 1386664829, 1386667186, 1386674067, 1386713337, 1386744407, 1386747897, 1386749166, 1386778090, 1386806404, 1386861318, 1386881290, 1386909547, 1386998623, 1396021598, 1396031001, 1396032322, 1396076535, 1396126413, 1396142964, 1396172128, 1396770236, 1396787172, 1396794483, 1396876793, 1396884342, 1396893103, 1407033855, 1407163736, 1407829369, 1407919806, 1407920929, 1417238684, 1417288614, 1417297185, 1417344789, 1417350984, 1417376765, 1417907627, 1417938820, 1417978479, 1427071745, 1427073246, 1427073790, 1427079474, 1427081876, 1427093616, 1427093632, 1427096809, 1427132265, 1427153899, 1427166446, 1427167477, 1427227552, 1427249598, 1427316157, 1437109444, 1437109444, 1437161999, 1437162740, 1437194933, 1437223955, 1437242831, 1437253168, 1437271194, 1437271194, 1437311529, 1437351384, 1437388089, 1437450236, 1437514429, 1437514544, 1437526316, 1447231345, 1447265046, 1447279435, 1447293485, 1447302963, 1447347646, 1447380183, 1447442900, 1447499926, 1447548524, 1447550132, 1447573928, 1447680236, 1447680236, 1457372229, 1457380206, 1457384927, 1457393050, 1457408783, 1457416000, 1457490765, 1457581902, 1457603078, 1457682460, 1457716144, 1467426783, 1467441808, 1467530873, 1467543165, 1467568337, 1467614370, 1467703991, 1467759266, 1467793752, 1467821173, 1477564060, 1477564375, 1477571404, 1477667467, 1477694834, 1477751675, 1477855773, 1477898419, 1487657839, 1487657995, 1487671558, 1487690863, 1487759767, 1487763637, 1487968301, 1487979928, 1487984043, 1497016471, 1497039390, 1497097968, 1497102479, 1497125959, 1497704431, 1497767966, 1497776447, 1497797963, 1508016197, 1508081100, 1508119918, 1508146010, 1508299710, 1508877143, 1508891532, 1508948381, 1518038215, 1518073832, 1518086339, 1518150598, 1518168608, 1518228980, 1518291269, 1518347020, 1518902725, 1518979947, 1518982347, 1518985233, 1528003910, 1528070992, 1528074168, 1528079100, 1528079167, 1528106713, 1528146644, 1528157179, 1528163227, 1528190626, 1528205218, 1528254885, 1528259108) 
mail_order_npi_list_2 = c(1528275724, 1528301561, 1528306032, 1528306032, 1528345519, 1528362076, 1528398393, 1528498490, 1538103981, 1538170659, 1538200266, 1538215744, 1538224951, 1538271531, 1538285101, 1538310776, 1538357314, 1538458088, 1538497235, 1538557178, 1538596788, 1548262470, 1548275183, 1548285448, 1548287337, 1548339757, 1548357742, 1548371610, 1548375587, 1548398803, 1548572092, 1548606403, 1548635683, 1558303859, 1558307942, 1558401851, 1558418863, 1558437137, 1558443911, 1558453290, 1558466243, 1558488924, 1558517508, 1558539056, 1558580241, 1558593269, 1558593269, 1558605196, 1558738864, 1558795385, 1568407880, 1568474716, 1568495463, 1568514180, 1568565075, 1568571420, 1568640928, 1568653988, 1568760114, 1568761963, 1568795102, 1568822021, 1568883924, 1578505897, 1578544474, 1578575379, 1578587234, 1578589073, 1578609327, 1578646519, 1578660411, 1578668331, 1578671509, 1578799201, 1578836698, 1578846051, 1578851044, 1578856340, 1578866737, 1578886461, 1578919767, 1578967782, 1588036826, 1588040869, 1588080881, 1588088066, 1588097729, 1588604078, 1588670996, 1588704605, 1588709455, 1588724728, 1588760540, 1588768535, 1588792865, 1588826655, 1588894281, 1588923700, 1598013864, 1598071581, 1598714503, 1598776098, 1598778797, 1598786543, 1598844706, 1598911265, 1598933590, 1609221647, 1609224120, 1609224120, 1609821644, 1609829100, 1609899590, 1609970664, 1609979954, 1619027034, 1619078698, 1619214616, 1619215381, 1619238607, 1619252160, 1619291671, 1619305976, 1619350972, 1619970845, 1619992427, 1629027867, 1629089131, 1629089362, 1629146865, 1629184957, 1629235882, 1629341177, 1629355482, 1629440631, 1639126139, 1639191315, 1639194459, 1639238751, 1639264229, 1639266836, 1639276561, 1639289572, 1639310600, 1639396732, 1639400070, 1639446149, 1639488422, 1639510217, 1649229410, 1649317579, 1649352006, 1649397019, 1649413352, 1649413832, 1649427139, 1649440512, 1649507815, 1649516113, 1649539578, 1649555749, 1649604166, 1649619222, 1649660630, 1649668286, 1659300861, 1659311629, 1659316859, 1659403749, 1659417202, 1659441624, 1659457992, 1659502151, 1659530541, 1659612737, 1659612885, 1659669976, 1659704617, 1659762524, 1669453569, 1669468435, 1669498515, 1669515763, 1669601878, 1669617734, 1669631198, 1669682233, 1669759650, 1669881777, 1679519854, 1679563720, 1679599336, 1679666002, 1679682686, 1679735765, 1679823876, 1679841423, 1679859862, 1679918049, 1679971485, 1689030470, 1689620932, 1689685794, 1689695900, 1689748303, 1689761462, 1689773764, 1689783656, 1689836777, 1689859217, 1699032573, 1699065797, 1699086546, 1699096842, 1699113902, 1699119313, 1699195164, 1699725739, 1699728634, 1699780205, 1699795500, 1699818211, 1699835827, 1699879296, 1699925271, 1700025707)
mail_order_npi_list_2.5 = c(1700026028, 1700074580, 1700095973, 1700124245, 1700168788, 1700181476, 1700181476, 1700218443, 1700233160, 1700245289, 1700895133, 1700955119, 1700967288, 1700971264, 1700988698, 1700996345, 1710013636, 1710021092, 1710022322, 1710033923, 1710074919, 1710085790, 1710089982, 1710142286, 1710276100, 1710371026, 1710900022, 1710902531, 1710904149, 1710912118, 1710922927, 1710923602, 1710927462, 1710992300, 1710996723, 1720018492, 1720024524, 1720096266, 1720134901, 1720149818, 1720150238, 1720161748, 1720171622, 1720187362, 1720244353, 1720317159, 1720356264, 1720384779, 1730100785, 1730137936, 1730194374, 1730228933, 1730228941, 1730237561, 1730261199, 1730272170, 1730273582, 1730276932, 1730295650, 1730298712, 1730303504, 1730311465, 1730321316, 1730431487, 1730562968, 1740201532, 1740230135, 1740236322, 1740239623, 1740261643, 1740295922, 1740327915, 1740359959, 1740385129, 1740390202, 1740395631, 1740491844, 1740582519, 1740630862, 1740687979, 1750306619, 1750362653, 1750462479, 1750493664, 1750517108, 1750533816, 1750533816, 1750592564, 1750659447, 1750686432, 1750686432, 1750699088, 1760464523, 1760529580, 1760557227, 1760586929, 1760623607, 1760716799, 1760773337, 1760793111, 1760835839, 1760889448, 1760889448, 1770504094, 1770509218, 1770509432, 1770525354, 1770529687, 1770599318, 1770628059, 1770666968, 1770676918, 1770696437, 1770733693, 1770799983, 1770839771, 1770883662, 1770893273, 1770916736, 1770931677, 1770953853, 1770973828, 1770983884, 1770994956, 1780015461, 1780030163, 1780614461, 1780617290, 1780629998, 1780699140, 1780811380, 1780846188, 1780936237, 1780953802, 1780980631, 1780988774, 1790034460, 1790049484, 1790066348, 1790100642, 1790132280, 1790147833, 1790150266, 1790730182, 1790732220, 1790734036, 1790795300, 1790796209, 1790799591, 1790819647, 1790824092, 1790824571, 1790926368, 1801058995, 1801197744, 1801807318, 1801817861, 1801817861, 1801830658, 1801937537, 1801971031, 1801998265, 1811097504, 1811141815, 1811145220, 1811199979, 1811201452, 1811397508, 1811924897, 1811930688, 1811940810, 1811946759, 1811985161, 1821009325, 1821010315, 1821010315, 1821019878, 1821024472, 1821033044, 1821120981, 1821192519, 1821220559, 1821286691, 1821323759, 1821391608, 1821400656, 1821438862, 1831100486, 
                            1831114453, 1831223544, 1831238062, 1831238880, 1831265594, 1831294214, 1831312297, 1831341577, 1831362607, 1831427301, 1831434331, 1831464726, 1831467711, 1831520006, 1841203007, 1841211802, 1841232410, 1841235603, 1841235850, 1841249182, 1841271756, 1841271764, 1841293651, 1841303195, 1841355229, 1841518248, 1841599289, 1841647179, 1841668530, 1841694510, 1851302400, 1851306476, 1851312177, 1851390496, 1851400824, 1851407738, 1851467518, 1851496715, 1851665954, 1851695936, 1851787741, 1861404139, 1861413189, 1861434490, 1861504854, 1861507980, 1861530099, 1861599896, 1861654949, 1861753865, 1861787798, 1861873598, 1871606152, 1871664896, 1871693119, 1871718650, 1871741421, 1871801449, 1871812420, 1871913327, 1871926030, 1871967315, 1871991422, 1881064772, 1881076842, 1881606093, 1881615839, 1881629780, 1881639755, 1881737039, 1881769149, 1881792364, 1881867943, 1881915031, 1881928000, 1881952851, 1891010724, 1891071098, 1891090726, 1891779211, 1891822003, 1891830030, 1891892303, 1891946679, 1891982096, 1891983672, 1902026198, 1902042054, 1902074438, 1902167596, 1902198724, 1902233257, 1902245947, 1902295801, 1902350150, 1902841224, 1902887805, 1902919145, 1902988363, 1902993926, 1912001017, 1912021213, 1912060740, 1912143934, 1912156183, 1912227703, 1912242629, 1912287731, 1912297060, 1912314642, 1912364282, 1912900226, 1912910100, 1912911330, 1912917550, 1912953431, 1912979287, 1922019421, 1922019561, 1922105766, 1922116805, 1922266873, 1922398791, 1932122918, 1932133535, 1932239662, 1932247178, 1932257599, 1932288438, 1932328192, 1932346103, 1932361029, 1932365293, 1932400397, 1932428547, 1932477809, 1932490695, 1932520863, 1932529823, 1942215587, 1942230131, 1942230131, 1942234448, 1942269956, 1942311329, 1942398995, 1942444799, 1942449335, 1942501655, 1942508460, 1942528278, 1942578240, 1942582572, 1942647151, 1942670831, 1952308280, 1952311433, 1952312449, 1952418725, 1952481103, 1952563058, 1952587396, 1952612848, 1952630840, 1952663676, 1962413468, 1962432641, 1962436592, 1962444737, 1962456194, 1962532689, 1962591875, 1962595793, 1962648261, 1962693234, 1962726059, 1962728360, 1962760827, 1962813089, 1962842062, 1962892349, 1972501294, 1972502102, 1972505113, 1972515757, 1972534154, 1972534154, 1972603827, 1972674141, 1972688463, 1972698702, 1972710176, 1972721926, 1972741825, 1972792331, 1972880789, 1982053690, 1982613428, 1982665923, 1982701819, 1982709044, 1982713566, 1982727038, 1982835856, 1982837514, 1982866075, 1982921235, 1982936886, 1992033575, 1992057756, 1992134381, 1992163141, 1992176192, 1992259246, 1992711782, 1992738686, 1992809560, 1992817217, 1992824874, 1992833198, 1992851174, 1992892194, 1992933907, 1992939607, 1992955157, 1992960595)

rxnkaw[, mail_order_dummy := ifelse(npi %in% c(mail_order_npi_list_1, mail_order_npi_list_1.5, mail_order_npi_list_2, mail_order_npi_list_2.5)
                                    | grepl("MAIL", entity) | grepl("mail", entity), 1, 0)]
print(mean(rxnkaw$mail_order_dummy)) #0.09837254

###################################################################################################################################################################################
####Section 2: generate outcomes ####
###################################################################################################################################################################################
####get the fill date for the next fill####
print("get the fill date for the next fill")
setorder(rxnkaw, personkey, rxclass, pick_up_date)
gc()
#index the fills for each person-drug combo
rxnkaw[, person_rxclass_index := seq_len(.N), by = .(personkey, rxclass)]

#new data set with only the dates and the person-drug combo and the index
lead_data = rxnkaw[,.(personkey, rxclass, pick_up_date, person_rxclass_index)]
#rename the date for merging
lead_data[, next_pick_up_date := pick_up_date]
#shift the index
lead_data[, lag_person_rxclass_index := person_rxclass_index - 1]
#drop the first fills
lead_data = lead_data[lag_person_rxclass_index != 0]
#merge back onto the full data on the lagged index
setkey(rxnkaw, personkey, rxclass, person_rxclass_index)
setkey(lead_data, personkey, rxclass, lag_person_rxclass_index)
rxnkaw[lead_data, next_pick_up_date := next_pick_up_date]
rm(lead_data)
gc()

#days uncovered####
print("#days uncovered####")
rxnkaw[, days_uncovered := as.numeric(next_pick_up_date - (pick_up_date + rxdays))]

#mpr####
print("#mpr####")
rxnkaw[, mpr := rxdays / as.numeric(next_pick_up_date - pick_up_date)]

  mean(rxnkaw[as.numeric(next_pick_up_date - pick_up_date) != 0]$mpr, na.rm=T)
  
  #does MPR change around the openings?####
  rxnkaw[as.numeric(next_pick_up_date - pick_up_date) == 0, mpr := NA]
  rxnkaw[, mean_mpr_ms_zip_1 := mean(mpr, na.rm=T), by = .(ms_zip_1)] #increases memory usage quite a bit
  (setorder(unique(rxnkaw[ms_zip_1 <= 7 & ms_zip_1 >= -7,.(mean_mpr_ms_zip_1, ms_zip_1)]),ms_zip_1))
  #seems like there is a pretty solid increase in mean MPR overall

  rxnkaw[, mean_days_uncovered_ms_zip_1 := mean(days_uncovered, na.rm=T), by = .(ms_zip_1)] #increases memory usage quite a bit
  (setorder(unique(rxnkaw[ms_zip_1 <= 7 & ms_zip_1 >= -7,.(mean_days_uncovered_ms_zip_1, ms_zip_1)]),ms_zip_1))
  #seems like there is a pretty solid decrease in mean days_uncovered overall
  
  #does MPR change around the closings?####
  rxnkaw[, mean_mpr_msc_zip_1 := mean(mpr, na.rm=T), by = .(msc_zip_1)] #increases memory usage quite a bit
  (setorder(unique(rxnkaw[msc_zip_1 <= 7 & msc_zip_1 >= -7,.(mean_mpr_msc_zip_1, msc_zip_1)]),msc_zip_1))
  gc()
  #linear increase in mpr

  rxnkaw[, mean_days_uncovered_msc_zip_1 := mean(days_uncovered, na.rm=T), by = .(msc_zip_1)] #increases memory usage quite a bit
  (setorder(unique(rxnkaw[msc_zip_1 <= 7 & msc_zip_1 >= -7,.(mean_days_uncovered_msc_zip_1, msc_zip_1)]),msc_zip_1))
  gc()
  #u shape? 
  
  #does MPR change around the walgreens?####
  rxnkaw[, mean_mpr_pick_up_yrmon := mean(mpr, na.rm=T), by = .(pick_up_yrmon)] #increases memory usage quite a bit
  (setorder(unique(rxnkaw[pick_up_yrmon >= as.yearmon("Jun 2011", format = "%b %Y") 
                          & pick_up_yrmon <= as.yearmon("Oct 2012", format = "%b %Y"),
                          .(mean_mpr_pick_up_yrmon, pick_up_yrmon)]),pick_up_yrmon))
  gc()
  #big jump down in jan 2012

  rxnkaw[, mean_days_uncovered_pick_up_yrmon := mean(days_uncovered, na.rm=T), by = .(pick_up_yrmon)] #increases memory usage quite a bit
  (setorder(unique(rxnkaw[pick_up_yrmon >= as.yearmon("Jun 2011", format = "%b %Y") 
                          & pick_up_yrmon <= as.yearmon("Oct 2012", format = "%b %Y"),
                          .(mean_days_uncovered_pick_up_yrmon, pick_up_yrmon)]),pick_up_yrmon))
  gc()
  #seems like a delayed decrease
  
  gc()

####existing patient####
print("####existing patient####")
#get the date of the patient first fill
setorder(rxnkaw, personkey, pick_up_date) #first fill of any drug

#index the fills within each person
rxnkaw[, person_fill_index := seq_len(.N), by = c("personkey")]
rxnkaw[person_fill_index == 1, person_first_fill_yrmon_nf := pick_up_yrmon]
rxnkaw[, person_first_fill_yrmon := na.locf(person_first_fill_yrmon_nf), by = c("personkey")]
rxnkaw[, person_last_fill_yrmon := max(pick_up_yrmon, na.rm=T), by = c("personkey")]

#need to make four dummies for if the patient was existing during the four openings
#I will need to appropriately stack these later
rxnkaw[, existing1 := as.numeric(person_first_fill_yrmon <= open_ym_zip_1 - 0.5 & person_last_fill_yrmon >= open_ym_zip_1 + 0.5)]
rxnkaw[, existing2 := as.numeric(person_first_fill_yrmon <= open_ym_zip_2 - 0.5 & person_last_fill_yrmon >= open_ym_zip_2 + 0.5)]
rxnkaw[, existing3 := as.numeric(person_first_fill_yrmon <= open_ym_zip_3 - 0.5 & person_last_fill_yrmon >= open_ym_zip_3 + 0.5)]
rxnkaw[, existing4 := as.numeric(person_first_fill_yrmon <= open_ym_zip_4 - 0.5 & person_last_fill_yrmon >= open_ym_zip_4 + 0.5)]

rxnkaw[, existing_c_1 := as.numeric(person_first_fill_yrmon <= close_ym_zip_1 - 0.5 & person_last_fill_yrmon >= close_ym_zip_1 + 0.5)]
rxnkaw[, existing_c_2 := as.numeric(person_first_fill_yrmon <= close_ym_zip_2 - 0.5 & person_last_fill_yrmon >= close_ym_zip_2 + 0.5)]

rxnkaw[, existing_w := as.numeric(person_first_fill_yrmon <= as.yearmon("Jan 2012") - 0.5 & person_last_fill_yrmon >= as.yearmon("Jan 2012") + 0.5)]

  #does MPR change around the openings?####
  nrow(rxnkaw[existing1==1])
  rxnkaw[existing1 == 1, mean_mpr_ms_zip_1_existing := mean(mpr, na.rm=T), by = .(ms_zip_1)] #increases memory usage quite a bit
  (setorder(unique(rxnkaw[existing1 == 1 & ms_zip_1 <= 7 & ms_zip_1 >= -7,.(mean_mpr_ms_zip_1_existing, ms_zip_1)]),ms_zip_1))
  gc()
  mpr_reg <- lm(mean_mpr_ms_zip_1_existing ~ as.factor(ms_zip_1) + as.factor(ZIP) + as.factor(pick_up_yrmon) - 1,
                data = rxnkaw[existing1 == 1 & ms_zip_1 <= 6 & ms_zip_1 >= -6 & !is.na(open_ym_zip_1)])
  round(summary(mpr_reg)$coefficients,3)
  #seems like there is a pretty solid increase in mean MPR overall
  
  rxnkaw[existing1 == 1, mean_days_uncovered_ms_zip_1_existing := mean(days_uncovered, na.rm=T), by = .(ms_zip_1)] #increases memory usage quite a bit
  (setorder(unique(rxnkaw[ms_zip_1 <= 7 & ms_zip_1 >= -7 & !is.na(mean_days_uncovered_ms_zip_1_existing),.(mean_days_uncovered_ms_zip_1_existing, ms_zip_1)]),ms_zip_1))
  gc()
  #seems like there is a pretty solid decrease in mean days_uncovered overall
  
  #does MPR change around the closings?####
  rxnkaw[, mean_mpr_msc_zip_1 := mean(mpr, na.rm=T), by = .(msc_zip_1)] #increases memory usage quite a bit
  (setorder(unique(rxnkaw[msc_zip_1 <= 7 & msc_zip_1 >= -7,.(mean_mpr_msc_zip_1, msc_zip_1)]),msc_zip_1))
  gc()
  #linear increase in mpr
  
  rxnkaw[, mean_days_uncovered_msc_zip_1 := mean(days_uncovered, na.rm=T), by = .(msc_zip_1)] #increases memory usage quite a bit
  (setorder(unique(rxnkaw[msc_zip_1 <= 7 & msc_zip_1 >= -7,.(mean_days_uncovered_msc_zip_1, msc_zip_1)]),msc_zip_1))
  gc()
  #u shape? 
  
  #does MPR change around the walgreens?####
  rxnkaw[, mean_mpr_pick_up_yrmon := mean(mpr, na.rm=T), by = .(pick_up_yrmon)] #increases memory usage quite a bit
  (setorder(unique(rxnkaw[pick_up_yrmon >= as.yearmon("Jun 2011", format = "%b %Y") 
                          & pick_up_yrmon <= as.yearmon("Oct 2012", format = "%b %Y"),
                          .(mean_mpr_pick_up_yrmon, pick_up_yrmon)]),pick_up_yrmon))
  gc()
  #big jump down in jan 2012
  
  rxnkaw[, mean_days_uncovered_pick_up_yrmon := mean(days_uncovered, na.rm=T), by = .(pick_up_yrmon)] #increases memory usage quite a bit
  (setorder(unique(rxnkaw[pick_up_yrmon >= as.yearmon("Jun 2011", format = "%b %Y") 
                          & pick_up_yrmon <= as.yearmon("Oct 2012", format = "%b %Y"),
                          .(mean_days_uncovered_pick_up_yrmon, pick_up_yrmon)]),pick_up_yrmon))
  gc()
  #seems like a delayed decrease
#####age####
print("#####age####")
rxnkaw[, age := pick_up_year - yob]

####merge on county-year mortality data####
print("####merge on county-year mortality data####")
load("~/current/pharmacy_deserts/data/zip_county_age_mortality.RData")
#keep only the totals
zc_wide_t = zc_wide[age_group == "Total"]
zc_wide_t[, ZIP := as.numeric(ZIP)]

rxnkaw[, year := year(pick_up_yrmon)]
setkey(rxnkaw, ZIP, year)
setkey(zc_wide_t, ZIP, year)

rxnkaw[zc_wide_t, ami_deaths := total_AcutemyocardialinfarctionI21I22]
rxnkaw[zc_wide_t, total_deaths := total_Total]
rxnkaw[zc_wide_t, other_artery_deaths := total_OtherdiseasesofarteriesarteriolesandcapillariesI72I78]
rxnkaw[zc_wide_t, suicides := `total_Intentionalselfharmsuicidebyotherandunspecifiedmeansandtheirsequelae*U03X60X71X75X84Y870` +
         total_IntentionalselfharmsuicidebydischargeoffirearmsX72X74]
rxnkaw[zc_wide_t, hypertensive_deaths := total_HypertensiveheartdiseaseI11]
rxnkaw[zc_wide_t, hf_deaths := total_HeartfailureI50]
rxnkaw[zc_wide_t, hypertensive_renal_deaths := total_EssentialhypertensionandhypertensiverenaldiseaseI10I12I15]
rxnkaw[zc_wide_t, diab_deaths := total_DiabetesmellitusE10E14]
rxnkaw[zc_wide_t, other_heart_deaths := total_AllotherformsofheartdiseaseI26I28I34I38I42I49I51]
rxnkaw[zc_wide_t, county_name := county_name]
rxnkaw[zc_wide_t, county := COUNTY]

rxnkaw[, total_heart_deaths := ami_deaths + other_artery_deaths + hypertensive_deaths + hf_deaths +
         hypertensive_renal_deaths + other_heart_deaths]
rm(zc_wide, zc_wide_t)
gc()

####insurance_type####
print("####insurance_type####")
rxnkaw[, insurance_type_53 := paste(paytype, apac_payer, prod, sep="+")]
rxnkaw[, insurance_type_18 := paste(paytype, apac_payer,  sep="+")]
rxnkaw[, insurance_type_23 := prod]
rxnkaw[, insurance_type_8 := apac_payer]
rxnkaw[, insurance_type_6 := paytype]
gc()


####30-day and 90-day dummies####
print("####30-day and 90-day dummies####")
rxnkaw[, rxdays_30 := ifelse(rxdays == 30 , 1, 0)]
rxnkaw[, rxdays_90 := ifelse(rxdays == 90 , 1, 0)]
rxnkaw[, rxdays_180 := ifelse(rxdays == 180 , 1, 0)]
rxnkaw[, rxdays_other := ifelse(rxdays_30 == 0 & rxdays_90 == 0 & rxdays_180 == 0, 1, 0)]
rxnkaw[, rxdays_greater_than_30 := ifelse(rxdays > 30, 1, 0)]
rxnkaw[, rxdays_broad := ifelse(rxdays_30 == 1, "30", ifelse(rxdays_90 == 1, "90", "other"))]

gc()

####dummy for if the zip code was a walgreens zip code####
print("####dummy for if the zip code was a walgreens zip code####")
rxnkaw[, walgreens := ifelse(grepl("WALG", entity), 1, 0)]
rxnkaw[is.na(walgreens), walgreens := 0]
walgreens_zips = unique(rxnkaw[walgreens == 1,]$pharm_zip)
rxnkaw[, walgreens_zip_person := ifelse(ZIP %in% walgreens_zips,1,0)]

walgreens_counties = unique(rxnkaw[walgreens == 1,]$county)
rxnkaw[, walgreens_county_person := ifelse(county %in% walgreens_counties,1,0)]

####number of walgreens in each zip code####
print("####number of walgreens in each zip code####")
rxnkaw[walgreens == 1, pz_ym_pharmadd_index := seq_len(.N), by = c("pharm_zip","pick_up_yrmon","pharm_add_full")]
rxnkaw[pz_ym_pharmadd_index==1, num_walg_pz_ym_nf := sum(pz_ym_pharmadd_index), by = c("pharm_zip","pick_up_yrmon")]
rxnkaw[, num_walg_pz_ym := na.locf(num_walg_pz_ym_nf), by = c("pharm_zip","pick_up_yrmon")]
rxnkaw[is.na(num_walg_pz_ym), num_walg_pz_ym := 0]

rxnkaw[,num_walg_z_ym:=NULL]
rxnkaw[,num_walg_z_ym_nf:=NULL]
rxnkaw[pharm_zip == ZIP & !is.na(pharm_zip), num_walg_z_ym_nf := num_walg_pz_ym]
rxnkaw[, num_walg_z_ym := na.locf(num_walg_z_ym_nf), by = c("ZIP","pick_up_yrmon")]

rxnkaw[,c("num_walg_pz_ym_nf","num_walg_z_ym_nf","pz_ym_pharmadd_index") := NULL]

####number of walgreens in each county####
print("####number of walgreens in each county####")
rxnkaw[, max_walg_z_y := max(num_walg_z_ym), by=c("year","ZIP")]

rxnkaw[, z_y_index := seq_len(.N), by = c("ZIP","year")]
rxnkaw[, num_walg_c_y_nf := NULL]
rxnkaw[z_y_index==1,
       num_walg_c_y_nf := sum(max_walg_z_y), 
       by = c("year","county")]
rxnkaw[, num_walg_c_y := na.locf(num_walg_c_y_nf),
       by = c("year","county")]

gc()

####months since the walgreens-expess scripts separation####
rxnkaw[, ms_wal := round((pick_up_yrmon - as.yearmon("Jan 2012"))*12)]

####if the event opening was a not pharmacy only, expand the entity broad to all obs in the event####
print("####if the event opening was a not pharmacy only, expand the entity broad to all obs in the event####")
for (X in c(seq(1,max(rxnkaw$zip_open_index, na.rm=T)))){
  grocery = paste("grocery",X,sep="_")
  opening_entity_broad = paste("opening_entity_broad",X,sep="_")
  
  rxnkaw[, grocery := NULL, with=F]
  rxnkaw[get(opening_entity_broad) %in% c("walmart","target","bimart","fred_meyer","safeway"),
         grocery := get(opening_entity_broad),
         with=F]
  gc()
}
gc()


#one obs per person-month####
print("#one obs per person-month####")
rxnkaw[, person_ym_index := seq_len(.N), by = c("personkey","pick_up_yrmon")]

#define spending vars####
print("#define spending vars####")
rxnkaw[, total_money := paid]

#medicaid dummy####
print("#medicaid dummy####")
rxnkaw[, medicaid_dummy := ifelse(paytype == "D", 1, 0)]

#pbm_dummy####
print("#pbm_dummy####")
rxnkaw[, pbm_dummy := ifelse(insurance_type_23 == "PH", 1, 0)]

#pre-period drug price decile####
print("#pre-period drug price decile####")
rxnkaw[pick_up_yrmon < open_ym_zip_1, mean_paid_rxclass_pre_opening_nf := mean(paid, na.rm=T), by = .(rxclass)]
rxnkaw[, mean_paid_rxclass_pre_opening := na.locf(mean_paid_rxclass_pre_opening_nf), by = .(rxclass)]
#one obs per rxclass
rxnkaw[, rxclass_index := seq_len(.N), by = .(rxclass)]
#deciles
rxnkaw[rxclass_index==1, rxclass_paid_pre_opening_decile_nf := findInterval(mean_paid_rxclass_pre_opening, quantile(mean_paid_rxclass_pre_opening, probs = seq(0,1,0.1), na.rm=T)) ]
rxnkaw[, rxclass_paid_pre_opening_decile := na.locf(rxclass_paid_pre_opening_decile_nf), by = .(rxclass)]
rxnkaw[, c("rxclass_paid_pre_opening_decile_nf","mean_paid_rxclass_pre_opening_nf") := NULL]
#closings####
rxnkaw[pick_up_yrmon < close_ym_zip_1, mean_paid_rxclass_pre_closing_nf := mean(paid, na.rm=T), by = .(rxclass)]
rxnkaw[, mean_paid_rxclass_pre_closing := na.locf(mean_paid_rxclass_pre_closing_nf), by = .(rxclass)]
#deciles
rxnkaw[rxclass_index==1, rxclass_paid_pre_closing_decile_nf := findInterval(mean_paid_rxclass_pre_closing, quantile(mean_paid_rxclass_pre_closing, probs = seq(0,1,0.1), na.rm=T)) ]
rxnkaw[, rxclass_paid_pre_closing_decile := na.locf(rxclass_paid_pre_closing_decile_nf), by = .(rxclass)]
rxnkaw[, c("rxclass_paid_pre_closing_decile_nf","mean_paid_rxclass_pre_closing_nf") := NULL]
#walgreensings####
print("#walgreensings")
rxnkaw[pick_up_yrmon < as.yearmon("Jan 2012"), mean_paid_rxclass_pre_walgreens_nf := mean(paid, na.rm=T), by = .(rxclass)]
rxnkaw[, mean_paid_rxclass_pre_walgreens := na.locf(mean_paid_rxclass_pre_walgreens_nf), by = .(rxclass)]
#deciles
rxnkaw[rxclass_index==1, rxclass_paid_pre_walgreens_decile_nf := findInterval(mean_paid_rxclass_pre_walgreens, quantile(mean_paid_rxclass_pre_walgreens, probs = seq(0,1,0.1), na.rm=T)) ]
rxnkaw[, rxclass_paid_pre_walgreens_decile := na.locf(rxclass_paid_pre_walgreens_decile_nf), by = .(rxclass)]
rxnkaw[, c("rxclass_paid_pre_walgreens_decile_nf","mean_paid_rxclass_pre_walgreens_nf") := NULL]


####chronic dummy####
print("####chronic dummy####")
#empirically chronic: avg number of fills per person in all three years > 24?
rxnkaw[, fills_per_person_rxclass := .N, by = .(personkey, rxclass)]
rxnkaw[, mean_fills_per_person_rxclass := mean(fills_per_person_rxclass, na.rm=T), by = .(rxclass)]
rxnkaw[, emp_chronic_dummy := ifelse(mean_fills_per_person_rxclass >= 24, 1, 0)]
print(length(unique(rxnkaw[emp_chronic_dummy==1]$rxclass)))
print(unique(rxnkaw[emp_chronic_dummy==1]$rxclass))

####weekend dummy####
print("####weekend dummy####")
rxnkaw[, dayofweek := weekdays(pick_up_date)]
rxnkaw[, weekend_dummy := ifelse(dayofweek %in% c("Saturday","Sunday"), 1, 0)]

# vars to add to the "numeric vars means" for loop#####
print("# vars to add to the numeric vars means for loop")
numeric_vars_list = c("age","mail_order_dummy","total_money","copay","oop", "medicaid_dummy",
                      "mpr","days_uncovered","rxdays","pbm_dummy",
                      "ami_deaths", "total_deaths", "other_artery_deaths", "suicides", "hypertensive_deaths", "hf_deaths", "hypertensive_renal_deaths", 
                      "diab_deaths", "other_heart_deaths", "total_heart_deaths",
                      "rxdays_90","rxdays_180", "rxdays_greater_than_30","rx_180_days_dummy")

# registerDoMC(4)
for (Y in numeric_vars_list){
# foreach(Y = numeric_vars_list) %dopar% {
  # Y = "age"
  print(Y)

  Y_ym = paste("mean", Y, "ym", sep="_")
  Y_ym_z = paste("mean", Y, "ym", "z", sep="_")
  
  Y_ym_c = paste("mean", Y, "ym", "c", sep="_")
  
  Y_ym_z_rx2 = paste("mean", Y, "ym", "z", "rx2", sep="_")
  Y_ym_c_rx2 = paste("mean", Y, "ym", "c", "rx2", sep="_")
  
  Y_ym_z_mail = paste("mean", Y, "ym", "z", "mail", sep="_")
  
  Y_ym_z_ins23 = paste("mean", Y, "ym", "z", "ins23", sep="_")
  Y_ym_z_ins53 = paste("mean", Y, "ym", "z", "ins53", sep="_")
  Y_ym_z_ins6 = paste("mean", Y, "ym", "z", "ins6", sep="_")
  Y_ym_z_ins8 = paste("mean", Y, "ym", "z", "ins8", sep="_")
  
  Y_ym_z_pharmtype = paste("mean", Y, "ym", "z", "pharmtype", sep="_")
  
  Y_ym_z_branded = paste("mean", Y, "ym", "z", "branded", sep="_")
  
  Y_ym_z_drugpricedecile = paste("mean", Y, "ym", "z", "rxclass_paid_pre_opening_decile", sep="_")
  Y_ym_z_drugpricedecile_closing = paste("mean", Y, "ym", "z", "rxclass_paid_pre_closing_decile", sep="_")
  Y_ym_z_drugpricedecile_walgreens = paste("mean", Y, "ym", "z", "rxclass_paid_pre_walgreens_decile", sep="_")
  
  Y_ym_z_weekend = paste("mean", Y, "ym", "z", "weekend", sep="_")
  
  # Y_ym_z_chronic = paste("mean", Y, "ym", "z", "chronic", sep="_")
  Y_ym_z_empchronic = paste("mean", Y, "ym", "z", "empchronic", sep="_")
  
  Y_ym_z_existing1 = paste("mean", Y, "ym", "z", "existing1", sep="_")
  Y_ym_z_existing2 = paste("mean", Y, "ym", "z", "existing2", sep="_")
  Y_ym_z_existing3 = paste("mean", Y, "ym", "z", "existing3", sep="_")
  Y_ym_z_existing4 = paste("mean", Y, "ym", "z", "existing4", sep="_")
  
  Y_ym_z_existing_c_1 = paste("mean", Y, "ym", "z", "existing_c_1", sep="_")
  Y_ym_z_existing_c_2 = paste("mean", Y, "ym", "z", "existing_c_2", sep="_")
  
  Y_ym_z_existing_w = paste("mean", Y, "ym", "z", "existing_w", sep="_")
  
  ###########################################################################################################################
  rxnkaw[, (Y_ym) := mean(get(Y), na.rm=T), by = .(pick_up_yrmon)]
  rxnkaw[, (Y_ym_z) := mean(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP)]
  rxnkaw[, (Y_ym_c) := mean(get(Y), na.rm=T), by = .(pick_up_yrmon, county)]
  
  rxnkaw[, (Y_ym_z_rx2) := mean(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, rx2)]
  rxnkaw[, (Y_ym_c_rx2) := mean(get(Y), na.rm=T), by = .(pick_up_yrmon, county, rx2)]
  
  rxnkaw[, (Y_ym_z_mail) := mean(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, mail_order_dummy)]
  
  rxnkaw[, (Y_ym_z_ins23) := mean(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, insurance_type_23)]
  rxnkaw[, (Y_ym_z_ins53) := mean(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, insurance_type_53)]
  rxnkaw[, (Y_ym_z_ins6) := mean(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, insurance_type_6)]
  rxnkaw[, (Y_ym_z_ins8) := mean(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, insurance_type_8)]
  
  print("pharmtype")
  rxnkaw[, (Y_ym_z_pharmtype) := mean(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, entity_broad)]
  # rxnkaw[, (Y_ym_z_branded) := mean(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, branded)]
  rxnkaw[, (Y_ym_z_drugpricedecile) := mean(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, rxclass_paid_pre_opening_decile)]
  rxnkaw[, (Y_ym_z_drugpricedecile_closing) := mean(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, rxclass_paid_pre_closing_decile)]
  rxnkaw[, (Y_ym_z_drugpricedecile_walgreens) := mean(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, rxclass_paid_pre_walgreens_decile)]
  rxnkaw[, (Y_ym_z_weekend) := mean(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, weekend_dummy)]
  # rxnkaw[, (Y_ym_z_chronic) := mean(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, chronic)]
  rxnkaw[, (Y_ym_z_empchronic) := mean(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, emp_chronic_dummy)]
  rxnkaw[, (Y_ym_z_existing1) := mean(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, existing1)]
  rxnkaw[, (Y_ym_z_existing2) := mean(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, existing2)]
  rxnkaw[, (Y_ym_z_existing3) := mean(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, existing3)]
  rxnkaw[, (Y_ym_z_existing4) := mean(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, existing4)]
  
  rxnkaw[, (Y_ym_z_existing_c_1) := mean(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, existing_c_1)]
  rxnkaw[, (Y_ym_z_existing_c_2) := mean(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, existing_c_2)]
  
  rxnkaw[, (Y_ym_z_existing_w) := mean(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, existing_w)]
  
  gc()
}
###################################################################################################################
print("saving")
print(Sys.time())
a = Sys.time()
save(rxnkaw, file="~/current/pharmacy_deserts/data/rxnkaw_after_total_means.RData")
b = Sys.time()
print(b - a)
print(names(rxnkaw))
gc()
###################################################################################################################

print("####total claims####")#####
Y = "claims"
print(Y)
Y_ym = paste("total", Y, "ym", sep="_")
Y_ym_z = paste("total", Y, "ym", "z", sep="_")

Y_ym_c = paste("total", Y, "ym", "c", sep="_")

Y_ym_z_rx2 = paste("total", Y, "ym", "z", "rx2", sep="_")
Y_ym_c_rx2 = paste("total", Y, "ym", "c", "rx2", sep="_")

Y_ym_z_mail = paste("total", Y, "ym", "z", "mail", sep="_")

Y_ym_z_ins23 = paste("total", Y, "ym", "z", "ins23", sep="_")
Y_ym_z_ins53 = paste("total", Y, "ym", "z", "ins53", sep="_")
Y_ym_z_ins6 = paste("total", Y, "ym", "z", "ins6", sep="_")
Y_ym_z_ins8 = paste("total", Y, "ym", "z", "ins8", sep="_")

Y_ym_z_pharmtype = paste("total", Y, "ym", "z", "pharmtype", sep="_")

Y_ym_z_branded = paste("total", Y, "ym", "z", "branded", sep="_")

Y_ym_z_drugpricedecile = paste("total", Y, "ym", "z", "rxclass_paid_pre_opening_decile", sep="_")
Y_ym_z_drugpricedecile_closing = paste("total", Y, "ym", "z", "rxclass_paid_pre_closing_decile", sep="_")
Y_ym_z_drugpricedecile_walgreens = paste("total", Y, "ym", "z", "rxclass_paid_pre_walgreens_decile", sep="_")

Y_ym_z_weekend = paste("total", Y, "ym", "z", "weekend", sep="_")

# Y_ym_z_chronic = paste("total", Y, "ym", "z", "chronic", sep="_")
Y_ym_z_empchronic = paste("total", Y, "ym", "z", "empchronic", sep="_")

Y_ym_z_existing1 = paste("total", Y, "ym", "z", "existing1", sep="_")
Y_ym_z_existing2 = paste("total", Y, "ym", "z", "existing2", sep="_")
Y_ym_z_existing3 = paste("total", Y, "ym", "z", "existing3", sep="_")
Y_ym_z_existing4 = paste("total", Y, "ym", "z", "existing4", sep="_")

Y_ym_z_existing_c_1 = paste("total", Y, "ym", "z", "existing_c_1", sep="_")
Y_ym_z_existing_c_2 = paste("total", Y, "ym", "z", "existing_c_2", sep="_")

Y_ym_z_existing_w = paste("total", Y, "ym", "z", "existing_w", sep="_")

###########################################################################################################################
rxnkaw[, (Y_ym) := .N, by = .(pick_up_yrmon)]
rxnkaw[, (Y_ym_z) := .N, by = .(pick_up_yrmon, ZIP)]
rxnkaw[, (Y_ym_c) := .N, by = .(pick_up_yrmon, county)]

rxnkaw[, (Y_ym_z_rx2) := .N, by = .(pick_up_yrmon, ZIP, rx2)]
rxnkaw[, (Y_ym_c_rx2) := .N, by = .(pick_up_yrmon, county, rx2)]

rxnkaw[, (Y_ym_z_mail) := .N, by = .(pick_up_yrmon, ZIP, mail_order_dummy)]

rxnkaw[, (Y_ym_z_ins23) := .N, by = .(pick_up_yrmon, ZIP, insurance_type_23)]
rxnkaw[, (Y_ym_z_ins53) := .N, by = .(pick_up_yrmon, ZIP, insurance_type_53)]
rxnkaw[, (Y_ym_z_ins6) := .N, by = .(pick_up_yrmon, ZIP, insurance_type_6)]
rxnkaw[, (Y_ym_z_ins8) := .N, by = .(pick_up_yrmon, ZIP, insurance_type_8)]

rxnkaw[, (Y_ym_z_pharmtype) := .N, by = .(pick_up_yrmon, ZIP, entity_broad)]
# rxnkaw[, (Y_ym_z_branded) := .N, by = .(pick_up_yrmon, ZIP, branded)]
rxnkaw[, (Y_ym_z_drugpricedecile) := .N, by = .(pick_up_yrmon, ZIP, rxclass_paid_pre_opening_decile)]
rxnkaw[, (Y_ym_z_drugpricedecile_closing) := .N, by = .(pick_up_yrmon, ZIP, rxclass_paid_pre_closing_decile)]
rxnkaw[, (Y_ym_z_drugpricedecile_walgreens) := .N, by = .(pick_up_yrmon, ZIP, rxclass_paid_pre_walgreens_decile)]
rxnkaw[, (Y_ym_z_weekend) := .N, by = .(pick_up_yrmon, ZIP, weekend_dummy)]
# rxnkaw[, (Y_ym_z_chronic) := .N, by = .(pick_up_yrmon, ZIP, chronic)]
rxnkaw[, (Y_ym_z_empchronic) := .N, by = .(pick_up_yrmon, ZIP, emp_chronic_dummy)]
rxnkaw[, (Y_ym_z_existing1) := .N, by = .(pick_up_yrmon, ZIP, existing1)]
rxnkaw[, (Y_ym_z_existing2) := .N, by = .(pick_up_yrmon, ZIP, existing2)]
rxnkaw[, (Y_ym_z_existing3) := .N, by = .(pick_up_yrmon, ZIP, existing3)]
rxnkaw[, (Y_ym_z_existing4) := .N, by = .(pick_up_yrmon, ZIP, existing4)]

rxnkaw[, (Y_ym_z_existing_c_1) := .N, by = .(pick_up_yrmon, ZIP, existing_c_1)]
rxnkaw[, (Y_ym_z_existing_c_2) := .N, by = .(pick_up_yrmon, ZIP, existing_c_2)]

rxnkaw[, (Y_ym_z_existing_w) := .N, by = .(pick_up_yrmon, ZIP, existing_w)]

gc()#####

print("sum_vars_list = c(total_money,qtydisp)")
sum_vars_list = c("total_money","qtydisp")

for (Y in sum_vars_list){
  print(Y)
  Y_ym = paste("sum", Y, "ym", sep="_")
  Y_ym_z = paste("sum", Y, "ym", "z", sep="_")
  
  Y_ym_c = paste("sum", Y, "ym", "c", sep="_")
  
  Y_ym_z_rx2 = paste("sum", Y, "ym", "z", "rx2", sep="_")
  Y_ym_c_rx2 = paste("sum", Y, "ym", "c", "rx2", sep="_")
  
  Y_ym_z_mail = paste("sum", Y, "ym", "z", "mail", sep="_")
  
  Y_ym_z_ins23 = paste("sum", Y, "ym", "z", "ins23", sep="_")
  Y_ym_z_ins53 = paste("sum", Y, "ym", "z", "ins53", sep="_")
  Y_ym_z_ins6 = paste("sum", Y, "ym", "z", "ins6", sep="_")
  Y_ym_z_ins8 = paste("sum", Y, "ym", "z", "ins8", sep="_")
  
  Y_ym_z_pharmtype = paste("sum", Y, "ym", "z", "pharmtype", sep="_")
  
  Y_ym_z_branded = paste("sum", Y, "ym", "z", "branded", sep="_")
  
  Y_ym_z_drugpricedecile = paste("sum", Y, "ym", "z", "rxclass_paid_pre_opening_decile", sep="_")
  Y_ym_z_drugpricedecile_closing = paste("sum", Y, "ym", "z", "rxclass_paid_pre_closing_decile", sep="_")
  Y_ym_z_drugpricedecile_walgreens = paste("sum", Y, "ym", "z", "rxclass_paid_pre_walgreens_decile", sep="_")
  
  Y_ym_z_weekend = paste("sum", Y, "ym", "z", "weekend", sep="_")
  
  # Y_ym_z_chronic = paste("sum", Y, "ym", "z", "chronic", sep="_")
  Y_ym_z_empchronic = paste("sum", Y, "ym", "z", "empchronic", sep="_")
  
  Y_ym_z_existing1 = paste("sum", Y, "ym", "z", "existing1", sep="_")
  Y_ym_z_existing2 = paste("sum", Y, "ym", "z", "existing2", sep="_")
  Y_ym_z_existing3 = paste("sum", Y, "ym", "z", "existing3", sep="_")
  Y_ym_z_existing4 = paste("sum", Y, "ym", "z", "existing4", sep="_")
  
  Y_ym_z_existing_c_1 = paste("sum", Y, "ym", "z", "existing_c_1", sep="_")
  Y_ym_z_existing_c_2 = paste("sum", Y, "ym", "z", "existing_c_2", sep="_")
  
  Y_ym_z_existing_w = paste("sum", Y, "ym", "z", "existing_w", sep="_")
  
  ###########################################################################################################################
  rxnkaw[, (Y_ym) := sum(get(Y), na.rm=T), by = .(pick_up_yrmon)]
  rxnkaw[, (Y_ym_z) := sum(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP)]
  rxnkaw[, (Y_ym_c) := sum(get(Y), na.rm=T), by = .(pick_up_yrmon, county)]
  
  rxnkaw[, (Y_ym_z_rx2) := sum(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, rx2)]
  rxnkaw[, (Y_ym_c_rx2) := sum(get(Y), na.rm=T), by = .(pick_up_yrmon, county, rx2)]
  
  rxnkaw[, (Y_ym_z_mail) := sum(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, mail_order_dummy)]
  
  rxnkaw[, (Y_ym_z_ins23) := sum(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, insurance_type_23)]
  rxnkaw[, (Y_ym_z_ins53) := sum(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, insurance_type_53)]
  rxnkaw[, (Y_ym_z_ins6) := sum(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, insurance_type_6)]
  rxnkaw[, (Y_ym_z_ins8) := sum(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, insurance_type_8)]
  
  rxnkaw[, (Y_ym_z_pharmtype) := sum(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, entity_broad)]
  # rxnkaw[, (Y_ym_z_branded) := sum(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, branded)]
  rxnkaw[, (Y_ym_z_drugpricedecile) := sum(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, rxclass_paid_pre_opening_decile)]
  rxnkaw[, (Y_ym_z_drugpricedecile_closing) := sum(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, rxclass_paid_pre_closing_decile)]
  rxnkaw[, (Y_ym_z_drugpricedecile_walgreens) := sum(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, rxclass_paid_pre_walgreens_decile)]
  rxnkaw[, (Y_ym_z_weekend) := sum(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, weekend_dummy)]
  # rxnkaw[, (Y_ym_z_chronic) := sum(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, chronic)]
  rxnkaw[, (Y_ym_z_empchronic) := sum(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, emp_chronic_dummy)]
  rxnkaw[, (Y_ym_z_existing1) := sum(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, existing1)]
  rxnkaw[, (Y_ym_z_existing2) := sum(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, existing2)]
  rxnkaw[, (Y_ym_z_existing3) := sum(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, existing3)]
  rxnkaw[, (Y_ym_z_existing4) := sum(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, existing4)]
  
  rxnkaw[, (Y_ym_z_existing_c_1) := sum(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, existing_c_1)]
  rxnkaw[, (Y_ym_z_existing_c_2) := sum(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, existing_c_2)]
  
  rxnkaw[, (Y_ym_z_existing_w) := sum(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, existing_w)]
  
  gc()
}

print("length_unique_vars_list")
length_unique_vars_list = c("personkey")
for (Y in sum_vars_list){
  print(Y)
  Y_ym = paste("unique", Y, "ym", sep="_")
  Y_ym_z = paste("unique", Y, "ym", "z", sep="_")
  
  Y_ym_c = paste("unique", Y, "ym", "c", sep="_")
  
  Y_ym_z_rx2 = paste("unique", Y, "ym", "z", "rx2", sep="_")
  Y_ym_c_rx2 = paste("unique", Y, "ym", "c", "rx2", sep="_")
  
  Y_ym_z_mail = paste("unique", Y, "ym", "z", "mail", sep="_")
  
  Y_ym_z_ins23 = paste("unique", Y, "ym", "z", "ins23", sep="_")
  Y_ym_z_ins53 = paste("unique", Y, "ym", "z", "ins53", sep="_")
  Y_ym_z_ins6 = paste("unique", Y, "ym", "z", "ins6", sep="_")
  Y_ym_z_ins8 = paste("unique", Y, "ym", "z", "ins8", sep="_")
  
  Y_ym_z_pharmtype = paste("unique", Y, "ym", "z", "pharmtype", sep="_")
  
  Y_ym_z_branded = paste("unique", Y, "ym", "z", "branded", sep="_")
  
  Y_ym_z_drugpricedecile = paste("unique", Y, "ym", "z", "rxclass_paid_pre_opening_decile", sep="_")
  Y_ym_z_drugpricedecile_closing = paste("unique", Y, "ym", "z", "rxclass_paid_pre_closing_decile", sep="_")
  Y_ym_z_drugpricedecile_walgreens = paste("unique", Y, "ym", "z", "rxclass_paid_pre_walgreens_decile", sep="_")
  
  Y_ym_z_weekend = paste("unique", Y, "ym", "z", "weekend", sep="_")
  
  # Y_ym_z_chronic = paste("unique", Y, "ym", "z", "chronic", sep="_")
  Y_ym_z_empchronic = paste("unique", Y, "ym", "z", "empchronic", sep="_")
  
  Y_ym_z_existing1 = paste("unique", Y, "ym", "z", "existing1", sep="_")
  Y_ym_z_existing2 = paste("unique", Y, "ym", "z", "existing2", sep="_")
  Y_ym_z_existing3 = paste("unique", Y, "ym", "z", "existing3", sep="_")
  Y_ym_z_existing4 = paste("unique", Y, "ym", "z", "existing4", sep="_")
  
  Y_ym_z_existing_c_1 = paste("unique", Y, "ym", "z", "existing_c_1", sep="_")
  Y_ym_z_existing_c_2 = paste("unique", Y, "ym", "z", "existing_c_2", sep="_")
  
  Y_ym_z_existing_w = paste("unique", Y, "ym", "z", "existing_w", sep="_")
  
  ###########################################################################################################################
  rxnkaw[, (Y_ym) := length(unique(get(Y))), by = .(pick_up_yrmon)]
  rxnkaw[, (Y_ym_z) := length(unique(get(Y))), by = .(pick_up_yrmon, ZIP)]
  rxnkaw[, (Y_ym_c) := length(unique(get(Y))), by = .(pick_up_yrmon, county)]
  
  rxnkaw[, (Y_ym_z_rx2) := length(unique(get(Y))), by = .(pick_up_yrmon, ZIP, rx2)]
  rxnkaw[, (Y_ym_c_rx2) := length(unique(get(Y))), by = .(pick_up_yrmon, county, rx2)]
  
  rxnkaw[, (Y_ym_z_mail) := length(unique(get(Y))), by = .(pick_up_yrmon, ZIP, mail_order_dummy)]
  
  rxnkaw[, (Y_ym_z_ins23) := length(unique(get(Y))), by = .(pick_up_yrmon, ZIP, insurance_type_23)]
  rxnkaw[, (Y_ym_z_ins53) := length(unique(get(Y))), by = .(pick_up_yrmon, ZIP, insurance_type_53)]
  rxnkaw[, (Y_ym_z_ins6) := length(unique(get(Y))), by = .(pick_up_yrmon, ZIP, insurance_type_6)]
  rxnkaw[, (Y_ym_z_ins8) := length(unique(get(Y))), by = .(pick_up_yrmon, ZIP, insurance_type_8)]
  
  rxnkaw[, (Y_ym_z_pharmtype) := length(unique(get(Y))), by = .(pick_up_yrmon, ZIP, entity_broad)]
  # rxnkaw[, (Y_ym_z_branded) := length(unique(get(Y))), by = .(pick_up_yrmon, ZIP, branded)]
  rxnkaw[, (Y_ym_z_drugpricedecile) := length(unique(get(Y))), by = .(pick_up_yrmon, ZIP, rxclass_paid_pre_opening_decile)]
  rxnkaw[, (Y_ym_z_drugpricedecile_closing) := length(unique(get(Y))), by = .(pick_up_yrmon, ZIP, rxclass_paid_pre_closing_decile)]
  rxnkaw[, (Y_ym_z_drugpricedecile_walgreens) := length(unique(get(Y))), by = .(pick_up_yrmon, ZIP, rxclass_paid_pre_walgreens_decile)]
  rxnkaw[, (Y_ym_z_weekend) := length(unique(get(Y))), by = .(pick_up_yrmon, ZIP, weekend_dummy)]
  # rxnkaw[, (Y_ym_z_chronic) := length(unique(get(Y))), by = .(pick_up_yrmon, ZIP, chronic)]
  rxnkaw[, (Y_ym_z_empchronic) := length(unique(get(Y))), by = .(pick_up_yrmon, ZIP, emp_chronic_dummy)]
  rxnkaw[, (Y_ym_z_existing1) := length(unique(get(Y))), by = .(pick_up_yrmon, ZIP, existing1)]
  rxnkaw[, (Y_ym_z_existing2) := length(unique(get(Y))), by = .(pick_up_yrmon, ZIP, existing2)]
  rxnkaw[, (Y_ym_z_existing3) := length(unique(get(Y))), by = .(pick_up_yrmon, ZIP, existing3)]
  rxnkaw[, (Y_ym_z_existing4) := length(unique(get(Y))), by = .(pick_up_yrmon, ZIP, existing4)]
  
  rxnkaw[, (Y_ym_z_existing_c_1) := length(unique(get(Y))), by = .(pick_up_yrmon, ZIP, existing_c_1)]
  rxnkaw[, (Y_ym_z_existing_c_2) := length(unique(get(Y))), by = .(pick_up_yrmon, ZIP, existing_c_2)]
  
  rxnkaw[, (Y_ym_z_existing_w) := length(unique(get(Y))), by = .(pick_up_yrmon, ZIP, existing_w)]
  
  gc()
}

print("sd_vars_list = c(total_money,oop, copay)")
sd_vars_list = c("total_money","oop","copay")

for (Y in sd_vars_list){
  print(Y)
  Y_ym = paste("sd", Y, "ym", sep="_")
  Y_ym_z = paste("sd", Y, "ym", "z", sep="_")
  
  Y_ym_c = paste("sd", Y, "ym", "c", sep="_")
  
  Y_ym_z_rx2 = paste("sd", Y, "ym", "z", "rx2", sep="_")
  Y_ym_c_rx2 = paste("sd", Y, "ym", "c", "rx2", sep="_")
  
  Y_ym_z_mail = paste("sd", Y, "ym", "z", "mail", sep="_")
  
  Y_ym_z_ins23 = paste("sd", Y, "ym", "z", "ins23", sep="_")
  Y_ym_z_ins53 = paste("sd", Y, "ym", "z", "ins53", sep="_")
  Y_ym_z_ins6 = paste("sd", Y, "ym", "z", "ins6", sep="_")
  Y_ym_z_ins8 = paste("sd", Y, "ym", "z", "ins8", sep="_")
  
  Y_ym_z_pharmtype = paste("sd", Y, "ym", "z", "pharmtype", sep="_")
  
  Y_ym_z_branded = paste("sd", Y, "ym", "z", "branded", sep="_")
  
  Y_ym_z_drugpricedecile = paste("sd", Y, "ym", "z", "rxclass_paid_pre_opening_decile", sep="_")
  Y_ym_z_drugpricedecile_closing = paste("sd", Y, "ym", "z", "rxclass_paid_pre_closing_decile", sep="_")
  Y_ym_z_drugpricedecile_walgreens = paste("sd", Y, "ym", "z", "rxclass_paid_pre_walgreens_decile", sep="_")
  
  Y_ym_z_weekend = paste("sd", Y, "ym", "z", "weekend", sep="_")
  
  # Y_ym_z_chronic = paste("sd", Y, "ym", "z", "chronic", sep="_")
  Y_ym_z_empchronic = paste("sd", Y, "ym", "z", "empchronic", sep="_")
  
  Y_ym_z_existing1 = paste("sd", Y, "ym", "z", "existing1", sep="_")
  Y_ym_z_existing2 = paste("sd", Y, "ym", "z", "existing2", sep="_")
  Y_ym_z_existing3 = paste("sd", Y, "ym", "z", "existing3", sep="_")
  Y_ym_z_existing4 = paste("sd", Y, "ym", "z", "existing4", sep="_")
  
  Y_ym_z_existing_c_1 = paste("sd", Y, "ym", "z", "existing_c_1", sep="_")
  Y_ym_z_existing_c_2 = paste("sd", Y, "ym", "z", "existing_c_2", sep="_")
  
  Y_ym_z_existing_w = paste("sd", Y, "ym", "z", "existing_w", sep="_")
  
  ###########################################################################################################################
  rxnkaw[, (Y_ym) := sd(get(Y), na.rm=T), by = .(pick_up_yrmon)]
  rxnkaw[, (Y_ym_z) := sd(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP)]
  rxnkaw[, (Y_ym_c) := sd(get(Y), na.rm=T), by = .(pick_up_yrmon, county)]
  
  rxnkaw[, (Y_ym_z_rx2) := sd(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, rx2)]
  rxnkaw[, (Y_ym_c_rx2) := sd(get(Y), na.rm=T), by = .(pick_up_yrmon, county, rx2)]
  
  rxnkaw[, (Y_ym_z_mail) := sd(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, mail_order_dummy)]
  
  rxnkaw[, (Y_ym_z_ins23) := sd(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, insurance_type_23)]
  rxnkaw[, (Y_ym_z_ins53) := sd(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, insurance_type_53)]
  rxnkaw[, (Y_ym_z_ins6) := sd(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, insurance_type_6)]
  rxnkaw[, (Y_ym_z_ins8) := sd(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, insurance_type_8)]
  
  rxnkaw[, (Y_ym_z_pharmtype) := sd(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, entity_broad)]
  # rxnkaw[, (Y_ym_z_branded) := sd(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, branded)]
  rxnkaw[, (Y_ym_z_drugpricedecile) := sd(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, rxclass_paid_pre_opening_decile)]
  rxnkaw[, (Y_ym_z_drugpricedecile_closing) := sd(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, rxclass_paid_pre_closing_decile)]
  rxnkaw[, (Y_ym_z_drugpricedecile_walgreens) := sd(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, rxclass_paid_pre_walgreens_decile)]
  rxnkaw[, (Y_ym_z_weekend) := sd(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, weekend_dummy)]
  # rxnkaw[, (Y_ym_z_chronic) := sd(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, chronic)]
  rxnkaw[, (Y_ym_z_empchronic) := sd(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, emp_chronic_dummy)]
  rxnkaw[, (Y_ym_z_existing1) := sd(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, existing1)]
  rxnkaw[, (Y_ym_z_existing2) := sd(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, existing2)]
  rxnkaw[, (Y_ym_z_existing3) := sd(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, existing3)]
  rxnkaw[, (Y_ym_z_existing4) := sd(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, existing4)]
  
  rxnkaw[, (Y_ym_z_existing_c_1) := sd(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, existing_c_1)]
  rxnkaw[, (Y_ym_z_existing_c_2) := sd(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, existing_c_2)]
  
  rxnkaw[, (Y_ym_z_existing_w) := sd(get(Y), na.rm=T), by = .(pick_up_yrmon, ZIP, existing_w)]
  
  gc()
}




####share of patients on medicaid in the zip code each month####
#this is a proxy for income
rxnkaw[, total_medicaid_ym_z_nf := NULL]
rxnkaw[, total_medicaid_ym_z := NULL]
rxnkaw[paytype == "D", total_medicaid_ym_z_nf :=  length(unique(personkey)), 
       by = c("pick_up_yrmon","ZIP")]
rxnkaw[, total_medicaid_ym_z :=  na.locf(total_medicaid_ym_z_nf), 
       by = c("pick_up_yrmon","ZIP")]

rxnkaw[paytype == "D", total_medicaid_y_c_nf :=  length(unique(personkey)), 
       by = c("year","county")]
rxnkaw[, total_medicaid_y_c :=  na.locf(total_medicaid_y_c_nf), 
       by = c("year","county")]

rxnkaw[, share_medicaid_ym_z := total_medicaid_ym_z / unique_personkey_ym_z]
rxnkaw[, share_medicaid_y_c := total_medicaid_y_c / unique_personkey_y_c]
gc()


####number of patients starting this month (new patients)####
print("_ym")######
#sort
setorder(rxnkaw, personkey, pick_up_yrmon)
#index patients
rxnkaw[, personkey_fill_index := seq_len(.N), by = c("personkey")]
#sum first fills over zip code
rxnkaw[personkey_fill_index == 1, total_new_pats_ym_nf := sum(personkey_fill_index), by = c("pick_up_yrmon")]
rxnkaw[, total_new_pats_ym := na.locf(total_new_pats_ym_nf), by = c("pick_up_yrmon")]
rxnkaw[is.na(total_new_pats_ym), total_new_pats_ym := 0]
#index months
rxnkaw[, ym_index := seq_len(.N), by = .(pick_up_yrmon)]
#cumsum over time
rxnkaw[ym_index == 1, cum_total_new_pats_ym_nf := cumsum(total_new_pats_ym)]
#fill
rxnkaw[, cum_total_new_pats_ym := na.locf(cum_total_new_pats_ym_nf), by = .(pick_up_yrmon)]


print("_ym_z")#####
#sort
setorder(rxnkaw, ZIP, personkey, pick_up_yrmon)
#index patients
rxnkaw[, personkey_ZIP_fill_index := seq_len(.N), by = c("personkey", "ZIP")]
#sum first fills over zip code
rxnkaw[personkey_ZIP_fill_index == 1, total_new_pats_ym_z_nf := sum(personkey_ZIP_fill_index), by = c("pick_up_yrmon", "ZIP")]
rxnkaw[, total_new_pats_ym_z := na.locf(total_new_pats_ym_z_nf), by = c("pick_up_yrmon", "ZIP")]
rxnkaw[is.na(total_new_pats_ym_z), total_new_pats_ym_z := 0]
#index months
rxnkaw[, ym_z_index := seq_len(.N), by = .(pick_up_yrmon, ZIP)]
#cumsum over time
rxnkaw[ym_z_index == 1, cum_total_new_pats_ym_z_nf := cumsum(total_new_pats_ym_z), by = .(ZIP)]
#fill
rxnkaw[, cum_total_new_pats_ym_z := na.locf(cum_total_new_pats_ym_z_nf), by = .(pick_up_yrmon, ZIP)]
ggplot(data = rxnkaw[ym_z_index == 1 & ZIP == 97124]) +
  geom_line(aes(x=pick_up_yrmon, y=cum_total_new_pats_ym_z))

print("_ym_c")######
#sort
setorder(rxnkaw, county, personkey, pick_up_yrmon)
#index patients
rxnkaw[, personkey_county_fill_index := seq_len(.N), by = c("personkey", "county")]
#sum first fills over county code
rxnkaw[personkey_county_fill_index == 1, total_new_pats_ym_c_nf := sum(personkey_county_fill_index), by = c("pick_up_yrmon", "county")]
rxnkaw[, total_new_pats_ym_c := na.locf(total_new_pats_ym_c_nf), by = c("pick_up_yrmon", "county")]
rxnkaw[is.na(total_new_pats_ym_c), total_new_pats_ym_c := 0]
#index months
rxnkaw[, ym_c_index := seq_len(.N), by = .(pick_up_yrmon, county)]
#cumsum over time
rxnkaw[ym_c_index == 1, cum_total_new_pats_ym_c_nf := cumsum(total_new_pats_ym_c), by = .(county)]
#fill
rxnkaw[, cum_total_new_pats_ym_c := na.locf(cum_total_new_pats_ym_c_nf), by = .(pick_up_yrmon, county)]


print("_ym_z_rx2") #####
#sort
setorder(rxnkaw, ZIP, rx2, personkey, pick_up_yrmon)
#index patients
rxnkaw[, personkey_ZIP_rx2_fill_index := seq_len(.N), by = c("personkey", "ZIP", "rx2")]
#sum first fills over zip code
rxnkaw[personkey_ZIP_rx2_fill_index == 1, total_new_pats_ym_z_rx2_nf := sum(personkey_ZIP_rx2_fill_index), by = c("pick_up_yrmon", "ZIP", "rx2")]
rxnkaw[, total_new_pats_ym_z_rx2 := na.locf(total_new_pats_ym_z_rx2_nf), by = c("pick_up_yrmon", "ZIP", "rx2")]
rxnkaw[is.na(total_new_pats_ym_z_rx2), total_new_pats_ym_z_rx2 := 0]
#index months
rxnkaw[, ym_z_rx2_index := seq_len(.N), by = .(pick_up_yrmon, ZIP, rx2)]
#cumsum over time
rxnkaw[ym_z_rx2_index == 1, cum_total_new_pats_ym_z_rx2_nf := cumsum(total_new_pats_ym_z_rx2), by = .(ZIP, rx2)]
#fill
rxnkaw[, cum_total_new_pats_ym_z_rx2 := na.locf(cum_total_new_pats_ym_z_rx2_nf), by = .(pick_up_yrmon, ZIP, rx2)]

print("_ym_c_rx2") #####
#sort
setorder(rxnkaw, county, rx2, personkey, pick_up_yrmon)
#index patients
rxnkaw[, personkey_county_rx2_fill_index := seq_len(.N), by = c("personkey", "county", "rx2")]
#sum first fills over zip code
rxnkaw[personkey_county_rx2_fill_index == 1, total_new_pats_ym_c_rx2_nf := sum(personkey_county_rx2_fill_index), by = c("pick_up_yrmon", "county", "rx2")]
rxnkaw[, total_new_pats_ym_c_rx2 := na.locf(total_new_pats_ym_c_rx2_nf), by = c("pick_up_yrmon", "county", "rx2")]
rxnkaw[is.na(total_new_pats_ym_c_rx2), total_new_pats_ym_c_rx2 := 0]
#index months
rxnkaw[, ym_c_rx2_index := seq_len(.N), by = .(pick_up_yrmon, county, rx2)]
#cumsum over time
rxnkaw[ym_c_rx2_index == 1, cum_total_new_pats_ym_c_rx2_nf := cumsum(total_new_pats_ym_c_rx2), by = .(county, rx2)]
#fill
rxnkaw[, cum_total_new_pats_ym_c_rx2 := na.locf(cum_total_new_pats_ym_c_rx2_nf), by = .(pick_up_yrmon, county, rx2)]

print("_ym_z_mail") #####
#sort
setorder(rxnkaw, ZIP, mail_order_dummy, personkey, pick_up_yrmon)
#index patients
rxnkaw[, personkey_ZIP_mail_fill_index := seq_len(.N), by = c("personkey", "ZIP", "mail_order_dummy")]
#sum first fills over zip code
rxnkaw[personkey_ZIP_mail_fill_index == 1, total_new_pats_ym_z_mail_nf := sum(personkey_ZIP_mail_fill_index), by = c("pick_up_yrmon", "ZIP", "mail_order_dummy")]
rxnkaw[, total_new_pats_ym_z_mail := na.locf(total_new_pats_ym_z_mail_nf), by = c("pick_up_yrmon", "ZIP", "mail_order_dummy")]
rxnkaw[is.na(total_new_pats_ym_z_mail), total_new_pats_ym_z_mail := 0]
#index months
rxnkaw[, ym_z_mail_index := seq_len(.N), by = .(pick_up_yrmon, ZIP, mail_order_dummy)]
#cumsum over time
rxnkaw[ym_z_mail_index == 1, cum_total_new_pats_ym_z_mail_nf := cumsum(total_new_pats_ym_z_mail), by = .(ZIP, mail_order_dummy)]
#fill
rxnkaw[, cum_total_new_pats_ym_z_mail := na.locf(cum_total_new_pats_ym_z_mail_nf), by = .(pick_up_yrmon, ZIP, mail_order_dummy)]

print("_ym_z_ins23") #####
#sort
setorder(rxnkaw, ZIP, insurance_type_23, personkey, pick_up_yrmon)
#index patients
rxnkaw[, personkey_ZIP_ins23_fill_index := seq_len(.N), by = c("personkey", "ZIP", "insurance_type_23")]
#sum first fills over zip code
rxnkaw[personkey_ZIP_ins23_fill_index == 1, total_new_pats_ym_z_ins23_nf := sum(personkey_ZIP_ins23_fill_index), by = c("pick_up_yrmon", "ZIP", "insurance_type_23")]
rxnkaw[, total_new_pats_ym_z_ins23 := na.locf(total_new_pats_ym_z_ins23_nf), by = c("pick_up_yrmon", "ZIP", "insurance_type_23")]
rxnkaw[is.na(total_new_pats_ym_z_ins23), total_new_pats_ym_z_ins23 := 0]
#index months
rxnkaw[, ym_z_ins23_index := seq_len(.N), by = .(pick_up_yrmon, ZIP, insurance_type_23)]
#cumsum over time
rxnkaw[ym_z_ins23_index == 1, cum_total_new_pats_ym_z_ins23_nf := cumsum(total_new_pats_ym_z_ins23), by = .(ZIP, insurance_type_23)]
#fill
rxnkaw[, cum_total_new_pats_ym_z_ins23 := na.locf(cum_total_new_pats_ym_z_ins23_nf), by = .(pick_up_yrmon, ZIP, insurance_type_23)]

print("_ym_z_ins53") #####
#sort
setorder(rxnkaw, ZIP, insurance_type_53, personkey, pick_up_yrmon)
#index patients
rxnkaw[, personkey_ZIP_ins53_fill_index := seq_len(.N), by = c("personkey", "ZIP", "insurance_type_53")]
#sum first fills over zip code
rxnkaw[personkey_ZIP_ins53_fill_index == 1, total_new_pats_ym_z_ins53_nf := sum(personkey_ZIP_ins53_fill_index), by = c("pick_up_yrmon", "ZIP", "insurance_type_53")]
rxnkaw[, total_new_pats_ym_z_ins53 := na.locf(total_new_pats_ym_z_ins53_nf), by = c("pick_up_yrmon", "ZIP", "insurance_type_53")]
rxnkaw[is.na(total_new_pats_ym_z_ins53), total_new_pats_ym_z_ins53 := 0]
#index months
rxnkaw[, ym_z_ins53_index := seq_len(.N), by = .(pick_up_yrmon, ZIP, insurance_type_53)]
#cumsum over time
rxnkaw[ym_z_ins53_index == 1, cum_total_new_pats_ym_z_ins53_nf := cumsum(total_new_pats_ym_z_ins53), by = .(ZIP, insurance_type_53)]
#fill
rxnkaw[, cum_total_new_pats_ym_z_ins53 := na.locf(cum_total_new_pats_ym_z_ins53_nf), by = .(pick_up_yrmon, ZIP, insurance_type_53)]

print("_ym_z_ins6") #####
#sort
setorder(rxnkaw, ZIP, insurance_type_6, personkey, pick_up_yrmon)
#index patients
rxnkaw[, personkey_ZIP_ins6_fill_index := seq_len(.N), by = c("personkey", "ZIP", "insurance_type_6")]
#sum first fills over zip code
rxnkaw[personkey_ZIP_ins6_fill_index == 1, total_new_pats_ym_z_ins6_nf := sum(personkey_ZIP_ins6_fill_index), by = c("pick_up_yrmon", "ZIP", "insurance_type_6")]
rxnkaw[, total_new_pats_ym_z_ins6 := na.locf(total_new_pats_ym_z_ins6_nf), by = c("pick_up_yrmon", "ZIP", "insurance_type_6")]
rxnkaw[is.na(total_new_pats_ym_z_ins6), total_new_pats_ym_z_ins6 := 0]
#index months
rxnkaw[, ym_z_ins6_index := seq_len(.N), by = .(pick_up_yrmon, ZIP, insurance_type_6)]
#cumsum over time
rxnkaw[ym_z_ins6_index == 1, cum_total_new_pats_ym_z_ins6_nf := cumsum(total_new_pats_ym_z_ins6), by = .(ZIP, insurance_type_6)]
#fill
rxnkaw[, cum_total_new_pats_ym_z_ins6 := na.locf(cum_total_new_pats_ym_z_ins6_nf), by = .(pick_up_yrmon, ZIP, insurance_type_6)]

print("_ym_z_ins8") #####
#sort
setorder(rxnkaw, ZIP, insurance_type_8, personkey, pick_up_yrmon)
#index patients
rxnkaw[, personkey_ZIP_ins8_fill_index := seq_len(.N), by = c("personkey", "ZIP", "insurance_type_8")]
#sum first fills over zip code
rxnkaw[personkey_ZIP_ins8_fill_index == 1, total_new_pats_ym_z_ins8_nf := sum(personkey_ZIP_ins8_fill_index), by = c("pick_up_yrmon", "ZIP", "insurance_type_8")]
rxnkaw[, total_new_pats_ym_z_ins8 := na.locf(total_new_pats_ym_z_ins8_nf), by = c("pick_up_yrmon", "ZIP", "insurance_type_8")]
rxnkaw[is.na(total_new_pats_ym_z_ins8), total_new_pats_ym_z_ins8 := 0]
#index months
rxnkaw[, ym_z_ins8_index := seq_len(.N), by = .(pick_up_yrmon, ZIP, insurance_type_8)]
#cumsum over time
rxnkaw[ym_z_ins8_index == 1, cum_total_new_pats_ym_z_ins8_nf := cumsum(total_new_pats_ym_z_ins8), by = .(ZIP, insurance_type_8)]
#fill
rxnkaw[, cum_total_new_pats_ym_z_ins8 := na.locf(cum_total_new_pats_ym_z_ins8_nf), by = .(pick_up_yrmon, ZIP, insurance_type_8)]

print("_ym_z_pharmtype") #####
#sort
setorder(rxnkaw, ZIP, entity_broad, personkey, pick_up_yrmon)
#index patients
rxnkaw[, personkey_ZIP_pharmtype_fill_index := seq_len(.N), by = c("personkey", "ZIP", "entity_broad")]
#sum first fills over zip code
rxnkaw[personkey_ZIP_pharmtype_fill_index == 1, total_new_pats_ym_z_pharmtype_nf := sum(personkey_ZIP_pharmtype_fill_index), by = c("pick_up_yrmon", "ZIP", "entity_broad")]
rxnkaw[, total_new_pats_ym_z_pharmtype := na.locf(total_new_pats_ym_z_pharmtype_nf), by = c("pick_up_yrmon", "ZIP", "entity_broad")]
rxnkaw[is.na(total_new_pats_ym_z_pharmtype), total_new_pats_ym_z_pharmtype := 0]
#index months
rxnkaw[, ym_z_pharmtype_index := seq_len(.N), by = .(pick_up_yrmon, ZIP, entity_broad)]
#cumsum over time
rxnkaw[ym_z_pharmtype_index == 1, cum_total_new_pats_ym_z_pharmtype_nf := cumsum(total_new_pats_ym_z_pharmtype), by = .(ZIP, entity_broad)]
#fill
rxnkaw[, cum_total_new_pats_ym_z_pharmtype := na.locf(cum_total_new_pats_ym_z_pharmtype_nf), by = .(pick_up_yrmon, ZIP, entity_broad)]

print("_ym_z_drugpricedecile") #####
#sort
setorder(rxnkaw, ZIP, rxclass_paid_pre_opening_decile, personkey, pick_up_yrmon)
#index patients
rxnkaw[, personkey_ZIP_drugpricedecile_fill_index := seq_len(.N), by = c("personkey", "ZIP", "rxclass_paid_pre_opening_decile")]
#sum first fills over zip code
rxnkaw[personkey_ZIP_drugpricedecile_fill_index == 1, total_new_pats_ym_z_drugpricedecile_nf := sum(personkey_ZIP_drugpricedecile_fill_index), by = c("pick_up_yrmon", "ZIP", "rxclass_paid_pre_opening_decile")]
rxnkaw[, total_new_pats_ym_z_drugpricedecile := na.locf(total_new_pats_ym_z_drugpricedecile_nf), by = c("pick_up_yrmon", "ZIP", "rxclass_paid_pre_opening_decile")]
rxnkaw[is.na(total_new_pats_ym_z_drugpricedecile), total_new_pats_ym_z_drugpricedecile := 0]
#index months
rxnkaw[, ym_z_drugpricedecile_index := seq_len(.N), by = .(pick_up_yrmon, ZIP, rxclass_paid_pre_opening_decile)]
#cumsum over time
rxnkaw[ym_z_drugpricedecile_index == 1, cum_total_new_pats_ym_z_drugpricedecile_nf := cumsum(total_new_pats_ym_z_drugpricedecile), by = .(ZIP, rxclass_paid_pre_opening_decile)]
#fill
rxnkaw[, cum_total_new_pats_ym_z_drugpricedecile := na.locf(cum_total_new_pats_ym_z_drugpricedecile_nf), by = .(pick_up_yrmon, ZIP, rxclass_paid_pre_opening_decile)]
print("_ym_z_drugpricedecile_closings") #####
#sort
setorder(rxnkaw, ZIP, rxclass_paid_pre_closing_decile, personkey, pick_up_yrmon)
#index patients
rxnkaw[, personkey_ZIP_drugpricedecile_closings_fill_index := seq_len(.N), by = c("personkey", "ZIP", "rxclass_paid_pre_closing_decile")]
#sum first fills over zip code
rxnkaw[personkey_ZIP_drugpricedecile_closings_fill_index == 1, total_new_pats_ym_z_drugpricedecile_closings_nf := sum(personkey_ZIP_drugpricedecile_closings_fill_index), by = c("pick_up_yrmon", "ZIP", "rxclass_paid_pre_closing_decile")]
rxnkaw[, total_new_pats_ym_z_drugpricedecile_closings := na.locf(total_new_pats_ym_z_drugpricedecile_closings_nf), by = c("pick_up_yrmon", "ZIP", "rxclass_paid_pre_closing_decile")]
rxnkaw[is.na(total_new_pats_ym_z_drugpricedecile_closings), total_new_pats_ym_z_drugpricedecile_closings := 0]
#index months
rxnkaw[, ym_z_drugpricedecile_closings_index := seq_len(.N), by = .(pick_up_yrmon, ZIP, rxclass_paid_pre_closing_decile)]
#cumsum over time
rxnkaw[ym_z_drugpricedecile_closings_index == 1, 
       cum_total_new_pats_ym_z_drugpricedecile_closings_nf := cumsum(total_new_pats_ym_z_drugpricedecile_closings), by = .(ZIP, rxclass_paid_pre_closing_decile)]
#fill
rxnkaw[, cum_total_new_pats_ym_z_drugpricedecile_closings := na.locf(cum_total_new_pats_ym_z_drugpricedecile_closings_nf), by = .(pick_up_yrmon, ZIP, rxclass_paid_pre_closing_decile)]
print("_ym_z_drugpricedecile_walgreens") #####
#sort
setorder(rxnkaw, ZIP, rxclass_paid_pre_walgreens_decile, personkey, pick_up_yrmon)
#index patients
rxnkaw[, personkey_ZIP_drugpricedecile_walgreens_fill_index := seq_len(.N), by = c("personkey", "ZIP", "rxclass_paid_pre_walgreens_decile")]
#sum first fills over zip code
rxnkaw[personkey_ZIP_drugpricedecile_walgreens_fill_index == 1, total_new_pats_ym_z_drugpricedecile_walgreens_nf := sum(personkey_ZIP_drugpricedecile_walgreens_fill_index), by = c("pick_up_yrmon", "ZIP", "rxclass_paid_pre_walgreens_decile")]
rxnkaw[, total_new_pats_ym_z_drugpricedecile_walgreens := na.locf(total_new_pats_ym_z_drugpricedecile_walgreens_nf), by = c("pick_up_yrmon", "ZIP", "rxclass_paid_pre_walgreens_decile")]
rxnkaw[is.na(total_new_pats_ym_z_drugpricedecile_walgreens), total_new_pats_ym_z_drugpricedecile_walgreens := 0]
#index months
rxnkaw[, ym_z_drugpricedecile_walgreens_index := seq_len(.N), by = .(pick_up_yrmon, ZIP, rxclass_paid_pre_walgreens_decile)]
#cumsum over time
rxnkaw[ym_z_drugpricedecile_walgreens_index == 1, 
       cum_total_new_pats_ym_z_drugpricedecile_walgreens_nf := cumsum(total_new_pats_ym_z_drugpricedecile_walgreens), 
       by = .(ZIP, rxclass_paid_pre_walgreens_decile)]
#fill
rxnkaw[, cum_total_new_pats_ym_z_drugpricedecile_walgreens := na.locf(cum_total_new_pats_ym_z_drugpricedecile_walgreens_nf), by = .(pick_up_yrmon, ZIP, rxclass_paid_pre_walgreens_decile)]
print("_ym_z_weekend") #####
#sort
setorder(rxnkaw, ZIP, weekend_dummy, personkey, pick_up_yrmon)
#index patients
rxnkaw[, personkey_ZIP_weekend_fill_index := seq_len(.N), by = c("personkey", "ZIP", "weekend_dummy")]
#sum first fills over zip code
rxnkaw[personkey_ZIP_weekend_fill_index == 1, total_new_pats_ym_z_weekend_nf := sum(personkey_ZIP_weekend_fill_index), by = c("pick_up_yrmon", "ZIP", "weekend_dummy")]
rxnkaw[, total_new_pats_ym_z_weekend := na.locf(total_new_pats_ym_z_weekend_nf), by = c("pick_up_yrmon", "ZIP", "weekend_dummy")]
rxnkaw[is.na(total_new_pats_ym_z_weekend), total_new_pats_ym_z_weekend := 0]
#index months
rxnkaw[, ym_z_weekend_index := seq_len(.N), by = .(pick_up_yrmon, ZIP, weekend_dummy)]
#cumsum over time
rxnkaw[ym_z_weekend_index == 1, cum_total_new_pats_ym_z_weekend_nf := cumsum(total_new_pats_ym_z_weekend), 
       by = .(ZIP, weekend_dummy)]
#fill
rxnkaw[, cum_total_new_pats_ym_z_weekend := na.locf(cum_total_new_pats_ym_z_weekend_nf), by = .(pick_up_yrmon, ZIP, weekend_dummy)]

print("_ym_z_empchronic") #####
#sort
setorder(rxnkaw, ZIP, emp_chronic_dummy, personkey, pick_up_yrmon)
#index patients
rxnkaw[, personkey_ZIP_empchronic_fill_index := seq_len(.N), by = c("personkey", "ZIP", "emp_chronic_dummy")]
#sum first fills over zip code
rxnkaw[personkey_ZIP_empchronic_fill_index == 1, total_new_pats_ym_z_empchronic_nf := sum(personkey_ZIP_empchronic_fill_index), by = c("pick_up_yrmon", "ZIP", "emp_chronic_dummy")]
rxnkaw[, total_new_pats_ym_z_empchronic := na.locf(total_new_pats_ym_z_empchronic_nf), by = c("pick_up_yrmon", "ZIP", "emp_chronic_dummy")]
rxnkaw[is.na(total_new_pats_ym_z_empchronic), total_new_pats_ym_z_empchronic := 0]
#index months
rxnkaw[, ym_z_empchronic_index := seq_len(.N), by = .(pick_up_yrmon, ZIP, emp_chronic_dummy)]
#cumsum over time
rxnkaw[ym_z_empchronic_index == 1, cum_total_new_pats_ym_z_empchronic_nf := cumsum(total_new_pats_ym_z_empchronic), 
       by = .(ZIP, emp_chronic_dummy)]
#fill
rxnkaw[, cum_total_new_pats_ym_z_empchronic := na.locf(cum_total_new_pats_ym_z_empchronic_nf), by = .(pick_up_yrmon, ZIP, emp_chronic_dummy)]


print("_ym_z_existing1") #####
#sort
setorder(rxnkaw, ZIP, existing1, personkey, pick_up_yrmon)
#index patients
rxnkaw[, personkey_ZIP_existing1_fill_index := seq_len(.N), by = c("personkey", "ZIP", "existing1")]
#sum first fills over zip code
rxnkaw[personkey_ZIP_existing1_fill_index == 1, total_new_pats_ym_z_existing1_nf := sum(personkey_ZIP_existing1_fill_index), by = c("pick_up_yrmon", "ZIP", "existing1")]
rxnkaw[, total_new_pats_ym_z_existing1 := na.locf(total_new_pats_ym_z_existing1_nf), by = c("pick_up_yrmon", "ZIP", "existing1")]
rxnkaw[is.na(total_new_pats_ym_z_existing1), total_new_pats_ym_z_existing1 := 0]
#index months
rxnkaw[, ym_z_existing1_index := seq_len(.N), by = .(pick_up_yrmon, ZIP, existing1)]
#cumsum over time
rxnkaw[ym_z_existing1_index == 1, cum_total_new_pats_ym_z_existing1_nf := cumsum(total_new_pats_ym_z_existing1), 
       by = .(ZIP, existing1)]
#fill
rxnkaw[, cum_total_new_pats_ym_z_existing1 := na.locf(cum_total_new_pats_ym_z_existing1_nf), by = .(pick_up_yrmon, ZIP, existing1)]


print("_ym_z_existing2") #####
#sort
setorder(rxnkaw, ZIP, existing2, personkey, pick_up_yrmon)
#index patients
rxnkaw[, personkey_ZIP_existing2_fill_index := seq_len(.N), by = c("personkey", "ZIP", "existing2")]
#sum first fills over zip code
rxnkaw[personkey_ZIP_existing2_fill_index == 1, total_new_pats_ym_z_existing2_nf := sum(personkey_ZIP_existing2_fill_index), by = c("pick_up_yrmon", "ZIP", "existing2")]
rxnkaw[, total_new_pats_ym_z_existing2 := na.locf(total_new_pats_ym_z_existing2_nf), by = c("pick_up_yrmon", "ZIP", "existing2")]
rxnkaw[is.na(total_new_pats_ym_z_existing2), total_new_pats_ym_z_existing2 := 0]
#index months
rxnkaw[, ym_z_existing2_index := seq_len(.N), by = .(pick_up_yrmon, ZIP, existing2)]
#cumsum over time
rxnkaw[ym_z_existing2_index == 1, cum_total_new_pats_ym_z_existing2_nf := cumsum(total_new_pats_ym_z_existing2), 
       by = .(ZIP, existing2)]
#fill
rxnkaw[, cum_total_new_pats_ym_z_existing2 := na.locf(cum_total_new_pats_ym_z_existing2_nf), by = .(pick_up_yrmon, ZIP, existing2)]



print("_ym_z_existing3") #####
#sort
setorder(rxnkaw, ZIP, existing3, personkey, pick_up_yrmon)
#index patients
rxnkaw[, personkey_ZIP_existing3_fill_index := seq_len(.N), by = c("personkey", "ZIP", "existing3")]
#sum first fills over zip code
rxnkaw[personkey_ZIP_existing3_fill_index == 1, total_new_pats_ym_z_existing3_nf := sum(personkey_ZIP_existing3_fill_index), by = c("pick_up_yrmon", "ZIP", "existing3")]
rxnkaw[, total_new_pats_ym_z_existing3 := na.locf(total_new_pats_ym_z_existing3_nf), by = c("pick_up_yrmon", "ZIP", "existing3")]
rxnkaw[is.na(total_new_pats_ym_z_existing3), total_new_pats_ym_z_existing3 := 0]
#index months
rxnkaw[, ym_z_existing3_index := seq_len(.N), by = .(pick_up_yrmon, ZIP, existing3)]
#cumsum over time
rxnkaw[ym_z_existing3_index == 1, cum_total_new_pats_ym_z_existing3_nf := cumsum(total_new_pats_ym_z_existing3), 
       by = .(ZIP, existing3)]
#fill
rxnkaw[, cum_total_new_pats_ym_z_existing3 := na.locf(cum_total_new_pats_ym_z_existing3_nf), by = .(pick_up_yrmon, ZIP, existing3)]



print("_ym_z_existing4") #####
#sort
setorder(rxnkaw, ZIP, existing4, personkey, pick_up_yrmon)
#index patients
rxnkaw[, personkey_ZIP_existing4_fill_index := seq_len(.N), by = c("personkey", "ZIP", "existing4")]
#sum first fills over zip code
rxnkaw[personkey_ZIP_existing4_fill_index == 1, total_new_pats_ym_z_existing4_nf := sum(personkey_ZIP_existing4_fill_index), by = c("pick_up_yrmon", "ZIP", "existing4")]
rxnkaw[, total_new_pats_ym_z_existing4 := na.locf(total_new_pats_ym_z_existing4_nf), by = c("pick_up_yrmon", "ZIP", "existing4")]
rxnkaw[is.na(total_new_pats_ym_z_existing4), total_new_pats_ym_z_existing4 := 0]
#index months
rxnkaw[, ym_z_existing4_index := seq_len(.N), by = .(pick_up_yrmon, ZIP, existing4)]
#cumsum over time
rxnkaw[ym_z_existing4_index == 1, cum_total_new_pats_ym_z_existing4_nf := cumsum(total_new_pats_ym_z_existing4), 
       by = .(ZIP, existing4)]
#fill
rxnkaw[, cum_total_new_pats_ym_z_existing4 := na.locf(cum_total_new_pats_ym_z_existing4_nf), by = .(pick_up_yrmon, ZIP, existing4)]


print("_ym_z_existing_c_1") #####
#sort
setorder(rxnkaw, ZIP, existing_c_1, personkey, pick_up_yrmon)
#index patients
rxnkaw[, personkey_ZIP_existing_c_1_fill_index := seq_len(.N), by = c("personkey", "ZIP", "existing_c_1")]
#sum first fills over zip code
rxnkaw[personkey_ZIP_existing_c_1_fill_index == 1, total_new_pats_ym_z_existing_c_1_nf := sum(personkey_ZIP_existing_c_1_fill_index), by = c("pick_up_yrmon", "ZIP", "existing_c_1")]
rxnkaw[, total_new_pats_ym_z_existing_c_1 := na.locf(total_new_pats_ym_z_existing_c_1_nf), by = c("pick_up_yrmon", "ZIP", "existing_c_1")]
rxnkaw[is.na(total_new_pats_ym_z_existing_c_1), total_new_pats_ym_z_existing_c_1 := 0]
#index months
rxnkaw[, ym_z_existing_c_1_index := seq_len(.N), by = .(pick_up_yrmon, ZIP, existing_c_1)]
#cumsum over time
rxnkaw[ym_z_existing_c_1_index == 1, cum_total_new_pats_ym_z_existing_c_1_nf := cumsum(total_new_pats_ym_z_existing_c_1), 
       by = .(ZIP, existing_c_1)]
#fill
rxnkaw[, cum_total_new_pats_ym_z_existing_c_1 := na.locf(cum_total_new_pats_ym_z_existing_c_1_nf), by = .(pick_up_yrmon, ZIP, existing_c_1)]


print("_ym_z_existing_c_2") #####
#sort
setorder(rxnkaw, ZIP, existing_c_2, personkey, pick_up_yrmon)
#index patients
rxnkaw[, personkey_ZIP_existing_c_2_fill_index := seq_len(.N), by = c("personkey", "ZIP", "existing_c_2")]
#sum first fills over zip code
rxnkaw[personkey_ZIP_existing_c_2_fill_index == 1, total_new_pats_ym_z_existing_c_2_nf := sum(personkey_ZIP_existing_c_2_fill_index), by = c("pick_up_yrmon", "ZIP", "existing_c_2")]
rxnkaw[, total_new_pats_ym_z_existing_c_2 := na.locf(total_new_pats_ym_z_existing_c_2_nf), by = c("pick_up_yrmon", "ZIP", "existing_c_2")]
rxnkaw[is.na(total_new_pats_ym_z_existing_c_2), total_new_pats_ym_z_existing_c_2 := 0]
#index months
rxnkaw[, ym_z_existing_c_2_index := seq_len(.N), by = .(pick_up_yrmon, ZIP, existing_c_2)]
#cumsum over time
rxnkaw[ym_z_existing_c_2_index == 1, cum_total_new_pats_ym_z_existing_c_2_nf := cumsum(total_new_pats_ym_z_existing_c_2), 
       by = .(ZIP, existing_c_2)]
#fill
rxnkaw[, cum_total_new_pats_ym_z_existing_c_2 := na.locf(cum_total_new_pats_ym_z_existing_c_2_nf), by = .(pick_up_yrmon, ZIP, existing_c_2)]




print("_ym_z_existing_w") #####
#sort
setorder(rxnkaw, ZIP, existing_w, personkey, pick_up_yrmon)
#index patients
rxnkaw[, personkey_ZIP_existing_w_fill_index := seq_len(.N), by = c("personkey", "ZIP", "existing_w")]
#sum first fills over zip code
rxnkaw[personkey_ZIP_existing_w_fill_index == 1, total_new_pats_ym_z_existing_w_nf := sum(personkey_ZIP_existing_w_fill_index), by = c("pick_up_yrmon", "ZIP", "existing_w")]
rxnkaw[, total_new_pats_ym_z_existing_w := na.locf(total_new_pats_ym_z_existing_w_nf), by = c("pick_up_yrmon", "ZIP", "existing_w")]
rxnkaw[is.na(total_new_pats_ym_z_existing_w), total_new_pats_ym_z_existing_w := 0]
#index months
rxnkaw[, ym_z_existing_w_index := seq_len(.N), by = .(pick_up_yrmon, ZIP, existing_w)]
#cumsum over time
rxnkaw[ym_z_existing_w_index == 1, cum_total_new_pats_ym_z_existing_w_nf := cumsum(total_new_pats_ym_z_existing_w), 
       by = .(ZIP, existing_w)]
#fill
rxnkaw[, cum_total_new_pats_ym_z_existing_w := na.locf(cum_total_new_pats_ym_z_existing_w_nf), by = .(pick_up_yrmon, ZIP, existing_w)]









#sort by patient-pickup date to find each patient's first fill
setkey(rxnkaw, personkey, pick_up_date) #first fill of any drug

#index the fills within each person
rxnkaw[, person_fill_index := seq_len(.N), by = c("personkey")]

#sum the first fills over the zip code month
rxnkaw[person_fill_index == 1, total_new_pats_ym_z_nf := sum(person_fill_index), by = c("ZIP","pick_up_yrmon")]
rxnkaw[, total_new_pats_ym_z := na.locf(total_new_pats_ym_z_nf), by = c("ZIP","pick_up_yrmon")]
rxnkaw[is.na(total_new_pats_ym_z), total_new_pats_ym_z := 0]

#first fill of each drug
setkey(rxnkaw, personkey, rx2, pick_up_date)
#index the fills within each person
rxnkaw[, person_drug_fill_index := seq_len(.N), by = c("personkey","rx2")]

#sum the first fills over the zip code month drug
rxnkaw[person_drug_fill_index == 1, total_new_pats_ym_rx_z_nf := sum(person_drug_fill_index), 
       by = c("ZIP","pick_up_yrmon", "rx2")]
rxnkaw[, total_new_pats_ym_rx_z := na.locf(total_new_pats_ym_rx_z_nf), 
       by = c("ZIP","pick_up_yrmon", "rx2")]
rxnkaw[is.na(total_new_pats_ym_rx_z), total_new_pats_ym_rx_z := 0]
gc()
#first fill with insurance 8
setkey(rxnkaw, personkey, insurance_type_8, pick_up_date)
#index the fills within each person
rxnkaw[, person_ins_8_fill_index := seq_len(.N), by = c("personkey","insurance_type_8")]

#sum the first fills over the zip code month
rxnkaw[person_ins_8_fill_index == 1, total_new_pats_ym_ins_8_z_nf := sum(person_ins_8_fill_index), 
       by = c("ZIP","pick_up_yrmon", "insurance_type_8")]
rxnkaw[, total_new_pats_ym_ins_8_z := na.locf(total_new_pats_ym_ins_8_z_nf), 
       by = c("ZIP","pick_up_yrmon", "insurance_type_8")]
rxnkaw[is.na(total_new_pats_ym_ins_8_z), total_new_pats_ym_ins_8_z := 0]
gc()

#sum the first fills over the month
rxnkaw[person_ins_8_fill_index == 1, total_new_pats_ym_ins_8_nf := sum(person_ins_8_fill_index), 
       by = c("pick_up_yrmon", "insurance_type_8")]
rxnkaw[, total_new_pats_ym_ins_8 := na.locf(total_new_pats_ym_ins_8_nf), 
       by = c("pick_up_yrmon", "insurance_type_8")]
rxnkaw[is.na(total_new_pats_ym_ins_8), total_new_pats_ym_ins_8 := 0]
gc()


#first fill with insurance 8 by drug
setkey(rxnkaw, personkey, insurance_type_8, rx2, pick_up_date)
#index the fills within each person
rxnkaw[, person_ins_8_rx_fill_index := seq_len(.N), by = c("personkey", "insurance_type_8","rx2")]

#sum the first fills over the month drug
rxnkaw[person_ins_8_rx_fill_index == 1, total_new_pats_ym_ins_8_rx_nf := sum(person_ins_8_rx_fill_index), 
       by = c("pick_up_yrmon", "insurance_type_8","rx2")]
rxnkaw[, total_new_pats_ym_ins_8_rx := na.locf(total_new_pats_ym_ins_8_rx_nf), 
       by = c("pick_up_yrmon", "insurance_type_8","rx2")]
rxnkaw[is.na(total_new_pats_ym_ins_8_rx), total_new_pats_ym_ins_8_rx := 0]
gc()

#first fill with insurance 23 by drug
setkey(rxnkaw, personkey, insurance_type_23, rx2, pick_up_date)
#index the fills within each person
rxnkaw[, person_ins_23_rx_fill_index := seq_len(.N), by = c("personkey", "insurance_type_23","rx2")]

#sum the first fills over the month drug
rxnkaw[person_ins_23_rx_fill_index == 1, total_new_pats_ym_ins_23_rx_nf := sum(person_ins_23_rx_fill_index), 
       by = c("pick_up_yrmon", "insurance_type_23","rx2")]
rxnkaw[, total_new_pats_ym_ins_23_rx := na.locf(total_new_pats_ym_ins_23_rx_nf), 
       by = c("pick_up_yrmon", "insurance_type_23","rx2")]
rxnkaw[is.na(total_new_pats_ym_ins_23_rx), total_new_pats_ym_ins_23_rx := 0]
gc()

#first fill with insurance 6 by drug
setkey(rxnkaw, personkey, insurance_type_6, rx2, pick_up_date)
#index the fills within each person
rxnkaw[, person_ins_6_rx_fill_index := seq_len(.N), by = c("personkey", "insurance_type_6","rx2")]
rxnkaw[person_ins_6_rx_fill_index == 1, total_new_pats_ym_ins_6_rx_nf := sum(person_ins_6_rx_fill_index), 
       by = c("pick_up_yrmon", "insurance_type_6","rx2")]
rxnkaw[, total_new_pats_ym_ins_6_rx := na.locf(total_new_pats_ym_ins_6_rx_nf), 
       by = c("pick_up_yrmon", "insurance_type_6","rx2")]
rxnkaw[is.na(total_new_pats_ym_ins_6_rx), total_new_pats_ym_ins_6_rx := 0]
gc()

#first fill with insurance 23
setkey(rxnkaw, personkey, insurance_type_23, pick_up_date)
#index the fills within each person
rxnkaw[, person_ins_23_fill_index := seq_len(.N), by = c("personkey", "insurance_type_23")]
rxnkaw[, person_ins_6_fill_index := seq_len(.N), by = c("personkey", "insurance_type_6")]

#sum the first fills over the zip code month
rxnkaw[person_ins_23_fill_index == 1, total_new_pats_ym_ins_23_z_nf := sum(person_ins_23_fill_index), 
       by = c("ZIP","pick_up_yrmon", "insurance_type_23")]
rxnkaw[person_ins_6_fill_index == 1, total_new_pats_ym_ins_6_z_nf := sum(person_ins_23_fill_index), 
       by = c("ZIP","pick_up_yrmon", "insurance_type_6")]
rxnkaw[, total_new_pats_ym_ins_23_z := na.locf(total_new_pats_ym_ins_23_z_nf), 
       by = c("ZIP","pick_up_yrmon", "insurance_type_23")]
rxnkaw[is.na(total_new_pats_ym_ins_23_z), total_new_pats_ym_ins_23_z := 0]
rxnkaw[, total_new_pats_ym_ins_6_z := na.locf(total_new_pats_ym_ins_6_z_nf), 
       by = c("ZIP","pick_up_yrmon", "insurance_type_6")]
rxnkaw[is.na(total_new_pats_ym_ins_6_z), total_new_pats_ym_ins_6_z := 0]

rxnkaw[person_ins_23_fill_index == 1, total_new_pats_y_ins_23_c_nf := sum(person_ins_23_fill_index), 
       by = c("county","year", "insurance_type_23")]
rxnkaw[person_ins_6_fill_index == 1, total_new_pats_y_ins_6_c_nf := sum(person_ins_23_fill_index), 
       by = c("county","year", "insurance_type_6")]
rxnkaw[, total_new_pats_y_ins_23_c := na.locf(total_new_pats_y_ins_23_c_nf), 
       by = c("county","year", "insurance_type_23")]
rxnkaw[is.na(total_new_pats_y_ins_23_c), total_new_pats_y_ins_23_c := 0]
rxnkaw[, total_new_pats_y_ins_6_c := na.locf(total_new_pats_y_ins_6_c_nf), 
       by = c("county","year", "insurance_type_6")]
rxnkaw[is.na(total_new_pats_y_ins_6_c), total_new_pats_y_ins_6_c := 0]
gc()

#first fill with insurance 23 - rx2
setkey(rxnkaw, personkey, insurance_type_23, rx2, pick_up_date)
#index the fills within each person
rxnkaw[, person_ins_23_rx_fill_index := seq_len(.N), by = c("personkey", "insurance_type_23","rx2")]

#sum the first fills over the zip code month
rxnkaw[person_ins_23_rx_fill_index == 1, total_new_pats_ym_ins_23_rx_z_nf := sum(person_ins_23_rx_fill_index), 
       by = c("ZIP","pick_up_yrmon", "insurance_type_23","rx2")]
rxnkaw[, total_new_pats_ym_ins_23_rx_z := na.locf(total_new_pats_ym_ins_23_rx_z_nf), 
       by = c("ZIP","pick_up_yrmon", "insurance_type_23","rx2")]
rxnkaw[is.na(total_new_pats_ym_ins_23_rx_z), total_new_pats_ym_ins_23_rx_z := 0]

rxnkaw[person_ins_23_rx_fill_index == 1, total_new_pats_y_ins_23_rx_c_nf := sum(person_ins_23_rx_fill_index), 
       by = c("county","year", "insurance_type_23","rx2")]
rxnkaw[, total_new_pats_y_ins_23_rx_c := na.locf(total_new_pats_y_ins_23_rx_c_nf), 
       by = c("county","year", "insurance_type_23","rx2")]
rxnkaw[is.na(total_new_pats_y_ins_23_rx_c), total_new_pats_y_ins_23_rx_c := 0]
gc()

#first fill with insurance 6 - rx2
setkey(rxnkaw, personkey, insurance_type_6, rx2, pick_up_date)
#index the fills within each person
rxnkaw[, person_ins_6_rx_fill_index := seq_len(.N), by = c("personkey", "insurance_type_6","rx2")]

#sum the first fills over the zip code month
rxnkaw[person_ins_6_rx_fill_index == 1, total_new_pats_ym_ins_6_rx_z_nf := sum(person_ins_6_rx_fill_index), 
       by = c("ZIP","pick_up_yrmon", "insurance_type_6","rx2")]
rxnkaw[, total_new_pats_ym_ins_6_rx_z := na.locf(total_new_pats_ym_ins_6_rx_z_nf), 
       by = c("ZIP","pick_up_yrmon", "insurance_type_6","rx2")]
rxnkaw[is.na(total_new_pats_ym_ins_6_rx_z), total_new_pats_ym_ins_6_rx_z := 0]

rxnkaw[person_ins_6_rx_fill_index == 1, total_new_pats_y_ins_6_rx_c_nf := sum(person_ins_6_rx_fill_index), 
       by = c("county","year", "insurance_type_6","rx2")]
rxnkaw[, total_new_pats_y_ins_6_rx_c := na.locf(total_new_pats_y_ins_6_rx_c_nf), 
       by = c("county","year", "insurance_type_6","rx2")]
rxnkaw[is.na(total_new_pats_y_ins_6_rx_c), total_new_pats_y_ins_6_rx_c := 0]
gc()

#first fill with insurance 23 - pharmtype
setkey(rxnkaw, personkey, insurance_type_23, entity_broad, pick_up_date)
#index the fills within each person
rxnkaw[, person_ins_23_pharmtype_fill_index := seq_len(.N), by = c("personkey", "insurance_type_23","entity_broad")]

#sum the first fills over the zip code month
rxnkaw[person_ins_23_pharmtype_fill_index == 1, total_new_pats_ym_ins_23_pharmtype_z_nf := sum(person_ins_23_pharmtype_fill_index), 
       by = c("ZIP","pick_up_yrmon", "insurance_type_23","entity_broad")]
rxnkaw[, total_new_pats_ym_ins_23_pharmtype_z := na.locf(total_new_pats_ym_ins_23_pharmtype_z_nf), 
       by = c("ZIP","pick_up_yrmon", "insurance_type_23","entity_broad")]
rxnkaw[is.na(total_new_pats_ym_ins_23_pharmtype_z), total_new_pats_ym_ins_23_pharmtype_z := 0]
gc()


#first fill with insurance 53
setkey(rxnkaw, personkey, insurance_type_53, pick_up_date)
#index the fills within each person
rxnkaw[, person_ins_53_fill_index := seq_len(.N), by = c("personkey", "insurance_type_53")]

#sum the first fills over the zip code month
rxnkaw[person_ins_53_fill_index == 1, total_new_pats_ym_ins_53_z_nf := sum(person_ins_53_fill_index), 
       by = c("ZIP","pick_up_yrmon", "insurance_type_53")]
rxnkaw[, total_new_pats_ym_ins_53_z := na.locf(total_new_pats_ym_ins_53_z_nf), 
       by = c("ZIP","pick_up_yrmon", "insurance_type_53")]
rxnkaw[is.na(total_new_pats_ym_ins_53_z), total_new_pats_ym_ins_53_z := 0]

#first fill with insurance 18
setkey(rxnkaw, personkey, insurance_type_18, pick_up_date)
#index the fills within each person
rxnkaw[, person_ins_18_fill_index := seq_len(.N), by = c("personkey", "insurance_type_18")]

#sum the first fills over the zip code month
rxnkaw[person_ins_18_fill_index == 1, total_new_pats_ym_ins_18_z_nf := sum(person_ins_18_fill_index), 
       by = c("ZIP","pick_up_yrmon", "insurance_type_18")]
rxnkaw[, total_new_pats_ym_ins_18_z := na.locf(total_new_pats_ym_ins_18_z_nf), 
       by = c("ZIP","pick_up_yrmon", "insurance_type_18")]
rxnkaw[is.na(total_new_pats_ym_ins_18_z), total_new_pats_ym_ins_18_z := 0]

gc()
#first fill with mail
setkey(rxnkaw, personkey, mail_order_dummy, pick_up_date)
#index the fills within each person
rxnkaw[, person_mail_fill_index := seq_len(.N), by = c("personkey", "mail_order_dummy")]

#sum the first fills over the zip code month
rxnkaw[person_mail_fill_index == 1, total_new_pats_ym_mail_z_nf := sum(person_mail_fill_index), 
       by = c("ZIP","pick_up_yrmon", "mail_order_dummy")]
rxnkaw[, total_new_pats_ym_mail_z := na.locf(total_new_pats_ym_mail_z_nf), 
       by = c("ZIP","pick_up_yrmon", "mail_order_dummy")]
rxnkaw[is.na(total_new_pats_ym_mail_z), total_new_pats_ym_mail_z := 0]
gc()

#first fill with pharmtype
setkey(rxnkaw, personkey, entity_broad, pick_up_date)
#index the fills within each person
rxnkaw[, person_pharmtype_fill_index := seq_len(.N), by = c("personkey", "entity_broad")]

#sum the first fills over the zip code month
rxnkaw[person_pharmtype_fill_index == 1, total_new_pats_ym_pharmtype_z_nf := sum(person_pharmtype_fill_index), 
       by = c("ZIP","pick_up_yrmon", "entity_broad")]
rxnkaw[, total_new_pats_ym_pharmtype_z := na.locf(total_new_pats_ym_pharmtype_z_nf), 
       by = c("ZIP","pick_up_yrmon", "entity_broad")]
rxnkaw[is.na(total_new_pats_ym_pharmtype_z), total_new_pats_ym_pharmtype_z := 0]
gc()
#first fill with 30v90
# setkey(rxnkaw, personkey, rxdays_broad, pick_up_date)
# #index the fills within each person
# rxnkaw[, person_30v90_fill_index := seq_len(.N), by = c("personkey")]

#sum the first fills over the zip code month
# rxnkaw[person_30v90_fill_index == 1, total_new_pats_ym_30v90_z_nf := sum(person_30v90_fill_index), 
#        by = c("ZIP","pick_up_yrmon", "rxdays_broad")]
# rxnkaw[, total_new_pats_ym_30v90_z := na.locf(total_new_pats_ym_30v90_z_nf), 
#        by = c("ZIP","pick_up_yrmon", "rxdays_broad")]
# rxnkaw[is.na(total_new_pats_ym_30v90_z), total_new_pats_ym_30v90_z := 0]
gc()




####pills per person####
rxnkaw[, pills_per_person_ym := sum_qtydisp_ym / unique_personkey_ym]
rxnkaw[, pills_per_person_ym_z := sum_qtydisp_ym_z / unique_personkey_ym_z]
rxnkaw[, pills_per_person_ym_c := sum_qtydisp_ym_c / unique_personkey_ym_c]

rxnkaw[, pills_per_person_ym_z_rx2 := sum_qtydisp_ym_z_rx2 / unique_personkey_ym_z_rx2]
rxnkaw[, pills_per_person_ym_c_rx2 := sum_qtydisp_ym_c_rx2 / unique_personkey_ym_c_rx2]

rxnkaw[, pills_per_person_ym_z_mail := sum_qtydisp_ym_z_mail / unique_personkey_ym_z_mail]

rxnkaw[, pills_per_person_ym_z_ins23 := sum_qtydisp_ym_z_ins23 / unique_personkey_ym_z_ins23]
rxnkaw[, pills_per_person_ym_z_ins53 := sum_qtydisp_ym_z_ins53 / unique_personkey_ym_z_ins53]
rxnkaw[, pills_per_person_ym_z_ins6 := sum_qtydisp_ym_z_ins6 / unique_personkey_ym_z_ins6]
rxnkaw[, pills_per_person_ym_z_ins8 := sum_qtydisp_ym_z_ins8 / unique_personkey_ym_z_ins8]

rxnkaw[, pills_per_person_ym_z_pharmtype := sum_qtydisp_ym_z_pharmtype / unique_personkey_ym_z_pharmtype]
# rxnkaw[, pills_per_person_ym_z_branded := sum_qtydisp_ym_z_branded / unique_personkey_ym_z_branded]
rxnkaw[, pills_per_person_ym_z_drugpricedecile := sum_qtydisp_ym_z_drugpricedecile / unique_personkey_ym_z_drugpricedecile]
rxnkaw[, pills_per_person_ym_z_drugpricedecile_closing := sum_qtydisp_ym_z_drugpricedecile_closing / unique_personkey_ym_z_drugpricedecile_closing]
rxnkaw[, pills_per_person_ym_z_drugpricedecile_walgreens := sum_qtydisp_ym_z_drugpricedecile_walgreens / unique_personkey_ym_z_drugpricedecile_walgreens]
rxnkaw[, pills_per_person_ym_z_weekend := sum_qtydisp_ym_z_weekend / unique_personkey_ym_z_weekend]
# rxnkaw[, pills_per_person_ym_z_chronic := sum_qtydisp_ym_z_chronic / unique_personkey_ym_z_chronic]
rxnkaw[, pills_per_person_ym_z_empchronic := sum_qtydisp_ym_z_empchronic / unique_personkey_ym_z_empchronic]
rxnkaw[, pills_per_person_ym_z_existing1 := sum_qtydisp_ym_z_existing1 / unique_personkey_ym_z_existing1]
rxnkaw[, pills_per_person_ym_z_existing2 := sum_qtydisp_ym_z_existing2 / unique_personkey_ym_z_existing2]
rxnkaw[, pills_per_person_ym_z_existing3 := sum_qtydisp_ym_z_existing3 / unique_personkey_ym_z_existing3]
rxnkaw[, pills_per_person_ym_z_existing4 := sum_qtydisp_ym_z_existing4 / unique_personkey_ym_z_existing4]

rxnkaw[, pills_per_person_ym_z_existing_c_1 := sum_qtydisp_ym_z_existing_c_1 / unique_personkey_ym_z_existing_c_1]
rxnkaw[, pills_per_person_ym_z_existing_c_2 := sum_qtydisp_ym_z_existing_c_2 / unique_personkey_ym_z_existing_c_2]

rxnkaw[, pills_per_person_ym_z_existing_w := sum_qtydisp_ym_z_existing_w / unique_personkey_ym_z_existing_w]

gc()



####total stock of pharmacies in each ZIP####
##ZIP-pharmacy index
rxnkaw[, zip_pharm_month_index := NULL]
rxnkaw[pharm_state == "OR"
       & pharm_zip == ZIP,
       zip_pharm_month_index := seq_len(.N), 
       by=c("ZIP","pharm_add_full","pick_up_yrmon")]

rxnkaw[, tot_pharms_ym_z_nf := NULL]
rxnkaw[zip_pharm_month_index == 1, 
       tot_pharms_ym_z_nf := length(unique(pharm_add_full)), 
       by = c("pick_up_yrmon","ZIP")]

rxnkaw[, tot_pharms_ym_z := NULL]
rxnkaw[, tot_pharms_ym_z := na.locf(tot_pharms_ym_z_nf),
       by = c("pick_up_yrmon","ZIP")]
rxnkaw[, tot_pharms_ym_z_nf := NULL]

#replace the na in OR with 0
rxnkaw[is.na(tot_pharms_ym_z) & STATE == "OR",
       tot_pharms_ym_z := 0]

rxnkaw[,max_tot_pharms_y_z := max(tot_pharms_ym_z), by = c("ZIP","year")]


rxnkaw[, z_y_index := seq_len(.N), by = c("ZIP","year")]
rxnkaw[, tot_pharms_y_c_nf := NULL]
rxnkaw[z_y_index==1,
       tot_pharms_y_c_nf := sum(max_tot_pharms_y_z), 
       by = c("year","county")]
rxnkaw[, tot_pharms_y_c := na.locf(tot_pharms_y_c_nf),
       by = c("year","county")]

gc()


rm(entity, entity_nf, full, grocery, mail_order_npi_list_1, mail_order_npi_list_2, 
   new_pharm_add, new_pharm_add_nf, nf, opening_entity_broad, X)
gc()


#####
nf_names = grep("nf", names(rxnkaw), value=T)
rxnkaw[,(nf_names):=NULL,with=F]
names(rxnkaw)

gc()
#

rm(j, fifth_fill_prior_to_opening, fifth_fill_prior_to_opening_nf,
   mean_mpr_ym_rx_z_secondary, mean_mpr_ym_rx_z_secondary_chronic,
   mean_mpr_ym_rx_z_secondary_nf, mean_mpr_ym_rx_z_secondary_chronic_nf,
   ms, nf_names, second_fill_prior_to_opening, second_fill_prior_to_opening_nf)
gc()

####END TOTAL CREATION####
print("saving")
print(Sys.time())
a = Sys.time()
save(rxnkaw, file="~/current/pharmacy_deserts/data/rxnkaw_after_total_creation.RData")
b = Sys.time()
print(b - a)
STOP
###################################################################################################################################################################################
####Section 3: generate controls ####
###################################################################################################################################################################################

# # ####factor variables for fixed effects####
rxnkaw[, zip_factor := as.factor(ZIP)]
rxnkaw[, yrmon_factor := as.factor(pick_up_yrmon)]
rxnkaw[, yrmon_number := as.numeric(pick_up_yrmon)]
rxnkaw[, yrqtr_factor := as.factor(pick_up_yrqtr)]
#
#
#
#
# ####balance the events####
# a dummy for if the claim is usable
rxnkaw[, usable := ifelse(
  new_pharm == 1
  & last_year_mon >= opening_year_mon + .5 # I want the openings to last
  & !is.na(zip_open_index) #takes out the obs where other zip codes shopped at the pharm
  & opening_year_mon > "Jun 2011" & opening_year_mon < "Jul 2013" #this is so I have 6 months before and 6 after
  , 1, 0)
  ]
unique(rxnkaw[usable == 1, .(opening_year_mon, pharm_zip)])

#closings
rxnkaw[, usable_c := ifelse(
  closing_pharm == 1
  & last_year_mon >= opening_year_mon + .5 # I want the openings to last
  & !is.na(zip_close_index) #takes out the obs where other zip codes shopped at the pharm
  & last_year_mon > "Jun 2011" & last_year_mon < "Jul 2013" #this is so I have 6 months before and 6 after
  , 1, 0)
  ]
unique(rxnkaw[usable_c == 1, .(last_year_mon, pharm_zip)]) #17?

#the whole ZIP should be usable if there is a usable claim from it
rxnkaw[zip_open_index == 1, usable_nf_1 := max(usable), by = ZIP]
rxnkaw[, usable_1 := na.locf(usable_nf_1), by = ZIP]
rxnkaw[zip_open_index == 2, usable_nf_2 := max(usable), by = ZIP]
rxnkaw[, usable_2 := na.locf(usable_nf_2), by = ZIP]
rxnkaw[zip_open_index == 3, usable_nf_3 := max(usable), by = ZIP]
rxnkaw[, usable_3 := na.locf(usable_nf_3), by = ZIP]
rxnkaw[zip_open_index == 4, usable_nf_4 := max(usable), by = ZIP]
rxnkaw[, usable_4 := na.locf(usable_nf_4), by = ZIP]

#closings
rxnkaw[zip_close_index == 1, usable_c_nf_1 := max(usable_c), by = ZIP]
rxnkaw[, usable_c_1 := na.locf(usable_c_nf_1), by = ZIP]
rxnkaw[zip_close_index == 2, usable_c_nf_2 := max(usable_c), by = ZIP]
rxnkaw[, usable_c_2 := na.locf(usable_c_nf_2), by = ZIP]


#the second opening is not usable if the second opening is the same month as the first
rxnkaw[usable_2 == 1
       & open_ym_zip_1 == open_ym_zip_2,
       usable_2:=0]
rxnkaw[usable_3 == 1
       & (open_ym_zip_3 == open_ym_zip_2 | open_ym_zip_3 == open_ym_zip_1),
       usable_3:=0]
rxnkaw[usable_4 == 1
       & (open_ym_zip_4 == open_ym_zip_3 | open_ym_zip_4 == open_ym_zip_2 | open_ym_zip_4 == open_ym_zip_1),
       usable_4:=0]

#closings
rxnkaw[usable_c_2 == 1
       & close_ym_zip_1 == close_ym_zip_2,
       usable_c_2:=0]

#there should be 36 opening events
#how many events are the first openings in the zip code
unique(rxnkaw[usable_1 == 1,.(open_ym_zip_1, ZIP)]) #26
unique(rxnkaw[usable_2 == 1,.(open_ym_zip_2, ZIP)]) #7
unique(rxnkaw[usable_3 == 1,.(open_ym_zip_3, ZIP)]) #2
unique(rxnkaw[usable_4 == 1,.(open_ym_zip_4, ZIP)]) #1
#36!
opening_zips = sort(unique(rxnkaw[usable_1 == 1 | usable_2 == 1| usable_3 == 1 | usable_4 == 1,]$ZIP)) #30

#there should be 17 closing events
unique(rxnkaw[usable_c_1 == 1,.(close_ym_zip_1, ZIP)])
unique(rxnkaw[usable_c_2 == 1,.(close_ym_zip_2, ZIP)])
#17?
closing_zips = sort(unique(rxnkaw[usable_c_1 == 1 | usable_c_2 == 1,]$ZIP)) #?
#
#
# gc()
#
# ######################################################################################################
# ####This is the balance requirement####
# B = 6
# ######################################################################################################
#
#
#
#
# ####opener_zip Dummy####
rxnkaw[, opener_zip_dummy := ifelse(ZIP %in% opening_zips, 1, 0)]
rxnkaw[, closer_zip_dummy := ifelse(ZIP %in% closing_zips, 1, 0)]
# gc()
#
#

#
# ####add in controls####
#
# ####get urban vs rural status from ACS####
DEC_10_SF1_P2_with_ann = as.data.table(read.csv("~/current/pharmacy_deserts/data/urban_rural/DEC_10_SF1_P2_with_ann.csv"))
DEC_10_SF1_P2_with_ann[, percent_urban := Urban. / Total.]
DEC_10_SF1_P2_with_ann[, percent_rural := Rural / Total.]
DEC_10_SF1_P2_with_ann[, urban_dummy := ifelse(percent_urban >= 0.75, 1, 0)]
setkey(DEC_10_SF1_P2_with_ann, ZIP)
setkey(rxnkaw, ZIP)

rxnkaw[, percent_urban := NULL]
rxnkaw[, percent_rural := NULL]
rxnkaw[, urban_dummy := NULL]
rxnkaw[DEC_10_SF1_P2_with_ann, percent_urban := percent_urban]
rxnkaw[DEC_10_SF1_P2_with_ann, percent_rural := percent_rural]
rxnkaw[DEC_10_SF1_P2_with_ann, urban_dummy:= urban_dummy]

#how many of the opening zip codes do we know urban/rural status?
unique(rxnkaw[opener_zip == 1, ]$urban_dummy)
#looks like we have it for all of them!

####ZBP####

#load in the data
BP_2010_00CZ2_with_ann <- as.data.table(read.csv("~/current/pharmacy_deserts/data/zpb/BP_2010_00CZ2_with_ann.csv"))
BP_2011_00CZ2_with_ann <- as.data.table(read.csv("~/current/pharmacy_deserts/data/zpb/BP_2011_00CZ2_with_ann.csv"))
BP_2012_00CZ2_with_ann <- as.data.table(read.csv("~/current/pharmacy_deserts/data/zpb/BP_2012_00CZ2_with_ann.csv"))
BP_2013_00CZ2_with_ann <- as.data.table(read.csv("~/current/pharmacy_deserts/data/zpb/BP_2013_00CZ2_with_ann.csv"))

BP_2010_00CZ2_with_ann[, year := Year]
BP_2011_00CZ2_with_ann[, year := Year]
BP_2012_00CZ2_with_ann[, year := Year]
BP_2013_00CZ2_with_ann[, year := Year]

BP_2010_00CZ2_with_ann[, naics_def := Meaning.of.2007.NAICS.code]
BP_2011_00CZ2_with_ann[, naics_def := Meaning.of.2007.NAICS.code]
BP_2012_00CZ2_with_ann[, naics_def := Meaning.of.2012.NAICS.code]
BP_2013_00CZ2_with_ann[, naics_def := Meaning.of.2012.NAICS.code]

BP_2010_00CZ2_with_ann[, emp_size_def := Meaning.of.Employment.size.of.establishment]
BP_2011_00CZ2_with_ann[, emp_size_def := Meaning.of.Employment.size.of.establishment]
BP_2012_00CZ2_with_ann[, emp_size_def := Meaning.of.Employment.size.of.establishment]
BP_2013_00CZ2_with_ann[, emp_size_def := Meaning.of.Employment.size.of.establishment]

BP_2010_00CZ2_with_ann[, num_estabs := Number.of.establishments]
BP_2011_00CZ2_with_ann[, num_estabs := Number.of.establishments]
BP_2012_00CZ2_with_ann[, num_estabs := Number.of.establishments]
BP_2013_00CZ2_with_ann[, num_estabs := Number.of.establishments]

BP_2010_00CZ2_with_ann[, ZIP := Id2]
BP_2011_00CZ2_with_ann[, ZIP := Id2]
BP_2012_00CZ2_with_ann[, ZIP := Id2]
BP_2013_00CZ2_with_ann[, ZIP := Id2]


zbp = rbindlist(list(BP_2010_00CZ2_with_ann,
                     BP_2011_00CZ2_with_ann,
                     BP_2012_00CZ2_with_ann,
                     BP_2013_00CZ2_with_ann),
                fill=T)


#get the total number of firms for all sectors and for health care in a given zip code year
zbp = zbp[naics_def %in% c("Total for all sectors","Health care and social assistance"),]

#only look at total number of establishments. Size of establishments might come in later
zbp = zbp[emp_size_def == "All establishments",]

#should be 8 obs for each zip
zbp[, zip_obs := .N, by = ZIP]
zbp[naics_def == "Health care and social assistance", health_zip_obs := .N, by=ZIP]
zbp[naics_def == "Total for all sectors", all_zip_obs := .N, by=ZIP]

#######NEED to be more careful with this data####
#######################################!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#"reshape" so each obs is a zip-year
zbp[, total_health_estabs := shift(num_estabs, n=1, fill=NA, type="lead"),
    by = c("ZIP", "year")]
zbp[, total_estabs := num_estabs]

zbp = zbp[!is.na(total_health_estabs),
          .(ZIP, year, total_health_estabs, total_estabs)]

####merge the zbp data on to the rxnkaw####
setkey(zbp, ZIP, year)

setkey(rxnkaw, ZIP, year)

#the merge
rxnkaw[zbp, total_health_estabs := total_health_estabs]
rxnkaw[zbp, total_estabs := total_estabs]


#####get total population of the zip code ####
ACS_11_5YR_S0101_with_ann <- as.data.table(read.csv("~/current/pharmacy_deserts/data/acs_age_sex/ACS_11_5YR_S0101_with_ann.csv"))
ACS_12_5YR_S0101_with_ann <- as.data.table(read.csv("~/current/pharmacy_deserts/data/acs_age_sex/ACS_12_5YR_S0101_with_ann.csv"))
ACS_13_5YR_S0101_with_ann <- as.data.table(read.csv("~/current/pharmacy_deserts/data/acs_age_sex/ACS_13_5YR_S0101_with_ann.csv"))

ACS_11_5YR_S0101_with_ann[, year := 2011]
ACS_12_5YR_S0101_with_ann[, year := 2012]
ACS_13_5YR_S0101_with_ann[, year := 2013]

acs = rbindlist(list(ACS_11_5YR_S0101_with_ann,ACS_12_5YR_S0101_with_ann,ACS_13_5YR_S0101_with_ann),fill=T)

setkey(acs, Id2, year)
setkey(rxnkaw, ZIP, pick_up_year)

rxnkaw[, tot_pop := NULL]
rxnkaw[acs, tot_pop := as.numeric(as.character(Total..Estimate..Total.population))]



#####get total population change of the zip code ####
DEC_10_SF1_QTP1_with_ann <- as.data.table(read.csv("~/current/pharmacy_deserts/data/acs_pharm_entry/DEC_10_SF1_QTP1_with_ann.csv"))
DEC_00_SF1_P012_with_ann <- as.data.table(read.csv("~/current/pharmacy_deserts/data/acs_pharm_entry/DEC_00_SF1_P012_with_ann.csv"))

DEC_10_SF1_QTP1_with_ann[, year := 2010]
DEC_00_SF1_P012_with_ann[, year := 2000]


acs = rbindlist(list(DEC_00_SF1_P012_with_ann,DEC_10_SF1_QTP1_with_ann),fill=T)


setkey(acs, Id2, year)
acs[, pop := ifelse(year == 2010,Number...Both.sexes..Total.population, Total.)]
acs[, lag_pop := shift(pop), by = Id2]
acs[, pop_change := pop - lag_pop]


acs[, pop_65 := ifelse(year == 2010,Number...Both.sexes..Total.population...65.years.and.over,
                       Female....65.and.66.years + Female....67.to.69.years + Female....70.to.74.years +
                         Female....75.to.79.years + Female....80.to.84.years + Female....85.years.and.over +
                         Male....65.and.66.years + Male....67.to.69.years + Male....70.to.74.years +
                         Male....75.to.79.years + Male....80.to.84.years + Male....85.years.and.over)]
acs[, lag_pop_65 := shift(pop_65), by = Id2]
acs[, pop_65_change := pop_65 - lag_pop_65]


acs = acs[!is.na(pop_change),]

acs[, ZIP := as.integer(as.character(Id2))]

setkey(acs, ZIP)
setkey(rxnkaw, ZIP)

rxnkaw[, pop_change := NULL]
rxnkaw[acs, pop_change := as.numeric(as.character(pop_change))]

rxnkaw[, pop_65_change := NULL]
rxnkaw[acs, pop_65_change := as.numeric(as.character(pop_65_change))]
#
#
# ####get the total number of patients who use mail order at all  in each zip year ####
rxnkaw[, person_mail_order_dummy := max(mail_order_dummy),
       by = c("personkey","pick_up_year")]
rxnkaw[, person_mail_order_dummy_month := max(mail_order_dummy),
       by = c("personkey","pick_up_yrmon")]

#one obs per person-year
rxnkaw[, person_year_index := seq_len(.N), by = c("personkey","pick_up_year")]
rxnkaw[, person_month_index := seq_len(.N), by = c("personkey","pick_up_month")]
gc()
rxnkaw[, total_mail_order_patients_zip_year_nf := NULL]
rxnkaw[, total_mail_order_patients_zip_year := NULL]
rxnkaw[person_year_index == 1,
       total_mail_order_patients_zip_year_nf := sum(person_mail_order_dummy, na.rm=T),
       by = c("ZIP","pick_up_year")]
rxnkaw[,total_mail_order_patients_zip_year := na.locf(total_mail_order_patients_zip_year_nf),
       by = c("ZIP","pick_up_year")]

rxnkaw[, total_mail_order_patients_zip_month_nf := NULL]
rxnkaw[, total_mail_order_patients_zip_month := NULL]
rxnkaw[person_month_index == 1,
       total_mail_order_patients_zip_month_nf := sum(person_mail_order_dummy_month, na.rm=T),
       by = c("ZIP","pick_up_yrmon")]
rxnkaw[,total_mail_order_patients_zip_month := na.locf(total_mail_order_patients_zip_month_nf),
       by = c("ZIP","pick_up_yrmon")]
gc()
#mail order users divided by total population of the zip code
# rxnkaw[,mail_order_users_per_cap := total_mail_order_patients_zip_year / tot_pop ]
rxnkaw[, mail_order_users_per_tot_pats := total_mail_order_patients_zip_month / total_pats_ym_z]

#one obs per zip year
rxnkaw[, zip_year_index := seq_len(.N),
       by = c("ZIP","pick_up_year")]

# zip_map_data = rxnkaw[STATE == "OR" & zip_year_index == 1,]


####set the non-opener ms to negative 1####
rxnkaw[, non_opener := ifelse(is.na(ms_zip_1), 1, 0)]
rxnkaw[, non_closer := ifelse(is.na(msc_zip_1), 1, 0)]
#non_opener is those that never had openings. opener_zip_dummy is for those that had openings that are usable (in the balance time frame)
rxnkaw[non_opener == 1, ms_zip_1 := -1]
rxnkaw[non_closer == 1, msc_zip_1 := -1]
gc()
#

#
#
# ###################################################################################################################################################################################
# ####Section 5: get adjacent zip code information ####
# ###################################################################################################################################################################################
# 
# ####find the ring of zip codes around each opening zip code####
#use distance between zip centroids from http://www.nber.org/data/zip-code-distance-database.html
zip_dist = as.data.table(read.csv("/home/gwg/current/pharmacy_deserts/data/sf12010zcta5distance25miles.csv"))
zip_dist_or = zip_dist[grepl("^97", as.character(zip1)) & nchar(as.character(zip1)) == 5,]
rm(zip_dist)
gc()

#get the ttoal number of pharms in eahc month for each zip2
#duplicate each obs 36 times
zip_dist_or[, index := seq_len(.N)]
zip_dist_or_36 <- zip_dist_or[rep(seq(1, nrow(zip_dist_or)), 36)]
setorder(zip_dist_or_36, index)

zip_dist_or_36[, zip1_zip2_index := seq_len(.N), by = .(index)]
zip_dist_or_36[, pick_up_yrmon := as.yearmon(((zip1_zip2_index - 1) / 12) + 2011)]

#merge on the zip2 ym number of pharms
zip_dist_or_36[, zip2 := as.character(zip2)]
setkey(zip_dist_or_36, zip2, pick_up_yrmon)
rxnkaw[, pharmzip_ym_index := seq_len(.N), by = .(pharm_zip, pick_up_yrmon)]
pharm_zip_counts <- rxnkaw[pharmzip_ym_index == 1, .(pharm_zip, pick_up_yrmon, num_pharms_ym_pz)]
setkey(pharm_zip_counts, pharm_zip, pick_up_yrmon)
zip_dist_or_36[pharm_zip_counts, num_pharms_zip2 := num_pharms_ym_pz]

zip_dist_or_36[, zip1 := as.character(zip1)]
setkey(zip_dist_or_36, zip1, pick_up_yrmon)
zip_dist_or_36[pharm_zip_counts, num_pharms_zip1 := num_pharms_ym_pz]

#sum the num of pharms for each zip1 within X miles
for (X in seq(1, 10)) {
  zip_dist_or_36[mi_to_zcta5 <= eval(X), (paste("num_pharms_zip_within_", X, sep="")) := num_pharms_zip1 + sum(num_pharms_zip2, na.rm=T), by = .(pick_up_yrmon, zip1)]
  zip_dist_or_36[, (paste("num_pharms_zip_within_", X, sep="")) := ifelse(is.na(get(paste("num_pharms_zip_within_", X, sep=""))), num_pharms_zip1, get(paste("num_pharms_zip_within_", X, sep="")))]
  zip_dist_or_36[, (paste("num_pharms_zip_within_", X, sep="")) := ifelse(is.na(get(paste("num_pharms_zip_within_", X, sep=""))), 0, get(paste("num_pharms_zip_within_", X, sep="")))]
}
zip_dist_or_36[, zip1_ym_index := seq_len(.N), by = .(zip1, pick_up_yrmon)]
zip_dist_or_36[, zip1 := as.integer(zip1)]
zip_dist_or_36 = zip_dist_or_36[zip1_ym_index == 1]
setkey(zip_dist_or_36, zip1, pick_up_yrmon)
setkey(rxnkaw, ZIP, pick_up_yrmon)

rxnkaw[zip_dist_or_36, c("num_pharms_zip_within_1", "num_pharms_zip_within_2",
                         "num_pharms_zip_within_3", "num_pharms_zip_within_4",
                         "num_pharms_zip_within_5", "num_pharms_zip_within_6",
                         "num_pharms_zip_within_7", "num_pharms_zip_within_8",
                         "num_pharms_zip_within_9", "num_pharms_zip_within_10") := 
         .(num_pharms_zip_within_1, num_pharms_zip_within_2,
           num_pharms_zip_within_3, num_pharms_zip_within_4,
           num_pharms_zip_within_5, num_pharms_zip_within_6,
           num_pharms_zip_within_7, num_pharms_zip_within_8,
           num_pharms_zip_within_9, num_pharms_zip_within_10)]

###plot # pharms####
pdf(file = "~/current/pharmacy_deserts/output/num_pharms.pdf")#####
for (X in seq(1,10)){
  plot = ggplot(data = rxnkaw[ym_z_index == 1 & !is.na(open_ym_zip_1) & open_ym_zip_1 >= "Jun 2011" & open_ym_zip_1 <= "Jul 2013"]) +
    geom_line(aes(x=ms_zip_1, y=get(paste("num_pharms_zip_within_",X,sep="")), color=as.factor(ZIP))) +
    ggtitle(eval(X))
  print(plot)
  plot = ggplot(data = rxnkaw[ym_z_index == 1 & !is.na(open_ym_zip_1) & open_ym_zip_1 >= "Jun 2011" & open_ym_zip_1 <= "Jul 2013"]) +
    geom_line(aes(x=ms_zip_1, y=I(log(get(paste("num_pharms_zip_within_",X,sep="")))), color=as.factor(ZIP))) +
    ggtitle(eval(X))
  print(plot)
}
dev.off()#####

# 
# ##########################################################################################################
# #####CHECKPOINT####
# # save(rxnkawz, file="~/current/pharmacy_deserts/data/rxnkawz_prior_to_stack.RData")

# rm(rxnkaw, rxnkawm)
# gc()
# load("~/current/pharmacy_deserts/data/rxnkawz_prior_to_stack.RData") #~3min
# 
# ###################################################################################################################################################################################
# ####Section 4: collapse to zip code level ####
# ###################################################################################################################################################################################
# # #### CHECKPOINT 3: just prior to collapse####
gc()

#remove all unneeded variables
names_to_rm = c("medflag","rxflag","yob","MSA","paydate","todate","rxcompound","puym","year","shop_at_bad_npi_flag",
                "shop_at_bad_npi_flag_max","deact_reason","pharm_address","days_btw_last_deact",
                "days_btw_enum_open","unknown_zip_flag","unknown_zip_flag_max","adjust_flag","duplicate_counter",
                "qtydisp_freq","qtydisp_index","qtydisp_pctile","pills_per_day","pills_per_day_index","rxdays_index",
                "rxdays_pctile","pills_per_day_pctile","second_fill_prior_to_opening_1"      ,
                "fifth_fill_prior_to_opening_1" ,           "mean_mpr_ym_rx_z_secondary_1"     ,        "mean_mpr_ym_rx_z_secondary_chronic_1",
                "second_fill_prior_to_opening_2"      ,     "fifth_fill_prior_to_opening_2"   ,         "mean_mpr_ym_rx_z_secondary_2"  ,
                "mean_mpr_ym_rx_z_secondary_chronic_2",     "second_fill_prior_to_opening_3"  ,         "fifth_fill_prior_to_opening_3" ,
                "mean_mpr_ym_rx_z_secondary_3"    ,         "mean_mpr_ym_rx_z_secondary_chronic_3" ,    "second_fill_prior_to_opening_4",
                "fifth_fill_prior_to_opening_4"   ,         "mean_mpr_ym_rx_z_secondary_4"        ,     "mean_mpr_ym_rx_z_secondary_chronic_4" ,
                "max_ms_zip_1"     ,                        "min_ms_zip_1"     ,
                "max_ms_zip_2"          ,                   "min_ms_zip_2"        ,                     "max_ms_zip_3"  ,
                "min_ms_zip_3"      ,                       "max_ms_zip_4"       ,                      "min_ms_zip_4",
                "mail_order_users_per_tot_pats_neg_1_1_nf", "mail_order_users_per_tot_pats_neg_1_2_nf","mail_order_users_per_tot_pats_neg_1_3_nf",
                "mail_order_users_per_tot_pats_neg_1_4_nf")
rxnkaw[, (names_to_rm) := NULL]
rm(j,i, colnames, mail_order_users_per_tot_pats_neg_1,
   mail_order_users_per_tot_pats_neg_1_nf,
   max, min, ms, names_to_rm, open, open_ym_zip, X)
gc()


#
# save(rxnkaw, file="~/current/pharmacy_deserts/data/rxnkaw_prior_to_distance.RData") #didn't use any extra CPU or RAM but took a long time
# # load("~/current/pharmacy_deserts/data/rxnkaw_prior_to_distance.RData") #138gb to hold
# # ####Since distance takes so long, I put it here so I cold remove the monster rxnkaw
# # #then use just a subset and merge it back on
####DISTANCE from person zip centroid to pharmacy coordinates####

pharm_coord_1 = as.data.table(read.csv("~/current/pharmacy_deserts/data/pharm_coord.csv"))

pharm_coord_1[, id := seq_len(.N)]

load("~/current/pharmacy_deserts/data/oregon_pharmacy_latlon.RData")

setkey(geo_data, ID)
setkey(pharm_coord_1, id)
pharm_coord_1[geo_data, pharm_lonlat := lon_lat]
pharm_coord_1[, pharm_lon := NULL]
pharm_coord_1[, pharm_lat := NULL]

pharm_coord_1[, pharm_lon := gsub(",.*", "", pharm_lonlat)]
pharm_coord_1[, pharm_lat := gsub(".*?,", "", pharm_lonlat)]

#need distance from zip code centroid to pharm add full
data(zipcode)
zipcode = as.data.table(zipcode)[state == "OR"]
zipcode[, ZIP := as.integer(zip)]
setkey(zipcode, ZIP)
setkey(rxnkaw, ZIP)
rxnkaw[zipcode, person_lat := latitude] #person zip code coordinates
rxnkaw[zipcode, person_lon := longitude] #person zip code coordinates

setkey(rxnkaw, pharm_add_full)
setkey(pharm_coord_1, possible_choices)

rxnkaw[pharm_coord_1, pharm_lon := pharm_lon] #pharmacy coordinates
rxnkaw[pharm_coord_1, pharm_lat := pharm_lat] #pharmacy coordinates
rxnkaw[, pharm_lat := as.numeric(pharm_lat)]
rxnkaw[, pharm_lon := as.numeric(pharm_lon)]



gc()
distance_data <- rxnkaw[,list(person_lon, person_lat, pharm_lon, pharm_lat,
                              clmid, pick_up_yrmon, ZIP, rx2, mail_order_dummy)]
# rm(rxnkaw)
gc()
#

person = cbind(distance_data$person_lon,distance_data$person_lat)
pharm = cbind(distance_data$pharm_lon, distance_data$pharm_lat)
distance_data[, distance := distHaversine(person, pharm)] #48g - 65g (only added 17..)
#set mail distance to 0
distance_data[mail_order_dummy == 1, distance := 0]

#####avg distance####
distance_data[, avg_distance_ym_z := mean(distance, na.rm=T), by = c("pick_up_yrmon","ZIP")]
setkey(distance_data, clmid)
save(distance_data, file= "~/current/pharmacy_deserts/data/rxnkaw_distance_data.RData")

gc()

rm(person, pharm, pharm_coord_1, geo_data, zipcode)
gc()

# # #10-15 min
# # load("~/current/pharmacy_deserts/data/rxnkaw_prior_to_distance.RData") #10-15 min
# # # load("~/current/pharmacy_deserts/data/rxnkaw_distance_data.RData")
setkey(rxnkaw, clmid)
rxnkaw[,c("avg_distance_ym_z","avg_distance_ym_rx_z","distance") := NULL]
rxnkaw[distance_data, avg_distance_ym_z := avg_distance_ym_z]
rxnkaw[distance_data, distance := distance]
rm(person, pharm, test, distance_data)
gc()
# # 
# # 
# # ####get the "maintenance" variable for each rx2:####
#the mean number of fills per patient over the whole data
rxnkaw[, fills_per_patient_rx2 := .N, by = c("personkey","rx2")]
#one obs per patient drug
rxnkaw[, person_drug_index := seq_len(.N), by = c("personkey","rx2")]
rxnkaw[, drug_index := seq_len(.N), by = c("rx2")]
rxnkaw[person_drug_index==1, mean_fills_per_patient_rx2_nf := mean(fills_per_patient_rx2,na.rm=T), by = c("rx2")]
rxnkaw[, mean_fills_per_patient_rx2 := na.locf(mean_fills_per_patient_rx2_nf), by = c("rx2")]

rx2_maintenance_data = rxnkaw[drug_index == 1,.(rx2,mean_fills_per_patient_rx2)]
save(rx2_maintenance_data, file = "~/current/pharmacy_deserts/data/rx2_maintenance_data.RData")
rm(rx2_maintenance_data)
gc()
# # 
# # ####names to keep to cut down on memory####
# # save(rxnkaw, file = "~/current/pharmacy_deserts/data/rxnkaw_just_before_names_cut.RData")
# # # 
# load("~/current/pharmacy_deserts/data/rxnkaw_just_before_names_cut.RData")
# names(rxnkaw)
# gc()
# 
# names_to_keep = c("age",
#                   "apac_payer",
#                   "avg_age_y_ins_23_c",
#                   "avg_age_y_ins_23_rx_c",
#                   "avg_age_y_ins_6_c",
#                   "avg_age_y_ins_6_rx_c",
#                   "avg_age_ym_ins_18_z",
#                   "avg_age_ym_ins_23_pharmtype_z",
#                   "avg_age_ym_ins_23_rx_z",
#                   "avg_age_ym_ins_23_z",
#                   "avg_age_ym_ins_53_z",
#                   "avg_age_ym_ins_6_rx_z",
#                   "avg_age_ym_ins_6_z",
#                   "avg_age_ym_ins_8_z",
#                   "avg_age_ym_mail_z",
#                   "avg_age_ym_pharmtype_z",
#                   "avg_age_ym_rx_z",
#                   "avg_age_ym_z",
#                   "avg_copay_y_ins_23_c",
#                   "avg_copay_y_ins_23_rx_c",
#                   "avg_copay_y_ins_6_c",
#                   "avg_copay_y_ins_6_rx_c",
#                   "avg_copay_ym_ins_18",
#                   "avg_copay_ym_ins_18_rx",
#                   "avg_copay_ym_ins_18_rx_pharmtype",
#                   "avg_copay_ym_ins_18_rx_z",
#                   "avg_copay_ym_ins_18_z",
#                   "avg_copay_ym_ins_23",
#                   "avg_copay_ym_ins_23_pharmtype_z",
#                   "avg_copay_ym_ins_23_rx",
#                   "avg_copay_ym_ins_23_rx_pharmtype",
#                   "avg_copay_ym_ins_23_rx_z",
#                   "avg_copay_ym_ins_23_z",
#                   "avg_copay_ym_ins_53",
#                   "avg_copay_ym_ins_53_rx",
#                   "avg_copay_ym_ins_53_rx_pharmtype",
#                   "avg_copay_ym_ins_53_rx_z",
#                   "avg_copay_ym_ins_53_z",
#                   "avg_copay_ym_ins_6_pharmtype_z",
#                   "avg_copay_ym_ins_6_rx_z",
#                   "avg_copay_ym_ins_6_z",
#                   "avg_copay_ym_ins_8",
#                   "avg_copay_ym_ins_8_rx",
#                   "avg_copay_ym_ins_8_rx_pharmtype",
#                   "avg_copay_ym_ins_8_rx_z",
#                   "avg_copay_ym_ins_8_z",
#                   "avg_copay_ym_mail_z",
#                   "avg_copay_ym_pharmtype_rx_ins_18_z",
#                   "avg_copay_ym_pharmtype_rx_ins_23_z",
#                   "avg_copay_ym_pharmtype_rx_ins_53_z",
#                   "avg_copay_ym_pharmtype_rx_ins_8_z",
#                   "avg_copay_ym_pharmtype_rx_z",
#                   "avg_copay_ym_pharmtype_z",
#                   "avg_copay_ym_rx",
#                   "avg_copay_ym_rx_z",
#                   "avg_copay_ym_z",
#                   "avg_distance_ym_rx_z",
#                   "avg_distance_ym_z",
#                   "close_ym_zip_1",
#                   "close_ym_zip_2",
#                   "closer_zip_dummy",
#                   "coins",
#                   "copay",
#                   "county",
#                   "distance",
#                   "entity",
#                   "entity_broad",
#                   "ethn",
#                   "gender",
#                   "grocery_1",
#                   "grocery_2",
#                   "grocery_3",
#                   "grocery_4 = NA",
#                   "insurance_type_18",
#                   "insurance_type_23",
#                   "insurance_type_53",
#                   "insurance_type_53",
#                   "insurance_type_6",
#                   "insurance_type_8",
#                   "last_year_mon",
#                   "mail_order_dummy",
#                   "mail_order_users_per_tot_pats_neg_1_1",
#                   "mail_order_users_per_tot_pats_neg_1_2",
#                   "mail_order_users_per_tot_pats_neg_1_3",
#                   "mail_order_users_per_tot_pats_neg_1_4",
#                   "mail_order_users_per_tot_pats_neg_1c_1",
#                   "mail_order_users_per_tot_pats_neg_1c_2",
#                   "mail_order_users_per_tot_pats_neg_1c_3",
#                   "mail_order_users_per_tot_pats_neg_1c_4",
#                   "mail_share_y_ins_23_c",
#                   "mail_share_y_ins_23_rx_c",
#                   "mail_share_y_ins_6_c",
#                   "mail_share_y_ins_6_rx_c",
#                   "mail_share_ym_ins_18_z",
#                   "mail_share_ym_ins_23_pharmtype_z",
#                   "mail_share_ym_ins_23_rx",
#                   "mail_share_ym_ins_23_rx_z",
#                   "mail_share_ym_ins_23_z",
#                   "mail_share_ym_ins_53_z",
#                   "mail_share_ym_ins_6_rx_z",
#                   "mail_share_ym_ins_6_z",
#                   "mail_share_ym_ins_8_z",
#                   "mail_share_ym_mail_z",
#                   "mail_share_ym_pharmtype_z",
#                   "mail_share_ym_rx",
#                   "mail_share_ym_rx_z",
#                   "mail_share_ym_z",
#                   "ms_wal",
#                   "ms_zip_1",
#                   "ms_zip_2",
#                   "ms_zip_3",
#                   "ms_zip_4",
#                   "msc_zip_1",
#                   "msc_zip_2",
#                   "ndc",
#                   "new_pharm",
#                   "non_closer",
#                   "non_opener",
#                   "npi",
#                   "num_walg_c_y",
#                   "num_walg_pz_ym",
#                   "num_walg_z_ym",
#                   "oop",
#                   "open_ym_zip_1",
#                   "open_ym_zip_2",
#                   "open_ym_zip_3",
#                   "open_ym_zip_4",
#                   "opener_zip_dummy",
#                   "opening_entity_broad_1",
#                   "opening_entity_broad_2",
#                   "opening_entity_broad_3",
#                   "opening_entity_broad_4",
#                   "opening_year_mon",
#                   "paid",
#                   "personkey",
#                   "pharm_add_full",
#                   "pharm_state",
#                   "pharm_zip",
#                   "pick_up_yrmon",
#                   "pick_up_yrmon",
#                   "pills_per_person_y_ins_23_c",
#                   "pills_per_person_y_ins_23_rx_c",
#                   "pills_per_person_y_ins_6_c",
#                   "pills_per_person_y_ins_6_rx_c",
#                   "pills_per_person_ym_ins_18_z",
#                   "pills_per_person_ym_ins_23_pharmtype_z",
#                   "pills_per_person_ym_ins_23_rx",
#                   "pills_per_person_ym_ins_23_rx_z",
#                   "pills_per_person_ym_ins_23_z",
#                   "pills_per_person_ym_ins_53_z",
#                   "pills_per_person_ym_ins_6_rx_z",
#                   "pills_per_person_ym_ins_6_z",
#                   "pills_per_person_ym_ins_8_z",
#                   "pills_per_person_ym_mail_z",
#                   "pills_per_person_ym_pharmtype_z",
#                   "pills_per_person_ym_rx",
#                   "pills_per_person_ym_rx_z",
#                   "pills_per_person_ym_z",
#                   "pills_per_person_ym_z_exist_1",
#                   "pills_per_person_ym_z_exist_2",
#                   "pills_per_person_ym_z_exist_3",
#                   "pills_per_person_ym_z_exist_4",
#                   "pills_per_person_ym_z_exist_wal",
#                   "qtydisp",
#                   "race",
#                   "rx2",
#                   "rx4",
#                   "rx6",
#                   "rxclass",
#                   "rxdays",
#                   "rxdays_broad",
#                   "share_medicaid_y_c",
#                   "share_medicaid_ym_z",
#                   "STATE",
#                   "tot_pharms_y_c",
#                   "tot_pharms_ym_z",
#                   "total_claims_y_ins_23_c",
#                   "total_claims_y_ins_23_rx_c",
#                   "total_claims_y_ins_6_c",
#                   "total_claims_y_ins_6_rx_c",
#                   "total_claims_ym_ins_18_z",
#                   "total_claims_ym_ins_23_pharmtype_z",
#                   "total_claims_ym_ins_23_rx",
#                   "total_claims_ym_ins_23_rx_z",
#                   "total_claims_ym_ins_23_z",
#                   "total_claims_ym_ins_53_z",
#                   "total_claims_ym_ins_6_rx_z",
#                   "total_claims_ym_ins_6_z",
#                   "total_claims_ym_ins_8_z",
#                   "total_claims_ym_mail_z",
#                   "total_claims_ym_pharmtype_z",
#                   "total_claims_ym_rx",
#                   "total_claims_ym_rx_z",
#                   "total_claims_ym_z",
#                   "total_claims_ym_z_exist_1",
#                   "total_claims_ym_z_exist_2",
#                   "total_claims_ym_z_exist_3",
#                   "total_claims_ym_z_exist_4",
#                   "total_claims_ym_z_exist_wal",
#                   "total_money_y_ins_23_c",
#                   "total_money_y_ins_23_rx_c",
#                   "total_money_y_ins_6_c",
#                   "total_money_y_ins_6_rx_c",
#                   "total_money_ym_ins_23_pharmtype_z",
#                   "total_money_ym_ins_23_rx_z",
#                   "total_money_ym_ins_23_z",
#                   "total_money_ym_ins_6_rx_z",
#                   "total_money_ym_ins_6_z",
#                   "total_new_pats_y_ins_23_c",
#                   "total_new_pats_y_ins_23_rx_c",
#                   "total_new_pats_y_ins_6_c",
#                   "total_new_pats_y_ins_6_rx_c",
#                   "total_new_pats_ym_ins_18_z",
#                   "total_new_pats_ym_ins_23_pharmtype_z",
#                   "total_new_pats_ym_ins_23_rx",
#                   "total_new_pats_ym_ins_23_rx_z",
#                   "total_new_pats_ym_ins_23_z",
#                   "total_new_pats_ym_ins_53_z",
#                   "total_new_pats_ym_ins_6_rx_z",
#                   "total_new_pats_ym_ins_6_z",
#                   "total_new_pats_ym_ins_8_z",
#                   "total_new_pats_ym_mail_z",
#                   "total_new_pats_ym_pharmtype_z",
#                   "total_new_pats_ym_rx_z",
#                   "total_new_pats_ym_z",
#                   "total_pats_y_c",
#                   "total_pats_y_ins_23_c",
#                   "total_pats_y_ins_23_rx_c",
#                   "total_pats_y_ins_6_c",
#                   "total_pats_y_ins_6_rx_c",
#                   "total_pats_ym_ins_18_z",
#                   "total_pats_ym_ins_23_pharmtype_z",
#                   "total_pats_ym_ins_23_rx",
#                   "total_pats_ym_ins_23_rx_z",
#                   "total_pats_ym_ins_23_z",
#                   "total_pats_ym_ins_53_z",
#                   "total_pats_ym_ins_6_rx_z",
#                   "total_pats_ym_ins_6_z",
#                   "total_pats_ym_ins_8_z",
#                   "total_pats_ym_mail_z",
#                   "total_pats_ym_pharmtype_z",
#                   "total_pats_ym_rx",
#                   "total_pats_ym_rx_z",
#                   "total_pats_ym_z",
#                   "total_pats_ym_z_exist_1",
#                   "total_pats_ym_z_exist_2",
#                   "total_pats_ym_z_exist_3",
#                   "total_pats_ym_z_exist_4",
#                   "total_pats_ym_z_exist_wal",
#                   "total_pills_dispensed_y_ins_23_c",
#                   "total_pills_dispensed_y_ins_23_rx_c",
#                   "total_pills_dispensed_y_ins_6_c",
#                   "total_pills_dispensed_y_ins_6_rx_c",
#                   "total_pills_dispensed_ym_ins_18_z",
#                   "total_pills_dispensed_ym_ins_23_pharmtype_z",
#                   "total_pills_dispensed_ym_ins_23_rx",
#                   "total_pills_dispensed_ym_ins_23_rx_z",
#                   "total_pills_dispensed_ym_ins_23_z",
#                   "total_pills_dispensed_ym_ins_53_z",
#                   "total_pills_dispensed_ym_ins_6_rx_z",
#                   "total_pills_dispensed_ym_ins_6_z",
#                   "total_pills_dispensed_ym_ins_8_z",
#                   "total_pills_dispensed_ym_mail_z",
#                   "total_pills_dispensed_ym_pharmtype_z",
#                   "total_pills_dispensed_ym_rx",
#                   "total_pills_dispensed_ym_rx_z",
#                   "total_pills_dispensed_ym_z",
#                   "total_pills_dispensed_ym_z_exist_1",
#                   "total_pills_dispensed_ym_z_exist_2",
#                   "total_pills_dispensed_ym_z_exist_3",
#                   "total_pills_dispensed_ym_z_exist_4",
#                   "total_pills_dispensed_ym_z_exist_wal",
#                   "total_unique_zips_ym_z",
#                   "usable_1",
#                   "usable_2",
#                   "usable_3",
#                   "usable_4",
#                   "usable_c_1",
#                   "usable_c_2",
#                   "walgreens",
#                   "walgreens_county_person",
#                   "walgreens_zip_person",
#                   "yrmon_factor",
#                   "yrmon_number",
#                   "yrqtr_factor",
#                   "ZIP",
#                   "zip_factor" )
# 
# names_to_drop = names(rxnkaw)[!names(rxnkaw) %in% names_to_keep]
# rxnkaw[,(names_to_drop) := NULL] 
# gc()
# 
# names(rxnkaw)
# 
# save(rxnkaw, file="~/current/pharmacy_deserts/data/rxnkaw_prior_to_collapse.RData")
# # 
# 

####COLLAPSE: 1 data set for each index####
Y = "index"
print(Y)
Y_ym = paste(Y, "ym", sep="_")
Y_ym_z = paste(Y, "ym", "z", sep="_")

Y_ym_c = paste(Y, "ym", "c", sep="_")

Y_ym_z_rx2 = paste(Y, "ym", "z", "rx2", sep="_")
Y_ym_c_rx2 = paste(Y, "ym", "c", "rx2", sep="_")

Y_ym_z_mail = paste(Y, "ym", "z", "mail", sep="_")

Y_ym_z_ins23 = paste(Y, "ym", "z", "ins23", sep="_")
Y_ym_z_ins53 = paste(Y, "ym", "z", "ins53", sep="_")
Y_ym_z_ins6 = paste(Y, "ym", "z", "ins6", sep="_")
Y_ym_z_ins8 = paste(Y, "ym", "z", "ins8", sep="_")

Y_ym_z_pharmtype = paste(Y, "ym", "z", "pharmtype", sep="_")

Y_ym_z_branded = paste(Y, "ym", "z", "branded", sep="_")

Y_ym_z_drugpricedecile = paste(Y, "ym", "z", "rxclass_paid_pre_opening_decile", sep="_")
Y_ym_z_drugpricedecile_closing = paste(Y, "ym", "z", "rxclass_paid_pre_closing_decile", sep="_")
Y_ym_z_drugpricedecile_walgreens = paste(Y, "ym", "z", "rxclass_paid_pre_walgreens_decile", sep="_")

Y_ym_z_weekend = paste(Y, "ym", "z", "weekend", sep="_")

# Y_ym_z_chronic = paste(Y, "ym", "z", "chronic", sep="_")
Y_ym_z_empchronic = paste(Y, "ym", "z", "empchronic", sep="_")

Y_ym_z_existing1 = paste(Y, "ym", "z", "existing1", sep="_")
Y_ym_z_existing2 = paste(Y, "ym", "z", "existing2", sep="_")
Y_ym_z_existing3 = paste(Y, "ym", "z", "existing3", sep="_")
Y_ym_z_existing4 = paste(Y, "ym", "z", "existing4", sep="_")

Y_ym_z_existing_c_1 = paste(Y, "ym", "z", "existing_c_1", sep="_")
Y_ym_z_existing_c_2 = paste(Y, "ym", "z", "existing_c_2", sep="_")

Y_ym_z_existing_w = paste(Y, "ym", "z", "existing_w", sep="_")

###########################################################################################################################
rxnkaw[, (Y_ym) := seq_len(.N), by = .(pick_up_yrmon)]
rxnkaw[, (Y_ym_z) := seq_len(.N), by = .(pick_up_yrmon, ZIP)]
rxnkaw[, (Y_ym_c) := seq_len(.N), by = .(pick_up_yrmon, county)]

rxnkaw[, (Y_ym_z_rx2) := seq_len(.N), by = .(pick_up_yrmon, ZIP, rx2)]
rxnkaw[, (Y_ym_c_rx2) := seq_len(.N), by = .(pick_up_yrmon, county, rx2)]

rxnkaw[, (Y_ym_z_mail) := seq_len(.N), by = .(pick_up_yrmon, ZIP, mail_order_dummy)]

rxnkaw[, (Y_ym_z_ins23) := seq_len(.N), by = .(pick_up_yrmon, ZIP, insurance_type_23)]
rxnkaw[, (Y_ym_z_ins53) := seq_len(.N), by = .(pick_up_yrmon, ZIP, insurance_type_53)]
rxnkaw[, (Y_ym_z_ins6) := seq_len(.N), by = .(pick_up_yrmon, ZIP, insurance_type_6)]
rxnkaw[, (Y_ym_z_ins8) := seq_len(.N), by = .(pick_up_yrmon, ZIP, insurance_type_8)]

rxnkaw[, (Y_ym_z_pharmtype) := seq_len(.N), by = .(pick_up_yrmon, ZIP, entity_broad)]
# rxnkaw[, (Y_ym_z_branded) := seq_len(.N), by = .(pick_up_yrmon, ZIP, branded)]
rxnkaw[, (Y_ym_z_drugpricedecile) := seq_len(.N), by = .(pick_up_yrmon, ZIP, rxclass_paid_pre_opening_decile)]
rxnkaw[, (Y_ym_z_drugpricedecile_closing) := seq_len(.N), by = .(pick_up_yrmon, ZIP, rxclass_paid_pre_closing_decile)]
rxnkaw[, (Y_ym_z_drugpricedecile_walgreens) := seq_len(.N), by = .(pick_up_yrmon, ZIP, rxclass_paid_pre_walgreens_decile)]
rxnkaw[, (Y_ym_z_weekend) := seq_len(.N), by = .(pick_up_yrmon, ZIP, weekend_dummy)]
# rxnkaw[, (Y_ym_z_chronic) := seq_len(.N), by = .(pick_up_yrmon, ZIP, chronic)]
rxnkaw[, (Y_ym_z_empchronic) := seq_len(.N), by = .(pick_up_yrmon, ZIP, emp_chronic_dummy)]
rxnkaw[, (Y_ym_z_existing1) := seq_len(.N), by = .(pick_up_yrmon, ZIP, existing1)]
rxnkaw[, (Y_ym_z_existing2) := seq_len(.N), by = .(pick_up_yrmon, ZIP, existing2)]
rxnkaw[, (Y_ym_z_existing3) := seq_len(.N), by = .(pick_up_yrmon, ZIP, existing3)]
rxnkaw[, (Y_ym_z_existing4) := seq_len(.N), by = .(pick_up_yrmon, ZIP, existing4)]

rxnkaw[, (Y_ym_z_existing_c_1) := seq_len(.N), by = .(pick_up_yrmon, ZIP, existing_c_1)]
rxnkaw[, (Y_ym_z_existing_c_2) := seq_len(.N), by = .(pick_up_yrmon, ZIP, existing_c_2)]

rxnkaw[, (Y_ym_z_existing_w) := seq_len(.N), by = .(pick_up_yrmon, ZIP, existing_w)]

gc()
names_to_include <- c(names(rxnkaw)[grepl("_ym$", names(rxnkaw))], "pick_up_yrmon")
rxnkaw_ym <- rxnkaw[get(Y_ym) == 1, ..names_to_include]

names_to_include <- c(names(rxnkaw)[grepl("_ym_z$", names(rxnkaw))], "pick_up_yrmon", "ZIP", "non_opener","opener_zip_dummy","closer_zip_dummy","non_closer", "open_ym_zip_1","open_ym_zip_2", "open_ym_zip_3","open_ym_zip_4",
                      "close_ym_zip_1","close_ym_zip_2", "ms_wal", "ms_zip_1", "ms_zip_2", "ms_zip_3", "ms_zip_4", "msc_zip_1", "msc_zip_2",
                      "share_medicaid_ym_z", "tot_pharms_ym_z", "total_estabs", "percent_rural", "urban_dummy", "total_health_estabs", "tot_pop", names(rxnkaw)[grepl("num_pharms_zip_within_", names(rxnkaw))])
rxnkaw_ym_z <- rxnkaw[get(Y_ym_z) == 1, ..names_to_include]
save(rxnkaw_ym_z, file="~/current/pharmacy_deserts/data/rxnkaw_ym_z.RData")

names_to_include <- c(names(rxnkaw)[grepl("_ym_c$", names(rxnkaw))], "pick_up_yrmon", "county")
rxnkaw_ym_c <- rxnkaw[get(Y_ym_c) == 1, ..names_to_include]
save(rxnkaw_ym_c, file="~/current/pharmacy_deserts/data/rxnkaw_ym_c.RData")

names_to_include <- c(names(rxnkaw)[grepl("_ym_z_rx2$", names(rxnkaw))], "pick_up_yrmon", "ZIP", "rx2", "non_opener","opener_zip_dummy","closer_zip_dummy","non_closer", "open_ym_zip_1","open_ym_zip_2", "open_ym_zip_3","open_ym_zip_4",
                      "close_ym_zip_1","close_ym_zip_2", "ms_wal", "ms_zip_1", "ms_zip_2", "ms_zip_3", "ms_zip_4", "msc_zip_1", "msc_zip_2",
                      "share_medicaid_ym_z", "tot_pharms_ym_z", "total_estabs", "percent_rural", "urban_dummy", "total_health_estabs", "tot_pop", names(rxnkaw)[grepl("num_pharms_zip_within_", names(rxnkaw))])
rxnkaw_ym_z_rx2 <- rxnkaw[get(Y_ym_z_rx2) == 1, ..names_to_include]
save(rxnkaw_ym_z_rx2, file="~/current/pharmacy_deserts/data/rxnkaw_ym_z_rx2.RData")

names_to_include <- c(names(rxnkaw)[grepl("_ym_c_rx2$", names(rxnkaw))], "pick_up_yrmon", "county", "rx2")
rxnkaw_ym_c_rx2 <- rxnkaw[get(Y_ym_c_rx2) == 1, ..names_to_include]
save(rxnkaw_ym_c_rx2, file="~/current/pharmacy_deserts/data/rxnkaw_ym_c_rx2.RData")

names_to_include <- c(names(rxnkaw)[grepl("_ym_z_mail$", names(rxnkaw))], "pick_up_yrmon", "ZIP", "mail_order_dummy", "non_opener","opener_zip_dummy","closer_zip_dummy","non_closer", "open_ym_zip_1","open_ym_zip_2", "open_ym_zip_3","open_ym_zip_4",
                      "close_ym_zip_1","close_ym_zip_2", "ms_wal", "ms_zip_1", "ms_zip_2", "ms_zip_3", "ms_zip_4", "msc_zip_1", "msc_zip_2",
                      "share_medicaid_ym_z", "tot_pharms_ym_z", "total_estabs", "percent_rural", "urban_dummy", "total_health_estabs", "tot_pop", names(rxnkaw)[grepl("num_pharms_zip_within_", names(rxnkaw))])
rxnkaw_ym_z_mail <- rxnkaw[get(Y_ym_z_mail) == 1, ..names_to_include]
save(rxnkaw_ym_z_mail, file="~/current/pharmacy_deserts/data/rxnkaw_ym_z_mail.RData")

names_to_include <- c(names(rxnkaw)[grepl("_ym_z_ins23$", names(rxnkaw))], "pick_up_yrmon", "ZIP", "insurance_type_23", "non_opener","opener_zip_dummy","closer_zip_dummy","non_closer", "open_ym_zip_1","open_ym_zip_2", "open_ym_zip_3","open_ym_zip_4",
                      "close_ym_zip_1","close_ym_zip_2", "ms_wal", "ms_zip_1", "ms_zip_2", "ms_zip_3", "ms_zip_4", "msc_zip_1", "msc_zip_2",
                      "share_medicaid_ym_z", "tot_pharms_ym_z", "total_estabs", "percent_rural", "urban_dummy", "total_health_estabs", "tot_pop", names(rxnkaw)[grepl("num_pharms_zip_within_", names(rxnkaw))])
rxnkaw_ym_z_ins23 <- rxnkaw[get(Y_ym_z_ins23) == 1, ..names_to_include]
save(rxnkaw_ym_z_ins23, file="~/current/pharmacy_deserts/data/rxnkaw_ym_z_ins23.RData")

names_to_include <- c(names(rxnkaw)[grepl("_ym_z_ins53$", names(rxnkaw))], "pick_up_yrmon", "ZIP", "insurance_type_53", "non_opener","opener_zip_dummy","closer_zip_dummy","non_closer", "open_ym_zip_1","open_ym_zip_2", "open_ym_zip_3","open_ym_zip_4",
                      "close_ym_zip_1","close_ym_zip_2", "ms_wal", "ms_zip_1", "ms_zip_2", "ms_zip_3", "ms_zip_4", "msc_zip_1", "msc_zip_2",
                      "share_medicaid_ym_z", "tot_pharms_ym_z", "total_estabs", "percent_rural", "urban_dummy", "total_health_estabs", "tot_pop", names(rxnkaw)[grepl("num_pharms_zip_within_", names(rxnkaw))])
rxnkaw_ym_z_ins53 <- rxnkaw[get(Y_ym_z_ins53) == 1, ..names_to_include]
save(rxnkaw_ym_z_ins53, file="~/current/pharmacy_deserts/data/rxnkaw_ym_z_ins53.RData")

names_to_include <- c(names(rxnkaw)[grepl("_ym_z_ins6$", names(rxnkaw))], "pick_up_yrmon", "ZIP", "insurance_type_6", "non_opener","opener_zip_dummy","closer_zip_dummy","non_closer", "open_ym_zip_1","open_ym_zip_2", "open_ym_zip_3","open_ym_zip_4",
                      "close_ym_zip_1","close_ym_zip_2", "ms_wal", "ms_zip_1", "ms_zip_2", "ms_zip_3", "ms_zip_4", "msc_zip_1", "msc_zip_2",
                      "share_medicaid_ym_z", "tot_pharms_ym_z", "total_estabs", "percent_rural", "urban_dummy", "total_health_estabs", "tot_pop", names(rxnkaw)[grepl("num_pharms_zip_within_", names(rxnkaw))])
rxnkaw_ym_z_ins6 <- rxnkaw[get(Y_ym_z_ins6) == 1, ..names_to_include]
save(rxnkaw_ym_z_ins6, file="~/current/pharmacy_deserts/data/rxnkaw_ym_z_ins6.RData")

names_to_include <- c(names(rxnkaw)[grepl("_ym_z_ins8$", names(rxnkaw))], "pick_up_yrmon", "ZIP", "insurance_type_8", "non_opener","opener_zip_dummy","closer_zip_dummy","non_closer", "open_ym_zip_1","open_ym_zip_2", "open_ym_zip_3","open_ym_zip_4",
                      "close_ym_zip_1","close_ym_zip_2", "ms_wal", "ms_zip_1", "ms_zip_2", "ms_zip_3", "ms_zip_4", "msc_zip_1", "msc_zip_2",
                      "share_medicaid_ym_z", "tot_pharms_ym_z", "total_estabs", "percent_rural", "urban_dummy", "total_health_estabs", "tot_pop", names(rxnkaw)[grepl("num_pharms_zip_within_", names(rxnkaw))])
rxnkaw_ym_z_ins8 <- rxnkaw[get(Y_ym_z_ins8) == 1, ..names_to_include]
save(rxnkaw_ym_z_ins8, file="~/current/pharmacy_deserts/data/rxnkaw_ym_z_ins8.RData")

names_to_include <- c(names(rxnkaw)[grepl("_ym_z_pharmtyp$", names(rxnkaw))], "pick_up_yrmon", "ZIP", "entity_broad", "non_opener","opener_zip_dummy","closer_zip_dummy","non_closer", "open_ym_zip_1","open_ym_zip_2", "open_ym_zip_3","open_ym_zip_4",
                      "close_ym_zip_1","close_ym_zip_2", "ms_wal", "ms_zip_1", "ms_zip_2", "ms_zip_3", "ms_zip_4", "msc_zip_1", "msc_zip_2",
                      "share_medicaid_ym_z", "tot_pharms_ym_z", "total_estabs", "percent_rural", "urban_dummy", "total_health_estabs", "tot_pop", names(rxnkaw)[grepl("num_pharms_zip_within_", names(rxnkaw))])
rxnkaw_ym_z_pharmtyp <- rxnkaw[get(Y_ym_z_pharmtyp) == 1, ..names_to_include]
save(rxnkaw_ym_z_pharmtyp, file="~/current/pharmacy_deserts/data/rxnkaw_ym_z_pharmtyp.RData")

names_to_include <- c(names(rxnkaw)[grepl("_ym_z_drugpricedecile$", names(rxnkaw))], "pick_up_yrmon", "ZIP", "rxclass_paid_pre_opening_decile", "non_opener","opener_zip_dummy","closer_zip_dummy","non_closer", "open_ym_zip_1","open_ym_zip_2", "open_ym_zip_3","open_ym_zip_4",
                      "close_ym_zip_1","close_ym_zip_2", "ms_wal", "ms_zip_1", "ms_zip_2", "ms_zip_3", "ms_zip_4", "msc_zip_1", "msc_zip_2",
                      "share_medicaid_ym_z", "tot_pharms_ym_z", "total_estabs", "percent_rural", "urban_dummy", "total_health_estabs", "tot_pop", names(rxnkaw)[grepl("num_pharms_zip_within_", names(rxnkaw))])
rxnkaw_ym_z_drugpricedecile <- rxnkaw[get(Y_ym_z_drugpricedecile) == 1, ..names_to_include]
save(rxnkaw_ym_z_drugpricedecile, file="~/current/pharmacy_deserts/data/rxnkaw_ym_z_drugpricedecile.RData")

names_to_include <- c(names(rxnkaw)[grepl("_ym_z_drugpricedecile_closings$", names(rxnkaw))], "pick_up_yrmon", "ZIP", "rxclass_paid_pre_closing_decile", "non_opener","opener_zip_dummy","closer_zip_dummy","non_closer", "open_ym_zip_1","open_ym_zip_2", "open_ym_zip_3","open_ym_zip_4",
                      "close_ym_zip_1","close_ym_zip_2", "ms_wal", "ms_zip_1", "ms_zip_2", "ms_zip_3", "ms_zip_4", "msc_zip_1", "msc_zip_2",
                      "share_medicaid_ym_z", "tot_pharms_ym_z", "total_estabs", "percent_rural", "urban_dummy", "total_health_estabs", "tot_pop", names(rxnkaw)[grepl("num_pharms_zip_within_", names(rxnkaw))])
rxnkaw_ym_z_drugpricedecile_closings <- rxnkaw[get(Y_ym_z_drugpricedecile_closings) == 1, ..names_to_include]
save(rxnkaw_ym_z_drugpricedecile_closings, file="~/current/pharmacy_deserts/data/rxnkaw_ym_z_drugpricedecile_closings.RData")

names_to_include <- c(names(rxnkaw)[grepl("_ym_z_drugpricedecile_walgreens$", names(rxnkaw))], "pick_up_yrmon", "ZIP", "rxclass_paid_pre_walgreens_decile", "non_opener","opener_zip_dummy","closer_zip_dummy","non_closer", "open_ym_zip_1","open_ym_zip_2", "open_ym_zip_3","open_ym_zip_4",
                      "close_ym_zip_1","close_ym_zip_2", "ms_wal", "ms_zip_1", "ms_zip_2", "ms_zip_3", "ms_zip_4", "msc_zip_1", "msc_zip_2",
                      "share_medicaid_ym_z", "tot_pharms_ym_z", "total_estabs", "percent_rural", "urban_dummy", "total_health_estabs", "tot_pop", names(rxnkaw)[grepl("num_pharms_zip_within_", names(rxnkaw))])
rxnkaw_ym_z_drugpricedecile_walgreens <- rxnkaw[get(Y_ym_z_drugpricedecile_walgreens) == 1, ..names_to_include]
save(rxnkaw_ym_z_drugpricedecile_walgreens, file="~/current/pharmacy_deserts/data/rxnkaw_ym_z_drugpricedecile_walgreens.RData")

names_to_include <- c(names(rxnkaw)[grepl("_ym_z_drugpricedecile_weekend$", names(rxnkaw))], "pick_up_yrmon", "ZIP", "weekend_dummy", "non_opener","opener_zip_dummy","closer_zip_dummy","non_closer", "open_ym_zip_1","open_ym_zip_2", "open_ym_zip_3","open_ym_zip_4",
                      "close_ym_zip_1","close_ym_zip_2", "ms_wal", "ms_zip_1", "ms_zip_2", "ms_zip_3", "ms_zip_4", "msc_zip_1", "msc_zip_2",
                      "share_medicaid_ym_z", "tot_pharms_ym_z", "total_estabs", "percent_rural", "urban_dummy", "total_health_estabs", "tot_pop", names(rxnkaw)[grepl("num_pharms_zip_within_", names(rxnkaw))])
rxnkaw_ym_z_drugpricedecile_weekend <- rxnkaw[get(Y_ym_z_drugpricedecile_weekend) == 1, ..names_to_include]
save(rxnkaw_ym_z_drugpricedecile_weekend, file="~/current/pharmacy_deserts/data/rxnkaw_ym_z_drugpricedecile_weekend.RData")

names_to_include <- c(names(rxnkaw)[grepl("_ym_z_drugpricedecile_empchronic$", names(rxnkaw))], "pick_up_yrmon", "ZIP", "emp_chronic_dummy", "non_opener","opener_zip_dummy","closer_zip_dummy","non_closer", "open_ym_zip_1","open_ym_zip_2", "open_ym_zip_3","open_ym_zip_4",
                      "close_ym_zip_1","close_ym_zip_2", "ms_wal", "ms_zip_1", "ms_zip_2", "ms_zip_3", "ms_zip_4", "msc_zip_1", "msc_zip_2",
                      "share_medicaid_ym_z", "tot_pharms_ym_z", "total_estabs", "percent_rural", "urban_dummy", "total_health_estabs", "tot_pop", names(rxnkaw)[grepl("num_pharms_zip_within_", names(rxnkaw))])
rxnkaw_ym_z_drugpricedecile_empchronic <- rxnkaw[get(Y_ym_z_drugpricedecile_empchronic) == 1, ..names_to_include]
save(rxnkaw_ym_z_drugpricedecile_empchronic, file="~/current/pharmacy_deserts/data/rxnkaw_ym_z_drugpricedecile_empchronic.RData")

names_to_include <- c(names(rxnkaw)[grepl("_ym_z_drugpricedecile_existing1$", names(rxnkaw))], "pick_up_yrmon", "ZIP", "existing1", "non_opener","opener_zip_dummy","closer_zip_dummy","non_closer", "open_ym_zip_1","open_ym_zip_2", "open_ym_zip_3","open_ym_zip_4",
                      "close_ym_zip_1","close_ym_zip_2", "ms_wal", "ms_zip_1", "ms_zip_2", "ms_zip_3", "ms_zip_4", "msc_zip_1", "msc_zip_2",
                      "share_medicaid_ym_z", "tot_pharms_ym_z", "total_estabs", "percent_rural", "urban_dummy", "total_health_estabs", "tot_pop", names(rxnkaw)[grepl("num_pharms_zip_within_", names(rxnkaw))])
rxnkaw_ym_z_drugpricedecile_existing1 <- rxnkaw[get(Y_ym_z_drugpricedecile_existing1) == 1, ..names_to_include]
save(rxnkaw_ym_z_drugpricedecile_existing1, file="~/current/pharmacy_deserts/data/rxnkaw_ym_z_drugpricedecile_existing1.RData")

names_to_include <- c(names(rxnkaw)[grepl("_ym_z_drugpricedecile_existing2$", names(rxnkaw))], "pick_up_yrmon", "ZIP", "existing2", "non_opener","opener_zip_dummy","closer_zip_dummy","non_closer", "open_ym_zip_1","open_ym_zip_2", "open_ym_zip_3","open_ym_zip_4",
                      "close_ym_zip_1","close_ym_zip_2", "ms_wal", "ms_zip_1", "ms_zip_2", "ms_zip_3", "ms_zip_4", "msc_zip_1", "msc_zip_2",
                      "share_medicaid_ym_z", "tot_pharms_ym_z", "total_estabs", "percent_rural", "urban_dummy", "total_health_estabs", "tot_pop", names(rxnkaw)[grepl("num_pharms_zip_within_", names(rxnkaw))])
rxnkaw_ym_z_drugpricedecile_existing2 <- rxnkaw[get(Y_ym_z_drugpricedecile_existing2) == 1, ..names_to_include]
save(rxnkaw_ym_z_drugpricedecile_existing2, file="~/current/pharmacy_deserts/data/rxnkaw_ym_z_drugpricedecile_existing2.RData")

names_to_include <- c(names(rxnkaw)[grepl("_ym_z_drugpricedecile_existing3$", names(rxnkaw))], "pick_up_yrmon", "ZIP", "existing3", "non_opener","opener_zip_dummy","closer_zip_dummy","non_closer", "open_ym_zip_1","open_ym_zip_2", "open_ym_zip_3","open_ym_zip_4",
                      "close_ym_zip_1","close_ym_zip_2", "ms_wal", "ms_zip_1", "ms_zip_2", "ms_zip_3", "ms_zip_4", "msc_zip_1", "msc_zip_2",
                      "share_medicaid_ym_z", "tot_pharms_ym_z", "total_estabs", "percent_rural", "urban_dummy", "total_health_estabs", "tot_pop", names(rxnkaw)[grepl("num_pharms_zip_within_", names(rxnkaw))])
rxnkaw_ym_z_drugpricedecile_existing3 <- rxnkaw[get(Y_ym_z_drugpricedecile_existing3) == 1, ..names_to_include]
save(rxnkaw_ym_z_drugpricedecile_existing3, file="~/current/pharmacy_deserts/data/rxnkaw_ym_z_drugpricedecile_existing3.RData")

names_to_include <- c(names(rxnkaw)[grepl("_ym_z_drugpricedecile_existing4$", names(rxnkaw))], "pick_up_yrmon", "ZIP", "existing4", "non_opener","opener_zip_dummy","closer_zip_dummy","non_closer", "open_ym_zip_1","open_ym_zip_2", "open_ym_zip_3","open_ym_zip_4",
                      "close_ym_zip_1","close_ym_zip_2", "ms_wal", "ms_zip_1", "ms_zip_2", "ms_zip_3", "ms_zip_4", "msc_zip_1", "msc_zip_2",
                      "share_medicaid_ym_z", "tot_pharms_ym_z", "total_estabs", "percent_rural", "urban_dummy", "total_health_estabs", "tot_pop", names(rxnkaw)[grepl("num_pharms_zip_within_", names(rxnkaw))])
rxnkaw_ym_z_drugpricedecile_existing4 <- rxnkaw[get(Y_ym_z_drugpricedecile_existing4) == 1, ..names_to_include]
save(rxnkaw_ym_z_drugpricedecile_existing4, file="~/current/pharmacy_deserts/data/rxnkaw_ym_z_drugpricedecile_existing4.RData")

names_to_include <- c(names(rxnkaw)[grepl("_ym_z_drugpricedecile_existing_c_1$", names(rxnkaw))], "pick_up_yrmon", "ZIP", "existing_c_1", "non_opener","opener_zip_dummy","closer_zip_dummy","non_closer", "open_ym_zip_1","open_ym_zip_2", "open_ym_zip_3","open_ym_zip_4",
                      "close_ym_zip_1","close_ym_zip_2", "ms_wal", "ms_zip_1", "ms_zip_2", "ms_zip_3", "ms_zip_4", "msc_zip_1", "msc_zip_2",
                      "share_medicaid_ym_z", "tot_pharms_ym_z", "total_estabs", "percent_rural", "urban_dummy", "total_health_estabs", "tot_pop", names(rxnkaw)[grepl("num_pharms_zip_within_", names(rxnkaw))])
rxnkaw_ym_z_drugpricedecile_existing_c_1 <- rxnkaw[get(Y_ym_z_drugpricedecile_existing_c_1) == 1, ..names_to_include]
save(rxnkaw_ym_z_drugpricedecile_existing_c_1, file="~/current/pharmacy_deserts/data/rxnkaw_ym_z_drugpricedecile_existing_c_1.RData")

names_to_include <- c(names(rxnkaw)[grepl("_ym_z_drugpricedecile_existing_c_2$", names(rxnkaw))], "pick_up_yrmon", "ZIP", "existing_c_2", "non_opener","opener_zip_dummy","closer_zip_dummy","non_closer", "open_ym_zip_1","open_ym_zip_2", "open_ym_zip_3","open_ym_zip_4",
                      "close_ym_zip_1","close_ym_zip_2", "ms_wal", "ms_zip_1", "ms_zip_2", "ms_zip_3", "ms_zip_4", "msc_zip_1", "msc_zip_2",
                      "share_medicaid_ym_z", "tot_pharms_ym_z", "total_estabs", "percent_rural", "urban_dummy", "total_health_estabs", "tot_pop", names(rxnkaw)[grepl("num_pharms_zip_within_", names(rxnkaw))])
rxnkaw_ym_z_drugpricedecile_existing_c_2 <- rxnkaw[get(Y_ym_z_drugpricedecile_existing_c_2) == 1, ..names_to_include]
save(rxnkaw_ym_z_drugpricedecile_existing_c_2, file="~/current/pharmacy_deserts/data/rxnkaw_ym_z_drugpricedecile_existing_c_2.RData")

names_to_include <- c(names(rxnkaw)[grepl("_ym_z_drugpricedecile_existing_w$", names(rxnkaw))], "pick_up_yrmon", "ZIP", "existing_w", "non_opener","opener_zip_dummy","closer_zip_dummy","non_closer", "open_ym_zip_1","open_ym_zip_2", "open_ym_zip_3","open_ym_zip_4",
                      "close_ym_zip_1","close_ym_zip_2", "ms_wal", "ms_zip_1", "ms_zip_2", "ms_zip_3", "ms_zip_4", "msc_zip_1", "msc_zip_2",
                      "share_medicaid_ym_z", "tot_pharms_ym_z", "total_estabs", "percent_rural", "urban_dummy", "total_health_estabs", "tot_pop", names(rxnkaw)[grepl("num_pharms_zip_within_", names(rxnkaw))])
rxnkaw_ym_z_drugpricedecile_existing_w <- rxnkaw[get(Y_ym_z_drugpricedecile_existing_w) == 1, ..names_to_include]
save(rxnkaw_ym_z_drugpricedecile_existing_w, file="~/current/pharmacy_deserts/data/rxnkaw_ym_z_drugpricedecile_existing_w.RData")


gc()


# ####months since dummies####
# rxnkawz[, ms_neg6 := ifelse(ms_zip_1 == -6 | ms_zip_2 == -6 | ms_zip_3 == -6 | ms_zip_4 == -6,1,0)]
# rxnkawz[is.na(ms_neg6), ms_neg6 := 0]
# rxnkawz[, ms_neg5 := ifelse(ms_zip_1 == -5 | ms_zip_2 == -5 | ms_zip_3 == -5 | ms_zip_4 == -5,1,0)]
# rxnkawz[is.na(ms_neg5), ms_neg5 := 0]
# rxnkawz[, ms_neg4 := ifelse(ms_zip_1 == -4 | ms_zip_2 == -4 | ms_zip_3 == -4 | ms_zip_4 == -4,1,0)]
# rxnkawz[is.na(ms_neg4), ms_neg4 := 0]
# rxnkawz[, ms_neg3 := ifelse(ms_zip_1 == -3 | ms_zip_2 == -3 | ms_zip_3 == -3 | ms_zip_4 == -3,1,0)]
# rxnkawz[is.na(ms_neg3), ms_neg3 := 0]
# rxnkawz[, ms_neg2 := ifelse(ms_zip_1 == -2 | ms_zip_2 == -2 | ms_zip_3 == -2 | ms_zip_4 == -2,1,0)]
# rxnkawz[is.na(ms_neg2), ms_neg2 := 0]
# rxnkawz[, ms_neg1 := ifelse(ms_zip_1 == -1 | ms_zip_2 == -1 | ms_zip_3 == -1 | ms_zip_4 == -1,1,0)]
# rxnkawz[is.na(ms_neg1), ms_neg1 := 0]
# rxnkawz[, ms_0 := ifelse(ms_zip_1 == 0 | ms_zip_2 == 0 | ms_zip_3 == 0 | ms_zip_4 == 0,1,0)]
# rxnkawz[is.na(ms_0), ms_0 := 0]
# rxnkawz[, ms_1 := ifelse(ms_zip_1 == 1 | ms_zip_2 == 1 | ms_zip_3 == 1 | ms_zip_4 == 1,1,0)]
# rxnkawz[is.na(ms_1), ms_1 := 0]
# rxnkawz[, ms_2 := ifelse(ms_zip_1 == 2 | ms_zip_2 == 2 | ms_zip_3 == 2 | ms_zip_4 == 2,1,0)]
# rxnkawz[is.na(ms_2), ms_2 := 0]
# rxnkawz[, ms_3 := ifelse(ms_zip_1 == 3 | ms_zip_2 == 3 | ms_zip_3 == 3 | ms_zip_4 == 3,1,0)]
# rxnkawz[is.na(ms_3), ms_3 := 0]
# rxnkawz[, ms_4 := ifelse(ms_zip_1 == 4 | ms_zip_2 == 4 | ms_zip_3 == 4 | ms_zip_4 == 4,1,0)]
# rxnkawz[is.na(ms_4), ms_4 := 0]
# rxnkawz[, ms_5 := ifelse(ms_zip_1 == 5 | ms_zip_2 == 5 | ms_zip_3 == 5 | ms_zip_4 == 5,1,0)]
# rxnkawz[is.na(ms_5), ms_5 := 0]
# rxnkawz[, ms_6 := ifelse(ms_zip_1 == 6 | ms_zip_2 == 6 | ms_zip_3 == 6 | ms_zip_4 == 6,1,0)]
# rxnkawz[is.na(ms_6), ms_6 := 0]
# 
# #closings
# rxnkawz[, msc_neg6 := ifelse(msc_zip_1 == -6 | msc_zip_2 == -6 ,1,0)]
# rxnkawz[is.na(msc_neg6), msc_neg6 := 0]
# rxnkawz[, msc_neg5 := ifelse(msc_zip_1 == -5 | msc_zip_2 == -5 ,1,0)]
# rxnkawz[is.na(msc_neg5), msc_neg5 := 0]
# rxnkawz[, msc_neg4 := ifelse(msc_zip_1 == -4 | msc_zip_2 == -4 ,1,0)]
# rxnkawz[is.na(msc_neg4), msc_neg4 := 0]
# rxnkawz[, msc_neg3 := ifelse(msc_zip_1 == -3 | msc_zip_2 == -3 ,1,0)]
# rxnkawz[is.na(msc_neg3), msc_neg3 := 0]
# rxnkawz[, msc_neg2 := ifelse(msc_zip_1 == -2 | msc_zip_2 == -2 ,1,0)]
# rxnkawz[is.na(msc_neg2), msc_neg2 := 0]
# rxnkawz[, msc_neg1 := ifelse(msc_zip_1 == -1 | msc_zip_2 == -1 ,1,0)]
# rxnkawz[is.na(msc_neg1), msc_neg1 := 0]
# rxnkawz[, msc_0 := ifelse(msc_zip_1 == 0 | msc_zip_2 == 0 ,1,0)]
# rxnkawz[is.na(msc_0), msc_0 := 0]
# rxnkawz[, msc_1 := ifelse(msc_zip_1 == 1 | msc_zip_2 == 1 ,1,0)]
# rxnkawz[is.na(msc_1), msc_1 := 0]
# rxnkawz[, msc_2 := ifelse(msc_zip_1 == 2 | msc_zip_2 == 2 ,1,0)]
# rxnkawz[is.na(msc_2), msc_2 := 0]
# rxnkawz[, msc_3 := ifelse(msc_zip_1 == 3 | msc_zip_2 == 3 ,1,0)]
# rxnkawz[is.na(msc_3), msc_3 := 0]
# rxnkawz[, msc_4 := ifelse(msc_zip_1 == 4 | msc_zip_2 == 4 ,1,0)]
# rxnkawz[is.na(msc_4), msc_4 := 0]
# rxnkawz[, msc_5 := ifelse(msc_zip_1 == 5 | msc_zip_2 == 5 ,1,0)]
# rxnkawz[is.na(msc_5), msc_5 := 0]
# rxnkawz[, msc_6 := ifelse(msc_zip_1 == 6 | msc_zip_2 == 6,1,0)]
# rxnkawz[is.na(msc_6), msc_6 := 0]
# 
# 
# 
# gc()
# ####end not run on MBA####
# mem_used()
# 

# ##########################################################################################################
# ####SECTION 6: STACK####
# ##########################################################################################################
# B = 6
# ####stack the data ####
# for (X in c(seq(1,4))) {
#   #   X = 1
#   dt = paste("zip_data_", X, sep="")
#   usable = paste("usable_",X,sep="")
#   ms_zip = paste("ms_zip_",X,sep="")
#   mail_order_users_per_tot_pats_neg_1 = paste("mail_order_users_per_tot_pats_neg_1_",X,sep="")
#   grocery = paste("grocery_",X,sep="")
#   pharms_neg_1 = paste("tot_pharms_ym_z_neg_1_",X,sep="")
#   share_medicaid_neg_1 = paste("share_medicaid_ym_z_neg_1_",X,sep="")
#   opening_entity = paste("opening_entity_broad_",X,sep="")
#   total_pills_exist = paste("total_pills_dispensed_ym_z_exist_",X,sep="")
#   total_pats_exist = paste("total_pats_ym_z_exist_",X,sep="")
#   total_claims_exist = paste("total_claims_ym_z_exist_",X,sep="")
#   pills_per_person_exist = paste("pills_per_person_ym_z_exist_",X,sep="")
#   total_pills_exist_wal = paste("total_pills_dispensed_ym_z_exist_","wal",sep="")
#   total_pats_exist_wal = paste("total_pats_ym_z_exist_","wal",sep="")
#   total_claims_exist_wal = paste("total_claims_ym_z_exist_","wal",sep="")
#   pills_per_person_exist_wal = paste("pills_per_person_ym_z_exist_","wal",sep="")
#   
#   assign(eval(dt),rxnkawz[
#     !is.na(get(ms_zip)) #this keeps all on the first round, but only those with openings on the second-fourth
#     #     (get(usable) == 1 | non_opener == 1) 
#     #     & get(ms_zip) <= B+6 & get(ms_zip) >= -B-6
#     # & opening_zip == 1
#     & grepl("^97",ZIP) #patients live in Oregon
#     ,
#     
#     list(ZIP,
#          pick_up_yrmon, 
#          zip_ym_index,
#          yrqtr_factor, yrmon_factor, yrmon_number,
#          zip_index,   zip_ym_mail_index,
#          zip_ym_ins_53_index,
#          zip_ym_ins_23_index,
#          zip_ym_ins_23_rx_index,
#          zip_ym_ins_23_pharmtype_index,
#          zip_ym_ins_18_index,zip_ym_ins_8_index,
#          zip_ym_ins_8_rx_index, zip_ym_ins_8_index,
#          zip_ym_pharmtype_index,#zip_ym_30v90_index,
#          total_unique_zips_ym_z,
#          zip_ym_rx_index,
#          ym_ins_53_rx_pharmtype_index,ym_ins_53_rx_index,ym_ins_53_index,
#          ym_ins_23_rx_pharmtype_index,ym_ins_23_rx_index,ym_ins_23_index,
#          ym_ins_18_rx_pharmtype_index,ym_ins_18_rx_index,ym_ins_18_index,
#          ym_ins_8_rx_pharmtype_index,ym_ins_8_rx_index,ym_ins_8_index,
#          rx2,mail_order_dummy,entity_broad,  
#          insurance_type_53,  insurance_type_23, insurance_type_18, insurance_type_8, 
#          #          rxdays_broad,
#          ms = get(ms_zip),
#          ms_wal,
#          walgreens_zip_person,
#          opener_zip_dummy, non_opener,
#          usable = get(usable),
#          grocery = get(grocery),
#          event = X,
#          total_pills_dispensed_ym_z,
#          total_pills_dispensed_ym_z_exist = get(total_pills_exist),
#          total_pills_dispensed_ym_z_exist_wal = get(total_pills_exist_wal),
#          total_pills_dispensed_ym_rx_z,
#          total_pills_dispensed_ym_ins_18_z,total_pills_dispensed_ym_ins_53_z,
#          total_pills_dispensed_ym_ins_23_z,
#          total_pills_dispensed_ym_ins_23_rx_z,
#          total_pills_dispensed_ym_ins_6_z,
#          total_pills_dispensed_ym_ins_6_rx_z,
#          total_pills_dispensed_ym_ins_23_pharmtype_z,
#          total_pills_dispensed_ym_ins_8_z,
#          total_pills_dispensed_ym_mail_z,total_pills_dispensed_ym_pharmtype_z,#total_pills_dispensed_ym_30v90_z,
#          total_claims_ym_z,
#          total_claims_ym_z_exist = get(total_claims_exist),
#          total_claims_ym_z_exist_wal = get(total_claims_exist_wal),
#          total_claims_ym_rx_z,
#          total_claims_ym_ins_18_z,total_claims_ym_ins_53_z,
#          total_claims_ym_ins_23_z,
#          total_claims_ym_ins_23_rx_z,
#          total_claims_ym_ins_6_z,
#          total_claims_ym_ins_6_rx_z,
#          total_claims_ym_ins_23_pharmtype_z,
#          total_claims_ym_ins_8_z,
#          total_claims_ym_mail_z,total_claims_ym_pharmtype_z,#total_claims_ym_30v90_z, 
#          mail_share_ym_z,mail_share_ym_rx_z,mail_share_ym_rx,
#          mail_share_ym_ins_53_z,
#          mail_share_ym_ins_23_z,
#          mail_share_ym_ins_23_rx_z,
#          mail_share_ym_ins_6_z,
#          mail_share_ym_ins_6_rx_z,
#          mail_share_ym_ins_23_pharmtype_z,
#          mail_share_ym_ins_23_rx,mail_share_ym_ins_18_z,mail_share_ym_ins_8_z,
#          mail_share_ym_mail_z,mail_share_ym_pharmtype_z,
#          pills_per_person_ym_z,
#          pills_per_person_ym_z_exist = get(pills_per_person_exist),
#          pills_per_person_ym_z_exist_wal = get(pills_per_person_exist_wal),
#          pills_per_person_ym_rx_z,pills_per_person_ym_rx,
#          pills_per_person_ym_ins_53_z,
#          pills_per_person_ym_ins_23_z,
#          pills_per_person_ym_ins_23_rx_z,
#          pills_per_person_ym_ins_6_z,
#          pills_per_person_ym_ins_6_rx_z,
#          pills_per_person_ym_ins_23_pharmtype_z,
#          pills_per_person_ym_ins_23_rx,pills_per_person_ym_ins_18_z,pills_per_person_ym_ins_8_z,
#          pills_per_person_ym_mail_z,pills_per_person_ym_pharmtype_z,
#          total_money_ym_ins_23_z,
#          total_money_ym_ins_23_rx_z,
#          total_money_ym_ins_6_z,
#          total_money_ym_ins_6_rx_z,
#          total_money_ym_ins_23_pharmtype_z,
#          avg_copay_ym_ins_53_rx_pharmtype,avg_copay_ym_ins_23_rx_pharmtype,
#          avg_copay_ym_ins_53_rx,avg_copay_ym_ins_53,
#          avg_copay_ym_ins_23_rx,avg_copay_ym_ins_23,
#          avg_copay_ym_ins_18_rx_pharmtype,avg_copay_ym_ins_18_rx,avg_copay_ym_ins_18,
#          avg_copay_ym_ins_8_rx_pharmtype,avg_copay_ym_ins_8_rx,avg_copay_ym_ins_8,
#          avg_copay_ym_z,avg_copay_ym_rx_z,
#          avg_copay_ym_ins_53_z,
#          avg_copay_ym_ins_23_z,
#          avg_copay_ym_ins_23_rx_z,
#          avg_copay_ym_ins_6_z,
#          avg_copay_ym_ins_6_rx_z,
#          avg_copay_ym_ins_23_pharmtype_z,
#          avg_copay_ym_ins_18_z,avg_copay_ym_ins_8_z,
#          avg_copay_ym_mail_z,avg_copay_ym_pharmtype_z,#avg_copay_ym_30v90_z,
#          avg_copay_ym_pharmtype_rx_ins_53_z,avg_copay_ym_pharmtype_rx_ins_23_z,avg_copay_ym_pharmtype_rx_ins_18_z,avg_copay_ym_pharmtype_rx_ins_8_z,
#          avg_copay_ym_pharmtype_rx_z,
#          total_new_pats_ym_z,total_new_pats_ym_rx_z,
#          total_new_pats_ym_ins_53_z,
#          total_new_pats_ym_ins_23_z,
#          total_new_pats_ym_ins_23_rx_z,
#          total_new_pats_ym_ins_6_z,
#          total_new_pats_ym_ins_6_rx_z,
#          total_new_pats_ym_ins_23_pharmtype_z,
#          total_new_pats_ym_ins_18_z,total_new_pats_ym_ins_8_z,
#          total_new_pats_ym_mail_z,total_new_pats_ym_pharmtype_z,#total_new_pats_ym_30v90_z,
#          total_pats_ym_z,
#          total_pats_ym_z_exist = get(total_pats_exist),
#          total_pats_ym_z_exist_wal = get(total_pats_exist_wal),
#          total_pats_ym_rx_z,
#          total_pats_ym_ins_53_z,
#          total_pats_ym_ins_23_z,
#          total_pats_ym_ins_23_rx_z,
#          total_pats_ym_ins_6_z,
#          total_pats_ym_ins_6_rx_z,
#          total_pats_ym_ins_23_pharmtype_z,
#          total_pats_ym_ins_18_z,total_pats_ym_ins_8_z,
#          total_pats_ym_mail_z,total_pats_ym_pharmtype_z,#total_pats_ym_30v90_z,
#          avg_distance_ym_rx_z,avg_distance_ym_z,
#          avg_age_ym_z,
#          avg_age_ym_rx_z,
#          avg_age_ym_ins_53_z,
#          avg_age_ym_ins_23_z,
#          avg_age_ym_ins_23_rx_z,
#          avg_age_ym_ins_6_z,
#          avg_age_ym_ins_6_rx_z,
#          avg_age_ym_ins_23_pharmtype_z,
#          avg_age_ym_ins_18_z,avg_age_ym_ins_8_z,
#          avg_age_ym_mail_z,
#          avg_age_ym_pharmtype_z,
#          share_medicaid_ym_z,
#          tot_pharms_ym_z,
#          zip_factor,
#          mail_order_users_per_tot_pats_neg_1 = get(mail_order_users_per_tot_pats_neg_1),
#          #          percent_rural, urban_dummy, #UNCOMMENT THIS
#          #          total_estabs,pop_change, percent_rural, urban_dummy, total_health_estabs, tot_pop, #UNCOMMENT THIS
#          tot_pharms_neg_1 = get(pharms_neg_1),
#          share_medicaid_neg_1 = get(share_medicaid_neg_1),
#          opening_entity_broad = get(opening_entity)
#     )])
#   
#   rm(dt, bal, ms_zip, mail_users_per_cap_6_months, mail_order_users_per_tot_pats_neg_1,
#      grocery, pharms_neg_1, share_medicaid_neg_1, opening_entity,
#      mean_mpr_ym_rx_z_secondary_chronic, mean_mpr_ym_rx_z_secondary)
#   
#   gc()
# }
# gc()
# 
# 
# ####STACK EM UP####
# zip_data_stacked = rbindlist(list(zip_data_1, zip_data_2
#                                   , zip_data_3, zip_data_4 #UNCOMMENT THIS
# ))
# rm(zip_data_1, zip_data_2, zip_data_3, zip_data_4)
# gc()
# 
# setkey(zip_data_stacked, ZIP, ms)
# 
# 
# 
# 
# ####event time dummies####
# zip_data_stacked[, ms_neg9 := ifelse(ms == -9, 1, 0)]
# zip_data_stacked[, ms_neg8 := ifelse(ms == -8, 1, 0)]
# zip_data_stacked[, ms_neg7 := ifelse(ms == -7, 1, 0)]
# zip_data_stacked[, ms_neg6 := ifelse(ms == -6, 1, 0)]
# zip_data_stacked[, ms_neg5 := ifelse(ms == -5, 1, 0)]
# zip_data_stacked[, ms_neg4 := ifelse(ms == -4, 1, 0)]
# zip_data_stacked[, ms_neg3 := ifelse(ms == -3, 1, 0)]
# zip_data_stacked[, ms_neg2 := ifelse(ms == -2, 1, 0)]
# zip_data_stacked[, ms_neg1 := ifelse(ms == -1, 1, 0)]
# zip_data_stacked[, ms_0 := ifelse(ms == 0, 1, 0)]
# zip_data_stacked[, ms_1 := ifelse(ms == 1, 1, 0)]
# zip_data_stacked[, ms_2 := ifelse(ms == 2, 1, 0)]
# zip_data_stacked[, ms_3 := ifelse(ms == 3, 1, 0)]
# zip_data_stacked[, ms_4 := ifelse(ms == 4, 1, 0)]
# zip_data_stacked[, ms_5 := ifelse(ms == 5, 1, 0)]
# zip_data_stacked[, ms_6 := ifelse(ms == 6, 1, 0)]
# zip_data_stacked[, ms_7 := ifelse(ms == 7, 1, 0)]
# zip_data_stacked[, ms_8 := ifelse(ms == 8, 1, 0)]
# zip_data_stacked[, ms_9 := ifelse(ms == 9, 1, 0)]
# 
# zip_data_stacked[, post := ifelse(ms > 0 , 1, 0)]
# 
# #walgreens event dummies
# zip_data_stacked[, ms_wal_neg12 := ifelse(ms_wal == -12, 1, 0)]
# zip_data_stacked[, ms_wal_neg11 := ifelse(ms_wal == -11, 1, 0)]
# zip_data_stacked[, ms_wal_neg10 := ifelse(ms_wal == -10, 1, 0)]
# zip_data_stacked[, ms_wal_neg9 := ifelse(ms_wal == -9, 1, 0)]
# zip_data_stacked[, ms_wal_neg8 := ifelse(ms_wal == -8, 1, 0)]
# zip_data_stacked[, ms_wal_neg7 := ifelse(ms_wal == -7, 1, 0)]
# zip_data_stacked[, ms_wal_neg6 := ifelse(ms_wal == -6, 1, 0)]
# zip_data_stacked[, ms_wal_neg5 := ifelse(ms_wal == -5, 1, 0)]
# zip_data_stacked[, ms_wal_neg4 := ifelse(ms_wal == -4, 1, 0)]
# zip_data_stacked[, ms_wal_neg3 := ifelse(ms_wal == -3, 1, 0)]
# zip_data_stacked[, ms_wal_neg2 := ifelse(ms_wal == -2, 1, 0)]
# zip_data_stacked[, ms_wal_neg1 := ifelse(ms_wal == -1, 1, 0)]
# zip_data_stacked[, ms_wal_0 := ifelse(ms_wal == 0, 1, 0)]
# zip_data_stacked[, ms_wal_1 := ifelse(ms_wal == 1, 1, 0)]
# zip_data_stacked[, ms_wal_2 := ifelse(ms_wal == 2, 1, 0)]
# zip_data_stacked[, ms_wal_3 := ifelse(ms_wal == 3, 1, 0)]
# zip_data_stacked[, ms_wal_4 := ifelse(ms_wal == 4, 1, 0)]
# zip_data_stacked[, ms_wal_5 := ifelse(ms_wal == 5, 1, 0)]
# zip_data_stacked[, ms_wal_6 := ifelse(ms_wal == 6, 1, 0)]
# zip_data_stacked[, ms_wal_7 := ifelse(ms_wal == 7, 1, 0)]
# zip_data_stacked[, ms_wal_8 := ifelse(ms_wal == 8, 1, 0)]
# zip_data_stacked[, ms_wal_9 := ifelse(ms_wal == 9, 1, 0)]
# zip_data_stacked[, ms_wal_10 := ifelse(ms_wal == 10, 1, 0)]
# zip_data_stacked[, ms_wal_11 := ifelse(ms_wal == 11, 1, 0)]
# zip_data_stacked[, ms_wal_12 := ifelse(ms_wal == 12, 1, 0)]
# zip_data_stacked[, ms_wal_13 := ifelse(ms_wal == 13, 1, 0)]
# zip_data_stacked[, ms_wal_14 := ifelse(ms_wal == 14, 1, 0)]
# zip_data_stacked[, ms_wal_15 := ifelse(ms_wal == 15, 1, 0)]
# zip_data_stacked[, ms_wal_16 := ifelse(ms_wal == 16, 1, 0)]
# zip_data_stacked[, ms_wal_17 := ifelse(ms_wal == 17, 1, 0)]
# zip_data_stacked[, ms_wal_18 := ifelse(ms_wal == 18, 1, 0)]
# zip_data_stacked[, ms_wal_19 := ifelse(ms_wal == 19, 1, 0)]
# zip_data_stacked[, ms_wal_20 := ifelse(ms_wal == 20, 1, 0)]
# zip_data_stacked[, ms_wal_21 := ifelse(ms_wal == 21, 1, 0)]
# zip_data_stacked[, ms_wal_22 := ifelse(ms_wal == 22, 1, 0)]
# zip_data_stacked[, ms_wal_23 := ifelse(ms_wal == 23, 1, 0)]
# 
# zip_data_stacked[, post_wal := ifelse(ms_wal >= 0 , 1, 0)]
# 
# 
# 
# ####get weights for duplicated observations####
# zip_data_stacked[, num_obs_zip_month_nf := NULL]
# zip_data_stacked[, num_obs_zip_month := NULL]
# zip_data_stacked[zip_ym_index == 1 , num_obs_zip_month_nf := .N, by = c("ZIP","pick_up_yrmon")]
# zip_data_stacked[, num_obs_zip_month := na.locf(num_obs_zip_month_nf), by = c("ZIP","pick_up_yrmon")]
# 
# zip_data_stacked[, weight := 1 / num_obs_zip_month]
# # hist(zip_data_stacked[opening_zip == 1, ]$weight)
# gc()
# 
# #####the opening entity needs to be filled for some events####
# zip_data_stacked[,opening_entity_broad_filled := NULL ]
# zip_data_stacked[ms <= B & ms >= -B & non_opener == 0 &  zip_ym_index == 1,
#                  opening_entity_broad_filled := na.locf(grocery), by = c("ZIP","event")]
# zip_data_stacked[!is.na(opening_entity_broad_filled), grocery := opening_entity_broad_filled]
# zip_data_stacked[,opening_entity_broad_filled := NULL ]
# gc()
# 
# 
# ####Save####
# save(zip_data_stacked, file = "~/current/pharmacy_deserts/data/zip_data_stacked.RData")
# 
# 
# ####CLOSINGS STACK####
# B = 6
# ####stack the data ####
# for (X in c(seq(1,2))) {
#   #   X = 1
#   dt = paste("zip_data_", X, sep="")
#   usable_c = paste("usable_c_",X,sep="")
#   msc_zip = paste("msc_zip_",X,sep="")
#   mail_order_users_per_tot_pats_neg_1c = paste("mail_order_users_per_tot_pats_neg_1c_",X,sep="")
#   grocery = paste("grocery_",X,sep="")
#   pharms_neg_1c = paste("tot_pharms_ym_z_neg_1c_",X,sep="")
#   share_medicaid_neg_1c = paste("share_medicaid_ym_z_neg_1c_",X,sep="")
#   closing_entity = paste("closing_entity_broad_",X,sep="")
#   
#   assign(eval(dt),rxnkawz[
#     !is.na(get(msc_zip)) #this keeps all on the first round, but only those with openings on the second-fourth
#     #     (get(usable) == 1 | non_opener == 1) 
#     #     & get(ms_zip) <= B+6 & get(ms_zip) >= -B-6
#     # & opening_zip == 1
#     & grepl("^97",ZIP)
#     ,
#     
#     list(ZIP,
#          pick_up_yrmon, 
#          zip_ym_index,
#          yrqtr_factor, yrmon_factor, yrmon_number,
#          zip_index,   zip_ym_mail_index,
#          zip_ym_ins_53_index,zip_ym_ins_23_index,zip_ym_ins_18_index,zip_ym_ins_8_index,
#          zip_ym_ins_8_rx_index, zip_ym_ins_8_index,
#          zip_ym_pharmtype_index,#zip_ym_30v90_index,
#          total_unique_zips_ym_z,
#          zip_ym_rx_index,
#          ym_ins_53_rx_pharmtype_index,ym_ins_53_rx_index,ym_ins_53_index,
#          ym_ins_23_rx_pharmtype_index,ym_ins_23_rx_index,ym_ins_23_index,
#          ym_ins_18_rx_pharmtype_index,ym_ins_18_rx_index,ym_ins_18_index,
#          ym_ins_8_rx_pharmtype_index,ym_ins_8_rx_index,ym_ins_8_index,
#          rx2,mail_order_dummy,entity_broad,  
#          insurance_type_53,  insurance_type_23, insurance_type_18, insurance_type_8, 
#          #          rxdays_broad,
#          msc = get(msc_zip),
#          closer_zip_dummy, non_closer,
#          usable_c = get(usable_c),
#          event = X,
#          total_pills_dispensed_ym_z,total_pills_dispensed_ym_rx_z,
#          total_pills_dispensed_ym_ins_18_z,total_pills_dispensed_ym_ins_53_z,total_pills_dispensed_ym_ins_23_z,total_pills_dispensed_ym_ins_8_z,
#          total_pills_dispensed_ym_mail_z,total_pills_dispensed_ym_pharmtype_z,#total_pills_dispensed_ym_30v90_z,
#          total_claims_ym_z,total_claims_ym_rx_z,
#          total_claims_ym_ins_18_z,total_claims_ym_ins_53_z,total_claims_ym_ins_23_z,total_claims_ym_ins_8_z,
#          total_claims_ym_mail_z,total_claims_ym_pharmtype_z,#total_claims_ym_30v90_z,
#          mail_share_ym_z,mail_share_ym_rx_z,mail_share_ym_rx,
#          mail_share_ym_ins_53_z,
#          mail_share_ym_ins_23_z,
#          mail_share_ym_ins_23_rx_z,
#          mail_share_ym_ins_6_z,
#          mail_share_ym_ins_6_rx_z,
#          mail_share_ym_ins_23_pharmtype_z,
#          mail_share_ym_ins_23_rx,mail_share_ym_ins_18_z,mail_share_ym_ins_8_z,
#          mail_share_ym_mail_z,mail_share_ym_pharmtype_z,
#          pills_per_person_ym_z,pills_per_person_ym_rx_z,pills_per_person_ym_rx,
#          pills_per_person_ym_ins_53_z,
#          pills_per_person_ym_ins_23_z,
#          pills_per_person_ym_ins_23_rx_z,
#          pills_per_person_ym_ins_6_z,
#          pills_per_person_ym_ins_6_rx_z,
#          pills_per_person_ym_ins_23_pharmtype_z,
#          pills_per_person_ym_ins_23_rx,pills_per_person_ym_ins_18_z,pills_per_person_ym_ins_8_z,
#          pills_per_person_ym_mail_z,pills_per_person_ym_pharmtype_z,
#          avg_copay_ym_ins_53_rx_pharmtype,avg_copay_ym_ins_23_rx_pharmtype,
#          avg_copay_ym_ins_53_rx,avg_copay_ym_ins_53,
#          avg_copay_ym_ins_23_rx,avg_copay_ym_ins_23,
#          avg_copay_ym_ins_18_rx_pharmtype,avg_copay_ym_ins_18_rx,avg_copay_ym_ins_18,
#          avg_copay_ym_ins_8_rx_pharmtype,avg_copay_ym_ins_8_rx,avg_copay_ym_ins_8,
#          avg_copay_ym_z,avg_copay_ym_rx_z,
#          avg_copay_ym_ins_53_z,avg_copay_ym_ins_23_z,avg_copay_ym_ins_18_z,avg_copay_ym_ins_8_z,
#          avg_copay_ym_mail_z,avg_copay_ym_pharmtype_z,#avg_copay_ym_30v90_z,
#          avg_copay_ym_pharmtype_rx_ins_53_z,avg_copay_ym_pharmtype_rx_ins_23_z,avg_copay_ym_pharmtype_rx_ins_18_z,avg_copay_ym_pharmtype_rx_ins_8_z,
#          avg_copay_ym_pharmtype_rx_z,
#          total_new_pats_ym_z,total_new_pats_ym_rx_z,
#          total_new_pats_ym_ins_53_z,total_new_pats_ym_ins_23_z,total_new_pats_ym_ins_18_z,total_new_pats_ym_ins_8_z,
#          total_new_pats_ym_mail_z,total_new_pats_ym_pharmtype_z,#total_new_pats_ym_30v90_z,
#          total_pats_ym_z,total_pats_ym_rx_z,
#          total_pats_ym_ins_53_z,total_pats_ym_ins_23_z,total_pats_ym_ins_18_z,total_pats_ym_ins_8_z,
#          total_pats_ym_mail_z,total_pats_ym_pharmtype_z,#total_pats_ym_30v90_z,
#          avg_distance_ym_rx_z,avg_distance_ym_z,
#          avg_age_ym_z,
#          avg_age_ym_rx_z,
#          avg_age_ym_ins_53_z,avg_age_ym_ins_23_z,avg_age_ym_ins_18_z,avg_age_ym_ins_8_z,
#          avg_age_ym_mail_z,
#          avg_age_ym_pharmtype_z,
#          share_medicaid_ym_z,
#          tot_pharms_ym_z,
#          zip_factor,
#          mail_order_users_per_tot_pats_neg_1c = get(mail_order_users_per_tot_pats_neg_1c),
#          #          percent_rural, urban_dummy, #UNCOMMENT THIS
#          #          total_estabs,pop_change, percent_rural, urban_dummy, total_health_estabs, tot_pop, #UNCOMMENT THIS
#          tot_pharms_neg_1c = get(pharms_neg_1c),
#          share_medicaid_neg_1c = get(share_medicaid_neg_1c)
#     )])
#   
#   rm(dt, bal, msc_zip,  mail_order_users_per_tot_pats_neg_1c,
#      grocery, pharms_neg_1c, share_medicaid_neg_1c, closing_entity)
#   
#   gc()
# }
# gc()
# 
# 
# ####STACK EM UP####
# zip_data_stacked_closings = rbindlist(list(zip_data_1, zip_data_2))
# rm(zip_data_1, zip_data_2)
# gc()
# 
# setkey(zip_data_stacked_closings, ZIP, msc)
# 
# 
# 
# 
# ####event time dummies####
# zip_data_stacked_closings[, msc_neg9 := ifelse(msc == -9, 1, 0)]
# zip_data_stacked_closings[, msc_neg8 := ifelse(msc == -8, 1, 0)]
# zip_data_stacked_closings[, msc_neg7 := ifelse(msc == -7, 1, 0)]
# zip_data_stacked_closings[, msc_neg6 := ifelse(msc == -6, 1, 0)]
# zip_data_stacked_closings[, msc_neg5 := ifelse(msc == -5, 1, 0)]
# zip_data_stacked_closings[, msc_neg4 := ifelse(msc == -4, 1, 0)]
# zip_data_stacked_closings[, msc_neg3 := ifelse(msc == -3, 1, 0)]
# zip_data_stacked_closings[, msc_neg2 := ifelse(msc == -2, 1, 0)]
# zip_data_stacked_closings[, msc_neg1 := ifelse(msc == -1, 1, 0)]
# zip_data_stacked_closings[, msc_0 := ifelse(msc == 0, 1, 0)]
# zip_data_stacked_closings[, msc_1 := ifelse(msc == 1, 1, 0)]
# zip_data_stacked_closings[, msc_2 := ifelse(msc == 2, 1, 0)]
# zip_data_stacked_closings[, msc_3 := ifelse(msc == 3, 1, 0)]
# zip_data_stacked_closings[, msc_4 := ifelse(msc == 4, 1, 0)]
# zip_data_stacked_closings[, msc_5 := ifelse(msc == 5, 1, 0)]
# zip_data_stacked_closings[, msc_6 := ifelse(msc == 6, 1, 0)]
# zip_data_stacked_closings[, msc_7 := ifelse(msc == 7, 1, 0)]
# zip_data_stacked_closings[, msc_8 := ifelse(msc == 8, 1, 0)]
# zip_data_stacked_closings[, msc_9 := ifelse(msc == 9, 1, 0)]
# 
# zip_data_stacked_closings[, post := ifelse(msc > 0 , 1, 0)]
# 
# 
# 
# ####get weights for duplicated observations####
# zip_data_stacked_closings[, num_obs_zip_month_nf := NULL]
# zip_data_stacked_closings[, num_obs_zip_month := NULL]
# zip_data_stacked_closings[zip_ym_index == 1 , num_obs_zip_month_nf := .N, by = c("ZIP","pick_up_yrmon")]
# zip_data_stacked_closings[, num_obs_zip_month := na.locf(num_obs_zip_month_nf), by = c("ZIP","pick_up_yrmon")]
# 
# zip_data_stacked_closings[, weight := 1 / num_obs_zip_month]
# # hist(zip_data_stacked_closings[opening_zip == 1, ]$weight)
# gc()
# 
# 
# 
# 
# 
# 
# ####SAVE####
# save(zip_data_stacked_closings, file = "~/current/pharmacy_deserts/data/zip_data_stacked_closings.RData")
# 
# 
# 
# 
# ####mail order events stack####
# ####stack the data ####
# for (X in c("sn2","hmo","mli","mpd")) {
#   
#   #   X = "sn2"
#   upperX = toupper(X)
#   dt = paste("mail_data_", X, sep="")
#   ms_mail = paste("ms_mail_ins23_",X,sep="")
#   
#   assign(eval(dt),rxnkawz[
#     insurance_type_23 == upperX #so we only have the insurance type corresponding to the event
#     & get(ms_mail) >= -9 & get(ms_mail) <= 9 # limit to the event range
#     & grepl("^97",ZIP) #patients live in Oregon
#     ,
#     
#     list(insurance_type_23,
#          pick_up_yrmon, 
#          yrqtr_factor, yrmon_factor, yrmon_number,
#          ym_ins_23_rx_index,
#          rx2, 
#          #          mail_order_dummy,
#          insurance_type_18, insurance_type_8, 
#          #          rxdays_broad,
#          ms = get(ms_mail),
#          total_pills_dispensed_ym_ins_23_rx,
#          total_claims_ym_ins_23_rx,
#          avg_copay_ym_ins_23_rx,
#          total_new_pats_ym_ins_23_rx,
#          total_pats_ym_ins_23_rx
#     )])
#   
#   gc()
# }
# gc()
# 
# 
# ####STACK EM UP####
# mail_data_stacked = rbindlist(list(mail_data_sn2, mail_data_hmo, mail_data_mli, mail_data_mpd))
# rm(mail_data_sn2, mail_data_hmo, mail_data_mli, mail_data_mpd)
# gc()
# 
# setkey(mail_data_stacked, insurance_type_23,rx2, ms)
# 
# 
# #merge on the rxclass label
# rxclass_label = as.data.table(read.csv("~/current/pharmacy_deserts/data/rxclass_labels.csv"))
# rxclass_label[, drug := as.character(as.numeric(rx2))]
# rxclass_label[nchar(drug) == 1, drug := paste("0",drug,sep="")]
# setkey(rxclass_label, drug)
# setkey(mail_data_stacked, rx2)
# mail_data_stacked[rxclass_label, rx_label := label]
# gc()
# 
# 
# 
# ####event time dummies####
# mail_data_stacked[, ms_neg9 := ifelse(ms == -9, 1, 0)]
# mail_data_stacked[, ms_neg8 := ifelse(ms == -8, 1, 0)]
# mail_data_stacked[, ms_neg7 := ifelse(ms == -7, 1, 0)]
# mail_data_stacked[, ms_neg6 := ifelse(ms == -6, 1, 0)]
# mail_data_stacked[, ms_neg5 := ifelse(ms == -5, 1, 0)]
# mail_data_stacked[, ms_neg4 := ifelse(ms == -4, 1, 0)]
# mail_data_stacked[, ms_neg3 := ifelse(ms == -3, 1, 0)]
# mail_data_stacked[, ms_neg2 := ifelse(ms == -2, 1, 0)]
# mail_data_stacked[, ms_neg1 := ifelse(ms == -1, 1, 0)]
# mail_data_stacked[, ms_0 := ifelse(ms == 0, 1, 0)]
# mail_data_stacked[, ms_1 := ifelse(ms == 1, 1, 0)]
# mail_data_stacked[, ms_2 := ifelse(ms == 2, 1, 0)]
# mail_data_stacked[, ms_3 := ifelse(ms == 3, 1, 0)]
# mail_data_stacked[, ms_4 := ifelse(ms == 4, 1, 0)]
# mail_data_stacked[, ms_5 := ifelse(ms == 5, 1, 0)]
# mail_data_stacked[, ms_6 := ifelse(ms == 6, 1, 0)]
# mail_data_stacked[, ms_7 := ifelse(ms == 7, 1, 0)]
# mail_data_stacked[, ms_8 := ifelse(ms == 8, 1, 0)]
# mail_data_stacked[, ms_9 := ifelse(ms == 9, 1, 0)]
# 
# mail_data_stacked[, post := ifelse(ms > 0 , 1, 0)]
# 
# 
# ###which drugs changed and which did not####
# mail_data_stacked[, mail_treated_drug := ifelse((insurance_type_23 == "SN2"
#                                                  & rx_label %in% c("BETA BLOCKERS", "ANTIHYPERTENSIVES",
#                                                                    "ANTIDIABETICS","ANTIHYPERLIPIDEMICS"))
#                                                 | (insurance_type_23 == "HMO"
#                                                    & rx_label == "THYROID AGENTS")
#                                                 | (insurance_type_23 == "MPD"
#                                                    & rx_label %in% c("BETA BLOCKERS", "ANTIHYPERTENSIVES",
#                                                                      "THYROID AGENTS","ANTIHYPERLIPIDEMICS",
#                                                                      "CALCIUM CHANNEL BLOCKERS"))
#                                                 | (insurance_type_23 == "MLI"
#                                                    & rx_label == "THYROID AGENTS"),
#                                                 1 , 0 )]
# 
# 
# ####SAVE####
# save(mail_data_stacked, file = "~/current/pharmacy_deserts/data/mail_data_stacked.RData")
# 
# 
# ###################################################################################################################################################################################
# ####Section 7: collapsed/stacked event study regressions and plots ####
# ###################################################################################################################################################################################
# 
# B=6
# gc()
# ####stacked data########################################################################
# ####event study regressions####
# 
# 
# # zip_data_stacked[, zip_ms_index := seq_len(.N),
# #                  by = c("ZIP","pick_up_yrmon")]
# ##NOTE: zip_ms_index is created AFTER the stack so it pulls out zips with multiple openings in the same month
# ##but zip_ym_index was created before the stack so it only removes obs with other ins types or rxclasses
# 
# ####plus 1####
# for (X in c("total_pills_dispensed_ym_z","total_pats_ym_z","total_claims_ym_z",
#             "avg_age_ym_z","avg_copay_ym_z", "total_new_pats_ym_z","share_medicaid_ym_z",
#             #                   "share_claims_inzip_ym_z", "total_claims_inzip_ym_z",
#             "total_unique_zips_ym_z",
#             "total_pills_dispensed_ym_rx_z","total_pats_ym_rx_z","total_claims_ym_rx_z",
#             "avg_age_ym_rx_z","avg_copay_ym_rx_z", "total_new_pats_ym_rx_z",
#             #                   "mean_mpr_ym_rx_z_secondary_chronic","mean_mpr_ym_rx_z_secondary",
#             "total_pills_dispensed_ym_ins_18_z","total_pats_ym_ins_18_z","total_claims_ym_ins_18_z",
#             "avg_age_ym_ins_18_z","avg_copay_ym_ins_18_z", "total_new_pats_ym_ins_18_z" ,
#             "total_pills_dispensed_ym_ins_8_z","total_pats_ym_ins_8_z","total_claims_ym_ins_8_z",
#             "avg_age_ym_ins_8_z","avg_copay_ym_ins_8_z", "total_new_pats_ym_ins_8_z" ,
#             "total_pills_dispensed_ym_ins_23_z","total_pats_ym_ins_23_z","total_claims_ym_ins_23_z",
#             "avg_age_ym_ins_23_z","avg_copay_ym_ins_23_z", "total_new_pats_ym_ins_23_z" ,
#             # "total_pills_dispensed_ym_ins_6_z","total_pats_ym_ins_6_z","total_claims_ym_ins_6_z",
#             # "avg_age_ym_ins_6_z","avg_copay_ym_ins_6_z", "total_new_pats_ym_ins_6_z" ,
#             "total_pills_dispensed_ym_ins_53_z","total_pats_ym_ins_53_z","total_claims_ym_ins_53_z",
#             "avg_age_ym_ins_53_z","avg_copay_ym_ins_53_z", "total_new_pats_ym_ins_53_z" ,
#             "total_pills_dispensed_ym_mail_z","total_pats_ym_mail_z","total_claims_ym_mail_z",
#             "avg_copay_ym_mail_z", "total_new_pats_ym_mail_z",
#             "total_pills_dispensed_ym_pharmtype_z","total_pats_ym_pharmtype_z","total_claims_ym_pharmtype_z",
#             "avg_age_ym_pharmtype_z","avg_copay_ym_pharmtype_z",
#             "avg_distance_ym_rx_z","avg_distance_ym_z",
#             "total_pills_dispensed_ym_rx_z","total_money_ym_ins_23_z",
#             "total_pills_dispensed_ym_ins_23_rx_z","total_pats_ym_ins_23_rx_z","total_claims_ym_ins_23_rx_z",
#             "total_new_pats_ym_ins_23_rx_z","total_money_ym_ins_23_rx_z",
#             # "total_pills_dispensed_ym_ins_6_rx_z","total_pats_ym_ins_6_rx_z","total_claims_ym_ins_6_rx_z",
#             # "total_new_pats_ym_ins_6_rx_z","total_money_ym_ins_6_rx_z",
#             "total_pills_dispensed_ym_ins_23_pharmtype_z","total_pats_ym_ins_23_pharmtype_z","total_claims_ym_ins_23_pharmtype_z",
#             "total_new_pats_ym_ins_23_pharmtype_z","total_money_ym_ins_23_pharmtype_z")) {
#   
#   plus_1 = paste(X, "_plus_1",sep="")
#   zip_data_stacked[, (plus_1) := get(X) + 1]
#   # rxnkawz[, (plus_1) := get(X) + 1]
#   #   mail_data_stacked[, (plus_1) := get(X) + 1]
#   #   zip_data_stacked_closings[, (plus_1) := get(X) + 1]
#   
# }
# 
# #merge on the rxclass label
# rxclass_label = as.data.table(read.csv("~/current/pharmacy_deserts/data/rxclass_labels.csv"))
# rxclass_label[, drug := as.character(rx2)]
# rxclass_label[nchar(drug) == 1, drug := paste("0",drug, sep="")]
# setkey(rxclass_label, drug)
# setkey(zip_data_stacked, rx2)
# zip_data_stacked[rxclass_label, rx_label := label]
# setkey(rxnkawz, rx2)
# rxnkawz[rxclass_label, rx_label := label]
# gc()
# ####log the variables that need logging####
# for (outcome in c("total_pills_dispensed_ym_z","total_pats_ym_z","total_claims_ym_z",
#                   "avg_age_ym_z","avg_copay_ym_z","avg_copay_ym_z_plus_1", "total_new_pats_ym_z_plus_1","share_medicaid_ym_z",
#                   #                   "share_claims_inzip_ym_z", "total_claims_inzip_ym_z",
#                   "total_unique_zips_ym_z",
#                   "total_pills_dispensed_ym_rx_z","total_pats_ym_rx_z","total_claims_ym_rx_z",
#                   "total_claims_ym_z_exist","total_pats_ym_z_exist","total_pills_dispensed_ym_z_exist","pills_per_person_ym_z_exist",
#                   "total_claims_ym_z_exist_wal","total_pats_ym_z_exist_wal","total_pills_dispensed_ym_z_exist_wal","pills_per_person_ym_z_exist_wal",
#                   "avg_age_ym_rx_z","avg_copay_ym_rx_z_plus_1", "total_new_pats_ym_rx_z_plus_1",
#                   #                   "mean_mpr_ym_rx_z_secondary_chronic","mean_mpr_ym_rx_z_secondary",
#                   "total_pills_dispensed_ym_ins_18_z","total_pats_ym_ins_18_z","total_claims_ym_ins_18_z",
#                   "avg_age_ym_ins_18_z","avg_copay_ym_ins_18_z_plus_1", "total_new_pats_ym_ins_18_z_plus_1" ,
#                   "total_pills_dispensed_ym_ins_8_z","total_pats_ym_ins_8_z","total_claims_ym_ins_8_z",
#                   "avg_age_ym_ins_8_z","avg_copay_ym_ins_8_z_plus_1", "total_new_pats_ym_ins_8_z_plus_1" ,
#                   "total_pills_dispensed_ym_ins_23_z","total_pats_ym_ins_23_z","total_claims_ym_ins_23_z",
#                   "avg_age_ym_ins_23_z","avg_copay_ym_ins_23_z_plus_1", "total_new_pats_ym_ins_23_z_plus_1" ,
#                   # "total_pills_dispensed_ym_ins_6_z","total_pats_ym_ins_6_z","total_claims_ym_ins_6_z",
#                   # "avg_age_ym_ins_6_z","avg_copay_ym_ins_6_z_plus_1", "total_new_pats_ym_ins_6_z_plus_1" ,
#                   "total_pills_dispensed_ym_ins_53_z","total_pats_ym_ins_53_z","total_claims_ym_ins_53_z",
#                   "avg_age_ym_ins_53_z","avg_copay_ym_ins_53_z_plus_1", "total_new_pats_ym_ins_53_z_plus_1" ,
#                   "total_pills_dispensed_ym_mail_z","total_pats_ym_mail_z","total_claims_ym_mail_z",
#                   "avg_copay_ym_mail_z_plus_1", "total_new_pats_ym_mail_z_plus_1",
#                   "total_pills_dispensed_ym_pharmtype_z","total_pats_ym_pharmtype_z","total_claims_ym_pharmtype_z",
#                   "avg_age_ym_pharmtype_z","avg_copay_ym_pharmtype_z",
#                   "avg_distance_ym_rx_z_plus_1","avg_distance_ym_z_plus_1","total_money_ym_ins_23_z",
#                   "total_pills_dispensed_ym_rx_z_plus_1","total_money_ym_ins_23_z_plus_1",
#                   # "total_money_ym_ins_6_z_plus_1",
#                   "total_pills_dispensed_ym_ins_23_rx_z","total_pats_ym_ins_23_rx_z","total_claims_ym_ins_23_rx_z",
#                   # "total_pills_dispensed_ym_ins_6_rx_z","total_pats_ym_ins_6_rx_z","total_claims_ym_ins_6_rx_z",
#                   "total_new_pats_ym_ins_23_rx_z_plus_1","total_money_ym_ins_23_rx_z",
#                   # "total_new_pats_ym_ins_6_rx_z_plus_1","total_money_ym_ins_6_rx_z",
#                   "total_pills_dispensed_ym_ins_23_pharmtype_z","total_pats_ym_ins_23_pharmtype_z","total_claims_ym_ins_23_pharmtype_z",
#                   "total_new_pats_ym_ins_23_pharmtype_z_plus_1","total_money_ym_ins_23_pharmtype_z_plus_1",
#                   "pills_per_person_ym_z","pills_per_person_ym_rx_z" ,"pills_per_person_ym_rx"        ,             
#                   "pills_per_person_ym_ins_53_z","pills_per_person_ym_ins_23_z","pills_per_person_ym_ins_23_rx_z" ,
#                   # "pills_per_person_ym_ins_6_z","pills_per_person_ym_ins_6_rx_z" ,
#                   "pills_per_person_ym_ins_23_pharmtype_z","pills_per_person_ym_ins_23_rx","pills_per_person_ym_ins_18_z" ,              
#                   "pills_per_person_ym_ins_8_z","pills_per_person_ym_mail_z","pills_per_person_ym_pharmtype_z" )) {
#   # outcome = "total_money_ym_ins_23_z_plus_1"
#   log = paste("log_",outcome, sep="")
#   zip_data_stacked[, (log) := log(get(outcome))]
#   # rxnkawz[, (log) := log(get(outcome))]
#   # zip_data_stacked_closings[, (log) := log(get(outcome))]
#   rm(log)
#   gc()
# }
# 
# for (outcome in c("total_pills_dispensed_ym_ins_23_rx","total_pats_ym_ins_23_rx","total_claims_ym_ins_23_rx",
#                   "total_new_pats_ym_ins_23_rx_plus_1",
#                   "total_pills_dispensed_ym_ins_6_rx","total_pats_ym_ins_6_rx","total_claims_ym_ins_6_rx",
#                   "total_new_pats_ym_ins_6_rx_plus_1"
# )) {
#   log = paste("log_",outcome, sep="")
#   rxnkawz[, (log) := log(get(outcome))]
#   rm(log)
#   gc()
# }
# 
# 
# ####Summary stats####
# #six columns: opener zips, closer zips, and nonopener, noncloser zips,  pre-post
# #everything at  the per-month-patient level
# zip_data_stacked_closings[, total_pills_dispensed_ym_z_per_pat := total_pills_dispensed_ym_z / total_pats_ym_z]
# zip_data_stacked_closings[, total_claims_ym_z_per_pat := total_claims_ym_z / total_pats_ym_z]
# zip_data_stacked_closings[, total_new_pats_ym_z_per_pat := total_new_pats_ym_z / total_pats_ym_z]
# zip_data_stacked_closings[, tot_pharms_ym_z_per_pat := tot_pharms_ym_z * 1000 / total_pats_ym_z]
# zip_data_stacked[, total_pills_dispensed_ym_z_per_pat := total_pills_dispensed_ym_z / total_pats_ym_z]
# zip_data_stacked[, total_claims_ym_z_per_pat := total_claims_ym_z / total_pats_ym_z]
# zip_data_stacked[, total_new_pats_ym_z_per_pat := total_new_pats_ym_z / total_pats_ym_z]
# zip_data_stacked[, tot_pharms_ym_z_per_pat := tot_pharms_ym_z * 1000 / total_pats_ym_z]
# 
# opener_sum_stats_table = zip_data_stacked[zip_ym_index == 1 & (usable == 1 | non_opener == 1),
#                                           list(mean_avg_age_ym_z = round(mean(avg_age_ym_z, na.rm=T),3),
#                                                sd_avg_age_ym_z = round(sd(avg_age_ym_z, na.rm=T),3),
#                                                mean_share_medicaid_ym_z = round(mean(share_medicaid_ym_z, na.rm=T),3),
#                                                sd_share_medicaid_ym_z = round(sd(share_medicaid_ym_z, na.rm=T),3),
#                                                mean_avg_copay_ym_z = round(mean(avg_copay_ym_z, na.rm=T),3),
#                                                sd_avg_copay_ym_z = round(sd(avg_copay_ym_z, na.rm=T),3),
#                                                mean_total_pats_ym_z = round(mean(total_pats_ym_z, na.rm=T),3),
#                                                sd_total_pats_ym_z = round(sd(total_pats_ym_z, na.rm=T),3),
#                                                mean_total_pills_dispensed_ym_z_per_pat = round(mean(total_pills_dispensed_ym_z_per_pat, na.rm=T),3),
#                                                sd_total_pills_dispensed_ym_z_per_pat = round(sd(total_pills_dispensed_ym_z_per_pat, na.rm=T),3),
#                                                mean_total_claims_ym_z_per_pat = round(mean(total_claims_ym_z_per_pat, na.rm=T),3),
#                                                sd_total_claims_ym_z_per_pat = round(sd(total_claims_ym_z_per_pat, na.rm=T),3), 
#                                                mean_total_new_pats_ym_z_per_pat = round(mean(total_new_pats_ym_z_per_pat, na.rm=T),3),
#                                                sd_total_new_pats_ym_z_per_pat = round(sd(total_new_pats_ym_z_per_pat, na.rm=T),3), 
#                                                mean_tot_pharms_ym_z_per_pat = round(mean(tot_pharms_ym_z_per_pat, na.rm=T),3),
#                                                sd_tot_pharms_ym_z_per_pat = round(sd(tot_pharms_ym_z_per_pat, na.rm=T),3)), 
#                                           by = .(usable, non_opener, post)]
# opener_sum_stats_table[, header := paste(non_opener, post, sep="+")]
# opener_sum_stats_table = dcast(melt(opener_sum_stats_table, id.vars = "header"), variable ~ header)
# write.csv(opener_sum_stats_table, file = "~/current/pharmacy_deserts/output/opener_sum_stats_table.csv")
# 
# 
# ###closings####
# 
# closer_sum_stats_table = zip_data_stacked_closings[zip_ym_index == 1 & (usable_c == 1 | non_closer == 1),
#                                                    list(mean_avg_age_ym_z = round(mean(avg_age_ym_z, na.rm=T),3),
#                                                         sd_avg_age_ym_z = round(sd(avg_age_ym_z, na.rm=T),3),
#                                                         mean_share_medicaid_ym_z = round(mean(share_medicaid_ym_z, na.rm=T),3),
#                                                         sd_share_medicaid_ym_z = round(sd(share_medicaid_ym_z, na.rm=T),3),
#                                                         mean_avg_copay_ym_z = round(mean(avg_copay_ym_z, na.rm=T),3),
#                                                         sd_avg_copay_ym_z = round(sd(avg_copay_ym_z, na.rm=T),3),
#                                                         mean_total_pats_ym_z = round(mean(total_pats_ym_z, na.rm=T),3),
#                                                         sd_total_pats_ym_z = round(sd(total_pats_ym_z, na.rm=T),3),
#                                                         mean_total_pills_dispensed_ym_z_per_pat = round(mean(total_pills_dispensed_ym_z_per_pat, na.rm=T),3),
#                                                         sd_total_pills_dispensed_ym_z_per_pat = round(sd(total_pills_dispensed_ym_z_per_pat, na.rm=T),3),
#                                                         mean_total_claims_ym_z_per_pat = round(mean(total_claims_ym_z_per_pat, na.rm=T),3),
#                                                         sd_total_claims_ym_z_per_pat = round(sd(total_claims_ym_z_per_pat, na.rm=T),3), 
#                                                         mean_total_new_pats_ym_z_per_pat = round(mean(total_new_pats_ym_z_per_pat, na.rm=T),3),
#                                                         sd_total_new_pats_ym_z_per_pat = round(sd(total_new_pats_ym_z_per_pat, na.rm=T),3), 
#                                                         mean_tot_pharms_ym_z_per_pat = round(mean(tot_pharms_ym_z_per_pat, na.rm=T),3),
#                                                         sd_tot_pharms_ym_z_per_pat = round(sd(tot_pharms_ym_z_per_pat, na.rm=T),3)), 
#                                                    by = .(usable_c, non_closer, post)]
# closer_sum_stats_table[, header := paste("closer", non_closer, post, sep="+")]
# closer_sum_stats_table = dcast(melt(closer_sum_stats_table, id.vars = "header"), variable ~ header)
# 
# write.csv(closer_sum_stats_table, file = "~/current/pharmacy_deserts/output/closer_sum_stats_table.csv")
# closer_sum_stats_table[, variable := NULL]
# sum_stats_table = cbind(opener_sum_stats_table, closer_sum_stats_table)
# write.csv(sum_stats_table, file = "~/current/pharmacy_deserts/output/sum_stats_table.csv")
# 
# #non_opener is those that never had openings. opener_zip_dummy is for those that had openings that are usable (in the balance time frame)
# 
# B = 6
# ####months since regs####
# pdf(file = paste("~/current/pharmacy_deserts/event_study_no_weights_",B,".pdf",sep=""), width = 18, height = 10)
# for (outcome in c("log_total_pills_dispensed_ym_z","log_total_pats_ym_z","log_total_claims_ym_z",
#                   "log_total_new_pats_ym_z_plus_1", "log_pills_per_person_ym_z",
#                   "log_avg_age_ym_z","log_avg_copay_ym_z_plus_1","share_medicaid_ym_z",
#                   "avg_distance_ym_z","log_avg_distance_ym_z_plus_1"
# )) {
#   
#   #   outcome = "log_pills_per_person_ym_z"
#   reg_ms_nc = paste("reg_ms_nc_",outcome, sep="")
#   reg_ms_nc_cl = paste("reg_ms_nc_cl_",outcome, sep="")
#   reg_ms_c = paste("reg_ms_c_",outcome, sep="")
#   reg_ms_c_cl = paste("reg_ms_c_cl_",outcome, sep="")
#   
#   reg_ms_nc_all = paste("reg_ms_nc_all_",outcome, sep="")
#   reg_ms_nc_all_cl = paste("reg_ms_nc_all_cl_",outcome, sep="")
#   reg_ms_c_all = paste("reg_ms_c_all_",outcome, sep="")
#   reg_ms_c_all_cl = paste("reg_ms_c_all_cl_",outcome, sep="")
#   
#   reg_msc_nc = paste("reg_msc_nc_",outcome, sep="")
#   reg_msc_nc_cl = paste("reg_msc_nc_cl_",outcome, sep="")
#   reg_msc_c = paste("reg_msc_c_",outcome, sep="")
#   reg_msc_c_cl = paste("reg_msc_c_cl_",outcome, sep="")
#   
#   reg_msc_nc_all = paste("reg_msc_nc_all_",outcome, sep="")
#   reg_msc_nc_all_cl = paste("reg_msc_nc_all_cl_",outcome, sep="")
#   reg_msc_c_all = paste("reg_msc_c_all_",outcome, sep="")
#   reg_msc_c_all_cl = paste("reg_msc_c_all_cl_",outcome, sep="")
#   
#   reg_ms_nc_ppl = paste("reg_ms_nc_ppl_",outcome, sep="")
#   reg_ms_nc_ppl_cl = paste("reg_ms_nc_ppl_cl_",outcome, sep="")
#   reg_ms_c_ppl = paste("reg_ms_c_ppl_",outcome, sep="")
#   reg_ms_c_ppl_cl = paste("reg_ms_c_ppl_cl_",outcome, sep="")
#   
#   reg_ms_nc_npl = paste("reg_ms_nc_npl_",outcome, sep="")
#   reg_ms_nc_npl_cl = paste("reg_ms_nc_npl_cl_",outcome, sep="")
#   reg_ms_c_npl = paste("reg_ms_c_npl_",outcome, sep="")
#   reg_ms_c_npl_cl = paste("reg_ms_c_npl_cl_",outcome, sep="")
#   
#   reg_ms_nc_g = paste("reg_ms_nc_g_",outcome, sep="")
#   reg_ms_nc_cl_g = paste("reg_ms_nc_cl_g_",outcome, sep="")
#   reg_ms_c_g = paste("reg_ms_c_g_",outcome, sep="")
#   reg_ms_c_cl_g = paste("reg_ms_c_cl_g_",outcome, sep="")
#   
#   
#   
#   if (grepl("total_pills_dispensed",outcome) & !(grepl("ring",outcome))) {
#     title = "Pills dispensed"
#   } else if (grepl("total_pats",outcome) & !(grepl("ring",outcome))) {
#     title = "Unique patients"
#   } else if (grepl("pills_per_person",outcome)) {
#     title = "Pills per Patient"
#   } else if (grepl("total_claims",outcome) & !(grepl("ring",outcome)) & !(grepl("inzip",outcome))){
#     title = "Total claims"
#   } else if (grepl("ring_mean_total_pills_dispensed",outcome)) {
#     title = "Ring Pills dispensed"
#   } else if (grepl("ring_mean_total_pats",outcome)) {
#     title = "Ring Unique patients"
#   } else if (grepl("ring_mean_total_claims",outcome)){
#     title = "Ring Total claims"
#   } else if (grepl("total_money",outcome)){
#     title = "Total money"
#   } else if (grepl("avg_age",outcome)) {
#     title = "Average age"
#   } else if (grepl("avg_copay",outcome)){
#     title = "Average copay"
#   } else if (grepl("total_new_pats",outcome)){
#     title = "New patients"
#   } else if (grepl("mean_mpr",outcome)){
#     title = "Med. Possesion Ratio"
#   } else if (grepl("share_medicaid",outcome)){
#     title = "Medicaid Share of Patients"
#   } else if (grepl("share_claims_inzip",outcome)) {
#     title = "Share of in-zip claims"
#   } else if (grepl("total_claims_inzip",outcome)) {
#     title = "Total in-zip claims"
#   } else if (grepl("total_unique_zips",outcome)) {
#     title = "Unique Pharmacy Zips"
#   } else if (grepl("log_avg_distance",outcome)) {
#     title = "Log Avg. Distance"
#   } else if (grepl("avg_distance",outcome)) {
#     title = "Avg. Distance"
#   }
#   
#   if (grepl("z_1", outcome)) {
#     title = paste(title,"\n(1-5 mi ring)",sep="")
#   } else if (grepl("z_6", outcome)) {
#     title = paste(title,"\n(6-10 mi ring)",sep="")
#   } else if (grepl("z_11", outcome)) {
#     title = paste(title,"\n(11-15 mi ring)",sep="")
#   } else if (grepl("z_16", outcome)) {
#     title = paste(title,"\n(16-20 mi ring)",sep="")
#   } else if (grepl("z_21", outcome)) {
#     title = paste(title,"\n(21-25 mi ring)",sep="")
#   } else if (grepl("z_26", outcome)) {
#     title = paste(title,"\n(26-30 mi ring)",sep="")
#   }
#   
#   print(outcome)
#   
#   ####histogram of raw outcome####
#   if (outcome %in% c("mean_mpr_ym_z","share_medicaid_ym_z","share_claims_inzip_ym_z")) {
#     hist = ggplot(data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]) +
#       geom_histogram(aes(x=get(outcome))) +
#       theme_bw() +  xlab("") +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title,"\n(raw)",sep="")) 
#     print(hist)
#     
#     hist = ggplot(data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]) +
#       geom_histogram(aes(x=log(get(outcome)))) +
#       theme_bw() +  xlab("") +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title,"\n(log)",sep="")) 
#     print(hist)
#   } else {
#     hist = ggplot(data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]) +
#       geom_histogram(aes(x=exp(get(outcome)))) +
#       theme_bw() +  xlab("") +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title,"\n(raw)",sep="")) 
#     print(hist)
#     
#     hist = ggplot(data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]) +
#       geom_histogram(aes(x=get(outcome))) +
#       theme_bw() +  xlab("") +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title,"\n(log)",sep="")) 
#     print(hist)
#   }
#   
#   
#   ####no controls####
#   assign(eval(reg_ms_nc) , lm(get(outcome) ~ 
#                                 ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0 
#                               + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6 
#                               + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                               ,
#                               #                               #weights =weight,
#                               data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & zip_ym_index == 1 & usable ==1,]))
#   ##NOTE: zip_ms_index is created AFTER the stack so it pulls out zips with multiple openings in the same month
#   ##but zip_ym_index was created before the stack so it only removes obs with other ins types or rxclasses
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_nc_cl) , coeftest(get(reg_ms_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= B & ms >= -B & opener_zip_dummy ==1 & zip_ym_index == 1 & usable ==1,]$ZIP)))
#   
#   print(get(reg_ms_nc_cl)[1:(2*B+1),])
#   print(nobs(get(reg_ms_nc)))
#   
#   coef_vector = c(get(reg_ms_nc_cl)[2:B,1], 0, get(reg_ms_nc_cl)[(B+1):(B*2+1),1])
#   se_vector = c(get(reg_ms_nc_cl)[2:B,2], NA, get(reg_ms_nc_cl)[(B+1):(B*2+1),2])
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   
#   ####plot###
#   plot_nc = ggplot(data = NULL) +
#     geom_point(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B))),size = 8) +
#     geom_line(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B)))) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=c(seq(-B,-2),-1,seq(0,B))),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   ####with controls####
#   assign(eval(reg_ms_c) , lm(get(outcome) ~ 
#                                ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0 
#                              + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6 
#                              + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                              + avg_copay_ym_z + avg_age_ym_z
#                              #                              + total_health_estabs + total_estabs
#                              + tot_pharms_neg_1
#                              + mail_order_users_per_tot_pats_neg_1
#                              + share_medicaid_neg_1
#                              ,
#                              #weights =weight,
#                              data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_c_cl) , coeftest(get(reg_ms_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]$ZIP)))
#   
#   print(get(reg_ms_c_cl)[1:(B*2+1),])
#   print(nobs(get(reg_ms_c)))
#   
#   coef_vector = c(get(reg_ms_c_cl)[2:B,1], 0, get(reg_ms_c_cl)[(B+1):(B*2+1),1])
#   se_vector = c(get(reg_ms_c_cl)[2:B,2], NA, get(reg_ms_c_cl)[(B+1):(B*2+1),2])
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   
#   ####plot###
#   plot_c = ggplot(data = NULL) +
#     geom_point(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B))),size = 8) +
#     geom_line(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B)))) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=c(seq(-B,-2),-1,seq(0,B))),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(with controls)",sep=""))
#   print(plot_c)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   ####use all zip codes as controls####
#   ####no controls####
#   assign(eval(reg_ms_nc_all) , lm(get(outcome) ~ 
#                                     ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0 
#                                   + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6 
#                                   + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                   ,
#                                   #weights =weight,
#                                   data = zip_data_stacked[ ms <= B & ms >= -B &  zip_ym_index == 1 & (usable == 1 | non_opener == 1),]))
#   ##NOTE: zip_ms_index is created AFTER the stack so it pulls out zips with multiple openings in the same month
#   ##but zip_ym_index was created before the stack so it only removes obs with other ins types or rxclasses
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_nc_all_cl) , coeftest(get(reg_ms_nc_all), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= B & ms >= -B & zip_ym_index == 1 & (usable == 1 | non_opener == 1),]$ZIP)))
#   
#   print(get(reg_ms_nc_all_cl)[1:(2*B+1),])
#   print(nobs(get(reg_ms_nc_all)))
#   
#   coef_vector = c(get(reg_ms_nc_all_cl)[2:B,1], 0, get(reg_ms_nc_all_cl)[(B+1):(B*2+1),1])
#   se_vector = c(get(reg_ms_nc_all_cl)[2:B,2], NA, get(reg_ms_nc_all_cl)[(B+1):(B*2+1),2])
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   
#   ####plot###
#   plot_nc = ggplot(data = NULL) +
#     geom_point(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B))),size = 8) +
#     geom_line(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B)))) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=c(seq(-B,-2),-1,seq(0,B))),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\nall zip codes",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   ####with controls####
#   assign(eval(reg_ms_c_all) , lm(get(outcome) ~ 
#                                    ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0 
#                                  + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6 
#                                  + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                  + avg_copay_ym_z + avg_age_ym_z
#                                  #                              + total_health_estabs + total_estabs
#                                  + tot_pharms_neg_1
#                                  + mail_order_users_per_tot_pats_neg_1
#                                  + share_medicaid_neg_1
#                                  ,
#                                  #weights =weight,
#                                  data = zip_data_stacked[ ms <= B & ms >= -B &  zip_ym_index == 1 & (usable == 1 | non_opener == 1),]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_c_all_cl) , coeftest(get(reg_ms_c_all), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= B & ms >= -B & zip_ym_index == 1 & (usable == 1 | non_opener == 1),]$ZIP)))
#   
#   print(get(reg_ms_c_all_cl)[1:(B*2+1),])
#   print(nobs(get(reg_ms_c_all)))
#   
#   coef_vector = c(get(reg_ms_c_all_cl)[2:B,1], 0, get(reg_ms_c_all_cl)[(B+1):(B*2+1),1])
#   se_vector = c(get(reg_ms_c_all_cl)[2:B,2], NA, get(reg_ms_c_all_cl)[(B+1):(B*2+1),2])
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   
#   ####plot###
#   plot_c = ggplot(data = NULL) +
#     geom_point(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B))),size = 8) +
#     geom_line(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B)))) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=c(seq(-B,-2),-1,seq(0,B))),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(with controls)\nall zip codes",sep=""))
#   print(plot_c)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   
#   
#   ####only pharmacies that have grocery stores no controls####
#   assign(eval(reg_ms_nc_g) , lm(get(outcome) ~ 
#                                   ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0 
#                                 + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6 
#                                 + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                 ,
#                                 #weights =weight,
#                                 data = zip_data_stacked[ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & !is.na(grocery) & zip_ym_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_nc_cl_g) , coeftest(get(reg_ms_nc_g), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & !is.na(grocery) & zip_ym_index == 1,]$ZIP)))
#   
#   print(get(reg_ms_nc_cl_g)[1:(B*2+1),])
#   print(nobs(get(reg_ms_nc_g)))
#   
#   coef_vector = c(get(reg_ms_nc_cl_g)[2:B,1], 0, get(reg_ms_nc_cl_g)[(B+1):(B*2+1),1])
#   se_vector = c(get(reg_ms_nc_cl_g)[2:B,2], NA, get(reg_ms_nc_cl_g)[(B+1):(B*2+1),2])
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   ####plot###
#   plot_nc_g = ggplot(data = NULL) +
#     geom_point(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B))),size = 8) +
#     geom_line(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B)))) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=c(seq(-B,-2),-1,seq(0,B))),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls, grocery only)",sep=""))
#   print(plot_nc_g)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   ####only pharmacies that have grocery stores with controls####
#   assign(eval(reg_ms_c_g) , lm(get(outcome) ~ 
#                                  ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0 
#                                + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6 
#                                + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                + avg_copay_ym_z + avg_age_ym_z
#                                #                                + total_health_estabs + total_estabs
#                                + tot_pharms_neg_1
#                                + mail_order_users_per_tot_pats_neg_1
#                                + share_medicaid_neg_1
#                                ,
#                                #weights =weight,
#                                data = zip_data_stacked[ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & !is.na(grocery) & zip_ym_index == 1,]))
#   
#   ##WHY ARE SOME EVENTS MISSING MONTHS SINCE??
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_c_cl_g) , coeftest(get(reg_ms_c_g), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & !is.na(grocery) & zip_ym_index == 1,]$ZIP)))
#   
#   print(get(reg_ms_c_cl_g)[1:(B*2+1),])
#   print(nobs(get(reg_ms_c_g)))
#   
#   coef_vector = c(get(reg_ms_c_cl_g)[2:B,1], 0, get(reg_ms_c_cl_g)[(B+1):(B*2+1),1])
#   se_vector = c(get(reg_ms_c_cl_g)[2:B,2], NA, get(reg_ms_c_cl_g)[(B+1):(B*2+1),2])
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   ####plot###
#   plot_c_g = ggplot(data = NULL) +
#     geom_point(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B))),size = 8) +
#     geom_line(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B)))) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=c(seq(-B,-2),-1,seq(0,B))),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(with controls, grocery only)",sep=""))
#   print(plot_c_g)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   ####no controls - only those two that went from 0 to 1####
#   zip_data_stacked[, deserts := ifelse(ZIP %in% c(97370, 97818), 1, 0)]
#   assign(eval(reg_ms_nc) , lm(get(outcome) ~ 
#                                 deserts:(ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0 
#                                          + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6)
#                               + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                               ,
#                               #                               #weights =weight,
#                               data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & zip_ym_index == 1 & usable ==1,]))
#   ##NOTE: zip_ms_index is created AFTER the stack so it pulls out zips with multiple openings in the same month
#   ##but zip_ym_index was created before the stack so it only removes obs with other ins types or rxclasses
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_nc_cl) , coeftest(get(reg_ms_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= B & ms >= -B & opener_zip_dummy ==1 & zip_ym_index == 1 & usable ==1,]$ZIP)))
#   
#   print(get(reg_ms_nc_cl)[1:(2*B+1),])
#   print(nobs(get(reg_ms_nc)))
#   
#   print(summary(get(reg_ms_nc)))
#   
#   names_vector = names(coef( summary(get(reg_ms_nc)))[,1])[grepl("ms_",names(coef( summary(get(reg_ms_nc)))[,1]))]
#   coef_vector = coef( summary(get(reg_ms_nc)))[,1][names_vector]
#   se_vector = coef( summary(get(reg_ms_nc)))[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*_","",names) ]
#   coef_data[, ms := as.numeric(gsub("neg","-",ms)) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\n(0 Initial Pharmacies)",sep="")) 
#   
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   
#   #####CLOSINGS#####
#   ####no controls####
#   assign(eval(reg_msc_nc) , lm(get(outcome) ~ 
#                                  msc_neg6 + msc_neg5 + msc_neg4 + msc_neg3 + msc_neg2 + msc_0 
#                                + msc_1 + msc_2 + msc_3 + msc_4 + msc_5 + msc_6 
#                                + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                ,
#                                #                               #weights =weight,
#                                data = zip_data_stacked_closings[ msc <= B & msc >= -B & closer_zip_dummy ==1 & zip_ym_index == 1 & usable_c ==1,]))
#   ##NOTE: zip_msc_index is created AFTER the stack so it pulls out zips with multiple openings in the same month
#   ##but zip_ym_index was created before the stack so it only removes obs with other ins types or rxclasses
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_msc_nc_cl) , coeftest(get(reg_msc_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked_closings[msc <= B & msc >= -B & closer_zip_dummy ==1 & zip_ym_index == 1 & usable_c ==1,]$ZIP)))
#   
#   print(get(reg_msc_nc_cl)[1:(2*B+1),])
#   print(nobs(get(reg_msc_nc)))
#   
#   coef_vector = c(get(reg_msc_nc_cl)[2:B,1], 0, get(reg_msc_nc_cl)[(B+1):(B*2+1),1])
#   se_vector = c(get(reg_msc_nc_cl)[2:B,2], NA, get(reg_msc_nc_cl)[(B+1):(B*2+1),2])
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   
#   ####plot###
#   plot_nc = ggplot(data = NULL) +
#     geom_point(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B))),size = 8) +
#     geom_line(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B)))) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=c(seq(-B,-2),-1,seq(0,B))),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Closing") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   ####with controls####
#   assign(eval(reg_msc_c) , lm(get(outcome) ~ 
#                                 msc_neg6 + msc_neg5 + msc_neg4 + msc_neg3 + msc_neg2 + msc_0 
#                               + msc_1 + msc_2 + msc_3 + msc_4 + msc_5 + msc_6 
#                               + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                               + avg_copay_ym_z + avg_age_ym_z
#                               #                              + total_health_estabs + total_estabs
#                               + tot_pharms_neg_1c
#                               + mail_order_users_per_tot_pats_neg_1c
#                               + share_medicaid_neg_1c
#                               ,
#                               #weights =weight,
#                               data = zip_data_stacked_closings[ msc <= B & msc >= -B & closer_zip_dummy ==1 & usable_c ==1 & zip_ym_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_msc_c_cl) , coeftest(get(reg_msc_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked_closings[msc <= B & msc >= -B & closer_zip_dummy ==1 & usable_c ==1 & zip_ym_index == 1,]$ZIP)))
#   
#   print(get(reg_msc_c_cl)[1:(B*2+1),])
#   print(nobs(get(reg_msc_c)))
#   
#   coef_vector = c(get(reg_msc_c_cl)[2:B,1], 0, get(reg_msc_c_cl)[(B+1):(B*2+1),1])
#   se_vector = c(get(reg_msc_c_cl)[2:B,2], NA, get(reg_msc_c_cl)[(B+1):(B*2+1),2])
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   
#   ####plot###
#   plot_c = ggplot(data = NULL) +
#     geom_point(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B))),size = 8) +
#     geom_line(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B)))) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=c(seq(-B,-2),-1,seq(0,B))),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Closing") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(with controls)",sep=""))
#   print(plot_c)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   ####use all zip codes as controls####
#   ####no controls####
#   assign(eval(reg_msc_nc_all) , lm(get(outcome) ~ 
#                                      msc_neg6 + msc_neg5 + msc_neg4 + msc_neg3 + msc_neg2 + msc_0 
#                                    + msc_1 + msc_2 + msc_3 + msc_4 + msc_5 + msc_6 
#                                    + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                    ,
#                                    #weights =weight,
#                                    data = zip_data_stacked_closings[ msc <= B & msc >= -B &  zip_ym_index == 1 & (usable_c == 1 | non_closer == 1),]))
#   ##NOTE: zip_msc_index is created AFTER the stack so it pulls out zips with multiple openings in the same month
#   ##but zip_ym_index was created before the stack so it only removes obs with other ins types or rxclasses
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_msc_nc_all_cl) , coeftest(get(reg_msc_nc_all), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked_closings[msc <= B & msc >= -B & zip_ym_index == 1 & (usable_c == 1 | non_closer == 1),]$ZIP)))
#   
#   print(get(reg_msc_nc_all_cl)[1:(2*B+1),])
#   print(nobs(get(reg_msc_nc_all)))
#   
#   coef_vector = c(get(reg_msc_nc_all_cl)[2:B,1], 0, get(reg_msc_nc_all_cl)[(B+1):(B*2+1),1])
#   se_vector = c(get(reg_msc_nc_all_cl)[2:B,2], NA, get(reg_msc_nc_all_cl)[(B+1):(B*2+1),2])
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   
#   ####plot###
#   plot_nc = ggplot(data = NULL) +
#     geom_point(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B))),size = 8) +
#     geom_line(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B)))) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=c(seq(-B,-2),-1,seq(0,B))),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Closing") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\nall zip codes",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   ####with controls####
#   assign(eval(reg_msc_c_all) , lm(get(outcome) ~ 
#                                     msc_neg6 + msc_neg5 + msc_neg4 + msc_neg3 + msc_neg2 + msc_0 
#                                   + msc_1 + msc_2 + msc_3 + msc_4 + msc_5 + msc_6 
#                                   + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                   + avg_copay_ym_z + avg_age_ym_z
#                                   #                              + total_health_estabs + total_estabs
#                                   + tot_pharms_neg_1c
#                                   + mail_order_users_per_tot_pats_neg_1c
#                                   + share_medicaid_neg_1c
#                                   ,
#                                   #weights =weight,
#                                   data = zip_data_stacked_closings[ msc <= B & msc >= -B &  zip_ym_index == 1 & (usable_c == 1 | non_closer == 1),]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_msc_c_all_cl) , coeftest(get(reg_msc_c_all), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked_closings[msc <= B & msc >= -B & zip_ym_index == 1 & (usable_c == 1 | non_closer == 1),]$ZIP)))
#   
#   print(get(reg_msc_c_all_cl)[1:(B*2+1),])
#   print(nobs(get(reg_msc_c_all)))
#   
#   coef_vector = c(get(reg_msc_c_all_cl)[2:B,1], 0, get(reg_msc_c_all_cl)[(B+1):(B*2+1),1])
#   se_vector = c(get(reg_msc_c_all_cl)[2:B,2], NA, get(reg_msc_c_all_cl)[(B+1):(B*2+1),2])
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   
#   ####plot###
#   plot_c = ggplot(data = NULL) +
#     geom_point(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B))),size = 8) +
#     geom_line(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B)))) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=c(seq(-B,-2),-1,seq(0,B))),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Closing") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(with controls)\nall zip codes",sep=""))
#   print(plot_c)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   ####no controls - only those two that went from 0 to 1####
#   zip_data_stacked_closings[, deserts := ifelse(ZIP %in% c(97064, 97818), 1, 0)]
#   assign(eval(reg_msc_nc) , lm(get(outcome) ~ 
#                                  deserts:(msc_neg6 + msc_neg5 + msc_neg4 + msc_neg3 + msc_neg2 + msc_0 
#                                           + msc_1 + msc_2 + msc_3 + msc_4 + msc_5 + msc_6)
#                                + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                ,
#                                #                               #weights =weight,
#                                data = zip_data_stacked_closings[ msc <= B & msc >= -B & closer_zip_dummy ==1 & zip_ym_index == 1 & usable_c ==1,]))
#   ##NOTE: zip_ms_index is created AFTER the stack so it pulls out zips with multiple openings in the same month
#   ##but zip_ym_index was created before the stack so it only removes obs with other ins types or rxclasses
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_msc_nc_cl) , coeftest(get(reg_msc_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked_closings[msc <= B & msc >= -B & closer_zip_dummy ==1 & zip_ym_index == 1 & usable_c ==1,]$ZIP)))
#   
#   print(get(reg_msc_nc_cl)[1:(2*B+1),])
#   print(nobs(get(reg_msc_nc)))
#   
#   print(summary(get(reg_msc_nc)))
#   
#   names_vector = names(coef( summary(get(reg_msc_nc)))[,1])[grepl("msc_",names(coef( summary(get(reg_msc_nc)))[,1]))]
#   coef_vector = coef( summary(get(reg_msc_nc)))[,1][names_vector]
#   se_vector = coef( summary(get(reg_msc_nc)))[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*_","",names) ]
#   coef_data[, ms := as.numeric(gsub("neg","-",ms)) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\n(0 Ending Pharmacies)",sep="")) 
#   
#   print(plot_nc)
#   
#   
#   
#   
# }
# dev.off()
# # rm(list= ls()[!(ls() %in% c("zip_data_stacked","rxnkawz","pat_data_stacked","B"))])
# 
# 
# ####only for existing patients####
# ####months since regs####
# pdf(file = paste("~/current/pharmacy_deserts/event_study_exist_no_weights_",B,".pdf",sep=""), width = 18, height = 10)
# for (outcome in c("log_total_pills_dispensed_ym_z_exist","log_total_pats_ym_z_exist",
#                   "log_total_claims_ym_z_exist","log_pills_per_person_ym_z_exist"
# )) {
#   
#   #   outcome = "log_pills_per_person_ym_z"
#   reg_ms_nc = paste("reg_ms_nc_",outcome, sep="")
#   reg_ms_nc_cl = paste("reg_ms_nc_cl_",outcome, sep="")
#   reg_ms_c = paste("reg_ms_c_",outcome, sep="")
#   reg_ms_c_cl = paste("reg_ms_c_cl_",outcome, sep="")
#   
#   reg_ms_nc_all = paste("reg_ms_nc_all_",outcome, sep="")
#   reg_ms_nc_all_cl = paste("reg_ms_nc_all_cl_",outcome, sep="")
#   reg_ms_c_all = paste("reg_ms_c_all_",outcome, sep="")
#   reg_ms_c_all_cl = paste("reg_ms_c_all_cl_",outcome, sep="")
#   
#   reg_msc_nc = paste("reg_msc_nc_",outcome, sep="")
#   reg_msc_nc_cl = paste("reg_msc_nc_cl_",outcome, sep="")
#   reg_msc_c = paste("reg_msc_c_",outcome, sep="")
#   reg_msc_c_cl = paste("reg_msc_c_cl_",outcome, sep="")
#   
#   reg_msc_nc_all = paste("reg_msc_nc_all_",outcome, sep="")
#   reg_msc_nc_all_cl = paste("reg_msc_nc_all_cl_",outcome, sep="")
#   reg_msc_c_all = paste("reg_msc_c_all_",outcome, sep="")
#   reg_msc_c_all_cl = paste("reg_msc_c_all_cl_",outcome, sep="")
#   
#   reg_ms_nc_ppl = paste("reg_ms_nc_ppl_",outcome, sep="")
#   reg_ms_nc_ppl_cl = paste("reg_ms_nc_ppl_cl_",outcome, sep="")
#   reg_ms_c_ppl = paste("reg_ms_c_ppl_",outcome, sep="")
#   reg_ms_c_ppl_cl = paste("reg_ms_c_ppl_cl_",outcome, sep="")
#   
#   reg_ms_nc_npl = paste("reg_ms_nc_npl_",outcome, sep="")
#   reg_ms_nc_npl_cl = paste("reg_ms_nc_npl_cl_",outcome, sep="")
#   reg_ms_c_npl = paste("reg_ms_c_npl_",outcome, sep="")
#   reg_ms_c_npl_cl = paste("reg_ms_c_npl_cl_",outcome, sep="")
#   
#   reg_ms_nc_g = paste("reg_ms_nc_g_",outcome, sep="")
#   reg_ms_nc_cl_g = paste("reg_ms_nc_cl_g_",outcome, sep="")
#   reg_ms_c_g = paste("reg_ms_c_g_",outcome, sep="")
#   reg_ms_c_cl_g = paste("reg_ms_c_cl_g_",outcome, sep="")
#   
#   
#   
#   if (grepl("total_pills_dispensed",outcome) & !(grepl("ring",outcome))) {
#     title = "Pills dispensed"
#   } else if (grepl("total_pats",outcome) & !(grepl("ring",outcome))) {
#     title = "Unique patients"
#   } else if (grepl("pills_per_person",outcome)) {
#     title = "Pills per Patient"
#   } else if (grepl("total_claims",outcome) & !(grepl("ring",outcome)) & !(grepl("inzip",outcome))){
#     title = "Total claims"
#   } else if (grepl("ring_mean_total_pills_dispensed",outcome)) {
#     title = "Ring Pills dispensed"
#   } else if (grepl("ring_mean_total_pats",outcome)) {
#     title = "Ring Unique patients"
#   } else if (grepl("ring_mean_total_claims",outcome)){
#     title = "Ring Total claims"
#   } else if (grepl("total_money",outcome)){
#     title = "Total money"
#   } else if (grepl("avg_age",outcome)) {
#     title = "Average age"
#   } else if (grepl("avg_copay",outcome)){
#     title = "Average copay"
#   } else if (grepl("total_new_pats",outcome)){
#     title = "New patients"
#   } else if (grepl("mean_mpr",outcome)){
#     title = "Med. Possesion Ratio"
#   } else if (grepl("share_medicaid",outcome)){
#     title = "Medicaid Share of Patients"
#   } else if (grepl("share_claims_inzip",outcome)) {
#     title = "Share of in-zip claims"
#   } else if (grepl("total_claims_inzip",outcome)) {
#     title = "Total in-zip claims"
#   } else if (grepl("total_unique_zips",outcome)) {
#     title = "Unique Pharmacy Zips"
#   } else if (grepl("log_avg_distance",outcome)) {
#     title = "Log Avg. Distance"
#   } else if (grepl("avg_distance",outcome)) {
#     title = "Avg. Distance"
#   }
#   
#   if (grepl("z_1", outcome)) {
#     title = paste(title,"\n(1-5 mi ring)",sep="")
#   } else if (grepl("z_6", outcome)) {
#     title = paste(title,"\n(6-10 mi ring)",sep="")
#   } else if (grepl("z_11", outcome)) {
#     title = paste(title,"\n(11-15 mi ring)",sep="")
#   } else if (grepl("z_16", outcome)) {
#     title = paste(title,"\n(16-20 mi ring)",sep="")
#   } else if (grepl("z_21", outcome)) {
#     title = paste(title,"\n(21-25 mi ring)",sep="")
#   } else if (grepl("z_26", outcome)) {
#     title = paste(title,"\n(26-30 mi ring)",sep="")
#   }
#   
#   print(outcome)
#   
#   ####histogram of raw outcome####
#   if (outcome %in% c("mean_mpr_ym_z","share_medicaid_ym_z","share_claims_inzip_ym_z")) {
#     hist = ggplot(data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]) +
#       geom_histogram(aes(x=get(outcome))) +
#       theme_bw() +  xlab("") +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title,"\n(raw)",sep="")) 
#     print(hist)
#     
#     hist = ggplot(data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]) +
#       geom_histogram(aes(x=log(get(outcome)))) +
#       theme_bw() +  xlab("") +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title,"\n(log)",sep="")) 
#     print(hist)
#   } else {
#     hist = ggplot(data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]) +
#       geom_histogram(aes(x=exp(get(outcome)))) +
#       theme_bw() +  xlab("") +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title,"\n(raw)",sep="")) 
#     print(hist)
#     
#     hist = ggplot(data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]) +
#       geom_histogram(aes(x=get(outcome))) +
#       theme_bw() +  xlab("") +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title,"\n(log)",sep="")) 
#     print(hist)
#   }
#   
#   
#   ####no controls####
#   assign(eval(reg_ms_nc) , lm(get(outcome) ~ 
#                                 ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0 
#                               + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6 
#                               + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                               ,
#                               #                               #weights =weight,
#                               data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & zip_ym_index == 1 & usable ==1,]))
#   ##NOTE: zip_ms_index is created AFTER the stack so it pulls out zips with multiple openings in the same month
#   ##but zip_ym_index was created before the stack so it only removes obs with other ins types or rxclasses
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_nc_cl) , coeftest(get(reg_ms_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= B & ms >= -B & opener_zip_dummy ==1 & zip_ym_index == 1 & usable ==1,]$ZIP)))
#   
#   print(get(reg_ms_nc_cl)[1:(2*B+1),])
#   print(nobs(get(reg_ms_nc)))
#   
#   coef_vector = c(get(reg_ms_nc_cl)[2:B,1], 0, get(reg_ms_nc_cl)[(B+1):(B*2+1),1])
#   se_vector = c(get(reg_ms_nc_cl)[2:B,2], NA, get(reg_ms_nc_cl)[(B+1):(B*2+1),2])
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   
#   ####plot###
#   plot_nc = ggplot(data = NULL) +
#     geom_point(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B))),size = 8) +
#     geom_line(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B)))) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=c(seq(-B,-2),-1,seq(0,B))),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   ####with controls####
#   assign(eval(reg_ms_c) , lm(get(outcome) ~ 
#                                ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0 
#                              + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6 
#                              + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                              + avg_copay_ym_z + avg_age_ym_z
#                              #                              + total_health_estabs + total_estabs
#                              + tot_pharms_neg_1
#                              + mail_order_users_per_tot_pats_neg_1
#                              + share_medicaid_neg_1
#                              ,
#                              #weights =weight,
#                              data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_c_cl) , coeftest(get(reg_ms_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]$ZIP)))
#   
#   print(get(reg_ms_c_cl)[1:(B*2+1),])
#   print(nobs(get(reg_ms_c)))
#   
#   coef_vector = c(get(reg_ms_c_cl)[2:B,1], 0, get(reg_ms_c_cl)[(B+1):(B*2+1),1])
#   se_vector = c(get(reg_ms_c_cl)[2:B,2], NA, get(reg_ms_c_cl)[(B+1):(B*2+1),2])
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   
#   ####plot###
#   plot_c = ggplot(data = NULL) +
#     geom_point(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B))),size = 8) +
#     geom_line(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B)))) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=c(seq(-B,-2),-1,seq(0,B))),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(with controls)",sep=""))
#   print(plot_c)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   
#   
#   
#   
# }
# dev.off()
# 
# ####months since regs control for rx2####
# 
# pdf(file = paste("~/current/pharmacy_deserts/event_study_rx2_control_no_weights_",B,".pdf",sep=""), width = 18, height = 10)
# for (outcome in c("log_total_pills_dispensed_ym_rx_z","log_total_pats_ym_rx_z","log_total_claims_ym_rx_z",
#                   "log_avg_age_ym_rx_z","log_avg_copay_ym_rx_z_plus_1", "log_total_new_pats_ym_rx_z_plus_1",
#                   "avg_distance_ym_rx_z","log_avg_distance_ym_rx_z_plus_1")) {
#   #     outcome = "log_total_pills_dispensed_ym_rx_z"
#   
#   reg_ms_nc = paste("reg_ms_nc_",outcome, sep="")
#   reg_ms_nc_cl = paste("reg_ms_nc_cl_",outcome, sep="")
#   reg_ms_c = paste("reg_ms_c_",outcome, sep="")
#   reg_ms_c_cl = paste("reg_ms_c_cl_",outcome, sep="")
#   
#   reg_ms_nc_g = paste("reg_ms_nc_g_",outcome, sep="")
#   reg_ms_nc_cl_g = paste("reg_ms_nc_cl_g_",outcome, sep="")
#   reg_ms_c_g = paste("reg_ms_c_g_",outcome, sep="")
#   reg_ms_c_cl_g = paste("reg_ms_c_cl_g_",outcome, sep="")
#   
#   if (grepl("total_pills_dispensed",outcome)) {
#     title = "Pills dispensed"
#   } else if (grepl("total_pats",outcome)) {
#     title = "Unique patients"
#   } else if (grepl("total_claims",outcome)){
#     title = "Total claims"
#   } else if (grepl("total_money",outcome)){
#     title = "Total money"
#   } else if (grepl("avg_age",outcome)) {
#     title = "Average age"
#   } else if (grepl("avg_copay",outcome)){
#     title = "Average copay"
#   } else if (grepl("total_new_pats",outcome)){
#     title = "New patients"
#   } else if (grepl("mean_mpr_ym",outcome) & !grepl("secondary", outcome)){
#     title = "Med. Possesion Ratio"
#   } else if (grepl("mean_mpr_ym_rx_z_secondary_chronic",outcome)){
#     title = "Med. Possesion Ratio (chronic)"
#   } else if (grepl("mean_mpr_ym_rx_z_secondary",outcome)){
#     title = "Med. Possesion Ratio (secondary)"
#   }  else if (grepl("log_avg_distance",outcome)) {
#     title = "Log Avg. Distance"
#   } else if (grepl("avg_distance",outcome)) {
#     title = "Avg. Distance"
#   }
#   
#   ####histogram of outcome####
#   hist = ggplot(data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_rx_index == 1,]) +
#     geom_histogram(aes(x=get(outcome))) +
#     theme_bw() +  xlab("") +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title,"",sep="")) 
#   print(hist)
#   
#   
#   ####no controls####
#   assign(eval(reg_ms_nc) , lm(get(outcome) ~ 
#                                 ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0 + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6
#                               + zip_factor + zip_factor:yrmon_number + yrmon_factor + as.factor(rx2)
#                               ,
#                               #weights =weight,
#                               data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                                       & is.finite(get(outcome))
#                                                       & opener_zip_dummy ==1 & usable ==1 & zip_ym_rx_index == 1 ,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_nc_cl) , coeftest(get(reg_ms_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ ms <= 6 & ms >= -6 
#                                      & opener_zip_dummy ==1 
#                                      & is.finite(get(outcome))
#                                      & usable ==1 
#                                      & zip_ym_rx_index == 1 ,]$ZIP)))
#   
#   print(get(reg_ms_nc_cl)[1:13,])
#   print(nobs(get(reg_ms_nc)))
#   
#   coef_vector = c(get(reg_ms_nc_cl)[2:6,1], 0, get(reg_ms_nc_cl)[7:13,1])
#   se_vector = c(get(reg_ms_nc_cl)[2:6,2], NA, get(reg_ms_nc_cl)[7:13,2])
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   ####plot###
#   plot_nc = ggplot(data = NULL) +
#     geom_point(aes(y=coef_vector,x=c(seq(-6,-2),-1,seq(0,6))),size = 8) +
#     geom_line(aes(y=coef_vector,x=c(seq(-6,-2),-1,seq(0,6)))) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=c(seq(-6,-2),-1,seq(0,6))),width = .1) +
#     theme_bw() + ylab("Coefficient") + xlab("Months Since Opening") +
#     scale_x_continuous(breaks = seq(-6,6,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)",sep=""))
#   print(plot_nc)
#   
#   
#   ####with controls####
#   assign(eval(reg_ms_c) , lm(get(outcome) ~ 
#                                ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0 + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6
#                              + zip_factor + zip_factor:yrmon_number + yrmon_factor  + as.factor(rx2)
#                              + avg_copay_ym_rx_z + avg_age_ym_rx_z
#                              #                              + total_health_estabs + total_estabs
#                              + tot_pharms_neg_1
#                              + mail_order_users_per_tot_pats_neg_1
#                              + share_medicaid_neg_1
#                              ,
#                              #weights =weight,
#                              data = zip_data_stacked[ ms <= 6 & ms >= -6 
#                                                       & opener_zip_dummy ==1
#                                                       & is.finite(get(outcome))
#                                                       & usable ==1 & zip_ym_rx_index == 1 ,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_c_cl) , coeftest(get(reg_ms_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ ms <= 6 & ms >= -6 
#                                      & opener_zip_dummy ==1 
#                                      & is.finite(get(outcome))
#                                      & usable ==1 & zip_ym_rx_index == 1 ,]$ZIP)))
#   
#   print(get(reg_ms_c_cl)[1:13,])
#   print(nobs(get(reg_ms_c)))
#   
#   coef_vector = c(get(reg_ms_c_cl)[2:6,1], 0, get(reg_ms_c_cl)[7:13,1])
#   se_vector = c(get(reg_ms_c_cl)[2:6,2], NA, get(reg_ms_c_cl)[7:13,2])
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   ####plot###
#   plot_c = ggplot(data = NULL) +
#     geom_point(aes(y=coef_vector,x=c(seq(-6,-2),-1,seq(0,6))),size = 8) +
#     geom_line(aes(y=coef_vector,x=c(seq(-6,-2),-1,seq(0,6)))) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=c(seq(-6,-2),-1,seq(0,6))),width = .1) +
#     theme_bw() + ylab("Coefficient") + xlab("Months Since Opening") +
#     scale_x_continuous(breaks = seq(-6,6,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(with controls)",sep=""))
#   print(plot_c)
#   
#   
#   
#   
# }
# dev.off()
# 
# 
# 
# 
# 
# 
# 
# ####months since regs split by rx2####
# 
# pdf(file = paste("~/current/pharmacy_deserts/event_study_rx2_split_no_weights_",B,".pdf",sep=""), width = 18, height = 10)
# 
# 
# 
# for (outcome in c("log_total_pills_dispensed_ym_rx_z","log_total_pats_ym_rx_z","log_total_claims_ym_rx_z",
#                   "log_avg_age_ym_rx_z","log_avg_copay_ym_rx_z_plus_1", "log_total_new_pats_ym_rx_z_plus_1",
#                   "log_avg_distance_ym_rx_z_plus_1")) {
#   
#   pill_type_rx_list_int_nc = list()
#   pill_type_coef_list_int_nc = list()
#   pill_type_se_list_int_nc = list()
#   pill_type_p_list_int_nc = list()
#   
#   pill_type_rx_list_int_c = list()
#   pill_type_coef_list_int_c = list()
#   pill_type_se_list_int_c = list()
#   pill_type_p_list_int_c = list()
#   
#   #       outcome = "log_total_pills_dispensed_ym_rx_z"
#   print (outcome)
#   reg_ms_nc = paste("reg_ms_nc_",outcome, sep="")
#   reg_ms_nc_cl = paste("reg_ms_nc_cl_",outcome, sep="")
#   reg_ms_c = paste("reg_ms_c_",outcome, sep="")
#   reg_ms_c_cl = paste("reg_ms_c_cl_",outcome, sep="")
#   
#   
#   if (grepl("total_pills_dispensed",outcome)) {
#     title = "Pills dispensed"
#   } else if (grepl("total_pats",outcome)) {
#     title = "Unique patients"
#   } else if (grepl("total_claims",outcome)){
#     title = "Total claims"
#   } else if (grepl("total_money",outcome)){
#     title = "Total money"
#   } else if (grepl("avg_age",outcome)) {
#     title = "Average age"
#   } else if (grepl("avg_copay",outcome)){
#     title = "Average copay"
#   } else if (grepl("total_new_pats",outcome)){
#     title = "New patients"
#   } else if (grepl("mean_mpr_ym",outcome) & !grepl("secondary", outcome)){
#     title = "Med. Possesion Ratio"
#   } else if (grepl("mean_mpr_ym_rx_z_secondary_chronic",outcome)){
#     title = "Med. Possesion Ratio (chronic)"
#   } else if (grepl("mean_mpr_ym_rx_z_secondary",outcome) & !grepl("chronic", outcome)){
#     title = "Med. Possesion Ratio (secondary)"
#   } else if (grepl("log_avg_distance",outcome)) {
#     title = "Log Avg. Distance"
#   } else if (grepl("avg_distance",outcome)) {
#     title = "Avg. Distance"
#   }
#   
#   ####histogram of outcome####
#   hist = ggplot(data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                         & opener_zip_dummy ==1 & usable ==1 
#                                         & zip_ym_rx_index == 1,]) +
#     geom_histogram(aes(x=get(outcome))) +
#     theme_bw() +  xlab("") +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title,"(drug type split)",sep="\n")) 
#   print(hist)
#   
#   #hist of raw outcome
#   if (outcome %in% c("mean_mpr_ym_rx_z","mean_mpr_ym_rx_z_secondary_chronic","mean_mpr_ym_rx_z_secondary")) {
#     hist = ggplot(data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_rx_index == 1,]) +
#       geom_histogram(aes(x=get(outcome))) +
#       theme_bw() +  xlab("") +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title,"(drug type split)",sep="\n"))  
#     print(hist)
#   } else {
#     hist = ggplot(data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_rx_index == 1,]) +
#       geom_histogram(aes(x=exp(get(outcome)))) +
#       theme_bw() +  xlab("") +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title,"(drug type split)",sep="\n")) 
#     print(hist)
#   }
#   ####no controls####
#   assign(eval(reg_ms_nc) , lm(get(outcome) ~ 
#                                 rx2:(ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0 + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6)
#                               + rx2
#                               + zip_factor + zip_factor:yrmon_number + yrmon_factor 
#                               ,
#                               #weights =weight,
#                               data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                                       & opener_zip_dummy ==1 & usable ==1
#                                                       & is.finite(get(outcome))
#                                                       & zip_ym_rx_index == 1
#                                                       #                                                         & rx2 == RX
#                                                       ,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_nc_cl) , coeftest(get(reg_ms_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                     & opener_zip_dummy ==1 & usable ==1
#                                     & is.finite(get(outcome))
#                                     & zip_ym_rx_index == 1
#                                     #                                       & rx2 == RX
#                                     ,]$ZIP)))
#   
#   coefs = names(get(reg_ms_nc_cl)[,1])
#   print(get(reg_ms_nc_cl)[grepl("ms",coefs),1])
#   print(nobs(get(reg_ms_nc)))
#   print(coefs[grepl("ms",coefs)])
#   
#   
#   pill_type_rx_list_int_nc = coefs[grepl("ms",coefs)]
#   pill_type_coef_list_int_nc = get(reg_ms_nc_cl)[grepl("ms",coefs),1]
#   pill_type_se_list_int_nc = get(reg_ms_nc_cl)[grepl("ms",coefs),2]
#   pill_type_p_list_int_nc = get(reg_ms_nc_cl)[grepl("ms",coefs),4]
#   
#   rx_ms_split_int_nc = data.table(rx = unlist(pill_type_rx_list_int_nc),
#                                   coef = unlist(pill_type_coef_list_int_nc),
#                                   se = unlist(pill_type_se_list_int_nc),
#                                   p = unlist(pill_type_p_list_int_nc))
#   
#   rx_ms_split_int_nc[, ms := ifelse(grepl("neg6", rx), -6,
#                                     ifelse(grepl("neg5", rx), -5,
#                                            ifelse(grepl("neg4", rx), -4,
#                                                   ifelse(grepl("neg3", rx), -3,
#                                                          ifelse(grepl("neg2", rx), -2,
#                                                                 ifelse(grepl("_0", rx), 0,
#                                                                        ifelse(grepl("_1", rx), 1,
#                                                                               ifelse(grepl("_2", rx), 2,
#                                                                                      ifelse(grepl("_3", rx), 3,
#                                                                                             ifelse(grepl("_4", rx), 4,
#                                                                                                    ifelse(grepl("_5", rx), 5,
#                                                                                                           ifelse(grepl("_6", rx), 6,
#                                                                                                                  NA))))))))))))]
#   rx_ms_split_int_nc[,drug := as.character(as.numeric(substr(rx,4,5)))]
#   
#   rx_ms_split_int_nc[, ymin := coef - 1.96 * se]
#   rx_ms_split_int_nc[, ymax := coef + 1.96 * se]
#   
#   #merge on the rxclass label
#   rxclass_label = as.data.table(read.csv("~/current/pharmacy_deserts/data/rxclass_labels.csv"))
#   rxclass_label[, drug := as.character(rx2)]
#   setkey(rxclass_label, drug)
#   setkey(rx_ms_split_int_nc, drug)
#   rx_ms_split_int_nc[rxclass_label, rx_label := label]
#   
#   
#   
#   
#   
#   
#   
#   ####with controls####
#   assign(eval(reg_ms_c) , lm(get(outcome) ~ 
#                                rx2:(ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0 + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6)
#                              + rx2
#                              + zip_factor + zip_factor:yrmon_number + yrmon_factor 
#                              + avg_copay_ym_rx_z + avg_age_ym_rx_z
#                              #                              + total_health_estabs + total_estabs
#                              + tot_pharms_neg_1
#                              + mail_order_users_per_tot_pats_neg_1
#                              + share_medicaid_neg_1
#                              ,
#                              #weights =weight,
#                              data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                                      & opener_zip_dummy ==1 & usable ==1
#                                                      & is.finite(get(outcome))
#                                                      & zip_ym_rx_index == 1
#                                                      #                                                         & rx2 == RX
#                                                      ,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_c_cl) , coeftest(get(reg_ms_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                     & opener_zip_dummy ==1 & usable ==1
#                                     & is.finite(get(outcome))
#                                     & zip_ym_rx_index == 1
#                                     #                                       & rx2 == RX
#                                     ,]$ZIP)))
#   
#   coefs = names(get(reg_ms_c_cl)[,1])
#   print(get(reg_ms_c_cl)[grepl("ms",coefs),1])
#   print(nobs(get(reg_ms_c)))
#   print(coefs[grepl("ms",coefs)])
#   
#   
#   pill_type_rx_list_int_c = coefs[grepl("ms",coefs)]
#   pill_type_coef_list_int_c = get(reg_ms_c_cl)[grepl("ms",coefs),1]
#   pill_type_se_list_int_c = get(reg_ms_c_cl)[grepl("ms",coefs),2]
#   pill_type_p_list_int_c = get(reg_ms_c_cl)[grepl("ms",coefs),4]
#   
#   rx_ms_split_int_c = data.table(rx = unlist(pill_type_rx_list_int_c),
#                                  coef = unlist(pill_type_coef_list_int_c),
#                                  se = unlist(pill_type_se_list_int_c),
#                                  p = unlist(pill_type_p_list_int_c))
#   
#   rx_ms_split_int_c[, ms := ifelse(grepl("neg6", rx), -6,
#                                    ifelse(grepl("neg5", rx), -5,
#                                           ifelse(grepl("neg4", rx), -4,
#                                                  ifelse(grepl("neg3", rx), -3,
#                                                         ifelse(grepl("neg2", rx), -2,
#                                                                ifelse(grepl("_0", rx), 0,
#                                                                       ifelse(grepl("_1", rx), 1,
#                                                                              ifelse(grepl("_2", rx), 2,
#                                                                                     ifelse(grepl("_3", rx), 3,
#                                                                                            ifelse(grepl("_4", rx), 4,
#                                                                                                   ifelse(grepl("_5", rx), 5,
#                                                                                                          ifelse(grepl("_6", rx), 6,
#                                                                                                                 NA))))))))))))]
#   rx_ms_split_int_c[,drug := as.character(as.numeric(substr(rx,4,5)))]
#   
#   rx_ms_split_int_c[, ymin := coef - 1.96 * se]
#   rx_ms_split_int_c[, ymax := coef + 1.96 * se]
#   
#   #merge on the rxclass label
#   rxclass_label = as.data.table(read.csv("~/current/pharmacy_deserts/data/rxclass_labels.csv"))
#   rxclass_label[, drug := as.character(rx2)]
#   setkey(rxclass_label, drug)
#   setkey(rx_ms_split_int_c, drug)
#   rx_ms_split_int_c[rxclass_label, rx_label := label]
#   
#   
#   
#   
#   
#   
#   for (RX in sort(unique(rx_ms_split_int_nc$drug))) {
#     #         RX = 58
#     #add in 0 at negative 1
#     add0 = data.table(rx = RX,
#                       coef = 0,
#                       drug = RX,
#                       ms = -1)
#     rx_ms_split_int_nc = rbindlist(list(rx_ms_split_int_nc, add0), fill=T)
#     
#     ####plot###
#     plot_nc = ggplot(data = rx_ms_split_int_nc[drug == RX,]) +
#       geom_point(aes(y=coef,x=ms),size = 8) +
#       geom_line(aes(y=coef,x=ms)) +
#       geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#       geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#       theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#       scale_x_continuous(breaks = seq(-6,6,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title," \n(no controls)\n",rx_ms_split_int_nc[drug == RX,]$rx_label,sep=""))
#     print(plot_nc)
#     
#     
#     rx_ms_split_int_c = rbindlist(list(rx_ms_split_int_c, add0), fill=T)
#     
#     ####plot###
#     plot_c = ggplot(data = rx_ms_split_int_c[drug == RX,]) +
#       geom_point(aes(y=coef,x=ms),size = 8) +
#       geom_line(aes(y=coef,x=ms)) +
#       geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#       geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#       theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#       scale_x_continuous(breaks = seq(-6,6,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title," \n(with controls)\n",rx_ms_split_int_c[drug == RX,]$rx_label,sep=""))
#     print(plot_c)
#   }
#   
#   
# }
# dev.off()
# 
# 
# ####months since regs split by insurance type 18####
# 
# pdf(file = paste("~/current/pharmacy_deserts/event_study_ins_18_split_no_weights_",B,".pdf",sep=""), width = 18, height = 10)
# # for (INS in sort(unique(zip_data_stacked$insurance_type_18))) {
# ins_type_ins_18_list_int_nc = list()
# ins_type_coef_list_int_nc = list()
# ins_type_se_list_int_nc = list()
# ins_type_p_list_int_nc = list()
# 
# ins_type_ins_18_list_int_c = list()
# ins_type_coef_list_int_c = list()
# ins_type_se_list_int_c = list()
# ins_type_p_list_int_c = list()
# 
# for (outcome in c("log_total_pills_dispensed_ym_ins_18_z","log_total_pats_ym_ins_18_z","log_total_claims_ym_ins_18_z",
#                   "log_avg_age_ym_ins_18_z","log_avg_copay_ym_ins_18_z_plus_1", "log_total_new_pats_ym_ins_18_z_plus_1")) {
#   
#   #     outcome = "log_total_pills_dispensed_ym_ins_18_z"
#   print(outcome)
#   
#   reg_ms_nc = paste("reg_ms_nc_",outcome, sep="")
#   reg_ms_nc_cl = paste("reg_ms_nc_cl_",outcome, sep="")
#   reg_ms_c = paste("reg_ms_c_",outcome, sep="")
#   reg_ms_c_cl = paste("reg_ms_c_cl_",outcome, sep="")
#   
#   reg_ms_nc_g = paste("reg_ms_nc_g_",outcome, sep="")
#   reg_ms_nc_cl_g = paste("reg_ms_nc_cl_g_",outcome, sep="")
#   reg_ms_c_g = paste("reg_ms_c_g_",outcome, sep="")
#   reg_ms_c_cl_g = paste("reg_ms_c_cl_g_",outcome, sep="")
#   
#   if (grepl("total_pills_dispensed",outcome)) {
#     title = "Pills dispensed"
#   } else if (grepl("total_pats",outcome)) {
#     title = "Unique patients"
#   } else if (grepl("total_claims",outcome)){
#     title = "Total claims"
#   } else if (grepl("total_money",outcome)){
#     title = "Total money"
#   } else if (grepl("avg_age",outcome)) {
#     title = "Average age"
#   } else if (grepl("avg_copay",outcome)){
#     title = "Average copay"
#   } else if (grepl("total_new_pats",outcome)){
#     title = "New patients"
#   } else if (grepl("mean_mpr",outcome)){
#     title = "Med. Possesion Ratio"
#   }
#   
#   ####histogram of outcome####
#   hist = ggplot(data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                         & opener_zip_dummy ==1 & usable ==1 
#                                         & zip_ym_ins_18_index == 1
#                                         #                                           & insurance_type_18 == INS
#                                         ,]) +
#     geom_histogram(aes(x=get(outcome))) +
#     theme_bw() +  xlab("") +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title,"(log)",sep="\n")) 
#   print(hist)
#   
#   hist = ggplot(data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                         & opener_zip_dummy ==1 & usable ==1 
#                                         & zip_ym_ins_18_index == 1
#                                         #                                           & insurance_type_18 == INS
#                                         ,]) +
#     geom_histogram(aes(x=exp(get(outcome)))) +
#     theme_bw() +  xlab("") +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title,"(raw)",sep="\n")) 
#   print(hist)
#   
#   
#   
#   
#   ####no controls####
#   assign(eval(reg_ms_nc) , lm(get(outcome) ~ 
#                                 (ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0
#                                  + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6):as.factor(insurance_type_18) 
#                               + as.factor(insurance_type_18)
#                               + zip_factor + zip_factor:yrmon_number + yrmon_factor 
#                               ,
#                               #weights =weight,
#                               data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                                       & opener_zip_dummy ==1 & usable ==1 
#                                                       & zip_ym_ins_18_index == 1
#                                                       #                                                         & insurance_type_18 == INS
#                                                       ,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_nc_cl) , coeftest(get(reg_ms_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                     & opener_zip_dummy ==1 & usable ==1 
#                                     & zip_ym_ins_18_index == 1
#                                     #                                       & insurance_type_18 == INS
#                                     ,]$ZIP)))
#   
#   coefs = names(get(reg_ms_nc_cl)[,1])
#   print(get(reg_ms_nc_cl)[grepl("ms",coefs),1])
#   print(nobs(get(reg_ms_nc)))
#   print(coefs[grepl("ms",coefs)])
#   
#   
#   ins_type_ins_18_list_int_nc = coefs[grepl("ms",coefs)]
#   ins_type_coef_list_int_nc = get(reg_ms_nc_cl)[grepl("ms",coefs),1]
#   ins_type_se_list_int_nc = get(reg_ms_nc_cl)[grepl("ms",coefs),2]
#   ins_type_p_list_int_nc = get(reg_ms_nc_cl)[grepl("ms",coefs),4]
#   
#   ins_18_ms_split_int_nc = data.table(ins_18 = unlist(ins_type_ins_18_list_int_nc),
#                                       coef = unlist(ins_type_coef_list_int_nc),
#                                       se = unlist(ins_type_se_list_int_nc),
#                                       p = unlist(ins_type_p_list_int_nc))
#   
#   ins_18_ms_split_int_nc[, ms := ifelse(grepl("neg6", ins_18), -6,
#                                         ifelse(grepl("neg5", ins_18), -5,
#                                                ifelse(grepl("neg4", ins_18), -4,
#                                                       ifelse(grepl("neg3", ins_18), -3,
#                                                              ifelse(grepl("neg2", ins_18), -2,
#                                                                     ifelse(grepl("_0", ins_18), 0,
#                                                                            ifelse(grepl("ms_1", ins_18), 1,
#                                                                                   ifelse(grepl("_2", ins_18), 2,
#                                                                                          ifelse(grepl("_3", ins_18), 3,
#                                                                                                 ifelse(grepl("_4", ins_18), 4,
#                                                                                                        ifelse(grepl("_5", ins_18), 5,
#                                                                                                               ifelse(grepl("_6", ins_18), 6,
#                                                                                                                      NA))))))))))))]
#   ins_18_ms_split_int_nc[,ins_type := ifelse(ms < 0, substr(ins_18,37,42),substr(ins_18,34,39))]
#   
#   ins_18_ms_split_int_nc[, ymin := coef - 1.96 * se]
#   ins_18_ms_split_int_nc[, ymax := coef + 1.96 * se]
#   
#   
#   
#   
#   
#   ####with controls####
#   assign(eval(reg_ms_c) , lm(get(outcome) ~ 
#                                (ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0
#                                 + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6):as.factor(insurance_type_18) 
#                              + as.factor(insurance_type_18)
#                              + zip_factor + zip_factor:yrmon_number + yrmon_factor  
#                              + avg_copay_ym_ins_18_z + avg_age_ym_ins_18_z
#                              #                                + total_health_estabs + total_estabs
#                              + tot_pharms_neg_1
#                              + mail_order_users_per_tot_pats_neg_1
#                              ,
#                              #weights =weight,
#                              data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                                      & opener_zip_dummy ==1 & usable ==1 
#                                                      & zip_ym_ins_18_index == 1
#                                                      #                                                        & insurance_type_18 == INS
#                                                      ,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_c_cl) , coeftest(get(reg_ms_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                     & opener_zip_dummy ==1 & usable ==1 
#                                     & zip_ym_ins_18_index == 1
#                                     #                                       & insurance_type_18 == INS
#                                     ,]$ZIP)))
#   
#   coefs = names(get(reg_ms_c_cl)[,1])
#   print(get(reg_ms_c_cl)[grepl("ms",coefs),1])
#   print(nobs(get(reg_ms_c)))
#   print(coefs[grepl("ms",coefs)])
#   
#   
#   ins_type_ins_18_list_int_c = coefs[grepl("ms",coefs)]
#   ins_type_coef_list_int_c = get(reg_ms_c_cl)[grepl("ms",coefs),1]
#   ins_type_se_list_int_c = get(reg_ms_c_cl)[grepl("ms",coefs),2]
#   ins_type_p_list_int_c = get(reg_ms_c_cl)[grepl("ms",coefs),4]
#   
#   ins_18_ms_split_int_c = data.table(ins_18 = unlist(ins_type_ins_18_list_int_c),
#                                      coef = unlist(ins_type_coef_list_int_c),
#                                      se = unlist(ins_type_se_list_int_c),
#                                      p = unlist(ins_type_p_list_int_c))
#   
#   ins_18_ms_split_int_c[, ms := ifelse(grepl("neg6", ins_18), -6,
#                                        ifelse(grepl("neg5", ins_18), -5,
#                                               ifelse(grepl("neg4", ins_18), -4,
#                                                      ifelse(grepl("neg3", ins_18), -3,
#                                                             ifelse(grepl("neg2", ins_18), -2,
#                                                                    ifelse(grepl("_0", ins_18), 0,
#                                                                           ifelse(grepl("ms_1", ins_18), 1,
#                                                                                  ifelse(grepl("_2", ins_18), 2,
#                                                                                         ifelse(grepl("_3", ins_18), 3,
#                                                                                                ifelse(grepl("_4", ins_18), 4,
#                                                                                                       ifelse(grepl("_5", ins_18), 5,
#                                                                                                              ifelse(grepl("_6", ins_18), 6,
#                                                                                                                     NA))))))))))))]
#   ins_18_ms_split_int_c[,ins_type := ifelse(ms < 0, substr(ins_18,37,42),substr(ins_18,34,39))]
#   
#   ins_18_ms_split_int_c[, ymin := coef - 1.96 * se]
#   ins_18_ms_split_int_c[, ymax := coef + 1.96 * se]
#   
#   
#   for (INS_18 in sort(unique(ins_18_ms_split_int_nc$ins_type))) {
#     #   INS_18 = "C+ADV"
#     #add in 0 at negative 1
#     add0 = data.table(ins_type = INS_18,
#                       coef = 0,
#                       ins_18 = INS_18,
#                       ms = -1)
#     ins_18_ms_split_int_nc = rbindlist(list(ins_18_ms_split_int_nc, add0), fill=T)
#     
#     ####plot###
#     plot_nc = ggplot(data = ins_18_ms_split_int_nc[ins_type == INS_18,]) +
#       geom_point(aes(y=coef,x=ms),size = 8) +
#       geom_line(aes(y=coef,x=ms)) +
#       geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#       geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .15) +
#       theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#       scale_x_continuous(breaks = seq(-6,6,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title," \n(no controls)\n",ins_18_ms_split_int_nc[ins_type == INS_18,]$ins_type,sep=""))
#     print(plot_nc)
#     
#     
#     ins_18_ms_split_int_c = rbindlist(list(ins_18_ms_split_int_c, add0), fill=T)
#     
#     ####plot###
#     plot_c = ggplot(data = ins_18_ms_split_int_c[ins_type == INS_18,]) +
#       geom_point(aes(y=coef,x=ms),size = 8) +
#       geom_line(aes(y=coef,x=ms)) +
#       geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#       geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .15) +
#       theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#       scale_x_continuous(breaks = seq(-6,6,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title," \n(with controls)\n",ins_18_ms_split_int_c[ins_type == INS_18,]$ins_type,sep=""))
#     print(plot_c)
#   }
#   
# }
# 
# dev.off()
# 
# ####months since regs split by insurance type 8####
# 
# pdf(file = paste("~/current/pharmacy_deserts/event_study_ins_8_split_no_weights_",B,".pdf",sep=""), width = 18, height = 10)
# ins_type_ins_8_list_int_nc = list()
# ins_type_coef_list_int_nc = list()
# ins_type_se_list_int_nc = list()
# ins_type_p_list_int_nc = list()
# 
# ins_type_ins_8_list_int_c = list()
# ins_type_coef_list_int_c = list()
# ins_type_se_list_int_c = list()
# ins_type_p_list_int_c = list()
# 
# for (outcome in c("log_total_pills_dispensed_ym_ins_8_z","log_total_pats_ym_ins_8_z","log_total_claims_ym_ins_8_z",
#                   "log_avg_age_ym_ins_8_z","log_avg_copay_ym_ins_8_z_plus_1", "log_total_new_pats_ym_ins_8_z_plus_1")) {
#   
#   #       outcome = "log_total_pills_dispensed_ym_ins_8_z"
#   print(outcome)
#   
#   reg_ms_nc = paste("reg_ms_nc_",outcome, sep="")
#   reg_ms_nc_cl = paste("reg_ms_nc_cl_",outcome, sep="")
#   reg_ms_c = paste("reg_ms_c_",outcome, sep="")
#   reg_ms_c_cl = paste("reg_ms_c_cl_",outcome, sep="")
#   
#   reg_ms_nc_g = paste("reg_ms_nc_g_",outcome, sep="")
#   reg_ms_nc_cl_g = paste("reg_ms_nc_cl_g_",outcome, sep="")
#   reg_ms_c_g = paste("reg_ms_c_g_",outcome, sep="")
#   reg_ms_c_cl_g = paste("reg_ms_c_cl_g_",outcome, sep="")
#   
#   if (grepl("total_pills_dispensed",outcome)) {
#     title = "Pills dispensed"
#   } else if (grepl("total_pats",outcome)) {
#     title = "Unique patients"
#   } else if (grepl("total_claims",outcome)){
#     title = "Total claims"
#   } else if (grepl("total_money",outcome)){
#     title = "Total money"
#   } else if (grepl("avg_age",outcome)) {
#     title = "Average age"
#   } else if (grepl("avg_copay",outcome)){
#     title = "Average copay"
#   } else if (grepl("total_new_pats",outcome)){
#     title = "New patients"
#   } else if (grepl("mean_mpr",outcome)){
#     title = "Med. Possesion Ratio"
#   }
#   
#   ####histogram of outcome####
#   hist = ggplot(data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                         & opener_zip_dummy ==1 & usable ==1 
#                                         & zip_ym_ins_8_index == 1
#                                         #                                           & insurance_type_8 == INS
#                                         ,]) +
#     geom_histogram(aes(x=get(outcome))) +
#     theme_bw() +  xlab("") +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title,"(log)",sep="\n")) 
#   print(hist)
#   
#   hist = ggplot(data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                         & opener_zip_dummy ==1 & usable ==1 
#                                         & zip_ym_ins_8_index == 1
#                                         #                                           & insurance_type_8 == INS
#                                         ,]) +
#     geom_histogram(aes(x=exp(get(outcome)))) +
#     theme_bw() +  xlab("") +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title,"(raw)",sep="\n")) 
#   print(hist)
#   
#   
#   
#   
#   ####no controls####
#   assign(eval(reg_ms_nc) , lm(get(outcome) ~ 
#                                 (ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0
#                                  + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6):as.factor(insurance_type_8) 
#                               + as.factor(insurance_type_8)
#                               + zip_factor + zip_factor:yrmon_number + yrmon_factor 
#                               ,
#                               #weights =weight,
#                               data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                                       & opener_zip_dummy ==1 & usable ==1 
#                                                       & zip_ym_ins_8_index == 1
#                                                       #                                                         & insurance_type_8 == INS
#                                                       ,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_nc_cl) , coeftest(get(reg_ms_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                     & opener_zip_dummy ==1 & usable ==1 
#                                     & zip_ym_ins_8_index == 1
#                                     #                                       & insurance_type_8 == INS
#                                     ,]$ZIP)))
#   
#   coefs = names(get(reg_ms_nc_cl)[,1])
#   print(get(reg_ms_nc_cl)[grepl("ms",coefs),1])
#   print(nobs(get(reg_ms_nc)))
#   print(coefs[grepl("ms",coefs)])
#   
#   
#   ins_type_ins_8_list_int_nc = coefs[grepl("ms",coefs)]
#   ins_type_coef_list_int_nc = get(reg_ms_nc_cl)[grepl("ms",coefs),1]
#   ins_type_se_list_int_nc = get(reg_ms_nc_cl)[grepl("ms",coefs),2]
#   ins_type_p_list_int_nc = get(reg_ms_nc_cl)[grepl("ms",coefs),4]
#   
#   ins_8_ms_split_int_nc = data.table(ins_8 = unlist(ins_type_ins_8_list_int_nc),
#                                      coef = unlist(ins_type_coef_list_int_nc),
#                                      se = unlist(ins_type_se_list_int_nc),
#                                      p = unlist(ins_type_p_list_int_nc))
#   
#   ins_8_ms_split_int_nc[, ms := ifelse(grepl("neg6", ins_8), -6,
#                                        ifelse(grepl("neg5", ins_8), -5,
#                                               ifelse(grepl("neg4", ins_8), -4,
#                                                      ifelse(grepl("neg3", ins_8), -3,
#                                                             ifelse(grepl("neg2", ins_8), -2,
#                                                                    ifelse(grepl("_0", ins_8), 0,
#                                                                           ifelse(grepl("ms_1", ins_8), 1,
#                                                                                  ifelse(grepl("_2", ins_8), 2,
#                                                                                         ifelse(grepl("_3", ins_8), 3,
#                                                                                                ifelse(grepl("_4", ins_8), 4,
#                                                                                                       ifelse(grepl("_5", ins_8), 5,
#                                                                                                              ifelse(grepl("_6", ins_8), 6,
#                                                                                                                     NA))))))))))))]
#   ins_8_ms_split_int_nc[,ins_type := ifelse(ms < 0, substr(ins_8,36,42),substr(ins_8,33,39))]
#   
#   ins_8_ms_split_int_nc[, ymin := coef - 1.96 * se]
#   ins_8_ms_split_int_nc[, ymax := coef + 1.96 * se]
#   
#   
#   
#   
#   
#   ####with controls####
#   assign(eval(reg_ms_c) , lm(get(outcome) ~ 
#                                (ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0
#                                 + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6):as.factor(insurance_type_8) 
#                              + as.factor(insurance_type_8)
#                              + zip_factor + zip_factor:yrmon_number + yrmon_factor  
#                              + avg_copay_ym_ins_8_z + avg_age_ym_ins_8_z
#                              #                                + total_health_estabs + total_estabs
#                              + tot_pharms_neg_1
#                              + mail_order_users_per_tot_pats_neg_1
#                              ,
#                              #weights =weight,
#                              data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                                      & opener_zip_dummy ==1 & usable ==1 
#                                                      & zip_ym_ins_8_index == 1
#                                                      #                                                        & insurance_type_8 == INS
#                                                      ,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_c_cl) , coeftest(get(reg_ms_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                     & opener_zip_dummy ==1 & usable ==1 
#                                     & zip_ym_ins_8_index == 1
#                                     #                                       & insurance_type_8 == INS
#                                     ,]$ZIP)))
#   
#   coefs = names(get(reg_ms_c_cl)[,1])
#   print(get(reg_ms_c_cl)[grepl("ms",coefs),1])
#   print(nobs(get(reg_ms_c)))
#   print(coefs[grepl("ms",coefs)])
#   
#   
#   ins_type_ins_8_list_int_c = coefs[grepl("ms",coefs)]
#   ins_type_coef_list_int_c = get(reg_ms_c_cl)[grepl("ms",coefs),1]
#   ins_type_se_list_int_c = get(reg_ms_c_cl)[grepl("ms",coefs),2]
#   ins_type_p_list_int_c = get(reg_ms_c_cl)[grepl("ms",coefs),4]
#   
#   ins_8_ms_split_int_c = data.table(ins_8 = unlist(ins_type_ins_8_list_int_c),
#                                     coef = unlist(ins_type_coef_list_int_c),
#                                     se = unlist(ins_type_se_list_int_c),
#                                     p = unlist(ins_type_p_list_int_c))
#   
#   ins_8_ms_split_int_c[, ms := ifelse(grepl("neg6", ins_8), -6,
#                                       ifelse(grepl("neg5", ins_8), -5,
#                                              ifelse(grepl("neg4", ins_8), -4,
#                                                     ifelse(grepl("neg3", ins_8), -3,
#                                                            ifelse(grepl("neg2", ins_8), -2,
#                                                                   ifelse(grepl("_0", ins_8), 0,
#                                                                          ifelse(grepl("ms_1", ins_8), 1,
#                                                                                 ifelse(grepl("_2", ins_8), 2,
#                                                                                        ifelse(grepl("_3", ins_8), 3,
#                                                                                               ifelse(grepl("_4", ins_8), 4,
#                                                                                                      ifelse(grepl("_5", ins_8), 5,
#                                                                                                             ifelse(grepl("_6", ins_8), 6,
#                                                                                                                    NA))))))))))))]
#   ins_8_ms_split_int_c[,ins_type := ifelse(ms < 0, substr(ins_8,36,42),substr(ins_8,33,39))]
#   
#   ins_8_ms_split_int_c[, ymin := coef - 1.96 * se]
#   ins_8_ms_split_int_c[, ymax := coef + 1.96 * se]
#   
#   
#   for (INS_8 in sort(unique(ins_8_ms_split_int_nc$ins_type))) {
#     #       INS_8 = "ADV"
#     #add in 0 at negative 1
#     add0 = data.table(ins_type = INS_8,
#                       coef = 0,
#                       ins_8 = INS_8,
#                       ms = -1)
#     ins_8_ms_split_int_nc = rbindlist(list(ins_8_ms_split_int_nc, add0), fill=T)
#     
#     ####plot###
#     plot_nc = ggplot(data = ins_8_ms_split_int_nc[ins_type == INS_8,]) +
#       geom_point(aes(y=coef,x=ms),size = 8) +
#       geom_line(aes(y=coef,x=ms)) +
#       geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#       geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .15) +
#       theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#       scale_x_continuous(breaks = seq(-6,6,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title," \n(no controls)\n",ins_8_ms_split_int_nc[ins_type == INS_8,]$ins_type,sep=""))
#     print(plot_nc)
#     
#     
#     ins_8_ms_split_int_c = rbindlist(list(ins_8_ms_split_int_c, add0), fill=T)
#     
#     ####plot###
#     plot_c = ggplot(data = ins_8_ms_split_int_c[ins_type == INS_8,]) +
#       geom_point(aes(y=coef,x=ms),size = 8) +
#       geom_line(aes(y=coef,x=ms)) +
#       geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#       geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .15) +
#       theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#       scale_x_continuous(breaks = seq(-6,6,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title," \n(with controls)\n",ins_8_ms_split_int_c[ins_type == INS_8,]$ins_type,sep=""))
#     print(plot_c)
#   }
#   
#   
#   ####use all zips as controls####
#   ####no controls####
#   assign(eval(reg_ms_nc) , lm(get(outcome) ~ 
#                                 (ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0
#                                  + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6):as.factor(insurance_type_8) 
#                               + as.factor(insurance_type_8)
#                               + zip_factor + zip_factor:yrmon_number + yrmon_factor 
#                               ,
#                               #weights =weight,
#                               data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                                       & (usable ==1 | non_opener == 1)
#                                                       & is.finite(get(outcome))  
#                                                       & zip_ym_ins_8_index == 1
#                                                       #                                                         & insurance_type_8 == INS
#                                                       ,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_nc_cl) , coeftest(get(reg_ms_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                     & (usable ==1 | non_opener == 1)
#                                     & is.finite(get(outcome)) 
#                                     & zip_ym_ins_8_index == 1
#                                     #                                       & insurance_type_8 == INS
#                                     ,]$ZIP)))
#   
#   coefs = names(get(reg_ms_nc_cl)[,1])
#   print(get(reg_ms_nc_cl)[grepl("ms",coefs),1])
#   print(nobs(get(reg_ms_nc)))
#   print(coefs[grepl("ms",coefs)])
#   
#   
#   ins_type_ins_8_list_int_nc = coefs[grepl("ms",coefs)]
#   ins_type_coef_list_int_nc = get(reg_ms_nc_cl)[grepl("ms",coefs),1]
#   ins_type_se_list_int_nc = get(reg_ms_nc_cl)[grepl("ms",coefs),2]
#   ins_type_p_list_int_nc = get(reg_ms_nc_cl)[grepl("ms",coefs),4]
#   
#   ins_8_ms_split_int_nc = data.table(ins_8 = unlist(ins_type_ins_8_list_int_nc),
#                                      coef = unlist(ins_type_coef_list_int_nc),
#                                      se = unlist(ins_type_se_list_int_nc),
#                                      p = unlist(ins_type_p_list_int_nc))
#   
#   ins_8_ms_split_int_nc[, ms := ifelse(grepl("neg6", ins_8), -6,
#                                        ifelse(grepl("neg5", ins_8), -5,
#                                               ifelse(grepl("neg4", ins_8), -4,
#                                                      ifelse(grepl("neg3", ins_8), -3,
#                                                             ifelse(grepl("neg2", ins_8), -2,
#                                                                    ifelse(grepl("_0", ins_8), 0,
#                                                                           ifelse(grepl("ms_1", ins_8), 1,
#                                                                                  ifelse(grepl("_2", ins_8), 2,
#                                                                                         ifelse(grepl("_3", ins_8), 3,
#                                                                                                ifelse(grepl("_4", ins_8), 4,
#                                                                                                       ifelse(grepl("_5", ins_8), 5,
#                                                                                                              ifelse(grepl("_6", ins_8), 6,
#                                                                                                                     NA))))))))))))]
#   ins_8_ms_split_int_nc[,ins_type := ifelse(ms < 0, substr(ins_8,36,42),substr(ins_8,33,39))]
#   
#   ins_8_ms_split_int_nc[, ymin := coef - 1.96 * se]
#   ins_8_ms_split_int_nc[, ymax := coef + 1.96 * se]
#   
#   
#   
#   
#   
#   ####with controls####
#   assign(eval(reg_ms_c) , lm(get(outcome) ~ 
#                                (ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0
#                                 + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6):as.factor(insurance_type_8) 
#                              + as.factor(insurance_type_8)
#                              + zip_factor + zip_factor:yrmon_number + yrmon_factor  
#                              + avg_copay_ym_ins_8_z + avg_age_ym_ins_8_z
#                              #                                + total_health_estabs + total_estabs
#                              + tot_pharms_neg_1
#                              + mail_order_users_per_tot_pats_neg_1
#                              ,
#                              #weights =weight,
#                              data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                                      & (usable ==1 | non_opener == 1)
#                                                      & is.finite(get(outcome))
#                                                      & zip_ym_ins_8_index == 1
#                                                      #                                                        & insurance_type_8 == INS
#                                                      ,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_c_cl) , coeftest(get(reg_ms_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                     & (usable ==1 | non_opener == 1)
#                                     & is.finite(get(outcome)) 
#                                     & zip_ym_ins_8_index == 1
#                                     #                                       & insurance_type_8 == INS
#                                     ,]$ZIP)))
#   
#   coefs = names(get(reg_ms_c_cl)[,1])
#   print(get(reg_ms_c_cl)[grepl("ms",coefs),1])
#   print(nobs(get(reg_ms_c)))
#   print(coefs[grepl("ms",coefs)])
#   
#   
#   ins_type_ins_8_list_int_c = coefs[grepl("ms",coefs)]
#   ins_type_coef_list_int_c = get(reg_ms_c_cl)[grepl("ms",coefs),1]
#   ins_type_se_list_int_c = get(reg_ms_c_cl)[grepl("ms",coefs),2]
#   ins_type_p_list_int_c = get(reg_ms_c_cl)[grepl("ms",coefs),4]
#   
#   ins_8_ms_split_int_c = data.table(ins_8 = unlist(ins_type_ins_8_list_int_c),
#                                     coef = unlist(ins_type_coef_list_int_c),
#                                     se = unlist(ins_type_se_list_int_c),
#                                     p = unlist(ins_type_p_list_int_c))
#   
#   ins_8_ms_split_int_c[, ms := ifelse(grepl("neg6", ins_8), -6,
#                                       ifelse(grepl("neg5", ins_8), -5,
#                                              ifelse(grepl("neg4", ins_8), -4,
#                                                     ifelse(grepl("neg3", ins_8), -3,
#                                                            ifelse(grepl("neg2", ins_8), -2,
#                                                                   ifelse(grepl("_0", ins_8), 0,
#                                                                          ifelse(grepl("ms_1", ins_8), 1,
#                                                                                 ifelse(grepl("_2", ins_8), 2,
#                                                                                        ifelse(grepl("_3", ins_8), 3,
#                                                                                               ifelse(grepl("_4", ins_8), 4,
#                                                                                                      ifelse(grepl("_5", ins_8), 5,
#                                                                                                             ifelse(grepl("_6", ins_8), 6,
#                                                                                                                    NA))))))))))))]
#   ins_8_ms_split_int_c[,ins_type := ifelse(ms < 0, substr(ins_8,36,42),substr(ins_8,33,39))]
#   
#   ins_8_ms_split_int_c[, ymin := coef - 1.96 * se]
#   ins_8_ms_split_int_c[, ymax := coef + 1.96 * se]
#   
#   
#   for (INS_8 in sort(unique(ins_8_ms_split_int_nc$ins_type))) {
#     #     INS_8 = "ADV"
#     #add in 0 at negative 1
#     add0 = data.table(ins_type = INS_8,
#                       coef = 0,
#                       ins_8 = INS_8,
#                       ms = -1)
#     ins_8_ms_split_int_nc = rbindlist(list(ins_8_ms_split_int_nc, add0), fill=T)
#     
#     ####plot###
#     plot_nc = ggplot(data = ins_8_ms_split_int_nc[ins_type == INS_8,]) +
#       geom_point(aes(y=coef,x=ms),size = 8) +
#       geom_line(aes(y=coef,x=ms)) +
#       geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#       geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .15) +
#       theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#       scale_x_continuous(breaks = seq(-6,6,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title," \n(no controls)\n",ins_8_ms_split_int_nc[ins_type == INS_8,]$ins_type,"\n(all zips)",sep=""))
#     print(plot_nc)
#     
#     
#     ins_8_ms_split_int_c = rbindlist(list(ins_8_ms_split_int_c, add0), fill=T)
#     
#     ####plot###
#     plot_c = ggplot(data = ins_8_ms_split_int_c[ins_type == INS_8,]) +
#       geom_point(aes(y=coef,x=ms),size = 8) +
#       geom_line(aes(y=coef,x=ms)) +
#       geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#       geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .15) +
#       theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#       scale_x_continuous(breaks = seq(-6,6,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title," \n(with controls)\n",ins_8_ms_split_int_c[ins_type == INS_8,]$ins_type,"\n(all zips)",sep=""))
#     print(plot_c)
#   }
#   
# }
# 
# dev.off()
# 
# ####months since regs split by insurance type 23####
# 
# pdf(file = paste("~/current/pharmacy_deserts/event_study_ins_23_split_no_weights_",B,".pdf",sep=""), width = 18, height = 10)
# ins_type_ins_23_list_int_nc = list()
# ins_type_coef_list_int_nc = list()
# ins_type_se_list_int_nc = list()
# ins_type_p_list_int_nc = list()
# 
# ins_type_ins_23_list_int_c = list()
# ins_type_coef_list_int_c = list()
# ins_type_se_list_int_c = list()
# ins_type_p_list_int_c = list()
# 
# for (outcome in c("log_total_pills_dispensed_ym_ins_23_z","log_total_pats_ym_ins_23_z","log_total_claims_ym_ins_23_z",
#                   "log_avg_age_ym_ins_23_z","log_avg_copay_ym_ins_23_z_plus_1", "log_total_new_pats_ym_ins_23_z_plus_1")) {
#   
#   #       outcome = "log_total_pills_dispensed_ym_ins_23_z"
#   print(outcome)
#   
#   reg_ms_nc = paste("reg_ms_nc_",outcome, sep="")
#   reg_ms_nc_cl = paste("reg_ms_nc_cl_",outcome, sep="")
#   reg_ms_c = paste("reg_ms_c_",outcome, sep="")
#   reg_ms_c_cl = paste("reg_ms_c_cl_",outcome, sep="")
#   
#   reg_ms_nc_g = paste("reg_ms_nc_g_",outcome, sep="")
#   reg_ms_nc_cl_g = paste("reg_ms_nc_cl_g_",outcome, sep="")
#   reg_ms_c_g = paste("reg_ms_c_g_",outcome, sep="")
#   reg_ms_c_cl_g = paste("reg_ms_c_cl_g_",outcome, sep="")
#   
#   if (grepl("total_pills_dispensed",outcome)) {
#     title = "Pills dispensed"
#   } else if (grepl("total_pats",outcome)) {
#     title = "Unique patients"
#   } else if (grepl("total_claims",outcome)){
#     title = "Total claims"
#   } else if (grepl("total_money",outcome)){
#     title = "Total money"
#   } else if (grepl("avg_age",outcome)) {
#     title = "Average age"
#   } else if (grepl("avg_copay",outcome)){
#     title = "Average copay"
#   } else if (grepl("total_new_pats",outcome)){
#     title = "New patients"
#   } else if (grepl("mean_mpr",outcome)){
#     title = "Med. Possesion Ratio"
#   }
#   
#   ####histogram of outcome####
#   hist = ggplot(data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                         & opener_zip_dummy ==1 & usable ==1 
#                                         & zip_ym_ins_23_index == 1
#                                         #                                           & insurance_type_23 == INS
#                                         ,]) +
#     geom_histogram(aes(x=get(outcome))) +
#     theme_bw() +  xlab("") +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title,"(log)",sep="\n")) 
#   print(hist)
#   
#   hist = ggplot(data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                         & opener_zip_dummy ==1 & usable ==1 
#                                         & zip_ym_ins_23_index == 1
#                                         #                                           & insurance_type_23 == INS
#                                         ,]) +
#     geom_histogram(aes(x=exp(get(outcome)))) +
#     theme_bw() +  xlab("") +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title,"(raw)",sep="\n")) 
#   print(hist)
#   
#   
#   
#   
#   ####no controls####
#   assign(eval(reg_ms_nc) , lm(get(outcome) ~ 
#                                 (ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0
#                                  + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6):as.factor(insurance_type_23) 
#                               + as.factor(insurance_type_23)
#                               + zip_factor + zip_factor:yrmon_number + yrmon_factor 
#                               ,
#                               #weights =weight,
#                               data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                                       & opener_zip_dummy ==1 & usable ==1 
#                                                       & zip_ym_ins_23_index == 1
#                                                       & is.finite(get(outcome))
#                                                       #                                                         & insurance_type_23 == INS
#                                                       ,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_nc_cl) , coeftest(get(reg_ms_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                     & opener_zip_dummy ==1 & usable ==1 
#                                     & zip_ym_ins_23_index == 1
#                                     & is.finite(get(outcome))
#                                     #                                       & insurance_type_23 == INS
#                                     ,]$ZIP)))
#   
#   coefs = names(get(reg_ms_nc_cl)[,1])
#   print(get(reg_ms_nc_cl)[grepl("ms",coefs),1])
#   print(nobs(get(reg_ms_nc)))
#   print(coefs[grepl("ms",coefs)])
#   
#   
#   ins_type_ins_23_list_int_nc = coefs[grepl("ms",coefs)]
#   ins_type_coef_list_int_nc = get(reg_ms_nc_cl)[grepl("ms",coefs),1]
#   ins_type_se_list_int_nc = get(reg_ms_nc_cl)[grepl("ms",coefs),2]
#   ins_type_p_list_int_nc = get(reg_ms_nc_cl)[grepl("ms",coefs),4]
#   
#   ins_23_ms_split_int_nc = data.table(ins_23 = unlist(ins_type_ins_23_list_int_nc),
#                                       coef = unlist(ins_type_coef_list_int_nc),
#                                       se = unlist(ins_type_se_list_int_nc),
#                                       p = unlist(ins_type_p_list_int_nc))
#   
#   ins_23_ms_split_int_nc[, ms := ifelse(grepl("neg6", ins_23), -6,
#                                         ifelse(grepl("neg5", ins_23), -5,
#                                                ifelse(grepl("neg4", ins_23), -4,
#                                                       ifelse(grepl("neg3", ins_23), -3,
#                                                              ifelse(grepl("neg2", ins_23), -2,
#                                                                     ifelse(grepl("_0", ins_23), 0,
#                                                                            ifelse(grepl("ms_1", ins_23), 1,
#                                                                                   ifelse(grepl("ms_2", ins_23), 2,
#                                                                                          ifelse(grepl("_3", ins_23), 3,
#                                                                                                 ifelse(grepl("_4", ins_23), 4,
#                                                                                                        ifelse(grepl("_5", ins_23), 5,
#                                                                                                               ifelse(grepl("_6", ins_23), 6,
#                                                                                                                      NA))))))))))))]
#   ins_23_ms_split_int_nc[,ins_type := ifelse(ms < 0, substr(ins_23,37,42),substr(ins_23,34,39))]
#   
#   ins_23_ms_split_int_nc[, ymin := coef - 1.96 * se]
#   ins_23_ms_split_int_nc[, ymax := coef + 1.96 * se]
#   
#   
#   
#   
#   
#   ####with controls####
#   assign(eval(reg_ms_c) , lm(get(outcome) ~ 
#                                (ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0
#                                 + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6):as.factor(insurance_type_23) 
#                              + as.factor(insurance_type_23)
#                              + zip_factor + zip_factor:yrmon_number + yrmon_factor  
#                              + avg_copay_ym_ins_23_z + avg_age_ym_ins_23_z
#                              #                                + total_health_estabs + total_estabs
#                              + tot_pharms_neg_1
#                              + mail_order_users_per_tot_pats_neg_1
#                              ,
#                              #weights =weight,
#                              data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                                      & opener_zip_dummy ==1 & usable ==1 
#                                                      & zip_ym_ins_23_index == 1
#                                                      & is.finite(get(outcome))
#                                                      #                                                        & insurance_type_23 == INS
#                                                      ,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_c_cl) , coeftest(get(reg_ms_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                     & opener_zip_dummy ==1 & usable ==1 
#                                     & zip_ym_ins_23_index == 1
#                                     & is.finite(get(outcome))
#                                     #                                       & insurance_type_23 == INS
#                                     ,]$ZIP)))
#   
#   coefs = names(get(reg_ms_c_cl)[,1])
#   print(get(reg_ms_c_cl)[grepl("ms",coefs),1])
#   print(nobs(get(reg_ms_c)))
#   print(coefs[grepl("ms",coefs)])
#   
#   
#   ins_type_ins_23_list_int_c = coefs[grepl("ms",coefs)]
#   ins_type_coef_list_int_c = get(reg_ms_c_cl)[grepl("ms",coefs),1]
#   ins_type_se_list_int_c = get(reg_ms_c_cl)[grepl("ms",coefs),2]
#   ins_type_p_list_int_c = get(reg_ms_c_cl)[grepl("ms",coefs),4]
#   
#   ins_23_ms_split_int_c = data.table(ins_23 = unlist(ins_type_ins_23_list_int_c),
#                                      coef = unlist(ins_type_coef_list_int_c),
#                                      se = unlist(ins_type_se_list_int_c),
#                                      p = unlist(ins_type_p_list_int_c))
#   
#   ins_23_ms_split_int_c[, ms := ifelse(grepl("neg6", ins_23), -6,
#                                        ifelse(grepl("neg5", ins_23), -5,
#                                               ifelse(grepl("neg4", ins_23), -4,
#                                                      ifelse(grepl("neg3", ins_23), -3,
#                                                             ifelse(grepl("neg2", ins_23), -2,
#                                                                    ifelse(grepl("_0", ins_23), 0,
#                                                                           ifelse(grepl("ms_1", ins_23), 1,
#                                                                                  ifelse(grepl("ms_2", ins_23), 2,
#                                                                                         ifelse(grepl("_3", ins_23), 3,
#                                                                                                ifelse(grepl("_4", ins_23), 4,
#                                                                                                       ifelse(grepl("_5", ins_23), 5,
#                                                                                                              ifelse(grepl("_6", ins_23), 6,
#                                                                                                                     NA))))))))))))]
#   ins_23_ms_split_int_c[,ins_type := ifelse(ms < 0, substr(ins_23,37,42),substr(ins_23,34,39))]
#   
#   ins_23_ms_split_int_c[, ymin := coef - 1.96 * se]
#   ins_23_ms_split_int_c[, ymax := coef + 1.96 * se]
#   
#   
#   for (INS_23 in sort(unique(ins_23_ms_split_int_nc$ins_type))) {
#     #       INS_23 = "MC"
#     #add in 0 at negative 1
#     add0 = data.table(ins_type = INS_23,
#                       coef = 0,
#                       ins_23 = INS_23,
#                       ms = -1)
#     ins_23_ms_split_int_nc = rbindlist(list(ins_23_ms_split_int_nc, add0), fill=T)
#     
#     ####plot###
#     plot_nc = ggplot(data = ins_23_ms_split_int_nc[ins_type == INS_23,]) +
#       geom_point(aes(y=coef,x=ms),size = 8) +
#       geom_line(aes(y=coef,x=ms)) +
#       geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#       geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .15) +
#       theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#       scale_x_continuous(breaks = seq(-6,6,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title," \n(no controls)\n",ins_23_ms_split_int_nc[ins_type == INS_23,]$ins_type,sep=""))
#     print(plot_nc)
#     
#     
#     ins_23_ms_split_int_c = rbindlist(list(ins_23_ms_split_int_c, add0), fill=T)
#     
#     ####plot###
#     plot_c = ggplot(data = ins_23_ms_split_int_c[ins_type == INS_23,]) +
#       geom_point(aes(y=coef,x=ms),size = 8) +
#       geom_line(aes(y=coef,x=ms)) +
#       geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#       geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .15) +
#       theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#       scale_x_continuous(breaks = seq(-6,6,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title," \n(with controls)\n",ins_23_ms_split_int_c[ins_type == INS_23,]$ins_type,sep=""))
#     print(plot_c)
#   }
#   
# }
# 
# 
# dev.off()
# 
# ####months since regs split by insurance type 53####
# 
# pdf(file = paste("~/current/pharmacy_deserts/event_study_ins_53_split_no_weights_",B,".pdf",sep=""), width = 18, height = 10)
# ins_type_ins_53_list_int_nc = list()
# ins_type_coef_list_int_nc = list()
# ins_type_se_list_int_nc = list()
# ins_type_p_list_int_nc = list()
# 
# ins_type_ins_53_list_int_c = list()
# ins_type_coef_list_int_c = list()
# ins_type_se_list_int_c = list()
# ins_type_p_list_int_c = list()
# 
# for (outcome in c("log_total_pills_dispensed_ym_ins_53_z","log_total_pats_ym_ins_53_z","log_total_claims_ym_ins_53_z",
#                   "log_avg_age_ym_ins_53_z","log_avg_copay_ym_ins_53_z_plus_1", "log_total_new_pats_ym_ins_53_z_plus_1")) {
#   
#   #       outcome = "log_total_pills_dispensed_ym_ins_53_z"
#   print(outcome)
#   
#   reg_ms_nc = paste("reg_ms_nc_",outcome, sep="")
#   reg_ms_nc_cl = paste("reg_ms_nc_cl_",outcome, sep="")
#   reg_ms_c = paste("reg_ms_c_",outcome, sep="")
#   reg_ms_c_cl = paste("reg_ms_c_cl_",outcome, sep="")
#   
#   reg_ms_nc_g = paste("reg_ms_nc_g_",outcome, sep="")
#   reg_ms_nc_cl_g = paste("reg_ms_nc_cl_g_",outcome, sep="")
#   reg_ms_c_g = paste("reg_ms_c_g_",outcome, sep="")
#   reg_ms_c_cl_g = paste("reg_ms_c_cl_g_",outcome, sep="")
#   
#   if (grepl("total_pills_dispensed",outcome)) {
#     title = "Pills dispensed"
#   } else if (grepl("total_pats",outcome)) {
#     title = "Unique patients"
#   } else if (grepl("total_claims",outcome)){
#     title = "Total claims"
#   } else if (grepl("total_money",outcome)){
#     title = "Total money"
#   } else if (grepl("avg_age",outcome)) {
#     title = "Average age"
#   } else if (grepl("avg_copay",outcome)){
#     title = "Average copay"
#   } else if (grepl("total_new_pats",outcome)){
#     title = "New patients"
#   } else if (grepl("mean_mpr",outcome)){
#     title = "Med. Possesion Ratio"
#   }
#   
#   ####histogram of outcome####
#   hist = ggplot(data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                         & opener_zip_dummy ==1 & usable ==1 
#                                         & zip_ym_ins_53_index == 1
#                                         #                                           & insurance_type_53 == INS
#                                         ,]) +
#     geom_histogram(aes(x=get(outcome))) +
#     theme_bw() +  xlab("") +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title,"(log)",sep="\n")) 
#   print(hist)
#   
#   hist = ggplot(data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                         & opener_zip_dummy ==1 & usable ==1 
#                                         & zip_ym_ins_53_index == 1
#                                         #                                           & insurance_type_53 == INS
#                                         ,]) +
#     geom_histogram(aes(x=exp(get(outcome)))) +
#     theme_bw() +  xlab("") +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title,"(raw)",sep="\n")) 
#   print(hist)
#   
#   
#   
#   
#   ####no controls####
#   assign(eval(reg_ms_nc) , lm(get(outcome) ~ 
#                                 (ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0
#                                  + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6):as.factor(insurance_type_53) 
#                               + as.factor(insurance_type_53)
#                               + zip_factor + zip_factor:yrmon_number + yrmon_factor 
#                               ,
#                               #weights =weight,
#                               data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                                       & opener_zip_dummy ==1 & usable ==1 
#                                                       & zip_ym_ins_53_index == 1
#                                                       & is.finite(get(outcome))
#                                                       #                                                         & insurance_type_53 == INS
#                                                       ,]))
#   
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_nc_cl) , coeftest(get(reg_ms_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                     & opener_zip_dummy ==1 & usable ==1 
#                                     & zip_ym_ins_53_index == 1
#                                     & is.finite(get(outcome))
#                                     #                                       & insurance_type_53 == INS
#                                     ,]$ZIP)))
#   
#   coefs = names(get(reg_ms_nc_cl)[,1])
#   print(get(reg_ms_nc_cl)[grepl("ms",coefs),1])
#   print(nobs(get(reg_ms_nc)))
#   print(coefs[grepl("ms",coefs)])
#   
#   
#   ins_type_ins_53_list_int_nc = coefs[grepl("ms",coefs)]
#   ins_type_coef_list_int_nc = get(reg_ms_nc_cl)[grepl("ms",coefs),1]
#   ins_type_se_list_int_nc = get(reg_ms_nc_cl)[grepl("ms",coefs),2]
#   ins_type_p_list_int_nc = get(reg_ms_nc_cl)[grepl("ms",coefs),4]
#   
#   ins_53_ms_split_int_nc = data.table(ins_53 = unlist(ins_type_ins_53_list_int_nc),
#                                       coef = unlist(ins_type_coef_list_int_nc),
#                                       se = unlist(ins_type_se_list_int_nc),
#                                       p = unlist(ins_type_p_list_int_nc))
#   
#   ins_53_ms_split_int_nc[, ms := ifelse(grepl("neg6", ins_53), -6,
#                                         ifelse(grepl("neg5", ins_53), -5,
#                                                ifelse(grepl("neg4", ins_53), -4,
#                                                       ifelse(grepl("neg3", ins_53), -3,
#                                                              ifelse(grepl("neg2", ins_53), -2,
#                                                                     ifelse(grepl("_0", ins_53), 0,
#                                                                            ifelse(grepl("ms_1", ins_53), 1,
#                                                                                   ifelse(grepl("_2", ins_53), 2,
#                                                                                          ifelse(grepl("_3", ins_53), 3,
#                                                                                                 ifelse(grepl("_4", ins_53), 4,
#                                                                                                        ifelse(grepl("ms_5", ins_53), 5,
#                                                                                                               ifelse(grepl("_6", ins_53), 6,
#                                                                                                                      NA))))))))))))]
#   ins_53_ms_split_int_nc[,ins_type := ifelse(ms < 0, substr(ins_53,37,46),substr(ins_53,34,43))]
#   
#   ins_53_ms_split_int_nc[, ymin := coef - 1.96 * se]
#   ins_53_ms_split_int_nc[, ymax := coef + 1.96 * se]
#   
#   
#   
#   
#   
#   ####with controls####
#   assign(eval(reg_ms_c) , lm(get(outcome) ~ 
#                                (ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0
#                                 + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6):as.factor(insurance_type_53) 
#                              + as.factor(insurance_type_53)
#                              + zip_factor + zip_factor:yrmon_number + yrmon_factor  
#                              + avg_copay_ym_ins_53_z + avg_age_ym_ins_53_z
#                              #                                + total_health_estabs + total_estabs
#                              + tot_pharms_neg_1
#                              + mail_order_users_per_tot_pats_neg_1
#                              ,
#                              #weights =weight,
#                              data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                                      & opener_zip_dummy ==1 & usable ==1 
#                                                      & zip_ym_ins_53_index == 1
#                                                      & is.finite(get(outcome))
#                                                      #                                                        & insurance_type_53 == INS
#                                                      ,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_c_cl) , coeftest(get(reg_ms_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                     & opener_zip_dummy ==1 & usable ==1 
#                                     & zip_ym_ins_53_index == 1
#                                     & is.finite(get(outcome))
#                                     #                                       & insurance_type_53 == INS
#                                     ,]$ZIP)))
#   
#   coefs = names(get(reg_ms_c_cl)[,1])
#   print(get(reg_ms_c_cl)[grepl("ms",coefs),1])
#   print(nobs(get(reg_ms_c)))
#   print(coefs[grepl("ms",coefs)])
#   
#   
#   ins_type_ins_53_list_int_c = coefs[grepl("ms",coefs)]
#   ins_type_coef_list_int_c = get(reg_ms_c_cl)[grepl("ms",coefs),1]
#   ins_type_se_list_int_c = get(reg_ms_c_cl)[grepl("ms",coefs),2]
#   ins_type_p_list_int_c = get(reg_ms_c_cl)[grepl("ms",coefs),4]
#   
#   ins_53_ms_split_int_c = data.table(ins_53 = unlist(ins_type_ins_53_list_int_c),
#                                      coef = unlist(ins_type_coef_list_int_c),
#                                      se = unlist(ins_type_se_list_int_c),
#                                      p = unlist(ins_type_p_list_int_c))
#   
#   ins_53_ms_split_int_c[, ms := ifelse(grepl("neg6", ins_53), -6,
#                                        ifelse(grepl("neg5", ins_53), -5,
#                                               ifelse(grepl("neg4", ins_53), -4,
#                                                      ifelse(grepl("neg3", ins_53), -3,
#                                                             ifelse(grepl("neg2", ins_53), -2,
#                                                                    ifelse(grepl("_0", ins_53), 0,
#                                                                           ifelse(grepl("ms_1", ins_53), 1,
#                                                                                  ifelse(grepl("_2", ins_53), 2,
#                                                                                         ifelse(grepl("_3", ins_53), 3,
#                                                                                                ifelse(grepl("_4", ins_53), 4,
#                                                                                                       ifelse(grepl("ms_5", ins_53), 5,
#                                                                                                              ifelse(grepl("_6", ins_53), 6,
#                                                                                                                     NA))))))))))))]
#   ins_53_ms_split_int_c[,ins_type := ifelse(ms < 0, substr(ins_53,37,46),substr(ins_53,34,43))]
#   
#   ins_53_ms_split_int_c[, ymin := coef - 1.96 * se]
#   ins_53_ms_split_int_c[, ymax := coef + 1.96 * se]
#   
#   
#   for (INS_53 in sort(unique(ins_53_ms_split_int_nc$ins_type))) {
#     #       INS_53 = "C+ADV+MC"
#     #add in 0 at negative 1
#     add0 = data.table(ins_type = INS_53,
#                       coef = 0,
#                       ins_53 = INS_53,
#                       ms = -1)
#     ins_53_ms_split_int_nc = rbindlist(list(ins_53_ms_split_int_nc, add0), fill=T)
#     
#     ####plot###
#     plot_nc = ggplot(data = ins_53_ms_split_int_nc[ins_type == INS_53,]) +
#       geom_point(aes(y=coef,x=ms),size = 8) +
#       geom_line(aes(y=coef,x=ms)) +
#       geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#       geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .15) +
#       theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#       scale_x_continuous(breaks = seq(-6,6,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title," \n(no controls)\n",ins_53_ms_split_int_nc[ins_type == INS_53,]$ins_type,sep=""))
#     print(plot_nc)
#     
#     
#     ins_53_ms_split_int_c = rbindlist(list(ins_53_ms_split_int_c, add0), fill=T)
#     
#     ####plot###
#     plot_c = ggplot(data = ins_53_ms_split_int_c[ins_type == INS_53,]) +
#       geom_point(aes(y=coef,x=ms),size = 8) +
#       geom_line(aes(y=coef,x=ms)) +
#       geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#       geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .15) +
#       theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#       scale_x_continuous(breaks = seq(-6,6,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title," \n(with controls)\n",ins_53_ms_split_int_c[ins_type == INS_53,]$ins_type,sep=""))
#     print(plot_c)
#   }
#   
# }
# 
# dev.off()
# 
# 
# ####post regs####
# for (outcome in c("log_total_pills_dispensed_ym_z","log_total_pats_ym_z","log_total_claims_ym_z",
#                   "log_avg_age_ym_z", "log_total_new_pats_ym_z_plus_1","share_medicaid_ym_z",
#                   "avg_distance_ym_z","log_avg_distance_ym_z_plus_1"
# )) {
#   #   outcome = "log_total_pills_dispensed_ym_z"
#   reg_post_nc = paste("reg_post_nc_",outcome, sep="")
#   reg_post_nc_cl = paste("reg_post_nc_cl_",outcome, sep="")
#   reg_post_c = paste("reg_post_c_",outcome, sep="")
#   reg_post_c_cl = paste("reg_post_c_cl_",outcome, sep="")
#   
#   reg_post_nc_a = paste("reg_post_nc_a_",outcome, sep="")
#   reg_post_nc_a_cl = paste("reg_post_nc_a_cl_",outcome, sep="")
#   reg_post_c_a = paste("reg_post_c_a_",outcome, sep="")
#   reg_post_c_a_cl = paste("reg_post_c_a_cl_",outcome, sep="")
#   
#   reg_pharms_nc = paste("reg_pharms_nc_",outcome, sep="")
#   reg_pharms_nc_cl = paste("reg_pharms_nc_cl_",outcome, sep="")
#   reg_pharms_c = paste("reg_pharms_c_",outcome, sep="")
#   reg_pharms_c_cl = paste("reg_pharms_c_cl_",outcome, sep="")
#   
#   reg_pharmspost_nc = paste("reg_pharmspost_nc_",outcome, sep="")
#   reg_pharmspost_nc_cl = paste("reg_pharmspost_nc_cl_",outcome, sep="")
#   reg_pharmspost_c = paste("reg_pharmspost_c_",outcome, sep="")
#   reg_pharmspost_c_cl = paste("reg_pharmspost_c_cl_",outcome, sep="")
#   
#   
#   ####no controls####
#   assign(eval(reg_post_nc) , lm(get(outcome) ~ 
#                                   post
#                                 #                                 tot_pharms_ym_z
#                                 + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                 ,
#                                 #                                 #weights =weight,
#                                 data = zip_data_stacked[ms <= 6 & ms >= -6 & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_post_nc_cl) , coeftest(get(reg_post_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]$ZIP)))
#   
#   print(get(reg_post_nc_cl)[1:2,])
#   print(nobs(get(reg_post_nc)))
#   
#   ####with controls####
#   assign(eval(reg_post_c) , lm(get(outcome) ~ 
#                                  post
#                                #                                tot_pharms_ym_z
#                                + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                + avg_copay_ym_z + avg_age_ym_z
#                                #                                + total_health_estabs + total_estabs
#                                + tot_pharms_neg_1
#                                + mail_order_users_per_tot_pats_neg_1
#                                + share_medicaid_neg_1
#                                ,
#                                #weights =weight,
#                                data = zip_data_stacked[ms <= 6 & ms >= -6 & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_post_c_cl) , coeftest(get(reg_post_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]$ZIP)))
#   
#   print(get(reg_post_c_cl)[1:2,])
#   print(nobs(get(reg_post_c)))
#   
#   
#   
#   ####no controls all others at neg 1####
#   assign(eval(reg_post_nc_a) , lm(get(outcome) ~ 
#                                     post
#                                   + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                   ,
#                                   #weights =weight,
#                                   data = zip_data_stacked[ ms <= 6 & ms >= -6 & zip_ym_index == 1
#                                                            & (usable == 1 | non_opener == 1),]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_post_nc_a_cl) , coeftest(get(reg_post_nc_a), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 & zip_ym_index == 1
#                                     & (usable == 1 | non_opener == 1),]$ZIP)))
#   
#   print(get(reg_post_nc_a_cl)[1:2,])
#   print(nobs(get(reg_post_nc_a)))
#   
#   ####with controls all others at neg 1####
#   assign(eval(reg_post_c_a) , lm(get(outcome) ~ 
#                                    post
#                                  + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                  + avg_copay_ym_z + avg_age_ym_z
#                                  #                                  + total_health_estabs + total_estabs
#                                  + tot_pharms_neg_1
#                                  + mail_order_users_per_tot_pats_neg_1
#                                  + share_medicaid_neg_1
#                                  ,
#                                  #weights =weight,
#                                  data = zip_data_stacked[ ms <= 6 & ms >= -6 & zip_ym_index == 1
#                                                           & (usable == 1 | non_opener == 1),]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_post_c_a_cl) , coeftest(get(reg_post_c_a), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 & zip_ym_index == 1
#                                     & (usable == 1 | non_opener == 1),]$ZIP)))
#   
#   print(get(reg_post_c_a_cl)[1:2,])
#   print(nobs(get(reg_post_c_a)))
#   
#   
#   
#   
#   ####num pharms####
#   ####no controls####
#   assign(eval(reg_pharms_nc) , lm(get(outcome) ~ 
#                                     tot_pharms_ym_z
#                                   #                                 tot_pharms_ym_z
#                                   + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                   ,
#                                   #weights =weight,
#                                   data = zip_data_stacked[ms <= 6 & ms >= -6 & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_pharms_nc_cl) , coeftest(get(reg_pharms_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]$ZIP)))
#   
#   print(get(reg_pharms_nc_cl)[1:2,])
#   print(nobs(get(reg_pharms_nc)))
#   
#   ####with controls####
#   assign(eval(reg_pharms_c) , lm(get(outcome) ~ 
#                                    tot_pharms_ym_z
#                                  #                                tot_pharms_ym_z
#                                  + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                  + avg_copay_ym_z + avg_age_ym_z
#                                  #                                + total_health_estabs + total_estabs
#                                  + tot_pharms_neg_1
#                                  + mail_order_users_per_tot_pats_neg_1
#                                  + share_medicaid_neg_1
#                                  ,
#                                  #weights =weight,
#                                  data = zip_data_stacked[ms <= 6 & ms >= -6 & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_pharms_c_cl) , coeftest(get(reg_pharms_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]$ZIP)))
#   
#   print(get(reg_pharms_c_cl)[1:2,])
#   print(nobs(get(reg_pharms_c)))
#   
#   
#   ####num pharms interacted with post####
#   ####no controls####
#   assign(eval(reg_pharmspost_nc) , lm(get(outcome) ~ 
#                                         as.factor(tot_pharms_neg_1):post
#                                       #                                 tot_pharms_ym_z
#                                       + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                       ,
#                                       #weights =weight,
#                                       data = zip_data_stacked[ms <= 6 & ms >= -6 & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_pharmspost_nc_cl) , coeftest(get(reg_pharmspost_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]$ZIP)))
#   
#   print(get(reg_pharmspost_nc_cl))
#   print(nobs(get(reg_pharmspost_nc)))
#   
#   ####with controls####
#   assign(eval(reg_pharmspost_c) , lm(get(outcome) ~ 
#                                        tot_pharms_neg_1:post
#                                      #                                tot_pharms_ym_z
#                                      + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                      + avg_copay_ym_z + avg_age_ym_z
#                                      #                                + total_health_estabs + total_estabs
#                                      #                                  + tot_pharms_neg_1
#                                      + mail_order_users_per_tot_pats_neg_1
#                                      + share_medicaid_neg_1
#                                      ,
#                                      #weights =weight,
#                                      data = zip_data_stacked[ms <= 6 & ms >= -6 & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_pharmspost_c_cl) , coeftest(get(reg_pharmspost_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]$ZIP)))
#   
#   print(get(reg_pharmspost_c_cl)[1:2,])
#   print(nobs(get(reg_pharmspost_c)))
#   
#   
#   
# }
# 
# 
# ####tables####
# for (outcome in c("log_total_pills_dispensed_ym_z","log_total_pats_ym_z","log_total_claims_ym_z", 
#                   "log_total_new_pats_ym_z_plus_1","share_medicaid_ym_z",
#                   "avg_distance_ym_z","log_avg_distance_ym_z_plus_1"
# )) {
#   # outcome = "total_pills_dispensed_ym_z"
#   reg_post_nc = paste("reg_post_nc_",outcome, sep="")
#   reg_post_nc_cl = paste("reg_post_nc_cl_",outcome, sep="")
#   reg_post_c = paste("reg_post_c_",outcome, sep="")
#   reg_post_c_cl = paste("reg_post_c_cl_",outcome, sep="")
#   
#   reg_post_nc_a = paste("reg_post_nc_a_",outcome, sep="")
#   reg_post_nc_a_cl = paste("reg_post_nc_a_cl_",outcome, sep="")
#   reg_post_c_a = paste("reg_post_c_a_",outcome, sep="")
#   reg_post_c_a_cl = paste("reg_post_c_a_cl_",outcome, sep="")
#   
#   reg_pharms_nc = paste("reg_pharms_nc_",outcome, sep="")
#   reg_pharms_nc_cl = paste("reg_pharms_nc_cl_",outcome, sep="")
#   reg_pharms_c = paste("reg_pharms_c_",outcome, sep="")
#   reg_pharms_c_cl = paste("reg_pharms_c_cl_",outcome, sep="")
#   
#   print(outcome)
#   stargazer(get(reg_post_nc), get(reg_post_c), get(reg_post_nc_a), get(reg_post_c_a),get(reg_pharms_nc), get(reg_pharms_c),
#             no.space=T,
#             out = paste("~/current/pharmacy_deserts/post_regs_",outcome,"_no_weight.tex",sep=""),
#             omit.stat = c("rsq", "f","adj.rsq","ser"),
#             dep.var.labels = c("Post Opening","Post Opening","Post Opening","Post Opening","Number of Pharmacies","Number of Pharmacies"),
#             dep.var.caption="",
#             covariate.labels = c(outcome),
#             model.numbers=F,
#             multicolumn=T,
#             model.names=F,
#             header=F,
#             keep = c("post","pharms_y"),
#             align =T,
#             font.size = "footnotesize",
#             column.sep.width = "1pt",
#             p = list(get(reg_post_nc_cl)[,4], get(reg_post_c_cl)[,4], 
#                      get(reg_post_nc_a_cl)[,4],get(reg_post_c_a_cl)[,4],
#                      get(reg_pharms_nc_cl)[,4], get(reg_pharms_c_cl)[,4]),
#             se = list(get(reg_post_nc_cl)[,2], get(reg_post_c_cl)[,2], 
#                       get(reg_post_nc_a_cl)[,2],get(reg_post_c_a_cl)[,2],
#                       get(reg_pharms_nc_cl)[,2], get(reg_pharms_c_cl)[,2]),
#             add.lines = list(c("Controls", 
#                                "\\multicolumn{1}{c}{N}","\\multicolumn{1}{c}{Y}",
#                                "\\multicolumn{1}{c}{N}","\\multicolumn{1}{c}{Y}",
#                                "\\multicolumn{1}{c}{N}","\\multicolumn{1}{c}{Y}"),
#                              c("All Zips",
#                                "\\multicolumn{1}{c}{N}","\\multicolumn{1}{c}{N}",
#                                "\\multicolumn{1}{c}{Y}","\\multicolumn{1}{c}{Y}",
#                                "\\multicolumn{1}{c}{N}","\\multicolumn{1}{c}{N}"))
#   )
# }
# 
# 
# ####closings####
# ####post regs####
# for (outcome in c("log_total_pills_dispensed_ym_z","log_total_pats_ym_z","log_total_claims_ym_z",
#                   "log_avg_age_ym_z", "log_total_new_pats_ym_z_plus_1","share_medicaid_ym_z",
#                   "avg_distance_ym_z","log_avg_distance_ym_z_plus_1"
# )) {
#   #   outcome = "log_total_pills_dispensed_ym_z"
#   reg_post_nc = paste("reg_post_nc_",outcome, sep="")
#   reg_post_nc_cl = paste("reg_post_nc_cl_",outcome, sep="")
#   reg_post_c = paste("reg_post_c_",outcome, sep="")
#   reg_post_c_cl = paste("reg_post_c_cl_",outcome, sep="")
#   
#   reg_post_nc_a = paste("reg_post_nc_a_",outcome, sep="")
#   reg_post_nc_a_cl = paste("reg_post_nc_a_cl_",outcome, sep="")
#   reg_post_c_a = paste("reg_post_c_a_",outcome, sep="")
#   reg_post_c_a_cl = paste("reg_post_c_a_cl_",outcome, sep="")
#   
#   reg_pharms_nc = paste("reg_pharms_nc_",outcome, sep="")
#   reg_pharms_nc_cl = paste("reg_pharms_nc_cl_",outcome, sep="")
#   reg_pharms_c = paste("reg_pharms_c_",outcome, sep="")
#   reg_pharms_c_cl = paste("reg_pharms_c_cl_",outcome, sep="")
#   
#   
#   ####no controls####
#   assign(eval(reg_post_nc) , lm(get(outcome) ~ 
#                                   post
#                                 #                                 tot_pharms_ym_z
#                                 + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                 ,
#                                 #                                 #weights =weight,
#                                 data = zip_data_stacked_closings[msc <= 6 & msc >= -6 & closer_zip_dummy ==1 & usable_c ==1 & zip_ym_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_post_nc_cl) , coeftest(get(reg_post_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked_closings[msc <= 6 & msc >= -6 & closer_zip_dummy ==1 & usable_c ==1 & zip_ym_index == 1,]$ZIP)))
#   
#   print(get(reg_post_nc_cl)[1:2,])
#   print(nobs(get(reg_post_nc)))
#   
#   ####with controls####
#   assign(eval(reg_post_c) , lm(get(outcome) ~ 
#                                  post
#                                #                                tot_pharms_ym_z
#                                + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                + avg_copay_ym_z + avg_age_ym_z
#                                #                                + total_health_estabs + total_estabs
#                                + tot_pharms_neg_1c
#                                + mail_order_users_per_tot_pats_neg_1c
#                                + share_medicaid_neg_1c
#                                ,
#                                #weights =weight,
#                                data = zip_data_stacked_closings[msc <= 6 & msc >= -6 & closer_zip_dummy ==1 & usable_c ==1 & zip_ym_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_post_c_cl) , coeftest(get(reg_post_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked_closings[msc <= 6 & msc >= -6 & closer_zip_dummy ==1 & usable_c ==1 & zip_ym_index == 1,]$ZIP)))
#   
#   print(get(reg_post_c_cl)[1:2,])
#   print(nobs(get(reg_post_c)))
#   
#   
#   
#   ####no controls all others at neg 1####
#   assign(eval(reg_post_nc_a) , lm(get(outcome) ~ 
#                                     post
#                                   + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                   ,
#                                   #weights =weight,
#                                   data = zip_data_stacked_closings[ msc <= 6 & msc >= -6 & zip_ym_index == 1
#                                                                     & (usable_c == 1 | non_closer == 1),]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_post_nc_a_cl) , coeftest(get(reg_post_nc_a), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked_closings[msc <= 6 & msc >= -6 & zip_ym_index == 1
#                                              & (usable_c == 1 | non_closer == 1),]$ZIP)))
#   
#   print(get(reg_post_nc_a_cl)[1:2,])
#   print(nobs(get(reg_post_nc_a)))
#   
#   ####with controls all others at neg 1####
#   assign(eval(reg_post_c_a) , lm(get(outcome) ~ 
#                                    post
#                                  + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                  + avg_copay_ym_z + avg_age_ym_z
#                                  #                                  + total_health_estabs + total_estabs
#                                  + tot_pharms_neg_1c
#                                  + mail_order_users_per_tot_pats_neg_1c
#                                  + share_medicaid_neg_1c
#                                  ,
#                                  #weights =weight,
#                                  data = zip_data_stacked_closings[ msc <= 6 & msc >= -6 & zip_ym_index == 1
#                                                                    & (usable_c == 1 | non_closer == 1),]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_post_c_a_cl) , coeftest(get(reg_post_c_a), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked_closings[msc <= 6 & msc >= -6 & zip_ym_index == 1
#                                              & (usable_c == 1 | non_closer == 1),]$ZIP)))
#   
#   print(get(reg_post_c_a_cl)[1:2,])
#   print(nobs(get(reg_post_c_a)))
#   
#   
#   
#   
#   ####num pharms####
#   ####no controls####
#   assign(eval(reg_pharms_nc) , lm(get(outcome) ~ 
#                                     tot_pharms_ym_z
#                                   #                                 tot_pharms_ym_z
#                                   + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                   ,
#                                   #weights =weight,
#                                   data = zip_data_stacked_closings[msc <= 6 & msc >= -6 & closer_zip_dummy ==1 & usable_c ==1 & zip_ym_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_pharms_nc_cl) , coeftest(get(reg_pharms_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked_closings[msc <= 6 & msc >= -6 & closer_zip_dummy ==1 & usable_c ==1 & zip_ym_index == 1,]$ZIP)))
#   
#   print(get(reg_pharms_nc_cl)[1:2,])
#   print(nobs(get(reg_pharms_nc)))
#   
#   ####with controls####
#   assign(eval(reg_pharms_c) , lm(get(outcome) ~ 
#                                    tot_pharms_ym_z
#                                  #                                tot_pharms_ym_z
#                                  + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                  + avg_copay_ym_z + avg_age_ym_z
#                                  #                                + total_health_estabs + total_estabs
#                                  + tot_pharms_neg_1c
#                                  + mail_order_users_per_tot_pats_neg_1c
#                                  + share_medicaid_neg_1c
#                                  ,
#                                  #weights =weight,
#                                  data = zip_data_stacked_closings[msc <= 6 & msc >= -6 & closer_zip_dummy ==1 & usable_c ==1 & zip_ym_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_pharms_c_cl) , coeftest(get(reg_pharms_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked_closings[msc <= 6 & msc >= -6 & closer_zip_dummy ==1 & usable_c ==1 & zip_ym_index == 1,]$ZIP)))
#   
#   print(get(reg_pharms_c_cl)[1:2,])
#   print(nobs(get(reg_pharms_c)))
#   
#   
#   
#   
# }
# 
# 
# ####tables####
# for (outcome in c("log_total_pills_dispensed_ym_z","log_total_pats_ym_z","log_total_claims_ym_z", 
#                   "log_total_new_pats_ym_z_plus_1","share_medicaid_ym_z",
#                   "avg_distance_ym_z","log_avg_distance_ym_z_plus_1"
# )) {
#   # outcome = "total_pills_dispensed_ym_z"
#   reg_post_nc = paste("reg_post_nc_",outcome, sep="")
#   reg_post_nc_cl = paste("reg_post_nc_cl_",outcome, sep="")
#   reg_post_c = paste("reg_post_c_",outcome, sep="")
#   reg_post_c_cl = paste("reg_post_c_cl_",outcome, sep="")
#   
#   reg_post_nc_a = paste("reg_post_nc_a_",outcome, sep="")
#   reg_post_nc_a_cl = paste("reg_post_nc_a_cl_",outcome, sep="")
#   reg_post_c_a = paste("reg_post_c_a_",outcome, sep="")
#   reg_post_c_a_cl = paste("reg_post_c_a_cl_",outcome, sep="")
#   
#   reg_pharms_nc = paste("reg_pharms_nc_",outcome, sep="")
#   reg_pharms_nc_cl = paste("reg_pharms_nc_cl_",outcome, sep="")
#   reg_pharms_c = paste("reg_pharms_c_",outcome, sep="")
#   reg_pharms_c_cl = paste("reg_pharms_c_cl_",outcome, sep="")
#   
#   print(outcome)
#   stargazer(get(reg_post_nc), get(reg_post_c), get(reg_post_nc_a), get(reg_post_c_a),get(reg_pharms_nc), get(reg_pharms_c),
#             no.space=T,
#             out = paste("~/current/pharmacy_deserts/post_regs_",outcome,"_closings_no_weight.tex",sep=""),
#             omit.stat = c("rsq", "f","adj.rsq","ser"),
#             dep.var.labels = c("Post Closing","Post Closing","Post Closing","Post Closing","Number of Pharmacies","Number of Pharmacies"),
#             dep.var.caption="",
#             covariate.labels = c(outcome),
#             model.numbers=F,
#             multicolumn=T,
#             model.names=F,
#             header=F,
#             keep = c("post","pharms_y"),
#             align =T,
#             font.size = "footnotesize",
#             column.sep.width = "1pt",
#             p = list(get(reg_post_nc_cl)[,4], get(reg_post_c_cl)[,4], 
#                      get(reg_post_nc_a_cl)[,4],get(reg_post_c_a_cl)[,4],
#                      get(reg_pharms_nc_cl)[,4], get(reg_pharms_c_cl)[,4]),
#             se = list(get(reg_post_nc_cl)[,2], get(reg_post_c_cl)[,2], 
#                       get(reg_post_nc_a_cl)[,2],get(reg_post_c_a_cl)[,2],
#                       get(reg_pharms_nc_cl)[,2], get(reg_pharms_c_cl)[,2]),
#             add.lines = list(c("Controls", 
#                                "\\multicolumn{1}{c}{N}","\\multicolumn{1}{c}{Y}",
#                                "\\multicolumn{1}{c}{N}","\\multicolumn{1}{c}{Y}",
#                                "\\multicolumn{1}{c}{N}","\\multicolumn{1}{c}{Y}"),
#                              c("All Zips",
#                                "\\multicolumn{1}{c}{N}","\\multicolumn{1}{c}{N}",
#                                "\\multicolumn{1}{c}{Y}","\\multicolumn{1}{c}{Y}",
#                                "\\multicolumn{1}{c}{N}","\\multicolumn{1}{c}{N}"))
#   )
# }
# 
# 
# 
# 
# ####post regs rxclass control####
# for (outcome in c("log_total_pills_dispensed_ym_rx_z","log_total_pats_ym_rx_z","log_total_claims_ym_rx_z",
#                   "log_avg_age_ym_rx_z","log_avg_copay_ym_rx_z_plus_1", "log_total_new_pats_ym_rx_z_plus_1",
#                   "avg_distance_ym_rx_z","log_avg_distance_ym_rx_z_plus_1")) {
#   # outcome = "total_pills_dispensed_ym_rx_z"
#   reg_post_nc = paste("reg_post_nc_",outcome, sep="")
#   reg_post_nc_cl = paste("reg_post_nc_cl_",outcome, sep="")
#   reg_post_c = paste("reg_post_c_",outcome, sep="")
#   reg_post_c_cl = paste("reg_post_c_cl_",outcome, sep="")
#   
#   reg_post_nc_a = paste("reg_post_nc_a_",outcome, sep="")
#   reg_post_nc_a_cl = paste("reg_post_nc_a_cl_",outcome, sep="")
#   reg_post_c_a = paste("reg_post_c_a_",outcome, sep="")
#   reg_post_c_a_cl = paste("reg_post_c_a_cl_",outcome, sep="")
#   
#   reg_pharms_nc = paste("reg_pharms_nc_",outcome, sep="")
#   reg_pharms_nc_cl = paste("reg_pharms_nc_cl_",outcome, sep="")
#   reg_pharms_c = paste("reg_pharms_c_",outcome, sep="")
#   reg_pharms_c_cl = paste("reg_pharms_c_cl_",outcome, sep="")
#   
#   ####no controls####
#   assign(eval(reg_post_nc) , lm(get(outcome) ~ 
#                                   post
#                                 + zip_factor + zip_factor:yrmon_number + yrmon_factor + as.factor(rx2)
#                                 ,
#                                 #weights =weight,
#                                 data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                                         & is.finite(get(outcome))
#                                                         & opener_zip_dummy ==1 
#                                                         & usable ==1 
#                                                         & zip_ym_rx_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_post_nc_cl) , coeftest(get(reg_post_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                     & is.finite(get(outcome))
#                                     & opener_zip_dummy ==1 
#                                     & usable ==1 
#                                     & zip_ym_rx_index == 1,]$ZIP)))
#   
#   print(get(reg_post_nc_cl)[1:2,])
#   print(nobs(get(reg_post_nc)))
#   
#   ####with controls####
#   assign(eval(reg_post_c) , lm(get(outcome) ~ 
#                                  post
#                                + zip_factor + zip_factor:yrmon_number + yrmon_factor  + as.factor(rx2)
#                                + avg_copay_ym_z + avg_age_ym_z
#                                # + total_health_estabs + total_estabs
#                                + tot_pharms_neg_1
#                                + mail_order_users_per_tot_pats_neg_1
#                                ,
#                                #weights =weight,
#                                data = zip_data_stacked[ ms <= 6 & ms >= -6 
#                                                         & is.finite(get(outcome))
#                                                         & opener_zip_dummy ==1 
#                                                         & usable ==1 
#                                                         & zip_ym_rx_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_post_c_cl) , coeftest(get(reg_post_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                     & is.finite(get(outcome))
#                                     & opener_zip_dummy ==1 
#                                     & usable ==1 
#                                     & zip_ym_rx_index == 1,]$ZIP)))
#   
#   print(get(reg_post_c_cl)[1:2,])
#   print(nobs(get(reg_post_c)))
#   
#   
#   
#   ####total pharms####
#   ####no controls####
#   assign(eval(reg_pharms_nc) , lm(get(outcome) ~ 
#                                     tot_pharms_ym_z
#                                   + zip_factor + zip_factor:yrmon_number + yrmon_factor + as.factor(rx2)
#                                   ,
#                                   #weights =weight,
#                                   data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                                           & is.finite(get(outcome))
#                                                           & opener_zip_dummy ==1 
#                                                           & usable ==1 
#                                                           & zip_ym_rx_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_pharms_nc_cl) , coeftest(get(reg_pharms_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                     & is.finite(get(outcome))
#                                     & opener_zip_dummy ==1 
#                                     & usable ==1 
#                                     & zip_ym_rx_index == 1,]$ZIP)))
#   
#   print(get(reg_pharms_nc_cl)[1:2,])
#   print(nobs(get(reg_pharms_nc)))
#   
#   ####with controls####
#   assign(eval(reg_pharms_c) , lm(get(outcome) ~ 
#                                    tot_pharms_ym_z
#                                  + zip_factor + zip_factor:yrmon_number + yrmon_factor  + as.factor(rx2)
#                                  + avg_copay_ym_z + avg_age_ym_z
#                                  # + total_health_estabs + total_estabs
#                                  + tot_pharms_neg_1
#                                  + mail_order_users_per_tot_pats_neg_1
#                                  ,
#                                  #weights =weight,
#                                  data = zip_data_stacked[ ms <= 6 & ms >= -6 
#                                                           & is.finite(get(outcome))
#                                                           & opener_zip_dummy ==1 
#                                                           & usable ==1 
#                                                           & zip_ym_rx_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_pharms_c_cl) , coeftest(get(reg_pharms_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                     & is.finite(get(outcome))
#                                     & opener_zip_dummy ==1 
#                                     & usable ==1 
#                                     & zip_ym_rx_index == 1,]$ZIP)))
#   
#   print(get(reg_pharms_c_cl)[1:2,])
#   print(nobs(get(reg_pharms_c)))
#   
#   
#   
#   
#   #   ####no controls all others at neg 1####
#   #   assign(eval(reg_post_nc_a) , lm(get(outcome) ~ 
#   #                                     post
#   #                                   + zip_factor + zip_factor:yrmon_number + yrmon_factor + as.factor(rx2)
#   #                                   ,
#   #                                   data = zip_data_stacked[zip_ms_index>=1 & ms <= 6 & ms >= -6 & zip_ym_rx_index == 1,]))
#   #   
#   #   #cluster standard errors at the zip code level
#   #   assign(eval(reg_post_nc_a_cl) , coeftest(get(reg_post_nc_a), vcov=function(x) 
#   #     cluster.vcov(x,zip_data_stacked[zip_ms_index>=1 & ms <= 6 & ms >= -6 & zip_ym_rx_index == 1,]$ZIP)))
#   #   
#   #   print(get(reg_post_nc_a_cl)[1:2,])
#   #   print(nobs(get(reg_post_nc_a)))
#   #   
#   #   ####with controls all others at neg 1####
#   #   assign(eval(reg_post_c_a) , lm(get(outcome) ~ 
#   #                                    post
#   #                                  + zip_factor + zip_factor:yrmon_number + yrmon_factor + as.factor(rx2)
#   #                                  + avg_copay_ym_z + avg_age_ym_z
#   #                                  # + total_health_estabs + total_estabs
#   #                                  + tot_pharms_neg_1
#   #                                  + mail_order_users_per_tot_pats_neg_1
#   #                                  ,
#   #                                  data = zip_data_stacked[ms <= 6 & ms >= -6 & zip_ym_rx_index == 1,]))
#   #   
#   #   #cluster standard errors at the zip code level
#   #   assign(eval(reg_post_c_a_cl) , coeftest(get(reg_post_c_a), vcov=function(x) 
#   #     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 & zip_ym_rx_index == 1,]$ZIP)))
#   #   
#   #   print(get(reg_post_c_a_cl)[1:2,])
#   #   print(nobs(get(reg_post_c_a)))
#   
# }
# 
# 
# ####tables rx control####
# for (outcome in c("log_total_pills_dispensed_ym_rx_z","log_total_pats_ym_rx_z","log_total_claims_ym_rx_z",
#                   "log_avg_age_ym_rx_z","log_avg_copay_ym_rx_z_plus_1", "log_total_new_pats_ym_rx_z_plus_1",
#                   "avg_distance_ym_rx_z","log_avg_distance_ym_rx_z_plus_1")) {
#   # outcome = "total_pills_dispensed_ym_rx_z"
#   reg_post_nc = paste("reg_post_nc_",outcome, sep="")
#   reg_post_nc_cl = paste("reg_post_nc_cl_",outcome, sep="")
#   reg_post_c = paste("reg_post_c_",outcome, sep="")
#   reg_post_c_cl = paste("reg_post_c_cl_",outcome, sep="")
#   
#   reg_post_nc_a = paste("reg_post_nc_a_",outcome, sep="")
#   reg_post_nc_a_cl = paste("reg_post_nc_a_cl_",outcome, sep="")
#   reg_post_c_a = paste("reg_post_c_a_",outcome, sep="")
#   reg_post_c_a_cl = paste("reg_post_c_a_cl_",outcome, sep="")
#   
#   reg_pharms_nc = paste("reg_pharms_nc_",outcome, sep="")
#   reg_pharms_nc_cl = paste("reg_pharms_nc_cl_",outcome, sep="")
#   reg_pharms_c = paste("reg_pharms_c_",outcome, sep="")
#   reg_pharms_c_cl = paste("reg_pharms_c_cl_",outcome, sep="")
#   
#   print(outcome)
#   stargazer(get(reg_post_nc), get(reg_post_c), get(reg_pharms_nc), get(reg_pharms_c), #get(reg_post_nc_a), get(reg_post_c_a),
#             no.space=T,
#             out = paste("~/current/pharmacy_deserts/post_regs_",outcome,"_rx2_control_no_weight.tex",sep=""),
#             omit.stat = c("rsq", "f","adj.rsq","ser"),
#             dep.var.labels = c("Post Opening","Post Opening","Num. Pharms","Num. Pharms"),#"Post Opening","Post Opening"),
#             dep.var.caption="",
#             covariate.labels = c(outcome),
#             model.numbers=F,
#             multicolumn=T,
#             model.names=F,
#             header=F,
#             keep = c("post","pharms_y"),
#             align =T,
#             font.size = "footnotesize",
#             column.sep.width = "1pt",
#             p = list(get(reg_post_nc_cl)[,4], get(reg_post_c_cl)[,4],
#                      get(reg_pharms_nc_cl)[,4], get(reg_pharms_c_cl)[,4]),# get(reg_post_nc_a_cl)[,4],get(reg_post_c_a_cl)[,4]),
#             se = list(get(reg_post_nc_cl)[,2], get(reg_post_c_cl)[,2],
#                       get(reg_pharms_nc_cl)[,2], get(reg_pharms_c_cl)[,2]),# get(reg_post_nc_a_cl)[,2],get(reg_post_c_a_cl)[,2]),
#             add.lines = list(c("Controls", 
#                                "\\multicolumn{1}{c}{N}","\\multicolumn{1}{c}{Y}",
#                                "\\multicolumn{1}{c}{N}","\\multicolumn{1}{c}{Y}"),#"\\multicolumn{1}{c}{N}","\\multicolumn{1}{c}{Y}"),
#                              c("All Zips", 
#                                "\\multicolumn{1}{c}{N}","\\multicolumn{1}{c}{N}", 
#                                "\\multicolumn{1}{c}{N}","\\multicolumn{1}{c}{N}")) #,"\\multicolumn{1}{c}{Y}","\\multicolumn{1}{c}{Y}"))
#   )
# }
# 
# 
# ####post regs rxclass split####
# load("~/current/pharmacy_deserts/data/rx2_maintenance_data.RData") #maintenance level for each drug
# 
# pdf(file = paste("~/current/pharmacy_deserts/post_rx2_split_coefs_no_weight_",B,".pdf",sep=""), width = 18, height = 10)
# for (outcome in c("log_total_pills_dispensed_ym_rx_z_plus_1","log_total_pats_ym_rx_z","log_total_claims_ym_rx_z",
#                   #                   "log_avg_age_ym_rx_z", 
#                   "log_avg_copay_ym_rx_z_plus_1",
#                   "log_total_new_pats_ym_rx_z_plus_1","log_avg_distance_ym_rx_z_plus_1")) {
#   #       outcome = "log_total_pats_ym_rx_z"
#   #   outcome = "log_avg_copay_ym_rx_z_plus_1"
#   
#   if (grepl("total_pills_dispensed",outcome) & !(grepl("ring",outcome))) {
#     title = "Pills dispensed"
#   } else if (grepl("total_pats",outcome) & !(grepl("ring",outcome))) {
#     title = "Unique patients"
#   } else if (grepl("total_claims",outcome) & !(grepl("ring",outcome)) & !(grepl("inzip",outcome))){
#     title = "Total claims"
#   } else if (grepl("ring_mean_total_pills_dispensed",outcome)) {
#     title = "Ring Pills dispensed"
#   } else if (grepl("ring_mean_total_pats",outcome)) {
#     title = "Ring Unique patients"
#   } else if (grepl("ring_mean_total_claims",outcome)){
#     title = "Ring Total claims"
#   } else if (grepl("total_money",outcome)){
#     title = "Total money"
#   } else if (grepl("avg_age",outcome)) {
#     title = "Average age"
#   } else if (grepl("avg_copay",outcome)){
#     title = "Average copay"
#   } else if (grepl("total_new_pats",outcome)){
#     title = "New patients"
#   } else if (grepl("mean_mpr",outcome)){
#     title = "Med. Possesion Ratio"
#   } else if (grepl("share_medicaid",outcome)){
#     title = "Medicaid Share of Patients"
#   } else if (grepl("share_claims_inzip",outcome)) {
#     title = "Share of in-zip claims"
#   } else if (grepl("total_claims_inzip",outcome)) {
#     title = "Total in-zip claims"
#   } else if (grepl("total_unique_zips",outcome)) {
#     title = "Unique Pharmacy Zips"
#   } else if (grepl("log_avg_distance",outcome)) {
#     title = "Log Avg. Distance"
#   } else if (grepl("avg_distance",outcome)) {
#     title = "Avg. Distance"
#   }
#   
#   
#   pill_type_coef_list_int_c = list()
#   pill_type_se_list_int_c = list()
#   pill_type_p_list_int_c = list()
#   pill_type_rx_list_int_c = list()
#   
#   pill_type_coef_list_int_nc = list()
#   pill_type_se_list_int_nc = list()
#   pill_type_p_list_int_nc = list()
#   pill_type_rx_list_int_nc = list()
#   
#   
#   
#   
#   #INTERACTIONS
#   # run the interaction regressions
#   
#   reg_post_int_nc = paste("reg_post_int_nc_",outcome, sep="")
#   reg_post_int_nc_cl = paste("reg_post_int_nc_cl_",outcome, sep="")
#   reg_post_int_c = paste("reg_post_int_c_",outcome, sep="")
#   reg_post_int_c_cl = paste("reg_post_int_c_cl_",outcome, sep="")
#   
#   
#   ####no controls####
#   assign(eval(reg_post_int_nc) , lm(get(outcome) ~ 
#                                       post:rx2 + rx2 
#                                     + zip_factor + zip_factor:yrmon_number + yrmon_factor 
#                                     ,
#                                     #weights =weight,
#                                     data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                                             & opener_zip_dummy ==1 & usable ==1 
#                                                             & zip_ym_rx_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_post_int_nc_cl) , coeftest(get(reg_post_int_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                     & opener_zip_dummy ==1 & usable ==1
#                                     & zip_ym_rx_index == 1,]$ZIP)))
#   
#   coefs = names(get(reg_post_int_nc_cl)[,1])
#   print(get(reg_post_int_nc_cl)[grepl("post",coefs),1])
#   print(nobs(get(reg_post_int_nc)))
#   print(coefs[grepl("post",coefs)])
#   
#   pill_type_rx_list_int_nc = coefs[grepl("post",coefs)]
#   pill_type_coef_list_int_nc = get(reg_post_int_nc_cl)[grepl("post",coefs),1]
#   pill_type_se_list_int_nc = get(reg_post_int_nc_cl)[grepl("post",coefs),2]
#   pill_type_p_list_int_nc = get(reg_post_int_nc_cl)[grepl("post",coefs),4]
#   
#   rx_post_split_int_nc = data.table(rx = as.integer(gsub("post:rx2","",unlist(pill_type_rx_list_int_nc))),
#                                     coef = unlist(pill_type_coef_list_int_nc),
#                                     se = unlist(pill_type_se_list_int_nc),
#                                     p = unlist(pill_type_p_list_int_nc))
#   
#   ####save the coef for later
#   opening_effects_coef_nc = paste(outcome, "opening_effects_coef_nc",sep="_")
#   assign(eval(opening_effects_coef_nc), data.table(rx = as.integer(gsub("post:rx2","",unlist(pill_type_rx_list_int_nc))),
#                                                    coef = unlist(pill_type_coef_list_int_nc),
#                                                    outcome = outcome))
#   
#   #merge on the rxclass label
#   rxclass_label = as.data.table(read.csv("~/current/pharmacy_deserts/data/rxclass_labels.csv"))
#   setkey(rxclass_label, rx2)
#   setkey(rx_post_split_int_nc, rx)
#   rx_post_split_int_nc[rxclass_label, rx_label := label]
#   
#   rx_post_split_int_nc[, ymin := coef - 1.96*se]
#   rx_post_split_int_nc[, ymax := coef + 1.96*se]
#   
#   rx_post_split_int_nc[, sig := ifelse(p <= .05, "Significant", "Not Significant")]
#   
#   setkey(rx_post_split_int_nc, coef)
#   rx_post_split_int_nc[, rank := seq_len(.N)]
#   
#   rx_post_split_int_nc[coef >= 0 , min_pos_rank := min(rank)]
#   
#   rx_post_split_int_nc[, mean_coef := NULL]
#   rx_post_split_int_nc[, mean_coef := mean(coef)]
#   
#   #histogram of effects
#   nc_hist_plot = ggplot(data = rx_post_split_int_nc) +
#     geom_histogram(aes(x=coef), bins = 150) +
#     geom_vline(aes(xintercept=mean_coef), color="Red") +
#     geom_text(aes(x=mean_coef+0.2, y = 16), label=round(unique(rx_post_split_int_nc$mean_coef)[1],3), size = 10) +
#     theme_bw() + 
#     xlab("Post X Drug Coefficient") + 
#     ggtitle(paste(title, "\n(no controls)",sep="")) + theme(text = element_text(size = 30))
#   print(nc_hist_plot)
#   
#   #rank effects
#   nc_rank_plot = ggplot(data = rx_post_split_int_nc[coef < .5 & coef > -.5]) +
#     geom_point(data = rx_post_split_int_nc[sig == "Not Significant" & coef < .5 & coef > -.5],
#                aes(x=rank, y=coef, color=as.factor(sig), shape=as.factor(sig)), size = 4) +
#     geom_text(data = rx_post_split_int_nc[sig == "Significant" & coef < .5 & coef > -.5],
#               aes(x=rank, y=coef, label=paste(rx_label,round(coef,2),sep=": "), color=as.factor(sig)), angle = 90, size = 5) + 
#     geom_line(aes(x=rank, y=ymin), size = 0.25, linetype="dashed",alpha=.00) +
#     geom_line(aes(x=rank, y=ymax), size = 0.25, linetype="dashed",alpha=.00) +
#     theme_bw() +
#     scale_x_reverse() +
#     scale_color_discrete(guide = guide_legend(title = "p <= .05", label.theme = element_text(angle = 90, size = 30),
#                                               title.theme = element_text(angle = 90, size = 30),
#                                               ncol = 1,
#                                               direction = "horizontal",
#                                               label.vjust = 0.5,
#                                               label.hjust = 0.5,
#                                               title.hjust = 0.5,
#                                               label.position = "top")) + 
#     guides(shape=F)  +
#     #   scale_color_gradient(low="blue",high="red") + 
#     geom_vline(aes(xintercept = min_pos_rank-0.5)) + 
#     geom_text(aes(x=min_pos_rank + 1, y = .2), label="Coef = 0", size = 10, angle = 90) +
#     ylab(paste("Outcome:", title, "(no controls)",sep=" ")) +
#     theme(text = element_text(size = 30),
#           axis.text.x = element_text(angle = 90),
#           axis.title.x = element_text(angle = 180),
#           axis.text.y = element_text(angle = 90)) 
#   print(nc_rank_plot)
#   
#   #no text plot
#   nc_rank_plot = ggplot(data = rx_post_split_int_nc[coef < 1 & coef > -1]) +
#     geom_point(aes(x=rank, y=coef, color=as.factor(sig), shape=as.factor(sig)), size = 4) +
#     #   geom_text(aes(x=rank, y=coef, label=paste(rx_label,round(coef,2),sep=": "), color=as.factor(sig)), angle = 90, size = 6.5) + 
#     geom_line(aes(x=rank, y=ymin), size = 0.25, linetype="dashed",alpha=.75) +
#     geom_line(aes(x=rank, y=ymax), size = 0.25, linetype="dashed",alpha=.75) +
#     theme_bw() +
#     #     scale_x_reverse() +
#     scale_color_discrete(guide = guide_legend(title = "", label.theme = element_text(angle = 0, size = 30),
#                                               title.theme = element_text(angle = 0, size = 30),
#                                               ncol = 2,
#                                               direction = "horizontal",
#                                               label.vjust = 0.5,
#                                               label.hjust = 0.5,
#                                               title.hjust = 0.5,
#                                               label.position = "right")) + 
#     theme(legend.position = "bottom") + 
#     guides(shape=F)  +
#     scale_shape(guide = "none") + 
#     geom_vline(aes(xintercept = min_pos_rank-0.5)) + 
#     geom_text(data=NULL,aes(x=min_pos_rank + 1, y = 0.2), label="Coef = 0", size = 10, angle = 90) +
#     ylab(paste("Outcome:", title, "(no controls)",sep=" ")) + 
#     theme(text = element_text(size = 30),
#           axis.text.y = element_text(angle = 90)) 
#   print(nc_rank_plot)
#   #   save(rx_post_split_int_nc, file="~/current/pharmacy_deserts/data/rx_post_split_int_nc.RData")
#   
#   #are the effects correlated with the maintenance data?
#   setkey(rx_post_split_int_nc, rx)
#   
#   load("~/current/pharmacy_deserts/data/rx2_maintenance_data.RData")
#   rx2_maintenance_data[, rx := as.numeric(rx2)]
#   setkey(rx2_maintenance_data, rx)
#   rx_post_split_int_nc[rx2_maintenance_data, mean_fills_per_patient_rx2 := mean_fills_per_patient_rx2]
#   
#   maintenance_plot = ggplot(data = rx_post_split_int_nc[coef < .5 & coef > -.5]) +
#     geom_point(aes(x=mean_fills_per_patient_rx2, y=coef), size = 4.5) +
#     geom_smooth(aes(x=mean_fills_per_patient_rx2, y=coef), se=F, method="lm") +
#     #     geom_errorbar(aes(x=mean_fills_per_patient_rx2, ymin=ymin, ymax=ymax), width=.1, alpha=.5) +
#     theme_bw() +
#     ggtitle(paste("Outcome:", title, "\n(no controls)",sep=" ")) +
#     xlab("Mean Fills per Person") + ylab("Coefficient") +
#     theme(text = element_text(size = 40))
#   print(maintenance_plot)
#   
#   # reg = lm(coef ~ mean_fills_per_patient_rx2,
#   #          data = rx_post_split_int_nc)
#   # summary(reg)
#   
#   
#   #are the effects correlated with the drug story data?
#   setkey(rx_post_split_int_nc, rx)
#   
#   drug_type_story = as.data.table(read.csv("~/current/pharmacy_deserts/data/drug_type_story.csv"))
#   setkey(drug_type_story, rx2)
#   rx_post_split_int_nc[drug_type_story, avg_copay_rx2 := avg_copay_rx2]
#   rx_post_split_int_nc[drug_type_story, avg_age_rx2 := avg_age_rx2]
#   rx_post_split_int_nc[drug_type_story, total_fills_rx2 := total_fills_rx2]
#   
#   age_plot = ggplot(data = rx_post_split_int_nc[coef < .5 & coef > -.5]) +
#     geom_point(aes(x=avg_age_rx2, y=coef), size = 4.5) +
#     geom_smooth(aes(x=avg_age_rx2, y=coef), se=F, method="lm") +
#     #     geom_errorbar(aes(x=mean_fills_per_patient_rx2, ymin=ymin, ymax=ymax), width=.1, alpha=.5) +
#     theme_bw() +
#     ggtitle(paste("Outcome:", title, "\n(no controls)",sep=" ")) +
#     xlab("Average Age of Drug Patients") + ylab("Coefficient") +
#     theme(text = element_text(size = 40))
#   print(age_plot)
#   
#   copay_plot = ggplot(data = rx_post_split_int_nc[coef < .5 & coef > -.5]) +
#     geom_point(aes(x=avg_copay_rx2, y=coef), size = 4.5) +
#     geom_smooth(aes(x=avg_copay_rx2, y=coef), se=F, method="lm") +
#     #     geom_errorbar(aes(x=mean_fills_per_patient_rx2, ymin=ymin, ymax=ymax), width=.1, alpha=.5) +
#     theme_bw() +
#     ggtitle(paste("Outcome:", title, "\n(no controls)",sep=" ")) +
#     xlab("Average Drug Copay") + ylab("Coefficient") +
#     theme(text = element_text(size = 40))
#   print(copay_plot)
#   
#   fills_plot = ggplot(data = rx_post_split_int_nc[coef < .5 & coef > -.5]) +
#     geom_point(aes(x=total_fills_rx2, y=coef), size = 4.5) +
#     geom_smooth(aes(x=total_fills_rx2, y=coef), se=F, method="lm") +
#     #     geom_errorbar(aes(x=mean_fills_per_patient_rx2, ymin=ymin, ymax=ymax), width=.1, alpha=.5) +
#     theme_bw() +
#     ggtitle(paste("Outcome:", title, "\n(no controls)",sep=" ")) +
#     xlab("Total Drug Fills") + ylab("Coefficient") +
#     theme(text = element_text(size = 40))
#   print(fills_plot)
#   
#   
#   ####with controls####
#   assign(eval(reg_post_int_c) , lm(get(outcome) ~ 
#                                      post:rx2 + rx2 
#                                    + zip_factor + zip_factor:yrmon_number + yrmon_factor 
#                                    + avg_copay_ym_rx_z + avg_age_ym_rx_z
#                                    #                                + total_health_estabs + total_estabs
#                                    + tot_pharms_neg_1
#                                    + mail_order_users_per_tot_pats_neg_1
#                                    + share_medicaid_neg_1
#                                    ,
#                                    #weights =weight,
#                                    data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                                            & opener_zip_dummy ==1 & usable ==1 
#                                                            & zip_ym_rx_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_post_int_c_cl) , coeftest(get(reg_post_int_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                     & opener_zip_dummy ==1 & usable ==1
#                                     & zip_ym_rx_index == 1,]$ZIP)))
#   
#   coefs = names(get(reg_post_int_c_cl)[,1])
#   print(get(reg_post_int_c_cl)[grepl("post",coefs),1])
#   print(nobs(get(reg_post_int_c)))
#   print(coefs[grepl("post",coefs)])
#   
#   pill_type_rx_list_int_c = coefs[grepl("post",coefs)]
#   pill_type_coef_list_int_c = get(reg_post_int_c_cl)[grepl("post",coefs),1]
#   pill_type_se_list_int_c = get(reg_post_int_c_cl)[grepl("post",coefs),2]
#   pill_type_p_list_int_c = get(reg_post_int_c_cl)[grepl("post",coefs),4]
#   
#   rx_post_split_int_c = data.table(rx = as.integer(gsub("post:rx2","",unlist(pill_type_rx_list_int_c))),
#                                    coef = unlist(pill_type_coef_list_int_c),
#                                    se = unlist(pill_type_se_list_int_c),
#                                    p = unlist(pill_type_p_list_int_c))
#   
#   ####save the coef for later
#   opening_effects_coef_c = paste(outcome, "opening_effects_coef_c",sep="_")
#   assign(eval(opening_effects_coef_c), data.table(rx = as.integer(gsub("post:rx2","",unlist(pill_type_rx_list_int_c))),
#                                                   coef = unlist(pill_type_coef_list_int_c),
#                                                   outcome = outcome))
#   
#   #merge on the rxclass label
#   rxclass_label = as.data.table(read.csv("~/current/pharmacy_deserts/data/rxclass_labels.csv"))
#   setkey(rxclass_label, rx2)
#   setkey(rx_post_split_int_c, rx)
#   rx_post_split_int_c[rxclass_label, rx_label := label]
#   
#   rx_post_split_int_c[, ymin := coef - 1.96*se]
#   rx_post_split_int_c[, ymax := coef + 1.96*se]
#   
#   rx_post_split_int_c[, sig := ifelse(p <= .05, "Significant", "Not Significant")]
#   
#   setkey(rx_post_split_int_c, coef)
#   rx_post_split_int_c[, rank := seq_len(.N)]
#   
#   rx_post_split_int_c[coef >= 0 , min_pos_rank := min(rank)]
#   
#   rx_post_split_int_c[, mean_coef := NULL]
#   rx_post_split_int_c[, mean_coef := mean(coef)]
#   
#   #histogram of effects
#   c_hist_plot = ggplot(data = rx_post_split_int_c) +
#     geom_histogram(aes(x=coef), bins = 150) +
#     geom_vline(aes(xintercept=mean_coef), color="Red") +
#     geom_text(aes(x=mean_coef+0.1, y = 12), label=round(unique(rx_post_split_int_c$mean_coef)[1],3), size = 10) +
#     theme_bw() + 
#     xlab("Post X Drug Coefficient") + 
#     ggtitle(paste(title, "\n(with controls)",sep="")) + theme(text = element_text(size = 30))
#   print(c_hist_plot)
#   
#   #rank effects
#   c_rank_plot = ggplot(data = rx_post_split_int_c[coef < 1 & coef > -1]) +
#     geom_point(data = rx_post_split_int_c[sig == "Not Significant" & coef < 1 & coef > -1],
#                aes(x=rank, y=coef, color=as.factor(sig), shape=as.factor(sig)), size = 4) +
#     geom_text(data = rx_post_split_int_c[sig == "Significant" & coef < 1 & coef > -1],
#               aes(x=rank, y=coef, label=paste(rx_label,round(coef,2),sep=": "), color=as.factor(sig)), angle = 90, size = 5) + 
#     geom_line(aes(x=rank, y=ymin), size = 0.25, linetype="dashed",alpha=.00) +
#     geom_line(aes(x=rank, y=ymax), size = 0.25, linetype="dashed",alpha=.00) +
#     theme_bw() +
#     scale_x_reverse() +
#     scale_color_discrete(guide = guide_legend(title = "p <= .05", label.theme = element_text(angle = 90, size = 30),
#                                               title.theme = element_text(angle = 90, size = 30),
#                                               col = 1,
#                                               direction = "horizontal",
#                                               label.vjust = 0.5,
#                                               label.hjust = 0.5,
#                                               title.hjust = 0.5,
#                                               label.position = "top")) + 
#     guides(shape=F)  +
#     #   scale_color_gradient(low="blue",high="red") + 
#     geom_vline(aes(xintercept = min_pos_rank-0.5)) + 
#     geom_text(aes(x=min_pos_rank + 1, y = 0.5), label="Coef = 0", size = 10, angle = 90) +
#     ylab(paste("Outcome:", title, "(with controls)",sep=" ")) +
#     theme(text = element_text(size = 30),
#           axis.text.x = element_text(angle = 90),
#           axis.title.x = element_text(angle = 180),
#           axis.text.y = element_text(angle = 90)) 
#   print(c_rank_plot)
#   
#   #No text plot
#   c_rank_plot = ggplot(data = rx_post_split_int_c[coef < 1 & coef > -1]) +
#     geom_point(aes(x=rank, y=coef, color=as.factor(sig), shape=as.factor(sig)), size = 4) +
#     #   geom_text(aes(x=rank, y=coef, label=paste(rx_label,round(coef,2),sep=": "), color=as.factor(sig)), angle = 90, size = 6.5) + 
#     geom_line(aes(x=rank, y=ymin), size = 0.25, linetype="dashed",alpha=.75) +
#     geom_line(aes(x=rank, y=ymax), size = 0.25, linetype="dashed",alpha=.75) +
#     theme_bw() +
#     scale_x_reverse() +
#     scale_color_discrete(guide = guide_legend(title = "p <= .05", label.theme = element_text(angle = 90, size = 30),
#                                               title.theme = element_text(angle = 90, size = 30),
#                                               col = 1,
#                                               direction = "horizontal",
#                                               label.vjust = 0.5,
#                                               label.hjust = 0.5,
#                                               title.hjust = 0.5,
#                                               label.position = "top")) + 
#     guides(shape=F)  +
#     scale_shape(guide = "none") + 
#     geom_vline(aes(xintercept = min_pos_rank-0.5)) + 
#     geom_text(aes(x=min_pos_rank + 1, y = 0.5), label="Coef = 0", size = 10, angle = 90) +
#     ylab(paste("Outcome:", title, "(with controls)",sep=" ")) + theme(text = element_text(size = 30),
#                                                                       axis.text.x = element_text(angle = 90),
#                                                                       axis.title.x = element_text(angle = 180),
#                                                                       axis.text.y = element_text(angle = 90)) 
#   print(c_rank_plot)
#   
#   #are the effects correlated with the maintenance data?
#   setkey(rx_post_split_int_c, rx)
#   rx2_maintenance_data[, rx := as.numeric(rx2)]
#   setkey(rx2_maintenance_data, rx)
#   rx_post_split_int_c[rx2_maintenance_data, mean_fills_per_patient_rx2 := mean_fills_per_patient_rx2]
#   
#   maintenance_plot = ggplot(data = rx_post_split_int_c[coef < .5 & coef > -.5]) +
#     geom_point(aes(x=mean_fills_per_patient_rx2, y=coef), size = 3) +
#     geom_smooth(aes(x=mean_fills_per_patient_rx2, y=coef), se=F, method="lm") +
#     #     geom_errorbar(aes(x=mean_fills_per_patient_rx2, ymin=ymin, ymax=ymax), width=.1, alpha=.5) +
#     theme_bw() +
#     ggtitle(paste("Outcome:", title, "\n(with controls)",sep=" ")) +
#     xlab("Mean Fills per Person") + ylab("Coefficient") +
#     theme(text = element_text(size = 40))
#   print(maintenance_plot)
#   
# }
# dev.off()
# 
# 
# 
# 
# 
# 
# #####POST INSURANCE TYPE####
# 
# ####post regs insurance type 8 control####
# for (outcome in c("log_total_pills_dispensed_ym_ins_8_z","log_total_pats_ym_ins_8_z","log_total_claims_ym_ins_8_z",
#                   "log_total_new_pats_ym_ins_8_z_plus_1")) {
#   # outcome = "total_pills_dispensed_ym_rx_z"
#   reg_post_nc = paste("reg_post_nc_",outcome, sep="")
#   reg_post_nc_cl = paste("reg_post_nc_cl_",outcome, sep="")
#   reg_post_c = paste("reg_post_c_",outcome, sep="")
#   reg_post_c_cl = paste("reg_post_c_cl_",outcome, sep="")
#   
#   
#   ####no controls####
#   assign(eval(reg_post_nc) , lm(get(outcome) ~ 
#                                   post
#                                 + zip_factor + zip_factor:yrmon_number + yrmon_factor + as.factor(insurance_type_8)
#                                 ,
#                                 #weights =weight,
#                                 data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                                         & is.finite(get(outcome))
#                                                         & opener_zip_dummy ==1 
#                                                         & usable ==1 
#                                                         & zip_ym_ins_8_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_post_nc_cl) , coeftest(get(reg_post_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                     & is.finite(get(outcome))
#                                     & opener_zip_dummy ==1 
#                                     & usable ==1 
#                                     & zip_ym_ins_8_index == 1,]$ZIP)))
#   
#   print(get(reg_post_nc_cl)[1:2,])
#   print(nobs(get(reg_post_nc)))
#   
#   ####with controls####
#   assign(eval(reg_post_c) , lm(get(outcome) ~ 
#                                  post
#                                + zip_factor + zip_factor:yrmon_number + yrmon_factor  + insurance_type_8
#                                + avg_copay_ym_z + avg_age_ym_z
#                                # + total_health_estabs + total_estabs
#                                + tot_pharms_neg_1
#                                + mail_order_users_per_tot_pats_neg_1
#                                ,
#                                #weights =weight,
#                                data = zip_data_stacked[ ms <= 6 & ms >= -6 
#                                                         & is.finite(get(outcome))
#                                                         & opener_zip_dummy ==1 
#                                                         & usable ==1 
#                                                         & zip_ym_ins_8_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_post_c_cl) , coeftest(get(reg_post_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                     & is.finite(get(outcome))
#                                     & opener_zip_dummy ==1 
#                                     & usable ==1 
#                                     & zip_ym_ins_8_index == 1,]$ZIP)))
#   
#   print(get(reg_post_c_cl)[1:2,])
#   print(nobs(get(reg_post_c)))
#   
#   
# }
# 
# 
# ####tables insurance type 8 control####
# for (outcome in c("log_total_pills_dispensed_ym_ins_8_z","log_total_pats_ym_ins_8_z","log_total_claims_ym_ins_8_z",
#                   "log_total_new_pats_ym_ins_8_z_plus_1")) {
#   # outcome = "total_pills_dispensed_ym_rx_z"
#   reg_post_nc = paste("reg_post_nc_",outcome, sep="")
#   reg_post_nc_cl = paste("reg_post_nc_cl_",outcome, sep="")
#   reg_post_c = paste("reg_post_c_",outcome, sep="")
#   reg_post_c_cl = paste("reg_post_c_cl_",outcome, sep="")
#   
#   
#   print(outcome)
#   stargazer(get(reg_post_nc), get(reg_post_c), #get(reg_pharms_nc), get(reg_pharms_c), #get(reg_post_nc_a), get(reg_post_c_a),
#             no.space=T,
#             out = paste("~/current/pharmacy_deserts/post_regs_",outcome,"_ins8_control_no_weight.tex",sep=""),
#             omit.stat = c("rsq", "f","adj.rsq","ser"),
#             dep.var.labels = c("Post Opening","Post Opening"),#"Post Opening","Post Opening"),
#             dep.var.caption="",
#             covariate.labels = c(outcome),
#             model.numbers=F,
#             multicolumn=T,
#             model.names=F,
#             header=F,
#             keep = c("post","pharms_y"),
#             align =T,
#             font.size = "footnotesize",
#             column.sep.width = "1pt",
#             p = list(get(reg_post_nc_cl)[,4], get(reg_post_c_cl)[,4]),# get(reg_post_nc_a_cl)[,4],get(reg_post_c_a_cl)[,4]),
#             se = list(get(reg_post_nc_cl)[,2], get(reg_post_c_cl)[,2]),# get(reg_post_nc_a_cl)[,2],get(reg_post_c_a_cl)[,2]),
#             add.lines = list(c("Controls", 
#                                "\\multicolumn{1}{c}{N}","\\multicolumn{1}{c}{Y}"),#"\\multicolumn{1}{c}{N}","\\multicolumn{1}{c}{Y}"),
#                              c("All Zips", 
#                                "\\multicolumn{1}{c}{N}","\\multicolumn{1}{c}{N}")) #,"\\multicolumn{1}{c}{Y}","\\multicolumn{1}{c}{Y}"))
#   )
# }
# 
# 
# ####post regs ins8 split####
# 
# pdf(file = paste("~/current/pharmacy_deserts/post_ins8_split_coefs_no_weight_",B,".pdf",sep=""), width = 18, height = 10)
# for (outcome in c("log_total_pills_dispensed_ym_ins_8_z","log_total_pats_ym_ins_8_z","log_total_claims_ym_ins_8_z",
#                   "log_total_new_pats_ym_ins_8_z_plus_1")) {
#   #       outcome = "log_total_pills_dispensed_ym_ins_8_z"
#   
#   if (grepl("total_pills_dispensed",outcome) & !(grepl("ring",outcome))) {
#     title = "Pills dispensed"
#   } else if (grepl("total_pats",outcome) & !(grepl("ring",outcome))) {
#     title = "Unique patients"
#   } else if (grepl("total_claims",outcome) & !(grepl("ring",outcome)) & !(grepl("inzip",outcome))){
#     title = "Total claims"
#   } else if (grepl("ring_mean_total_pills_dispensed",outcome)) {
#     title = "Ring Pills dispensed"
#   } else if (grepl("ring_mean_total_pats",outcome)) {
#     title = "Ring Unique patients"
#   } else if (grepl("ring_mean_total_claims",outcome)){
#     title = "Ring Total claims"
#   } else if (grepl("total_money",outcome)){
#     title = "Total money"
#   } else if (grepl("avg_age",outcome)) {
#     title = "Average age"
#   } else if (grepl("avg_copay",outcome)){
#     title = "Average copay"
#   } else if (grepl("total_new_pats",outcome)){
#     title = "New patients"
#   } else if (grepl("mean_mpr",outcome)){
#     title = "Med. Possesion Ratio"
#   } else if (grepl("share_medicaid",outcome)){
#     title = "Medicaid Share of Patients"
#   } else if (grepl("share_claims_inzip",outcome)) {
#     title = "Share of in-zip claims"
#   } else if (grepl("total_claims_inzip",outcome)) {
#     title = "Total in-zip claims"
#   } else if (grepl("total_unique_zips",outcome)) {
#     title = "Unique Pharmacy Zips"
#   } else if (grepl("log_avg_distance",outcome)) {
#     title = "Log Avg. Distance"
#   } else if (grepl("avg_distance",outcome)) {
#     title = "Avg. Distance"
#   }
#   
#   
#   coef_list_int_c = list()
#   se_list_int_c = list()
#   p_list_int_c = list()
#   rx_list_int_c = list()
#   
#   coef_list_int_nc = list()
#   se_list_int_nc = list()
#   p_list_int_nc = list()
#   rx_list_int_nc = list()
#   
#   
#   
#   
#   #INTERACTIONS
#   # run the interaction regressions
#   
#   reg_post_int_nc = paste("reg_post_int_nc_",outcome, sep="")
#   reg_post_int_nc_cl = paste("reg_post_int_nc_cl_",outcome, sep="")
#   reg_post_int_c = paste("reg_post_int_c_",outcome, sep="")
#   reg_post_int_c_cl = paste("reg_post_int_c_cl_",outcome, sep="")
#   
#   
#   ####no controls####
#   assign(eval(reg_post_int_nc) , lm(get(outcome) ~ 
#                                       post:insurance_type_8 + insurance_type_8 
#                                     + zip_factor + zip_factor:yrmon_number + yrmon_factor 
#                                     ,
#                                     #weights =weight,
#                                     data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                                             & is.finite(get(outcome))
#                                                             & opener_zip_dummy ==1 & usable ==1 
#                                                             & zip_ym_ins_8_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_post_int_nc_cl) , coeftest(get(reg_post_int_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                     & is.finite(get(outcome))
#                                     & opener_zip_dummy ==1 & usable ==1
#                                     & zip_ym_ins_8_index == 1,]$ZIP)))
#   
#   coefs = names(get(reg_post_int_nc_cl)[,1])
#   print(get(reg_post_int_nc_cl)[grepl("post",coefs),1])
#   print(nobs(get(reg_post_int_nc)))
#   print(coefs[grepl("post",coefs)])
#   
#   rx_list_int_nc = coefs[grepl("post",coefs)]
#   coef_list_int_nc = get(reg_post_int_nc_cl)[grepl("post",coefs),1]
#   se_list_int_nc = get(reg_post_int_nc_cl)[grepl("post",coefs),2]
#   p_list_int_nc = get(reg_post_int_nc_cl)[grepl("post",coefs),4]
#   
#   rx_post_split_int_nc = data.table(rx = as.character(gsub("post:insurance_type_8","",unlist(rx_list_int_nc))),
#                                     coef = unlist(coef_list_int_nc),
#                                     se = unlist(se_list_int_nc),
#                                     p = unlist(p_list_int_nc))
#   
#   ####save the coef for later
#   opening_effects_coef_nc = paste(outcome, "opening_effects_coef_nc",sep="_")
#   assign(eval(opening_effects_coef_nc), data.table(rx = as.character(gsub("post:insurance_type_8","",unlist(rx_list_int_nc))),
#                                                    coef = unlist(coef_list_int_nc),
#                                                    outcome = outcome))
#   
#   
#   
#   rx_post_split_int_nc[, ymin := coef - 1.96*se]
#   rx_post_split_int_nc[, ymax := coef + 1.96*se]
#   
#   rx_post_split_int_nc[, sig := ifelse(p <= .05, "Significant", "Not Significant")]
#   
#   setkey(rx_post_split_int_nc, coef)
#   rx_post_split_int_nc[, rank := seq_len(.N)]
#   
#   rx_post_split_int_nc[coef >= 0 , min_pos_rank := min(rank)]
#   
#   rx_post_split_int_nc[, mean_coef := NULL]
#   rx_post_split_int_nc[, mean_coef := mean(coef)]
#   
#   
#   #bar effects
#   nc_bar_plot = ggplot(data = rx_post_split_int_nc[rx != "UNK"]) +
#     geom_bar(aes(x=reorder(rx,rank), y=coef), fill="Blue",color="Black", stat="identity", alpha = .4) +
#     geom_errorbar(aes(x=reorder(rx,rank), ymin=ymin, ymax=ymax), width = 0.1) +
#     theme_bw() +
#     theme(legend.position = "none") +
#     ggtitle(paste("Outcome:", title, "\n(no controls)","\nN = ",nobs(get(reg_post_int_nc)),sep=" ")) + 
#     xlab("") + ylab("Coefficient") + 
#     theme(text = element_text(size = 50),
#           axis.text.x = element_text(angle = 40,
#                                      vjust = 0.5,
#                                      size = 50),
#           axis.text.y = element_text(angle = 90,
#                                      size = 50)) 
#   print(nc_bar_plot)
#   
#   #merge on the insurance type story data
#   setkey(rx_post_split_int_nc, rx)
#   
#   ins8_type_story = as.data.table(read.csv("~/current/pharmacy_deserts/data/ins8_type_story.csv"))
#   setkey(ins8_type_story, insurance_type_8)
#   rx_post_split_int_nc[ins8_type_story, total_fills_ins8 := total_fills_ins8 ]
#   rx_post_split_int_nc[ins8_type_story, avg_copay_ins8 := avg_copay_ins8 ]
#   rx_post_split_int_nc[ins8_type_story, avg_age_ins8 := avg_age_ins8 ]
#   
#   age_plot = ggplot(data = rx_post_split_int_nc[coef < .5 & coef > -.5 & rx != "UNK"]) +
#     #     geom_point(aes(x=avg_age_ins8, y=coef, label=rx), size = 4.5) +
#     geom_text(aes(x=avg_age_ins8, y=coef, label=rx), size = 10) +
#     geom_smooth(aes(x=avg_age_ins8, y=coef), se=F, method="lm") +
#     #     geom_errorbar(aes(x=mean_fills_per_patient_rx2, ymin=ymin, ymax=ymax), width=.1, alpha=.5) +
#     theme_bw() +
#     ggtitle(paste("Outcome:", title, "\n(no controls)",sep=" ")) +
#     xlab("Average Age of Insurance Patients") + ylab("Coefficient") +
#     theme(text = element_text(size = 40))
#   print(age_plot)
#   
#   copay_plot = ggplot(data = rx_post_split_int_nc[coef < .5 & coef > -.5 & rx != "UNK"]) +
#     geom_text(aes(x=avg_copay_ins8, y=coef, label=rx), size = 10) +
#     geom_smooth(aes(x=avg_copay_ins8, y=coef), se=F, method="lm") +
#     #     geom_errorbar(aes(x=mean_fills_per_patient_rx2, ymin=ymin, ymax=ymax), width=.1, alpha=.5) +
#     theme_bw() +
#     ggtitle(paste("Outcome:", title, "\n(no controls)",sep=" ")) +
#     xlab("Average Insurance Copay") + ylab("Coefficient") +
#     theme(text = element_text(size = 40))
#   print(copay_plot)
#   
#   fills_plot = ggplot(data = rx_post_split_int_nc[coef < .5 & coef > -.5 & rx != "UNK"]) +
#     geom_text(aes(x=total_fills_ins8, y=coef, label=rx), size = 10) +
#     geom_smooth(aes(x=total_fills_ins8, y=coef), se=F, method="lm") +
#     #     geom_errorbar(aes(x=mean_fills_per_patient_rx2, ymin=ymin, ymax=ymax), width=.1, alpha=.5) +
#     theme_bw() +
#     ggtitle(paste("Outcome:", title, "\n(no controls)",sep=" ")) +
#     xlab("Total Insurance Fills") + ylab("Coefficient") +
#     theme(text = element_text(size = 40))
#   print(fills_plot)
#   
#   
#   
#   
#   ####with controls####
#   assign(eval(reg_post_int_c) , lm(get(outcome) ~ 
#                                      post:insurance_type_8 + insurance_type_8 
#                                    + zip_factor + zip_factor:yrmon_number + yrmon_factor 
#                                    + avg_copay_ym_rx_z + avg_age_ym_rx_z
#                                    #                                + total_health_estabs + total_estabs
#                                    + tot_pharms_neg_1
#                                    + mail_order_users_per_tot_pats_neg_1
#                                    + share_medicaid_neg_1
#                                    ,
#                                    #weights =weight,
#                                    data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                                            & is.finite(get(outcome))
#                                                            & opener_zip_dummy ==1 & usable ==1 
#                                                            & zip_ym_ins_8_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_post_int_c_cl) , coeftest(get(reg_post_int_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                     & is.finite(get(outcome))
#                                     & opener_zip_dummy ==1 & usable ==1
#                                     & zip_ym_ins_8_index == 1,]$ZIP)))
#   
#   
#   coefs = names(get(reg_post_int_c_cl)[,1])
#   print(get(reg_post_int_c_cl)[grepl("post",coefs),1])
#   print(nobs(get(reg_post_int_c)))
#   print(coefs[grepl("post",coefs)])
#   
#   rx_list_int_c = coefs[grepl("post",coefs)]
#   coef_list_int_c = get(reg_post_int_c_cl)[grepl("post",coefs),1]
#   se_list_int_c = get(reg_post_int_c_cl)[grepl("post",coefs),2]
#   p_list_int_c = get(reg_post_int_c_cl)[grepl("post",coefs),4]
#   
#   rx_post_split_int_c = data.table(rx = as.character(gsub("post:insurance_type_8","",unlist(rx_list_int_c))),
#                                    coef = unlist(coef_list_int_c),
#                                    se = unlist(se_list_int_c),
#                                    p = unlist(p_list_int_c))
#   
#   ####save the coef for later
#   opening_effects_coef_c = paste(outcome, "opening_effects_coef_c",sep="_")
#   assign(eval(opening_effects_coef_c), data.table(rx = as.character(gsub("post:insurance_type_8","",unlist(rx_list_int_c))),
#                                                   coef = unlist(coef_list_int_c),
#                                                   outcome = outcome))
#   
#   
#   
#   rx_post_split_int_c[, ymin := coef - 1.96*se]
#   rx_post_split_int_c[, ymax := coef + 1.96*se]
#   
#   rx_post_split_int_c[, sig := ifelse(p <= .05, "Significant", "Not Significant")]
#   
#   setkey(rx_post_split_int_c, coef)
#   rx_post_split_int_c[, rank := seq_len(.N)]
#   
#   rx_post_split_int_c[coef >= 0 , min_pos_rank := min(rank)]
#   
#   rx_post_split_int_c[, mean_coef := NULL]
#   rx_post_split_int_c[, mean_coef := mean(coef)]
#   
#   
#   #bar effects
#   c_bar_plot = ggplot(data = rx_post_split_int_c[rx != "UNK"]) +
#     geom_bar(aes(x=reorder(rx,rank), y=coef), fill="Blue",color="Black", stat="identity", alpha = .4) +
#     geom_errorbar(aes(x=reorder(rx,rank), ymin=ymin, ymax=ymax), width = 0.1) +
#     theme_bw() +
#     theme(legend.position = "none") +
#     ggtitle(paste("Outcome:", title, "\n(with controls)","\nN = ",nobs(get(reg_post_int_c)),sep=" ")) + 
#     xlab("") + ylab("Coefficient") + 
#     theme(text = element_text(size = 50),
#           axis.text.x = element_text(angle = 40,
#                                      vjust = 0.5,
#                                      size = 50),
#           axis.text.y = element_text(angle = 90,
#                                      size = 50)) 
#   print(c_bar_plot)
#   
# }
# dev.off()
# 
# 
# ####post regs ins23 split####
# 
# pdf(file = paste("~/current/pharmacy_deserts/post_ins23_split_coefs_no_weight_",B,".pdf",sep=""), width = 18, height = 10)
# for (outcome in c("log_total_pills_dispensed_ym_ins_23_z","log_total_pats_ym_ins_23_z","log_total_claims_ym_ins_23_z",
#                   "log_total_new_pats_ym_ins_23_z_plus_1")) {
#   #       outcome = "log_total_pills_dispensed_ym_ins_23_z"
#   
#   if (grepl("total_pills_dispensed",outcome) & !(grepl("ring",outcome))) {
#     title = "Pills dispensed"
#   } else if (grepl("total_pats",outcome) & !(grepl("ring",outcome))) {
#     title = "Unique patients"
#   } else if (grepl("total_claims",outcome) & !(grepl("ring",outcome)) & !(grepl("inzip",outcome))){
#     title = "Total claims"
#   } else if (grepl("ring_mean_total_pills_dispensed",outcome)) {
#     title = "Ring Pills dispensed"
#   } else if (grepl("ring_mean_total_pats",outcome)) {
#     title = "Ring Unique patients"
#   } else if (grepl("ring_mean_total_claims",outcome)){
#     title = "Ring Total claims"
#   } else if (grepl("total_money",outcome)){
#     title = "Total money"
#   } else if (grepl("avg_age",outcome)) {
#     title = "Average age"
#   } else if (grepl("avg_copay",outcome)){
#     title = "Average copay"
#   } else if (grepl("total_new_pats",outcome)){
#     title = "New patients"
#   } else if (grepl("mean_mpr",outcome)){
#     title = "Med. Possesion Ratio"
#   } else if (grepl("share_medicaid",outcome)){
#     title = "Medicaid Share of Patients"
#   } else if (grepl("share_claims_inzip",outcome)) {
#     title = "Share of in-zip claims"
#   } else if (grepl("total_claims_inzip",outcome)) {
#     title = "Total in-zip claims"
#   } else if (grepl("total_unique_zips",outcome)) {
#     title = "Unique Pharmacy Zips"
#   } else if (grepl("log_avg_distance",outcome)) {
#     title = "Log Avg. Distance"
#   } else if (grepl("avg_distance",outcome)) {
#     title = "Avg. Distance"
#   }
#   
#   
#   coef_list_int_c = list()
#   se_list_int_c = list()
#   p_list_int_c = list()
#   rx_list_int_c = list()
#   
#   coef_list_int_nc = list()
#   se_list_int_nc = list()
#   p_list_int_nc = list()
#   rx_list_int_nc = list()
#   
#   
#   
#   
#   #INTERACTIONS
#   # run the interaction regressions
#   
#   reg_post_int_nc = paste("reg_post_int_nc_",outcome, sep="")
#   reg_post_int_nc_cl = paste("reg_post_int_nc_cl_",outcome, sep="")
#   reg_post_int_c = paste("reg_post_int_c_",outcome, sep="")
#   reg_post_int_c_cl = paste("reg_post_int_c_cl_",outcome, sep="")
#   
#   
#   ####no controls####
#   assign(eval(reg_post_int_nc) , lm(get(outcome) ~ 
#                                       post:insurance_type_23 + insurance_type_23 
#                                     + zip_factor + zip_factor:yrmon_number + yrmon_factor 
#                                     ,
#                                     #weights =weight,
#                                     data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                                             & is.finite(get(outcome))
#                                                             & opener_zip_dummy ==1 & usable ==1 
#                                                             & zip_ym_ins_23_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_post_int_nc_cl) , coeftest(get(reg_post_int_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                     & is.finite(get(outcome))
#                                     & opener_zip_dummy ==1 & usable ==1
#                                     & zip_ym_ins_23_index == 1,]$ZIP)))
#   
#   coefs = names(get(reg_post_int_nc_cl)[,1])
#   print(get(reg_post_int_nc_cl)[grepl("post",coefs),1])
#   print(nobs(get(reg_post_int_nc)))
#   print(coefs[grepl("post",coefs)])
#   
#   rx_list_int_nc = coefs[grepl("post",coefs)]
#   coef_list_int_nc = get(reg_post_int_nc_cl)[grepl("post",coefs),1]
#   se_list_int_nc = get(reg_post_int_nc_cl)[grepl("post",coefs),2]
#   p_list_int_nc = get(reg_post_int_nc_cl)[grepl("post",coefs),4]
#   
#   rx_post_split_int_nc = data.table(rx = as.character(gsub("post:insurance_type_23","",unlist(rx_list_int_nc))),
#                                     coef = unlist(coef_list_int_nc),
#                                     se = unlist(se_list_int_nc),
#                                     p = unlist(p_list_int_nc))
#   
#   ####save the coef for later
#   opening_effects_coef_nc = paste(outcome, "opening_effects_coef_nc",sep="_")
#   assign(eval(opening_effects_coef_nc), data.table(rx = as.character(gsub("post:insurance_type_23","",unlist(rx_list_int_nc))),
#                                                    coef = unlist(coef_list_int_nc),
#                                                    outcome = outcome))
#   
#   
#   
#   rx_post_split_int_nc[, ymin := coef - 1.96*se]
#   rx_post_split_int_nc[, ymax := coef + 1.96*se]
#   
#   rx_post_split_int_nc[, sig := ifelse(p <= .05, "Significant", "Not Significant")]
#   
#   setkey(rx_post_split_int_nc, coef)
#   rx_post_split_int_nc[, rank := seq_len(.N)]
#   
#   rx_post_split_int_nc[coef >= 0 , min_pos_rank := min(rank)]
#   
#   rx_post_split_int_nc[, mean_coef := NULL]
#   rx_post_split_int_nc[, mean_coef := mean(coef)]
#   
#   
#   #bar effects
#   nc_bar_plot = ggplot(data = rx_post_split_int_nc[rx != "ZZ"]) +
#     geom_bar(aes(x=reorder(rx,rank), y=coef), fill="Blue",color="Black", stat="identity", alpha = .4) +
#     geom_errorbar(aes(x=reorder(rx,rank), ymin=ymin, ymax=ymax), width = 0.1) +
#     theme_bw() +
#     theme(legend.position = "none") +
#     ggtitle(paste("Outcome:", title, "\n(no controls)","\nN = ",nobs(get(reg_post_int_nc)),sep=" ")) + 
#     xlab("") + ylab("Coefficient") + 
#     theme(text = element_text(size = 50),
#           axis.text.x = element_text(angle = 40,
#                                      vjust = 0.5,
#                                      size = 50),
#           axis.text.y = element_text(angle = 90,
#                                      size = 50)) 
#   print(nc_bar_plot)
#   
#   #merge on the insurance type story data
#   setkey(rx_post_split_int_nc, rx)
#   
#   ins23_type_story = as.data.table(read.csv("~/current/pharmacy_deserts/data/ins23_type_story.csv"))
#   setkey(ins23_type_story, insurance_type_23)
#   rx_post_split_int_nc[ins23_type_story, total_fills_ins23 := total_fills_ins23 ]
#   rx_post_split_int_nc[ins23_type_story, avg_copay_ins23 := avg_copay_ins23 ]
#   rx_post_split_int_nc[ins23_type_story, avg_age_ins23 := avg_age_ins23 ]
#   
#   age_plot = ggplot(data = rx_post_split_int_nc[coef < .5 & coef > -.5 & rx != "ZZ"]) +
#     #     geom_point(aes(x=avg_age_ins23, y=coef, label=rx), size = 4.5) +
#     geom_text(aes(x=avg_age_ins23, y=coef, label=rx), size = 10) +
#     geom_smooth(aes(x=avg_age_ins23, y=coef), se=F, method="lm") +
#     #     geom_errorbar(aes(x=mean_fills_per_patient_rx2, ymin=ymin, ymax=ymax), width=.1, alpha=.5) +
#     theme_bw() +
#     ggtitle(paste("Outcome:", title, "\n(no controls)",sep=" ")) +
#     xlab("Average Age of Insurance Patients") + ylab("Coefficient") +
#     theme(text = element_text(size = 40))
#   print(age_plot)
#   
#   copay_plot = ggplot(data = rx_post_split_int_nc[coef < .5 & coef > -.5 & rx != "ZZ"]) +
#     geom_text(aes(x=avg_copay_ins23, y=coef, label=rx), size = 10) +
#     geom_smooth(aes(x=avg_copay_ins23, y=coef), se=F, method="lm") +
#     #     geom_errorbar(aes(x=mean_fills_per_patient_rx2, ymin=ymin, ymax=ymax), width=.1, alpha=.5) +
#     theme_bw() +
#     ggtitle(paste("Outcome:", title, "\n(no controls)",sep=" ")) +
#     xlab("Average Insurance Copay") + ylab("Coefficient") +
#     theme(text = element_text(size = 40))
#   print(copay_plot)
#   
#   fills_plot = ggplot(data = rx_post_split_int_nc[coef < .5 & coef > -.5 & rx != "ZZ"]) +
#     geom_text(aes(x=total_fills_ins23, y=coef, label=rx), size = 10) +
#     geom_smooth(aes(x=total_fills_ins23, y=coef), se=F, method="lm") +
#     #     geom_errorbar(aes(x=mean_fills_per_patient_rx2, ymin=ymin, ymax=ymax), width=.1, alpha=.5) +
#     theme_bw() +
#     ggtitle(paste("Outcome:", title, "\n(no controls)",sep=" ")) +
#     xlab("Total Insurance Fills") + ylab("Coefficient") +
#     theme(text = element_text(size = 40))
#   print(fills_plot)
#   
#   
#   
#   
#   ####with controls####
#   assign(eval(reg_post_int_c) , lm(get(outcome) ~ 
#                                      post:insurance_type_23 + insurance_type_23 
#                                    + zip_factor + zip_factor:yrmon_number + yrmon_factor 
#                                    + avg_copay_ym_rx_z + avg_age_ym_rx_z
#                                    #                                + total_health_estabs + total_estabs
#                                    + tot_pharms_neg_1
#                                    + mail_order_users_per_tot_pats_neg_1
#                                    + share_medicaid_neg_1
#                                    ,
#                                    #weights =weight,
#                                    data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                                            & is.finite(get(outcome))
#                                                            & opener_zip_dummy ==1 & usable ==1 
#                                                            & zip_ym_ins_23_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_post_int_c_cl) , coeftest(get(reg_post_int_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                     & is.finite(get(outcome))
#                                     & opener_zip_dummy ==1 & usable ==1
#                                     & zip_ym_ins_23_index == 1,]$ZIP)))
#   
#   
#   coefs = names(get(reg_post_int_c_cl)[,1])
#   print(get(reg_post_int_c_cl)[grepl("post",coefs),1])
#   print(nobs(get(reg_post_int_c)))
#   print(coefs[grepl("post",coefs)])
#   
#   rx_list_int_c = coefs[grepl("post",coefs)]
#   coef_list_int_c = get(reg_post_int_c_cl)[grepl("post",coefs),1]
#   se_list_int_c = get(reg_post_int_c_cl)[grepl("post",coefs),2]
#   p_list_int_c = get(reg_post_int_c_cl)[grepl("post",coefs),4]
#   
#   rx_post_split_int_c = data.table(rx = as.character(gsub("post:insurance_type_23","",unlist(rx_list_int_c))),
#                                    coef = unlist(coef_list_int_c),
#                                    se = unlist(se_list_int_c),
#                                    p = unlist(p_list_int_c))
#   
#   ####save the coef for later
#   opening_effects_coef_c = paste(outcome, "opening_effects_coef_c",sep="_")
#   assign(eval(opening_effects_coef_c), data.table(rx = as.character(gsub("post:insurance_type_23","",unlist(rx_list_int_c))),
#                                                   coef = unlist(coef_list_int_c),
#                                                   outcome = outcome))
#   
#   
#   
#   rx_post_split_int_c[, ymin := coef - 1.96*se]
#   rx_post_split_int_c[, ymax := coef + 1.96*se]
#   
#   rx_post_split_int_c[, sig := ifelse(p <= .05, "Significant", "Not Significant")]
#   
#   setkey(rx_post_split_int_c, coef)
#   rx_post_split_int_c[, rank := seq_len(.N)]
#   
#   rx_post_split_int_c[coef >= 0 , min_pos_rank := min(rank)]
#   
#   rx_post_split_int_c[, mean_coef := NULL]
#   rx_post_split_int_c[, mean_coef := mean(coef)]
#   
#   
#   #bar effects
#   c_bar_plot = ggplot(data = rx_post_split_int_c[rx != "ZZ"]) +
#     geom_bar(aes(x=reorder(rx,rank), y=coef), fill="Blue",color="Black", stat="identity", alpha = .4) +
#     geom_errorbar(aes(x=reorder(rx,rank), ymin=ymin, ymax=ymax), width = 0.1) +
#     theme_bw() +
#     theme(legend.position = "none") +
#     ggtitle(paste("Outcome:", title, "\n(with controls)","\nN = ",nobs(get(reg_post_int_c)),sep=" ")) + 
#     xlab("") + ylab("Coefficient") + 
#     theme(text = element_text(size = 50),
#           axis.text.x = element_text(angle = 40,
#                                      vjust = 0.5,
#                                      size = 50),
#           axis.text.y = element_text(angle = 90,
#                                      size = 50)) 
#   print(c_bar_plot)
#   
# }
# dev.off()
# 
# 
# ####post regs ins53 split####
# 
# pdf(file = paste("~/current/pharmacy_deserts/post_ins53_split_coefs_no_weight_",B,".pdf",sep=""), width = 18, height = 10)
# for (outcome in c("log_total_pills_dispensed_ym_ins_53_z","log_total_pats_ym_ins_53_z","log_total_claims_ym_ins_53_z",
#                   "log_total_new_pats_ym_ins_53_z_plus_1")) {
#   #       outcome = "log_total_pills_dispensed_ym_ins_53_z"
#   
#   if (grepl("total_pills_dispensed",outcome) & !(grepl("ring",outcome))) {
#     title = "Pills dispensed"
#   } else if (grepl("total_pats",outcome) & !(grepl("ring",outcome))) {
#     title = "Unique patients"
#   } else if (grepl("total_claims",outcome) & !(grepl("ring",outcome)) & !(grepl("inzip",outcome))){
#     title = "Total claims"
#   } else if (grepl("ring_mean_total_pills_dispensed",outcome)) {
#     title = "Ring Pills dispensed"
#   } else if (grepl("ring_mean_total_pats",outcome)) {
#     title = "Ring Unique patients"
#   } else if (grepl("ring_mean_total_claims",outcome)){
#     title = "Ring Total claims"
#   } else if (grepl("total_money",outcome)){
#     title = "Total money"
#   } else if (grepl("avg_age",outcome)) {
#     title = "Average age"
#   } else if (grepl("avg_copay",outcome)){
#     title = "Average copay"
#   } else if (grepl("total_new_pats",outcome)){
#     title = "New patients"
#   } else if (grepl("mean_mpr",outcome)){
#     title = "Med. Possesion Ratio"
#   } else if (grepl("share_medicaid",outcome)){
#     title = "Medicaid Share of Patients"
#   } else if (grepl("share_claims_inzip",outcome)) {
#     title = "Share of in-zip claims"
#   } else if (grepl("total_claims_inzip",outcome)) {
#     title = "Total in-zip claims"
#   } else if (grepl("total_unique_zips",outcome)) {
#     title = "Unique Pharmacy Zips"
#   } else if (grepl("log_avg_distance",outcome)) {
#     title = "Log Avg. Distance"
#   } else if (grepl("avg_distance",outcome)) {
#     title = "Avg. Distance"
#   }
#   
#   
#   coef_list_int_c = list()
#   se_list_int_c = list()
#   p_list_int_c = list()
#   rx_list_int_c = list()
#   
#   coef_list_int_nc = list()
#   se_list_int_nc = list()
#   p_list_int_nc = list()
#   rx_list_int_nc = list()
#   
#   
#   
#   
#   #INTERACTIONS
#   # run the interaction regressions
#   
#   reg_post_int_nc = paste("reg_post_int_nc_",outcome, sep="")
#   reg_post_int_nc_cl = paste("reg_post_int_nc_cl_",outcome, sep="")
#   reg_post_int_c = paste("reg_post_int_c_",outcome, sep="")
#   reg_post_int_c_cl = paste("reg_post_int_c_cl_",outcome, sep="")
#   
#   
#   ####no controls####
#   assign(eval(reg_post_int_nc) , lm(get(outcome) ~ 
#                                       post:insurance_type_53 + insurance_type_53 
#                                     + zip_factor + zip_factor:yrmon_number + yrmon_factor 
#                                     ,
#                                     #weights =weight,
#                                     data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                                             & is.finite(get(outcome))
#                                                             & opener_zip_dummy ==1 & usable ==1 
#                                                             & zip_ym_ins_53_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_post_int_nc_cl) , coeftest(get(reg_post_int_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                     & is.finite(get(outcome))
#                                     & opener_zip_dummy ==1 & usable ==1
#                                     & zip_ym_ins_53_index == 1,]$ZIP)))
#   
#   coefs = names(get(reg_post_int_nc_cl)[,1])
#   print(get(reg_post_int_nc_cl)[grepl("post",coefs),1])
#   print(nobs(get(reg_post_int_nc)))
#   print(coefs[grepl("post",coefs)])
#   
#   rx_list_int_nc = coefs[grepl("post",coefs)]
#   coef_list_int_nc = get(reg_post_int_nc_cl)[grepl("post",coefs),1]
#   se_list_int_nc = get(reg_post_int_nc_cl)[grepl("post",coefs),2]
#   p_list_int_nc = get(reg_post_int_nc_cl)[grepl("post",coefs),4]
#   
#   rx_post_split_int_nc = data.table(rx = as.character(gsub("post:insurance_type_53","",unlist(rx_list_int_nc))),
#                                     coef = unlist(coef_list_int_nc),
#                                     se = unlist(se_list_int_nc),
#                                     p = unlist(p_list_int_nc))
#   
#   ####save the coef for later
#   opening_effects_coef_nc = paste(outcome, "opening_effects_coef_nc",sep="_")
#   assign(eval(opening_effects_coef_nc), data.table(rx = as.character(gsub("post:insurance_type_53","",unlist(rx_list_int_nc))),
#                                                    coef = unlist(coef_list_int_nc),
#                                                    outcome = outcome))
#   
#   
#   
#   rx_post_split_int_nc[, ymin := coef - 1.96*se]
#   rx_post_split_int_nc[, ymax := coef + 1.96*se]
#   
#   rx_post_split_int_nc[, sig := ifelse(p <= .05, "Significant", "Not Significant")]
#   
#   setkey(rx_post_split_int_nc, coef)
#   rx_post_split_int_nc[, rank := seq_len(.N)]
#   
#   rx_post_split_int_nc[coef >= 0 , min_pos_rank := min(rank)]
#   
#   rx_post_split_int_nc[, mean_coef := NULL]
#   rx_post_split_int_nc[, mean_coef := mean(coef)]
#   
#   
#   #bar effects
#   nc_bar_plot = ggplot(data = rx_post_split_int_nc[rx != "ZZ"]) +
#     geom_bar(aes(x=reorder(rx,rank), y=coef), fill="Blue",color="Black", stat="identity", alpha = .4) +
#     geom_errorbar(aes(x=reorder(rx,rank), ymin=ymin, ymax=ymax), width = 0.1) +
#     theme_bw() +
#     theme(legend.position = "none") +
#     ggtitle(paste("Outcome:", title, "\n(no controls)","\nN = ",nobs(get(reg_post_int_nc)),sep=" ")) + 
#     xlab("") + ylab("Coefficient") + 
#     theme(text = element_text(size = 50),
#           axis.text.x = element_text(angle = 40,
#                                      vjust = 0.5,
#                                      size = 50),
#           axis.text.y = element_text(angle = 90,
#                                      size = 50)) 
#   print(nc_bar_plot)
#   
#   #merge on the insurance type story data
#   setkey(rx_post_split_int_nc, rx)
#   
#   ins53_type_story = as.data.table(read.csv("~/current/pharmacy_deserts/data/ins53_type_story.csv"))
#   setkey(ins53_type_story, insurance_type_53)
#   rx_post_split_int_nc[ins53_type_story, total_fills_ins53 := total_fills_ins53 ]
#   rx_post_split_int_nc[ins53_type_story, avg_copay_ins53 := avg_copay_ins53 ]
#   rx_post_split_int_nc[ins53_type_story, avg_age_ins53 := avg_age_ins53 ]
#   
#   age_plot = ggplot(data = rx_post_split_int_nc[coef < .5 & coef > -.5 & rx != "P+UNK+ZZ"]) +
#     geom_point(aes(x=avg_age_ins53, y=coef, label=rx), size = 4.5) +
#     #     geom_text(aes(x=avg_age_ins53, y=coef, label=rx), size = 10) +
#     geom_smooth(aes(x=avg_age_ins53, y=coef), se=F, method="lm") +
#     #     geom_errorbar(aes(x=mean_fills_per_patient_rx2, ymin=ymin, ymax=ymax), width=.1, alpha=.5) +
#     theme_bw() +
#     ggtitle(paste("Outcome:", title, "\n(no controls)",sep=" ")) +
#     xlab("Average Age of Insurance Patients") + ylab("Coefficient") +
#     theme(text = element_text(size = 40))
#   print(age_plot)
#   
#   copay_plot = ggplot(data = rx_post_split_int_nc[coef < .5 & coef > -.5 & rx != "P+UNK+ZZ"]) +
#     geom_point(aes(x=avg_copay_ins53, y=coef, label=rx), size = 4.5) +
#     geom_smooth(aes(x=avg_copay_ins53, y=coef), se=F, method="lm") +
#     #     geom_errorbar(aes(x=mean_fills_per_patient_rx2, ymin=ymin, ymax=ymax), width=.1, alpha=.5) +
#     theme_bw() +
#     ggtitle(paste("Outcome:", title, "\n(no controls)",sep=" ")) +
#     xlab("Average Insurance Copay") + ylab("Coefficient") +
#     theme(text = element_text(size = 40))
#   print(copay_plot)
#   
#   fills_plot = ggplot(data = rx_post_split_int_nc[coef < .5 & coef > -.5 & rx != "P+UNK+ZZ"]) +
#     geom_point(aes(x=total_fills_ins53, y=coef, label=rx), size = 4.5) +
#     geom_smooth(aes(x=total_fills_ins53, y=coef), se=F, method="lm") +
#     #     geom_errorbar(aes(x=mean_fills_per_patient_rx2, ymin=ymin, ymax=ymax), width=.1, alpha=.5) +
#     theme_bw() +
#     ggtitle(paste("Outcome:", title, "\n(no controls)",sep=" ")) +
#     xlab("Total Insurance Fills") + ylab("Coefficient") +
#     theme(text = element_text(size = 40))
#   print(fills_plot)
#   
#   
#   
#   
#   ####with controls####
#   assign(eval(reg_post_int_c) , lm(get(outcome) ~ 
#                                      post:insurance_type_53 + insurance_type_53 
#                                    + zip_factor + zip_factor:yrmon_number + yrmon_factor 
#                                    + avg_copay_ym_rx_z + avg_age_ym_rx_z
#                                    #                                + total_health_estabs + total_estabs
#                                    + tot_pharms_neg_1
#                                    + mail_order_users_per_tot_pats_neg_1
#                                    + share_medicaid_neg_1
#                                    ,
#                                    #weights =weight,
#                                    data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                                            & is.finite(get(outcome))
#                                                            & opener_zip_dummy ==1 & usable ==1 
#                                                            & zip_ym_ins_53_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_post_int_c_cl) , coeftest(get(reg_post_int_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                     & is.finite(get(outcome))
#                                     & opener_zip_dummy ==1 & usable ==1
#                                     & zip_ym_ins_53_index == 1,]$ZIP)))
#   
#   
#   coefs = names(get(reg_post_int_c_cl)[,1])
#   print(get(reg_post_int_c_cl)[grepl("post",coefs),1])
#   print(nobs(get(reg_post_int_c)))
#   print(coefs[grepl("post",coefs)])
#   
#   rx_list_int_c = coefs[grepl("post",coefs)]
#   coef_list_int_c = get(reg_post_int_c_cl)[grepl("post",coefs),1]
#   se_list_int_c = get(reg_post_int_c_cl)[grepl("post",coefs),2]
#   p_list_int_c = get(reg_post_int_c_cl)[grepl("post",coefs),4]
#   
#   rx_post_split_int_c = data.table(rx = as.character(gsub("post:insurance_type_53","",unlist(rx_list_int_c))),
#                                    coef = unlist(coef_list_int_c),
#                                    se = unlist(se_list_int_c),
#                                    p = unlist(p_list_int_c))
#   
#   ####save the coef for later
#   opening_effects_coef_c = paste(outcome, "opening_effects_coef_c",sep="_")
#   assign(eval(opening_effects_coef_c), data.table(rx = as.character(gsub("post:insurance_type_53","",unlist(rx_list_int_c))),
#                                                   coef = unlist(coef_list_int_c),
#                                                   outcome = outcome))
#   
#   
#   
#   rx_post_split_int_c[, ymin := coef - 1.96*se]
#   rx_post_split_int_c[, ymax := coef + 1.96*se]
#   
#   rx_post_split_int_c[, sig := ifelse(p <= .05, "Significant", "Not Significant")]
#   
#   setkey(rx_post_split_int_c, coef)
#   rx_post_split_int_c[, rank := seq_len(.N)]
#   
#   rx_post_split_int_c[coef >= 0 , min_pos_rank := min(rank)]
#   
#   rx_post_split_int_c[, mean_coef := NULL]
#   rx_post_split_int_c[, mean_coef := mean(coef)]
#   
#   
#   #bar effects
#   c_bar_plot = ggplot(data = rx_post_split_int_c[rx != "ZZ"]) +
#     geom_bar(aes(x=reorder(rx,rank), y=coef), fill="Blue",color="Black", stat="identity", alpha = .4) +
#     geom_errorbar(aes(x=reorder(rx,rank), ymin=ymin, ymax=ymax), width = 0.1) +
#     theme_bw() +
#     theme(legend.position = "none") +
#     ggtitle(paste("Outcome:", title, "\n(with controls)","\nN = ",nobs(get(reg_post_int_c)),sep=" ")) + 
#     xlab("") + ylab("Coefficient") + 
#     theme(text = element_text(size = 50),
#           axis.text.x = element_text(angle = 40,
#                                      vjust = 0.5,
#                                      size = 50),
#           axis.text.y = element_text(angle = 90,
#                                      size = 50)) 
#   print(c_bar_plot)
#   
# }
# dev.off()
# 
# 
# 
# 
# 
# 
# 
# ####months since regs split by mail####
# pill_type_coef_list_c = list()
# pill_type_se_list_c = list()
# pill_type_p_list_c = list()
# pill_type_mail_list_c = list()
# 
# pill_type_coef_list_nc = list()
# pill_type_se_list_nc = list()
# pill_type_p_list_nc = list()
# pill_type_mail_list_nc = list()
# pdf(file = paste("~/current/pharmacy_deserts/post_mail_split_no_weights_",B,".pdf",sep=""), width = 18, height = 10)
# for (MAIL in c(0,1)) {
#   #   MAIL = 0
#   for (outcome in c("log_total_pills_dispensed_ym_mail_z","log_total_pats_ym_mail_z","log_total_claims_ym_mail_z",
#                     "log_avg_copay_ym_mail_z_plus_1", "log_total_new_pats_ym_mail_z_plus_1")) {
#     
#     #         outcome = "log_total_pills_dispensed_ym_mail_z"
#     reg_ms_nc = paste("reg_ms_nc_",outcome, sep="")
#     reg_ms_nc_cl = paste("reg_ms_nc_cl_",outcome, sep="")
#     reg_ms_c = paste("reg_ms_c_",outcome, sep="")
#     reg_ms_c_cl = paste("reg_ms_c_cl_",outcome, sep="")
#     
#     reg_post_nc = paste("reg_post_nc_",outcome, sep="")
#     reg_post_nc_cl = paste("reg_post_nc_cl_",outcome, sep="")
#     reg_post_c = paste("reg_post_c_",outcome, sep="")
#     reg_post_c_cl = paste("reg_post_c_cl_",outcome, sep="")
#     
#     if (grepl("total_pills_dispensed",outcome) & !(grepl("ring",outcome))) {
#       title = "Pills dispensed"
#     } else if (grepl("total_pats",outcome) & !(grepl("ring",outcome))) {
#       title = "Unique patients"
#     } else if (grepl("total_claims",outcome) & !(grepl("ring",outcome)) & !(grepl("inzip",outcome))){
#       title = "Total claims"
#     } else if (grepl("ring_mean_total_pills_dispensed",outcome)) {
#       title = "Ring Pills dispensed"
#     } else if (grepl("ring_mean_total_pats",outcome)) {
#       title = "Ring Unique patients"
#     } else if (grepl("ring_mean_total_claims",outcome)){
#       title = "Ring Total claims"
#     } else if (grepl("total_money",outcome)){
#       title = "Total money"
#     } else if (grepl("avg_age",outcome)) {
#       title = "Average age"
#     } else if (grepl("avg_copay",outcome)){
#       title = "Average copay"
#     } else if (grepl("total_new_pats",outcome)){
#       title = "New patients"
#     } else if (grepl("mean_mpr",outcome)){
#       title = "Med. Possesion Ratio"
#     } else if (grepl("share_medicaid",outcome)){
#       title = "Medicaid Share of Patients"
#     } else if (grepl("share_claims_inzip",outcome)) {
#       title = "Share of in-zip claims"
#     } else if (grepl("total_claims_inzip",outcome)) {
#       title = "Total in-zip claims"
#     } else if (grepl("total_unique_zips",outcome)) {
#       title = "Unique Pharmacy Zips"
#     } else if (grepl("log_avg_distance",outcome)) {
#       title = "Log Avg. Distance"
#     } else if (grepl("avg_distance",outcome)) {
#       title = "Avg. Distance"
#     }
#     
#     
#     
#     ####no controls####
#     assign(eval(reg_post_nc) , lm(get(outcome) ~ 
#                                     post
#                                   + zip_factor + zip_factor:yrmon_number + yrmon_factor 
#                                   ,
#                                   #weights =weight,
#                                   data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                                           & opener_zip_dummy ==1 & usable ==1 
#                                                           & zip_ym_mail_index == 1
#                                                           & mail_order_dummy == MAIL,]))
#     
#     #cluster standard errors at the zip code level
#     assign(eval(reg_post_nc_cl) , coeftest(get(reg_post_nc), vcov=function(x) 
#       cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                       & opener_zip_dummy ==1 & usable ==1 
#                                       & zip_ym_mail_index == 1
#                                       & mail_order_dummy == MAIL,]$ZIP)))
#     
#     print(get(reg_post_nc_cl)[1:2,])
#     print(nobs(get(reg_post_nc)))
#     
#     pill_type_mail_list_nc[[paste("mail:",eval(MAIL),sep="")]] = MAIL
#     pill_type_coef_list_nc[[paste(eval(reg_post_nc_cl),eval(MAIL),"_coef",sep="")]] = get(reg_post_nc_cl)[2,1]
#     pill_type_se_list_nc[[paste(eval(reg_post_nc_cl),eval(MAIL),"_se",sep="")]] = get(reg_post_nc_cl)[2,2]
#     pill_type_p_list_nc[[paste(eval(reg_post_nc_cl),eval(MAIL),"_p",sep="")]] = get(reg_post_nc_cl)[2,4]
#     #   
#     
#     ####with with controls####
#     assign(eval(reg_post_c) , lm(get(outcome) ~ 
#                                    post
#                                  + zip_factor + zip_factor:yrmon_number + yrmon_factor  
#                                  + avg_copay_ym_rx_z + avg_age_ym_rx_z
#                                  #                                + total_health_estabs + total_estabs
#                                  + tot_pharms_neg_1
#                                  + mail_order_users_per_tot_pats_neg_1
#                                  ,
#                                  #weights =weight,
#                                  data = zip_data_stacked[ms <= 6 & ms >= -6 
#                                                          & opener_zip_dummy ==1 & usable ==1 
#                                                          & zip_ym_mail_index == 1
#                                                          & mail_order_dummy == MAIL,]))
#     
#     #cluster standard errors at the zip code level
#     assign(eval(reg_post_c_cl) , coeftest(get(reg_post_c), vcov=function(x) 
#       cluster.vcov(x,zip_data_stacked[ms <= 6 & ms >= -6 
#                                       & opener_zip_dummy ==1 & usable ==1 
#                                       & zip_ym_mail_index == 1
#                                       & mail_order_dummy == MAIL,]$ZIP)))
#     
#     print(get(reg_post_c_cl)[1:2,])
#     print(nobs(get(reg_post_c)))
#     
#     pill_type_mail_list_c[[paste("mail:",eval(MAIL),sep="")]] = MAIL
#     pill_type_coef_list_c[[paste(eval(reg_post_c_cl),eval(MAIL),"_coef",sep="")]] = get(reg_post_c_cl)[2,1]
#     pill_type_se_list_c[[paste(eval(reg_post_c_cl),eval(MAIL),"_se",sep="")]] = get(reg_post_c_cl)[2,2]
#     pill_type_p_list_c[[paste(eval(reg_post_c_cl),eval(MAIL),"_p",sep="")]] = get(reg_post_c_cl)[2,4]
#     
#     
#     
#     
#     
#     ####plot the post coefs split by mailtype####
#     mail_post_split_c = data.table(mail = unlist(pill_type_mail_list_c),
#                                    coef = unlist(pill_type_coef_list_c),
#                                    se = unlist(pill_type_se_list_c),
#                                    p = unlist(pill_type_p_list_c))
#     
#     mail_post_split_c[, ymin := coef - 1.96*se]
#     mail_post_split_c[, ymax := coef + 1.96*se]
#     
#     mail_post_split_c[, sig := ifelse(p <= .05, 1, 0)]
#     
#     setkey(mail_post_split_c, coef)
#     mail_post_split_c[, rank := seq_len(.N)]
#     
#     plot = ggplot(data = mail_post_split_c[!is.na(p)]) +
#       #   geom_point(aes(x=rank, y=coef, color=as.factor(sig))) +
#       geom_text(aes(x=rank, y=coef, label=mail, color=as.factor(sig)), angle = 90) + 
#       geom_line(aes(x=rank, y=ymin), size = 0.5, linetype="dashed",alpha=.5) +
#       geom_line(aes(x=rank, y=ymax), size = 0.5, linetype="dashed",alpha=.5)
#     print(plot)
#     
#     #############MONTHS SINCE#######
#     
#     ####no controls####
#     assign(eval(reg_ms_nc) , lm(get(outcome) ~ 
#                                   ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0 
#                                 + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6 
#                                 + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                 ,
#                                 #weights =weight,
#                                 data = zip_data_stacked[ ms <= B & ms >= -B 
#                                                          & opener_zip_dummy ==1 
#                                                          & zip_ym_mail_index == 1 
#                                                          & usable ==1
#                                                          & mail_order_dummy == MAIL,]))
#     ##NOTE: zip_ms_index is created AFTER the stack so it pulls out zips with multiple openings in the same month
#     ##but zip_ym_index was created before the stack so it only removes obs with other ins types or rxclasses
#     
#     #cluster standard errors at the zip code level
#     assign(eval(reg_ms_nc_cl) , coeftest(get(reg_ms_nc), vcov=function(x) 
#       cluster.vcov(x,zip_data_stacked[ms <= B & ms >= -B 
#                                       & opener_zip_dummy ==1 
#                                       & zip_ym_mail_index == 1 
#                                       & usable ==1
#                                       & mail_order_dummy == MAIL,]$ZIP)))
#     
#     print(get(reg_ms_nc_cl)[1:(2*B+1),])
#     print(nobs(get(reg_ms_nc)))
#     
#     coef_vector = c(get(reg_ms_nc_cl)[2:B,1], 0, get(reg_ms_nc_cl)[(B+1):(B*2+1),1])
#     se_vector = c(get(reg_ms_nc_cl)[2:B,2], NA, get(reg_ms_nc_cl)[(B+1):(B*2+1),2])
#     
#     ymin = coef_vector - 1.96 * se_vector
#     ymax = coef_vector + 1.96 * se_vector
#     
#     
#     ####plot###
#     plot_nc = ggplot(data = NULL) +
#       geom_point(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B))),size = 8) +
#       geom_line(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B)))) +
#       geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#       geom_errorbar(aes(ymin = ymin, ymax = ymax, x=c(seq(-B,-2),-1,seq(0,B))),width = .1) +
#       theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#       scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title," \n(no controls)","\nMail Order =",MAIL,sep="")) 
#     
#     #                 
#     print(plot_nc)
#     rm(ymin, ymax, coef_vector, se_vector)
#     
#     
#     ####with controls####
#     assign(eval(reg_ms_c) , lm(get(outcome) ~ 
#                                  ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0 
#                                + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6 
#                                + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                + avg_copay_ym_z + avg_age_ym_z
#                                #                              + total_health_estabs + total_estabs
#                                + tot_pharms_neg_1
#                                + mail_order_users_per_tot_pats_neg_1
#                                + share_medicaid_neg_1
#                                ,
#                                #weights =weight,
#                                data = zip_data_stacked[ ms <= B & ms >= -B 
#                                                         & opener_zip_dummy ==1 
#                                                         & usable ==1 
#                                                         & zip_ym_mail_index == 1
#                                                         & mail_order_dummy == MAIL,]))
#     
#     #cluster standard errors at the zip code level
#     assign(eval(reg_ms_c_cl) , coeftest(get(reg_ms_c), vcov=function(x) 
#       cluster.vcov(x,zip_data_stacked[ms <= B & ms >= -B 
#                                       & opener_zip_dummy ==1 
#                                       & usable ==1 
#                                       & zip_ym_mail_index == 1
#                                       & mail_order_dummy == MAIL,]$ZIP)))
#     
#     print(get(reg_ms_c_cl)[1:(B*2+1),])
#     print(nobs(get(reg_ms_c)))
#     
#     coef_vector = c(get(reg_ms_c_cl)[2:B,1], 0, get(reg_ms_c_cl)[(B+1):(B*2+1),1])
#     se_vector = c(get(reg_ms_c_cl)[2:B,2], NA, get(reg_ms_c_cl)[(B+1):(B*2+1),2])
#     
#     ymin = coef_vector - 1.96 * se_vector
#     ymax = coef_vector + 1.96 * se_vector
#     
#     
#     ####plot###
#     plot_c = ggplot(data = NULL) +
#       geom_point(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B))),size = 8) +
#       geom_line(aes(y=coef_vector,x=c(seq(-B,-2),-1,seq(0,B)))) +
#       geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#       geom_errorbar(aes(ymin = ymin, ymax = ymax, x=c(seq(-B,-2),-1,seq(0,B))),width = .1) +
#       theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#       scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title,"\n(with controls)","\nMail Order =",MAIL,sep=""))
#     print(plot_c)
#     rm(ymin, ymax, coef_vector, se_vector)
#     
#   }
#   
#   
# }
# 
# dev.off()
# 
# 
# 
# ####months since regressions split by number of existing pharmacies####
# pdf(file = paste("~/current/pharmacy_deserts/event_study_no_weights_desert_split_",B,".pdf",sep=""), width = 18, height = 10)
# for (outcome in c("log_total_pills_dispensed_ym_z","log_total_pats_ym_z","log_total_claims_ym_z",
#                   "log_avg_age_ym_z","log_avg_copay_ym_z", "log_total_new_pats_ym_z_plus_1")) {
#   
#   #   outcome = "log_total_pills_dispensed_ym_z"
#   
#   # outcome = "mean_mpr_ym_z"
#   reg_ms_nc = paste("reg_ms_nc_",outcome, sep="")
#   reg_ms_nc_cl = paste("reg_ms_nc_cl_",outcome, sep="")
#   reg_ms_c = paste("reg_ms_c_",outcome, sep="")
#   reg_ms_c_cl = paste("reg_ms_c_cl_",outcome, sep="")
#   
#   
#   if (grepl("total_pills_dispensed",outcome) & !(grepl("ring",outcome))) {
#     title = "Pills dispensed"
#   } else if (grepl("total_pats",outcome) & !(grepl("ring",outcome))) {
#     title = "Unique patients"
#   } else if (grepl("total_claims",outcome) & !(grepl("ring",outcome)) & !(grepl("inzip",outcome))){
#     title = "Total claims"
#   } else if (grepl("ring_mean_total_pills_dispensed",outcome)) {
#     title = "Ring Pills dispensed"
#   } else if (grepl("ring_mean_total_pats",outcome)) {
#     title = "Ring Unique patients"
#   } else if (grepl("ring_mean_total_claims",outcome)){
#     title = "Ring Total claims"
#   } else if (grepl("total_money",outcome)){
#     title = "Total money"
#   } else if (grepl("avg_age",outcome)) {
#     title = "Average age"
#   } else if (grepl("avg_copay",outcome)){
#     title = "Average copay"
#   } else if (grepl("total_new_pats",outcome)){
#     title = "New patients"
#   } else if (grepl("mean_mpr",outcome)){
#     title = "Med. Possesion Ratio"
#   } else if (grepl("share_medicaid",outcome)){
#     title = "Medicaid Share of Patients"
#   } else if (grepl("share_claims_inzip",outcome)) {
#     title = "Share of in-zip claims"
#   } else if (grepl("total_claims_inzip",outcome)) {
#     title = "Total in-zip claims"
#   } else if (grepl("total_unique_zips",outcome)) {
#     title = "Unique Pharmacy Zips"
#   }
#   
#   if (grepl("z_1", outcome)) {
#     title = paste(title,"\n(1-5 mi ring)",sep="")
#   } else if (grepl("z_6", outcome)) {
#     title = paste(title,"\n(6-10 mi ring)",sep="")
#   } else if (grepl("z_11", outcome)) {
#     title = paste(title,"\n(11-15 mi ring)",sep="")
#   } else if (grepl("z_16", outcome)) {
#     title = paste(title,"\n(16-20 mi ring)",sep="")
#   } else if (grepl("z_21", outcome)) {
#     title = paste(title,"\n(21-25 mi ring)",sep="")
#   } else if (grepl("z_26", outcome)) {
#     title = paste(title,"\n(26-30 mi ring)",sep="")
#   }
#   
#   ####histogram of raw outcome####
#   if (outcome %in% c("mean_mpr_ym_z","share_medicaid_ym_z","share_claims_inzip_ym_z")) {
#     hist = ggplot(data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]) +
#       geom_histogram(aes(x=get(outcome))) +
#       theme_bw() +  xlab("") +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title,"",sep="")) 
#     print(hist)
#   } else {
#     hist = ggplot(data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]) +
#       geom_histogram(aes(x=exp(get(outcome)))) +
#       theme_bw() +  xlab("") +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title,"",sep="")) 
#     print(hist)
#   }
#   
#   #   loghist = ggplot(data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]) +
#   #     geom_histogram(aes(x=log(get(outcome)))) +
#   #     theme_bw() +  xlab("") +
#   #     theme(text = element_text(size=55, family = "serif")) +
#   #     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#   #     ggtitle(paste(title,"\n(log)",sep="")) 
#   #   print(loghist)
#   
#   ####no controls####
#   assign(eval(reg_ms_nc) , lm(get(outcome) ~ 
#                                 #                                 ms_neg9 + ms_neg8 + ms_neg7 +
#                                 (ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0 
#                                  + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6):as.factor(tot_pharms_neg_1)
#                               + as.factor(tot_pharms_neg_1)
#                               #                               + ms_7 + ms_8 + ms_9
#                               + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                               ,
#                               #weights =weight,
#                               data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]))
#   ##NOTE: zip_ms_index is created AFTER the stack so it pulls out zips with multiple openings in the same month
#   ##but zip_ym_index was created before the stack so it only removes obs with other ins types or rxclasses
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_nc_cl) , coeftest(get(reg_ms_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]$ZIP)))
#   
#   print(get(reg_ms_nc_cl))
#   print(nobs(get(reg_ms_nc)))
#   
#   for (X in seq(0,13)) {
#     #     X = 1
#     
#     coef_vector = paste("coef_vector_",X,sep="")
#     string = paste("as.factor\\(tot_pharms_neg_1\\)",X,"end",sep="")
#     
#     coefs = paste(names(get(reg_ms_nc_cl)[,1]),"end",sep="")
#     coefs_limited = coefs[grep(toString(eval(string)),coefs)]
#     
#     coef_list = as.list(get(reg_ms_nc_cl)[,1])
#     se_list = as.list(get(reg_ms_nc_cl)[,2])
#     coef_data = data.table(name = names(coef_list),
#                            coef = unlist(coef_list),
#                            se = unlist(se_list))
#     coef_data[, name := paste(name,"end",sep="")]
#     coef_data = coef_data[name %in% coefs_limited]
#     coef_data[, ymin := coef - 1.96*se]
#     coef_data[, ymax := coef + 1.96*se]
#     coef_data = coef_data[grepl("ms.*?:", name)] #limit to only the months since dummies
#     
#     
#     
#     add_neg1 = data.table(name= "ms_neg1",coef=0,se=NA, ymin= NA, ymax=NA)
#     coef_data = rbindlist(list(coef_data, add_neg1))
#     coef_data[, ms := unlist(strsplit(grep("ms_(\\.*)", name, perl=T,value=T),":"))[seq(1,25,2)]]
#     coef_data[, ms := gsub("ms_","",ms)]
#     coef_data[, ms := gsub("neg","-",ms)]
#     coef_data[, ms := as.numeric(ms)]
#     coef_data[, num_pharms := X]
#     
#     assign(eval(coef_vector), coef_data)
#     
#     
#     
#   }
#   
#   coef_data = rbindlist(list(coef_vector_0,coef_vector_1,coef_vector_2,coef_vector_3,coef_vector_4,coef_vector_5,coef_vector_6,
#                              coef_vector_7,coef_vector_8,coef_vector_9,coef_vector_10,coef_vector_11,coef_vector_12,coef_vector_13))
#   
#   for (X in seq(0,13)) { 
#     ####plot###
#     plot_nc = ggplot(data = coef_data[num_pharms <= X,]) +
#       geom_point(aes(x=ms, y=coef,color=as.factor(num_pharms)),size = 8) +
#       geom_line(aes(x=ms, y=coef,color=as.factor(num_pharms))) +
#       geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#       geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#       theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#       #     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title," \n(no controls)",sep="")) 
#     
#     #                 
#     print(plot_nc)
#   }
#   gc()
#   
#   ####with controls####
#   assign(eval(reg_ms_c) , lm(get(outcome) ~ 
#                                #                                ms_neg9 + ms_neg8 + ms_neg7 +
#                                (ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0 
#                                 + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6):as.factor(tot_pharms_neg_1)
#                              + as.factor(tot_pharms_neg_1)
#                              #                              + ms_7 + ms_8 + ms_9
#                              + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                              + avg_copay_ym_z + avg_age_ym_z
#                              #                              + total_health_estabs + total_estabs
#                              
#                              + mail_order_users_per_tot_pats_neg_1
#                              + share_medicaid_neg_1
#                              ,
#                              #weights =weight,
#                              data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_c_cl) , coeftest(get(reg_ms_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= B & ms >= -B &  opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]$ZIP)))
#   
#   for (X in seq(0,13)) {
#     #     X = 1
#     
#     coef_vector = paste("coef_vector_",X,sep="")
#     string = paste("as.factor\\(tot_pharms_neg_1\\)",X,"end",sep="")
#     
#     coefs = paste(names(get(reg_ms_c_cl)[,1]),"end",sep="")
#     coefs_limited = coefs[grep(toString(eval(string)),coefs)]
#     
#     coef_list = as.list(get(reg_ms_c_cl)[,1])
#     se_list = as.list(get(reg_ms_c_cl)[,2])
#     coef_data = data.table(name = names(coef_list),
#                            coef = unlist(coef_list),
#                            se = unlist(se_list))
#     coef_data[, name := paste(name,"end",sep="")]
#     coef_data = coef_data[name %in% coefs_limited]
#     coef_data[, ymin := coef - 1.96*se]
#     coef_data[, ymax := coef + 1.96*se]
#     coef_data = coef_data[grepl("ms.*?:", name)] #limit to only the months since dummies
#     
#     
#     add_neg1 = data.table(name= "ms_neg1",coef=0,se=NA, ymin= NA, ymax=NA)
#     coef_data = rbindlist(list(coef_data, add_neg1))
#     coef_data[, ms := unlist(strsplit(grep("ms_(\\.*)", name, perl=T,value=T),":"))[seq(1,25,2)]]
#     coef_data[, ms := gsub("ms_","",ms)]
#     coef_data[, ms := gsub("neg","-",ms)]
#     coef_data[, ms := as.numeric(ms)]
#     coef_data[, num_pharms := X]
#     
#     assign(eval(coef_vector), coef_data)
#     
#     
#     
#   }
#   
#   coef_data = rbindlist(list(coef_vector_0,coef_vector_1,coef_vector_2,coef_vector_3,coef_vector_4,coef_vector_5,coef_vector_6,
#                              coef_vector_7,coef_vector_8,coef_vector_9,coef_vector_10,coef_vector_11,coef_vector_12,coef_vector_13))
#   
#   for (X in seq(0,13)) { 
#     ####plot###
#     plot_c = ggplot(data = coef_data[num_pharms <= X,]) +
#       geom_point(aes(x=ms, y=coef,color=as.factor(num_pharms)),size = 8) +
#       geom_line(aes(x=ms, y=coef,color=as.factor(num_pharms))) +
#       geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#       geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#       theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#       #     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title," \n(with controls)",sep="")) 
#     
#     #                 
#     print(plot_c)
#   }
#   
#   
# }
# 
# #need a histogram of the number of zip codes with existing pharmacies
# zip_data_stacked[, zip_index := seq_len(.N), by = c("ZIP")]
# ggplot(data = zip_data_stacked[zip_index == 1 & non_opener==0, ]) +
#   geom_histogram(aes(x=as.numeric(tot_pharms_neg_1))) +
#   theme_bw() + ylab("Number of zip codes") + xlab("Number of pharmacies month prior to opening") +
#   #     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#   theme(text = element_text(size=55, family = "serif")) +
#   theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#   ggtitle("Zip Code Distribution") 
# 
# 
# dev.off()
# 
# 
# ####post regressions split by number of existing pharmacies####
# pdf(file = paste("~/current/pharmacy_deserts/post_no_weights_desert_split_",B,".pdf",sep=""), width = 18, height = 10)
# for (outcome in c("log_total_pills_dispensed_ym_z","log_total_pats_ym_z","log_total_claims_ym_z",
#                   "log_avg_age_ym_z","log_avg_copay_ym_z", "log_total_new_pats_ym_z_plus_1","share_medicaid_ym_z")) {
#   
#   #     outcome = "log_total_pills_dispensed_ym_z"
#   
#   # outcome = "mean_mpr_ym_z"
#   reg_post_nc = paste("reg_post_nc_",outcome, sep="")
#   reg_post_nc_cl = paste("reg_post_nc_cl_",outcome, sep="")
#   reg_post_c = paste("reg_post_c_",outcome, sep="")
#   reg_post_c_cl = paste("reg_post_c_cl_",outcome, sep="")
#   
#   
#   if (grepl("total_pills_dispensed",outcome) & !(grepl("ring",outcome))) {
#     title = "Pills dispensed"
#   } else if (grepl("total_pats",outcome) & !(grepl("ring",outcome))) {
#     title = "Unique patients"
#   } else if (grepl("total_claims",outcome) & !(grepl("ring",outcome)) & !(grepl("inzip",outcome))){
#     title = "Total claims"
#   } else if (grepl("ring_mean_total_pills_dispensed",outcome)) {
#     title = "Ring Pills dispensed"
#   } else if (grepl("ring_mean_total_pats",outcome)) {
#     title = "Ring Unique patients"
#   } else if (grepl("ring_mean_total_claims",outcome)){
#     title = "Ring Total claims"
#   } else if (grepl("total_money",outcome)){
#     title = "Total money"
#   } else if (grepl("avg_age",outcome)) {
#     title = "Average age"
#   } else if (grepl("avg_copay",outcome)){
#     title = "Average copay"
#   } else if (grepl("total_new_pats",outcome)){
#     title = "New patients"
#   } else if (grepl("mean_mpr",outcome)){
#     title = "Med. Possesion Ratio"
#   } else if (grepl("share_medicaid",outcome)){
#     title = "Medicaid Share of Patients"
#   } else if (grepl("share_claims_inzip",outcome)) {
#     title = "Share of in-zip claims"
#   } else if (grepl("total_claims_inzip",outcome)) {
#     title = "Total in-zip claims"
#   } else if (grepl("total_unique_zips",outcome)) {
#     title = "Unique Pharmacy Zips"
#   }
#   
#   
#   
#   ####no controls####
#   assign(eval(reg_post_nc) , lm(get(outcome) ~ 
#                                   post:as.factor(tot_pharms_neg_1)
#                                 + as.factor(tot_pharms_neg_1)
#                                 + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                 ,
#                                 #weights =weight,
#                                 data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]))
#   ##NOTE: zip_ms_index is created AFTER the stack so it pulls out zips with multiple openings in the same month
#   ##but zip_ym_index was created before the stack so it only removes obs with other ins types or rxclasses
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_post_nc_cl) , coeftest(get(reg_post_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]$ZIP)))
#   
#   print(get(reg_post_nc_cl))
#   print(nobs(get(reg_post_nc)))
#   
#   coefs = paste(names(get(reg_post_nc_cl)[,1]),sep="")
#   coefs_limited = coefs[grep("post",coefs)]
#   
#   coef_list = as.list(get(reg_post_nc_cl)[,1])
#   se_list = as.list(get(reg_post_nc_cl)[,2])
#   coef_data = data.table(name = names(coef_list),
#                          coef = unlist(coef_list),
#                          se = unlist(se_list))
#   coef_data = coef_data[name %in% coefs_limited]
#   coef_data[, ymin := coef - 1.96*se]
#   coef_data[, ymax := coef + 1.96*se]
#   
#   
#   coef_data[, num_pharms := gsub("post:as.factor\\(tot_pharms_neg_1\\)","",name)]
#   
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(x=as.numeric(num_pharms), y=coef),size = 8) +
#     geom_line(aes(x=as.numeric(num_pharms), y=coef)) +
#     geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=as.numeric(num_pharms)),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Number of Existing Pharmacies") +
#     #     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   
#   
#   
#   
#   ####with controls####
#   assign(eval(reg_post_c) , lm(get(outcome) ~ 
#                                  post:as.factor(tot_pharms_neg_1)
#                                + as.factor(tot_pharms_neg_1)
#                                + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                + avg_copay_ym_z + avg_age_ym_z
#                                #                              + total_health_estabs + total_estabs
#                                
#                                + mail_order_users_per_tot_pats_neg_1
#                                + share_medicaid_neg_1
#                                ,
#                                #weights =weight,
#                                data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_post_c_cl) , coeftest(get(reg_post_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= B & ms >= -B &  opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]$ZIP)))
#   
#   coefs = paste(names(get(reg_post_c_cl)[,1]),sep="")
#   coefs_limited = coefs[grep("post",coefs)]
#   
#   coef_list = as.list(get(reg_post_c_cl)[,1])
#   se_list = as.list(get(reg_post_c_cl)[,2])
#   coef_data = data.table(name = names(coef_list),
#                          coef = unlist(coef_list),
#                          se = unlist(se_list))
#   coef_data = coef_data[name %in% coefs_limited]
#   coef_data[, ymin := coef - 1.96*se]
#   coef_data[, ymax := coef + 1.96*se]
#   
#   
#   coef_data[, num_pharms := gsub("post:as.factor\\(tot_pharms_neg_1\\)","",name)]
#   
#   plot_c = ggplot(data = coef_data) +
#     geom_point(aes(x=as.numeric(num_pharms), y=coef),size = 8) +
#     geom_line(aes(x=as.numeric(num_pharms), y=coef)) +
#     geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=as.numeric(num_pharms)),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Number of Existing Pharmacies") +
#     #     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(with controls)",sep="")) 
#   
#   #                 
#   print(plot_c)
#   
# }
# 
# 
# 
# dev.off()
# 
# 
# ####months since regressions split by pharmtype####
# pdf(file = paste("~/current/pharmacy_deserts/event_study_no_weights_pharmtype_split_",B,".pdf",sep=""), width = 18, height = 10)
# for (outcome in c("log_total_pills_dispensed_ym_z","log_total_pats_ym_z","log_total_claims_ym_z",
#                   "log_avg_age_ym_z","log_avg_copay_ym_z", "log_total_new_pats_ym_z_plus_1","share_medicaid_ym_z")) {
#   
#   #     outcome = "log_total_pills_dispensed_ym_pharmtype_z"
#   
#   # outcome = "mean_mpr_ym_z"
#   reg_ms_nc = paste("reg_ms_nc_",outcome, sep="")
#   reg_ms_nc_cl = paste("reg_ms_nc_cl_",outcome, sep="")
#   reg_ms_c = paste("reg_ms_c_",outcome, sep="")
#   reg_ms_c_cl = paste("reg_ms_c_cl_",outcome, sep="")
#   
#   
#   if (grepl("total_pills_dispensed",outcome) & !(grepl("ring",outcome))) {
#     title = "Pills dispensed"
#   } else if (grepl("total_pats",outcome) & !(grepl("ring",outcome))) {
#     title = "Unique patients"
#   } else if (grepl("total_claims",outcome) & !(grepl("ring",outcome)) & !(grepl("inzip",outcome))){
#     title = "Total claims"
#   } else if (grepl("ring_mean_total_pills_dispensed",outcome)) {
#     title = "Ring Pills dispensed"
#   } else if (grepl("ring_mean_total_pats",outcome)) {
#     title = "Ring Unique patients"
#   } else if (grepl("ring_mean_total_claims",outcome)){
#     title = "Ring Total claims"
#   } else if (grepl("total_money",outcome)){
#     title = "Total money"
#   } else if (grepl("avg_age",outcome)) {
#     title = "Average age"
#   } else if (grepl("avg_copay",outcome)){
#     title = "Average copay"
#   } else if (grepl("total_new_pats",outcome)){
#     title = "New patients"
#   } else if (grepl("mean_mpr",outcome)){
#     title = "Med. Possesion Ratio"
#   } else if (grepl("share_medicaid",outcome)){
#     title = "Medicaid Share of Patients"
#   } else if (grepl("share_claims_inzip",outcome)) {
#     title = "Share of in-zip claims"
#   } else if (grepl("total_claims_inzip",outcome)) {
#     title = "Total in-zip claims"
#   } else if (grepl("total_unique_zips",outcome)) {
#     title = "Unique Pharmacy Zips"
#   }
#   
#   
#   
#   
#   
#   ####no controls####
#   assign(eval(reg_ms_nc) , lm(get(outcome) ~ 
#                                 (ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0 
#                                  + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6):as.factor(opening_entity_broad)
#                               + as.factor(opening_entity_broad)
#                               + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                               ,
#                               #weights =weight,
#                               data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]))
#   ##NOTE: zip_ms_index is created AFTER the stack so it pulls out zips with multiple openings in the same month
#   ##but zip_ym_index was created before the stack so it only removes obs with other ins types or rxclasses
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_nc_cl) , coeftest(get(reg_ms_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]$ZIP)))
#   
#   print(get(reg_ms_nc_cl))
#   print(nobs(get(reg_ms_nc)))
#   
#   for (X in sort(unique(zip_data_stacked$opening_entity_broad))) {
#     #         X = "safeway"
#     
#     coef_vector = paste("coef_vector_",X,sep="")
#     string = paste("as.factor\\(opening_entity_broad\\)",X,"end",sep="")
#     
#     coefs = paste(names(get(reg_ms_nc_cl)[,1]),"end",sep="")
#     coefs_limited = coefs[grep(toString(eval(string)),coefs)]
#     
#     coef_list = as.list(get(reg_ms_nc_cl)[,1])
#     se_list = as.list(get(reg_ms_nc_cl)[,2])
#     coef_data = data.table(name = names(coef_list),
#                            coef = unlist(coef_list),
#                            se = unlist(se_list))
#     coef_data[, name := paste(name,"end",sep="")]
#     coef_data = coef_data[name %in% coefs_limited]
#     coef_data[, ymin := coef - 1.96*se]
#     coef_data[, ymax := coef + 1.96*se]
#     
#     
#     
#     add_neg1 = data.table(name= "ms_neg1",coef=0,se=NA, ymin= NA, ymax=NA)
#     coef_data = rbindlist(list(coef_data, add_neg1))
#     coef_data[, ms := unlist(strsplit(grep("ms_(\\.*)", name, perl=T,value=T),":"))[seq(1,25,2)]]
#     coef_data[, ms := gsub("ms_","",ms)]
#     coef_data[, ms := gsub("neg","-",ms)]
#     coef_data[, ms := as.numeric(ms)]
#     coef_data[, entity_broad := X]
#     
#     ####plot###
#     plot_nc = ggplot(data = coef_data) +
#       geom_point(aes(x=ms, y=coef,color=as.factor(entity_broad)),size = 8) +
#       geom_line(aes(x=ms, y=coef,color=as.factor(entity_broad))) +
#       geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#       geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#       theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#       #     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title," \n(no controls)",sep="")) 
#     
#     #                 
#     print(plot_nc)
#     
#     assign(eval(coef_vector), coef_data)
#     
#     
#     
#   }
#   
#   coef_data = rbindlist(list(coef_vector_bimart,#coef_vector_cvs,
#                              coef_vector_fred_meyer,
#                              coef_vector_other,#coef_vector_safeway,
#                              coef_vector_walgreens,
#                              coef_vector_target,coef_vector_walmart))
#   
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(x=ms, y=coef,color=as.factor(entity_broad)),size = 8) +
#     geom_line(aes(x=ms, y=coef,color=as.factor(entity_broad))) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#     #     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(x=ms, y=coef,color=as.factor(entity_broad)),size = 8) +
#     geom_line(aes(x=ms, y=coef,color=as.factor(entity_broad))) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#     #     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)",sep="")) +
#     facet_grid(entity_broad~.)
#   
#   #                 
#   print(plot_nc)
#   
#   
#   ####with controls####
#   assign(eval(reg_ms_c) , lm(get(outcome) ~ 
#                                #                                ms_neg9 + ms_neg8 + ms_neg7 +
#                                (ms_neg6 + ms_neg5 + ms_neg4 + ms_neg3 + ms_neg2 + ms_0 
#                                 + ms_1 + ms_2 + ms_3 + ms_4 + ms_5 + ms_6):as.factor(opening_entity_broad)
#                              + as.factor(opening_entity_broad)
#                              #                              + ms_7 + ms_8 + ms_9
#                              + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                              + avg_copay_ym_z + avg_age_ym_z
#                              #                              + total_health_estabs + total_estabs
#                              
#                              + mail_order_users_per_tot_pats_neg_1
#                              + tot_pharms_neg_1
#                              + share_medicaid_neg_1
#                              ,
#                              #weights =weight,
#                              data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_c_cl) , coeftest(get(reg_ms_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= B & ms >= -B &  opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]$ZIP)))
#   
#   for (X in sort(unique(zip_data_stacked$opening_entity_broad))) {
#     #     X = 1
#     
#     coef_vector = paste("coef_vector_",X,sep="")
#     string = paste("as.factor\\(opening_entity_broad\\)",X,"end",sep="")
#     
#     coefs = paste(names(get(reg_ms_c_cl)[,1]),"end",sep="")
#     coefs_limited = coefs[grep(toString(eval(string)),coefs)]
#     
#     coef_list = as.list(get(reg_ms_c_cl)[,1])
#     se_list = as.list(get(reg_ms_c_cl)[,2])
#     coef_data = data.table(name = names(coef_list),
#                            coef = unlist(coef_list),
#                            se = unlist(se_list))
#     coef_data[, name := paste(name,"end",sep="")]
#     coef_data = coef_data[name %in% coefs_limited]
#     coef_data[, ymin := coef - 1.96*se]
#     coef_data[, ymax := coef + 1.96*se]
#     
#     
#     
#     add_neg1 = data.table(name= "ms_neg1",coef=0,se=NA, ymin= NA, ymax=NA)
#     coef_data = rbindlist(list(coef_data, add_neg1))
#     coef_data[, ms := unlist(strsplit(grep("ms_(\\.*)", name, perl=T,value=T),":"))[seq(1,25,2)]]
#     coef_data[, ms := gsub("ms_","",ms)]
#     coef_data[, ms := gsub("neg","-",ms)]
#     coef_data[, ms := as.numeric(ms)]
#     coef_data[, entity_broad := X]
#     
#     ####plot###
#     plot_c = ggplot(data = coef_data) +
#       geom_point(aes(x=ms, y=coef,color=as.factor(entity_broad)),size = 8) +
#       geom_line(aes(x=ms, y=coef,color=as.factor(entity_broad))) +
#       geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#       geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#       theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#       #     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title," \n(with controls)",sep=""))  
#     
#     #                 
#     print(plot_c)
#     
#     assign(eval(coef_vector), coef_data)
#     
#     
#     
#   }
#   
#   coef_data = rbindlist(list(coef_vector_bimart,#coef_vector_cvs,
#                              coef_vector_fred_meyer,
#                              coef_vector_other,#coef_vector_safeway,
#                              coef_vector_walgreens,
#                              coef_vector_target,coef_vector_walmart))
#   
#   ####plot###
#   plot_c = ggplot(data = coef_data) +
#     geom_point(aes(x=ms, y=coef,color=as.factor(entity_broad)),size = 8) +
#     geom_line(aes(x=ms, y=coef,color=as.factor(entity_broad))) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#     #     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(with controls)",sep=""))  +
#     facet_grid(entity_broad~.)
#   
#   #                 
#   print(plot_c)
#   
#   ####plot###
#   plot_c = ggplot(data = coef_data) +
#     geom_point(aes(x=ms, y=coef,color=as.factor(entity_broad)),size = 8) +
#     geom_line(aes(x=ms, y=coef,color=as.factor(entity_broad))) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#     #     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(with controls)",sep=""))  
#   
#   #                 
#   print(plot_c)
#   
#   
# }
# 
# dev.off()
# 
# ####post regressions split by pharmtype####
# pdf(file = paste("~/current/pharmacy_deserts/post_no_weights_pharmtype_split_",B,".pdf",sep=""), width = 18, height = 10)
# for (outcome in c("log_total_pills_dispensed_ym_z","log_total_pats_ym_z","log_total_claims_ym_z",
#                   "log_avg_age_ym_z","log_avg_copay_ym_z", "log_total_new_pats_ym_z_plus_1","share_medicaid_ym_z")) {
#   
#   #     outcome = "log_total_pills_dispensed_ym_z"
#   
#   # outcome = "mean_mpr_ym_z"
#   reg_post_nc = paste("reg_post_nc_",outcome, sep="")
#   reg_post_nc_cl = paste("reg_post_nc_cl_",outcome, sep="")
#   reg_post_c = paste("reg_post_c_",outcome, sep="")
#   reg_post_c_cl = paste("reg_post_c_cl_",outcome, sep="")
#   
#   
#   if (grepl("total_pills_dispensed",outcome) & !(grepl("ring",outcome))) {
#     title = "Pills dispensed"
#   } else if (grepl("total_pats",outcome) & !(grepl("ring",outcome))) {
#     title = "Unique patients"
#   } else if (grepl("total_claims",outcome) & !(grepl("ring",outcome)) & !(grepl("inzip",outcome))){
#     title = "Total claims"
#   } else if (grepl("ring_mean_total_pills_dispensed",outcome)) {
#     title = "Ring Pills dispensed"
#   } else if (grepl("ring_mean_total_pats",outcome)) {
#     title = "Ring Unique patients"
#   } else if (grepl("ring_mean_total_claims",outcome)){
#     title = "Ring Total claims"
#   } else if (grepl("total_money",outcome)){
#     title = "Total money"
#   } else if (grepl("avg_age",outcome)) {
#     title = "Average age"
#   } else if (grepl("avg_copay",outcome)){
#     title = "Average copay"
#   } else if (grepl("total_new_pats",outcome)){
#     title = "New patients"
#   } else if (grepl("mean_mpr",outcome)){
#     title = "Med. Possesion Ratio"
#   } else if (grepl("share_medicaid",outcome)){
#     title = "Medicaid Share of Patients"
#   } else if (grepl("share_claims_inzip",outcome)) {
#     title = "Share of in-zip claims"
#   } else if (grepl("total_claims_inzip",outcome)) {
#     title = "Total in-zip claims"
#   } else if (grepl("total_unique_zips",outcome)) {
#     title = "Unique Pharmacy Zips"
#   }
#   
#   
#   
#   ####no controls####
#   assign(eval(reg_post_nc) , lm(get(outcome) ~ 
#                                   post:as.factor(opening_entity_broad)
#                                 + as.factor(opening_entity_broad)
#                                 + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                 ,
#                                 #weights =weight,
#                                 data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]))
#   ##NOTE: zip_ms_index is created AFTER the stack so it pulls out zips with multiple openings in the same month
#   ##but zip_ym_index was created before the stack so it only removes obs with other ins types or rxclasses
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_post_nc_cl) , coeftest(get(reg_post_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]$ZIP)))
#   
#   print(get(reg_post_nc_cl))
#   print(nobs(get(reg_post_nc)))
#   
#   coefs = paste(names(get(reg_post_nc_cl)[,1]),sep="")
#   coefs_limited = coefs[grep("post",coefs)]
#   
#   coef_list = as.list(get(reg_post_nc_cl)[,1])
#   se_list = as.list(get(reg_post_nc_cl)[,2])
#   coef_data = data.table(name = names(coef_list),
#                          coef = unlist(coef_list),
#                          se = unlist(se_list))
#   coef_data = coef_data[name %in% coefs_limited]
#   coef_data[, ymin := coef - 1.96*se]
#   coef_data[, ymax := coef + 1.96*se]
#   
#   
#   coef_data[, opening_entity_broad := gsub("post:as.factor\\(opening_entity_broad\\)","",name)]
#   
#   plot_nc = ggplot(data = coef_data) +
#     geom_bar(aes(x=as.factor(opening_entity_broad), y=coef, fill = opening_entity_broad),size = 8, stat="identity", position ="dodge") +
#     #     geom_line(aes(x=as.factor(opening_entity_broad), y=coef)) +
#     geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=as.factor(opening_entity_broad)),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Number of Existing Pharmacies") +
#     #     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(legend.position = "none") + 
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60, angle = 45, vjust=1)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   
#   
#   
#   
#   ####with controls####
#   assign(eval(reg_post_c) , lm(get(outcome) ~ 
#                                  post:as.factor(opening_entity_broad)
#                                + as.factor(opening_entity_broad)
#                                + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                + avg_copay_ym_z + avg_age_ym_z
#                                #                              + total_health_estabs + total_estabs
#                                
#                                + mail_order_users_per_tot_pats_neg_1
#                                + tot_pharms_neg_1
#                                + share_medicaid_neg_1
#                                ,
#                                #weights =weight,
#                                data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_post_c_cl) , coeftest(get(reg_post_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= B & ms >= -B &  opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]$ZIP)))
#   
#   coefs = paste(names(get(reg_post_c_cl)[,1]),sep="")
#   coefs_limited = coefs[grep("post",coefs)]
#   
#   coef_list = as.list(get(reg_post_c_cl)[,1])
#   se_list = as.list(get(reg_post_c_cl)[,2])
#   coef_data = data.table(name = names(coef_list),
#                          coef = unlist(coef_list),
#                          se = unlist(se_list))
#   coef_data = coef_data[name %in% coefs_limited]
#   coef_data[, ymin := coef - 1.96*se]
#   coef_data[, ymax := coef + 1.96*se]
#   
#   
#   coef_data[, opening_entity_broad := gsub("post:as.factor\\(opening_entity_broad\\)","",name)]
#   
#   plot_c = ggplot(data = coef_data) +
#     geom_bar(aes(x=as.factor(opening_entity_broad), y=coef, fill = opening_entity_broad),size = 8, stat="identity", position ="dodge") +
#     #     geom_line(aes(x=as.factor(opening_entity_broad), y=coef)) +
#     geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=as.factor(opening_entity_broad)),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Number of Existing Pharmacies") +
#     #     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(legend.position = "none") + 
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60, angle = 45, vjust=1)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(with controls)",sep="")) 
#   
#   #                 
#   print(plot_c)
#   
# }
# 
# 
# 
# dev.off()
# 
# 
# 
# 
# ####post regressions interacted with share medicaid####
# pdf(file = paste("~/current/pharmacy_deserts/post_no_weights_wealth_split_",B,".pdf",sep=""), width = 18, height = 10)
# for (outcome in c("log_total_pills_dispensed_ym_z","log_total_pats_ym_z","log_total_claims_ym_z",
#                   "log_avg_age_ym_z","log_avg_copay_ym_z", "log_total_new_pats_ym_z_plus_1")) {
#   
#   #     outcome = "log_total_pills_dispensed_ym_z"
#   
#   # outcome = "mean_mpr_ym_z"
#   reg_post_nc = paste("reg_post_nc_",outcome, sep="")
#   reg_post_nc_cl = paste("reg_post_nc_cl_",outcome, sep="")
#   reg_post_c = paste("reg_post_c_",outcome, sep="")
#   reg_post_c_cl = paste("reg_post_c_cl_",outcome, sep="")
#   
#   
#   if (grepl("total_pills_dispensed",outcome) & !(grepl("ring",outcome))) {
#     title = "Pills dispensed"
#   } else if (grepl("total_pats",outcome) & !(grepl("ring",outcome))) {
#     title = "Unique patients"
#   } else if (grepl("total_claims",outcome) & !(grepl("ring",outcome)) & !(grepl("inzip",outcome))){
#     title = "Total claims"
#   } else if (grepl("ring_mean_total_pills_dispensed",outcome)) {
#     title = "Ring Pills dispensed"
#   } else if (grepl("ring_mean_total_pats",outcome)) {
#     title = "Ring Unique patients"
#   } else if (grepl("ring_mean_total_claims",outcome)){
#     title = "Ring Total claims"
#   } else if (grepl("total_money",outcome)){
#     title = "Total money"
#   } else if (grepl("avg_age",outcome)) {
#     title = "Average age"
#   } else if (grepl("avg_copay",outcome)){
#     title = "Average copay"
#   } else if (grepl("total_new_pats",outcome)){
#     title = "New patients"
#   } else if (grepl("mean_mpr",outcome)){
#     title = "Med. Possesion Ratio"
#   } else if (grepl("share_medicaid",outcome)){
#     title = "Medicaid Share of Patients"
#   } else if (grepl("share_claims_inzip",outcome)) {
#     title = "Share of in-zip claims"
#   } else if (grepl("total_claims_inzip",outcome)) {
#     title = "Total in-zip claims"
#   } else if (grepl("total_unique_zips",outcome)) {
#     title = "Unique Pharmacy Zips"
#   }
#   
#   
#   
#   ####no controls####
#   assign(eval(reg_post_nc) , lm(get(outcome) ~ 
#                                   post:as.factor(share_medicaid_neg_1)
#                                 + as.factor(share_medicaid_neg_1)
#                                 + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                 ,
#                                 #weights =weight,
#                                 data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]))
#   ##NOTE: zip_ms_index is created AFTER the stack so it pulls out zips with multiple openings in the same month
#   ##but zip_ym_index was created before the stack so it only removes obs with other ins types or rxclasses
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_post_nc_cl) , coeftest(get(reg_post_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]$ZIP)))
#   
#   print(get(reg_post_nc_cl))
#   print(nobs(get(reg_post_nc)))
#   
#   coefs = paste(names(get(reg_post_nc_cl)[,1]),sep="")
#   coefs_limited = coefs[grep("post",coefs)]
#   
#   coef_list = as.list(get(reg_post_nc_cl)[,1])
#   se_list = as.list(get(reg_post_nc_cl)[,2])
#   coef_data = data.table(name = names(coef_list),
#                          coef = unlist(coef_list),
#                          se = unlist(se_list))
#   coef_data = coef_data[name %in% coefs_limited]
#   coef_data[, ymin := coef - 1.96*se]
#   coef_data[, ymax := coef + 1.96*se]
#   
#   
#   coef_data[, share_medicaid_neg_1 := gsub("post:as.factor\\(share_medicaid_neg_1\\)","",name)]
#   
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(x=as.numeric(share_medicaid_neg_1), y=coef),size = 8) +
#     #     geom_line(aes(x=as.numeric(share_medicaid_neg_1), y=coef)) +
#     geom_hline(yintercept=0) +
#     #     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=as.numeric(share_medicaid_neg_1)),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Share of Patients on Medicaid \nmonth prior to opening") +
#     #     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   
#   
#   
#   
#   ####with controls####
#   assign(eval(reg_post_c) , lm(get(outcome) ~ 
#                                  post:as.factor(share_medicaid_neg_1)
#                                + as.factor(share_medicaid_neg_1)
#                                + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                                + avg_copay_ym_z + avg_age_ym_z
#                                #                              + total_health_estabs + total_estabs
#                                
#                                + mail_order_users_per_tot_pats_neg_1
#                                + tot_pharms_neg_1
#                                #                                + share_medicaid_neg_1
#                                ,
#                                #weights =weight,
#                                data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_post_c_cl) , coeftest(get(reg_post_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= B & ms >= -B &  opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]$ZIP)))
#   
#   coefs = paste(names(get(reg_post_c_cl)[,1]),sep="")
#   coefs_limited = coefs[grep("post",coefs)]
#   
#   coef_list = as.list(get(reg_post_c_cl)[,1])
#   se_list = as.list(get(reg_post_c_cl)[,2])
#   coef_data = data.table(name = names(coef_list),
#                          coef = unlist(coef_list),
#                          se = unlist(se_list))
#   coef_data = coef_data[name %in% coefs_limited]
#   coef_data[, ymin := coef - 1.96*se]
#   coef_data[, ymax := coef + 1.96*se]
#   
#   
#   coef_data[, share_medicaid_neg_1 := gsub("post:as.factor\\(share_medicaid_neg_1\\)","",name)]
#   
#   plot_c = ggplot(data = coef_data) +
#     geom_point(aes(x=as.numeric(share_medicaid_neg_1), y=coef),size = 8) +
#     #     geom_line(aes(x=as.numeric(share_medicaid_neg_1), y=coef)) +
#     geom_hline(yintercept=0) +
#     #     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=as.numeric(share_medicaid_neg_1)),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Share of Patients on Medicaid \nmonth prior to opening") +
#     #     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(with controls)",sep="")) 
#   
#   #                 
#   print(plot_c)
#   
# }
# 
# 
# 
# dev.off()
# 
# 
# ####zip code specific effects####
# pdf(file="~/current/pharmacy_deserts/prediction_figures_no_weights.pdf", width = 18, height = 10)
# for (outcome in c("log_total_pills_dispensed_ym_z","log_total_pats_ym_z","log_total_claims_ym_z",
#                   "log_total_new_pats_ym_z_plus_1"
# )) {
#   #     Z = 97006
#   #       outcome = "log_total_pills_dispensed_ym_z"
#   reg_ms_nc = paste("reg_ms_nc_",outcome, sep="")
#   reg_ms_nc_cl = paste("reg_ms_nc_cl_",outcome, sep="")
#   reg_ms_c = paste("reg_ms_c_",outcome, sep="")
#   reg_ms_c_cl = paste("reg_ms_c_cl_",outcome, sep="")
#   
#   
#   
#   if (grepl("total_pills_dispensed",outcome) & !(grepl("ring",outcome))) {
#     title = "Pills dispensed"
#   } else if (grepl("total_pats",outcome) & !(grepl("ring",outcome))) {
#     title = "Unique patients"
#   } else if (grepl("total_claims",outcome) & !(grepl("ring",outcome)) & !(grepl("inzip",outcome))){
#     title = "Total claims"
#   } else if (grepl("ring_mean_total_pills_dispensed",outcome)) {
#     title = "Ring Pills dispensed"
#   } else if (grepl("ring_mean_total_pats",outcome)) {
#     title = "Ring Unique patients"
#   } else if (grepl("ring_mean_total_claims",outcome)){
#     title = "Ring Total claims"
#   } else if (grepl("total_money",outcome)){
#     title = "Total money"
#   } else if (grepl("avg_age",outcome)) {
#     title = "Average age"
#   } else if (grepl("avg_copay",outcome)){
#     title = "Average copay"
#   } else if (grepl("total_new_pats",outcome)){
#     title = "New patients"
#   } else if (grepl("mean_mpr",outcome)){
#     title = "Med. Possesion Ratio"
#   } else if (grepl("share_medicaid",outcome)){
#     title = "Medicaid Share of Patients"
#   } else if (grepl("share_claims_inzip",outcome)) {
#     title = "Share of in-zip claims"
#   } else if (grepl("total_claims_inzip",outcome)) {
#     title = "Total in-zip claims"
#   } else if (grepl("total_unique_zips",outcome)) {
#     title = "Unique Pharmacy Zips"
#   } else if (grepl("log_avg_distance",outcome)) {
#     title = "Log Avg. Distance"
#   } else if (grepl("avg_distance",outcome)) {
#     title = "Avg. Distance"
#   }
#   
#   
#   print(outcome)
#   
#   
#   ####no controls####
#   assign(eval(reg_ms_nc) , lm(get(outcome) ~ 
#                                 post:zip_factor
#                               + zip_factor:yrmon_number + yrmon_factor + zip_factor
#                               ,
#                               #weights =weight,
#                               data = zip_data_stacked[ ms <= B & ms >= -B 
#                                                        & opener_zip_dummy ==1 
#                                                        & zip_ym_index == 1 
#                                                        & usable ==1,]))
#   ##NOTE: zip_ms_index is created AFTER the stack so it pulls out zips with multiple openings in the same month
#   ##but zip_ym_index was created before the stack so it only removes obs with other ins types or rxclasses
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_nc_cl) , coeftest(get(reg_ms_nc), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= B & ms >= -B 
#                                     & opener_zip_dummy ==1 
#                                     & zip_ym_index == 1 & usable ==1,]$ZIP)))
#   
#   print(get(reg_ms_nc_cl)[1:(2*B+1),])
#   print(nobs(get(reg_ms_nc)))
#   
#   #store the post coef and the zip
#   coefs = paste(names(get(reg_ms_nc_cl)[,1]),sep="")
#   coefs_limited = coefs[grep("post",coefs)]
#   
#   coef_list = as.list(get(reg_ms_nc_cl)[,1])
#   se_list = as.list(get(reg_ms_nc_cl)[,2])
#   coef_data = data.table(name = names(coef_list),
#                          coef = unlist(coef_list),
#                          se = unlist(se_list))
#   coef_data = coef_data[name %in% coefs_limited]
#   coef_data[, ymin := coef - 1.96*se]
#   coef_data[, ymax := coef + 1.96*se]
#   
#   coef_data[, zip_code := gsub("post:zip_factor","",name)]
#   
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(x=as.factor(zip_code), y=coef),size = 8) +
#     #     geom_line(aes(x=as.numeric(share_medicaid_neg_1), y=coef)) +
#     geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=as.factor(zip_code)),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Zip Code") +
#     #     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x=element_blank()) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)",sep="")) 
#   print(plot_nc)
#   
#   write.csv(coef_data, file = paste("~/current/pharmacy_deserts/",eval(outcome),"_prediction_set_nc.csv", sep=""))
#   
#   
#   ####with controls####
#   assign(eval(reg_ms_c) , lm(get(outcome) ~ 
#                                post:zip_factor
#                              + zip_factor + zip_factor:yrmon_number + yrmon_factor
#                              + avg_copay_ym_z + avg_age_ym_z
#                              #                              + total_health_estabs + total_estabs
#                              + tot_pharms_neg_1
#                              + mail_order_users_per_tot_pats_neg_1
#                              + share_medicaid_neg_1
#                              ,
#                              #weights =weight,
#                              data = zip_data_stacked[ ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(reg_ms_c_cl) , coeftest(get(reg_ms_c), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[ms <= B & ms >= -B & opener_zip_dummy ==1 & usable ==1 & zip_ym_index == 1,]$ZIP)))
#   
#   print(get(reg_ms_c_cl)[1:(B*2+1),])
#   print(nobs(get(reg_ms_c)))
#   
#   #store the post coef and the zip
#   coefs = paste(names(get(reg_ms_c_cl)[,1]),sep="")
#   coefs_limited = coefs[grep("post",coefs)]
#   
#   coef_list = as.list(get(reg_ms_c_cl)[,1])
#   se_list = as.list(get(reg_ms_c_cl)[,2])
#   coef_data = data.table(name = names(coef_list),
#                          coef = unlist(coef_list),
#                          se = unlist(se_list))
#   coef_data = coef_data[name %in% coefs_limited]
#   coef_data[, ymin := coef - 1.96*se]
#   coef_data[, ymax := coef + 1.96*se]
#   
#   coef_data[, zip_code := gsub("post:zip_factor","",name)]
#   
#   plot_c = ggplot(data = coef_data) +
#     geom_point(aes(x=as.factor(zip_code), y=coef),size = 8) +
#     #     geom_line(aes(x=as.numeric(share_medicaid_neg_1), y=coef)) +
#     geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=as.factor(zip_code)),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Zip Code") +
#     #     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x=element_blank()) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(with controls)",sep="")) 
#   print(plot_c)
#   
#   write.csv(coef_data, file = paste("~/current/pharmacy_deserts/",eval(outcome),"_prediction_set_c.csv", sep=""))
#   
#   
#   
#   
# }
# 
# dev.off()
# 
# 
# 
# 
# 
# ####walgreens-express scripts####
# #for the one year ahead counterfactual
# zip_data_stacked[, ms_wal_neg9_plus1 := ifelse(ms_wal == -9+12, 1, 0)]
# zip_data_stacked[, ms_wal_neg8_plus1 := ifelse(ms_wal == -8+12, 1, 0)]
# zip_data_stacked[, ms_wal_neg7_plus1 := ifelse(ms_wal == -7+12, 1, 0)]
# zip_data_stacked[, ms_wal_neg6_plus1 := ifelse(ms_wal == -6+12, 1, 0)]
# zip_data_stacked[, ms_wal_neg5_plus1 := ifelse(ms_wal == -5+12, 1, 0)]
# zip_data_stacked[, ms_wal_neg4_plus1 := ifelse(ms_wal == -4+12, 1, 0)]
# zip_data_stacked[, ms_wal_neg3_plus1 := ifelse(ms_wal == -3+12, 1, 0)]
# zip_data_stacked[, ms_wal_neg2_plus1 := ifelse(ms_wal == -2+12, 1, 0)]
# zip_data_stacked[, ms_wal_neg1_plus1 := ifelse(ms_wal == -1+12, 1, 0)]
# zip_data_stacked[, ms_wal_0_plus1 := ifelse(ms_wal == 0+12, 1, 0)]
# zip_data_stacked[, ms_wal_1_plus1 := ifelse(ms_wal == 1+12, 1, 0)]
# zip_data_stacked[, ms_wal_2_plus1 := ifelse(ms_wal == 2+12, 1, 0)]
# zip_data_stacked[, ms_wal_3_plus1 := ifelse(ms_wal == 3+12, 1, 0)]
# zip_data_stacked[, ms_wal_4_plus1 := ifelse(ms_wal == 4+12, 1, 0)]
# zip_data_stacked[, ms_wal_5_plus1 := ifelse(ms_wal == 5+12, 1, 0)]
# zip_data_stacked[, ms_wal_6_plus1 := ifelse(ms_wal == 6+12, 1, 0)]
# zip_data_stacked[, ms_wal_7_plus1 := ifelse(ms_wal == 7+12, 1, 0)]
# zip_data_stacked[, ms_wal_8_plus1 := ifelse(ms_wal == 8+12, 1, 0)]
# zip_data_stacked[, ms_wal_9_plus1 := ifelse(ms_wal == 9+12, 1, 0)]
# 
# zip_data_stacked[, post_wal_plus1 := ifelse(ms_wal >= 0+12 , 1, 0)]
# 
# #are any walgreens the only pharmacy in their zip code?
# sort(unique(zip_data_stacked[tot_pharms_ym_z == 1 & walgreens_zip_person==1]$ZIP)) #97027
# zip_data_stacked[, desert := ifelse(ZIP == 97027, 1, 0)]
# 
# #month of year control
# zip_data_stacked[, month_of_year := month(pick_up_yrmon)]
# 
# #zip_data_stacked should have the ms_wal variable and the walgreens_person variable
# #limit to private insurance
# #treatment: living in a zip code where walgreens is
# #control: living in a non walgreens zip code
# #months since figures
# pdf(file="~/current/pharmacy_deserts/walgreens_no_weights.pdf", width = 18, height = 10)
# for (outcome in c("log_total_pills_dispensed_ym_ins_23_z","log_total_pats_ym_ins_23_z","log_total_claims_ym_ins_23_z",
#                   "log_total_new_pats_ym_ins_23_z_plus_1","log_total_money_ym_ins_23_z_plus_1", "log_pills_per_person_ym_ins_23_z",
#                   "mail_share_ym_ins_23_z")) {
#   # outcome = "log_total_new_pats_ym_ins_23_z_plus_1"
#   wal_reg = paste("wal_reg_",outcome, sep="")
#   wal_reg_treated = paste("wal_reg_treated_",outcome, sep="")
#   
#   if (grepl("total_pills_dispensed",outcome) & !(grepl("ring",outcome))) {
#     title = "Pills dispensed"
#   } else if (grepl("total_pats",outcome) & !(grepl("ring",outcome))) {
#     title = "Unique patients"
#   } else if (grepl("pills_per_person",outcome)) {
#     title = "Pills per Patient"
#   } else if (grepl("mail_share",outcome)) {
#     title = "Mail Share of Claims"
#   } else if (grepl("total_claims",outcome) & !(grepl("ring",outcome)) & !(grepl("inzip",outcome))){
#     title = "Total claims"
#   } else if (grepl("ring_mean_total_pills_dispensed",outcome)) {
#     title = "Ring Pills dispensed"
#   } else if (grepl("ring_mean_total_pats",outcome)) {
#     title = "Ring Unique patients"
#   } else if (grepl("ring_mean_total_claims",outcome)){
#     title = "Ring Total claims"
#   } else if (grepl("total_money",outcome)){
#     title = "Total money"
#   } else if (grepl("avg_age",outcome)) {
#     title = "Average age"
#   } else if (grepl("avg_copay",outcome)){
#     title = "Average copay"
#   } else if (grepl("total_new_pats",outcome)){
#     title = "New patients"
#   } else if (grepl("mean_mpr",outcome)){
#     title = "Med. Possesion Ratio"
#   } else if (grepl("share_medicaid",outcome)){
#     title = "Medicaid Share of Patients"
#   } else if (grepl("share_claims_inzip",outcome)) {
#     title = "Share of in-zip claims"
#   } else if (grepl("total_claims_inzip",outcome)) {
#     title = "Total in-zip claims"
#   } else if (grepl("total_unique_zips",outcome)) {
#     title = "Unique Pharmacy Zips"
#   } else if (grepl("log_avg_distance",outcome)) {
#     title = "Log Avg. Distance"
#   } else if (grepl("avg_distance",outcome)) {
#     title = "Avg. Distance"
#   }
#   
#   
#   
#   ###treated zips only
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  #ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#               ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#             + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6 #+ ms_wal_7 + ms_wal_8 + ms_wal_9
#             + zip_factor #+ yrmon_factor 
#             + zip_factor:yrmon_number
#             ,
#             data = zip_data_stacked[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                                     & walgreens_zip_person == 1
#                                     & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                                     ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                                     & walgreens_zip_person == 1
#                                     & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                                     ]$ZIP)))
#   get(wal_reg_treated)
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms_",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*_","",names) ]
#   coef_data[, ms := gsub("neg","-",ms) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   coef_data[, ms := as.numeric(ms)]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\nPharmacy Ben. Only",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   
#   ####all zips####
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  #ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#               walgreens_zip_person:(ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#                                     + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6) #+ ms_wal_7 + ms_wal_8 + ms_wal_9
#             + zip_factor #+ yrmon_factor 
#             + zip_factor:yrmon_number
#             ,
#             data = zip_data_stacked[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                                     #                                   & walgreens_zip_person == 1
#                                     & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                                     #                                     & ((pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011") 
#                                     #                                        |  (pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012") )
#                                     #                                     & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                                     ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                                     #                                   & walgreens_zip_person == 1
#                                     #                                     & ((pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011") 
#                                     #                                        |  (pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012") )
#                                     & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                                     #                                     & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                                     ]$ZIP)))
#   get(wal_reg_treated)
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms_",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*_","",names) ]
#   coef_data[, ms := gsub("neg","-",ms) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   coef_data[, ms := as.numeric(ms)]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\nPharmacy Ben. Only\nAll Zip Codes",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   
#   ###treated zips only - all data window
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  ms_wal_neg12 + ms_wal_neg11 + ms_wal_neg10 + ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#               ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#             + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6 + ms_wal_7 + ms_wal_8 + ms_wal_9
#             + ms_wal_10 + ms_wal_11 + ms_wal_12 + ms_wal_13 + ms_wal_14 + ms_wal_15 + ms_wal_16 + ms_wal_17 + ms_wal_18 + ms_wal_19
#             + ms_wal_20 + ms_wal_21 + ms_wal_22 + ms_wal_23
#             #           + as.factor(month_of_year)
#             + zip_factor #+ yrmon_factor 
#             + zip_factor:yrmon_number
#             
#             ,
#             data = zip_data_stacked[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                                     & walgreens_zip_person == 1
#                                     #                                   & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                                     ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                                     & walgreens_zip_person == 1
#                                     #                                   & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                                     ]$ZIP)))
#   get(wal_reg_treated)
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms_",names(get(wal_reg_treated)[,1]))]
#   # names_vector = names(get(wal_reg_treated)[,1])[grepl("yrmon_factor",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*_","",names) ]
#   coef_data[, ms := as.numeric(gsub("neg","-",ms)) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   # coef_data[, ms := as.yearmon(gsub("yrmon_factor","",names)) ]
#   # coef_data[, ms := round(as.numeric(ms - as.yearmon("Jan 2012"))*12) ]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_vline(xintercept=9,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     #   scale_x_continuous(breaks = seq(-B-3,B+3,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\nPharmacy Ben. Only",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   
#   ###all zips big window
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  walgreens_zip_person:(ms_wal_neg12 + ms_wal_neg11 + ms_wal_neg10 + ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#                                                     ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#                                                   + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6 + ms_wal_7 + ms_wal_8 + ms_wal_9
#                                                   + ms_wal_10 + ms_wal_11 + ms_wal_12 + ms_wal_13 + ms_wal_14 + ms_wal_15 + ms_wal_16 + ms_wal_17 + ms_wal_18 + ms_wal_19
#                                                   + ms_wal_20 + ms_wal_21 + ms_wal_22 + ms_wal_23)
#             + zip_factor #+ walgreens_zip_person:yrmon_factor 
#             + zip_factor:yrmon_number
#             #           + as.factor(month_of_year)
#             ,
#             data = zip_data_stacked[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                                     #                                   & walgreens_zip_person == 1
#                                     #                                   & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                                     ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                                     #                                   & walgreens_zip_person == 1
#                                     #                                   & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                                     ]$ZIP)))
#   get(wal_reg_treated)
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms_",names(get(wal_reg_treated)[,1]))]
#   # names_vector = names(get(wal_reg_treated)[,1])[grepl("yrmon_factor",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*_","",names) ]
#   coef_data[, ms := as.numeric(gsub("neg","-",ms)) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   # coef_data[, ms := as.yearmon(gsub(".*yrmon_factor","",names)) ]
#   # coef_data[, ms := round(as.numeric(ms - as.yearmon("Jan 2012"))*12) ]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_vline(xintercept=9,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     #   scale_x_continuous(breaks = seq(-B-3,B+3,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\nPharmacy Ben. Only\nAll Zip Codes",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   
#   ###all zips big window - include month of year FE####
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  walgreens_zip_person:(ms_wal_neg12 + ms_wal_neg11 + ms_wal_neg10 + ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#                                                     ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#                                                   + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6 + ms_wal_7 + ms_wal_8 + ms_wal_9
#                                                   + ms_wal_10 + ms_wal_11 + ms_wal_12 + ms_wal_13 + ms_wal_14 + ms_wal_15 + ms_wal_16 + ms_wal_17 + ms_wal_18 + ms_wal_19
#                                                   + ms_wal_20 + ms_wal_21 + ms_wal_22 + ms_wal_23)
#             + zip_factor #+ walgreens_zip_person:yrmon_factor 
#             + zip_factor:yrmon_number
#             + as.factor(month_of_year)
#             ,
#             data = zip_data_stacked[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                                     #                                   & walgreens_zip_person == 1
#                                     #                                   & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                                     ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                                     #                                   & walgreens_zip_person == 1
#                                     #                                   & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                                     ]$ZIP)))
#   get(wal_reg_treated)
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms_",names(get(wal_reg_treated)[,1]))]
#   # names_vector = names(get(wal_reg_treated)[,1])[grepl("yrmon_factor",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*_","",names) ]
#   coef_data[, ms := as.numeric(gsub("neg","-",ms)) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   # coef_data[, ms := as.yearmon(gsub(".*yrmon_factor","",names)) ]
#   # coef_data[, ms := round(as.numeric(ms - as.yearmon("Jan 2012"))*12) ]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_vline(xintercept=9,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     #   scale_x_continuous(breaks = seq(-B-3,B+3,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\nPharmacy Ben. Only\nAll Zip Codes",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   ####as a counterfactual, look at one year ahead
#   
#   
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  #ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#               ms_wal_neg6_plus1 + ms_wal_neg5_plus1 + ms_wal_neg4_plus1 + ms_wal_neg3_plus1 + ms_wal_neg2_plus1 
#             + ms_wal_0_plus1 + ms_wal_1_plus1 + ms_wal_2_plus1 + ms_wal_3_plus1 + ms_wal_4_plus1 + ms_wal_5_plus1 + ms_wal_6_plus1 #+ ms_wal_7 + ms_wal_8 + ms_wal_9
#             + zip_factor #+ yrmon_factor 
#             + zip_factor:yrmon_number
#             ,
#             data = zip_data_stacked[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                                     & walgreens_zip_person == 1
#                                     & pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012"
#                                     #                                     & ((pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011") 
#                                     #                                        |  (pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012") )
#                                     #                                     & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                                     ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                                     & walgreens_zip_person == 1
#                                     #                                     & ((pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011") 
#                                     #                                        |  (pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012") )
#                                     & pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012"
#                                     #                                     & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                                     ]$ZIP)))
#   get(wal_reg_treated)
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   # coef_data[, ms := gsub(".*factor","",names) ]
#   coef_data[, ms := gsub("_plus1","",names) ]
#   coef_data[, ms := gsub("neg","-",ms) ]
#   coef_data[, ms := as.numeric(gsub("ms_wal_","",ms)) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   # coef_data[, ms := round((as.yearmon(ms) - as.yearmon("Jan 2013"))*12)]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") +
#     geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\n(+1 Year)\nPharmacy Ben. Only",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   
#   
#   ####another counterfactual: public insurance
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  #ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#               ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#             + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6 #+ ms_wal_7 + ms_wal_8 + ms_wal_9
#             + zip_factor #+ yrmon_factor 
#             + zip_factor:yrmon_number
#             ,
#             data = zip_data_stacked[zip_ym_ins_23_index == 1 & (insurance_type_23 %in% c("MLI","MD","MRB","MDF"))
#                                     & walgreens_zip_person == 1
#                                     & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                                     #                                     & ((pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011") 
#                                     #                                        |  (pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012") )
#                                     #                                     & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                                     ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[zip_ym_ins_23_index == 1 & (insurance_type_23 %in% c("MLI","MD","MRB","MDF"))
#                                     & walgreens_zip_person == 1
#                                     #                                     & ((pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011") 
#                                     #                                        |  (pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012") )
#                                     & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                                     #                                     & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                                     ]$ZIP)))
#   get(wal_reg_treated)
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms_",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*_","",names) ]
#   coef_data[, ms := gsub("neg","-",ms) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   coef_data[, ms := as.numeric(ms)]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\nMedicaid",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
# }
# 
# dev.off()
# 
# pdf(file="~/current/pharmacy_deserts/walgreens_no_weights_ins6.pdf", width = 18, height = 10)
# for (outcome in c("log_total_pills_dispensed_ym_ins_6_z","log_total_pats_ym_ins_6_z","log_total_claims_ym_ins_6_z",
#                   "log_total_new_pats_ym_ins_6_z_plus_1","log_total_money_ym_ins_6_z_plus_1", "log_pills_per_person_ym_ins_6_z",
#                   "mail_share_ym_ins_6_z")) {
#   # outcome = "log_total_new_pats_ym_ins_23_z_plus_1"
#   wal_reg = paste("wal_reg_",outcome, sep="")
#   wal_reg_treated = paste("wal_reg_treated_",outcome, sep="")
#   
#   if (grepl("total_pills_dispensed",outcome) & !(grepl("ring",outcome))) {
#     title = "Pills dispensed"
#   } else if (grepl("total_pats",outcome) & !(grepl("ring",outcome))) {
#     title = "Unique patients"
#   } else if (grepl("pills_per_person",outcome)) {
#     title = "Pills per Patient"
#   } else if (grepl("mail_share",outcome)) {
#     title = "Mail Share of Claims"
#   } else if (grepl("total_claims",outcome) & !(grepl("ring",outcome)) & !(grepl("inzip",outcome))){
#     title = "Total claims"
#   } else if (grepl("ring_mean_total_pills_dispensed",outcome)) {
#     title = "Ring Pills dispensed"
#   } else if (grepl("ring_mean_total_pats",outcome)) {
#     title = "Ring Unique patients"
#   } else if (grepl("ring_mean_total_claims",outcome)){
#     title = "Ring Total claims"
#   } else if (grepl("total_money",outcome)){
#     title = "Total money"
#   } else if (grepl("avg_age",outcome)) {
#     title = "Average age"
#   } else if (grepl("avg_copay",outcome)){
#     title = "Average copay"
#   } else if (grepl("total_new_pats",outcome)){
#     title = "New patients"
#   } else if (grepl("mean_mpr",outcome)){
#     title = "Med. Possesion Ratio"
#   } else if (grepl("share_medicaid",outcome)){
#     title = "Medicaid Share of Patients"
#   } else if (grepl("share_claims_inzip",outcome)) {
#     title = "Share of in-zip claims"
#   } else if (grepl("total_claims_inzip",outcome)) {
#     title = "Total in-zip claims"
#   } else if (grepl("total_unique_zips",outcome)) {
#     title = "Unique Pharmacy Zips"
#   } else if (grepl("log_avg_distance",outcome)) {
#     title = "Log Avg. Distance"
#   } else if (grepl("avg_distance",outcome)) {
#     title = "Avg. Distance"
#   }
#   
#   
#   
#   ###treated zips only
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  #ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#               ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#             + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6 #+ ms_wal_7 + ms_wal_8 + ms_wal_9
#             + zip_factor #+ yrmon_factor 
#             + zip_factor:yrmon_number
#             ,
#             data = zip_data_stacked[zip_ym_ins_6_index == 1 & (insurance_type_6 == "P")
#                                     & walgreens_zip_person == 1
#                                     & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                                     ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[zip_ym_ins_6_index == 1 & (insurance_type_6 == "P")
#                                     & walgreens_zip_person == 1
#                                     & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                                     ]$ZIP)))
#   get(wal_reg_treated)
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms_",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*_","",names) ]
#   coef_data[, ms := gsub("neg","-",ms) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   coef_data[, ms := as.numeric(ms)]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\nPharmacy Ben. Only",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   
#   ####all zips####
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  #ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#               walgreens_zip_person:(ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#                                     + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6) #+ ms_wal_7 + ms_wal_8 + ms_wal_9
#             + zip_factor #+ yrmon_factor 
#             + zip_factor:yrmon_number
#             ,
#             data = zip_data_stacked[zip_ym_ins_6_index == 1 & (insurance_type_6 == "P")
#                                     #                                   & walgreens_zip_person == 1
#                                     & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                                     #                                     & ((pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011") 
#                                     #                                        |  (pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012") )
#                                     #                                     & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                                     ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[zip_ym_ins_6_index == 1 & (insurance_type_6 == "P")
#                                     #                                   & walgreens_zip_person == 1
#                                     #                                     & ((pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011") 
#                                     #                                        |  (pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012") )
#                                     & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                                     #                                     & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                                     ]$ZIP)))
#   get(wal_reg_treated)
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms_",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*_","",names) ]
#   coef_data[, ms := gsub("neg","-",ms) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   coef_data[, ms := as.numeric(ms)]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\nPharmacy Ben. Only\nAll Zip Codes",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   
#   ###treated zips only - all data window
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  ms_wal_neg12 + ms_wal_neg11 + ms_wal_neg10 + ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#               ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#             + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6 + ms_wal_7 + ms_wal_8 + ms_wal_9
#             + ms_wal_10 + ms_wal_11 + ms_wal_12 + ms_wal_13 + ms_wal_14 + ms_wal_15 + ms_wal_16 + ms_wal_17 + ms_wal_18 + ms_wal_19
#             + ms_wal_20 + ms_wal_21 + ms_wal_22 + ms_wal_23
#             #           + as.factor(month_of_year)
#             + zip_factor #+ yrmon_factor 
#             + zip_factor:yrmon_number
#             
#             ,
#             data = zip_data_stacked[zip_ym_ins_6_index == 1 & (insurance_type_6 == "P")
#                                     & walgreens_zip_person == 1
#                                     #                                   & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                                     ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[zip_ym_ins_6_index == 1 & (insurance_type_6 == "P")
#                                     & walgreens_zip_person == 1
#                                     #                                   & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                                     ]$ZIP)))
#   get(wal_reg_treated)
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms_",names(get(wal_reg_treated)[,1]))]
#   # names_vector = names(get(wal_reg_treated)[,1])[grepl("yrmon_factor",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*_","",names) ]
#   coef_data[, ms := as.numeric(gsub("neg","-",ms)) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   # coef_data[, ms := as.yearmon(gsub("yrmon_factor","",names)) ]
#   # coef_data[, ms := round(as.numeric(ms - as.yearmon("Jan 2012"))*12) ]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_vline(xintercept=9,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     #   scale_x_continuous(breaks = seq(-B-3,B+3,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\nPharmacy Ben. Only",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   
#   ###all zips big window
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  walgreens_zip_person:(ms_wal_neg12 + ms_wal_neg11 + ms_wal_neg10 + ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#                                                     ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#                                                   + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6 + ms_wal_7 + ms_wal_8 + ms_wal_9
#                                                   + ms_wal_10 + ms_wal_11 + ms_wal_12 + ms_wal_13 + ms_wal_14 + ms_wal_15 + ms_wal_16 + ms_wal_17 + ms_wal_18 + ms_wal_19
#                                                   + ms_wal_20 + ms_wal_21 + ms_wal_22 + ms_wal_23)
#             + zip_factor #+ walgreens_zip_person:yrmon_factor 
#             + zip_factor:yrmon_number
#             #           + as.factor(month_of_year)
#             ,
#             data = zip_data_stacked[zip_ym_ins_6_index == 1 & (insurance_type_6 == "P")
#                                     #                                   & walgreens_zip_person == 1
#                                     #                                   & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                                     ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[zip_ym_ins_6_index == 1 & (insurance_type_6 == "P")
#                                     #                                   & walgreens_zip_person == 1
#                                     #                                   & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                                     ]$ZIP)))
#   get(wal_reg_treated)
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms_",names(get(wal_reg_treated)[,1]))]
#   # names_vector = names(get(wal_reg_treated)[,1])[grepl("yrmon_factor",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*_","",names) ]
#   coef_data[, ms := as.numeric(gsub("neg","-",ms)) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   # coef_data[, ms := as.yearmon(gsub(".*yrmon_factor","",names)) ]
#   # coef_data[, ms := round(as.numeric(ms - as.yearmon("Jan 2012"))*12) ]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_vline(xintercept=9,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     #   scale_x_continuous(breaks = seq(-B-3,B+3,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\nPharmacy Ben. Only\nAll Zip Codes",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   
#   ###all zips big window - include month of year FE####
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  walgreens_zip_person:(ms_wal_neg12 + ms_wal_neg11 + ms_wal_neg10 + ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#                                                     ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#                                                   + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6 + ms_wal_7 + ms_wal_8 + ms_wal_9
#                                                   + ms_wal_10 + ms_wal_11 + ms_wal_12 + ms_wal_13 + ms_wal_14 + ms_wal_15 + ms_wal_16 + ms_wal_17 + ms_wal_18 + ms_wal_19
#                                                   + ms_wal_20 + ms_wal_21 + ms_wal_22 + ms_wal_23)
#             + zip_factor #+ walgreens_zip_person:yrmon_factor 
#             + zip_factor:yrmon_number
#             + as.factor(month_of_year)
#             ,
#             data = zip_data_stacked[zip_ym_ins_6_index == 1 & (insurance_type_6 == "P")
#                                     #                                   & walgreens_zip_person == 1
#                                     #                                   & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                                     ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[zip_ym_ins_6_index == 1 & (insurance_type_6 == "P")
#                                     #                                   & walgreens_zip_person == 1
#                                     #                                   & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                                     ]$ZIP)))
#   get(wal_reg_treated)
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms_",names(get(wal_reg_treated)[,1]))]
#   # names_vector = names(get(wal_reg_treated)[,1])[grepl("yrmon_factor",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*_","",names) ]
#   coef_data[, ms := as.numeric(gsub("neg","-",ms)) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   # coef_data[, ms := as.yearmon(gsub(".*yrmon_factor","",names)) ]
#   # coef_data[, ms := round(as.numeric(ms - as.yearmon("Jan 2012"))*12) ]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_vline(xintercept=9,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     #   scale_x_continuous(breaks = seq(-B-3,B+3,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\nPharmacy Ben. Only\nAll Zip Codes",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   ####as a counterfactual, look at one year ahead
#   
#   
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  #ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#               ms_wal_neg6_plus1 + ms_wal_neg5_plus1 + ms_wal_neg4_plus1 + ms_wal_neg3_plus1 + ms_wal_neg2_plus1 
#             + ms_wal_0_plus1 + ms_wal_1_plus1 + ms_wal_2_plus1 + ms_wal_3_plus1 + ms_wal_4_plus1 + ms_wal_5_plus1 + ms_wal_6_plus1 #+ ms_wal_7 + ms_wal_8 + ms_wal_9
#             + zip_factor #+ yrmon_factor 
#             + zip_factor:yrmon_number
#             ,
#             data = zip_data_stacked[zip_ym_ins_6_index == 1 & (insurance_type_6 == "P")
#                                     & walgreens_zip_person == 1
#                                     & pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012"
#                                     #                                     & ((pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011") 
#                                     #                                        |  (pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012") )
#                                     #                                     & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                                     ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[zip_ym_ins_6_index == 1 & (insurance_type_6 == "P")
#                                     & walgreens_zip_person == 1
#                                     #                                     & ((pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011") 
#                                     #                                        |  (pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012") )
#                                     & pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012"
#                                     #                                     & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                                     ]$ZIP)))
#   get(wal_reg_treated)
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   # coef_data[, ms := gsub(".*factor","",names) ]
#   coef_data[, ms := gsub("_plus1","",names) ]
#   coef_data[, ms := gsub("neg","-",ms) ]
#   coef_data[, ms := as.numeric(gsub("ms_wal_","",ms)) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   # coef_data[, ms := round((as.yearmon(ms) - as.yearmon("Jan 2013"))*12)]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") +
#     geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\n(+1 Year)\nPharmacy Ben. Only",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   
#   
#   ####another counterfactual: public insurance
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  #ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#               ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#             + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6 #+ ms_wal_7 + ms_wal_8 + ms_wal_9
#             + zip_factor #+ yrmon_factor 
#             + zip_factor:yrmon_number
#             ,
#             data = zip_data_stacked[zip_ym_ins_6_index == 1 & (insurance_type_6 == "D")
#                                     & walgreens_zip_person == 1
#                                     & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                                     #                                     & ((pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011") 
#                                     #                                        |  (pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012") )
#                                     #                                     & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                                     ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[zip_ym_ins_6_index == 1 & (insurance_type_6 == "D")
#                                     & walgreens_zip_person == 1
#                                     #                                     & ((pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011") 
#                                     #                                        |  (pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012") )
#                                     & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                                     #                                     & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                                     ]$ZIP)))
#   get(wal_reg_treated)
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms_",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*_","",names) ]
#   coef_data[, ms := gsub("neg","-",ms) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   coef_data[, ms := as.numeric(ms)]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\nMedicaid",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
# }
# 
# dev.off()
# 
# ###use RXNKAWZ
# pdf(file="~/current/pharmacy_deserts/walgreens_no_weights_no_stack.pdf", width = 18, height = 10)
# for (outcome in c("log_total_pills_dispensed_ym_ins_23_z","log_total_pats_ym_ins_23_z","log_total_claims_ym_ins_23_z",
#                   "log_total_new_pats_ym_ins_23_z_plus_1","log_total_money_ym_ins_23_z_plus_1", "log_pills_per_person_ym_ins_23_z",
#                   "mail_share_ym_ins_23_z")) {
#   # outcome = "log_total_pills_dispensed_ym_ins_23_z"
#   wal_reg = paste("wal_reg_",outcome, sep="")
#   wal_reg_treated = paste("wal_reg_treated_",outcome, sep="")
#   
#   if (grepl("total_pills_dispensed",outcome) & !(grepl("ring",outcome))) {
#     title = "Pills dispensed"
#   } else if (grepl("total_pats",outcome) & !(grepl("ring",outcome))) {
#     title = "Unique patients"
#   } else if (grepl("pills_per_person",outcome)) {
#     title = "Pills per Patient"
#   } else if (grepl("mail_share",outcome)) {
#     title = "Mail Share of Claims"
#   } else if (grepl("total_claims",outcome) & !(grepl("ring",outcome)) & !(grepl("inzip",outcome))){
#     title = "Total claims"
#   } else if (grepl("ring_mean_total_pills_dispensed",outcome)) {
#     title = "Ring Pills dispensed"
#   } else if (grepl("ring_mean_total_pats",outcome)) {
#     title = "Ring Unique patients"
#   } else if (grepl("ring_mean_total_claims",outcome)){
#     title = "Ring Total claims"
#   } else if (grepl("total_money",outcome)){
#     title = "Total money"
#   } else if (grepl("avg_age",outcome)) {
#     title = "Average age"
#   } else if (grepl("avg_copay",outcome)){
#     title = "Average copay"
#   } else if (grepl("total_new_pats",outcome)){
#     title = "New patients"
#   } else if (grepl("mean_mpr",outcome)){
#     title = "Med. Possesion Ratio"
#   } else if (grepl("share_medicaid",outcome)){
#     title = "Medicaid Share of Patients"
#   } else if (grepl("share_claims_inzip",outcome)) {
#     title = "Share of in-zip claims"
#   } else if (grepl("total_claims_inzip",outcome)) {
#     title = "Total in-zip claims"
#   } else if (grepl("total_unique_zips",outcome)) {
#     title = "Unique Pharmacy Zips"
#   } else if (grepl("log_avg_distance",outcome)) {
#     title = "Log Avg. Distance"
#   } else if (grepl("avg_distance",outcome)) {
#     title = "Avg. Distance"
#   }
#   
#   
#   
#   ###treated zips only
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  #ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#               ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#             + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6 #+ ms_wal_7 + ms_wal_8 + ms_wal_9
#             + zip_factor #+ yrmon_factor 
#             + zip_factor:yrmon_number
#             ,
#             data = rxnkawz[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                            & walgreens_zip_person == 1
#                            & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                            ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,rxnkawz[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                            & walgreens_zip_person == 1
#                            & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                            ]$ZIP)))
#   get(wal_reg_treated)
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms_",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*_","",names) ]
#   coef_data[, ms := gsub("neg","-",ms) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   coef_data[, ms := as.numeric(ms)]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     #     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\nPharmacy Ben. Only",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   
#   ####all zips####
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  #ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#               walgreens_zip_person:(ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#                                     + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6) #+ ms_wal_7 + ms_wal_8 + ms_wal_9
#             + zip_factor #+ yrmon_factor 
#             + zip_factor:yrmon_number
#             ,
#             data = rxnkawz[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                            #                                   & walgreens_zip_person == 1
#                            & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                            #                                     & ((pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011") 
#                            #                                        |  (pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012") )
#                            #                                     & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                            ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,rxnkawz[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                            #                                   & walgreens_zip_person == 1
#                            #                                     & ((pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011") 
#                            #                                        |  (pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012") )
#                            & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                            #                                     & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                            ]$ZIP)))
#   get(wal_reg_treated)
#   # nobs(get(wal_reg_treated))
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms_",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*_","",names) ]
#   coef_data[, ms := gsub("neg","-",ms) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   coef_data[, ms := as.numeric(ms)]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     #     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\nPharmacy Ben. Only\nAll Zip Codes",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   
#   ###treated zips only - all data window
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  ms_wal_neg12 + ms_wal_neg11 + ms_wal_neg10 + ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#               ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#             + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6 + ms_wal_7 + ms_wal_8 + ms_wal_9
#             + ms_wal_10 + ms_wal_11 + ms_wal_12 + ms_wal_13 + ms_wal_14 + ms_wal_15 + ms_wal_16 + ms_wal_17 + ms_wal_18 + ms_wal_19
#             + ms_wal_20 + ms_wal_21 + ms_wal_22 + ms_wal_23
#             #           + as.factor(month_of_year)
#             + zip_factor #+ yrmon_factor 
#             + zip_factor:yrmon_number
#             
#             ,
#             data = rxnkawz[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                            & walgreens_zip_person == 1
#                            #                                   & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                            ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,rxnkawz[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                            & walgreens_zip_person == 1
#                            #                                   & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                            ]$ZIP)))
#   get(wal_reg_treated)
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms_",names(get(wal_reg_treated)[,1]))]
#   # names_vector = names(get(wal_reg_treated)[,1])[grepl("yrmon_factor",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*_","",names) ]
#   coef_data[, ms := as.numeric(gsub("neg","-",ms)) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   # coef_data[, ms := as.yearmon(gsub("yrmon_factor","",names)) ]
#   # coef_data[, ms := round(as.numeric(ms - as.yearmon("Jan 2012"))*12) ]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_vline(xintercept=9,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     #   scale_x_continuous(breaks = seq(-B-3,B+3,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\nPharmacy Ben. Only",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   
#   ###all zips big window
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  walgreens_zip_person:(ms_wal_neg12 + ms_wal_neg11 + ms_wal_neg10 + ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#                                                     ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#                                                   + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6 + ms_wal_7 + ms_wal_8 + ms_wal_9
#                                                   + ms_wal_10 + ms_wal_11 + ms_wal_12 + ms_wal_13 + ms_wal_14 + ms_wal_15 + ms_wal_16 + ms_wal_17 + ms_wal_18 + ms_wal_19
#                                                   + ms_wal_20 + ms_wal_21 + ms_wal_22 + ms_wal_23)
#             + zip_factor #+ walgreens_zip_person:yrmon_factor 
#             + zip_factor:yrmon_number
#             #           + as.factor(month_of_year)
#             ,
#             data = rxnkawz[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                            #                                   & walgreens_zip_person == 1
#                            #                                   & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                            ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,rxnkawz[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                            #                                   & walgreens_zip_person == 1
#                            #                                   & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                            ]$ZIP)))
#   get(wal_reg_treated)
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms_",names(get(wal_reg_treated)[,1]))]
#   # names_vector = names(get(wal_reg_treated)[,1])[grepl("yrmon_factor",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*_","",names) ]
#   coef_data[, ms := as.numeric(gsub("neg","-",ms)) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   # coef_data[, ms := as.yearmon(gsub(".*yrmon_factor","",names)) ]
#   # coef_data[, ms := round(as.numeric(ms - as.yearmon("Jan 2012"))*12) ]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_vline(xintercept=9,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     #   scale_x_continuous(breaks = seq(-B-3,B+3,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\nPharmacy Ben. Only\nAll Zip Codes",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   
#   ###all zips big window - include month of year FE####
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  walgreens_zip_person:(ms_wal_neg12 + ms_wal_neg11 + ms_wal_neg10 + ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#                                                     ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#                                                   + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6 + ms_wal_7 + ms_wal_8 + ms_wal_9
#                                                   + ms_wal_10 + ms_wal_11 + ms_wal_12 + ms_wal_13 + ms_wal_14 + ms_wal_15 + ms_wal_16 + ms_wal_17 + ms_wal_18 + ms_wal_19
#                                                   + ms_wal_20 + ms_wal_21 + ms_wal_22 + ms_wal_23)
#             + zip_factor #+ walgreens_zip_person:yrmon_factor 
#             + zip_factor:yrmon_number
#             + as.factor(month_of_year)
#             ,
#             data = rxnkawz[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                            #                                   & walgreens_zip_person == 1
#                            #                                   & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                            ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,rxnkawz[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                            #                                   & walgreens_zip_person == 1
#                            #                                   & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                            ]$ZIP)))
#   get(wal_reg_treated)
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms_",names(get(wal_reg_treated)[,1]))]
#   # names_vector = names(get(wal_reg_treated)[,1])[grepl("yrmon_factor",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*_","",names) ]
#   coef_data[, ms := as.numeric(gsub("neg","-",ms)) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   # coef_data[, ms := as.yearmon(gsub(".*yrmon_factor","",names)) ]
#   # coef_data[, ms := round(as.numeric(ms - as.yearmon("Jan 2012"))*12) ]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_vline(xintercept=9,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     #   scale_x_continuous(breaks = seq(-B-3,B+3,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\nPharmacy Ben. Only\nAll Zip Codes",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   ####as a counterfactual, look at one year ahead
#   
#   
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  #ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#               ms_wal_neg6_plus1 + ms_wal_neg5_plus1 + ms_wal_neg4_plus1 + ms_wal_neg3_plus1 + ms_wal_neg2_plus1 
#             + ms_wal_0_plus1 + ms_wal_1_plus1 + ms_wal_2_plus1 + ms_wal_3_plus1 + ms_wal_4_plus1 + ms_wal_5_plus1 + ms_wal_6_plus1 #+ ms_wal_7 + ms_wal_8 + ms_wal_9
#             + zip_factor #+ yrmon_factor 
#             + zip_factor:yrmon_number
#             ,
#             data = rxnkawz[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                            & walgreens_zip_person == 1
#                            & pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012"
#                            #                                     & ((pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011") 
#                            #                                        |  (pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012") )
#                            #                                     & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                            ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,rxnkawz[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                            & walgreens_zip_person == 1
#                            #                                     & ((pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011") 
#                            #                                        |  (pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012") )
#                            & pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012"
#                            #                                     & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                            ]$ZIP)))
#   get(wal_reg_treated)
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   # coef_data[, ms := gsub(".*factor","",names) ]
#   coef_data[, ms := gsub("_plus1","",names) ]
#   coef_data[, ms := gsub("neg","-",ms) ]
#   coef_data[, ms := as.numeric(gsub("ms_wal_","",ms)) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   # coef_data[, ms := round((as.yearmon(ms) - as.yearmon("Jan 2013"))*12)]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") +
#     geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     #     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\n(+1 Year)\nPharmacy Ben. Only",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   
#   
#   ####another counterfactual: public insurance
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  #ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#               ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#             + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6 #+ ms_wal_7 + ms_wal_8 + ms_wal_9
#             + zip_factor #+ yrmon_factor 
#             + zip_factor:yrmon_number
#             ,
#             data = rxnkawz[zip_ym_ins_23_index == 1 & (insurance_type_23 %in% c("MLI","MD","MRB","MDF"))
#                            & walgreens_zip_person == 1
#                            & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                            #                                     & ((pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011") 
#                            #                                        |  (pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012") )
#                            #                                     & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                            ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,rxnkawz[zip_ym_ins_23_index == 1 & (insurance_type_23 %in% c("MLI","MD","MRB","MDF"))
#                            & walgreens_zip_person == 1
#                            #                                     & ((pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011") 
#                            #                                        |  (pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012") )
#                            & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                            #                                     & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                            ]$ZIP)))
#   get(wal_reg_treated)
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms_",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*_","",names) ]
#   coef_data[, ms := gsub("neg","-",ms) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   coef_data[, ms := as.numeric(ms)]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     #     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\nMedicaid",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
# }
# 
# dev.off()
# 
# pdf(file="~/current/pharmacy_deserts/walgreens_no_weights_no_stack_ins6.pdf", width = 18, height = 10)
# for (outcome in c("log_total_pills_dispensed_ym_ins_6_z","log_total_pats_ym_ins_6_z","log_total_claims_ym_ins_6_z",
#                   "log_total_new_pats_ym_ins_6_z_plus_1","log_total_money_ym_ins_6_z_plus_1", "log_pills_per_person_ym_ins_6_z",
#                   "mail_share_ym_ins_6_z")) {
#   # outcome = "log_total_new_pats_ym_ins_6_z_plus_1"
#   wal_reg = paste("wal_reg_",outcome, sep="")
#   wal_reg_treated = paste("wal_reg_treated_",outcome, sep="")
#   
#   if (grepl("total_pills_dispensed",outcome) & !(grepl("ring",outcome))) {
#     title = "Pills dispensed"
#   } else if (grepl("total_pats",outcome) & !(grepl("ring",outcome))) {
#     title = "Unique patients"
#   } else if (grepl("pills_per_person",outcome)) {
#     title = "Pills per Patient"
#   } else if (grepl("mail_share",outcome)) {
#     title = "Mail Share of Claims"
#   } else if (grepl("total_claims",outcome) & !(grepl("ring",outcome)) & !(grepl("inzip",outcome))){
#     title = "Total claims"
#   } else if (grepl("ring_mean_total_pills_dispensed",outcome)) {
#     title = "Ring Pills dispensed"
#   } else if (grepl("ring_mean_total_pats",outcome)) {
#     title = "Ring Unique patients"
#   } else if (grepl("ring_mean_total_claims",outcome)){
#     title = "Ring Total claims"
#   } else if (grepl("total_money",outcome)){
#     title = "Total money"
#   } else if (grepl("avg_age",outcome)) {
#     title = "Average age"
#   } else if (grepl("avg_copay",outcome)){
#     title = "Average copay"
#   } else if (grepl("total_new_pats",outcome)){
#     title = "New patients"
#   } else if (grepl("mean_mpr",outcome)){
#     title = "Med. Possesion Ratio"
#   } else if (grepl("share_medicaid",outcome)){
#     title = "Medicaid Share of Patients"
#   } else if (grepl("share_claims_inzip",outcome)) {
#     title = "Share of in-zip claims"
#   } else if (grepl("total_claims_inzip",outcome)) {
#     title = "Total in-zip claims"
#   } else if (grepl("total_unique_zips",outcome)) {
#     title = "Unique Pharmacy Zips"
#   } else if (grepl("log_avg_distance",outcome)) {
#     title = "Log Avg. Distance"
#   } else if (grepl("avg_distance",outcome)) {
#     title = "Avg. Distance"
#   }
#   
#   
#   
#   ###treated zips only
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  #ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#               ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#             + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6 #+ ms_wal_7 + ms_wal_8 + ms_wal_9
#             + zip_factor #+ yrmon_factor 
#             + zip_factor:yrmon_number
#             ,
#             data = rxnkawz[zip_ym_ins_6_index == 1 & (insurance_type_6 == "P")
#                            & walgreens_zip_person == 1
#                            & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                            ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,rxnkawz[zip_ym_ins_6_index == 1 & (insurance_type_6 == "P")
#                            & walgreens_zip_person == 1
#                            & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                            ]$ZIP)))
#   get(wal_reg_treated)
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms_",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*_","",names) ]
#   coef_data[, ms := gsub("neg","-",ms) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   coef_data[, ms := as.numeric(ms)]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\nPharmacy Ben. Only",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   
#   ####all zips####
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  #ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#               walgreens_zip_person:(ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#                                     + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6) #+ ms_wal_7 + ms_wal_8 + ms_wal_9
#             + zip_factor #+ yrmon_factor 
#             + zip_factor:yrmon_number
#             ,
#             data = rxnkawz[zip_ym_ins_6_index == 1 & (insurance_type_6 == "P")
#                            #                                   & walgreens_zip_person == 1
#                            & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                            #                                     & ((pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011") 
#                            #                                        |  (pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012") )
#                            #                                     & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                            ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,rxnkawz[zip_ym_ins_6_index == 1 & (insurance_type_6 == "P")
#                            #                                   & walgreens_zip_person == 1
#                            #                                     & ((pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011") 
#                            #                                        |  (pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012") )
#                            & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                            #                                     & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                            ]$ZIP)))
#   get(wal_reg_treated)
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms_",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*_","",names) ]
#   coef_data[, ms := gsub("neg","-",ms) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   coef_data[, ms := as.numeric(ms)]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\nPharmacy Ben. Only\nAll Zip Codes",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   
#   ###treated zips only - all data window
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  ms_wal_neg12 + ms_wal_neg11 + ms_wal_neg10 + ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#               ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#             + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6 + ms_wal_7 + ms_wal_8 + ms_wal_9
#             + ms_wal_10 + ms_wal_11 + ms_wal_12 + ms_wal_13 + ms_wal_14 + ms_wal_15 + ms_wal_16 + ms_wal_17 + ms_wal_18 + ms_wal_19
#             + ms_wal_20 + ms_wal_21 + ms_wal_22 + ms_wal_23
#             #           + as.factor(month_of_year)
#             + zip_factor #+ yrmon_factor 
#             + zip_factor:yrmon_number
#             
#             ,
#             data = rxnkawz[zip_ym_ins_6_index == 1 & (insurance_type_6 == "P")
#                            & walgreens_zip_person == 1
#                            #                                   & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                            ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,rxnkawz[zip_ym_ins_6_index == 1 & (insurance_type_6 == "P")
#                            & walgreens_zip_person == 1
#                            #                                   & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                            ]$ZIP)))
#   get(wal_reg_treated)
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms_",names(get(wal_reg_treated)[,1]))]
#   # names_vector = names(get(wal_reg_treated)[,1])[grepl("yrmon_factor",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*_","",names) ]
#   coef_data[, ms := as.numeric(gsub("neg","-",ms)) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   # coef_data[, ms := as.yearmon(gsub("yrmon_factor","",names)) ]
#   # coef_data[, ms := round(as.numeric(ms - as.yearmon("Jan 2012"))*12) ]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_vline(xintercept=9,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     #   scale_x_continuous(breaks = seq(-B-3,B+3,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\nPharmacy Ben. Only",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   
#   ###all zips big window
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  walgreens_zip_person:(ms_wal_neg12 + ms_wal_neg11 + ms_wal_neg10 + ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#                                                     ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#                                                   + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6 + ms_wal_7 + ms_wal_8 + ms_wal_9
#                                                   + ms_wal_10 + ms_wal_11 + ms_wal_12 + ms_wal_13 + ms_wal_14 + ms_wal_15 + ms_wal_16 + ms_wal_17 + ms_wal_18 + ms_wal_19
#                                                   + ms_wal_20 + ms_wal_21 + ms_wal_22 + ms_wal_23)
#             + zip_factor #+ walgreens_zip_person:yrmon_factor 
#             + zip_factor:yrmon_number
#             #           + as.factor(month_of_year)
#             ,
#             data = rxnkawz[zip_ym_ins_6_index == 1 & (insurance_type_6 == "P")
#                            #                                   & walgreens_zip_person == 1
#                            #                                   & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                            ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,rxnkawz[zip_ym_ins_6_index == 1 & (insurance_type_6 == "P")
#                            #                                   & walgreens_zip_person == 1
#                            #                                   & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                            ]$ZIP)))
#   get(wal_reg_treated)
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms_",names(get(wal_reg_treated)[,1]))]
#   # names_vector = names(get(wal_reg_treated)[,1])[grepl("yrmon_factor",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*_","",names) ]
#   coef_data[, ms := as.numeric(gsub("neg","-",ms)) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   # coef_data[, ms := as.yearmon(gsub(".*yrmon_factor","",names)) ]
#   # coef_data[, ms := round(as.numeric(ms - as.yearmon("Jan 2012"))*12) ]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_vline(xintercept=9,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     #   scale_x_continuous(breaks = seq(-B-3,B+3,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\nPharmacy Ben. Only\nAll Zip Codes",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   
#   ###all zips big window - include month of year FE####
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  walgreens_zip_person:(ms_wal_neg12 + ms_wal_neg11 + ms_wal_neg10 + ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#                                                     ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#                                                   + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6 + ms_wal_7 + ms_wal_8 + ms_wal_9
#                                                   + ms_wal_10 + ms_wal_11 + ms_wal_12 + ms_wal_13 + ms_wal_14 + ms_wal_15 + ms_wal_16 + ms_wal_17 + ms_wal_18 + ms_wal_19
#                                                   + ms_wal_20 + ms_wal_21 + ms_wal_22 + ms_wal_23)
#             + zip_factor #+ walgreens_zip_person:yrmon_factor 
#             + zip_factor:yrmon_number
#             + as.factor(month_of_year)
#             ,
#             data = rxnkawz[zip_ym_ins_6_index == 1 & (insurance_type_6 == "P")
#                            #                                   & walgreens_zip_person == 1
#                            #                                   & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                            ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,rxnkawz[zip_ym_ins_6_index == 1 & (insurance_type_6 == "P")
#                            #                                   & walgreens_zip_person == 1
#                            #                                   & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                            ]$ZIP)))
#   get(wal_reg_treated)
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms_",names(get(wal_reg_treated)[,1]))]
#   # names_vector = names(get(wal_reg_treated)[,1])[grepl("yrmon_factor",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*_","",names) ]
#   coef_data[, ms := as.numeric(gsub("neg","-",ms)) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   # coef_data[, ms := as.yearmon(gsub(".*yrmon_factor","",names)) ]
#   # coef_data[, ms := round(as.numeric(ms - as.yearmon("Jan 2012"))*12) ]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_vline(xintercept=9,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     #   scale_x_continuous(breaks = seq(-B-3,B+3,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\nPharmacy Ben. Only\nAll Zip Codes",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   ####as a counterfactual, look at one year ahead
#   
#   
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  #ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#               ms_wal_neg6_plus1 + ms_wal_neg5_plus1 + ms_wal_neg4_plus1 + ms_wal_neg3_plus1 + ms_wal_neg2_plus1 
#             + ms_wal_0_plus1 + ms_wal_1_plus1 + ms_wal_2_plus1 + ms_wal_3_plus1 + ms_wal_4_plus1 + ms_wal_5_plus1 + ms_wal_6_plus1 #+ ms_wal_7 + ms_wal_8 + ms_wal_9
#             + zip_factor #+ yrmon_factor 
#             + zip_factor:yrmon_number
#             ,
#             data = rxnkawz[zip_ym_ins_6_index == 1 & (insurance_type_6 == "P")
#                            & walgreens_zip_person == 1
#                            & pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012"
#                            #                                     & ((pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011") 
#                            #                                        |  (pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012") )
#                            #                                     & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                            ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,rxnkawz[zip_ym_ins_6_index == 1 & (insurance_type_6 == "P")
#                            & walgreens_zip_person == 1
#                            #                                     & ((pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011") 
#                            #                                        |  (pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012") )
#                            & pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012"
#                            #                                     & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                            ]$ZIP)))
#   get(wal_reg_treated)
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   # coef_data[, ms := gsub(".*factor","",names) ]
#   coef_data[, ms := gsub("_plus1","",names) ]
#   coef_data[, ms := gsub("neg","-",ms) ]
#   coef_data[, ms := as.numeric(gsub("ms_wal_","",ms)) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   # coef_data[, ms := round((as.yearmon(ms) - as.yearmon("Jan 2013"))*12)]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") +
#     geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\n(+1 Year)\nPharmacy Ben. Only",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   
#   
#   ####another counterfactual: public insurance
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  #ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#               ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#             + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6 #+ ms_wal_7 + ms_wal_8 + ms_wal_9
#             + zip_factor #+ yrmon_factor 
#             + zip_factor:yrmon_number
#             ,
#             data = rxnkawz[zip_ym_ins_6_index == 1 & (insurance_type_6 %in% c("D"))
#                            & walgreens_zip_person == 1
#                            & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                            #                                     & ((pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011") 
#                            #                                        |  (pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012") )
#                            #                                     & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                            ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,rxnkawz[zip_ym_ins_6_index == 1 & (insurance_type_6 %in% c("D"))
#                            & walgreens_zip_person == 1
#                            #                                     & ((pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011") 
#                            #                                        |  (pick_up_yrmon < "Aug 2013" & pick_up_yrmon > "Jun 2012") )
#                            & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                            #                                     & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                            ]$ZIP)))
#   get(wal_reg_treated)
#   names_vector = names(get(wal_reg_treated)[,1])[grepl("ms_",names(get(wal_reg_treated)[,1]))]
#   coef_vector =get(wal_reg_treated)[,1][names_vector]
#   se_vector = get(wal_reg_treated)[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*_","",names) ]
#   coef_data[, ms := gsub("neg","-",ms) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   coef_data[, ms := as.numeric(ms)]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)\nMedicaid",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
# }
# 
# dev.off()
# 
# 
# 
# #loop over all insurance types individually
# pdf(file="~/current/pharmacy_deserts/walgreens_no_weights_each_ins23.pdf", width = 18, height = 10)
# for (outcome in c("log_total_pills_dispensed_ym_ins_23_z","log_total_pats_ym_ins_23_z","log_total_claims_ym_ins_23_z",
#                   "log_total_new_pats_ym_ins_23_z_plus_1","log_total_money_ym_ins_23_z_plus_1")) {
#   for (INS in sort(unique(zip_data_stacked[!(insurance_type_23 %in% c("SN1","SN2","SN3","ZZ"))]$insurance_type_23))) {
#     # outcome = "log_total_pills_dispensed_ym_ins_23_z"
#     wal_reg = paste("wal_reg_",outcome, sep="")
#     wal_reg_treated = paste("wal_reg_treated_",outcome, sep="")
#     
#     if (grepl("total_pills_dispensed",outcome) & !(grepl("ring",outcome))) {
#       title = "Pills dispensed"
#     } else if (grepl("total_pats",outcome) & !(grepl("ring",outcome))) {
#       title = "Unique patients"
#     } else if (grepl("total_claims",outcome) & !(grepl("ring",outcome)) & !(grepl("inzip",outcome))){
#       title = "Total claims"
#     } else if (grepl("ring_mean_total_pills_dispensed",outcome)) {
#       title = "Ring Pills dispensed"
#     } else if (grepl("ring_mean_total_pats",outcome)) {
#       title = "Ring Unique patients"
#     } else if (grepl("ring_mean_total_claims",outcome)){
#       title = "Ring Total claims"
#     } else if (grepl("total_money",outcome)){
#       title = "Total money"
#     } else if (grepl("avg_age",outcome)) {
#       title = "Average age"
#     } else if (grepl("avg_copay",outcome)){
#       title = "Average copay"
#     } else if (grepl("total_new_pats",outcome)){
#       title = "New patients"
#     } else if (grepl("mean_mpr",outcome)){
#       title = "Med. Possesion Ratio"
#     } else if (grepl("share_medicaid",outcome)){
#       title = "Medicaid Share of Patients"
#     } else if (grepl("share_claims_inzip",outcome)) {
#       title = "Share of in-zip claims"
#     } else if (grepl("total_claims_inzip",outcome)) {
#       title = "Total in-zip claims"
#     } else if (grepl("total_unique_zips",outcome)) {
#       title = "Unique Pharmacy Zips"
#     } else if (grepl("log_avg_distance",outcome)) {
#       title = "Log Avg. Distance"
#     } else if (grepl("avg_distance",outcome)) {
#       title = "Avg. Distance"
#     }
#     
#     
#     ###treated zips only
#     assign(eval(wal_reg_treated), 
#            lm(get(outcome) ~  #ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#                 ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#               + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6 #+ ms_wal_7 + ms_wal_8 + ms_wal_9
#               + zip_factor #+ yrmon_factor 
#               + zip_factor:yrmon_number
#               ,
#               data = zip_data_stacked[zip_ym_ins_23_index == 1 & (insurance_type_23 == INS)
#                                       & walgreens_zip_person == 1
#                                       & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                                       ]))
#     
#     #cluster standard errors at the zip code level
#     assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#       cluster.vcov(x,zip_data_stacked[zip_ym_ins_23_index == 1 & (insurance_type_23 == INS)
#                                       & walgreens_zip_person == 1
#                                       & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                                       ]$ZIP)))
#     get(wal_reg_treated)
#     names_vector = names(get(wal_reg_treated)[,1])[grepl("ms_",names(get(wal_reg_treated)[,1]))]
#     coef_vector =get(wal_reg_treated)[,1][names_vector]
#     se_vector = get(wal_reg_treated)[,2][names_vector]
#     
#     ymin = coef_vector - 1.96 * se_vector
#     ymax = coef_vector + 1.96 * se_vector
#     
#     coef_data = data.table(coef = coef_vector,
#                            se = se_vector,
#                            ymin = ymin,
#                            ymax = ymax,
#                            names = names_vector)
#     coef_data[, ms := gsub(".*_","",names) ]
#     coef_data[, ms := as.numeric(gsub("neg","-",ms)) ]
#     addneg1 = data.table(coef = 0, ms = -1)
#     coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#     coef_data[, ms := as.numeric(ms)]
#     
#     ####plot###
#     plot_nc = ggplot(data = coef_data) +
#       geom_point(aes(y=coef,x=ms),size = 8) +
#       geom_line(aes(y=coef,x=ms)) +
#       geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#       geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#       theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#       scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title," \n(no controls)\n",INS,sep="")) 
#     
#     #                 
#     print(plot_nc)
#     rm(ymin, ymax, coef_vector, se_vector)
#     
#     ####treated zips only - full time window
#     assign(eval(wal_reg_treated), 
#            lm(get(outcome) ~  ms_wal_neg12 + ms_wal_neg11 + ms_wal_neg10 + ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#                 ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#               + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6 + ms_wal_7 + ms_wal_8 + ms_wal_9
#               + ms_wal_10 + ms_wal_11 + ms_wal_12 + ms_wal_13 + ms_wal_14 + ms_wal_15 + ms_wal_16 + ms_wal_17 + ms_wal_18 + ms_wal_19
#               + ms_wal_20 + ms_wal_21 + ms_wal_22 + ms_wal_23
#               + zip_factor #+ yrmon_factor 
#               + zip_factor:yrmon_number
#               ,
#               data = zip_data_stacked[zip_ym_ins_23_index == 1 & (insurance_type_23 == INS)
#                                       & walgreens_zip_person == 1
#                                       #                                       & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                                       ]))
#     
#     #cluster standard errors at the zip code level
#     assign(eval(wal_reg_treated) , coeftest(get(wal_reg_treated), vcov=function(x) 
#       cluster.vcov(x,zip_data_stacked[zip_ym_ins_23_index == 1 & (insurance_type_23 == INS)
#                                       & walgreens_zip_person == 1
#                                       #                                       & pick_up_yrmon < "Nov 2012" & pick_up_yrmon > "Mar 2011"
#                                       ]$ZIP)))
#     get(wal_reg_treated)
#     names_vector = names(get(wal_reg_treated)[,1])[grepl("ms_",names(get(wal_reg_treated)[,1]))]
#     #     names_vector = names(get(wal_reg_treated)[,1])[grepl("yrmon_factor",names(get(wal_reg_treated)[,1]))]
#     coef_vector =get(wal_reg_treated)[,1][names_vector]
#     se_vector = get(wal_reg_treated)[,2][names_vector]
#     
#     ymin = coef_vector - 1.96 * se_vector
#     ymax = coef_vector + 1.96 * se_vector
#     
#     coef_data = data.table(coef = coef_vector,
#                            se = se_vector,
#                            ymin = ymin,
#                            ymax = ymax,
#                            names = names_vector)
#     coef_data[, ms := gsub(".*_","",names) ]
#     coef_data[, ms := as.numeric(gsub("neg","-",ms)) ]
#     addneg1 = data.table(coef = 0, ms = -1)
#     coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#     #     coef_data[, ms := as.yearmon(gsub("yrmon_factor","",names)) ]
#     #     coef_data[, ms := round(as.numeric(ms - as.yearmon("Jan 2012"))*12) ]
#     
#     ####plot###
#     plot_nc = ggplot(data = coef_data) +
#       geom_point(aes(y=coef,x=ms),size = 8) +
#       geom_line(aes(y=coef,x=ms)) +
#       geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#       geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#       theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#       #   scale_x_continuous(breaks = seq(-B-3,B+3,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title," \n(no controls)\n",INS,sep="")) 
#     
#     #                 
#     print(plot_nc)
#   }
# }
# 
# dev.off()
# 
# 
# ####walgreens DD: geography####
# zip_data_stacked[, back_in_network := ifelse(pick_up_yrmon > "Sep 2012", 1, 0)]
# zip_data_stacked[, out_of_network := ifelse(pick_up_yrmon <= "Sep 2012" & pick_up_yrmon >= "Jan 2012", 1, 0)]
# 
# 
# for (outcome in c("log_total_pills_dispensed_ym_ins_23_z","log_total_pats_ym_ins_23_z","log_total_claims_ym_ins_23_z",
#                   "log_total_new_pats_ym_ins_23_z_plus_1","log_total_money_ym_ins_23_z_plus_1", "log_pills_per_person_ym_ins_23_z",
#                   "mail_share_ym_ins_23_z")) {
#   # outcome = "log_total_pills_dispensed_ym_ins_23_z"
#   wal_reg = paste("wal_reg_",outcome, sep="")
#   wal_reg_cl = paste("wal_reg_cl_",outcome, sep="")
#   wal_reg_treated = paste("wal_reg_treated_",outcome, sep="")
#   wal_reg_treated_cl = paste("wal_reg_treated_cl_",outcome, sep="")
#   wal_reg_treated_2 = paste("wal_reg_treated_2_",outcome, sep="")
#   wal_reg_treated_2_cl = paste("wal_reg_treated_2_cl_",outcome, sep="")
#   
#   
#   #all zip codes
#   assign(eval(wal_reg), 
#          lm(get(outcome) ~ walgreens_zip_person:post_wal 
#             #             + post_wal
#             + zip_factor #+ yrmon_factor
#             + zip_factor:yrmon_number
#             ,
#             data = zip_data_stacked[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                                     & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_cl) , coeftest(get(wal_reg), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                                     & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"]$ZIP)))
#   
#   get(wal_reg_cl)[,1]["walgreens_zip_person:post_wal"]
#   get(wal_reg_cl)[,2]["walgreens_zip_person:post_wal"]
#   
#   
#   ###treated zips only
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~ post_wal
#             + zip_factor #+ yrmon_factor
#             + zip_factor:yrmon_number,
#             data = zip_data_stacked[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                                     #                                     & STATE == "OR" & pharm_state == "OR"
#                                     & walgreens_zip_person == 1
#                                     & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated_cl) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                                     #                                     & STATE == "OR" & pharm_state == "OR" 
#                                     & walgreens_zip_person == 1
#                                     & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"]$ZIP)))
#   
#   get(wal_reg_treated_cl)[,1]["post_wal"]
#   get(wal_reg_treated_cl)[,2]["post_wal"]
#   
#   ###treated zips only- 2 time dummies
#   assign(eval(wal_reg_treated_2), 
#          lm(get(outcome) ~ out_of_network + back_in_network
#             + zip_factor #+ yrmon_factor
#             + zip_factor:yrmon_number,
#             data = zip_data_stacked[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                                     #                                     & STATE == "OR" & pharm_state == "OR"
#                                     & walgreens_zip_person == 1]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated_2_cl) , coeftest(get(wal_reg_treated_2), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[zip_ym_ins_23_index == 1 & (insurance_type_23 == "PH")
#                                     #                                     & STATE == "OR" & pharm_state == "OR" 
#                                     & walgreens_zip_person == 1]$ZIP)))
#   
#   get(wal_reg_treated_2_cl)[,1]["out_of_network"]
#   get(wal_reg_treated_2_cl)[,2]["out_of_network"]
#   get(wal_reg_treated_2_cl)[,1]["back_in_network"] #should be 0
#   get(wal_reg_treated_2_cl)[,2]["back_in_network"]
#   
#   
# }
# ####walgreens tables####
# for (outcome in c("log_total_pills_dispensed_ym_ins_23_z","log_total_pats_ym_ins_23_z","log_total_claims_ym_ins_23_z",
#                   "log_total_new_pats_ym_ins_23_z_plus_1","log_total_money_ym_ins_23_z_plus_1", "log_pills_per_person_ym_ins_23_z",
#                   "mail_share_ym_ins_23_z")) {
#   
#   wal_reg = paste("wal_reg_",outcome, sep="")
#   wal_reg_cl = paste("wal_reg_cl_",outcome, sep="")
#   wal_reg_treated = paste("wal_reg_treated_",outcome, sep="")
#   wal_reg_treated_cl = paste("wal_reg_treated_cl_",outcome, sep="")
#   wal_reg_treated_2 = paste("wal_reg_treated_2_",outcome, sep="")
#   wal_reg_treated_2_cl = paste("wal_reg_treated_2_cl_",outcome, sep="")
#   
#   if (grepl("total_pills_dispensed",outcome) & !(grepl("ring",outcome))) {
#     title = "Pills dispensed"
#   } else if (grepl("total_pats",outcome) & !(grepl("ring",outcome))) {
#     title = "Unique patients"
#   } else if (grepl("pills_per_person",outcome)) {
#     title = "Pills per Patient"
#   } else if (grepl("mail_share",outcome)) {
#     title = "Mail Share of Claims"
#   } else if (grepl("total_claims",outcome) & !(grepl("ring",outcome)) & !(grepl("inzip",outcome))){
#     title = "Total claims"
#   } else if (grepl("ring_mean_total_pills_dispensed",outcome)) {
#     title = "Ring Pills dispensed"
#   } else if (grepl("ring_mean_total_pats",outcome)) {
#     title = "Ring Unique patients"
#   } else if (grepl("ring_mean_total_claims",outcome)){
#     title = "Ring Total claims"
#   } else if (grepl("total_money",outcome)){
#     title = "Total money"
#   } else if (grepl("avg_age",outcome)) {
#     title = "Average age"
#   } else if (grepl("avg_copay",outcome)){
#     title = "Average copay"
#   } else if (grepl("total_new_pats",outcome)){
#     title = "New patients"
#   } else if (grepl("mean_mpr",outcome)){
#     title = "Med. Possesion Ratio"
#   } else if (grepl("share_medicaid",outcome)){
#     title = "Medicaid Share of Patients"
#   } else if (grepl("share_claims_inzip",outcome)) {
#     title = "Share of in-zip claims"
#   } else if (grepl("total_claims_inzip",outcome)) {
#     title = "Total in-zip claims"
#   } else if (grepl("total_unique_zips",outcome)) {
#     title = "Unique Pharmacy Zips"
#   } else if (grepl("log_avg_distance",outcome)) {
#     title = "Log Avg. Distance"
#   } else if (grepl("avg_distance",outcome)) {
#     title = "Avg. Distance"
#   }
#   
#   print(outcome)
#   stargazer(get(wal_reg),get(wal_reg_treated),get(wal_reg_treated_2),
#             no.space=T,
#             #             type = "text",
#             out = paste("~/current/pharmacy_deserts/post_regs_",outcome,"_walgreens_no_weight.tex",sep=""),
#             omit.stat = c("rsq", "f","adj.rsq","ser"),
#             keep.stat = c("n"),
#             dep.var.labels = c(title),
#             dep.var.caption="",
#             #             covariate.labels = c(title,"Out of Network","Back in Network"),
#             model.numbers=F,
#             multicolumn=T,
#             model.names=F,
#             header=F,
#             keep = c("post","out_of_network","back_in_network"),
#             align =T,
#             font.size = "footnotesize",
#             column.sep.width = "1pt",
#             p = list(get(wal_reg_cl)[,4],get(wal_reg_treated_cl)[,4],get(wal_reg_treated_2_cl)[,4]),
#             se = list(get(wal_reg_cl)[,2],get(wal_reg_treated_cl)[,2],get(wal_reg_treated_2_cl)[,2]),
#             add.lines = list(c("All Zips", "Y","N","N"))
#   )
#   
#   
# }
# 
# 
# ####drug type heterogeneity####
# pdf(file="~/current/pharmacy_deserts/walgreens_no_weights_drug_split.pdf", width = 18, height = 10)
# #limit to private insurance
# #treatment: living in a zip code where walgreens is
# #control: living in a non walgreens zip code
# #months since figures
# for (outcome in c("log_total_pills_dispensed_ym_ins_23_rx_z","log_total_pats_ym_ins_23_rx_z","log_total_claims_ym_ins_23_rx_z",
#                   "log_total_new_pats_ym_ins_23_rx_z_plus_1","log_total_money_ym_ins_23_rx_z")) {
#   # outcome = "log_total_pills_dispensed_ym_ins_23_rx_z"
#   wal_reg = paste("wal_reg_",outcome, sep="")
#   wal_reg_cl = paste("wal_reg_cl_",outcome, sep="")
#   wal_reg_treated = paste("wal_reg_treated_",outcome, sep="")
#   wal_reg_treated_cl = paste("wal_reg_treated_cl_",outcome, sep="")
#   
#   if (grepl("total_pills_dispensed",outcome) & !(grepl("ring",outcome))) {
#     title = "Pills dispensed"
#   } else if (grepl("total_pats",outcome) & !(grepl("ring",outcome))) {
#     title = "Unique patients"
#   } else if (grepl("total_claims",outcome) & !(grepl("ring",outcome)) & !(grepl("inzip",outcome))){
#     title = "Total claims"
#   } else if (grepl("ring_mean_total_pills_dispensed",outcome)) {
#     title = "Ring Pills dispensed"
#   } else if (grepl("ring_mean_total_pats",outcome)) {
#     title = "Ring Unique patients"
#   } else if (grepl("ring_mean_total_claims",outcome)){
#     title = "Ring Total claims"
#   } else if (grepl("total_money",outcome)){
#     title = "Total money"
#   } else if (grepl("avg_age",outcome)) {
#     title = "Average age"
#   } else if (grepl("avg_copay",outcome)){
#     title = "Average copay"
#   } else if (grepl("total_new_pats",outcome)){
#     title = "New patients"
#   } else if (grepl("mean_mpr",outcome)){
#     title = "Med. Possesion Ratio"
#   } else if (grepl("share_medicaid",outcome)){
#     title = "Medicaid Share of Patients"
#   } else if (grepl("share_claims_inzip",outcome)) {
#     title = "Share of in-zip claims"
#   } else if (grepl("total_claims_inzip",outcome)) {
#     title = "Total in-zip claims"
#   } else if (grepl("total_unique_zips",outcome)) {
#     title = "Unique Pharmacy Zips"
#   } else if (grepl("log_avg_distance",outcome)) {
#     title = "Log Avg. Distance"
#   } else if (grepl("avg_distance",outcome)) {
#     title = "Avg. Distance"
#   }
#   
#   
#   
#   ###treated zips only
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  #ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#               as.factor(rx2):(ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#                               + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6) #+ ms_wal_7 + ms_wal_8 + ms_wal_9
#             + zip_factor #+ yrmon_factor 
#             + zip_factor:yrmon_number
#             + as.factor(rx2)
#             ,
#             data = zip_data_stacked[zip_ym_ins_23_rx_index == 1 & (insurance_type_23 == "PH")
#                                     & walgreens_zip_person == 1
#                                     & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                                     ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated_cl) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[zip_ym_ins_23_rx_index == 1 & (insurance_type_23 == "PH")
#                                     & walgreens_zip_person == 1
#                                     & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                                     ]$ZIP)))
#   coefs = names(get(wal_reg_treated_cl)[,1])
#   print(get(wal_reg_treated_cl)[grepl("ms",coefs),1])
#   print(nobs(get(wal_reg_treated)))
#   print(coefs[grepl("ms",coefs)])
#   
#   
#   wal_treated = data.table(rx = coefs[grepl("ms",coefs)],
#                            coef = get(wal_reg_treated_cl)[grepl("ms",coefs),1],
#                            se = get(wal_reg_treated_cl)[grepl("ms",coefs),2],
#                            p = get(wal_reg_treated_cl)[grepl("ms",coefs),4])
#   
#   wal_treated[, ms := ifelse(grepl("neg6", rx), -6,
#                              ifelse(grepl("neg5", rx), -5,
#                                     ifelse(grepl("neg4", rx), -4,
#                                            ifelse(grepl("neg3", rx), -3,
#                                                   ifelse(grepl("neg2", rx), -2,
#                                                          ifelse(grepl("_0", rx), 0,
#                                                                 ifelse(grepl("_1", rx), 1,
#                                                                        ifelse(grepl("_2", rx), 2,
#                                                                               ifelse(grepl("_3", rx), 3,
#                                                                                      ifelse(grepl("_4", rx), 4,
#                                                                                             ifelse(grepl("_5", rx), 5,
#                                                                                                    ifelse(grepl("_6", rx), 6,
#                                                                                                           NA))))))))))))]
#   wal_treated[,drug := as.character(as.numeric(substr(rx,15,16)))]
#   
#   wal_treated[, ymin := coef - 1.96 * se]
#   wal_treated[, ymax := coef + 1.96 * se]
#   
#   #merge on the rxclass label
#   rxclass_label = as.data.table(read.csv("~/current/pharmacy_deserts/data/rxclass_labels.csv"))
#   rxclass_label[, drug := as.character(rx2)]
#   #   rxclass_label[nchar(drug)==1, drug := paste("0",drug,sep="")]
#   setkey(rxclass_label, drug)
#   setkey(wal_treated, drug)
#   wal_treated[rxclass_label, rx_label := label]
#   
#   
#   
#   
#   
#   
#   for (RX in sort(unique(wal_treated$drug))) {
#     #         RX = 39
#     #add in 0 at negative 1
#     add0 = data.table(rx = RX,
#                       coef = 0,
#                       drug = RX,
#                       ms = -1)
#     wal_treated = rbindlist(list(wal_treated, add0), fill=T)
#     
#     ####plot###
#     plot_nc = ggplot(data = wal_treated[drug == RX,]) +
#       geom_point(aes(y=coef,x=ms),size = 8) +
#       geom_line(aes(y=coef,x=ms)) +
#       geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#       geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#       theme_bw() + ylab("Coefficient\n") + xlab("Months Since Break") +
#       scale_x_continuous(breaks = seq(-6,6,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title," \n(no controls)\n",wal_treated[drug == RX,]$rx_label,sep=""))
#     print(plot_nc)
#     
#     
#     
#     
#   }
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
# }
# 
# dev.off()
# 
# 
# ####how much did walgreens suffer?####
# pdf(file="~/current/pharmacy_deserts/walgreens_no_weights_pharmtype_split.pdf", width = 18, height = 10)
# for (outcome in c("log_total_pills_dispensed_ym_ins_23_pharmtype_z","log_total_pats_ym_ins_23_pharmtype_z","log_total_claims_ym_ins_23_pharmtype_z",
#                   "log_total_new_pats_ym_ins_23_pharmtype_z_plus_1","log_total_money_ym_ins_23_pharmtype_z_plus_1")) {
#   # outcome = "log_total_pills_dispensed_ym_ins_23_pharmtype_z"
#   wal_reg = paste("wal_reg_",outcome, sep="")
#   wal_reg_cl = paste("wal_reg_cl_",outcome, sep="")
#   wal_reg_treated = paste("wal_reg_treated_",outcome, sep="")
#   wal_reg_treated_cl = paste("wal_reg_treated_cl_",outcome, sep="")
#   
#   if (grepl("total_pills_dispensed",outcome) & !(grepl("ring",outcome))) {
#     title = "Pills dispensed"
#   } else if (grepl("total_pats",outcome) & !(grepl("ring",outcome))) {
#     title = "Unique patients"
#   } else if (grepl("total_claims",outcome) & !(grepl("ring",outcome)) & !(grepl("inzip",outcome))){
#     title = "Total claims"
#   } else if (grepl("ring_mean_total_pills_dispensed",outcome)) {
#     title = "Ring Pills dispensed"
#   } else if (grepl("ring_mean_total_pats",outcome)) {
#     title = "Ring Unique patients"
#   } else if (grepl("ring_mean_total_claims",outcome)){
#     title = "Ring Total claims"
#   } else if (grepl("total_money",outcome)){
#     title = "Total money"
#   } else if (grepl("avg_age",outcome)) {
#     title = "Average age"
#   } else if (grepl("avg_copay",outcome)){
#     title = "Average copay"
#   } else if (grepl("total_new_pats",outcome)){
#     title = "New patients"
#   } else if (grepl("mean_mpr",outcome)){
#     title = "Med. Possesion Ratio"
#   } else if (grepl("share_medicaid",outcome)){
#     title = "Medicaid Share of Patients"
#   } else if (grepl("share_claims_inzip",outcome)) {
#     title = "Share of in-zip claims"
#   } else if (grepl("total_claims_inzip",outcome)) {
#     title = "Total in-zip claims"
#   } else if (grepl("total_unique_zips",outcome)) {
#     title = "Unique Pharmacy Zips"
#   } else if (grepl("log_avg_distance",outcome)) {
#     title = "Log Avg. Distance"
#   } else if (grepl("avg_distance",outcome)) {
#     title = "Avg. Distance"
#   }
#   
#   
#   
#   ###treated zips only
#   assign(eval(wal_reg_treated), 
#          lm(get(outcome) ~  #ms_wal_neg9 + ms_wal_neg8 + ms_wal_neg7 + 
#               entity_broad:(ms_wal_neg6 + ms_wal_neg5 + ms_wal_neg4 + ms_wal_neg3 + ms_wal_neg2 
#                             + ms_wal_0 + ms_wal_1 + ms_wal_2 + ms_wal_3 + ms_wal_4 + ms_wal_5 + ms_wal_6) #+ ms_wal_7 + ms_wal_8 + ms_wal_9
#             + entity_broad
#             + zip_factor #+ yrmon_factor 
#             + zip_factor:yrmon_number
#             ,
#             data = zip_data_stacked[zip_ym_ins_23_pharmtype_index == 1 & (insurance_type_23 == "PH")
#                                     & walgreens_zip_person == 1
#                                     & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                                     ]))
#   
#   #cluster standard errors at the zip code level
#   assign(eval(wal_reg_treated_cl) , coeftest(get(wal_reg_treated), vcov=function(x) 
#     cluster.vcov(x,zip_data_stacked[zip_ym_ins_23_pharmtype_index == 1 & (insurance_type_23 == "PH")
#                                     & walgreens_zip_person == 1
#                                     & pick_up_yrmon < "Aug 2012" & pick_up_yrmon > "Jun 2011"
#                                     ]$ZIP)))
#   get(wal_reg_treated)
#   coefs = names(get(wal_reg_treated_cl)[,1])
#   print(get(wal_reg_treated_cl)[grepl("ms",coefs),1])
#   print(nobs(get(wal_reg_treated)))
#   print(coefs[grepl("ms",coefs)])
#   
#   
#   wal_treated = data.table(rx = coefs[grepl("ms",coefs)],
#                            coef = get(wal_reg_treated_cl)[grepl("ms",coefs),1],
#                            se = get(wal_reg_treated_cl)[grepl("ms",coefs),2],
#                            p = get(wal_reg_treated_cl)[grepl("ms",coefs),4])
#   
#   wal_treated[, ms := gsub(".*ms_wal_","",rx)]
#   wal_treated[, ms := as.numeric(gsub("neg","-",ms))]
#   wal_treated[,pharmtype := gsub(":ms.*","",gsub("entity_broad","",rx))]
#   
#   wal_treated[, ymin := coef - 1.96 * se]
#   wal_treated[, ymax := coef + 1.96 * se]
#   
#   
#   for (ENTITY in sort(unique(wal_treated$pharmtype))) {
#     #         ENTITY = "walgreens"
#     #         ENTITY = "walmart"
#     #add in 0 at negative 1
#     add0 = data.table(pharmtype = ENTITY,
#                       coef = 0,
#                       entity = ENTITY,
#                       ms = -1)
#     wal_treated = rbindlist(list(wal_treated, add0), fill=T)
#     
#     ####plot###
#     plot_nc = ggplot(data = wal_treated[pharmtype == ENTITY]) +
#       geom_point(aes(y=coef,x=ms),size = 8) +
#       geom_line(aes(y=coef,x=ms)) +
#       geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#       geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#       theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#       scale_x_continuous(breaks = seq(-6,6,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#       theme(text = element_text(size=55, family = "serif")) +
#       theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#       ggtitle(paste(title," \n(no controls)\n",ENTITY,sep=""))
#     print(plot_nc)
#     
#   }
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   
# }
# 
# dev.off()
# 
# #merge on the rxclass label
# rxclass_label = as.data.table(read.csv("~/current/pharmacy_deserts/data/rxclass_labels.csv"))
# rxclass_label[, drug := as.character(rx2)]
# rxclass_label[nchar(drug) == 1, drug := paste("0",drug)]
# setkey(rxclass_label, drug)
# setkey(rxnkawz, rx2)
# rxnkawz[rxclass_label, rx_label := label]
# 
# ####mail order events####
# B=6
# pdf(file = paste("~/current/pharmacy_deserts/mail_order_event_study_no_weights_",B,".pdf",sep=""), width = 18, height = 10)
# for (outcome in c("log_total_pills_dispensed_ym_ins_23_rx","log_total_pats_ym_ins_23_rx","log_total_claims_ym_ins_23_rx",
#                   "log_total_new_pats_ym_ins_23_rx_plus_1"
# )) {
#   
#   #   outcome = "log_total_pills_dispensed_ym_ins_23_rx"
#   reg_post_nc = paste("reg_post_nc_",outcome, sep="")
#   reg_ms_nc = paste("reg_ms_nc_",outcome, sep="")
#   reg_ms_nc_cl = paste("reg_ms_nc_cl_",outcome, sep="")
#   
#   
#   if (grepl("total_pills_dispensed",outcome) & !(grepl("ring",outcome))) {
#     title = "Pills dispensed"
#   } else if (grepl("total_pats",outcome) & !(grepl("ring",outcome))) {
#     title = "Unique patients"
#   } else if (grepl("total_claims",outcome) & !(grepl("ring",outcome)) & !(grepl("inzip",outcome))){
#     title = "Total claims"
#   } else if (grepl("ring_mean_total_pills_dispensed",outcome)) {
#     title = "Ring Pills dispensed"
#   } else if (grepl("ring_mean_total_pats",outcome)) {
#     title = "Ring Unique patients"
#   } else if (grepl("ring_mean_total_claims",outcome)){
#     title = "Ring Total claims"
#   } else if (grepl("total_money",outcome)){
#     title = "Total money"
#   } else if (grepl("avg_age",outcome)) {
#     title = "Average age"
#   } else if (grepl("avg_copay",outcome)){
#     title = "Average copay"
#   } else if (grepl("total_new_pats",outcome)){
#     title = "New patients"
#   } else if (grepl("mean_mpr",outcome)){
#     title = "Med. Possesion Ratio"
#   } else if (grepl("share_medicaid",outcome)){
#     title = "Medicaid Share of Patients"
#   } else if (grepl("share_claims_inzip",outcome)) {
#     title = "Share of in-zip claims"
#   } else if (grepl("total_claims_inzip",outcome)) {
#     title = "Total in-zip claims"
#   } else if (grepl("total_unique_zips",outcome)) {
#     title = "Unique Pharmacy Zips"
#   } else if (grepl("log_avg_distance",outcome)) {
#     title = "Log Avg. Distance"
#   } else if (grepl("avg_distance",outcome)) {
#     title = "Avg. Distance"
#   }
#   
#   
#   print(outcome)
#   
#   
#   ####Experiment 1: HMO July 2012###
#   #####Thyroid agents dropped, others stayed#####
#   rxnkawz[, thy_dummy := ifelse(rx_label == "THYROID AGENTS",1,0)]
#   rxnkawz[, ms_exp1 := round((pick_up_yrmon - as.yearmon("Jul 2012"))*12)]
#   
#   rxnkawz[, ms_exp1_neg9 := ifelse(ms_exp1 == -9, 1, 0)]
#   rxnkawz[, ms_exp1_neg8 := ifelse(ms_exp1 == -8, 1, 0)]
#   rxnkawz[, ms_exp1_neg7 := ifelse(ms_exp1 == -7, 1, 0)]
#   rxnkawz[, ms_exp1_neg6 := ifelse(ms_exp1 == -6, 1, 0)]
#   rxnkawz[, ms_exp1_neg5 := ifelse(ms_exp1 == -5, 1, 0)]
#   rxnkawz[, ms_exp1_neg4 := ifelse(ms_exp1 == -4, 1, 0)]
#   rxnkawz[, ms_exp1_neg3 := ifelse(ms_exp1 == -3, 1, 0)]
#   rxnkawz[, ms_exp1_neg2 := ifelse(ms_exp1 == -2, 1, 0)]
#   rxnkawz[, ms_exp1_neg1 := ifelse(ms_exp1 == -1, 1, 0)]
#   rxnkawz[, ms_exp1_0 := ifelse(ms_exp1 == 0, 1, 0)]
#   rxnkawz[, ms_exp1_1 := ifelse(ms_exp1 == 1, 1, 0)]
#   rxnkawz[, ms_exp1_2 := ifelse(ms_exp1 == 2, 1, 0)]
#   rxnkawz[, ms_exp1_3 := ifelse(ms_exp1 == 3, 1, 0)]
#   rxnkawz[, ms_exp1_4 := ifelse(ms_exp1 == 4, 1, 0)]
#   rxnkawz[, ms_exp1_5 := ifelse(ms_exp1 == 5, 1, 0)]
#   rxnkawz[, ms_exp1_6 := ifelse(ms_exp1 == 6, 1, 0)]
#   rxnkawz[, ms_exp1_7 := ifelse(ms_exp1 == 7, 1, 0)]
#   rxnkawz[, ms_exp1_8 := ifelse(ms_exp1 == 8, 1, 0)]
#   rxnkawz[, ms_exp1_9 := ifelse(ms_exp1 == 9, 1, 0)]
#   
#   rxnkawz[, post_exp1 := ifelse(ms_exp1 > 0 , 1, 0)]
#   
#   
#   ####Months Since####
#   assign(eval(reg_ms_nc) , lm(get(outcome) ~ 
#                                 thy_dummy:(ms_exp1_neg6 + ms_exp1_neg5 + ms_exp1_neg4 + ms_exp1_neg3 + ms_exp1_neg2 +
#                                              ms_exp1_0 + ms_exp1_1 + ms_exp1_2 +ms_exp1_3 + ms_exp1_4 + ms_exp1_5 + ms_exp1_6 )
#                               #                               + thy_dummy
#                               + as.factor(rx2)
#                               #                                                             + as.factor(rx2):yrmon_number 
#                               + yrmon_factor
#                               ,
#                               #                               #weights =weight,
#                               data = rxnkawz[pick_up_yrmon < "Feb 2013" & pick_up_yrmon > "Dec 2011"
#                                              #                                                         & rx_label %in% c("THYROID AGENTS","ANTIDIABETICS")
#                                              & rx_label %in% c("THYROID AGENTS",
#                                                                "BETA BLOCKERS"
#                                                                , "ANTIHYPERTENSIVES",
#                                                                "ANTIDIABETICS","ANTIHYPERLIPIDEMICS",
#                                                                "CALCIUM CHANNEL BLOCKERS"
#                                              ) 
#                                              & insurance_type_23 %in% c("HMO")
#                                              & ym_ins_23_rx_index == 1 ,]))
#   ##NOTE: zip_ms_index is created AFTER the stack so it pulls out zips with multiple openings in the same month
#   ##but zip_ym_index was created before the stack so it only removes obs with other ins types or rxclasses
#   summary(get(reg_ms_nc))
#   #   print(summary(get(reg_ms_nc))[1:(2*B+1),])
#   print(nobs(get(reg_ms_nc)))
#   
#   names_vector = names(coef( summary(get(reg_ms_nc)))[,1])[grepl("ms_",names(coef( summary(get(reg_ms_nc)))[,1]))]
#   coef_vector = coef( summary(get(reg_ms_nc)))[,1][names_vector]
#   se_vector = coef( summary(get(reg_ms_nc)))[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*exp1_","",names) ]
#   coef_data[, ms := gsub("neg","-",ms) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   coef_data[, ms := as.numeric(ms)]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   ####POST####
#   assign(eval(reg_post_nc) , lm(get(outcome) ~ 
#                                   thy_dummy:(post_exp1)
#                                 #+ thy_dummy
#                                 + as.factor(rx2)
#                                 #                               + as.factor(rx2):yrmon_number 
#                                 + yrmon_factor
#                                 ,
#                                 #                               #weights =weight,
#                                 data = rxnkawz[pick_up_yrmon < "Feb 2013" & pick_up_yrmon > "Dec 2011"
#                                                #                                                         & rx_label %in% c("THYROID AGENTS","ANTIDIABETICS")
#                                                & rx_label %in% c("BETA BLOCKERS", "ANTIHYPERTENSIVES",
#                                                                  "ANTIDIABETICS","ANTIHYPERLIPIDEMICS",
#                                                                  "THYROID AGENTS","CALCIUM CHANNEL BLOCKERS"
#                                                )
#                                                & insurance_type_23 %in% c("HMO")
#                                                & ym_ins_23_rx_index == 1 ,]))
#   ##NOTE: zip_ms_index is created AFTER the stack so it pulls out zips with multiple openings in the same month
#   ##but zip_ym_index was created before the stack so it only removes obs with other ins types or rxclasses
#   summary(get(reg_post_nc))
#   #   print(summary(get(reg_ms_nc))[1:(2*B+1),])
#   print(nobs(get(reg_post_nc)))
#   
#   
#   print(outcome)
#   stargazer(get(reg_post_nc),
#             no.space=T,
#             out = paste("~/current/pharmacy_deserts/post_regs_",outcome,"_mail_exp1_no_weight.tex",sep=""),
#             omit.stat = c("rsq", "f","adj.rsq","ser"),
#             dep.var.labels = c("Post"),
#             dep.var.caption="",
#             covariate.labels = c(outcome),
#             model.numbers=F,
#             multicolumn=T,
#             model.names=F,
#             header=F,
#             keep = c("post"),
#             align =T,
#             font.size = "footnotesize",
#             column.sep.width = "1pt"
#   )
#   
#   
#   
#   
#   ####EXPERIMENT 2: MPD Jan 2013#####
#   #####antidiabetics stayed , others jumped#####
#   rxnkawz[, diab_dummy := ifelse(rx_label == "ANTIDIABETICS",1,0)]
#   rxnkawz[, ms_exp2 := round((pick_up_yrmon - as.yearmon("Jan 2013"))*12)]
#   
#   rxnkawz[, ms_exp2_neg9 := ifelse(ms_exp2 == -9, 1, 0)]
#   rxnkawz[, ms_exp2_neg8 := ifelse(ms_exp2 == -8, 1, 0)]
#   rxnkawz[, ms_exp2_neg7 := ifelse(ms_exp2 == -7, 1, 0)]
#   rxnkawz[, ms_exp2_neg6 := ifelse(ms_exp2 == -6, 1, 0)]
#   rxnkawz[, ms_exp2_neg5 := ifelse(ms_exp2 == -5, 1, 0)]
#   rxnkawz[, ms_exp2_neg4 := ifelse(ms_exp2 == -4, 1, 0)]
#   rxnkawz[, ms_exp2_neg3 := ifelse(ms_exp2 == -3, 1, 0)]
#   rxnkawz[, ms_exp2_neg2 := ifelse(ms_exp2 == -2, 1, 0)]
#   rxnkawz[, ms_exp2_neg1 := ifelse(ms_exp2 == -1, 1, 0)]
#   rxnkawz[, ms_exp2_0 := ifelse(ms_exp2 == 0, 1, 0)]
#   rxnkawz[, ms_exp2_1 := ifelse(ms_exp2 == 1, 1, 0)]
#   rxnkawz[, ms_exp2_2 := ifelse(ms_exp2 == 2, 1, 0)]
#   rxnkawz[, ms_exp2_3 := ifelse(ms_exp2 == 3, 1, 0)]
#   rxnkawz[, ms_exp2_4 := ifelse(ms_exp2 == 4, 1, 0)]
#   rxnkawz[, ms_exp2_5 := ifelse(ms_exp2 == 5, 1, 0)]
#   rxnkawz[, ms_exp2_6 := ifelse(ms_exp2 == 6, 1, 0)]
#   rxnkawz[, ms_exp2_7 := ifelse(ms_exp2 == 7, 1, 0)]
#   rxnkawz[, ms_exp2_8 := ifelse(ms_exp2 == 8, 1, 0)]
#   rxnkawz[, ms_exp2_9 := ifelse(ms_exp2 == 9, 1, 0)]
#   
#   rxnkawz[, post_exp2 := ifelse(ms_exp2 > 0 , 1, 0)]
#   
#   
#   ####Months Since####
#   assign(eval(reg_ms_nc) , lm(get(outcome) ~ 
#                                 diab_dummy:(ms_exp2_neg6 + ms_exp2_neg5 + ms_exp2_neg4 + ms_exp2_neg3 + ms_exp2_neg2 +
#                                               ms_exp2_0 + ms_exp2_1 + ms_exp2_2 +ms_exp2_3 + ms_exp2_4 + ms_exp2_5 + ms_exp2_6 )
#                               #                               + thy_dummy
#                               + as.factor(rx2)
#                               #                                                             + as.factor(rx2):yrmon_number 
#                               + yrmon_factor
#                               ,
#                               #                               #weights =weight,
#                               data = rxnkawz[ms_exp2 >= -6 & ms_exp2 <= 6
#                                              #                                                         & rx_label %in% c("THYROID AGENTS","ANTIDIABETICS")
#                                              & rx_label %in% c("THYROID AGENTS",
#                                                                "BETA BLOCKERS"
#                                                                , "ANTIHYPERTENSIVES",
#                                                                "ANTIDIABETICS","ANTIHYPERLIPIDEMICS",
#                                                                "CALCIUM CHANNEL BLOCKERS"
#                                              ) 
#                                              & insurance_type_23 %in% c("MPD")
#                                              & ym_ins_23_rx_index == 1 ,]))
#   ##NOTE: zip_ms_index is created AFTER the stack so it pulls out zips with multiple openings in the same month
#   ##but zip_ym_index was created before the stack so it only removes obs with other ins types or rxclasses
#   summary(get(reg_ms_nc))
#   #   print(summary(get(reg_ms_nc))[1:(2*B+1),])
#   print(nobs(get(reg_ms_nc)))
#   
#   names_vector = names(coef( summary(get(reg_ms_nc)))[,1])[grepl("ms_",names(coef( summary(get(reg_ms_nc)))[,1]))]
#   coef_vector = coef( summary(get(reg_ms_nc)))[,1][names_vector]
#   se_vector = coef( summary(get(reg_ms_nc)))[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*exp2_","",names) ]
#   coef_data[, ms := gsub("neg","-",ms) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   coef_data[, ms := as.numeric(ms)]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)",sep="")) 
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   ####POST####
#   assign(eval(reg_post_nc) , lm(get(outcome) ~ 
#                                   diab_dummy:(post_exp2)
#                                 #+ thy_dummy
#                                 + as.factor(rx2)
#                                 #                               + as.factor(rx2):yrmon_number 
#                                 + yrmon_factor
#                                 ,
#                                 #                               #weights =weight,
#                                 data = rxnkawz[ms_exp2 <= 6 & ms_exp2 >= -6
#                                                #                                                         & rx_label %in% c("THYROID AGENTS","ANTIDIABETICS")
#                                                & rx_label %in% c("BETA BLOCKERS", "ANTIHYPERTENSIVES",
#                                                                  "ANTIDIABETICS","ANTIHYPERLIPIDEMICS",
#                                                                  "THYROID AGENTS","CALCIUM CHANNEL BLOCKERS"
#                                                )
#                                                & insurance_type_23 %in% c("MPD")
#                                                & ym_ins_23_rx_index == 1 ,]))
#   ##NOTE: zip_ms_index is created AFTER the stack so it pulls out zips with multiple openings in the same month
#   ##but zip_ym_index was created before the stack so it only removes obs with other ins types or rxclasses
#   summary(get(reg_post_nc))
#   #   print(summary(get(reg_ms_nc))[1:(2*B+1),])
#   print(nobs(get(reg_post_nc)))
#   
#   
#   print(outcome)
#   stargazer(get(reg_post_nc),
#             no.space=T,
#             out = paste("~/current/pharmacy_deserts/post_regs_",outcome,"_mail_exp2_no_weight.tex",sep=""),
#             omit.stat = c("rsq", "f","adj.rsq","ser"),
#             dep.var.labels = c("Post"),
#             dep.var.caption="",
#             covariate.labels = c(outcome),
#             model.numbers=F,
#             multicolumn=T,
#             model.names=F,
#             header=F,
#             keep = c("post"),
#             align =T,
#             font.size = "footnotesize",
#             column.sep.width = "1pt"
#   )
#   
#   
#   
#   ####EXPERIMENT 3: hyperlipidemics Jan 2013#####
#   #####MD stayed , PH jumped#####
#   rxnkawz[, ph_dummy := ifelse(insurance_type_23 %in% c("PH","MPD","MC"),1,0)]
#   rxnkawz[, ms_exp3 := round((pick_up_yrmon - as.yearmon("Jan 2013"))*12)]
#   
#   rxnkawz[, ms_exp3_neg9 := ifelse(ms_exp3 == -9, 1, 0)]
#   rxnkawz[, ms_exp3_neg8 := ifelse(ms_exp3 == -8, 1, 0)]
#   rxnkawz[, ms_exp3_neg7 := ifelse(ms_exp3 == -7, 1, 0)]
#   rxnkawz[, ms_exp3_neg6 := ifelse(ms_exp3 == -6, 1, 0)]
#   rxnkawz[, ms_exp3_neg5 := ifelse(ms_exp3 == -5, 1, 0)]
#   rxnkawz[, ms_exp3_neg4 := ifelse(ms_exp3 == -4, 1, 0)]
#   rxnkawz[, ms_exp3_neg3 := ifelse(ms_exp3 == -3, 1, 0)]
#   rxnkawz[, ms_exp3_neg2 := ifelse(ms_exp3 == -2, 1, 0)]
#   rxnkawz[, ms_exp3_neg1 := ifelse(ms_exp3 == -1, 1, 0)]
#   rxnkawz[, ms_exp3_0 := ifelse(ms_exp3 == 0, 1, 0)]
#   rxnkawz[, ms_exp3_1 := ifelse(ms_exp3 == 1, 1, 0)]
#   rxnkawz[, ms_exp3_2 := ifelse(ms_exp3 == 2, 1, 0)]
#   rxnkawz[, ms_exp3_3 := ifelse(ms_exp3 == 3, 1, 0)]
#   rxnkawz[, ms_exp3_4 := ifelse(ms_exp3 == 4, 1, 0)]
#   rxnkawz[, ms_exp3_5 := ifelse(ms_exp3 == 5, 1, 0)]
#   rxnkawz[, ms_exp3_6 := ifelse(ms_exp3 == 6, 1, 0)]
#   rxnkawz[, ms_exp3_7 := ifelse(ms_exp3 == 7, 1, 0)]
#   rxnkawz[, ms_exp3_8 := ifelse(ms_exp3 == 8, 1, 0)]
#   rxnkawz[, ms_exp3_9 := ifelse(ms_exp3 == 9, 1, 0)]
#   
#   rxnkawz[, post_exp3 := ifelse(ms_exp3 > 0 , 1, 0)]
#   
#   
#   ####Months Since####
#   assign(eval(reg_ms_nc) , lm(get(outcome) ~ 
#                                 ph_dummy:(ms_exp3_neg6 + ms_exp3_neg5 + ms_exp3_neg4 + ms_exp3_neg3 + ms_exp3_neg2 +
#                                             ms_exp3_0 + ms_exp3_1 + ms_exp3_2 +ms_exp3_3 + ms_exp3_4 + ms_exp3_5 + ms_exp3_6 )
#                               #                               + thy_dummy
#                               + insurance_type_23
#                               #                                                             + as.factor(rx2):yrmon_number 
#                               + yrmon_factor
#                               ,
#                               #                               #weights =weight,
#                               data = rxnkawz[ms_exp3 >= -6 & ms_exp3 <= 6
#                                              #                                                         & rx_label %in% c("THYROID AGENTS","ANTIDIABETICS")
#                                              & rx_label %in% c("ANTIHYPERLIPIDEMICS") 
#                                              & insurance_type_23 %in% c("MPD","MC",
#                                                                         "MD","MLI"
#                                              )
#                                              & ym_ins_23_rx_index == 1 ,]))
#   ##NOTE: zip_ms_index is created AFTER the stack so it pulls out zips with multiple openings in the same month
#   ##but zip_ym_index was created before the stack so it only removes obs with other ins types or rxclasses
#   summary(get(reg_ms_nc))
#   #   print(summary(get(reg_ms_nc))[1:(2*B+1),])
#   print(nobs(get(reg_ms_nc)))
#   
#   names_vector = names(coef( summary(get(reg_ms_nc)))[,1])[grepl("ms_",names(coef( summary(get(reg_ms_nc)))[,1]))]
#   coef_vector = coef( summary(get(reg_ms_nc)))[,1][names_vector]
#   se_vector = coef( summary(get(reg_ms_nc)))[,2][names_vector]
#   
#   ymin = coef_vector - 1.96 * se_vector
#   ymax = coef_vector + 1.96 * se_vector
#   
#   coef_data = data.table(coef = coef_vector,
#                          se = se_vector,
#                          ymin = ymin,
#                          ymax = ymax,
#                          names = names_vector)
#   coef_data[, ms := gsub(".*exp3_","",names) ]
#   coef_data[, ms := gsub("neg","-",ms) ]
#   addneg1 = data.table(coef = 0, ms = -1)
#   coef_data = rbindlist(list(coef_data, addneg1), fill=T)
#   coef_data[, ms := as.numeric(ms)]
#   
#   ####plot###
#   plot_nc = ggplot(data = coef_data) +
#     geom_point(aes(y=coef,x=ms),size = 8) +
#     geom_line(aes(y=coef,x=ms)) +
#     geom_vline(xintercept=0,color="Red") + geom_hline(yintercept=0) +
#     geom_errorbar(aes(ymin = ymin, ymax = ymax, x=ms),width = .1) +
#     theme_bw() + ylab("Coefficient\n") + xlab("Months Since Opening") +
#     scale_x_continuous(breaks = seq(-B,B,2)) + #scale_y_continuous(breaks = seq(-.02,.06,.02)) +
#     theme(text = element_text(size=55, family = "serif")) +
#     theme(axis.text.x = element_text(size = 60)) + theme(axis.text.y = element_text(size = 60)) +
#     ggtitle(paste(title," \n(no controls)",sep="")) 
#   # +
#   #   coord_cartesian(ylim = c(max(coef_data$coef)+.1, min(coef_data$coef)-.1))
#   
#   #                 
#   print(plot_nc)
#   rm(ymin, ymax, coef_vector, se_vector)
#   
#   
#   ####POST####
#   assign(eval(reg_post_nc) , lm(get(outcome) ~ 
#                                   ph_dummy:post_exp3
#                                 #                               + thy_dummy
#                                 + insurance_type_23
#                                 #                                                             + as.factor(rx2):yrmon_number 
#                                 + yrmon_factor
#                                 ,
#                                 #                               #weights =weight,
#                                 data = rxnkawz[ms_exp3 >= -6 & ms_exp3 <= 6
#                                                #                                                         & rx_label %in% c("THYROID AGENTS","ANTIDIABETICS")
#                                                & rx_label %in% c("ANTIHYPERLIPIDEMICS") 
#                                                & insurance_type_23 %in% c("MPD","MC",
#                                                                           "MD","MLI"
#                                                )
#                                                & ym_ins_23_rx_index == 1 ,]))
#   ##NOTE: zip_ms_index is created AFTER the stack so it pulls out zips with multiple openings in the same month
#   ##but zip_ym_index was created before the stack so it only removes obs with other ins types or rxclasses
#   summary(get(reg_post_nc))
#   #   print(summary(get(reg_ms_nc))[1:(2*B+1),])
#   print(nobs(get(reg_post_nc)))
#   
#   
#   print(outcome)
#   stargazer(get(reg_post_nc),
#             no.space=T,
#             out = paste("~/current/pharmacy_deserts/post_regs_",outcome,"_mail_exp3_no_weight.tex",sep=""),
#             omit.stat = c("rsq", "f","adj.rsq","ser"),
#             dep.var.labels = c("Post"),
#             dep.var.caption="",
#             covariate.labels = c(outcome),
#             model.numbers=F,
#             multicolumn=T,
#             model.names=F,
#             header=F,
#             keep = c("post"),
#             align =T,
#             font.size = "footnotesize",
#             column.sep.width = "1pt"
#   )
#   
# }
# dev.off()
# 
# 
# ###################################################################################################################################################################################
# ####Section 8: summary stats ####
# ###################################################################################################################################################################################
# 
# #see what happens to memory when we remove all unneeded variables
# names_to_keep = c("apac_payer","personkey","gender","race","ethn","STATE","ZIP","copay","paid","coins","oop","rx2","rx4",
#                   "qtydisp","rxdays","npi","pick_up_yrmon","pharm_zip","pharm_state","pharm_add_full","entity","entity_broad",
#                   "mail_order_dummy","age","new_pharm","opening_year_mon","last_year_mon","total_pats_ym_z","distance",
#                   "opener_zip_dummy", "ms_zip_1", "ms_zip_2", "ms_zip_3", "ms_zip_4", "tot_pharms_ym_z",
#                   "walgreens_zip_person","ms_wal","insurance_type_23","insurance_type_53","insurance_type_8")
# 
# names_to_drop = names(rxnkaw)[!names(rxnkaw) %in% names_to_keep]
# rxnkaw[,(names_to_drop) := NULL] #down to 15gb!!
# gc()
# 
# 
# ####data sets for drug type story####
# rxnkaw[, avg_age_rx2 := mean(age, na.rm=T), by = c("rx2")]
# rxnkaw[, avg_copay_rx2 := mean(copay, na.rm=T), by = c("rx2")]
# rxnkaw[, total_fills_rx2 := .N, by = c("rx2")]
# drug_type_story = as.data.table(unique(rxnkaw[,.(avg_age_rx2, avg_copay_rx2, total_fills_rx2, rx2)]))
# write.csv(drug_type_story, file = "~/current/pharmacy_deserts/data/drug_type_story.csv")
# 
# ####data sets for insurance type story####
# rxnkaw[, avg_age_ins8 := mean(age, na.rm=T), by = c("insurance_type_8")]
# rxnkaw[, avg_copay_ins8 := mean(copay, na.rm=T), by = c("insurance_type_8")]
# rxnkaw[, total_fills_ins8 := .N, by = c("insurance_type_8")]
# ins8_type_story = as.data.table(unique(rxnkaw[,.(avg_age_ins8, avg_copay_ins8, total_fills_ins8, insurance_type_8)]))
# write.csv(ins8_type_story, file = "~/current/pharmacy_deserts/data/ins8_type_story.csv")
# 
# rxnkaw[, avg_age_ins23 := mean(age, na.rm=T), by = c("insurance_type_23")]
# rxnkaw[, avg_copay_ins23 := mean(copay, na.rm=T), by = c("insurance_type_23")]
# rxnkaw[, total_fills_ins23 := .N, by = c("insurance_type_23")]
# ins23_type_story = as.data.table(unique(rxnkaw[,.(avg_age_ins23, avg_copay_ins23, total_fills_ins23, insurance_type_23)]))
# write.csv(ins23_type_story, file = "~/current/pharmacy_deserts/data/ins23_type_story.csv")
# 
# rxnkaw[, avg_age_ins53 := mean(age, na.rm=T), by = c("insurance_type_53")]
# rxnkaw[, avg_copay_ins53 := mean(copay, na.rm=T), by = c("insurance_type_53")]
# rxnkaw[, total_fills_ins53 := .N, by = c("insurance_type_53")]
# ins53_type_story = as.data.table(unique(rxnkaw[,.(avg_age_ins53, avg_copay_ins53, total_fills_ins53, insurance_type_53)]))
# write.csv(ins53_type_story, file = "~/current/pharmacy_deserts/data/ins53_type_story.csv")
# 
# ####mail order propensity in 2011####
# rxnkaw[year(pick_up_yrmon) == 2011, mail_share_age_ins23_z_rx2_copay := mean(mail_order_dummy, na.rm=T), by = c("age","insurance_type_23",
#                                                                                                                 "ZIP","rx2","copay")]
# mail_order_propensity = unique(rxnkaw[year(pick_up_yrmon) == 2011,.(mail_share_age_ins23_z_rx2_copay, age, insurance_type_23, ZIP, rx2, copay)])
# write.csv(mail_order_propensity, file = "~/current/pharmacy_deserts/mail_order_propensity.csv")
# 
# ####"share" variables####
# #share of claims at each pharmacy type
# rxnkaw[, total_by_pharm_type := .N, by = c("entity_broad")]
# rxnkaw[, share_by_pharm_type := total_by_pharm_type / .N]
# share_pharmtype_table = unique(rxnkaw[,.(entity_broad, share_by_pharm_type)])
# write.csv(share_pharmtype_table, file = "~/current/pharmacy_deserts/share_claims_by_pharmtype.csv")
# rm(share_pharmtype_table)
# rxnkaw[, c("total_by_pharm_type","share_by_pharm_type") := NULL]
# gc()
# 
# #share of claims at each pharmtype by insurance type
# rxnkaw[, total_ins_by_pharm_type := .N, by = c("entity_broad","insurance_type_23")]
# rxnkaw[, share_ins_by_pharm_type := total_ins_by_pharm_type / .N]
# share_ins_pharmtype_table = unique(rxnkaw[,.(entity_broad, insurance_type_23, share_ins_by_pharm_type)])
# write.csv(share_ins_pharmtype_table, file = "~/current/pharmacy_deserts/share_ins_claims_by_pharmtype.csv")
# rm(share_ins_pharmtype_table)
# rxnkaw[, c("total_ins_by_pharm_type","share_ins_by_pharm_type") := NULL]
# gc()
# 
# #share of claims on each insurance type
# rxnkaw[, total_ins_by_type := .N, by = apac_payer]
# rxnkaw[, share_ins_by_type := total_ins_by_type / .N]
# share_insurance_table = unique(rxnkaw[,.(apac_payer, share_ins_by_type)])
# write.csv(share_insurance_table, file = "~/current/pharmacy_deserts/share_claims_insurance_type.csv")
# rm(share_insurance_table)
# rxnkaw[, c("total_ins_by_type","share_ins_by_type") := NULL]
# gc()
# 
# #share of claims on each insurance type 23
# rxnkaw[, total_ins_by_type := .N, by = insurance_type_23]
# rxnkaw[, share_ins_by_type := total_ins_by_type / .N]
# share_insurance_table = unique(rxnkaw[,.(insurance_type_23, share_ins_by_type)])
# write.csv(share_insurance_table, file = "~/current/pharmacy_deserts/share_claims_insurance_type_23.csv")
# rm(share_insurance_table)
# rxnkaw[, c("total_ins_by_type","share_ins_by_type") := NULL]
# gc()
# 
# #share of claims by each race
# rxnkaw[, race_ethn := paste(race,ethn,sep="+")]
# rxnkaw[, total_race := .N, by = race_ethn]
# rxnkaw[, share_race := total_race/.N]
# share_race_table = unique(rxnkaw[,.(race_ethn, share_race)])
# write.csv(share_race_table, file = "~/current/pharmacy_deserts/share_claims_race_table.csv")
# rm(share_race_table)
# rxnkaw[, c("total_race","share_race") := NULL]
# gc()
# 
# #share of claims from each state
# rxnkaw[, total_state := .N, by = STATE]
# rxnkaw[, share_state := total_state/.N]
# share_state_table = unique(rxnkaw[,.(STATE, share_state)])
# write.csv(share_state_table, file = "~/current/pharmacy_deserts/share_claims_state_table.csv")
# rm(share_state_table)
# rxnkaw[, c("total_state","share_state") := NULL]
# gc()
# 
# #share of claims at a mail order pharmacy
# rxnkaw[, total_mail := .N, by = mail_order_dummy]
# rxnkaw[, share_mail := total_mail/.N]
# share_mail_table = unique(rxnkaw[,.(mail_order_dummy, share_mail)])
# write.csv(share_mail_table, file = "~/current/pharmacy_deserts/share_claims_mail_table.csv")
# rm(share_mail_table)
# rxnkaw[, c("total_mail","share_mail") := NULL]
# gc()
# 
# #share of claims at pharmacies in each state
# rxnkaw[, total_pharm_state := .N, by = pharm_state]
# rxnkaw[, share_pharm_state := total_pharm_state/.N]
# share_pharm_state_table = unique(rxnkaw[,.(pharm_state, share_pharm_state)])
# write.csv(share_pharm_state_table, file = "~/current/pharmacy_deserts/share_claims_pharm_state_table.csv")
# rm(share_pharm_state_table)
# rxnkaw[, c("total_pharm_state","share_pharm_state") := NULL]
# gc()
# 
# 
# 
# 
# ####"average" variables over all claims####
# #average age
# rxnkaw[, avg_age := mean(age, na.rm=T)]
# rxnkaw[, sd_age := sd(age, na.rm=T)]
# 
# #avg_copay
# rxnkaw[, avg_copay := mean(copay, na.rm=T)]
# rxnkaw[, sd_copay := sd(copay, na.rm=T)]
# 
# #coins
# rxnkaw[, avg_coins := mean(coins, na.rm=T)]
# rxnkaw[, sd_coins := sd(coins, na.rm=T)]
# 
# #oop
# rxnkaw[, avg_oop := mean(oop, na.rm=T)]
# rxnkaw[, sd_oop := sd(oop, na.rm=T)]
# 
# #qtydisp
# rxnkaw[, avg_qtydisp := mean(qtydisp, na.rm=T)]
# rxnkaw[, sd_qtydisp := sd(qtydisp, na.rm=T)]
# 
# #rxdays
# rxnkaw[, avg_rxdays := mean(rxdays, na.rm=T)]
# rxnkaw[, sd_rxdays := sd(rxdays, na.rm=T)]
# 
# #distance
# rxnkaw[STATE == "OR" & pharm_state == "OR", avg_distance := mean(distance, na.rm=T)]
# rxnkaw[STATE == "OR" & pharm_state == "OR", sd_distance := sd(distance, na.rm=T)]
# 
# all_means_table = unique(rxnkaw[,.(avg_age,sd_age,
#                                    avg_copay,sd_copay,
#                                    avg_coins, sd_coins,
#                                    avg_oop, sd_oop,
#                                    avg_qtydisp, sd_qtydisp,
#                                    avg_rxdays,sd_rxdays,
#                                    avg_distance,sd_distance)])
# amt = melt(all_means_table)
# write.csv(amt, file = "~/current/pharmacy_deserts/all_means_table.csv")
# rm(all_means_table, amt)
# rxnkaw[,c("avg_age","sd_age",
#           "avg_copay","sd_copay",
#           "avg_coins", "sd_coins",
#           "avg_oop", "sd_oop",
#           "avg_qtydisp", "sd_qtydisp",
#           "avg_rxdays","sd_rxdays",
#           "avg_distance","sd_distance") := NULL]
# gc()
# 
# 
# #distance by insurance type
# rxnkaw[STATE == "OR" & pharm_state == "OR", avg_dist_ins_type := mean(distance, na.rm=T), by = apac_payer]
# rxnkaw[STATE == "OR" & pharm_state == "OR", sd_dist_ins_type := sd(distance, na.rm=T), by = apac_payer]
# 
# ins_distance_table = unique(rxnkaw[,.(apac_payer, avg_dist_ins_type, sd_dist_ins_type)])
# write.csv(ins_distance_table, file = "~/current/pharmacy_deserts/ins_distance_table.csv")
# rm(ins_distance_table)
# rxnkaw[, c("avg_dist_ins_type","sd_dist_ins_type") := NULL]
# gc()
# 
# #distance by insurance type by opening zip
# rxnkaw[STATE == "OR" & pharm_state == "OR", avg_dist_ins_type_oz := mean(distance, na.rm=T), by = c("apac_payer","opener_zip_dummy")]
# rxnkaw[STATE == "OR" & pharm_state == "OR", sd_dist_ins_type_oz := sd(distance, na.rm=T), by = c("apac_payer","opener_zip_dummy")]
# 
# ins_distance_table = unique(rxnkaw[,.(apac_payer, opener_zip_dummy, avg_dist_ins_type_oz, sd_dist_ins_type_oz)])
# write.csv(ins_distance_table, file = "~/current/pharmacy_deserts/ins_distance_table_opener.csv")
# rm(ins_distance_table)
# rxnkaw[, c("avg_dist_ins_type_oz","sd_dist_ins_type_oz") := NULL]
# gc()
# 
# #distance by opening zip
# rxnkaw[STATE == "OR" & pharm_state == "OR", avg_dist_ins_type_oz := mean(distance, na.rm=T), by = c("opener_zip_dummy")]
# rxnkaw[STATE == "OR" & pharm_state == "OR", sd_dist_ins_type_oz := sd(distance, na.rm=T), by = c("opener_zip_dummy")]
# 
# ins_distance_table = unique(rxnkaw[,.(opener_zip_dummy, avg_dist_ins_type_oz, sd_dist_ins_type_oz)])
# write.csv(ins_distance_table, file = "~/current/pharmacy_deserts/ins_distance_table_opener_only.csv")
# rm(ins_distance_table)
# rxnkaw[, c("avg_dist_ins_type_oz","sd_dist_ins_type_oz") := NULL]
# gc()
# 
# 
# 
# 
# ####drug type histogram and drug specific shares and averages####
# #one obs per drug
# rxnkaw[, rx2_index := seq_len(.N), by = rx2]
# 
# #merge on the drug labels
# rxclass_label = as.data.table(read.csv("~/current/pharmacy_deserts/data/rxclass_labels.csv"))
# rxclass_label[, drug := as.character(rx2)]
# setkey(rxclass_label, drug)
# setkey(rxnkaw, rx2)
# rxnkaw[rxclass_label, rx_label := label]
# gc()
# 
# #counts of each drug type
# rxnkaw[, rx2_count := .N, by = rx2]
# 
# #avgs by drug
# rxnkaw[, avg_qtydisp_rx2 := mean(qtydisp, na.rm=T), by = rx2]
# rxnkaw[, avg_rxdays_rx2 := mean(rxdays, na.rm=T), by = rx2]
# rxnkaw[, avg_copay_rx2 := mean(copay, na.rm=T), by = rx2]
# rxnkaw[, share_mail_order_rx2 := mean(mail_order_dummy, na.rm=T), by = rx2]
# 
# 
# #the raw histogram
# rx2_hist_data = rxnkaw[rx2_index == 1,]
# 
# pdf(file = "~/current/pharmacy_deserts/drug_type_histogram.pdf")
# rx2_hist = ggplot(data = rx2_hist_data[!is.na(rx_label) & rx2_count > 0,]) + 
#   geom_bar(aes(x=reorder(rx_label, rx2_count), y=rx2_count/1000000), stat = "identity") +
#   theme_bw() + 
#   theme(axis.text.x = element_text(angle=90,
#                                    size = 7)) +
#   theme(axis.text.y = element_text(angle=90,size=15)) +
#   theme(axis.title.y = element_text(size=20)) +
#   xlab("") + ylab("Claims\n(millions)")
# print(rx2_hist)
# 
# #subset to the rx2s with over 2 million prescriptions and show the rx4 counts?
# #TODO
# 
# #claim order, but height is now avg qtydisp
# rx2_hist_qtydisp_claim_order = ggplot(data = rx2_hist_data[!is.na(rx_label) & rx2_count > 0,]) + 
#   geom_bar(aes(x=reorder(rx_label, rx2_count), y=avg_qtydisp_rx2), stat = "identity", fill = "Red") +
#   theme_bw() + 
#   theme(axis.text.x = element_text(angle=90,
#                                    size = 7)) +
#   theme(axis.text.y = element_text(angle=90,size=15)) +
#   theme(axis.title.y = element_text(size=20)) +
#   xlab("") + ylab("Mean Quantity Dispensed")
# print(rx2_hist_qtydisp_claim_order)
# 
# #qtydisp order
# rx2_hist_qtydisp_qtydisp_order = ggplot(data = rx2_hist_data[!is.na(rx_label) & rx2_count > 0,]) + 
#   geom_bar(aes(x=reorder(rx_label, avg_qtydisp_rx2), y=avg_qtydisp_rx2), stat = "identity", fill="Red") +
#   theme_bw() + 
#   theme(axis.text.x = element_text(angle=90,
#                                    size = 7)) +
#   theme(axis.text.y = element_text(angle=90,size=15)) +
#   theme(axis.title.y = element_text(size=20)) +
#   xlab("") + ylab("Mean Quantity Dispensed")
# print(rx2_hist_qtydisp_qtydisp_order)
# 
# #claim order, but height is now avg rxdays
# rx2_hist_rxdays_claim_order = ggplot(data = rx2_hist_data[!is.na(rx_label) & rx2_count > 0,]) + 
#   geom_bar(aes(x=reorder(rx_label, rx2_count), y=avg_rxdays_rx2), stat = "identity", fill = "Blue") +
#   theme_bw() + 
#   theme(axis.text.x = element_text(angle=90,
#                                    size = 7)) +
#   theme(axis.text.y = element_text(angle=90,size=15)) +
#   theme(axis.title.y = element_text(size=20)) +
#   xlab("") + ylab("Mean Days Covered")
# print(rx2_hist_rxdays_claim_order)
# 
# #rxdays order
# rx2_hist_rxdays_rxdays_order = ggplot(data = rx2_hist_data[!is.na(rx_label) & rx2_count > 0,]) + 
#   geom_bar(aes(x=reorder(rx_label, avg_rxdays_rx2), y=avg_rxdays_rx2), stat = "identity", fill="Blue") +
#   theme_bw() + 
#   theme(axis.text.x = element_text(angle=90,
#                                    size = 7)) +
#   theme(axis.text.y = element_text(angle=90,size=15)) +
#   theme(axis.title.y = element_text(size=20)) +
#   xlab("") + ylab("Mean Days Covered")
# print(rx2_hist_rxdays_rxdays_order)
# 
# #claim order, but height is now avg copay
# rx2_hist_copay_claim_order = ggplot(data = rx2_hist_data[!is.na(rx_label) & rx2_count > 0,]) + 
#   geom_bar(aes(x=reorder(rx_label, rx2_count), y=avg_copay_rx2), stat = "identity", fill = "Dark Green") +
#   theme_bw() + 
#   theme(axis.text.x = element_text(angle=90,
#                                    size = 7)) +
#   theme(axis.text.y = element_text(angle=90,size=15)) +
#   theme(axis.title.y = element_text(size=20)) +
#   xlab("") + ylab("Mean Copay")
# print(rx2_hist_copay_claim_order)
# 
# #copay order
# rx2_hist_copay_copay_order = ggplot(data = rx2_hist_data[!is.na(rx_label) & rx2_count > 0,]) + 
#   geom_bar(aes(x=reorder(rx_label, avg_copay_rx2), y=avg_copay_rx2), stat = "identity", fill="Dark Green") +
#   theme_bw() + 
#   theme(axis.text.x = element_text(angle=90,
#                                    size = 7)) +
#   theme(axis.text.y = element_text(angle=90,size=15)) +
#   theme(axis.title.y = element_text(size=20)) +
#   xlab("") + ylab("Mean Copay")
# print(rx2_hist_copay_copay_order)
# 
# 
# #claim order, but height is now avg mail
# rx2_hist_mail_claim_order = ggplot(data = rx2_hist_data[!is.na(rx_label) & rx2_count > 0,]) + 
#   geom_bar(aes(x=reorder(rx_label, rx2_count), y=share_mail_order_rx2), stat = "identity", fill = "Dark Orange") +
#   theme_bw() + 
#   theme(axis.text.x = element_text(angle=90,
#                                    size = 7)) +
#   theme(axis.text.y = element_text(angle=90,size=15)) +
#   theme(axis.title.y = element_text(size=20)) +
#   xlab("") + ylab("Share Mail Order")
# print(rx2_hist_mail_claim_order)
# 
# #mail order
# rx2_hist_mail_mail_order = ggplot(data = rx2_hist_data[!is.na(rx_label) & rx2_count > 0,]) + 
#   geom_bar(aes(x=reorder(rx_label, share_mail_order_rx2), y=share_mail_order_rx2), stat = "identity", fill="Dark Orange") +
#   theme_bw() + 
#   theme(axis.text.x = element_text(angle=90,
#                                    size = 7)) +
#   theme(axis.text.y = element_text(angle=90,size=15)) +
#   theme(axis.title.y = element_text(size=20)) +
#   xlab("") + ylab("Share Mail Order")
# print(rx2_hist_mail_mail_order)
# dev.off()
# 
# 
# ####number of events####
# 
# 
# nrow(unique(rxnkaw[last_year_mon < "Dec 2013" & pharm_state == "OR" #closing pharm
#                    & opening_year_mon <= last_year_mon - .5  # not just fluke pharms - careful - this counts as flukes those that close int he first part of 2011
#                    #but its okay because I requre 6 months before and 6 months after below
#                    & ZIP == pharm_zip #takes out the obs where other zip codes shopped at the pharm
#                    & last_year_mon > "Jun 2011" & last_year_mon < "Jul 2013" #this is so I have 6 months before and 6 after
#                    ,
#                    .(last_year_mon, pharm_zip)]))
# 
# 
# events_data = data.table(num_openings = nrow(unique(rxnkaw[new_pharm == 1 
#                                                            & last_year_mon >= opening_year_mon + .5 # I want the openings to last
#                                                            & ZIP == pharm_zip #takes out the obs where other zip codes shopped at the pharm
#                                                            & opening_year_mon > "Jun 2011" & opening_year_mon < "Jul 2013" #this is so I have 6 months before and 6 after
#                                                            ,
#                                                            .(opening_year_mon, pharm_zip)])),
#                          num_closings = nrow(unique(rxnkaw[last_year_mon < "Dec 2013" & pharm_state == "OR" #closing pharm
#                                                            & opening_year_mon <= last_year_mon - .5  # not just fluke pharms - careful - this counts as flukes those that close int he first part of 2011
#                                                            #but its okay because I requre 6 months before and 6 months after below
#                                                            & ZIP == pharm_zip #takes out the obs where other zip codes shopped at the pharm
#                                                            & last_year_mon > "Jun 2011" & last_year_mon < "Jul 2013" #this is so I have 6 months before and 6 after
#                                                            ,
#                                                            .(last_year_mon, pharm_zip)])))
# write.csv(events_data, file = "~/current/pharmacy_deserts/events_data.csv")
# 
# 
# 
# 
# ####hist of entity broad####
# rxnkaw[, entity_broad_count := .N, by = entity_broad]
# 
# #histogram
# rxnkaw[, ent_broad_index := seq_len(.N), by = entity_broad]
# entity_broad_hist_data = rxnkaw[ent_broad_index == 1,]
# 
# rxnkaw[, ent_broad_index_or := seq_len(.N), by = c("entity_broad","STATE", "pharm_state")]
# entity_broad_hist_data_or = rxnkaw[ent_broad_index_or == 1,]
# 
# pdf(file = "~/current/pharmacy_deserts/entity_broad_hist_raw.pdf")
# ent_broad_hist = ggplot(data = entity_broad_hist_data) + 
#   geom_bar(aes(x=entity_broad, y=entity_broad_count), stat="identity") + 
#   theme_bw() +
#   theme(text = element_text(size=20))
# print(ent_broad_hist)
# 
# ent_broad_hist = ggplot(data = entity_broad_hist_data_or[STATE=="OR" & pharm_state == "OR"]) + 
#   geom_bar(aes(x=entity_broad, y=entity_broad_count), stat="identity") + 
#   theme_bw() +
#   theme(text = element_text(size=20))
# print(ent_broad_hist)
# dev.off()
# 
# 
# ####timeline####
# 
# rxnkaw[, total_claims_ym := .N, by = pick_up_yrmon]
# rxnkaw[, mail_order_share_ym := mean(mail_order_dummy, na.rm=T), by = pick_up_yrmon]
# 
# rxnkaw[, month_index := seq_len(.N), by = pick_up_yrmon]
# timeline_plot = rxnkaw[ month_index == 1, ]
# 
# opening_dates = unique(rxnkaw[new_pharm == 1 
#                               & last_year_mon >= opening_year_mon + .5 # I want the openings to last
#                               & ZIP == pharm_zip #takes out the obs where other zip codes shopped at the pharm
#                               & opening_year_mon > "Jun 2011" & opening_year_mon < "Jul 2013" #this is so I have 6 months before and 6 after
#                               ,]$opening_year_mon)
# 
# # of claim numbers
# pdf(file = "~/current/pharmacy_deserts/raw_timeline.pdf")
# ggplot(data = timeline_plot) +
#   geom_point(aes(x=as.yearmon(pick_up_yrmon), y=total_claims_ym)) +
#   geom_line(aes(x=as.yearmon(pick_up_yrmon), y=total_claims_ym)) + 
#   geom_vline(xintercept=opening_dates, color = "Red", linetype = 2) +
#   theme_bw() +
#   ylab("Total Claims") + xlab("") + theme(text = element_text(size = 20))
# 
# ggplot(data = timeline_plot) +
#   geom_point(aes(x=as.yearmon(pick_up_yrmon), y=total_claims_ym)) +
#   geom_line(aes(x=as.yearmon(pick_up_yrmon), y=total_claims_ym)) + 
#   #   geom_vline(xintercept=opening_dates, color = "Red", linetype = 2) +
#   theme_bw() +
#   ylab("Total Claims") + xlab("") + theme(text = element_text(size = 20))
# 
# #mail order share over time
# ggplot(data = timeline_plot) +
#   geom_point(aes(x=as.yearmon(pick_up_yrmon), y=mail_order_share_ym)) +
#   geom_line(aes(x=as.yearmon(pick_up_yrmon), y=mail_order_share_ym)) + 
#   theme_bw() + xlab("") + theme(text = element_text(size = 20)) +
#   ylab("Share Mail Order")
# 
# 
# #openings and closings over time
# 
# load("~/current/pharmacy_deserts/data/zip_data_stacked.RData")
# ggplot(data = zip_data_stacked[usable==1 & ms == 0 & zip_ym_index == 1]) +
#   geom_bar(aes(x=as.numeric(pick_up_yrmon)),  stat="count") + #should have 36 total
#   theme_bw() + xlab("") + theme(text = element_text(size = 25)) +
#   xlim(2011,2014) + 
#   ylab("Openings")
# 
# # rm(zip_data_stacked)
# gc()
# 
# 
# dev.off()
# 
# 
# #####number of walgreens stores in Oregon####
# rxnkaw[ , c("num_walgreens_in_or", "num_walgreens_zips_in_or",
#             "num_pharms_in_or", "num_patients_ever_shop_at_walgreens",
#             "num_patients_live_in_walgreens_zip", "num_patients_live_in_walgreens_zip_and_ever_shop_at_walg",
#             "num_patients_live_in_walgreens_zip_and_only_shop_at_walg",
#             "total_patients_shop_only_at_walg","num_patients_live_in_walgreens_zip_with_ins_ph",
#             "num_patients_live_in_walgreens_zip_with_ins_ph_ever_shop_at_walg") := NULL ]
# rxnkaw[pharm_state == "OR" & grepl("walg",entity_broad), num_walgreens_in_or := length(unique(npi))]
# rxnkaw[pharm_state == "OR" & grepl("walg",entity_broad), num_walgreens_zips_in_or := length(unique(pharm_zip))]
# rxnkaw[pharm_state == "OR", num_pharms_in_or := length(unique(npi))]
# rxnkaw[grepl("walg",entity_broad) & STATE == "OR", num_patients_ever_shop_at_walgreens := length(unique(personkey))]
# rxnkaw[STATE == "OR" & walgreens_zip_person == 1,
#        num_patients_live_in_walgreens_zip := length(unique(personkey))]
# rxnkaw[grepl("walg",entity_broad) & STATE == "OR" & walgreens_zip_person == 1,
#        num_patients_live_in_walgreens_zip_and_ever_shop_at_walg := length(unique(personkey))]
# rxnkaw[, walg_dummy := ifelse(grepl("walg",entity_broad), 1, 0)]
# rxnkaw[, min_walg_dummy_person := min(walg_dummy, na.rm=T), by = personkey]
# rxnkaw[STATE == "OR", patient_only_shop_at_walgreens := ifelse(min_walg_dummy_person == 1, 1, 0)]
# rxnkaw[patient_only_shop_at_walgreens==1, total_patients_shop_only_at_walg := length(unique(personkey))]
# rxnkaw[patient_only_shop_at_walgreens==1 & STATE == "OR" & walgreens_zip_person == 1,
#        num_patients_live_in_walgreens_zip_and_only_shop_at_walg := length(unique(personkey))]
# 
# rxnkaw[STATE == "OR"  & walgreens_zip_person == 1 & insurance_type_23 == "PH",
#        num_patients_live_in_walgreens_zip_with_ins_ph := length(unique(personkey))]
# rxnkaw[STATE == "OR"  & walgreens_zip_person == 1 & insurance_type_23 == "PH" & grepl("walg",entity_broad),
#        num_patients_live_in_walgreens_zip_with_ins_ph_ever_shop_at_walg := length(unique(personkey))]
# 
# 
# walgreens_num_table = unique(rxnkaw[!is.na(num_walgreens_in_or) & !is.na( num_walgreens_zips_in_or) & !is.na(num_pharms_in_or) 
#                                     & !is.na( num_patients_ever_shop_at_walgreens) & !is.na(num_patients_live_in_walgreens_zip) 
#                                     & !is.na( num_patients_live_in_walgreens_zip_and_ever_shop_at_walg) 
#                                     & !is.na(num_patients_live_in_walgreens_zip_and_only_shop_at_walg) 
#                                     & !is.na(total_patients_shop_only_at_walg) 
#                                     & !is.na( num_patients_live_in_walgreens_zip_with_ins_ph) 
#                                     & !is.na(num_patients_live_in_walgreens_zip_with_ins_ph_ever_shop_at_walg),
#                                     .(num_walgreens_in_or, num_walgreens_zips_in_or,
#                                       num_pharms_in_or, num_patients_ever_shop_at_walgreens,
#                                       num_patients_live_in_walgreens_zip, num_patients_live_in_walgreens_zip_and_ever_shop_at_walg,
#                                       num_patients_live_in_walgreens_zip_and_only_shop_at_walg,
#                                       total_patients_shop_only_at_walg,
#                                       num_patients_live_in_walgreens_zip_with_ins_ph,
#                                       num_patients_live_in_walgreens_zip_with_ins_ph_ever_shop_at_walg)])
# 
# walg_num_tab = melt(walgreens_num_table)
# write.csv(walg_num_tab, file = "~/current/pharmacy_deserts/walg_num_tab.csv")
# gc()
# 
# 
# 
# ####map of patients####
# rxnkaw[, total_patients_z := length(unique(personkey)), by = ZIP]
# 
# #coordinates of pharmacies
# pharm_coord_1 = as.data.table(read.csv("~/current/pharmacy_deserts/data/pharm_coord.csv"))
# 
# pharm_coord_1[, id := seq_len(.N)]
# 
# load("~/current/pharmacy_deserts/data/oregon_pharmacy_latlon.RData")
# 
# setkey(geo_data, ID)
# setkey(pharm_coord_1, id)
# pharm_coord_1[geo_data, pharm_lonlat := lon_lat]
# pharm_coord_1[, pharm_lon := NULL]
# pharm_coord_1[, pharm_lat := NULL]
# for (Z in seq(1, nrow(pharm_coord_1))) {
#   
#   pharm_coord_1[!is.na(pharm_lonlat)
#                 & id == Z,
#                 pharm_lon := strsplit(pharm_lonlat, ",")[1]]
#   pharm_coord_1[!is.na(pharm_lonlat)
#                 & id == Z,
#                 pharm_lat := unlist(strsplit(pharm_lonlat, ","))[2]]
# }
# rm(geo_data)
# 
# setkey(rxnkaw, pharm_add_full)
# setkey(pharm_coord_1, possible_choices)
# rxnkaw[pharm_coord_1, pharm_lat := pharm_lat]
# rxnkaw[pharm_coord_1, pharm_lon := pharm_lon]
# rxnkaw[, pharm_index := seq_len(.N), by = pharm_add_full]
# pharm_map_data = rxnkaw[pharm_index == 1,]
# pharm_map_data[, new_pharm_zip := max(new_pharm, na.rm=T), by = pharm_zip]
# pharm_map_data[, pharm_zip_index := seq_len(.N), by = pharm_zip]
# pharm_map_data_color = pharm_map_data[pharm_zip_index ==1 & grepl("^97", pharm_zip),]
# pharm_map_data_color[, pharm_zip := as.integer(as.character(pharm_zip))]
# setkey(pharm_map_data_color, pharm_zip)
# 
# 
# # unzip, and load tools
# library(maptools); library(gpclib); library(sp);gpclibPermit()
# # read data into R
# shapefile <- readShapeSpatial('~/current/pharmacy_deserts/data/tl_2013_us_zcta510.shp',
#                               proj4string = CRS("+proj=longlat +datum=WGS84"))
# # convert to a data.frame for use with ggplot2/ggmap and plot
# zip_shapes_all <- as.data.table(fortify(shapefile, region = "ZCTA5CE10"))
# zip_shapes = zip_shapes_all[grepl("^97", id),]
# zip_shapes[, id_int := as.integer(id)]
# 
# # oregon_map = get_map(location = "oregon", maptype="roadmap", zoom=6, color="bw")
# # willamette_map = get_map(location = "willamette valley, oregon", maptype="roadmap", zoom=7, color="bw")
# portland_map = get_map(location = "portland, or", maptype="roadmap", zoom=10, color="bw")
# eugene_map = get_map(location = "eugene, or", maptype="roadmap", zoom=9, color="bw")
# bend_map = get_map(location = "bend, or", maptype="roadmap", zoom=7, color="bw")
# salem_map = get_map(location = "salem, or", maptype="roadmap", zoom=9, color="bw")
# albany_map = get_map(location = "albany, or", maptype="roadmap", zoom=9, color="bw")
# 
# #merge the zip map data and the zip shapes
# setkey(zip_shapes, id_int)
# 
# rxnkaw[, zip_index := seq_len(.N), by = ZIP]
# map_data = rxnkaw[zip_index == 1,]
# setkey(map_data, ZIP)
# 
# zip_shapes[, total_patients_z := NULL]
# zip_shapes[map_data, total_patients_z := total_patients_z]
# 
# setkey(zip_shapes, id_int)
# zip_shapes[, new_pharm_zip := NULL]
# zip_shapes[pharm_map_data_color, new_pharm_zip := opener_zip_dummy]
# zip_shapes[is.na(new_pharm_zip), new_pharm_zip := 0]
# 
# closer_zips = unique(rxnkaw[last_year_mon < "Dec 2013" & pharm_state == "OR" #closing pharm
#                             & opening_year_mon <= last_year_mon - .5  # not just fluke pharms - careful - this counts as flukes those that close int he first part of 2011
#                             #but its okay because I requre 6 months before and 6 months after below
#                             & ZIP == pharm_zip #takes out the obs where other zip codes shopped at the pharm
#                             & last_year_mon > "Jun 2011" & last_year_mon < "Jul 2013" #this is so I have 6 months before and 6 after
#                             ,]$pharm_zip)
# 
# opener_zips = unique(rxnkaw[new_pharm == 1 
#                             & last_year_mon >= opening_year_mon + .5 # I want the openings to last
#                             & ZIP == pharm_zip #takes out the obs where other zip codes shopped at the pharm
#                             & opening_year_mon > "Jun 2011" & opening_year_mon < "Jul 2013" #this is so I have 6 months before and 6 after
#                             ,]$pharm_zip)
# zip_shapes[, opener_zip_dummy := ifelse(id_int %in% opener_zips, 1, 0)]
# zip_shapes[, closer_zip_dummy := ifelse(id_int %in% closer_zips, 1, 0)]
# 
# 
# pdf(file = "~/current/pharmacy_deserts/access_maps.pdf")
# map = ggmap(bend_map) +
#   geom_polygon(data = zip_shapes[!is.na(total_patients_z)],
#                aes_string(x="long", y="lat", group = "group", fill = "as.numeric(total_patients_z)"),
#                alpha = .75, color = NA) +
#   geom_point(data = pharm_map_data, 
#              aes(x=as.numeric(pharm_lon), y=as.numeric(pharm_lat)),
#              size = .5,
#              color = "Dark Blue") + 
#   coord_map() + 
#   scale_fill_distiller(palette = "YlOrRd",
#                        #                        limits = c(0,25),
#                        #                        breaks = c(seq(1,25)), #pretty_breaks(n=12),
#                        #                        labels = c("<1",seq(3,23,2), ">25"),
#                        direction = 1) +
#   labs(fill = "Total Unique Patients") +
#   theme_nothing(legend = TRUE) +
#   guides(fill = guide_legend(reverse = TRUE, 
#                              override.aes = list(alpha = 1),
#                              title.position = "top",
#                              title.hjust =0.5,
#                              nrow = 1)) +
#   theme(text = element_text(size = 20, family = "serif")) +
#   theme(legend.position = "bottom") +
#   ggtitle("")
# print(map)
# 
# 
# #zoom in on portland
# map = ggmap(portland_map) +
#   geom_polygon(data = zip_shapes[!is.na(total_patients_z)],
#                aes_string(x="long", y="lat", group = "group", fill = "as.numeric(total_patients_z)"),
#                alpha = .75, color = NA) +
#   geom_point(data = pharm_map_data, 
#              aes(x=as.numeric(pharm_lon), y=as.numeric(pharm_lat)),
#              size = .5,
#              color = "Dark Blue") + 
#   coord_map() + 
#   scale_fill_distiller(palette = "YlOrRd",
#                        #                        limits = c(0,25),
#                        #                        breaks = c(seq(1,25)), #pretty_breaks(n=12),
#                        #                        labels = c("<1",seq(3,23,2), ">25"),
#                        direction = 1) +
#   labs(fill = "Total Unique Patients") +
#   theme_nothing(legend = TRUE) +
#   guides(fill = guide_legend(reverse = TRUE, 
#                              override.aes = list(alpha = 1),
#                              title.position = "top",
#                              title.hjust =0.5,
#                              nrow = 1)) +
#   theme(text = element_text(size = 20, family = "serif")) +
#   theme(legend.position = "bottom") +
#   ggtitle("")
# print(map)
# 
# 
# #zoom in on eugene
# #zoom in on portland
# map = ggmap(eugene_map) +
#   geom_polygon(data = zip_shapes[!is.na(total_patients_z)],
#                aes_string(x="long", y="lat", group = "group", fill = "as.numeric(total_patients_z)"),
#                alpha = .75, color = NA) +
#   geom_point(data = pharm_map_data, 
#              aes(x=as.numeric(pharm_lon), y=as.numeric(pharm_lat)),
#              size = .5,
#              color = "Dark Blue") + 
#   coord_map() + 
#   scale_fill_distiller(palette = "YlOrRd",
#                        #                        limits = c(0,25),
#                        #                        breaks = c(seq(1,25)), #pretty_breaks(n=12),
#                        #                        labels = c("<1",seq(3,23,2), ">25"),
#                        direction = 1) +
#   labs(fill = "Total Unique Patients") +
#   theme_nothing(legend = TRUE) +
#   guides(fill = guide_legend(reverse = TRUE, 
#                              override.aes = list(alpha = 1),
#                              title.position = "top",
#                              title.hjust =0.5,
#                              nrow = 1)) +
#   theme(text = element_text(size = 20, family = "serif")) +
#   theme(legend.position = "bottom") +
#   ggtitle("")
# print(map)
# 
# 
# ####map of openings and closings####
# map = ggmap(bend_map) +
#   geom_polygon(data = zip_shapes,
#                aes_string(x="long", y="lat", group = "group", fill = "as.factor(opener_zip_dummy)"),
#                alpha = .75, color = NA) +
#   #   geom_point(data = pharm_map_data[new_pharm==1], 
#   #              aes(x=as.numeric(pharm_lon), y=as.numeric(pharm_lat), shape = as.factor(new_pharm)),
#   #              size = 2,
#   #              color = "Dark Blue") + 
#   coord_map() + 
#   labs(fill = "Zip Codes with Openings") +
#   theme_nothing(legend = TRUE) +
#   scale_fill_manual(labels =  c("None","Opening"),
#                     values = c("Dark Red","Dark Blue")) + 
#   guides(fill = guide_legend(reverse = TRUE, 
#                              override.aes = list(alpha = 1),
#                              title.position = "top",
#                              title.hjust =0.75,
#                              nrow = 1)) +
#   theme(text = element_text(size = 20, family = "serif")) +
#   theme(legend.position = "bottom") +
#   ggtitle("")
# print(map)
# 
# map = ggmap(bend_map) +
#   geom_polygon(data = zip_shapes,
#                aes_string(x="long", y="lat", group = "group", fill = "as.factor(closer_zip_dummy)"),
#                alpha = .75, color = NA) +
#   #   geom_point(data = pharm_map_data[new_pharm==1], 
#   #              aes(x=as.numeric(pharm_lon), y=as.numeric(pharm_lat), shape = as.factor(new_pharm)),
#   #              size = 2,
#   #              color = "Dark Blue") + 
#   coord_map() + 
#   labs(fill = "Zip Codes with Closings") +
#   theme_nothing(legend = TRUE) +
#   scale_fill_manual(labels =  c("None","Closing"),
#                     values = c("Dark Red","Dark Orange")) + 
#   guides(fill = guide_legend(reverse = TRUE, 
#                              override.aes = list(alpha = 1),
#                              title.position = "top",
#                              title.hjust =0.75,
#                              nrow = 1)) +
#   theme(text = element_text(size = 20, family = "serif")) +
#   theme(legend.position = "bottom") +
#   ggtitle("")
# print(map)
# 
# dev.off()
# 
# 
# ####map of walgreens zips####
# #merge the zip map data and the zip shapes
# setkey(zip_shapes, id_int)
# 
# rxnkaw[, zip_index := NULL]
# rxnkaw[pick_up_yrmon == "Jan 2012", zip_index := seq_len(.N), by = ZIP]
# map_data = rxnkaw[zip_index == 1,]
# setkey(map_data, ZIP)
# 
# zip_shapes[, walgreens_zip_person := NULL]
# zip_shapes[map_data, walgreens_zip_person := walgreens_zip_person]
# zip_shapes[map_data, tot_pharms := tot_pharms_ym_z]
# zip_shapes[, tot_pharms_walg_zips := tot_pharms * walgreens_zip_person]
# 
# 
# #get points for walgreens locations
# pharm_map_data = rxnkaw[pharm_index == 1 & entity_broad == "walgreens",]
# 
# 
# 
# pdf(file = "~/current/pharmacy_deserts/walgreens_maps.pdf")
# map = ggmap(bend_map) +
#   geom_polygon(data = zip_shapes[!is.na(walgreens_zip_person)],
#                aes_string(x="long", y="lat", group = "group", fill = "as.factor(walgreens_zip_person)"),
#                alpha = .75, color = NA) +
#   #   geom_point(data = pharm_map_data, 
#   #              aes(x=as.numeric(pharm_lon), y=as.numeric(pharm_lat)),
#   #              size = 3,
#   #              color = "Dark Blue") + 
#   coord_map() + 
#   scale_fill_manual(breaks = c("0", "1"),
#                     labels = c("None","Walgreens"),
#                     values=c("Orange","Dark Red")) + 
#   labs(fill = "Walgreens Zip Code") +
#   theme_nothing(legend = TRUE) +
#   guides(fill = guide_legend(reverse = TRUE, 
#                              override.aes = list(alpha = 1),
#                              title.position = "top",
#                              title.hjust =0.5,
#                              nrow = 1)) +
#   theme(text = element_text(size = 20, family = "serif")) +
#   theme(legend.position = "bottom") +
#   ggtitle("")
# print(map)
# 
# 
# 
# map = ggmap(bend_map) +
#   geom_polygon(data = zip_shapes[!is.na(tot_pharms_walg_zips)],
#                aes_string(x="long", y="lat", group = "group", fill = "as.numeric(tot_pharms_walg_zips)"),
#                alpha = .75, color = NA) +
#   coord_map() + 
#   scale_fill_distiller(palette = "YlOrRd",
#                        #                        limits = c(0,25),
#                        #                        breaks = c(seq(1,25)), #pretty_breaks(n=12),
#                        #                        labels = c("<1",seq(3,23,2), ">25"),
#                        direction = 1) +
#   labs(fill = "Pharmacies in Walgreens Zip Codes") +
#   theme_nothing(legend = TRUE) +
#   guides(fill = guide_legend(reverse = TRUE, 
#                              override.aes = list(alpha = 1),
#                              title.position = "top",
#                              title.hjust =0.5,
#                              nrow = 1)) +
#   theme(text = element_text(size = 20, family = "serif")) +
#   theme(legend.position = "bottom") +
#   ggtitle("")
# print(map)
# 
# 
# dev.off()
# 
# 
# ####opening events####
# rxnkaw[pharm_zip %in% opener_zips, num_pharms_ym_pz := length(unique(pharm_add_full)), by = c("pharm_zip","pick_up_yrmon")]
# rxnkaw[pharm_zip %in% closer_zips, num_pharms_ym_pz := length(unique(pharm_add_full)), by = c("pharm_zip","pick_up_yrmon")]
# 
# rxnkaw[, ym_pz_index := seq_len(.N), by = c("pharm_zip","pick_up_yrmon")]
# 
# pdf(file = "~/current/pharmacy_deserts/entry_events_variation.pdf", width = 10)
# ggplot(data = rxnkaw[pharm_zip %in% opener_zips & ym_pz_index == 1]) +
#   geom_point(aes(x=pick_up_yrmon, y=num_pharms_ym_pz, color = as.factor(pharm_zip)), size =.5) +
#   geom_line(aes(x=pick_up_yrmon, y=num_pharms_ym_pz, color = as.factor(pharm_zip))) +
#   theme_bw() +
#   facet_wrap(~pharm_zip) +
#   theme(legend.position = "none",
#         text = element_text(size = 20),
#         axis.text.x = element_text(angle = 45, vjust = .5)) +
#   ylab("Number of pharmacies") + xlab("")
# 
# ggplot() +
#   geom_vline(data = rxnkaw[new_pharm == 1 
#                            & last_year_mon >= opening_year_mon + .5 # I want the openings to last
#                            & ZIP == pharm_zip #takes out the obs where other zip codes shopped at the pharm
#                            & opening_year_mon > "Jun 2011" & opening_year_mon < "Jul 2013" #this is so I have 6 months before and 6 after],
#                            ],
#              aes(xintercept = opening_year_mon)) + 
#   geom_point(data = rxnkaw[pharm_zip %in% opener_zips & ym_pz_index == 1],
#              aes(x=pick_up_yrmon, y=num_pharms_ym_pz, color = as.factor(pharm_zip)), size =.5) +
#   geom_line(data = rxnkaw[pharm_zip %in% opener_zips & ym_pz_index == 1],
#             aes(x=pick_up_yrmon, y=num_pharms_ym_pz, color = as.factor(pharm_zip))) +
#   theme_bw() +
#   facet_wrap(~pharm_zip) +
#   theme(legend.position = "none",
#         text = element_text(size = 20),
#         axis.text.x = element_text(angle = 45, vjust = .5)) +
#   ylab("Number of pharmacies") + xlab("")
# 
# 
# #list of addresses of pharms and months in 97701
# adds = as.data.table(unique(rxnkaw[pharm_zip == 97701 & (pick_up_yrmon == "Sep 2011" | pick_up_yrmon == "Aug 2011" | pick_up_yrmon == "Oct 2011"),
#                                    .(pharm_add_full, pick_up_yrmon)]))
# setkey(adds, pick_up_yrmon, pharm_add_full)
# 
# 
# ggplot(data = rxnkaw[pharm_zip %in% closer_zips & ym_pz_index == 1]) +
#   geom_point(aes(x=pick_up_yrmon, y=num_pharms_ym_pz, color = as.factor(pharm_zip)), size =.5) +
#   geom_line(aes(x=pick_up_yrmon, y=num_pharms_ym_pz, color = as.factor(pharm_zip))) +
#   theme_bw() +
#   facet_wrap(~pharm_zip) +
#   theme(legend.position = "none",
#         text = element_text(size = 20),
#         axis.text.x = element_text(angle = 45, vjust = .5)) +
#   ylab("Number of pharmacies") + xlab("")
# 
# ggplot() +
#   geom_vline(data = rxnkaw[last_year_mon < "Dec 2013" & pharm_state == "OR" #closing pharm
#                            & opening_year_mon <= last_year_mon - .5  # not just fluke pharms - careful - this counts as flukes those that close int he first part of 2011
#                            #but its okay because I requre 6 months before and 6 months after below
#                            & ZIP == pharm_zip #takes out the obs where other zip codes shopped at the pharm
#                            & last_year_mon > "Jun 2011" & last_year_mon < "Jul 2013" #this is so I have 6 months before and 6 after
#                            ],
#              aes(xintercept = last_year_mon)) + 
#   geom_point(data = rxnkaw[pharm_zip %in% closer_zips & ym_pz_index == 1],
#              aes(x=pick_up_yrmon, y=num_pharms_ym_pz, color = as.factor(pharm_zip)), size =.5) +
#   geom_line(data = rxnkaw[pharm_zip %in% closer_zips & ym_pz_index == 1],
#             aes(x=pick_up_yrmon, y=num_pharms_ym_pz, color = as.factor(pharm_zip))) +
#   theme_bw() +
#   facet_wrap(~pharm_zip) +
#   theme(legend.position = "none",
#         text = element_text(size = 20),
#         axis.text.x = element_text(angle = 45, vjust = .5)) +
#   ylab("Number of pharmacies") + xlab("")
# 
# 
# 
# 
# 
# 
# dev.off()
# 
# 
# 
# 
# ####mail copay for insurance types####
# rxnkaw[STATE=="OR", mean_copay_mail_ins8 := mean(copay, na.rm=T), by = c("insurance_type_8","mail_order_dummy")]
# rxnkaw[STATE=="OR", sd_copay_mail_ins8 := sd(copay, na.rm=T), by = c("insurance_type_8","mail_order_dummy")]
# mail_ins8_copay_table = unique(rxnkaw[STATE=="OR",.(insurance_type_8,mail_order_dummy,mean_copay_mail_ins8,sd_copay_mail_ins8)])
# mail_ins8_copay_table_wide = as.data.table(dcast(mail_ins8_copay_table,
#                                                  insurance_type_8 ~ mail_order_dummy, 
#                                                  value.var = "mean_copay_mail_ins8"))
# mail_ins8_copay_table_wide[, sd := 0]
# mail_ins8_copay_table_wide_sd = as.data.table(dcast(mail_ins8_copay_table,
#                                                     insurance_type_8 ~ mail_order_dummy, 
#                                                     value.var = "sd_copay_mail_ins8"))
# mail_ins8_copay_table_wide_sd[, sd := 1]
# mail_ins8_copay=rbindlist(list(mail_ins8_copay_table_wide,mail_ins8_copay_table_wide_sd), fill=T)
# setkey(mail_ins8_copay,insurance_type_8,sd)
# 
# write.csv(mail_ins8_copay, file = "~/current/pharmacy_deserts/mail_ins8_copay.csv")
# 
# ####random summary stats####
# #pharms per person and limited to within OR
# rxnkaw[, c("total_num_pharms","total_num_pharms_state","person_index") := NULL]
# rxnkaw[!is.na(pharm_state) & pharm_state != "", total_num_pharms := length(unique(pharm_add_full))]
# rxnkaw[!is.na(pharm_state) & pharm_state != "", total_num_pharms_state := length(unique(pharm_add_full)), by = pharm_state]
# 
# rxnkaw[!is.na(pharm_state) & pharm_state != "", pharms_per_person := length(unique(pharm_add_full)), by = personkey]
# rxnkaw[!is.na(pharm_state) & pharm_state != "", person_index := seq_len(.N), by = personkey]
# rxnkaw[!is.na(pharm_state) & pharm_state != "" & person_index == 1, avg_pharms_per_person := mean(pharms_per_person, na.rm=T) ]
# rxnkaw[!is.na(pharm_state) & pharm_state != "" & person_index == 1, sd_pharms_per_person := sd(pharms_per_person, na.rm=T) ]
# 
# rxnkaw[!is.na(pharm_state) & pharm_state != "" & STATE == "OR" & pharm_state == "OR", 
#        pharms_per_person_or := length(unique(pharm_add_full)), by = personkey]
# rxnkaw[!is.na(pharm_state) & pharm_state != "" & STATE == "OR" & pharm_state == "OR", 
#        person_index_or := seq_len(.N), by = personkey]
# rxnkaw[!is.na(pharm_state) & pharm_state != "" & person_index == 1 & STATE == "OR" & pharm_state == "OR", 
#        avg_pharms_per_person_or := mean(pharms_per_person, na.rm=T) ]
# rxnkaw[!is.na(pharm_state) & pharm_state != "" & person_index == 1 & STATE == "OR" & pharm_state == "OR", 
#        sd_pharms_per_person_or := sd(pharms_per_person, na.rm=T) ]
# 
# 
# 
# 
# 
# rxnkaw[!is.na(pharm_state) & pharm_state != "", patients_per_pharmacy := length(unique(personkey)), by = pharm_add_full ]
# rxnkaw[!is.na(pharm_state) & pharm_state != "", pharm_index := seq_len(.N), by = pharm_add_full]
# rxnkaw[!is.na(pharm_state) & pharm_state != "" & pharm_index == 1, avg_people_per_pharm := mean(patients_per_pharmacy, na.rm=T) ]
# rxnkaw[!is.na(pharm_state) & pharm_state != "" & pharm_index == 1, sd_people_per_pharm := sd(patients_per_pharmacy, na.rm=T) ]
# 
# rxnkaw[!is.na(pharm_state) & pharm_state != "" & STATE == "OR" & pharm_state == "OR", 
#        patients_per_pharmacy_or := length(unique(personkey)), by = pharm_add_full ]
# rxnkaw[!is.na(pharm_state) & pharm_state != "" & STATE == "OR" & pharm_state == "OR",
#        pharm_index_or := seq_len(.N), by = pharm_add_full]
# rxnkaw[!is.na(pharm_state) & pharm_state != "" & pharm_index == 1 & STATE == "OR" & pharm_state == "OR",
#        avg_people_per_pharm_or := mean(patients_per_pharmacy, na.rm=T) ]
# rxnkaw[!is.na(pharm_state) & pharm_state != "" & pharm_index == 1 & STATE == "OR" & pharm_state == "OR", 
#        sd_people_per_pharm_or := sd(patients_per_pharmacy, na.rm=T) ]
# 
# 
# rxnkaw[, claims_per_person := .N, by = personkey]
# rxnkaw[person_index == 1, mean_claims_per_person := mean(claims_per_person, na.rm=T)]
# rxnkaw[person_index == 1, sd_claims_per_person := sd(claims_per_person, na.rm=T)]
# 
# all_means_table = unique(rxnkaw[,.(avg_pharms_per_person,sd_pharms_per_person,
#                                    avg_pharms_per_person_or,sd_pharms_per_person_or,
#                                    avg_people_per_pharm,sd_people_per_pharm,
#                                    avg_people_per_pharm_or,sd_people_per_pharm_or)])
# amt = melt(all_means_table)
# write.csv(amt, file = "~/current/pharmacy_deserts/pharms_per_person_table.csv")
# rm(all_means_table, amt)
# 
# 
# 
# ###################################################################################################################################################################################
# ####Section 10: national prediction ####
# ###################################################################################################################################################################################
# 
# 
# B=6
# for (outcome in c("log_total_pills_dispensed_ym_z","log_total_pats_ym_z","log_total_claims_ym_z",
#                   "log_total_new_pats_ym_z_plus_1"
# )) {
#   
#   predict_data_nc = paste("prediction_",outcome,"_data_nc",sep="")
#   predict_data_c = paste("prediction_",outcome,"_data_c",sep="")
#   
#   assign(eval(predict_data_nc),as.data.table(read.csv(paste("~/current/pharmacy_deserts/",eval(outcome),"_prediction_set_nc.csv", sep=""))))
#   assign(eval(predict_data_c),as.data.table(read.csv(paste("~/current/pharmacy_deserts/",eval(outcome),"_prediction_set_c.csv", sep=""))))
#   
#   get(predict_data_nc)[, zip_factor := as.factor(zip_code)]
#   get(predict_data_c)[, zip_factor := as.factor(zip_code)]
# }
# 
# 
# ####merge on the covariates####
# 
# #ACS
# acs_predict_2013 <- as.data.table(read.csv("~/current/pharmacy_deserts/data/acs_predict/social_explorer_2013_predictors.csv"))
# acs_predict_2013 = acs_predict_2013[FIPS != "Geo_FIPS"]
# acs_predict_2013 = acs_predict_2013[State.U.S..Abbreviation..USPS. != "pr"]
# acs_predict_2011 <- as.data.table(read.csv("~/current/pharmacy_deserts/data/acs_predict/social_explorer_2011_predictors.csv"))
# acs_predict_2011 = acs_predict_2011[FIPS != "Geo_FIPS"]
# acs_predict_2011 = acs_predict_2011[State.U.S..Abbreviation..USPS. != "pr"]
# # 
# # names(acs_predict_2011)[!(names(acs_predict_2011) %in% names(acs_predict_2013))]
# # names(acs_predict_2013)[!(names(acs_predict_2013) %in% names(acs_predict_2011))]
# 
# # acs = rbindlist(list(acs_predict_2011, acs_predict_2013), fill=T)
# 
# #drop the 2011 for now (too messy and inconsistent with 2013)
# acs = acs_predict_2013
# 
# #ZBP
# zbp_2013 <- as.data.table(read.csv("~/current/pharmacy_deserts/data/zbp_predict/BP_2013_00CZ2_with_ann.csv"))
# zbp_2011 <- as.data.table(read.csv("~/current/pharmacy_deserts/data/zbp_predict/BP_2011_00CZ2_with_ann.csv"))
# 
# zbp = rbindlist(list(zbp_2011, zbp_2013), fill=T)
# 
# #"reshape" the zbp part
# zbp[, Meaning.of.NAICS.code :=ifelse(Year == 2013,as.character( Meaning.of.2012.NAICS.code), as.character( Meaning.of.2007.NAICS.code))]
# zbp[, v := paste( Meaning.of.NAICS.code, Employment.size.of.establishment, sep="+")]
# zbp[is.na(Number.of.establishments), Number.of.establishments := 0]
# zbpw = as.data.table(dcast(zbp, Id2 + Year ~ v, value.var = "Number.of.establishments" ))
# 
# 
# #all that above is junk until I get every state zbp
# zbp <- as.data.table(read.csv("~/current/pharmacy_deserts/data/zbp_predict/BP_2013_00CZ1_with_ann.csv"))
# 
# numeric_names_zbp = names(zbp)[!(names(zbp) %in% c("Id2", "Year"))]
# 
# for (X in numeric_names_zbp) {
#   zbp[, (X) := as.numeric(as.character(get(X)))]
#   zbp[is.na(get(X)), (X) := 0]
# }
# 
# #get the change from 2011 to 2013
# 
# #drop the 2011
# # zbpw = zbpw[Year == 2013]
# 
# 
# #####now merge####
# setkey(zbp, Id2, Year)
# acs[, zip := as.numeric(as.character(ZIP.Code.Tabulation.Area..5.digit.))]
# setkey(acs, zip)
# 
# acs_zbp = merge(acs, zbp, by.x="zip", by.y="Id2", all=T)
# 
# #every zip code with NA zbp data gets 0
# 
# for (X in numeric_names_zbp) {
#   acs_zbp[is.na(get(X)), (X) := 0]
# }
# 
# #convert all the predictors to numeric
# for (X in names(acs_zbp)) {
#   acs_zbp[, (X) := as.numeric(as.character(get(X)))]
#   acs_zbp[is.na(get(X)), (X) := 0]
# }
# 
# #remove colinear variables
# acs_zbp[, c("Area..Land..1","Area.Total..Area..Land.","Area..Water.","Housing.units..Vacant") := NULL]
# 
# #save
# fwrite(acs_zbp, file = "~/current/pharmacy_deserts/data/acs_zbp_predictors.csv")
# 
# non_numeric_names = c("Year","Meaning.of.2012.NAICS.code" ,"X2012.NAICS.code",
#                       "Geographic.area.name", "Geographic.identifier.code"  ,
#                       "zip"        ,                                                                                                                                                                              
#                       "FIPS"         ,                                                                                                                                                                            
#                       "Name.of.Area"  ,                                                                                                                                                                           
#                       "Qualifying.Name"       ,                                                                                                                                                                   
#                       "State.U.S..Abbreviation..USPS."     ,                                                                                                                                                      
#                       "Summary.Level"           ,                                                                                                                                                                 
#                       "Geographic.Component"    ,                                                                                                                                                                 
#                       "File.Identification"        ,                                                                                                                                                              
#                       "Logical.Record.Number"         ,                                                                                                                                                           
#                       "US"                         ,                                                                                                                                                              
#                       "Region"                      ,                                                                                                                                                             
#                       "Division"               ,                                                                                                                                                                  
#                       "State..Census.Code."       ,                                                                                                                                                               
#                       "State..FIPS."              ,                                                                                                                                                               
#                       "County"                   ,                                                                                                                                                                
#                       "County.Subdivision..FIPS."   ,                                                                                                                                                             
#                       "Place..FIPS.Code."              ,                                                                                                                                                          
#                       "Place..State.FIPS...Place.FIPS."    ,                                                                                                                                                      
#                       "Census.Tract"                                    ,                                                                                                                                         
#                       "Block.Group"                                       ,                                                                                                                                       
#                       "Consolidated.City"                                      ,                                                                                                                                  
#                       "American.Indian.Area.Alaska.Native.Area.Hawaiian.Home.Land..Census.",                                                                                                                      
#                       "American.Indian.Area.Alaska.Native.Area.Hawaiian.Home.Land..FIPS." ,                                                                                                                       
#                       "American.Indian.Trust.Land.Hawaiian.Home.Land.Indicator"   ,                                                                                                                               
#                       "American.Indian.Tribal.Subdivision..Census."         ,                                                                                                                                     
#                       "American.Indian.Tribal.Subdivision..FIPS."  ,                                                                                                                                              
#                       "Alaska.Native.Regional.Corporation..FIPS."  ,                                                                                                                                              
#                       "Metropolitan.and.Micropolitan.Statistical.Area"     ,                                                                                                                                      
#                       "Combined.Statistical.Area"          ,                                                                                                                                                      
#                       "Metropolitan.Division"        ,                                                                                                                                                            
#                       "Metropolitan.Area.Central.City"      ,                                                                                                                                                     
#                       "Metropolitan.Micropolitan.Indicator.Flag"     ,                                                                                                                                            
#                       "New.England.City.and.Town.Combined.Statistical.Area"     ,                                                                                                                                 
#                       "New.England.City.and.Town.Area"         ,                                                                                                                                                  
#                       "New.England.City.and.Town.Area.Division"  ,                                                                                                                                                
#                       "Urban.Area"                           ,                                                                                                                                                    
#                       "Urban.Area.Central.Place"            ,                                                                                                                                                     
#                       "Current.Congressional.District...." ,                                                                                                                                                      
#                       "State.Legislative.District.Upper"       ,                                                                                                                                                  
#                       "State.Legislative.District.Lower"   ,                                                                                                                                                      
#                       "Voting.District"                           ,                                                                                                                                               
#                       "ZIP.Code.Tabulation.Area..3.digit."    ,                                                                                                                                                   
#                       "ZIP.Code.Tabulation.Area..5.digit."      ,                                                                                                                                                 
#                       "Subbarrio..FIPS."       ,                                                                                                                                                                  
#                       "School.District..Elementary."   ,                                                                                                                                                          
#                       "School.District..Secondary."   ,                                                                                                                                                           
#                       "School.District..Unified."    ,                                                                                                                                                            
#                       "Urban.Rural"               ,                                                                                                                                                               
#                       "Principal.City.Indicator"  ,                                                                                                                                                               
#                       "Traffic.Analysis.Zone",                                                                                                                                                                    
#                       "Urban.Growth.Area"      ,                                                                                                                                                                  
#                       "Public.Use.Microdata.Area...5..File",                                                                                                                                                      
#                       "Public.Use.Microdata.Area...1..File",                                                                                                                                                      
#                       "Geographic.Identifier"  ,                                                                                                                                                                  
#                       "Tribal.Tract"  ,                                                                                                                                                                           
#                       "Tribal.Block.Group"   ,
#                       "X"                 ,                                                                                                                                                                       
#                       "name"             ,                                                                                                                                                                        
#                       "coef"           ,                                                                                                                                                                          
#                       "se"             ,                                                                                                                                                                          
#                       "ymin"      ,                                                                                                                                                                               
#                       "ymax"          ,                                                                                                                                                                           
#                       "zip_code"     ,                                                                                                                                                                            
#                       "zip_factor"  )                                                                                                                                                  
# 
# #load in the coefficient data and merge on the acs zbp
# for (outcome in c("log_total_pills_dispensed_ym_z","log_total_pats_ym_z","log_total_claims_ym_z",
#                   "log_total_new_pats_ym_z_plus_1"
# )) {
#   
#   predict_data_nc = paste("prediction_",outcome,"_data_nc",sep="")
#   predict_data_c = paste("prediction_",outcome,"_data_c",sep="")
#   
#   get(predict_data_nc)[, zip := as.numeric(as.character(zip_factor))]
#   get(predict_data_c)[, zip := as.numeric(as.character(zip_factor))]
#   
#   assign(eval(predict_data_nc) , 
#          merge(get(predict_data_nc), acs_zbp, by.x="zip", by.y="zip", all=T))
#   assign(eval(predict_data_c) , 
#          merge(get(predict_data_c), acs_zbp, by.x="zip", by.y="zip", all=T))
#   #   
#   #   assign(eval(predict_data_nc) , 
#   #          merge(get(predict_data_nc), zbp_2013, by.x="zip_factor", by.y="Id2", all=T))
#   #   assign(eval(predict_data_c) , 
#   #          merge(get(predict_data_c), zbp_2013, by.x="zip_factor", by.y="Id2", all=T))
#   #   
#   #   get(predict_data_nc)[is.na(Number.of.establishments), Number.of.establishments := "0"]
#   #   get(predict_data_nc)[is.na(Paid.employees.for.pay.period.including.March.12..number.), 
#   #                        Paid.employees.for.pay.period.including.March.12..number. := "0"]
#   #   get(predict_data_nc)[is.na(First.quarter.payroll...1.000.), 
#   #                        First.quarter.payroll...1.000. := "0"]
#   #   get(predict_data_nc)[is.na(Annual.payroll...1.000.), Annual.payroll...1.000. := "0"]
#   
# }
# 
# 
# 
# 
# 
# ####Find the best predictors####
# 
# # unzip, and load tools
# library(maptools); library(gpclib); library(sp);gpclibPermit()
# # read data into R
# shapefile <- readShapeSpatial('~/current/pharmacy_deserts/data/tl_2013_us_zcta510.shp',
#                               proj4string = CRS("+proj=longlat +datum=WGS84"))
# # convert to a data.frame for use with ggplot2/ggmap and plot
# zip_shapes_all <- as.data.table(fortify(shapefile, region = "ZCTA5CE10"))
# #   zip_shapes = zip_shapes_all[grepl("^97", id),]
# zip_shapes_all[, id_int := as.integer(id)]
# 
# us_map = get_map(location = "united states", maptype="roadmap", zoom=4, color="bw")
# # oregon_map = get_map(location = "oregon", maptype="roadmap", zoom=6, color="bw")
# # willamette_map = get_map(location = "willamette valley, oregon", maptype="roadmap", zoom=7, color="bw")
# #   portland_map = get_map(location = "portland, or", maptype="roadmap", zoom=10, color="bw")
# 
# 
# 
# 
# # pdf(file="~/current/pharmacy_deserts/predicted_effects_map.pdf", width = 18, height = 10)
# 
# for (outcome in c("log_total_pills_dispensed_ym_z","log_total_pats_ym_z","log_total_claims_ym_z",
#                   "log_total_new_pats_ym_z_plus_1"
# )) {
#   
#   # outcome = "log_total_pills_dispensed_ym_z"
#   # outcome = "log_total_pats_ym_z"
#   
#   if (grepl("total_pills_dispensed",outcome) & !(grepl("ring",outcome))) {
#     title = "Pills dispensed"
#   } else if (grepl("total_pats",outcome) & !(grepl("ring",outcome))) {
#     title = "Unique patients"
#   } else if (grepl("total_claims",outcome) & !(grepl("ring",outcome)) & !(grepl("inzip",outcome))){
#     title = "Total claims"
#   } else if (grepl("ring_mean_total_pills_dispensed",outcome)) {
#     title = "Ring Pills dispensed"
#   } else if (grepl("ring_mean_total_pats",outcome)) {
#     title = "Ring Unique patients"
#   } else if (grepl("ring_mean_total_claims",outcome)){
#     title = "Ring Total claims"
#   } else if (grepl("total_money",outcome)){
#     title = "Total money"
#   } else if (grepl("avg_age",outcome)) {
#     title = "Average age"
#   } else if (grepl("avg_copay",outcome)){
#     title = "Average copay"
#   } else if (grepl("total_new_pats",outcome)){
#     title = "New patients"
#   } else if (grepl("mean_mpr",outcome)){
#     title = "Med. Possesion Ratio"
#   } else if (grepl("share_medicaid",outcome)){
#     title = "Medicaid Share of Patients"
#   } else if (grepl("share_claims_inzip",outcome)) {
#     title = "Share of in-zip claims"
#   } else if (grepl("total_claims_inzip",outcome)) {
#     title = "Total in-zip claims"
#   } else if (grepl("total_unique_zips",outcome)) {
#     title = "Unique Pharmacy Zips"
#   } else if (grepl("log_avg_distance",outcome)) {
#     title = "Log Avg. Distance"
#   } else if (grepl("avg_distance",outcome)) {
#     title = "Avg. Distance"
#   }
#   
#   
#   predict_data_nc = paste("prediction_",outcome,"_data_nc",sep="")
#   predict_data_c = paste("prediction_",outcome,"_data_c",sep="")
#   
#   get(predict_data_nc)[, unif := NULL]
#   get(predict_data_nc)[, training := NULL]
#   
#   numeric_names = names(get(predict_data_nc))[!(names(get(predict_data_nc)) %in% non_numeric_names)]
#   
#   
#   
#   
#   
#   #use the built in CV tool for the glmnet####
#   #create a matrix of the predictors
#   predictor_matrix = as.matrix(get(predict_data_nc)[!is.na(ymin),.SD, .SDcols = numeric_names]) #these have coefficients
#   
#   #the vector of betas
#   betas = get(predict_data_nc)[!is.na(ymin),]$coef
#   
#   crossval <-  cv.glmnet(x = predictor_matrix, y = betas)
#   plot(crossval)
#   penalty <- crossval$lambda.min #optimal lambda
#   penalty #minimal shrinkage
#   
#   fit1 <-glmnet(x = predictor_matrix, y = betas, alpha = 1, lambda =  penalty*.1) #estimate the model with that
#   #   summary(fit1)
#   coefs = as.matrix(coef(fit1))
#   coefs_names = row.names(coefs)
#   coefs = as.data.table(cbind(coefs, coefs_names))
#   coefs[, coef := as.numeric(s0)]
#   setkey(coefs, coef)
#   #   View(coefs[coef != 0])
#   
#   #   coefs[, adj_coef := NA]
#   #   coefs[coefs_names == "Gini.Index" | coefs_names == "Average.Household.Size", adj_coef := coef / 100]
#   # coefs[is.na(adj_coef), adj_coef := coef]
#   
#   coefs[nchar(coefs_names) > 60, coefs_names := substr(coefs_names, 1, 60)]
#   
#   coefs[coef > 0 & coefs_names != "(Intercept)" , min_coef := min(coef)]
#   coefs[coef != 0 & coefs_names != "(Intercept)" , rank := seq_len(.N)]
#   
#   smallest_coef = min(coefs[coefs_names != "(Intercept)"]$coef)
#   
#   coefs[coefs_names != "(Intercept)", sds := (coef ) / sd(coef, na.rm=T)]
#   coefs[, adj_coef := ifelse(sds < - .1, coef / 10000,
#                              ifelse(sds > .1, coef / 10000,
#                                     ifelse(sds < -.01, coef / 100,
#                                            ifelse(sds > .01, coef / 100,
#                                                   coef))))]
#   coefs[, color := ifelse(sds < - .1, "Red",
#                           ifelse(sds > .1, "Red",
#                                  ifelse(sds < -.01, "Blue",
#                                         ifelse(sds > .01, "Blue",
#                                                "Black"))))]
#   
#   sig_predictors = ggplot(data = coefs[coef != 0 & coefs_names != "(Intercept)" 
#                                        #                                        & coef < .0002 & coef > -.002 
#                                        ]) +
#     geom_point(aes(x=reorder(coefs_names,coef), y=adj_coef, color = color, shape = color), size = 5) +
#     coord_flip() +
#     ggtitle(title) +
#     theme_bw() + 
#     ylab("Coefficient") + xlab("") +
#     theme(text = element_text(size = 20),
#           axis.text.x = element_text(size = 10)) +
#     geom_point(data = NULL,
#                aes(x = coefs[min_coef == coef]$rank - .5, 
#                    y = 0),
#                color = "Red", shape = 3, size = 10) +
#     scale_color_manual(values = c("Black","Blue","Red")) + 
#     theme(legend.position = "none")
#   ggsave(sig_predictors, file = paste("~/current/pharmacy_deserts/sig_predictors_",outcome,".png",sep=""), 
#          width = 15, height = 10)
#   print(sig_predictors)
#   
#   sig_pred = sort(coefs[coef != 0 & coefs_names != "(Intercept)" & coef < .1 & coef > -.2 ]$coefs_names)
#   write.csv(sig_pred, file = paste("~/current/pharmacy_deserts/",outcome,"_sig_pred.csv",sep=""))
#   
#   
#   #create a matrix of predictors from the predicting data set
#   newx = as.matrix(prediction_log_total_claims_ym_z_data_nc[is.na(ymin),.SD, .SDcols = numeric_names])
#   
#   #apply the lasso to the prediction data
#   predicted_coefs = predict(fit1, newx=newx)
#   
#   #merge on the zip codes 
#   newx_zips = as.matrix(prediction_log_total_claims_ym_z_data_nc[is.na(ymin),.SD, .SDcols = c("zip")])
#   prediction_data = as.data.table(cbind(newx_zips, predicted_coefs))
#   prediction_data
#   mean(as.numeric(as.character(prediction_data[s0 > -.25 & s0 < .25]$s0)), na.rm=T)
#   mean(as.numeric(as.character(prediction_data$s0)), na.rm=T)
#   median(as.numeric(as.character(prediction_data[s0 > -.25 & s0 < .25]$s0)), na.rm=T)
#   median(as.numeric(as.character(prediction_data$s0)), na.rm=T)
#   sd(as.numeric(as.character(prediction_data$s0)), na.rm=T)
#   nrow(prediction_data[!is.na(s0)])
#   min(prediction_data$s0)
#   max(prediction_data$s0)
#   qplot(x=s0, data = prediction_data[s0 < mean(as.numeric(as.character(prediction_data$s0)), na.rm=T) + 
#                                        1*sd(as.numeric(as.character(prediction_data$s0)), na.rm=T)
#                                      & s0 > mean(as.numeric(as.character(prediction_data$s0)), na.rm=T) - 
#                                        1*sd(as.numeric(as.character(prediction_data$s0)), na.rm=T)], geom="density")
#   qplot(x=s0, data = prediction_data, geom="density")
#   setkey(prediction_data, s0)
#   
#   prediction_effects_data = data.table(mean = mean(as.numeric(as.character(prediction_data$s0)), na.rm=T),
#                                        limited_mean = mean(as.numeric(as.character(prediction_data[s0 < mean(as.numeric(as.character(prediction_data$s0)), na.rm=T) + 
#                                                                                                      1*sd(as.numeric(as.character(prediction_data$s0)), na.rm=T)
#                                                                                                    & s0 > mean(as.numeric(as.character(prediction_data$s0)), na.rm=T) - 
#                                                                                                      1*sd(as.numeric(as.character(prediction_data$s0)), na.rm=T)]$s0)), na.rm=T),
#                                        sd =   sd(as.numeric(as.character(prediction_data$s0)), na.rm=T),
#                                        limited_sd =   sd(as.numeric(as.character(prediction_data[s0 < mean(as.numeric(as.character(prediction_data$s0)), na.rm=T) + 
#                                                                                                    1*sd(as.numeric(as.character(prediction_data$s0)), na.rm=T)
#                                                                                                  & s0 > mean(as.numeric(as.character(prediction_data$s0)), na.rm=T) - 
#                                                                                                    1*sd(as.numeric(as.character(prediction_data$s0)), na.rm=T)]$s0)), na.rm=T),
#                                        median = median(as.numeric(as.character(prediction_data$s0)), na.rm=T),
#                                        limited_median = median(as.numeric(as.character(prediction_data[s0 < mean(as.numeric(as.character(prediction_data$s0)), na.rm=T) + 
#                                                                                                          1*sd(as.numeric(as.character(prediction_data$s0)), na.rm=T)
#                                                                                                        & s0 > mean(as.numeric(as.character(prediction_data$s0)), na.rm=T) - 
#                                                                                                          1*sd(as.numeric(as.character(prediction_data$s0)), na.rm=T)]$s0)), na.rm=T),
#                                        min = min(prediction_data$s0),
#                                        limited_min = min(prediction_data[s0 < mean(as.numeric(as.character(prediction_data$s0)), na.rm=T) + 
#                                                                            1*sd(as.numeric(as.character(prediction_data$s0)), na.rm=T)
#                                                                          & s0 > mean(as.numeric(as.character(prediction_data$s0)), na.rm=T) - 
#                                                                            1*sd(as.numeric(as.character(prediction_data$s0)), na.rm=T)]$s0),
#                                        max = max(prediction_data$s0),
#                                        limited_max = max(prediction_data[s0 < mean(as.numeric(as.character(prediction_data$s0)), na.rm=T) + 
#                                                                            1*sd(as.numeric(as.character(prediction_data$s0)), na.rm=T)
#                                                                          & s0 > mean(as.numeric(as.character(prediction_data$s0)), na.rm=T) - 
#                                                                            1*sd(as.numeric(as.character(prediction_data$s0)), na.rm=T)]$s0),
#                                        outcome = eval(outcome))
#   prediction_effects_data
#   write.csv(prediction_effects_data, file = paste("~/current/pharmacy_deserts/",outcome,"_prediction_effects.csv",sep=""))
#   
#   ####Use the covariates at the national level to predict the effect in each zip code####
#   setkey(prediction_data, zip)
#   
#   
#   
#   
#   #merge the zip map data and the zip shapes
#   setkey(zip_shapes_all, id_int)
#   
#   setkey(prediction_data, zip)
#   
#   zip_shapes_all[, predicted_effect := NULL]
#   zip_shapes_all[prediction_data, predicted_effect := s0]
#   
#   zip_shapes_all[,predicted_effect := as.numeric(predicted_effect)] 
#   
#   zip_shapes_all[, adj_predicted_effect := ifelse(predicted_effect > .18, .18,
#                                                   ifelse(predicted_effect < -.1, -.1,
#                                                          predicted_effect))]
#   
#   
#   map = ggmap(us_map) +
#     geom_polygon(data = zip_shapes_all[!is.na(predicted_effect)],
#                  aes_string(x="long", y="lat", group = "group", fill = "adj_predicted_effect"),
#                  alpha = .75, color = NA) +
#     coord_map() + 
#     scale_fill_distiller(palette = "YlOrRd",
#                          limits = c(-.1, .18),
#                          breaks = c(seq(-.1, .18,.04)), 
#                          #pretty_breaks(n=12),
#                          #                                                 labels = c("<1",seq(3,23,2), ">25"),
#                          direction = 1) +
#     labs(fill = "Predicted Effect") +
#     theme_nothing(legend = TRUE) +
#     guides(fill = guide_legend(reverse = TRUE, 
#                                override.aes = list(alpha = 1),
#                                title.position = "top",
#                                title.hjust =0.5,
#                                nrow = 1)) +
#     theme(text = element_text(size = 20, family = "serif")) +
#     theme(legend.position = "bottom") +
#     ggtitle(paste(title,"\n(no controls)",sep=""))
#   #   print(map)
#   ggsave(map, file = paste("~/current/pharmacy_deserts/national_map_",outcome,".png",sep=""), 
#          width = 15, height = 10)
#   
#   
#   
#   
#   
#   
#   
#   # #CONTROLS
#   # numeric_names = names(get(predict_data_c))[!(names(get(predict_data_c)) %in% non_numeric_names)]
#   # 
#   # 
#   # 
#   # #randomly divide 30 observations to training and predicting sets to see how well it does####
#   # set.seed(1)
#   # get(predict_data_c)[, unif := runif(nrow(get(predict_data_c)[!is.na(ymin)]))]
#   # get(predict_data_c)[!is.na(ymin), training := unif <= .80]
#   # 
#   # #create a matrix of the predictors
#   # predictor_matrix = as.matrix(get(predict_data_c)[training == 1,.SD, .SDcols = numeric_names])
#   # 
#   # #the vector of betas
#   # betas = get(predict_data_c)[training==1,]$coef
#   # 
#   # #use lasso
#   # glmnet_train = glmnet(predictor_matrix, betas, alpha = 1)
#   # 
#   # #create a matrix of predictors from the predicting data set
#   # newx = as.matrix(get(predict_data_c)[training == 0,.SD, .SDcols = numeric_names])
#   # 
#   # #apply the lasso to the prediction data
#   # predicted_coefs = predict(glmnet_train, newx=newx)
#   # 
#   # #merge on the zip codes and the true betas
#   # newx_zips = as.matrix(get(predict_data_c)[training==0,.SD, .SDcols = c("zip","coef")])
#   # prediction_data = as.data.table(cbind(newx_zips, predicted_coefs))
#   # prediction_data
#   # 
#   # #try to get a sense of what the optimal lambda parameter should be at the national level
#   # for (X in seq(0,99)) {
#   #   diff = paste("diff_",X,sep="")
#   #   s = paste("s",X,sep="")
#   #   diff_sq = paste("diff_",X,"_sq",sep="")
#   #   sum = paste("sum_",X,sep="")
#   #   
#   #   prediction_data[, (diff) := as.numeric(coef) - as.numeric(get(s))]
#   #   prediction_data[, (diff_sq) := get(diff)^2]
#   #   prediction_data[, (sum) := sum(get(diff_sq))]
#   #   
#   #   
#   # }
#   # 
#   # 
#   # sum_names = names(prediction_data)[grepl("^sum",names(prediction_data))]
#   # sums = prediction_data[,.SD, .SDcols = sum_names][1]
#   # sums_long = melt(sums)
#   # sums_long[, s := gsub("sum_","",variable)]
#   # qplot(data=sums_long, x = as.numeric(as.character(s)), y=value)
#   # sums_long[as.numeric(s) > 0, min_val := min(value)]
#   # min_s = sums_long[value == min_val ]$s #I think this is the s that does the best on this little data set
#   # #doesn't do very well..?
#   # min_s
#   # splot = paste("s",min_s,sep="")
#   # prediction_data[,splot := NULL]
#   # prediction_data[, splot := get(splot)]
#   # qplot(data =prediction_data, x=as.numeric(as.character(coef)), y=as.numeric(as.character(splot)))
#   # 
#   # 
#   # 
#   # #use the built in CV tool for the glmnet####
#   # #create a matrix of the predictors
#   # predictor_matrix = as.matrix(get(predict_data_c)[!is.na(ymin),.SD, .SDcols = numeric_names]) #these have coefficients
#   # 
#   # #the vector of betas
#   # betas = prediction_log_total_claims_ym_z_data_c[!is.na(ymin),]$coef
#   # 
#   # crossval <-  cv.glmnet(x = predictor_matrix, y = betas)
#   # plot(crossval)
#   # penalty <- crossval$lambda.min #optimal lambda
#   # penalty #minimal shrinkage
#   # fit1 <-glmnet(x = predictor_matrix, y = betas, alpha = 1, lambda =  penalty*.75) #estimate the model with that
#   # 
#   # #create a matrix of predictors from the predicting data set
#   # newx = as.matrix(prediction_log_total_claims_ym_z_data_c[is.na(ymin),.SD, .SDcols = numeric_names])
#   # 
#   # #apply the lasso to the prediction data
#   # predicted_coefs = predict(fit1, newx=newx)
#   # 
#   # #merge on the zip codes 
#   # newx_zips = as.matrix(prediction_log_total_claims_ym_z_data_c[is.na(ymin),.SD, .SDcols = c("zip")])
#   # prediction_data = as.data.table(cbind(newx_zips, predicted_coefs))
#   # prediction_data
#   # mean(as.numeric(as.character(prediction_data$s0)), na.rm=T)
#   # sd(as.numeric(as.character(prediction_data$s0)), na.rm=T)
#   # nrow(prediction_data[!is.na(s0)])
#   # min(prediction_data$s0)
#   # max(prediction_data$s0)
#   # qplot(x=s0, data = prediction_data, geom="density")
#   # 
#   # 
#   # ####Use the covariates at the national level to predict the effect in each zip code####
#   # setkey(prediction_data, zip)
#   # 
#   # 
#   # # us_map = get_map(location = "united states", maptype="roadmap", zoom=3, color="bw")
#   # # oregon_map = get_map(location = "oregon", maptype="roadmap", zoom=6, color="bw")
#   # # willamette_map = get_map(location = "willamette valley, oregon", maptype="roadmap", zoom=7, color="bw")
#   # #   portland_map = get_map(location = "portland, or", maptype="roadmap", zoom=10, color="bw")
#   # #   eugene_map = get_map(location = "eugene, or", maptype="roadmap", zoom=9, color="bw")
#   # #   bend_map = get_map(location = "bend, or", maptype="roadmap", zoom=7, color="bw")
#   # #   salem_map = get_map(location = "salem, or", maptype="roadmap", zoom=9, color="bw")
#   # #   albany_map = get_map(location = "albany, or", maptype="roadmap", zoom=9, color="bw")
#   # 
#   # 
#   # #merge the zip map data and the zip shapes
#   # setkey(zip_shapes_all, id_int)
#   # 
#   # setkey(prediction_data, zip)
#   # 
#   # zip_shapes_all[, predicted_effect := NULL]
#   # zip_shapes_all[prediction_data, predicted_effect := s0]
#   # 
#   # map = ggmap(us_map) +
#   #   geom_polygon(data = zip_shapes_all[!is.na(predicted_effect)],
#   #                aes_string(x="long", y="lat", group = "group", fill = "as.numeric(predicted_effect)"),
#   #                alpha = .75, color = NA) +
#   #   coord_map() + 
#   #   scale_fill_distiller(palette = "YlOrRd",
#   #                                               limits = c(-.01, .11),
#   #                        breaks = c(seq(-.01, .11,.02)), 
#   #                        direction = 1) +
#   #   labs(fill = "Predicted Effect") +
#   #   theme_nothing(legend = TRUE) +
#   #   guides(fill = guide_legend(reverse = TRUE, 
#   #                              override.aes = list(alpha = 1),
#   #                              title.position = "top",
#   #                              title.hjust =0.5,
#   #                              nrow = 1)) +
#   #   theme(text = element_text(size = 20, family = "serif")) +
#   #   theme(legend.position = "bottom") +
#   #   ggtitle(paste(title,"\n(with controls)",sep=""))
#   # print(map)
#   #   
#   #   
#   
#   
# }
# 
# ####can I make the figure with average age in each zip?####
# acs_zbp[, tot_pop_over_65_rat := Total.Population..65.to.74.Years + Total.Population..75.to.84.Years + Total.Population..85.Years.and.Over / Total.Population.]
# 
# setkey(zip_shapes_all, id_int)
# setkey(acs_zbp, zip)
# 
# zip_shapes_all[, tot_pop_over_65_rat := NULL]
# zip_shapes_all[acs_zbp, tot_pop_over_65_rat := tot_pop_over_65_rat]
# 
# zip_shapes_all[,tot_pop_over_65_rat := as.numeric(tot_pop_over_65_rat)] 
# 
# # zip_shapes_all[, adj_tot_pop_over_65 := ifelse(tot_pop_over_65 > .18, .18,
# #                                                ifelse(tot_pop_over_65 < -.1, -.1,
# #                                                       tot_pop_over_65))]
# 
# 
# map = ggmap(us_map) +
#   geom_polygon(data = zip_shapes_all[!is.na(tot_pop_over_65_rat)],
#                aes_string(x="long", y="lat", group = "group", fill = "tot_pop_over_65_rat"),
#                alpha = .75, color = NA) +
#   coord_map() + 
#   scale_fill_distiller(palette = "YlOrRd",
#                        #                        limits = c(-.1, .18),
#                        #                        breaks = c(seq(-.1, .18,.04)), 
#                        #pretty_breaks(n=12),
#                        #                                                 labels = c("<1",seq(3,23,2), ">25"),
#                        direction = 1) +
#   labs(fill = "Predicted Effect") +
#   theme_nothing(legend = TRUE) +
#   guides(fill = guide_legend(reverse = TRUE, 
#                              override.aes = list(alpha = 1),
#                              title.position = "top",
#                              title.hjust =0.5,
#                              nrow = 1)) +
#   theme(text = element_text(size = 20, family = "serif")) +
#   theme(legend.position = "bottom") +
#   ggtitle(paste("Ratio of Total Population over 65","",sep=""))
# #   print(map)
# ggsave(map, file = paste("~/current/pharmacy_deserts/national_map_","tot_pop_over_65",".png",sep=""), 
#        width = 15, height = 10)
# 
# 
# # dev.off()
# 
