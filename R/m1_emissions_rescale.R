#' m1_emissions_rescale
#'
#'
#' Re-scales emissions from GCAM regions to TM5-FASST regions. The function also completes some additional transformation required such as pollutant transformation (e.g. OC to POM) or unit changes (Tg to Kg).
#' @keywords module_1, re-scale emissions
#' @return Emissions per TM5-FASST region for all the selected years (all_years)
#' @param db_path Path to the GCAM database
#' @param query_path Path to the query file
#' @param db_name Name of the GCAM database
#' @param prj_name Name of the rgcam project. This can be an existing project, or, if not, this will be the name
#' @param scen_name Name of the GCAM scenario to be processed
#' @param queries Name of the GCAM query file. The file by default includes the queries required to run rfasst
#' @param saveOutput Writes the emission files. By default=T
#' @param map Produce the maps. By default=F
#' @importFrom magrittr %>%
#' @export

m1_emissions_rescale<-function(db_path,query_path,db_name,prj_name,scen_name,queries,prj_path=NULL,read_prj=F,saveOutput=T,map=F){

  # Ancillary Functions
  `%!in%` = Negate(`%in%`)

  # Transform the loaded TM5-FASST regions
  FASST_reg<-fasst_reg %>%
    dplyr::rename(`ISO 3`=subRegionAlt,
           `FASST Region`=fasst_region)

  if(read_prj == FALSE){
    # Load the rgcam project:
    conn <- rgcam::localDBConn(db_path, db_name,migabble = FALSE)
    prj <- rgcam::addScenario(conn,prj_name,scen_name,paste0(query_path,"/",queries),clobber=F)

    rgcam::listScenarios(prj)
    QUERY_LIST <- c(rgcam::listQueries(prj, c(scen_name)))

  } else {
    qry <- c("nonCO2 emissions by region.proj",
             "International Aviation emissions.proj",
             "International Shipping emissions.proj")
    prj <- rgcam::loadProject(paste(prj_path, "nonCO2 emissions by sector (incl resources).proj", sep = '/'))
    proj_list <- lapply(qry, function(qry){
      prj <- rgcam::loadProject(paste(prj_path, qry, sep = '/'))
    })

    prj <- rgcam::mergeProjects(prj, proj_list)
    for (i in 1:length(prj[[1]])){
      prj[[1]][[i]] <- prj[[1]][[i]] %>%
        dplyr::mutate(scenario = experiment) %>%
        dplyr::select(-experiment, -old_scen_name)
    }
  }

  # Generate the adjusted emission database +air and ship emissions
  # Emission data (scen)
  scen<-rgcam::getQuery(prj,"nonCO2 emissions by sector (incl resources)") %>%
    dplyr::arrange(ghg) %>%
    dplyr::filter(ghg %in% unique(levels(as.factor(my_pol$My_Pollutant))))

  # Transform OC to POM
  pom<-scen %>%
    dplyr::filter(ghg %in% c("OC","OC_AWB")) %>%
    dplyr::mutate(value=dplyr::if_else(ghg=="OC_AWB",value*CONV_OCawb_POM,value*CONV_OC_POM)) %>%
    dplyr::mutate(ghg="POM")

  # Transform MTC to MTCO2
  co2<-scen %>%
    dplyr::filter( ghg=="CO2") %>%
    dplyr::mutate(value=value*MTC_MTCO2)

  # NMVOC are not reported by sector, so it needs to be treated independently
  nmvoc<-rgcam::getQuery(prj,"nonCO2 emissions by region") %>%
    dplyr::filter(grepl("NMVOC",ghg)) %>%
    dplyr::mutate(ghg = "NMVOC",
                  sector = "all")


  scen<-scen %>%
    dplyr::filter(ghg %!in% c("OC","OC_AWB","CO2")) %>%
    dplyr::bind_rows(pom,co2,nmvoc) %>%
    # transform value to kg
    dplyr::mutate(value=value*TG_KG) %>%
    dplyr::group_by(scenario,region,Units,ghg,year) %>%
    dplyr::summarise(value=sum(value))%>%
    dplyr::ungroup()%>%
    # dplyr::select(-scenario,-Units) %>% # comment by MZ
    dplyr::select(-Units) %>%
    dplyr::filter(year %in% all_years) %>%
    dplyr::rename(`GCAM Region` = region,
                  Pollutant = ghg,
                  Scenario = scenario) %>% # rename scenario by MZ
    dplyr::mutate(Pollutant = as.factor(Pollutant),
                  year = as.factor(year),
                  `GCAM Region` = as.factor(`GCAM Region`))

  # Aviation:
  air<-rgcam::getQuery(prj,"International Aviation emissions") %>%
    dplyr::mutate(ghg=dplyr::if_else(grepl("SO2",ghg),"SO2",as.character(ghg))) %>%
    dplyr::group_by(scenario,ghg,year) %>% # add scenario by MZ
    dplyr::summarise(value=sum(value)) %>%
    dplyr::ungroup() %>%
    # transform value to kg
    dplyr::mutate(value=value*TG_KG)  %>%
    dplyr::filter(year %in% all_years) %>%
    dplyr::mutate(ghg=as.factor(ghg)) %>%
    dplyr::rename(Pollutant = ghg,
                  Year = year,
                  Scenario = scenario) %>% # rename scenario by MZ
    dplyr::mutate(Pollutant = dplyr::if_else(Pollutant == "OC", "POM", as.character(Pollutant)),
                  Pollutant = as.factor(Pollutant)) %>%
    tidyr::spread(Pollutant,value) %>%
    dplyr::mutate(CH4=rep(0), N2O=rep(0), NH3=rep(0)) %>%
    dplyr::select(Scenario,Year,BC,CH4,CO2,CO,N2O,NH3,NOx,POM,SO2,NMVOC) # add scenario by MZ

  # Shipping:
  ship<-rgcam::getQuery(prj,"International Shipping emissions") %>%
    dplyr::mutate(ghg=dplyr::if_else(grepl("SO2",ghg),"SO2",as.character(ghg))) %>%
    dplyr::group_by(scenario,ghg,year) %>% # add scenario by MZ
    dplyr::summarise(value=sum(value)) %>%
    dplyr::ungroup() %>%
    # transform value to kg
    dplyr::mutate(value=value*TG_KG) %>%
    dplyr::filter(year %in% all_years) %>%
    dplyr::mutate(ghg=as.factor(ghg)) %>%
    dplyr::rename(Pollutant = ghg,
                  Year = year,
                  Scenario = scenario) %>% # rename scenario by MZ
    dplyr::mutate(Pollutant=dplyr::if_else(Pollutant=="OC","POM",as.character(Pollutant)),
           Pollutant=as.factor(Pollutant))%>%
    tidyr::spread(Pollutant,value) %>%
    dplyr::mutate(CH4=rep(0), N2O=rep(0), NH3=rep(0)) %>%
    dplyr::select(Scenario,Year,BC,CH4,CO2,CO,N2O,NH3,NOx,POM,SO2,NMVOC) # add scenario by MZ

  final_df_wide<-dplyr::inner_join(Percen %>% dplyr::mutate(`GCAM Region` = as.factor(`GCAM Region`)),
                                   scen,
                                   by = c('GCAM Region', 'Pollutant', 'year')) %>% # Change from left_join to inner_join by MZ
    dplyr::mutate(NewValue=Percentage*value) %>%
    dplyr::left_join(FASST_reg, by= 'ISO 3') %>%
    dplyr::group_by(Scenario, `FASST Region`,year,Pollutant) %>% # add scenario by MZ
    dplyr::summarise(NewValue=sum(NewValue)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(Region = `FASST Region`,
                  Year = year,
                  value = NewValue) %>%
    tidyr::spread(Pollutant,value)


  if(length(levels(as.factor(FASST_reg$`FASST Region`))) != length(levels(as.factor(final_df_wide$Region)))){
    print("ISO 3 countries do not match")
  }


  #Write the data
  #Create the function to write the csv files

  write_data<- function(year,save=saveOutput){

    a<-final_df_wide %>%
      dplyr::filter(Year==year) %>%
      dplyr::select(-Year) %>%
      dplyr::rename(COUNTRY = Region,
                    NOX = NOx,
                    OM = POM,
                    VOC = NMVOC) %>%
      dplyr::select(Scenario, COUNTRY,BC,CH4,CO2,CO,N2O,NH3,NOX,OM,SO2,VOC) # add scenario by MZ


    # Shipping and aviation
    vec_air<-air %>%
      dplyr::filter(Year==year) %>%
      dplyr::mutate(COUNTRY="AIR") %>%
      dplyr::select(Scenario, COUNTRY,BC,CH4,CO2,CO,N2O,NH3,NOx,POM,SO2,NMVOC) %>% # add scenario by MZ
      dplyr::rename(NOX= NOx, OM=POM, VOC=NMVOC)

    vec_ship<-ship %>%
      dplyr::filter(Year==year) %>%
      dplyr::mutate(COUNTRY="SHIP") %>%
      dplyr::select(Scenario, COUNTRY,BC,CH4,CO2,CO,N2O,NH3,NOx,POM,SO2,NMVOC) %>% # add scenario by MZ
      dplyr::rename(NOX= NOx, OM=POM, VOC=NMVOC)


    # Add RUE:
    rus<- a %>%
      dplyr::filter(COUNTRY=="RUS") %>%
      tidyr::gather(Pollutant,value, -COUNTRY, -Scenario) %>% # add ignore Scenario by MZ
      dplyr::mutate(COUNTRY = as.character(COUNTRY),
                    Pollutant = as.character(Pollutant))

    rue<- a %>%
      dplyr::filter(COUNTRY=="RUS") %>%
      tidyr::gather(Pollutant,value, -COUNTRY, -Scenario) %>% # add ignore Scenario by MZ
      dplyr::mutate(COUNTRY="RUE") %>%
      dplyr::mutate(COUNTRY = as.character(COUNTRY),
                    Pollutant = as.character(Pollutant))


    rus_fin<-dplyr::bind_rows(rus,rue) %>%
      dplyr::mutate(COUNTRY = as.factor(COUNTRY),
                    Pollutant = as.factor(Pollutant)) %>%
      dplyr::left_join(adj_rus %>% dplyr::mutate(COUNTRY=as.factor(COUNTRY)),by=c("COUNTRY","Pollutant")) %>%
      dplyr::mutate(value=value*perc) %>%
      dplyr::select(-perc) %>%
      tidyr::spread(Pollutant,value)


    # Add aviation, shipping and Russia
    a<- a %>%
      dplyr::filter(COUNTRY != "RUS") %>%
      dplyr::bind_rows(vec_air,vec_ship,rus_fin %>% dplyr::mutate(COUNTRY=as.character(COUNTRY)))



    # Total
    tot<-a %>%
      tidyr::gather(Pollutant,value,-COUNTRY, -Scenario) %>% # add ignore Scenario by MZ
      dplyr::group_by(Scenario, Pollutant) %>%
      dplyr::summarise(value=sum(value)) %>%
      dplyr::ungroup() %>%
      tidyr::spread(Pollutant,value)%>%
      dplyr::mutate(COUNTRY="*TOTAL*") %>%
      dplyr::select(Scenario, COUNTRY,BC,CH4,CO2,CO,N2O,NH3,NOX,OM,SO2,VOC)

    # Add total and PM2.5
    a<- a %>%
      dplyr::bind_rows(tot) %>%
      dplyr::mutate(PM25=rep(-99900))

    # a<-a[c(59,55,56,1:45,57,58,46:54),]

    if(save==T){
    # write.csv(a,file = paste0("output/","m1/",scen_name,'_',year,'.csv'),row.names = FALSE, quote = FALSE)
      write.csv(a,file = paste0("output/","m1/",'AllScenarios_',year,'.csv'),row.names = FALSE, quote = FALSE) # add by MZ
    }

    em<- a %>%
      dplyr::mutate(year=year)%>%
      tidyr::gather(pollutant,value,-COUNTRY,-year, -Scenario) %>% # add ignore Scenario by MZ
      dplyr::rename(region=COUNTRY) %>%
      dplyr::filter(region != "*TOTAL*")

    return(em)

  }

  # If map=T, it produces a map with the calculated outcomes

  if(map==T){
    final_df_wide.map<-final_df_wide %>%
      tidyr::gather(pollutant,value,-Region,-Year, -Scenario) %>%
      dplyr::filter(pollutant %in% map_pol,
                    Scenario %in% scen_name) %>%
      dplyr::rename(subRegion=Region)%>%
      dplyr::filter(subRegion != "RUE") %>%
      dplyr::select(subRegion,Year,pollutant,value) %>%
      dplyr::rename(class=pollutant,
                    year=Year) %>%
      dplyr::mutate(year=as.numeric(as.character(year)),
                    value=value*1E-6,
                    units="Gg")

    rmap::map(data = final_df_wide.map,
              shape = fasstSubset,
              folder ="output/maps/m1/maps_em",
              mapTitleOn = F,
              facetCols = 3,
              legendOutsideSingle = T,
              legendPosition = c("RIGHT","bottom"),
              legendTextSizeO = 0.5,
              legendTitleSizeO=0.7,
              background  = T)

  }


  # Apply the function to all of the years.
  # This can be modified and write the data just for the desired years
  invisible(lapply(all_years,write_data))

  #----------------------------------------------------------------------
  #----------------------------------------------------------------------

}



