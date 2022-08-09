#' @author Hoda Abdel Magid
#' @author Sam Jaros
#' @name calculate_ice
#' @import dplyr
#' @import tidycensus

#TODO: parallelize
#TODO: allow for dynamic creation of ICE vars

# Create a wrapper function for calculate_ice
create_ice = function(codes, codes_type, api_key=Sys.getenv("CENSUS_API_KEY"), cache_acs=TRUE){
  ## Lines for testing ##
  api_key=Sys.getenv("CENSUS_API_KEY"); cache_acs=TRUE
  # Pick which line to run based on testing
  #codes = c("08","19","33"); codes_type="state"
  codes = ex_counties$GEOID; codes_type="county"
  ##/ Lines for testing /##
  
  require(dplyr)
  require(tidycensus)
  require(tidyr)
  
  suppressMessages(census_api_key(api_key))
  #TODO: dynamically calculate the needed tables
  tables = c("B15003", "B25003", "B19001", "B03002", "B19001A", "B19001B", "B19001H")
  
  #TODO: check for valid codes
  if (codes_type == "state"){
    big_table = data.frame(GEOID=c(), variable=c(), estimate=c())
    for (table in tables){
      new_data = get_acs(geography=codes_type, table=table, cache_table=cache_acs, state=unique(codes)) %>% 
        select(GEOID, variable, estimate)
      big_table = rbind(big_table, new_data)
    }
    
  } else if (codes_type == "county"){
    states = unique(sapply(as.character(codes), substr, start=1, stop=2))
    big_table = data.frame(GEOID=c(), variable=c(), estimate=c())
    for (state in states){
      new_data = data.frame(GEOID=c(), variable=c(), estimate=c())
      for (table in tables) {
        data_pull = get_acs(geography=codes_type, table=table, cache_table=cache_acs, state=state) %>% 
          select(GEOID, variable, estimate)
        new_data = rbind(new_data, data_pull)
      }
      big_table = rbind(big_table, 
                        new_data %>% right_join(data.frame(GEOID=unique(codes[startsWith(codes, state)]))))
    }
    
  } else if (codes_type == "zcta"){
    #TODO: need to know which state we're in
    big_table = data.frame(GEOID=c(), variable=c(), estimate=c())
    for (table in tables){
      new_data = get_acs(geography="zcta", zcta=codes, table=table) %>% select(GEOID, variable, estimate)
      big_table = rbind(big_table, new_data)
    }
    
  } else if (codes_type == "tract"){
    states = unique(sapply(as.character(codes), substr, start=1, stop=2))
    big_table = data.frame(GEOID=c(), variable=c(), estimate=c())
    for (table in tables){
      new_data = get_acs(geography=codes_type, table=table) %>% select(GEOID, variable, estimate)
      big_table = rbind(big_table, new_data)
    }
    
  } else {
    stop(paste0("Invalid code type: ", codes_type, ". Must be one of: 'county', 'state', 'tract', or 'zcta'"))
  }
  
  ice_vars = big_table %>%
    # Flip long to wide
    pivot_wider(id_cols=GEOID, names_from=variable, values_from=estimate) %>%
    # Calculate ICE variables in the format
    # name = ((privileged) - 
    #         (deprived)) /
    #        (total)
    mutate(GEOID=GEOID,
           ICEedu=((B15003_022) -
                     (B15003_016+B15003_015+B15003_014+B15003_013+B15003_012+B15003_011+B15003_010+B15003_009+B15003_008+B15003_007+B15003_006+B15003_005+B15003_004+B15003_003+B15003_002)) /
             (B15003_001),
           ICEhome=((B25003_002) -
                      (B25003_003)) /
             (B25003_001),
           ICEinc=((B19001_014+B19001_015+B19001_016+B19001_017) -
                     (B19001_002+B19001_003+B19001_004)) / 
             (B19001_001),
           ICErace=((B03002_003) - 
                      (B03002_004)) / 
             (B03002_001),
           ICEwbinc=((B19001A_014+B19001A_015+B19001A_016+B19001A_017) -
                       (B19001B_002+B19001B_003+B19001B_004+B19001B_005)) /
             (B19001_001),
           ICEwnhinc=((B19001H_014+B19001H_015+B19001H_016+B19001H_017) -
                        (B19001_002+B19001_003+B19001_004+B19001_005-B19001H_002-B19001H_003-B19001H_004-B19001H_005)) / 
             (B19001_001),
           .keep="none")
}

