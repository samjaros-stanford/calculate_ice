#' @author Hoda Abdel Magid
#' @author Sam Jaros
#' @name calculate_ice
#' @import dplyr
#' @import tidycensus

# Create a wrapper function for calculate_ice
create_ice = function(geography_codes, api_key=Sys.getenv("CENSUS_API_KEY"))
  require(dplyr)
  require(tidycensus)
  
  census_api_key(api_key)
  
  
  
  
  
  
  
  ice_vars = 
  # Calculate ICE variables in the format
  # name = ((privileged) - 
  #         (deprived)) /
  #        (total)
  mutate(GEOID=GEIOD,
         ICEedu=((B15003_022E) -
                 (B15003_016E+B15003_015E+B15003_014E+B15003_013E+B15003_012E+B15003_011E+B15003_010E+B15003_009E+B15003_008E+B15003_007E+B15003_006E+B15003_005E+B15003_004E+B15003_003E+B15003_002E)) /
                (B15003_001E),
         ICEhome=((B25003_002E) -
                  (B25003_003E) /
                 (B25003_001E)),
         ICEinc=((B19001_014E+B19001_015E+B19001_016E+B19001_017E) -
                 (B19001_002E+B19001_003E+B19001_004E)) / 
                (B19001_001E),
         ICErace=((B03002_003E) - 
                  (B03002_004_E)) / 
                 (B03002_001E),
         ICEwbinc=((B19001A_014E+B19001A_015E+B19001A_016E+B19001A_017E) -
                   (B19001B_002E+B19001B_003E+B19001B_004E+B19001B_005E)) /
                  (B19001_001E),
         ICEwnhinc=((B19001H_014E+B19001H_015E+B19001H_016E+B19001H_017E) -
                    (B19001_002E+B19001_003E+B19001_004E+B19001_005E-B19001H_002E-B19001H_003E-B19001H_004E-B19001H_005E)) / 
                   (B19001_001E),
  .keep="none") %>%
  select(GEOID, ICEwbinc, ICEwnhinc)
  
  
