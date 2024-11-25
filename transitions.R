library(dplyr)
library(tidyr)
library(lubridate)

transition_map <- data.frame(
  target_code = c(
    # Northamptonshire split
    "E06000061",  # North Northamptonshire
    "E06000062",  # West Northamptonshire
    
    # Dorset changes
    "E06000058",  # Bournemouth, Christchurch and Poole
    "E06000059",  # Dorset
    
    # Buckinghamshire
    "E06000060",  # Buckinghamshire
    
    # Suffolk changes
    "E07000244",  # East Suffolk
    "E07000245",  # West Suffolk
    
    # Somerset West and Taunton (intermediate change)
    "E07000246"   # Somerset West and Taunton
  ),
  
  target_name = c(
    "North Northamptonshire",
    "West Northamptonshire",
    "Bournemouth, Christchurch and Poole",
    "Dorset",
    "Buckinghamshire",
    "East Suffolk",
    "West Suffolk",
    "Somerset West and Taunton"
  ),
  
  original_codes = c(
    # North Northamptonshire formed from
    "E07000150,E07000152,E07000153,E07000156",
    
    # West Northamptonshire formed from
    "E07000151,E07000154,E07000155",
    
    # Bournemouth, Christchurch and Poole
    "E06000028,E06000029,E07000048",
    
    # Dorset
    "E07000049,E07000050,E07000051,E07000052,E07000053",
    
    # Buckinghamshire
    "E07000004,E07000005,E07000006,E07000007",
    
    # East Suffolk
    "E07000205,E07000206",
    
    # West Suffolk
    "E07000201,E07000204",
    
    # Somerset West and Taunton
    "E07000190,E07000191"
  ),
  
  change_date = as.Date(c(
    "2021-04-01",  # North Northamptonshire
    "2021-04-01",  # West Northamptonshire
    "2019-04-01",  # Bournemouth, Christchurch and Poole
    "2019-04-01",  # Dorset
    "2020-04-01",  # Buckinghamshire
    "2019-04-01",  # East Suffolk
    "2019-04-01",  # West Suffolk
    "2019-04-01"   # Somerset West and Taunton
  ))
)

# Print the transition map
print(transition_map)
