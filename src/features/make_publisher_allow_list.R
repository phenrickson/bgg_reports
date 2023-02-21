# publisher list
publisher_allow_list = c(
        51 # Hasbo
        ,10 # Mayfair Games
        ,102 # Decision Games
        ,196 # Multi-Man Publishing
        ,396 # Alderac Entertainment Group aka AEG
        ,1027 # Days of Wonder
        ,21847 # Pandasaurus Games
        ,1001 # (web published)
        ,4 # (Self-Published)
        ,140 # Splotter Spellen
        ,157 # Asmodee
        ,34 # Ravensburger
        ,28 # Parker Brothers
        ,39 # Pegasus Speile
        ,37 # KOSMOS
        ,20 # Milton Bradley
        ,3 # Rio Grande Games
        ,538 # Z-Man Games
        ,52 # GMT Games
        # ,8923 # IELLO
        ,17 # Fantasy Flight Games
        ,5 # Avalon Hill
        ,3320 # (Unknown)
        ,597 # Eagle-Gryphon Games
        ,5400 # Matagot
        ,26 # Games Workshop Ltd
        ,47 # Queen Games
        ,11652 # Stronghold Games
        ,19 # Steve Jackson Games
        ,13 # Wizards of the Coast
        ,12024 # Cryptozoic Entertainment
        ,10754 # Plaid Hat Games
        ,21608 # CMON Global Limited
        ,108 # Gamewright
        ,221 # WizKids
        ,171 # (Public Domain)
        ,93 # Mattel, Inc
        ,25842 # Space Cowboys
        ,23202 # Stonemaier
        ,34188 # Plan  B
        ,30958 # Capstone Games
        ,22593 # Chip Theory Games
        ,17917 # Ares Games
        ,17543 # Greater Than Games
        ,28072 # Renegade Games
        ,34846 # Restoration Games
        ,29313 # Osprey Games
        ,21765 # Roxley
        ,7345 # Czech Games Edition
        ,29412 # Awaken Realms
        ,3929 # Compass Games
        ,26991 # Button Shy
        ,2456 # The Game Crafter
        ,12 # Cheapass Games
        ,9 # alea
        ,2164 # NorthStar Game Studio 
        ,5774 # Bézier Games
        ,18617 #Red Raven Games 
        ,102 #Decision Games (I)
        , 489# 3W (World Wide Wargames) 
)

# pin
library(pins)

# set local board
board = board_folder(here::here("data", "processed"),
                     versioned = T)

# write
board %>%
        pin_write(publisher_allow_list,
                  name = 'publisher_allow_list',
                  description = 'publishers that can be used in modeling')
                  
                  

