# Area-consumption-of-wind-turbines
This is an attempt to calcluate the actual area consumption of the wind turbines in Rhineland-Palatinate. Therefore the area covered from one wind turbine to the next one has to be assessed.
Eventually the electricity generated by wind turbines should be compared with the respective area consumption which then leads to an evaluation of the wind energy potential in Rhineland-Palatinate.
1. The csv file contains the merged data that origined from the master and movement operator Amprion and the market master data register (Marktstammdatenregister - MaStR) and contains the correct geographical information. The important variable names are:
* eeg_nr: System key according to the Renewable Energy Act on which the merging of the two datasets has been done (EEG Anlagenschlüssel)
* leistung: Capacity of the wint turbine 
* indatum: Commissioning date 
* gem: Community key
* b\_wgs84: Latitude with CRS WGS84
* l\_wgs84: Longitude with CRS WGS84
* rotor: Rotor diameter \[m\]
* menge_kwh: Electricity yield in 2019 \[kWh]
* menge_mwh: Electricity yield in 2019 \[MWh\]
* flh: Full load hours in 2019
(if \_m variable oiriginates from MaStR, if \_s variable originates from Amprion)
3. The R file creates a Leaflet map to display the data
4. The html file shows the output of the created Leaflet map on a glance
