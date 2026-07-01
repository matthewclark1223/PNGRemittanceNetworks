#Analyis 1, primary effects

dag {
  "Age HHH" [pos="0.778,0.564"]
  "commercialization of fishing" [outcome,pos="0.309,1.491"]
  "leadership status" [pos="-1.327,-0.326"]
  "network position (betweenness)" [exposure,pos="-0.737,0.924"]
  "number of adults in household" [pos="-0.549,0.004"]
  "number of children in household" [pos="0.543,-0.127"]
  "sources of external credit" [exposure,pos="-1.593,1.478"]
  "sources of internal credit" [exposure,pos="-1.574,0.472"]
  "state of the fishery" [pos="1.252,0.655"]
  MSL [pos="-0.441,-0.583"]
  "Age HHH" -> "commercialization of fishing"
  "Age HHH" -> "leadership status"
  "Age HHH" -> "network position (betweenness)"
  "Age HHH" -> "number of children in household"
  "Age HHH" -> "sources of external credit"
  "Age HHH" -> "sources of internal credit"
  "commercialization of fishing" -> MSL
  "leadership status" -> "commercialization of fishing"
  "leadership status" -> "sources of internal credit"
  "network position (betweenness)" -> "commercialization of fishing"
  "number of adults in household" -> "commercialization of fishing"
  "number of adults in household" -> "number of children in household"
  "number of adults in household" -> "sources of external credit"
  "number of adults in household" -> "sources of internal credit"
  "number of children in household" -> "commercialization of fishing"
  "number of children in household" -> "sources of external credit"
  "sources of external credit" -> "commercialization of fishing"
  "sources of external credit" -> MSL
  "sources of internal credit" -> "network position (betweenness)"
  "sources of internal credit" -> MSL
  "state of the fishery" -> "commercialization of fishing"
}

#analysis 2, secondary effects

dag {
  "Age HHH" [pos="0.778,0.564"]
  "commercialization of fishing" [exposure,pos="0.113,1.094"]
  "food security/fish sharing" [outcome,pos="-0.867,1.476"]
  "leadership status" [pos="-1.327,-0.326"]
  "network position (betweenness)" [exposure,pos="-0.764,0.603"]
  "number of adults in household" [pos="-0.536,0.187"]
  "number of children in household" [pos="0.543,-0.127"]
  "sources of external credit" [exposure,pos="-1.338,0.966"]
  "sources of internal credit" [exposure,pos="-1.396,0.461"]
  "state of the fishery" [pos="1.252,0.655"]
  MSL [pos="-0.441,-0.583"]
  "Age HHH" -> "commercialization of fishing"
  "Age HHH" -> "food security/fish sharing"
  "Age HHH" -> "leadership status"
  "Age HHH" -> "network position (betweenness)"
  "Age HHH" -> "number of children in household"
  "Age HHH" -> "sources of external credit"
  "Age HHH" -> "sources of internal credit"
  "commercialization of fishing" -> "food security/fish sharing"
  "commercialization of fishing" -> MSL
  "leadership status" -> "commercialization of fishing"
  "leadership status" -> "food security/fish sharing"
  "leadership status" -> "sources of internal credit"
  "network position (betweenness)" -> "commercialization of fishing"
  "network position (betweenness)" -> "food security/fish sharing"
  "number of adults in household" -> "commercialization of fishing"
  "number of adults in household" -> "food security/fish sharing"
  "number of adults in household" -> "number of children in household"
  "number of adults in household" -> "sources of external credit"
  "number of adults in household" -> "sources of internal credit"
  "number of children in household" -> "commercialization of fishing"
  "number of children in household" -> "food security/fish sharing"
  "number of children in household" -> "sources of external credit"
  "sources of external credit" -> "commercialization of fishing"
  "sources of external credit" -> "food security/fish sharing"
  "sources of external credit" -> MSL
  "sources of internal credit" -> "food security/fish sharing"
  "sources of internal credit" -> "network position (betweenness)"
  "sources of internal credit" -> MSL
  "state of the fishery" -> "commercialization of fishing"
}




