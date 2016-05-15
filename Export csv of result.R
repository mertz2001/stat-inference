#Import into google fusion tables
setwd("/Users/jamesbailie/Documents/uni/STAT3013/Project/stat-inference")
#Get postcode area code:
POAs_with_data["ID"] = POAs_with_data["postcode_n"]
merged <- merge(results, POAs_with_data,by="ID")

#for some reason results has 2 rows for each postcode
#Just take the '.id' = postcode_n row: (ask Mikkel tomorrow)

export <- merged[merged['.id'] == "postcode_n",]

#take the columns we care about
export2 <- cbind(export["POA_CODE"], export["mean"])

#Pad out three digit numbers into 4 digit numbers
export2["POA_CODE"] <- sprintf("%04d", export2[,"POA_CODE"])

#output as .csv file to put into Google Fusion Tables:
write.csv(export2, "results table.csv")


570
274
POAs_with_data[570,]
POAs_with_data[274,]
immunisation_data["Postcode"=="3660",]
subset(immunisation_data, Postcode == 3660)
subset(immunisation_data, Postcode == 2540)
