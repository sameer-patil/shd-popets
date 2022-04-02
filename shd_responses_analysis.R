#### Copyright 2022 Patrick Bombik, Tom Wenzel, Jens Grossklags, Sameer Patil
# Redistribution and use in source and binary forms, with or without modification, are permitted 
# provided that the following conditions are met:

# 1. Redistributions of source code must retain the above copyright notice, 
# this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright notice, 
# this list of conditions and the following disclaimer in the documentation and/or other materials 
# provided with the distribution.

# 3. Neither the name of the copyright holder nor the names of its contributors may be used 
# to endorse or promote products derived from this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR 
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE 
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; 
# OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
# STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF 
# THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



#### 
# This file is part of the set of files accompanying the paper 
# "A Multi-Region Investigation of the Perceptions and Use of Smart Home Devices" 
# by Patrick Bombik, Tom Wenzel, Jens Grossklags, and Sameer Patil 
# published in the Proceedings on Privacy Enhancing Technologies (PoPETS).

# This file contains the R code used to process and analyze the data 
# to generate the results reported in the paper.
# The data is available in the file shd_responses_data.csv.
# The codebook for the data that includes a list of all variables along with the corresponding questions
# from the  questionnaire and the values for the possible answer choices is shd_responses_codebook.pdf.

# This script can be run from the command line if R is installed on the machine. 
# Installation files and instructions for R can be obtained from: https://www.r-project.org/
# This script can also be run within any IDE that supports R, such as RStudio (https://www.rstudio.com/)


##### Load required libraries ----
library(dplyr)
library(doBy)
library(FSA)
library(formattable)

#### Replate "/PATH/TO" with the path to the file shd_responses_data.csv
shd_responses <- read.csv("/PATH/TO/shd_responses_data.csv")

# Calculate the measures needed for the analyses ----

# Create a Boolean variable to indicate if the participant has children
shd_responses$parent <- ifelse(shd_responses$numberofchildren > 0, 1, 0)


# Calculate the scores for regulatory protection by averaging the relevant items

shd_responses$rpthirdparty <-
  rowMeans(
    select(
      shd_responses,
      regulation_thirdpartyaccess,
      regulation_thirdpartysharing,
      regulation_thirdpartyprocessing
    )
  )

shd_responses$rppenalty <-
  rowMeans(
    select(
      shd_responses,
      regulation_accesspenalty,
      regulation_accesspenalty,
      regulation_storepenalty
    )
  )


# Reverse four items in SeBIS Proactive Awareness subscale
shd_responses <- mutate(shd_responses, sebis_awareness_openlink = 8 - sebis_awareness_openlink)
shd_responses <- mutate(shd_responses, sebis_awareness_sitelookfeel = 8 - sebis_awareness_sitelookfeel)
shd_responses <- mutate(shd_responses, sebis_awareness_urlverification = 8 - sebis_awareness_urlverification)
shd_responses <- mutate(shd_responses, sebis_awareness_someoneelse = 8 - sebis_awareness_someoneelse)



# Calculate the scores for SeBIS and MUIPC, including subscales

shd_responses$sebis <-
  rowMeans(
    select(
      shd_responses,
      sebis_securement_screenlockauto,
      sebis_securement_computerpassword,
      sebis_securement_screenlockmanual,
      sebis_securement_mobilepassword,
      sebis_awareness_openlink,
      sebis_awareness_sitelookfeel,
      sebis_awareness_urlverification,
      sebis_awareness_mouseover,
      sebis_awareness_someoneelse,
      sebis_updating_immediate,
      sebis_updating_ensure,
      sebis_updating_antivirus
    )
  )

shd_responses$sebis_devicesecurement <-
  rowMeans(
    select(
      shd_responses,
      sebis_securement_screenlockauto,
      sebis_securement_computerpassword,
      sebis_securement_screenlockmanual,
      sebis_securement_mobilepassword
    )
  )

shd_responses$sebis_proactiveawareness <-
  rowMeans(
    select(
      shd_responses,
      sebis_awareness_openlink,
      sebis_awareness_sitelookfeel,
      sebis_awareness_urlverification,
      sebis_awareness_mouseover,
      sebis_awareness_someoneelse
    )
  )

shd_responses$sebis_updatingbehavior <-
  rowMeans(
    select(
      shd_responses,
      sebis_updating_immediate,
      sebis_updating_ensure,
      sebis_updating_antivirus
    )
  )


shd_responses$muipc <-
  rowMeans(
    select(
      shd_responses,
      muipc_surveillance_location,
      muipc_surveillance_toomuch,
      muipc_surveillance_activities,
      muipc_intrusion_othersknow,
      muipc_intrusion_readilyavailable,
      muipc_intrusion_privacyinvasion,
      muipc_secondaryuse_unauthorized,
      muipc_secondaryuse_otherpurposes,
      muipc_secondaryuse_shareunauthorized
    )
  )

shd_responses$muipc_perceivedsurveillance <-
  rowMeans(
    select(
      shd_responses,
      muipc_surveillance_location,
      muipc_surveillance_toomuch,
      muipc_surveillance_activities
    )
  )

shd_responses$muipc_perceivedintrusion <-
  rowMeans(
    select(
      shd_responses,
      muipc_intrusion_othersknow,
      muipc_intrusion_readilyavailable,
      muipc_intrusion_privacyinvasion
    )
  )

shd_responses$muipc_secondaryuse <-
  rowMeans(
    select(
      shd_responses,
      muipc_secondaryuse_unauthorized,
      muipc_secondaryuse_otherpurposes,
      muipc_secondaryuse_shareunauthorized
    )
  )


shd_responses$devicerisk_combined <- rowMeans(
  select(
    shd_responses,
    devicerisk_coffeemaker,
    devicerisk_dishwasher,
    devicerisk_doorlock,
    devicerisk_doorbell,
    devicerisk_electricitymeter,
    devicerisk_outlet,
    devicerisk_fridge,
    devicerisk_gardening,
    devicerisk_hvac,
    devicerisk_homemonitor,
    devicerisk_lightbulb,
    devicerisk_oven,
    devicerisk_robot,
    devicerisk_speaker,
    devicerisk_stove,
    devicerisk_tv,
    devicerisk_thermostat,
    devicerisk_toy,
    devicerisk_vacuum,
    devicerisk_washer
  )
)



# Convert the householdsize variable to numeric
shd_responses$householdsize <- as.numeric(shd_responses$householdsize)


# Create separate datasets for each of the three regions for later analyses
gs <- subset(shd_responses, region == "GS")
us <- subset(shd_responses, region == "US")
uk <- subset(shd_responses, region == "UK")





##### Data Analysis ----

##### Summary of participant demographics split by the three regions covered in the study

agedata <- summaryBy(
  age_prolific ~ region,
  data = shd_responses,
  FUN = c(median, mean, sd, min, max),
  na.rm = TRUE
)
#3 people preferred not to enter their age

agedata <- formattable(agedata,format="f",digits=4)
agedata

table(shd_responses$region, shd_responses$gender)
table(us$gender)/nrow(us)*100
table(uk$gender)/nrow(uk)*100
table(gs$gender)/nrow(gs)*100



##### The distribution of participants based on the number of types of Smart Home Devices owned, split region
table(shd_responses$region, shd_responses$numberofshds)
table(shd_responses$numberofshds)




##### Demographics of the participants who reported owning the three most owned Smart Home Devices.
owneddevice1 <- subset(shd_responses, pickeddevice1_owned == 1)
owneddevice2 <- subset(shd_responses, pickeddevice2_owned == 1)
owneddevice3 <- subset(shd_responses, pickeddevice3_owned == 1)


owneddevice1 <- subset(shd_responses, pickeddevice1_owned == 1)
owneddevice1 <- subset(owneddevice1, owneddevice1$device1_usefrequency != 7)
owneddevice1 <- subset(owneddevice1, owneddevice1$pickeddevice1 == "Smart TV" | owneddevice1$pickeddevice1 == "Smart Speaker" | owneddevice1$pickeddevice1 == "Smart Light Bulb")

owneddevice2 <- subset(shd_responses, pickeddevice2_owned == 1)
owneddevice2 <- subset(owneddevice2, owneddevice2$device2_usefrequency != 7)
owneddevice2 <- subset(owneddevice2, owneddevice2$pickeddevice2 == "Smart TV" | owneddevice2$pickeddevice2 == "Smart Speaker" | owneddevice2$pickeddevice2 == "Smart Light Bulb")


owneddevice3 <- subset(shd_responses, pickeddevice3_owned == 1)
owneddevice3 <- subset(owneddevice3, owneddevice3$device3_usefrequency != 7)
owneddevice3 <- subset(owneddevice3, owneddevice3$pickeddevice3 == "Smart TV" | owneddevice3$pickeddevice3 == "Smart Speaker" | owneddevice3$pickeddevice3 == "Smart Light Bulb")

mostowneddevices_combined <- rbind(owneddevice1, owneddevice2, owneddevice3)
nrow(table(mostowneddevices_combined$id))

mostowneddevices_owners <- mostowneddevices_combined
mostowneddevices_owners <- mostowneddevices_owners %>% distinct(id, .keep_all = TRUE)
table(mostowneddevices_owners$gender)
table(mostowneddevices_owners$gender)/nrow(mostowneddevices_owners)*100
median(mostowneddevices_owners$age_prolific, na.rm=TRUE)




##### Relationship between perceived regulatory protection and perceived intrusion subscale of MUIPC
cor.test(shd_responses$rpthirdparty, shd_responses$muipc_perceivedintrusion)

cor.test(subset(shd_responses, numberofshds < 1)$rpthirdparty, subset(shd_responses, numberofshds < 1)$muipc_perceivedintrusion)
cor.test(subset(shd_responses, numberofshds > 0)$rpthirdparty, subset(shd_responses, numberofshds > 0)$muipc_perceivedintrusion)



#### Relationship between manufacturer responsibility for protecting privacy and security and perceived intrusion subscale of MUIPC
cor.test(shd_responses$devicesecurity, shd_responses$muipc_perceivedintrusion)



#### Relationship between perceived regulatory protection and the number of types of Smart Home Devices
cor.test(shd_responses$rpthirdparty, shd_responses$numberofshds)
cor.test(shd_responses$regulation_thirdpartyaccess, shd_responses$numberofshds)
cor.test(shd_responses$regulation_thirdpartysharing, shd_responses$numberofshds)
cor.test(shd_responses$regulation_thirdpartyprocessing, shd_responses$numberofshds)
p <- c(0.02135, 0.06435, 0.02868)
round(p.adjust(p, "bonferroni"), 4)


#### Relationships between perceived privacy risk and perceived regulatory protection as well as its three individual facets 

# For overall perceived privacy risk and all devices combined
cor.test(shd_responses$rpthirdparty, shd_responses$devicerisk_combined)
cor.test(shd_responses$regulation_thirdpartyaccess, shd_responses$devicerisk_combined)
cor.test(shd_responses$regulation_thirdpartysharing, shd_responses$devicerisk_combined)
cor.test(shd_responses$regulation_thirdpartyprocessing, shd_responses$devicerisk_combined)
p <- c(0.0004338, 0.0006497, 0.0008436, 0.0006966)
round(p.adjust(p, "bonferroni"), 4)

# For overall perceived privacy risk, separately for each of the three most-owned devices
cor.test(shd_responses$rpthirdparty, shd_responses$devicerisk_tv)
cor.test(shd_responses$rpthirdparty, shd_responses$devicerisk_speaker)
cor.test(shd_responses$rpthirdparty, shd_responses$devicerisk_lightbulb)
p <- c(0.0001196, 0.001193, 0.06201)
round(p.adjust(p, "bonferroni"), 4)

# For unwanted access by third parties, separately for each of the three most-owned devices
cor.test(shd_responses$regulation_thirdpartyaccess, shd_responses$devicerisk_tv)
cor.test(shd_responses$regulation_thirdpartyaccess, shd_responses$devicerisk_speaker)
cor.test(shd_responses$regulation_thirdpartyaccess, shd_responses$devicerisk_lightbulb)
p <- c(0.0003707, 0.0003755, 0.04344)
round(p.adjust(p, "bonferroni"), 4)

# For nwanted sharing with third parties, separately for each of the three most-owned devices
cor.test(shd_responses$regulation_thirdpartysharing, shd_responses$devicerisk_tv)
cor.test(shd_responses$regulation_thirdpartysharing, shd_responses$devicerisk_speaker)
cor.test(shd_responses$regulation_thirdpartysharing, shd_responses$devicerisk_lightbulb)
p <- c(9.62e-05, 0.002298, 0.1021)
round(p.adjust(p, "bonferroni"), 4)

# For unwanted processing and analysis by third parties, separately for each of the three most-owned devices
cor.test(shd_responses$regulation_thirdpartyprocessing, shd_responses$devicerisk_tv)
cor.test(shd_responses$regulation_thirdpartyprocessing, shd_responses$devicerisk_speaker)
cor.test(shd_responses$regulation_thirdpartyprocessing, shd_responses$devicerisk_lightbulb)
p <- c(0.0002941, 0.006115, 0.08366)
round(p.adjust(p, "bonferroni"), 4)





#### Relationship between perceived regulatory protection and use of the three most-owned Smart Home Devices

specificshds <-
  select(
    shd_responses,
    id,
    pickeddevice1,
    pickeddevice2,
    pickeddevice3,
    pickeddevice1_owned,
    pickeddevice2_owned,
    pickeddevice3_owned,
    device1_usefrequency,
    device2_usefrequency,
    device3_usefrequency,
    region,
    rpthirdparty,
    shddata_remotestorage
  )

# Create subsets for each device based on whether the participant reported owning the device

device1 <-
  select(
    owneddevice1,
    id,
    pickeddevice1,
    device1_usefrequency,
    region,
    rpthirdparty,
    shddata_remotestorage,
    parent,
    homeowner,
    householdsize,
    numberofshds,
    devicerisk_combined
  )

device2 <-
  select(
    owneddevice2,
    id,
    pickeddevice2,
    device2_usefrequency,
    region,
    rpthirdparty,
    shddata_remotestorage,
    parent,
    homeowner,
    householdsize,
    numberofshds,
    devicerisk_combined
  )

device3 <-
  select(
    owneddevice3,
    id,
    pickeddevice3,
    device2_usefrequency,
    region,
    rpthirdparty,
    shddata_remotestorage,
    parent,
    homeowner,
    householdsize,
    numberofshds,
    devicerisk_combined
  )

colnames(device1) <- c("id", "device", "usefrequency", "region", "rpthirdparty", "shddata_remotestorage", "parent", "homeowner", "householdsize", "numberofshds", "devicerisk_combined")
colnames(device2) <- c("id", "device", "usefrequency", "region", "rpthirdparty", "shddata_remotestorage", "parent", "homeowner", "householdsize", "numberofshds", "devicerisk_combined")
colnames(device3) <- c("id", "device", "usefrequency", "region", "rpthirdparty", "shddata_remotestorage", "parent", "homeowner", "householdsize", "numberofshds", "devicerisk_combined")

# Combine the three subsets into a single dataset
mostowneddevices_combined <- rbind(device1, device2, device3)

# Create separate datasets for each of the three most-owned devices
smarttv <- subset(mostowneddevices_combined, device == "Smart TV")
smartspeaker <- subset(mostowneddevices_combined, device == "Smart Speaker")
smartlightbulb <- subset(mostowneddevices_combined, device == "Smart Light Bulb")

# Analyze the use of each of the three most-owned devices separately
cor.test(smarttv$rpthirdparty, smarttv$usefrequency)
cor.test(smartspeaker$rpthirdparty, smartspeaker$usefrequency)
cor.test(smartlightbulb$rpthirdparty, smartlightbulb$usefrequency)
p <- c(0.01194, 0.001376, 0.261)
round(p.adjust(p, "bonferroni"), 4)


#### Relationships between perceived benefits of Smart Home Devices and region

benefitsanova_leisure <-  aov(shd_responses$benefit_leisure ~ shd_responses$region)
print(summary(benefitsanova_leisure))
TukeyHSD(benefitsanova_leisure)
round(tapply(shd_responses$benefit_leisure, shd_responses$region, mean), digits = 2)

benefitsanova_peaceofmind <-  aov(shd_responses$benefit_peaceofmind ~ shd_responses$region)
print(summary(benefitsanova_peaceofmind))
TukeyHSD(benefitsanova_peaceofmind)
round(tapply(shd_responses$benefit_peaceofmind, shd_responses$region, mean), digits = 2)

benefitsanova_comfort <-  aov(shd_responses$benefit_comfort ~ shd_responses$region)
print(summary(benefitsanova_comfort))
TukeyHSD(benefitsanova_comfort)
round(tapply(shd_responses$benefit_comfort, shd_responses$region, mean), digits = 2)

benefitsanova_safety <-  aov(shd_responses$benefit_safety ~ shd_responses$region)
print(summary(benefitsanova_safety))
TukeyHSD(benefitsanova_safety)
round(tapply(shd_responses$benefit_safety, shd_responses$region, mean), digits = 2)

benefitsanova_care <-  aov(shd_responses$benefit_care ~ shd_responses$region)
print(summary(benefitsanova_care))
TukeyHSD(benefitsanova_care)
round(tapply(shd_responses$benefit_care, shd_responses$region, mean), digits = 2)

benefitsanova_propertyvalue <- aov(shd_responses$benefit_propertyvalue ~ shd_responses$region)
print(summary(benefitsanova_propertyvalue))
TukeyHSD(benefitsanova_propertyvalue)
round(tapply(shd_responses$benefit_propertyvalue, shd_responses$region, mean), digits = 2)



#### Relationship between device adoption and region

regionanova <-  aov(shd_responses$numberofshds ~ shd_responses$region)
print(summary(regionanova))




#### Relationship between perceived regulatory protection and device adoption for each region

cor.test(us$rpthirdparty, us$numberofshds)
cor.test(uk$rpthirdparty, uk$numberofshds)
cor.test(gs$rpthirdparty, gs$numberofshds)
p <- c(0.224, 0.4643, 0.01803)
round(p.adjust(p, "bonferroni"), 4)



#### Relationship between the use of the three most-owned devices and region

# All devices combined
dunnTest(x = mostowneddevices_combined$usefrequency, mostowneddevices_combined$region)

# Smart TV
dunnTest(x = smarttv$usefrequency, smarttv$region)
aggregate(usefrequency~region, data=smarttv, FUN=function(x) c(mean=mean(x)))

# Smart speaker
dunnTest(x = smartspeaker$usefrequency, smartspeaker$region)

# Smart light bulb
dunnTest(x = smartlightbulb$usefrequency, smartlightbulb$region)


#### Differences between homeowners and renters

# Filter out those who did not answer either "own" or "rent" for their homes
ownersrenters <- subset(shd_responses, homeowner == 1 | homeowner == 2)
ownersrenters_mostowneddevices <- subset(mostowneddevices_combined, homeowner == 1 | homeowner == 2)
ownersrenters_smarttv <- subset(smarttv, homeowner == 1 | homeowner == 2)
ownersrenters_smartspeaker <- subset(smartspeaker, homeowner == 1 | homeowner == 2)
ownersrenters_smartlightbulb <- subset(smartlightbulb, homeowner == 1 | homeowner == 2)

# Perceived privacy risk
t.test(ownersrenters$devicerisk_combined ~ ownersrenters$homeowner)

# Adoption
t.test(ownersrenters$numberofshds ~ ownersrenters$homeowner)
aggregate(numberofshds~homeowner, data=ownersrenters, FUN=function(x) c(mean=mean(x)))

# Use
t.test(ownersrenters_mostowneddevices$usefrequency ~ ownersrenters_mostowneddevices$homeowner)

# Analyze use separately for the owners each of the three most-owned devices
t.test(ownersrenters_smarttv$usefrequency ~ ownersrenters_smarttv$homeowner)
t.test(ownersrenters_smartspeaker$usefrequency ~ ownersrenters_smartspeaker$homeowner)
t.test(ownersrenters_smartlightbulb$usefrequency ~ ownersrenters_smartlightbulb$homeowner)



##### Relationships with household size

# Perceived privacy risk
cor.test(shd_responses$devicerisk_combined, shd_responses$householdsize)
cor.test(mostowneddevices_combined$usefrequency, mostowneddevices_combined$householdsize)
p <- c(5.706e-05, 0.1039, 0.05806)
round(p.adjust(p, "bonferroni"), 4)


# Adoption
cor.test(shd_responses$numberofshds, shd_responses$householdsize)


cor.test(smarttv$usefrequency, smarttv$householdsize)
cor.test(smartspeaker$usefrequency, smartspeaker$householdsize)
cor.test(smartlightbulb$usefrequency, smartlightbulb$householdsize)
p <- c(0.0184, 0.8047, 0.5948)
round(p.adjust(p, "bonferroni"), 4)


#### Differences between parents and non-parents

# Perceipved privacy risk
t.test(shd_responses$devicerisk_combined ~ shd_responses$parent)

# Adoption
t.test(shd_responses$numberofshds ~ shd_responses$parent)

# Use
t.test(mostowneddevices_combined$usefrequency ~ mostowneddevices_combined$parent)
p <- c(0.03019, 0.0007448, 0.01429)
round(p.adjust(p, "bonferroni"), 4)

# Analyze use separately for each of the three most-owned devices
t.test(smarttv$usefrequency ~ smarttv$parent)
t.test(smartspeaker$usefrequency ~ smartspeaker$parent)
t.test(smartlightbulb$usefrequency ~ smartlightbulb$parent)
p <- c(0.01803, 0.1427, 0.9734)
round(p.adjust(p, "bonferroni"), 4)


# Comfort with various modes of interacting with Smart Home Devices
t.test(shd_responses$interactcomfort_speaker ~ shd_responses$parent)
t.test(shd_responses$interactcomfort_assistant ~ shd_responses$parent)
t.test(shd_responses$interactcomfort_app ~ shd_responses$parent)
t.test(shd_responses$interactcomfort_widget ~ shd_responses$parent)
t.test(shd_responses$interactcomfort_insidesensor ~ shd_responses$parent)
t.test(shd_responses$interactcomfort_outsidesensor ~ shd_responses$parent)
t.test(shd_responses$interactcomfort_automation ~ shd_responses$parent)
p <- c(0.001011, 6.889e-06, 0.9376, 0.3619, 0.08911, 0.09673, 0.9724)
round(p.adjust(p, "bonferroni"), 4)



#### Differences between device owners and those who own no Smart Home Devices

# Perceived regulatory protection
aggregate(rpthirdparty ~ numberofshds == 0, data=shd_responses, FUN=function(x) c(mean=mean(x)))

# MUIPC
t.test(shd_responses$muipc ~ shd_responses$numberofshds == 0)


#### Distribution of responses for which "dumb" devices would benefit from the addition of smart capabilities for the three most-owned devices
table(shd_responses$smartbenefit_tv)
table(shd_responses$smartbenefit_lightbulb)
table(shd_responses$smartbenefit_speaker)


#### Differences in perceived privacy risk for the three most-owned devices

tvrisk = as.data.frame(shd_responses$devicerisk_tv)
tvrisk$id = 'tv'
colnames(tvrisk) = c('devicerisk', 'device')

speakerrisk = as.data.frame(shd_responses$devicerisk_speaker)
speakerrisk$id = 'speaker'
colnames(speakerrisk) = c('devicerisk', 'device')

lightbulbrisk = as.data.frame(shd_responses$devicerisk_lightbulb)
lightbulbrisk$id = 'lightbulb'
colnames(lightbulbrisk) = c('devicerisk', 'device')


devicerisk_mostowneddevices_combined = rbind(tvrisk, speakerrisk, lightbulbrisk)
deviceriskanova =  aov(devicerisk_mostowneddevices_combined$devicerisk ~ devicerisk_mostowneddevices_combined$device)
print(summary(deviceriskanova))

tukeyhsd = TukeyHSD(deviceriskanova)
print(tukeyhsd)

mean(tvrisk$devicerisk)
mean(speakerrisk$devicerisk)
mean(lightbulbrisk$devicerisk)



#### Distribution of responses for the top reasons for choosing not to own a Smart Home Device
table(shd_responses$notown_cost)/nrow(shd_responses)*100
table(shd_responses$notown_adultprivacy)/nrow(shd_responses)*100
table(shd_responses$notown_childrenprivacy)/nrow(shd_responses)*100
table(shd_responses$notown_guestprivacy)/nrow(shd_responses)*100



#### Distribution of responses for reported disabling of features for owned Smart Home Devices
owneddevice1 <- subset(shd_responses, pickeddevice1_owned == 1)
owneddevice2 <- subset(shd_responses, pickeddevice2_owned == 1)
owneddevice3 <- subset(shd_responses, pickeddevice3_owned == 1)

table(owneddevice1$device1_disabledfeatures)
table(owneddevice2$device2_disabledfeatures)
table(owneddevice3$device3_disabledfeatures)


#### Relationship between manufacturer responsibility for protecting privacy and security and the proactive awareness subscale of SeBIS
cor.test(shd_responses$sebis_proactiveawareness, shd_responses$devicesecurity)


t.test(shd_responses$sebis_proactiveawareness ~ shd_responses$notown_adultprivacy)
t.test(shd_responses$sebis_proactiveawareness ~ shd_responses$notown_childrenprivacy)
t.test(shd_responses$sebis_proactiveawareness ~ shd_responses$notown_guestprivacy)
t.test(shd_responses$sebis_proactiveawareness ~ shd_responses$notown_petprivacy)
p <- c(0.2381, 0.599, 0.4754, 0.01507)
round(p.adjust(p, "bonferroni"), 4)