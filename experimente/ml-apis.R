# install_github("flovv/Roxford")
# needs(devtools, Roxford)
# 
# emotionkey = 'c9da9da734ec4d0eb9b3441e41d21262' # different key
# getEmotionResponse("Pictures/2017/2017-02-26/DSCF8418.JPG", emotionkey)
# getEmotionResponseURL("https://img.morgenpost.de/img/berlin/crop210561899/7150935410-w820-cv3_2-q85/GYI-682626224-8259.jpg", emotionkey)
# 
# 
# visionkey = '11a02df6f0ec480e8e32ad5c444235b9' # different key
# getDescriptionResponseURL("http://sizlingpeople.com/wp-content/uploads/2015/10/Kim-Kardashian-2015-21.jpg", visionkey)
# 
# getTaggingResponseURL("http://sizlingpeople.com/wp-content/uploads/2015/10/Kim-Kardashian-2015-21.jpg", visionkey)
# 
# getTaggingResponseURL("http://sizlingpeople.com/wp-content/uploads/2015/10/Kim-Kardashian-2015-21.jpg", visionkey)
# 
# ## can  be used to classify with domain specific models provided by Microsoft.
# # run  getDomainModels(visionkey) first to get a list with available models
# getDomainModelResponseURL("http://sizlingpeople.com/wp-content/uploads/2015/10/Kim-Kardashian-2015-21.jpg", visionkey, 'celebreties')
# 
# if (!require("devtools")) {
#   install.packages("ghit")
# }
# devtools::install_github("cloudyr/RoogleVision")
# 
# require(RoogleVision)
# 
# ### plugin your credentials
# options("googleAuthR.client_id" = "617642804154-8doojsmf93ahouok5snbaa0qvh6lrcm7.apps.googleusercontent.com")
# options("googleAuthR.client_secret" = "LZPYsTPOU1kPUPn3yTI-mZYm")
# 
# ## use the fantastic Google Auth R package
# ### define scope!
# options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/cloud-platform"))
# googleAuthR::gar_auth()
# 
# ############
# #Basic: you can provide both, local as well as online images:
# o <- getGoogleVisionResponse("brandlogos.png")
# o <- getGoogleVisionResponse(imagePath="brandlogos.png", feature="LOGO_DETECTION", numResults=4)
# getGoogleVisionResponse("Desktop/Bildschirmfoto 2017-05-13 um 22.40.07.png", feature="FACE_DETECTION", numResults=200)


### FEATURES
# with the parameter 'feature' you can define which type of analysis you want. Results differ by feature-type
# The default is set to 'LABEL_DETECTION' but you can choose one out of: FACE_DETECTION, LANDMARK_DETECTION, LOGO_DETECTION, LABEL_DETECTION, TEXT_DETECTION


# Load httr package
loadNamespace("httr")
# Set an endpoint for Emotion in Video API with 'perFrame' output
apiUrl <- "https://api.projectoxford.ai/emotion/v1.0/recognizeInVideo?outputStyle=perFrame"
# Set your API key for Emotion API
key <- 'ce9dd5c43c574b93a53d72cf64e9b04b'
  # Set URL for accessing to the video.
  urlVideo <- 'https://www.dropbox.com/s/zfmaswf8s9c58om/blog2.mp4?dl=1'
mybody <- list(url = urlVideo)
# Request data from Microsoft AI
faceEMO <- httr::POST(
  url = apiUrl,
  httr::content_type('application/json'),
  httr::add_headers(.headers = c('Ocp-Apim-Subscription-Key' = key)),
  body = mybody,
  encode = 'json'
)
# Get the location where the result is stored.
operationLocation <- httr::headers(faceEMO)[["operation-location"]]
operationLocation
