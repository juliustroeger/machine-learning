import httplib
import urllib
import base64
import json
import pandas as pd
import numpy as np
import requests
import time

# you have to sign up for an API key, which has some allowances. Check the API documentation for further details:
_url = 'https://api.projectoxford.ai/emotion/v1.0/recognizeInVideo'
_key = '1d8416c7c83f4918968942d17745e04e' #Here you have to paste your primary key
_maxNumRetries = 10

# URL direction: I hosted this on my domain
urlVideo = 'http://datacandy.co.uk/blog2.mp4'

# Computer Vision parameters
paramsPost = { 'outputStyle' : 'perFrame'}

headersPost = dict()
headersPost['Ocp-Apim-Subscription-Key'] = _key
headersPost['Content-Type'] = 'application/json'

jsonPost = { 'url': urlVideo }

responsePost = requests.request( 'post', _url, json = jsonPost, data = None, headers = headersPost, params = paramsPost )

if responsePost.status_code == 202: # everything went well!
    videoIDLocation = responsePost.headers['Operation-Location']
    print videoIDLocation

while True: 
	time.sleep(4)

	getResponse = requests.request( 'get', videoIDLocation, json = jsonGet, data = None, headers = headersGet, params = paramsGet )

jsonObject = json.loads(getResponse.text)
print jsonObject
if jsonObject['progress'] == 100.0: 
	break

## Wait a bit, it's processing
headersGet = dict()
headersGet['Ocp-Apim-Subscription-Key'] = _key

jsonGet = {}
paramsGet = urllib.urlencode({})



rawData = json.loads(json.loads(getResponse.text)['processingResult'])
timeScale = rawData['timescale']
frameRate = rawData['framerate']

emotionPerFramePerFace = {}
currFrameNum = 0

for currFragment in rawData['fragments']:
 for currEvent in currFragment['events']:
     emotionPerFramePerFace[currFrameNum] = currEvent
     currFrameNum += 1


# Data collection
person1, person2  = [], []
for frame_no, v in emotionPerFramePerFace.copy().items():
 for i, minidict in enumerate(v):
     for k, v in minidict['scores'].items():
         minidict[k] = v

     minidict['frame'] = frame_no
     if i == 0:
         person1.append(minidict)
     else:
         person2.append(minidict)


df1 = pd.DataFrame(person1)
df2 = pd.DataFrame(person2)
del df1['scores']
del df2['scores']

# Saving in pd data-frame format:
df1.to_csv("/your/file/path/trump.csv", index=False)
df2.to_csv("/your/file/path/clinton.csv", index=False)