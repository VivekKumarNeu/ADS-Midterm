#Import all the required packages
import json
import urllib2
import requests
import bs4
import numpy as np
import time
from dateutil import parser, rrule
from bs4 import BeautifulSoup,NavigableString, Tag
import pandas as pd
from pandas import DataFrame, read_csv
from datetime import datetime
import os

#Assign the current working directory to the path variable
path = os.getcwd()

########################################################################################
#Code for Holiday
########################################################################################

#Get holiday data by web page scraping
res=requests.get('http://www.timeanddate.com/holidays/finland/2013')
res.raise_for_status()
noScratchsoup=bs4.BeautifulSoup(res.text, 'html.parser')
div=noScratchsoup.select('th.nw')
a=[]
for tag in div:
    source_code=str(tag)
    soup=bs4.BeautifulSoup(source_code,'html.parser')
    tempString=soup.th.string
    key=datetime.strptime(tempString[4:]+'/'+tempString[0:3]+'/'+'2013','%d/%b/%Y')
    a.append(key)

#read and filter the input data
st=pd.read_csv(os.path.join(path, 'Finland_masked.csv'))
df1= pd.read_csv(os.path.join(path, 'Finland_addresses_area.csv'))
st1=(st.loc[st['type'] == 'elect'])
st2=(st.loc[st['type'] == 'Dist_Heating'])
frames = [st1, st2]
result = pd.concat(frames)
result=pd.DataFrame(result)

# finding the missing buildings name based on the consumption
nobuildingname= (result.loc[result['vac']=='                              '])

# filling building names
def buildingId(resultnew):
   if resultnew['BuildingID'] == 81909:
      return 'Building 27'
   return 'Building 9'
nobuildingname['vac'] = nobuildingname.apply (lambda x: buildingId(x),axis=1)

withbuildingname= (result.loc[result['vac']!='                              '])

frameset= [nobuildingname, withbuildingname]
result=pd.concat(frameset)

# derive date,DayofWeek,month,WeekDay and other fields.
df=pd.DataFrame(result)
df1=df1.rename(columns = {'building':'vac'})
df=pd.merge(df1,df,on='vac')
df['area/sq_meter'] = df['Consumption']/df['area_floor _m.sqr']
df['date']=pd.to_datetime(df['date'], format="%Y%m%d")
df['DayOfWeek']=pd.DatetimeIndex(df['date']).dayofweek
df['month']=pd.DatetimeIndex(df['date']).month
df['WeekDay']=[0 if (x == 0 or x == 6) else 1 for x in pd.DatetimeIndex(df['date']).weekday]
listValues=[0,1,2,3,4,22,23]
df['BaseHourFlag']=[True if (x in listValues) else False for x in df['hour']]
df['Holiday']=[True if (x in a) else False for x in df['date']]
df=df.rename(columns = {'vac':'Building Number','\t address':'address','hour':'Hour','area_floor _m.sqr':'Area(m_sq)','meternumb':'Meter_Number','type':'Consumption_Type','date':'Date','Consumption':'Consumption(kVA)','area/sq_meter':'kVA/sq_m'})

################################
#Vivek
################################

url_hit_count = 0
file_data = pd.read_csv(os.path.join(path, 'Finland_addresses_area.csv'))

addresses = file_data["\t address"]

address_location_dict = dict()


# getting the time in 24 hours
def timeinhours(tim):
    tim = tim.split(' ')
    if (tim[1] == 'PM'):
        if (int(tim[0].split(':')[0]) == 12):
            return 12
        else:
            return 12 + int(tim[0].split(':')[0])
    else:
        if (tim[0].split(':')[0].strip() == '12'):
            return 0
        else:
            return tim[0].split(':')[0].strip()


#getting geocode from the address
for address in addresses:
    global url_hit_count
    url_hit_count = url_hit_count + 1  # increasing url_hit_count on every date call
    if url_hit_count % 10 == 0:  # if 10 calls, sleep
        time.sleep(62)
    url = "https://maps.googleapis.com/maps/api/geocode/json?address={address}"
    url = url.format(address=address)
    response = requests.get(url)
    #response = urllib2.urlopen(url)

    # reading JSON data from maps api
    json_data = response.json()
    #json_data = json.load(response)

    # we get latitude and longitude from wunderground api
    nearest_airport = "http://api.wunderground.com/api/2a9107686ea85180/geolookup/q/{lat},{longi}.json"
    nearest_airport = nearest_airport.format(lat=json_data["results"][0]["geometry"]["location"]["lat"],
                                             longi=json_data["results"][0]["geometry"]["location"]["lng"])
    print nearest_airport

    time.sleep(1)
    # gathering airport geolocation
    airport_json_data = requests.get(nearest_airport).json()
    icao = airport_json_data["location"]["nearby_weather_stations"]["airport"]["station"][1]["icao"]
    address_location_dict[icao] = address_location_dict.get(icao, []) + [address]

#Empty dataframe with all the required columns
dframe = pd.DataFrame(
    columns=['AirportCode', 'Hour', 'Date', 'TimeEET', 'TemperatureF', 'Dew PointF', 'Humidity', 'Sea Level PressureIn',
             'VisibilityMPH', 'Wind Direction', 'Wind SpeedMPH', 'Gust SpeedMPH', 'PrecipitationIn', 'Events',
             'Conditions', 'WindDirDegrees', 'DateUTC'])


#get weather data from 01-Jan-2013 till 31-Dec-2013
for location_icao in address_location_dict:
    start_date = "2013-01-01"  # desired starting date
    end_date = "2013-12-31"  # desired ending date
    start = parser.parse(start_date)
    end = parser.parse(end_date)
    dates = list(
        rrule.rrule(rrule.DAILY, dtstart=start, until=end))  # generating the dates between starting and ending date

    dfAddress = pd.DataFrame({"AirportCode": location_icao, "address": address_location_dict[location_icao]})
    with open(os.path.join(path, 'zzzzz.csv'), 'a') as f:
        dfAddress.to_csv(f, index=False)

    for d in dates:

        url = "https://www.wunderground.com/history/airport/{icao}/{y}/{m}/{dd}/DailyHistory.html??format=1&format=1"
        url = url.format(icao=location_icao, y=d.year, m=d.month, dd=d.day)
        rr = requests.get(url).text
        soup1 = BeautifulSoup(rr, 'html.parser')

        for br in soup1.findAll('br'):
            next = br.nextSibling
            if not (next and isinstance(next, NavigableString)):
                continue
            next2 = next.nextSibling
            if next2 and isinstance(next2, Tag) and next2.name == 'br':
                text = str(next).strip()
                if text:
                    datee = next.split(',')[0].split(' ')[0].split(':')[0].strip()
                    dframe = dframe.append(pd.Series([location_icao, timeinhours(next.split(',')[0]),
                                                      "%02d" % (d.year) + "%02d" % (d.month) + "%02d" % (d.day),
                                                      next.split(',')[0].strip(), next.split(',')[1],
                                                      next.split(',')[2], next.split(',')[3], next.split(',')[4],
                                                      next.split(',')[5], next.split(',')[6], next.split(',')[7],
                                                      next.split(',')[8], next.split(',')[9],
                                                      next.split(',')[10], next.split(',')[11], next.split(',')[12],
                                                      next.split(',')[13]],
                                                     index=['AirportCode', 'Hour', 'Date', 'TimeEET', 'TemperatureF',
                                                            'Dew PointF', 'Humidity', 'Sea Level PressureIn',
                                                            'VisibilityMPH', 'Wind Direction', 'Wind SpeedMPH',
                                                            'Gust SpeedMPH', 'PrecipitationIn', 'Events', 'Conditions',
                                                            'WindDirDegrees', 'DateUTC']), ignore_index=True)

dframe['TemperatureF'] = dframe['TemperatureF'].replace([''],np.nan)
dframe['Conditions'].astype(basestring)
dframe['Gust SpeedMPH'] = dframe['Gust SpeedMPH'].replace(['-'], '0')
dframe['Humidity'] = dframe['Humidity'].replace([''],'0')
dframe['PrecipitationIn'] = dframe['PrecipitationIn'].replace(['', 'N/A'], 'N/A')
dframe.to_csv(os.path.join(path, 'out.csv'), index=False)

####################################

dfAddress1=pd.read_csv(os.path.join(path, 'zzzzz.csv'))
df = pd.merge(dfAddress1, df, on='address', how='right')
dfMerge=pd.read_csv(os.path.join(path, 'out.csv'))
dfMerge['Date']=pd.to_datetime(dfMerge['Date'], format="%Y%m%d")
dfMerge['VisibilityMPH'].replace([-9999],[0],inplace=True)
dfMerge['Wind Direction']=dfMerge['Wind Direction'].astype(str)
dfMerge['Wind Direction'].replace([''],['N/A'],inplace=True)
dfMerge['Conditions']=dfMerge['Conditions'].astype(str)
dfMerge['Conditions'].replace([''],['N/A'],inplace=True)
dfMerge.drop(['TimeEET','DateUTC','PrecipitationIn'],inplace=True,axis=1)
grouped=dfMerge.groupby(['AirportCode','Date','Hour'],as_index=False)

#group columns to get the mean value of all other values
df3=grouped.mean()
df4=grouped.agg({'Wind Direction':lambda x:','.join(x),
                 'Conditions': lambda x: ','.join(x)})
df3['Wind Direction']=df4['Wind Direction']
df3['Conditions']=df4['Conditions']
df = pd.merge(df3, df, on=['AirportCode','Date','Hour'], how='right')
df=df.rename(columns = {'Dew PointF':'Dew_PointF','Sea Level PressureIn':'Sea_Level_PressureIn','Gust SpeedMPH':'Gust_SpeedMPH','Wind Direction':'Wind_Direction','address':'Address','Building Number':'Building_Number','Area(m_sq)':'Area','Consumption(kVA)':'Consumption','kVA/sq_m':'KWH','month':'Month',})
df.to_csv(os.path.join(path, 'Final.csv'), index=False)

#delete the temporary tables
os.remove(os.path.join(path, 'zzzzz.csv'))
os.remove(os.path.join(path, 'out.csv'))