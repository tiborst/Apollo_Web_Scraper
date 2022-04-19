from bs4 import BeautifulSoup
from urllib.request import urlopen, Request
import json
import pandas as pd

#when making this into a function, make a arg for boss_name to
#organize the data

##to do:
    #make it a function
    #see if lists or np.arrays are needed for calculations
    #and mapping
    #rename everything nicely
    #get accustomed to matplotlib
    #make a loop over different urls? (maybe it might be 
    #possible to 'autofill' the last query in url)
    
    #function:
        #inputs: url, exact talent spec, realm, ???
        #outputs: dps, ilvl, length,???
        
    #what to calculate:
        #descriptive plots of max dps and average dps
        #over class and over spec
        #t-tests between classes? lol
        #correlation of fight length and dps and
        #ilvl and dps
        #overall correlation of class / spec and dps

url = "https://cata-twinhead.twinstar.cz/?boss-kill-npc-top=54080&class=4"

#pretending to be a browser
r = Request(url, headers = {'User-Agent': 'Mozilla/5.0'})

#loading page data into soup object
page = urlopen(r)
html = page.read().decode("utf-8")
soup = BeautifulSoup(html, "html.parser")


# <div class="listview" id="listview-generic"></div>,
all_the_stuff = soup.find_all('div')

#[12] is the one with the data
all_data = all_the_stuff[12]

str_all_data = str(all_data)

data_list = str_all_data.split('\n')
#ind[210] is what I whant (for reference)
rel_substring = 'Assassination'

#this is a 1 item list containing the huge string of all 
#apollo1 and apollo2 best dps as string
rel_data_list = [i for i in data_list if rel_substring in i]

rel_data = rel_data_list[0]     #clean list

#cutting off unnessecary ends
relevant = rel_data[rel_data.index('data: ')+6:rel_data.index("});")]

relevant = '{ "data": ' + relevant + '}'

#loading json as dictionary
data = json.loads(relevant)
data = data['data']

#turning it into a useless dataframe
data_frame = pd.DataFrame.from_dict(data)

df = data_frame
#'cuts' off Apollo1 data, gotta make Apollo2 str an input
df = df[df.realm=='Apollo2']

#turning string dps values to float
df['dps'] = df['dps'].astype(float)
df['length'] = df['length'].astype(float)
df['avg_item_lvl'] = df['avg_item_lvl'].astype(float)


#calculating average top 50 dps:
df_50 = df[:50]
df_50.reset_index(inplace=True, drop=True)

#top 50 dps
df_50_dps = df_50['dps']
df_50_dps_desc = df_50_dps.describe()

#if I should need this, but prolly not
#conf_int_upper = df_50_dps_desc['mean'] + (1.96*((df_50_dps_desc['std'])/50))
#conf_int_lower = df_50_dps_desc['mean'] - (1.96*((df_50_dps_desc['std'])/50))


#top 50 fight lengths
df_50_len = df_50['length']
df_50_len_desc = df_50_len.describe()

#top 50 item levels
df_50_ilvl = df_50['avg_item_lvl']
df_50_ilvl_describe = df_50_ilvl.describe()


#UNNESSECARY BELOW!!
#check if conversion to list is nessecary for calculations
#and plotting
#this one converts the top 50 dps values to a regular list
#bc dataframes apparently suck
#dps_50_str = df_50['dps'].tolist()
#dps_50 is the float list with dps values
#dps_50 = []
#for val in dps_50_str:
#    dps_50.append(float(val))
###UNNESSECARY END!




























