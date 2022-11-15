# imports
from bs4 import BeautifulSoup
from urllib.request import urlopen, Request
import json
import pandas as pd

# web scraper
def apollo_scraper(url, spec, realm_str, class_list, return50 = False, full_data = False):
    """Function to scrape dps table from apollo-wow and outputs pd_dataframe.
    Inputs: url as string, talent spec as string (e.g. 'Assassination'), 
    realm as string (e.g. 'Apollo2'), return50 as bool (default 'True' returns only top 50 values)
    Outputs: dataframe with all variables from apollo table
    """
        
    #pretending to be a browser
    r = Request(url, headers = {'User-Agent': 'Mozilla/5.0'})
    
    #loading page data into soup object
    page = urlopen(r)
    html = page.read().decode("utf-8")
    soup = BeautifulSoup(html, "html.parser")
    
    #convert soup object to string list, containing rel info
    html_scrape = soup.find_all('script')
    get_ind = 0
    for i in range(len(html_scrape)):
        if spec in str(html_scrape[i]):
            get_ind = i
            
    if get_ind == 0:
        return print('INDEXING ERROR')
    
    html_scrape_ind = html_scrape[get_ind]
    html_string = str(html_scrape_ind)
    div_list = html_string.split('\n')
    
    #extracting the sublist with the relevant table here:    
    if spec == class_list[0]:
        rel_data = div_list[2]
    elif spec == class_list[1]:
        rel_data = div_list[3]
    elif spec == class_list[2]:
        rel_data = div_list[4]

    #cutting off and adding some string chars to get it into json format
    rel_data_for_json = rel_data[rel_data.index('data: ') + 6:rel_data.index("});")]
    rel_data_for_json = '{ "data": ' + rel_data_for_json + '}'
    
    #loading json data as dictionary 
    data = json.loads(rel_data_for_json)
    data = data['data']
    
    #converting to dataframe
    df = pd.DataFrame.from_dict(data)
    
    if df.empty == True:
        print('No entries for ' + spec + ' found!!', end = '')
        return df
    
    else: 
        if full_data == False:
            #'cuts' off wrong realm and resetting indices
            df = df[df.realm==realm_str]
            df.reset_index(inplace = True, drop = True)
    
            #turning string dps values to float
            df['dps'] = df['dps'].astype(float)
            df['length'] = df['length'].astype(float)
            df['avg_item_lvl'] = df['avg_item_lvl'].astype(float)

            print('Entries for ' + spec + ': ' + str(len(df)), end = '')
            return df
        else:
            #turning string dps values to float
            df['dps'] = df['dps'].astype(float)
            df['length'] = df['length'].astype(float)
            df['avg_item_lvl'] = df['avg_item_lvl'].astype(float)

            print('Entries for ' + spec + ': ' + str(len(df)), end = '')
            return df

# url strings 
#string start:
start_url = 'https://cata-twinhead.twinstar.cz/?'

#classes
Warrior = '&class=1'
War1 = 'Arms'
War2 = 'Fury'
War3 = 'Protection'
Warlist = [War1, War2, War3]

Paladin = '&class=2'
Pal1 = 'Holy'
Pal2 = 'Protection'
Pal3 = 'Retribution'
Pallist = [Pal1, Pal2, Pal3]

Hunter = '&class=3'
Hunt1 = 'Beast Mastery'
Hunt2 = 'Marksmanship'
Hunt3 = 'Survival'
Huntlist = [Hunt1, Hunt2, Hunt3]

Rogue = '&class=4'
Rog1 = 'Assassination'
Rog2 = 'Combat'
Rog3 = 'Subtlety'
Roglist = [Rog1, Rog2, Rog3]

Priest = '&class=5'
Pri1 = 'Discipline'
Pri2 = 'Holy'
Pri3 = 'Shadow'
Prilist = [Pri1, Pri2, Pri3]

Death_Knight = '&class=6'
Dk1 = 'Blood'
Dk2 = 'Frost'
Dk3 = 'Unholy'
Dklist = [Dk1, Dk2, Dk3]

Shaman = '&class=7'
Sham1 = 'Elemental'
Sham2 = 'Enhancement'
Sham3 = 'Restoration'
Shamlist = [Sham1, Sham2, Sham3]

Mage = '&class=8'
Mag1 = 'Arcane'
Mag2 = 'Fire'
Mag3 = 'Frost'
Maglist = [Mag1, Mag2, Mag3]

Warlock = '&class=9'
Warl1 = 'Affliction'
Warl2 = 'Demonology'
Warl3 = 'Destruction'
Warllist = [Warl1, Warl2, Warl3]

Druid = '&class=11'
Dru1 = 'Balance'
Dru2 = 'Feral Combat'
Dru3 = 'Restoration'
Drulist = [Dru1, Dru2, Dru3]

class_url_strings = [Warrior, Paladin, Hunter, Rogue, Priest, Death_Knight, Shaman, Mage, Warlock, Druid]

#shannox overall dps/hps tables:
shannox_10_nm = 'npc=53691'
shannox_10_hc = 'npc=54079'
shannox_25_nm = 'npc=53979'
shannox_25_hc = 'npc=54080'
#on this page best 100 dps / hps data should be accessible
shannox_all_list = [shannox_10_nm, shannox_10_hc, shannox_25_nm, shannox_25_hc]

#shannox dps-spec-specific dps tables:
shannox_10_nm_top = 'boss-kill-npc-top=53691'
shannox_10_hc_top = 'boss-kill-npc-top=54079'
shannox_25_nm_top = 'boss-kill-npc-top=53979'
shannox_25_hc_top = 'boss-kill-npc-top=54080'
shannox_top_list = [shannox_10_nm_top, shannox_10_hc_top, shannox_25_nm_top, shannox_25_hc_top]

#Lord Ryolith overall dps/hps tables:
ryo_10_nm = 'npc=52558'
ryo_10_hc = 'npc=52560'
ryo_25_nm = 'npc=52559'
ryo_25_hc = 'npc=52561'
#on this page best 100 dps / hps data should be accessible
ryo_all_list = [ryo_10_nm, ryo_10_hc, ryo_25_nm, ryo_25_hc]

#Lord Ryolith dps-spec-specific dps tables:
ryo_10_nm_top = 'boss-kill-npc-top=52558'
ryo_10_hc_top = 'boss-kill-npc-top=52560'
ryo_25_nm_top = 'boss-kill-npc-top=52559'
ryo_25_hc_top = 'boss-kill-npc-top=52561'
ryo_top_list = [ryo_10_nm_top, ryo_10_hc_top, ryo_25_nm_top, ryo_25_hc_top]

#Beth'tilac overall dps/hps tables:
beth_10_nm = 'npc=52498'
beth_10_hc = 'npc=53577'
beth_25_nm = 'npc=53576'
beth_25_hc = 'npc=53578'
#on this page best 100 dps / hps data should be accessible
beth_all_list = [beth_10_nm, beth_10_hc, beth_25_nm, beth_25_hc]

#Beth'tilac dps-spec-specific dps tables:
beth_10_nm_top = 'boss-kill-npc-top=52498'
beth_10_hc_top = 'boss-kill-npc-top=53577'
beth_25_nm_top = 'boss-kill-npc-top=53576'
beth_25_hc_top = 'boss-kill-npc-top=53578'
beth_top_list = [beth_10_nm_top, beth_10_hc_top, beth_25_nm_top, beth_25_hc_top]

#Alysrazor overall dps/hps tables:
alys_10_nm = 'npc=52530'
alys_10_hc = 'npc=54045'
alys_25_nm = 'npc=54044'
alys_25_hc = 'npc=54046'
#on this page best 100 dps / hps data should be accessible
alys_all_list = [alys_10_nm, alys_10_hc, alys_25_nm, alys_25_hc]

#Alysrazor dps-spec-specific dps tables:
alys_10_nm_top = 'boss-kill-npc-top=52530'
alys_10_hc_top = 'boss-kill-npc-top=54045'
alys_25_nm_top = 'boss-kill-npc-top=54044'
alys_25_hc_top = 'boss-kill-npc-top=54046'
alys_top_list = [alys_10_nm_top, alys_10_hc_top, alys_25_nm_top, alys_25_hc_top]

#Bale overall dps/hps tables:
bale_10_nm = 'npc=53494'
bale_10_hc = 'npc=53588'
bale_25_nm = 'npc=53587'
bale_25_hc = 'npc=53589'
#on this page best 100 dps / hps data should be accessible
bale_all_list = [bale_10_nm, bale_10_hc, bale_25_nm, bale_25_hc]

#Bale dps-spec-specific dps tables:
bale_10_nm_top = 'boss-kill-npc-top=53494'
bale_10_hc_top = 'boss-kill-npc-top=53588'
bale_25_nm_top = 'boss-kill-npc-top=53587'
bale_25_hc_top = 'boss-kill-npc-top=53589'
bale_top_list = [bale_10_nm_top, bale_10_hc_top, bale_25_nm_top, bale_25_hc_top]

#Majordomo overall dps/hps tables:
majo_10_nm = 'npc=52571'
majo_10_hc = 'npc=53857'
majo_25_nm = 'npc=53856'
majo_25_hc = 'npc=53858'
#on this page best 100 dps / hps data should be accessible
majo_all_list = [majo_10_nm, majo_10_hc, majo_25_nm, majo_25_hc]

#Majordomo dps-spec-specific dps tables:
majo_10_nm_top = 'boss-kill-npc-top=52571'
majo_10_hc_top = 'boss-kill-npc-top=53857'
majo_25_nm_top = 'boss-kill-npc-top=53856'
majo_25_hc_top = 'boss-kill-npc-top=53858'
majo_top_list = [majo_10_nm_top, majo_10_hc_top, majo_25_nm_top, majo_25_hc_top]

#Rag overall dps/hps tables:
rag_10_nm = 'npc=52409'
rag_10_hc = 'npc=53798'
rag_25_nm = 'npc=53797'
rag_25_hc = 'npc=53799'
#on this page best 100 dps / hps data should be accessible
rag_all_list = [rag_10_nm, rag_10_hc, rag_25_nm, rag_25_hc]

#Rag dps-spec-specific dps tables:
rag_10_nm_top = 'boss-kill-npc-top=52409'
rag_10_hc_top = 'boss-kill-npc-top=53798'
rag_25_nm_top = 'boss-kill-npc-top=53797'
rag_25_hc_top = 'boss-kill-npc-top=53799'
rag_top_list = [rag_10_nm_top, rag_10_hc_top, rag_25_nm_top, rag_25_hc_top]

# wrapper function
def apollo_scraper_wrapper(date, bossname, bosslist, realm = 'Apollo2', class_url_strings = class_url_strings):
  '''Wrapper function to ease scraping for each boss, each class, and each difficulty.
  Inputs are: 
  - date: string in format DDMMYYYY
  - bossname: literal string, boss-specific
  - bosslist: list defined above containing boss-specific URLs
  - realm_str: default = 'Apollo2'
  - class_url_strings: default = list defined above containing class-specific URLs
  '''

  # assigning variables
  today = '_' + date
  pickle = '.pkl'
  endstring = today + pickle
  realm_str = realm
  bossname = bossname
  boss_list = bosslist
  class_url_strings = class_url_strings 
  start_url = 'https://cata-twinhead.twinstar.cz/?'
  
  # difficulty list
  mode_list = ['_10nm', '_10hc', '_25nm', '_25hc']

  # classlist
  abbrev = ['_War_', '_Pal_', '_Hunt_', '_Rog_', '_Pri_', '_Dk_', '_Sham_', '_Mag_', '_Warl_', '_Dru_']
  list_of_classlists = [Warlist, Pallist, Huntlist, Roglist, Prilist, Dklist, Shamlist, Maglist, Warllist, Drulist]


  for idx, val in enumerate(list_of_classlists):
    # class-specific loop
    abbr = abbrev[idx]
    curr_class = val
    class_url = class_url_strings[idx]

    for idx2, val2 in enumerate(boss_list):
      # boss-specific loop
      current_boss = val2

      for talent_spec in curr_class:
        # talent-specific loop
        url = start_url + current_boss + class_url

        # run scraper
        tmp = apollo_scraper(url, talent_spec, realm_str, curr_class)
        
        # display progress
        print(' for ' + mode_list[idx2][1::])

        if tmp.empty:
          print('No file created for empty df object.')

        else:
          # save
          tmp.to_pickle(bossname + mode_list[idx2] + abbr + talent_spec + endstring)

    print('------ ' + bossname + ' ' + abbr[1:-1] + '-data finished downloading.' + ' ------')

  print('Done.')


# Shannox - Apollo2
apollo_scraper_wrapper('30052021', 'Shannox', shannox_top_list)

# Lord Ryolith - Apollo2 
apollo_scraper_wrapper('31052021', 'Lord Ryolith', ryo_top_list)

# Beth'tilac - Apollo2
apollo_scraper_wrapper('31052021', 'Bethtilac', beth_top_list)

# Alysrazor - Apollo2
apollo_scraper_wrapper('01062021', 'Alysrazor', alys_top_list)

# Baleroc - Apollo2
apollo_scraper_wrapper('31052021', 'Baleroc', bale_top_list)

# Majordomo - Apollo2
apollo_scraper_wrapper('31052021', 'Majordomo', majo_top_list)

# Ragnaros - Apollo2
apollo_scraper_wrapper('01062021', 'Ragnaros', rag_top_list)