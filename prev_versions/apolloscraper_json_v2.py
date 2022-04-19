from bs4 import BeautifulSoup
from urllib.request import urlopen, Request
import json
import pandas as pd

def apollo_scraper(url, spec, realm_str, return50 = True):
    """Function to scrape dps table from apollo-wow and outputs pd_dataframe.
    Inputs: url as string, talent spec as string (e.g. 'Assassination'), 
    realm as string (e.g. 'Apollo2'), return50 as bool (default 'True' returns only top 50 values)
    Outputs: dps, ilvl, length of fight (all variables to use for calculations)
    """
    
    #pretending to be a browser
    r = Request(url, headers = {'User-Agent': 'Mozilla/5.0'})
    
    #loading page data into soup object
    page = urlopen(r)
    html = page.read().decode("utf-8")
    soup = BeautifulSoup(html, "html.parser")
    
    ##html as list of strings, each item is one 'div' class/object?
    #html_scrape = soup.find_all('div')
    #html_scrape_12 = html_scrape[12]        #[12] is the right div object
    #html_string = str(html_scrape_12)
    #div_list = html_string.split('\n')
    
    #convert soup object to string list, containing rel info
    html_scrape = soup.find_all('script')
    get_ind = 0
    for i in range(len(html_scrape)):
        if spec in str(html_scrape[i]):
            #print('It happened')
            get_ind = i
            
    if get_ind == 0:
        return print('INDEXING ERROR')
    
    html_scrape_ind = html_scrape[get_ind]
    html_string = str(html_scrape_ind)
    div_list = html_string.split('\n')
    
    #extracting the sublist with the relevant table here:    
    for i in div_list:
        if spec in i:
            rel_data = i

    #cutting off and adding some string chars to get it into json format
    rel_data_for_json = rel_data[rel_data.index('data: ') + 6:rel_data.index("});")]
    rel_data_for_json = '{ "data": ' + rel_data_for_json + '}'
    
    #loading json data as dictionary 
    data = json.loads(rel_data_for_json)
    data = data['data']
    
    #converting to dataframe
    df = pd.DataFrame.from_dict(data)
    
    #'cuts' off wrong realm and resetting indices
    df = df[df.realm==realm_str]
    df.reset_index(inplace = True, drop = True)
    
    #turning string dps values to float
    df['dps'] = df['dps'].astype(float)
    df['length'] = df['length'].astype(float)
    df['avg_item_lvl'] = df['avg_item_lvl'].astype(float)

    if return50 == True:
        #return top 50 
        df_50 = df[:50]
        df_dps = df_50['dps']
        df_len = df_50['length']
        df_ilvl = df_50['avg_item_lvl']
        print('No of values: ' + str(len(df_dps)))
    
    else:
        #return all data available and print length of data
        df_dps = df['dps']
        df_len = df['length']
        df_ilvl = df['avg_item_lvl']
        print('No of values: ' + str(len(df_dps)))
        
    return df, df_dps, df_len, df_ilvl
    
    
    
    
#trying it out for assa on apollo2 majordomo HC on 21.03.21:
url       = 'https://cata-twinhead.twinstar.cz/?boss-kill-npc-top=53858&class=4'
spec      = 'Assassination'
realm_str = 'Apollo2'
return50  = False
majohc_assa_all, majohc_assa_dps, majohc_assa_len, majohc_assa_ilvl = apollo_scraper(url, 
                                                                    spec, 
                                                                    realm_str, 
                                                                    return50 = False)
#combat on majordomo heroic on 21.03.21:
spec = 'Combat'
majohc_combat_dps, majohc_combat_len, majohc_combat_ilvl = apollo_scraper(url,
                                                                          spec,
                                                                          realm_str,
                                                                          return50 = True)

#subtlety on majordomo heroic on 21.03.21:
spec = 'Subtlety'
majohc_sub_dps, majohc_sub_len, majohc_sub_ilvl = apollo_scraper(url,
                                                                 spec,
                                                                 realm_str)


#just trying out stuff here
import matplotlib.pyplot as plt
#assa ilvl x dps
#merging two pandas series into a dataframe to plot it against each other
plot_df = pd.concat([majohc_assa_dps, majohc_assa_ilvl], axis = 1)
#plot the data with this syntax
plot_df.plot(x = 'dps', y = 'avg_item_lvl', kind = 'scatter')
plt.show()
import numpy as np
from sklearn.linear_model import LinearRegression
#converting dfs to np arrays and reshaping 'X' because it has to be one column array
majohc_assa_dps = majohc_assa_dps.to_numpy()
majohc_assa_ilvl = majohc_assa_ilvl.to_numpy()
majohc_assa_dps = majohc_assa_dps.reshape((-1, 1))
#calling the model and fitting it to my data
model = LinearRegression()
model.fit(majohc_assa_dps, majohc_assa_ilvl)
#getting R squared:
r_sq = model.score(majohc_assa_dps, majohc_assa_ilvl)


























