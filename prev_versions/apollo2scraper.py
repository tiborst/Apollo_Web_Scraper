from bs4 import BeautifulSoup
from urllib.request import urlopen, Request

#when making this into a function, make a arg for boss_name to
#organize the data

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

#sanity check
if data_list[210] == rel_data_list[0]:
    print('yay')
else:
    print('nay')
    
rel_data = rel_data_list[0]     #clean list

#turning string into something useful
#now getting a list with only apollo2 data
#but first getting a list of all players as one list item
char_list = rel_data.split('{')
char_list = [i for i in char_list if 'dps' in i]

#only apollo 2 chars:
apollo2_chars = [i for i in char_list if 'Apollo2' in i]

#splitting at ','
new_apollo2_chars = [i.split(',') for i in apollo2_chars]
print(len(new_apollo2_chars))

'''
#test item from list to try to make it a dictionary
test_dic = new_apollo2_chars[5]
#cleaning list items    
for i in range(len(test_dic)):
    test_dic[i] = test_dic[i].replace('"', "")
    test_dic[i] = test_dic[i].replace('}', "")

#subsplit list into lists at ':'
sub_test_dic = [i.split(':') for i in test_dic]

#get only important list items (dps, name, realm, talent_spec, length, avg_item_lvl):
any_of_this = ['dps', 'realm', 'name', 'talent_spec', 'length', 'avg_item_lvl']

final_test_ls = []
for val in sub_test_dic:
    for val2 in any_of_this:
        if val2 in val:
            final_test_ls.append(val)
'''

#applying to all sublists
test_copy = new_apollo2_chars
print(len(test_copy))
#get only important list items (dps, name, realm, talent_spec, length, avg_item_lvl):
any_of_this = ['dps', 'realm', 'name', 'talent_spec', 'length', 'avg_item_lvl']

for sublist in test_copy:
    for i in range(len(sublist)):
        sublist[i] = sublist[i].replace('"', "")
        sublist[i] = sublist[i].replace('}', "")

#subsplit list into lists at ':'
for sublist in test_copy:
    sublist = [i.split(':') for i in sublist]

#create new list with sublists that only contain what I want
reduced_test_copy = test_copy
for ind,val in enumerate(reduced_test_copy):
    #list level
    append_list = []
    for item in reduced_test_copy[ind]:
        #sublist item level
        for anys in any_of_this:
            #iterating through check list
            if anys in item:
                append_list.append(item)
    reduced_test_copy[ind] = append_list

#to get items from this list of lists of lists:
#list[0]       = overview of first character
#list[0][0]    = first entry of pseudo dictionary
#list[0][0][0] = key of first entry
#list[0][0][1] = value of first entry
for ind,val in enumerate(reduced_test_copy):
    pseudo_dic = [i.split(':') for i in val]
    reduced_test_copy[ind] = pseudo_dic

#nicer name
output = reduced_test_copy
#only highest 50 dps
first_50 = output[:50]












