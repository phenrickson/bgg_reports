# import standard libraries
import numpy as np
import pandas as pd

# import scraping libraries
from selenium import webdriver
from selenium.common.exceptions import TimeoutException
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from bs4 import BeautifulSoup
import re

# import support functions
from time import sleep
from datetime import datetime
from tqdm import tqdm
import os
import json

# credentials
# define creds file path
creds_path = f"{os.path.expanduser('~')}/secrets/bgg_creds.json"

# get bgg credentials
with open(creds_path) as creds_file:
        creds_dict = json.load(creds_file)

#%% -------------------------------
#-- ---Define Scraping Functions---
#-- -------------------------------

## define function to validate driver's current url vs target url
def validate_url(driver, target_url):
    
    # check if redirected to login
    if driver.current_url == 'https://boardgamegeek.com/login?redirect_server=1':
        
        # find username and password textboxes
        username_textbox = driver.find_element('id', 'inputUsername')
        password_textbox = driver.find_element('id', 'inputPassword')
        
        # input creds
        username_textbox.send_keys(creds_dict['username'])
        password_textbox.send_keys(creds_dict['password'])
        
        # click submit
        driver.find_element('xpath', '//*[@id="mainbody"]/div/div/gg-login-page/div[1]/div/gg-login-form/form/fieldset/div[3]/button[1]').click()
            
    # check if redirected to home page
    elif driver.current_url == 'https://boardgamegeek.com/':
        
        # get webpage (re-send get request if timeout due to bad response)
        try:
            driver.get(target_url)
        except TimeoutException:
            driver.get(target_url)
        
    # return driver url
    return driver

# now scrape
# define webdriver options
options = webdriver.ChromeOptions()
options.add_argument('--incognito')
options.add_argument('--headless')

# define driver
# driver = webdriver.Chrome(ChromeDriverManager().install(), options=options)
driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()), options = options)
driver.maximize_window()
driver.set_page_load_timeout(600)

# access initial BoardGameGeek webpageinput
url = "https://boardgamegeek.com/browse/boardgame/page/1?sort=rank"
driver.get(url)

# get html string from boardgamegeek
soup = str(BeautifulSoup(driver.page_source, 'html.parser'))

# obtain total pages to scrape
tot_pages = re.findall('title="last page">\[(.*?)\]</a></div>', soup)

if len(tot_pages) > 1:
        raise Exception("\n>>> More than one value for total pages discovered")
else:
        tot_pages = int(tot_pages[0])
        
print(f"\n>>> Scraping {tot_pages} pages")

# obtain game id & name for first page
results = [{'page': 1,
        'timestamp': datetime.now(),
        'game_id': result[0],
        'raw_name': result[1],
        'tidy_name': result[2]} for result in re.findall('<a class="primary" href="/boardgame/(.*?)/(.*?)">(.*?)</a>', soup)]
        
# now loop
  # iterate through the rest of the pages)
for page in tqdm(range(2, tot_pages+1), desc='Scraping pages', total=tot_pages):

        # wait a few seconds
        sleep(2)
        
        # re-define url
        url = f"https://boardgamegeek.com/browse/boardgame/page/{page}?sort=rank"
        
        # get webpage (re-send get request if timeout due to bad response)
        try:
            driver.get(url)
        except TimeoutException:
            driver.get(url)
        
        # validate url
        driver = validate_url(driver, url)
        
        # get html string from page
        soup = str(BeautifulSoup(driver.page_source, 'html.parser'))
        
        # expand w/ page results
        results.extend( [{'page': page,
                          'timestamp': datetime.now(),
                          'game_id': result[0],
                          'raw_name': result[1],
                          'tidy_name': result[2]} for result in re.findall('<a class="primary" href="/boardgame/(.*?)/(.*?)">(.*?)</a>', soup)] )
                          
# check for missing pages
missed_pages = list(set(np.unique([item['page'] for item in results]))^set(range(1,tot_pages+1)))

# print missing pages
print(f"\n>>> Missing pages: {set(np.unique([item['page'] for item in results]))^set(range(1, tot_pages+1))}")

 # iterate through the missed pages again
for page in tqdm(missed_pages, desc='Scraping missed pages...', total=len(missed_pages)):

        # wait a few seconds
        sleep(2)
        
        # re-define url
        url = f"https://boardgamegeek.com/browse/boardgame/page/{page}?sort=rank"
        
        # get webpage (re-send get request if timeout due to bad response)
        try:
            driver.get(url)
        except TimeoutException:
            driver.get(url)
        
        # validate url
        driver = validate_url(driver, url)
        
        # get html string from page
        soup = str(BeautifulSoup(driver.page_source, 'html.parser'))
        
        # expand w/ page results
        results.extend( [{'page': page,
                          'timestamp': datetime.now(),
                          'game_id': result[0],
                          'raw_name': result[1],
                          'tidy_name': result[2]} for result in re.findall('<a class="primary" href="/boardgame/(.*?)/(.*?)">(.*?)</a>', soup)] )
                          
print(f"\n>>> Saving scraped pages")

# close driver
driver.close()

# define path to save file
save_path = f"{os.getcwd()}/scraped/bgg_ids_{datetime.now().strftime('%Y-%m-%d')}.csv"

# save data
pd.DataFrame(results).to_csv(save_path, index=False)
