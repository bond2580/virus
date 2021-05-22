#!/usr/bin/env python
# coding: utf-8

# In[214]:


import os
from selenium import webdriver
from selenium.webdriver import ChromeOptions
import glob
import shutil
import time


# In[215]:


os.chdir("C:/Users/Path/") #ディレクトリの移動
current_dir = os.getcwd() #カレントディレクトリの取得
print(current_dir)

# In[216]:


#ダウンロードしたデータの保存先となるディレクトリを作成
os.mkdir("data")
tmp_download_dir = f'{current_dir}\\data' #
print(tmp_download_dir)


# In[217]:

#Chrome上でのダウンロードしたファイルの保存先をdataに変更
options = webdriver.ChromeOptions()
prefs = {'download.default_directory' : tmp_download_dir}
options.add_experimental_option('prefs', prefs)


# In[218]:

#webdriverを起動する自作関数
def initialize():
    #options = webdriver.ChromeOptions()
    driver_file = r"C:\Path\chromedriver.exe"
    return webdriver.Chrome(driver_file, options=options)


# In[219]:

#研究データの提供先
URL = "https://toyokeizai.net/sp/visual/tko/covid19/en.html"


# In[220]:

#ブラウザ(Chrome)を起動
browser = initialize()


# In[221]:

#URLをブラウザに入力
browser.get(URL)
time.sleep(5)


# In[222]:

#研究に必要なデータをクリック操作によってダウンロード
element = browser.find_element_by_partial_link_text("cases_total.csv")
element.click()
element = browser.find_element_by_partial_link_text("recovery_total.csv")
element.click()
element = browser.find_element_by_partial_link_text("death_total.csv")
element.click()
element = browser.find_element_by_partial_link_text("pcr_positive_daily.csv")
element.click()

print("データダウンロード完了")

# In[223]:


#ダウンロードしたファイルのファイル名を取得
time.sleep(15)
download_fileName = glob.glob(f'{tmp_download_dir}\\*.*')


# In[224]:


print(download_fileName[0])


# In[225]:


import pandas as pd


#SEIRモデルによるCOVID-19の流行予測に必要な変数を作成
Infected = pd.read_csv(download_fileName[0])
Infected.columns = ["日付", "I"]
Exposed = Infected
Death = pd.read_csv(download_fileName[1])
Recovery = pd.read_csv(download_fileName[3])
total = pd.read_csv(download_fileName[2])

df = pd.merge(Infected, Death, how="inner", on="日付")
df = pd.merge(df, Recovery, how="inner", on="日付")
df = pd.merge(df, Exposed, how="inner", on="日付")


#これまでの陽性者数の合計を求める
tmp = 0
s = []
for i in total["PCR 検査陽性者数(単日)"]:
    i = i+tmp
    s.append(i)
    tmp = i


# In[226]:


df.columns = ["date", "I", "death", "recover", "E"]
df["R"] = df["death"] + df["recover"]
df["S"] = 120000000 - pd.DataFrame(s) #120000000は日本の総人口(出生, 死亡による変化はないものとする)


# In[227]:


# ダウンロードしたファイルを移動
#shutil.move(download_fileName[0], current_dir)

#データフレームを保存
df.to_csv("COVID-19.csv", index=False, encoding="utf-8")

# tmpDownloadフォルダの削除
shutil.rmtree(tmp_download_dir)


# In[228]:

#ブラウザを閉じる
browser.close()
browser.quit()
print("Good Bye.")

# In[ ]:




