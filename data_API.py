#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun May 30 23:37:27 2021

@author: liutzuli
"""

import urllib

url = 'https://data.cdc.gov.tw/api/3/action/datastore_search?resource_id=3c1e263d-16ec-4d70-b56c-21c9e2171fc7&limit=5&q=title:jones'  
fileobj = urllib.request.urlopen(url)
print(fileobj.read())

fileobj.read()
