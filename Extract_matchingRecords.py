# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

#https://www.njdoctorlist.com

import pandas as pd

fields = ['NPI','Provider Last Name (Legal Name)','Provider First Name','Provider Business Practice Location Address City Name','Provider Business Practice Location Address State Name','Healthcare Provider Taxonomy Code_1']
df1 = pd.read_csv("C:/Users/aksha/Downloads/NPPES_Data_Dissemination_September_2017/npidata_20050523-20170910.csv",usecols=fields)

#NPI

#ProviderFirstName
#ProviderLastName(LegalName)

df2=pd.read_csv("C:/Users/aksha/Downloads/NPIs 2008-2013.csv")
list(df1)

df3 = pd.merge(df1,df2, on =['NPI'],how='inner')
df3.to_csv("C:/Users/aksha/Downloads/matching.csv", sep=',')