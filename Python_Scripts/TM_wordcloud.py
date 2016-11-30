#Generate Excel Documents of wordcounts to used for Tableau wordcloud
#Kevin Zhai 

from openpyxl import load_workbook

wb = load_workbook('MASTER_1968-2015.xlsx')
sheet = wb['Sheet1']