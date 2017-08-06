
import pandas as pd
from konlpy.tag import Twitter
twitter = Twitter()
#ex) print(twitter.pos(u'이것도 되나욬ㅋㅋ',norm=True, stem=True))

path='/Users/kims/'

# file1
file1 = pd.read_csv(path+'comments_17_df.csv')
file1.head()

# konlpy file1
text = []
len(file1)

for i in range(0,len(file1)):
    text_spider = twitter.pos(file1.loc[i,'value'],norm=True, stem=True) 
    text.append(text_spider)

text
text_df=pd.DataFrame.from_records(text)
text_df=text_df.stack()

text_df.to_csv('text_17.csv', encoding='utf-8')

# file2
file2 = pd.read_csv(path+'comments_12_df.csv')
file2.head()

# konlpy file2
text = []
len(file2)

for i in range(0,len(file2)):
    text_spider = twitter.pos(file2.loc[i,'value'],norm=True, stem=True) 
    text.append(text_spider)

text_df=pd.DataFrame.from_records(text)
text_df=text_df.stack()    
text_df.to_csv('text_12.csv', encoding='utf-8')
