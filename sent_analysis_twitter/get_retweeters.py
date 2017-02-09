import json
import tweepy
from tweepy import OAuthHandler
import codecs
import os
import time
import sys
import pandas as pd

#TWITTER
consumer_key = ""
consumer_secret = ""
access_key = ""
access_secret = ""

auth = OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_key, access_secret)
api = tweepy.API(auth)

#Cambiar tienda
q = 'homedepot';

tweets = pd.read_csv(q + "_procesed.csv", sep='~',dtype={'id': str, 'retweeted_id': str})
file = codecs.open(q + '_retweets.csv', 'a', 'utf-8')
header = "%s,%s,%s\n" % ('source', 'target','weight')
file.write(header)

retweets = tweets[tweets['is_retweet'] == 1]
retweets['id'] = retweets['retweeted_id']
retweets['user'] = retweets['retweeted_user']
retweets = retweets.append(tweets['is_retweet'] == 0)
retweets = retweets.drop_duplicates(['id'])

top_retweets = retweets.sort_values(['retweet_count','favorite_count','followers_count'], ascending=False).head(50)

#for i, row in []:
for i, row in top_retweets.iterrows():
	try:
		print row['id']
		people = api.retweets(str(row['id']))
	except tweepy.TweepError as e:
		print e
		print "sleeping..."
		time.sleep(60 * 15)
		continue
	for user in people:
		str_retweet = "%s,%s,%s\n" % (row['user'],user.user.screen_name,1)
		file.write(str_retweet)