import json
import tweepy
from tweepy import OAuthHandler
import codecs
import os
import time
import sys  

#encode
reload(sys)  
sys.setdefaultencoding('utf8')

#DATOS A BUSCAR
consumer_key = ""
consumer_secret = ""
access_key = ""
access_secret = ""

auth = OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_key, access_secret)
api = tweepy.API(auth)

#TERMINO A BUSCAR
query = 'homedepot'

c = tweepy.Cursor(api.search,q = query, include_entities=True, lang = 'en').items()
file = codecs.open(query + '.csv', 'a', 'utf-8')
header = "%s~%s~%s~%s~%s~%s~%s~%s~%s~%s~%s~%s~%s~%s~%s~%s~%s~%s~%s~%s~%s~%s\n" % ('id', 'user', 'date_created', 'lang', 
		'favorite_count','retweet_count','is_retweet', 'retweeted_id', 'retweeted_user','reply_status','reply_user',
		'text','country','city','ln','lt','source','followers_count',
		'friends_count','user_description','media','urls')

file.write(header)

while True:
	try:
		tweet = c.next()
		tweet_id = tweet.id_str
		user = tweet.user.screen_name
		created = tweet.created_at
		lang = tweet.lang
		favorite_count = tweet.favorite_count
		retweet_count = tweet.retweet_count
		
		is_retweet = 1 if hasattr(tweet, 'retweeted_status') else 0
		retweeted_id = tweet.retweeted_status.id_str if hasattr(tweet, 'retweeted_status') else ''
		retweeted_user = tweet.retweeted_status.user.screen_name if hasattr(tweet, 'retweeted_status') else ''
		
		reply_user = tweet.in_reply_to_screen_name
		reply_status = tweet.in_reply_to_status_id
		
		text = tweet.text.encode("utf-8").replace('\n', ' ').replace('\r', '').replace('~', '')
		country = tweet.place.country if hasattr(tweet.place, 'name') else ''
		city = tweet.place.name if hasattr(tweet.place, 'name') else tweet.user.location.replace('~', '') if hasattr(tweet.user, 'location') else ''
		ln = tweet.coordinates.coordinates[0] if hasattr(tweet.coordinates, 'coordinates') else ''
		lt = tweet.coordinates.coordinates[1] if hasattr(tweet.coordinates, 'coordinates') else ''
		source = tweet.source
		followers_count= tweet.user.followers_count
		friends_count = tweet.user.friends_count
		user_description = tweet.user.description.encode("utf-8").replace('\n', ' ').replace('\r', '').replace('~', '')
		media = tweet.entities.media if hasattr(tweet.entities, 'media') else ''
		urls = tweet.entities.urls if hasattr(tweet.entities, 'urls') else ''
		str_tweet = "%s~%s~%s~%s~%s~%s~%s~%s~%s~%s~%s~%s~%s~%s~%s~%s~%s~%s~%s~%s~%s~%s\n" % \
		(tweet_id,user, created, lang, favorite_count,retweet_count, is_retweet,retweeted_id,retweeted_user,
		reply_status, reply_user, text, country, city, ln, lt, source, followers_count, friends_count, user_description,media,urls)
		file.write(str_tweet)
	except tweepy.TweepError as e:
		print e
		time.sleep(60 * 15)
		continue
	except StopIteration:
		break