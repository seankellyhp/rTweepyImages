import tweepy
import pandas as pd
import time 

def search_twitter(bearer_key, query, start_time, end_time, max_results, page_limit):

    client = tweepy.Client(bearer_key, wait_on_rate_limit=True)

    # get tweets from the API
    tweets = []
    for tweet in tweepy.Paginator(client.search_all_tweets, query=query,
                                    start_time=start_time,
                                    end_time=end_time,
                                    tweet_fields=['context_annotations', 'created_at','source','public_metrics',
                                                    'lang','referenced_tweets','reply_settings','conversation_id',
                                                    'in_reply_to_user_id','geo'],
                                    expansions=['attachments.media_keys','author_id','geo.place_id'],
                                    media_fields=['preview_image_url','type','public_metrics','url'],
                                    place_fields=['place_type', 'geo'],
                                    user_fields=['name', 'username', 'location', 'verified', 'description',
                                                'profile_image_url','entities'],
                                    max_results=max_results, limit = page_limit):

                                    tweets.append(tweet)
                                    time.sleep(1)



    return tweets
