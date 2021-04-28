#%%
from profanity_check import predict, predict_prob
import praw
import csv
import time
import re
import os
from pycorenlp.corenlp import StanfordCoreNLP
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
from itertools import groupby, product
import warnings
import enchant

# lexicon for known english words
words = enchant.Dict("en")
is_known_word = words.check

## coreNLP sent. analysis
def getSentiment(text):
    ## connect to CoreNLP server
    host = "http://localhost"
    port = "9000"
    nlp = StanfordCoreNLP(host + ":" + port)

    # annotate text
    output = nlp.annotate(
    text,
    properties={
        "outputFormat": "json",
        "annotators": "sentiment"
    }
    )

    # grab sentiment
    total_sent = 0
    n = 0
    for sen in output['sentences']:
        total_sent = total_sent + int(sen["sentimentValue"])
        n = n + 1

    # avoid divide by 0
    if n != 0:
        return total_sent / n
    else:
        raise Exception("Comment length 0")

def remove_consecutive_dups(s):
    return re.sub(r'(?i)(.)\1+', r'\1', s)

def all_consecutive_duplicates_edits(word, max_repeat=float('inf')):
    chars = [[c*i for i in range(min(len(list(dups)), max_repeat), 0, -1)]
             for c, dups in groupby(word)]
    return map(''.join, product(*chars))

def isnan(string):
    """
  checks if input is a nan (works for strings)
  """
    return string != string

def remove_elongations_bro(line):
    """
  Removes elongated words.
  """
    if (isnan(line)) | (line is None):
        warnings.warn('This is a nan or NoneType line...\n')
        return None

    else:
        output = [next((e for e in all_consecutive_duplicates_edits(s)
                        if e and is_known_word(e)), remove_consecutive_dups(s))
                  for s in re.split(r'(\W+)', line)]
        line_no_elongs = ''.join(output)
        return line_no_elongs

def censor_profanity(comment_text, profanity_threshold=0.9):
    """
  Replaces profanity using a (probably) inefficient use of the alt-profanity-filter.

  Input: comment_text (str)
  Output: comment text with profane words censored 
  """
    repl_text = [(x,f"{x[0]}{''.join(['*' for x in range(len(x)-1)])}") for x in comment_text.split(' ') if predict_prob([x])[0]>profanity_threshold]
    comment_text_censored = comment_text
    for text in repl_text:
        comment_text_censored = comment_text_censored.replace(text[0], text[1])
    return comment_text_censored

#
# auth_file= open('drive/My Drive/boo_meter/auth.txt')
# auth_file = auth_file.read()
# auth_file = auth_file.split(', ')
# auth_file[0]

## INIT GLOBAL VARS
chunk_size = 5000
num_chunks = 0

## INIT REDDIT INSTANCE
auth_file= open('auth.txt')
auth_file = auth_file.read()

auth_file = auth_file.split(', ')
client_id = auth_file[0]
client_secret = auth_file[1]
username = auth_file[2]
pw = auth_file[3]
#%%
# print(pw)
#%%
reddit = praw.Reddit(client_id=client_id,
                     client_secret=client_secret,
                     user_agent="nfl boo meter 'by' nickwan",
                     username=username,
                     password=pw,
                     check_for_async=False)

## INIT PROFANITY FILTER

## Note: don't think we need this anymore; replaced this with a faster filter
# pf = ProfanityFilter()
# pf.censor_whole_words = False

## INIT VADER SENTIMENT
vader = SentimentIntensityAnalyzer()

## INIT TEXTCLEAN R

## Note: i don't think we need this since i rewrote the elongation func -nw 4-20-21
# r_library_directory = 'path/to/r/library' #looks like C:/Users/caiob/Documents/R/win-library/3.6
# textclean = importr('textclean', lib_loc = r_library_directory)
# importr('stringi', lib_loc = r_library_directory)

## INIT SAVE DIRECTORY
# comments_directory = "path/to/directory"
comments_directory = 'data'

with open(f'{comments_directory}/comments.csv', 'w+', newline='') as csvfile:
    # initiate csv writer
    comment_writer = csv.writer(csvfile, delimiter='\t',
                            quotechar='|', quoting=csv.QUOTE_MINIMAL)
    # write header
    comment_writer.writerow(["timestamp", "team", "body", "length",
                             "sentiment", "subreddit", "boo", "id", "body_full"])

# setup multireddit
team_subreddits = ["KansasCityChiefs", "raiders", "DenverBroncos", "Chargers", "Colts",
                   "Tennesseetitans", "Texans", "Jaguars", "bengals", "steelers",
                   "ravens", "Browns", "miamidolphins", "nyjets", "buffalobills",
                   "Patriots", "cowboys", "NYGiants", "eagles", "Redskins", "CHIBears",
                   "GreenBayPackers", "detroitlions", "minnesotavikings", "falcons",
                   "Saints", "panthers", "buccaneers" ,"AZCardinals", "49ers",
                   "LosAngelesRams", "Seahawks"]

team_names = ["Chiefs", "Raiders", "Broncos", "Chargers", "Colts", "Titans",
              "Texans", "Jaguars", "Bengals", "Steelers", "Ravens", "Browns",
              "Dolphins", "Jets", "Bills", "Patriots", "Cowboys", "Giants",
              "Eagles", "Redskins", "Bears", "Packers", "Lions", "Vikings",
              "Falcons", "Saints", "Panthers", "Buccaneers", "Cardinals",
              "49ers", "Rams", "Seahawks"]
teams_dict = dict(zip(team_subreddits,team_names))

while(True):
    # loop through comments
    n = 0
    behind_flag = False
    for idx, comment in (enumerate(reddit.subreddit("+".join(team_subreddits)+"+nfl").stream.comments(skip_existing=True))):
        ## this is for testing. 5 iterations = ~1 min of comments
        # if idx==50:
        #     break
        # grab time and id

        if behind_flag:
            reddit = praw.Reddit(
                client_id=client_id,
                client_secret=client_secret,
                user_agent="nfl boo meter 'by' nickwan",
                username=username,
                password=pw,
                check_for_async=False
            )

        comment_time = comment.created_utc #time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(comment.created_utc))
        comment_id = comment.id

        # control for falling behind
        if time.time() - comment_time > 30 and not behind_flag:
            print("BEHIND, INITIATING CATCHUP")
            behind_flag = True
            continue
        elif behind_flag:
            if time.time() - comment_time <= 5:
                print("CAUGHT UP, RESUMING AS USUAL")
                behind_flag = False
            continue

        # grab text and remove new lines
        comment_text = comment.body.replace("\n", "")
        comment_text = comment_text.replace("\t", "")
        comment_text = re.sub(r"[^a-zA-Z0-9 !?,.()]+", '', comment_text)
        comment_length = len(comment_text)

        # remove elongations from text

        try:
            comment_text_clean = remove_elongations_bro(comment_text.lower())

            # this is the old code -nw 4/20/21
            # comment_text_clean = textclean.replace_word_elongation(comment_text.lower())[0]
            # if "bo" in comment_text_clean.split(" ") or comment_text_clean == "bo":  ### GO RAVENS - Disclaimer: @SeanFromSeabeck blackmailed me to put this here
            #     comment_text_clean = comment_text_clean.replace("bo","boo")
        except:
            print("ERROR: WORD ELONGATION CLEANER FAILED")

        # grab subreddit team
        comment_subr = comment.subreddit.display_name
        if comment_subr == "nfl":
            comment_team = comment.author_flair_text
            if (comment_team not in team_names):
                continue
        else:
            comment_team = teams_dict[comment_subr]
            comment_subr = "team"

        # get sentiment
        try:
            # check with the revision history on what this old sentiment package was
            #comment_sent = getSentiment(comment.body)
            comment_sent = vader.polarity_scores(comment_text_clean)['compound']
        except:
            print("ERROR: SENTIMENT ANALYSIS FAILED")

        # sentiment heuristics
        if comment_text_clean in ["let's go", "lets go", "fuck yes",
                                  "fuck yeah", 'lfg', "let's fucking go",
                                  'lets fucking go']:
            comment_sent = .7
        elif comment_text_clean in ["boo"]:
            comment_sent = -1

        # get goodell boo-meter
        if "boo" in comment_text_clean.split(" ") or comment_text_clean == "boo":
            comment_boo = 1
        else:
            comment_boo = 0

        # visualize and write
        print([comment_time, comment_team, comment_text,
               comment_length, comment_sent, comment_subr])

        # filter out profanity
        try:
            comment_text_censored = censor_profanity(comment_text)

            # from the old repo -nw 4/21/21
            # comment_text_censored = pf.censor(comment_text)
        except:
            print("UNSUPPORTED CHARACTER IN PROFANITY CENSOR")
            continue


        ## keep track of chunks to cleanup after chunk size comments
        if n >= chunk_size:
            print('CHUNK SIZE HIT, STARTING NEW FILE')
            t = time.time()
            n = -1
            with open(f'{comments_directory}/comments_temp.csv', 'w', newline='') as csvfile:
                # initiate csv writer
                comment_writer = csv.writer(csvfile, delimiter='\t',
                                            quotechar='|',
                                            quoting=csv.QUOTE_MINIMAL)
                # write header
                comment_writer.writerow(["timestamp", "team", "body", "length",
                                         "sentiment", "subreddit", "boo", "id",
                                         "body_full"])

        ## write to temp file
        if n == -1:
            try:
                with open(f'{comments_directory}/comments_temp.csv', 'a', newline='') as csvfile:
                    # initiate csv writer
                    comment_writer = csv.writer(csvfile, delimiter='\t',
                                                quotechar='|',
                                                quoting=csv.QUOTE_MINIMAL)

                    comment_writer.writerow([comment_time, comment_team,
                                             comment_text_censored,
                                             comment_length, comment_sent,
                                             comment_subr, comment_boo,
                                             comment_id, comment_text])
            except:
                print("ERROR: CONFLICT IN USING TEMP FILE OR UNSUPPORTED CHARACTER")

        # write file
        try:
            with open(f'{comments_directory}/comments.csv', 'a', newline='') as csvfile:
                # initiate csv writer
                comment_writer = csv.writer(csvfile, delimiter='\t',
                                            quotechar='|', quoting=csv.QUOTE_MINIMAL)
                comment_writer.writerow([comment_time, comment_team,
                                         comment_text_censored, comment_length,
                                         comment_sent, comment_subr,
                                         comment_boo, comment_id, comment_text])
                if n !=-1 :
                    n += 1
        except:
            print("ERROR: CONFLICT IN USING FILE OR UNSUPPORTED CHARACTER")

        ## write to new file after 2 minutes
        if n == -1 and time.time() - t >= 15:
            print("CHUNK TIME PASSED, FIXING FILE")
            # swap files
            os.rename(f'{comments_directory}/comments.csv',
                      f'{comments_directory}/chunks/comments_{str(num_chunks)}.csv')

            os.rename(f'{comments_directory}/comments_temp.csv',
                      f'{comments_directory}/comments.csv')

            # update vars
            n = 0
            num_chunks += 1
    ## this is for testing. 5 iterations = ~1 min of comments
    # if idx==50:
    #     break

    # if 503 server overload, pause and try again
    time.sleep(2)


# %%
