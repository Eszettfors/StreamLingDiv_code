import LID
import pandas as pd
import regex as re

# this script takes the artitst title and lyrics from the MGD + genius data and applies LID to it

df_lyrics = pd.read_csv("data/processed/artist_title_lyrics_all_matches.csv")

# filter away cases that could not be matched
df_lyrics = df_lyrics[df_lyrics.lyrics.notnull()]

def remove_duplicate_lines(lyrics):
    # removes repeated phrases from each lyrics
    lines = lyrics.splitlines()

    deduped = []
    for line in lines:
        line = line.strip()

        if line and (not deduped or line != deduped[-1]):
            deduped.append(line)

    return " ".join(deduped)



def reduce_n_grams(text, n=1):
    # detects repeatings of a given n gram, e.g., n = 1 takes hey hey hey --> hey or n = 2; Hey you hey you --> hey you
    words = text.split()
    result = []

    i = 0
    while i < len(words):
        if i + 2*n <= len(words):
            phrase = words[i:i+n]
            next_phrase = words[i+n:i+2*n]

            if phrase == next_phrase:
                result.extend(phrase)
                i += n

                while i + n <= len(words) and words[i:i+n] == phrase:
                    i += n

                continue

        result.append(words[i])
        i += 1

    return " ".join(result)


def clean_lyrics(lyrics : str):
    # takes a lyrics and preprocesses it for LID
    # remove urls
    lyrics = re.sub(r'http\S+|www\.\S+', ' ', lyrics)

    # remove anything between [] --> indicates part of song e.g. Chorus
    lyrics = re.sub(r'\[[^\]]*\]', ' ', lyrics)

    # remove duplicate lines
    lyrics = remove_duplicate_lines(lyrics)

    # remove punctuation but keep unicode letters or numbers
    lyrics = re.sub(r'[^\p{L}\p{N}\s]', ' ', lyrics)
    
    # Remove vocalizations
    lyrics = re.sub(
        r'\b(?:ah|aah|aaah|aha|eh|eeh|uh|uhh|um|umm|hmm|mmm|oh|ooh|oooh|woo|wooo|whoa|la|na|lala|nana|yeah|yea|yo|blah|ey|yo)\b',
        ' ',
        lyrics,
        flags=re.IGNORECASE
    )

    # remove line breaks
    lyrics = re.sub("\n", ' ', lyrics)
    
    # reduce n grams
    lyrics = reduce_n_grams(lyrics, n = 1)
    lyrics = reduce_n_grams(lyrics, n = 2)
    lyrics = reduce_n_grams(lyrics, n = 3)
    lyrics = reduce_n_grams(lyrics, n = 4)
    lyrics = reduce_n_grams(lyrics, n = 5)
    lyrics = reduce_n_grams(lyrics, n = 6)


    # clean up whitespace
    lyrics = re.sub(r'\s+', ' ', lyrics).strip()
    
    return(lyrics)   


test = "ah, ah, yea yea yea, I love you\n I love you, they, ey, blah blah"
clean_lyrics(test)

test ="wee wim o weh wim o weh o wim o weh o wim o weh o wim weh wim o weh o wim o weh o wim o weh o wim weh in the jungle the mighty jungle the lion sleeps tonight in the jungle the quiet jungle the lion sleeps tonight wee wim o weh wim o weh o wim o weh o wim o weh o wim weh wim o weh o wim o weh o wim o weh o wim weh near the village the peaceful village the lion sleeps tonight near the village the quiet village the lion sleeps tonight wee wim o weh wim o weh o wim o weh o wim o weh o wim weh wim o weh o wim o weh o wim o weh o wim weh hush my darling don t fear my darling the lion sleeps tonight whuh whuh wim o weh wee wim o weh wee wim o weh wee wim o weh"

clean_lyrics(clean_lyrics(test))

# apply the lyrics cleaning
df_lyrics.lyrics = df_lyrics.lyrics.apply(clean_lyrics)


# apply LID to the lyrics
df_lyrics[["ft_lyrics_lang", "ft_lyrics_conf"]] = df_lyrics.lyrics.apply(LID.get_language_ft).tolist()

df_lyrics[["glot_lyrics_lang", "glot_lyrics_conf"]] = df_lyrics.lyrics.apply(LID.get_language_glot).tolist()


df_lyrics.to_csv("data/processed/artist_title_lyrics_lang.csv", index = False)

