from site import venv
import requests
import numpy as np
import logging
import time

def most_similar(positive, negative=[], topn=10):
    """Функция удаленно вызывает most_similar из модели Word2Vec"""
    logging.info("Api. Начало работы most similar.")
    tayga_rq = {
        "pos_words": list(positive),
        "neg_words": list(negative),
        "topn": int(topn)
    }
    # print(tayga_rq)
    r = requests.post('http://localhost:5100/most_similar/', json=tayga_rq)
    logging.info("Api. Конец работы most similar.")
    return r.json() 


vectors_cache = {}
def get_vector(words):
    """Функция удаленно вызывает model[word] из модели Word2Vec.
    
    :param words: Может быть списком, а может быть списком из одного слова. 
    """
    start = time.perf_counter()
    return_list = True
    if isinstance(words, str):
        words = [words]
        return_list = False
    words_to_query = [w for w in words if w not in vectors_cache]
    
    if words_to_query:

        tayga_rq = {
            "words": words_to_query
        }
        r = requests.post('http://localhost:5100/get_vectors/', json=tayga_rq)
        vectors = r.json().get("vectors")
        for word, vector in zip(words_to_query, vectors):
            vectors_cache[word] = np.array(vector) if vector else None
        logging.info("Api. Конец работы get_vector. %s", time.perf_counter() - start)

    if return_list is False:
        return vectors_cache[words[0]]
    return [vectors_cache[word] for word in words]
    
