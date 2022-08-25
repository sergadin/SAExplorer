
from __future__ import annotations
import logging
from dataclasses import dataclass
from typing import List, Optional
import numpy as np
import pymorphy2

from sklearn.cluster import DBSCAN
import tayga_api

morph = pymorphy2.MorphAnalyzer()

def isword(word: str):
    term = Term.from_string(word)
    if term.pos_tag in ['PROPN']:
        return True
    for p in morph.parse(term.word):
        if(term.pos_tag == str(p.tag.POS) and 
            type(p.methods_stack[0][0]) == pymorphy2.units.by_lookup.DictionaryAnalyzer):
            return True
    return False


class Term:
    def __init__(self, word: str, pos_tag = None) -> None:
        self.word: str = word
        self.pos_tag: Optional[str] = pos_tag

    @staticmethod
    def from_string(word_pos: str) -> Term:
        parts = word_pos.split('_')
        word = " ".join(parts[:-1])
        pos_tag = parts[-1]
        return Term(word=word, pos_tag=pos_tag)

@dataclass
class Nearest_word:
    """Слово из тайги с расстоянием."""
    term: Term
    dist: float
    cluster: List[Nearest_word]

    @property
    def word(self):
        return self.term.word
    
    @property
    def pos_tag(self):
        return self.term.pos_tag

    @staticmethod
    def from_string(word_pos: str, cluster: List[Nearest_word] = None) -> Nearest_word:
        term = Term.from_string(word_pos)
        return Nearest_word(term=term, dist=None, cluster=cluster)

class Suggestion:
    """Рекомендации из тайги."""

    def __init__(self):
        self.nearest_words: List[Nearest_word] = []

    def add_nearest(self, nearest_word: Nearest_word):
        self.nearest_words.append(nearest_word)


@dataclass
class Representative:
    """Представитель и его кластер.
    
    Слова и кластеры представляют из себя строки, возвращенные из Тайги."""
    word: str
    cluster: List[str]


arguments = dict()


def make_distancer(tokens):
    token_numbers = {num: t for num, t in enumerate(tokens)}

    def tokens_dist(t1, t2):
        try:
            arguments[t1[0], t1[1]] = 1
            arguments[t2[0], t2[1]] = 1
            tok1 = token_numbers[(t1[0])]
            tok2 = token_numbers[(t2[0])]
            # print(tok1, tok2, 1 - np.dot(w2v_model[tok1], w2v_model[tok2]))
            vec1 = tayga_api.get_vector(tok1)
            vec2 = tayga_api.get_vector(tok2)
            # logging.info(f"make_distancer: {vec1}, {vec2}")

            return 1 - np.dot(vec1, vec2)
        except KeyError:
            # print(t1[0], t2[0], t1, t2)

            return 1
    return tokens_dist, {v: k for k, v in token_numbers.items()}


def word_closest(pos_words, neg_words, topn):
    sim_list = tayga_api.most_similar(
        positive=pos_words,
        negative=neg_words,
        topn=topn)
    logging.info("word_closest: sim_list = %s", sim_list)
    sim_tokens = (set([token for token, _weight in sim_list]))
    logging.info("word_closest: sim_tokens = %s", sim_tokens)
    for word, _weight in sim_list:
        sub_list = tayga_api.most_similar(
            positive=[word], 
            negative=neg_words,
            topn=topn)
        for word2, _weight2 in sub_list:
            sim_tokens.add(word2)
    # print(sim_tokens)
    return sim_tokens

 
def word_to_clusters(pos_words, neg_words, topn):
    # обработка слова
    logging.info(f"{pos_words}")
    logging.info(f"{neg_words}")
    sim_tokens = word_closest(pos_words, neg_words, topn)
    logging.error(f"{sim_tokens}")
    # очистка с помощью pymorphy
    sim_tokens_copy = set([])
    for word in sim_tokens:
        print(word, isword(word))
        if isword(word):
            sim_tokens_copy.add(word)
    sim_tokens = sim_tokens_copy
    # кластеризация
    # Заполняем кэш векторов модели word2vec.
    tayga_api.get_vector(sim_tokens)
    dist, token_to_num = make_distancer(sim_tokens)
    data_points = [np.array([token_to_num[t], seq], dtype=np.float64)
                   for seq, t in enumerate(sim_tokens)]
    logging.error(f"word_to_clasters: data_points = {data_points}")
    # for seq,  t in enumerate(sim_tokens):
        # print(t, np.array([token_to_num[t], seq], dtype=np.float64))
    logging.info("word_to_clusters: Начало работы DBSCAN.")
    clustering = DBSCAN(eps=0.35, metric=dist, min_samples=2).fit(data_points)
    logging.info("word_to_clusters: Конец работы DBSCAN.")

    #вывод
    unique_labels = set(clustering.labels_)
    clusters = {}
    for label in unique_labels:
        clusters[label] = []

    num_to_token = {v: k for k, v in token_to_num.items()}

    for seq, point_label in enumerate(clustering.labels_):
        clusters[point_label].append(num_to_token[data_points[seq][0]])

    return clusters
 

def average_from_clusters(clusters) -> List[Representative]:
    logging.info("average_from_clusters: Начало работы.")
    representatives = []
    cl_word = ""
    for key in clusters:
        if key == -1:
            continue
        s = 0
        for i in range(1, len(clusters[key])):
            s += 1 - np.dot(tayga_api.get_vector(clusters[key][0]),
                            tayga_api.get_vector((clusters[key])[i]))
        cl_word = clusters[key][0]
        for i in range(1, len(clusters[key])):
            n = 0
            for j in range(0, len(clusters[key])):
                if j == i:
                    break
                n += 1 - np.dot(tayga_api.get_vector(clusters[key][i]),
                                tayga_api.get_vector((clusters[key])[j]))
            if n < s:
                s = n
                cl_word = clusters[key][i]
        representatives.append(Representative(word=cl_word, cluster=clusters[key]))
    logging.info("average_from_clusters: Конец работы.")
    return representatives


def word_nearest(pos_terms, neg_terms) -> List[Nearest_word]:
    def term_to_key(term):
        if term.pos_tag:
            return f"{term.word}_{term.pos_tag}"
        return term.word

    clusters = word_to_clusters(
        [term_to_key(term) for term in pos_terms], 
        [term_to_key(term) for term in neg_terms],
        topn=15)
    # print(clusters)
    representatives = average_from_clusters(clusters)
    # sim_list = tayga_api.most_similar(
        # [term_to_key(term) for term in pos_terms],
        # [term_to_key(term) for term in neg_terms],
        # 20)

    # sim_tokens = (set([token for token, _weight in sim_list]))

    seen_words = {}
    nearest = []
    for repr in representatives:
        cluster_n_w = [Nearest_word.from_string(w) for w in repr.cluster]
        repr_n_w = Nearest_word.from_string(word_pos=repr.word, cluster=cluster_n_w)
        if repr_n_w.word not in seen_words:
            nearest.append(repr_n_w)
        seen_words[repr_n_w.word] = True

    return nearest


def suggest(pos_words, neg_words) -> Suggestion:
    sug = Suggestion()
    # print(pos_words)
    # print(neg_words)
    for nearest_word in word_nearest(pos_words, neg_words):
        sug.add_nearest(nearest_word)
    return sug
