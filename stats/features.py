import numpy as np
import scipy.stats.mstats as stats

def prominent_elements(X, *, sigma=1.0):
    """
    Returns a masked array of the 'prominent' elements of the array `X`, where
    prominent is defined as being more than `sigma` standard deviations above
    the mean.
    """
    return stats.zscore(X) > sigma

def prominence(X, *, sigma=1.0):
    """
    Returns the 'prominence' of the array `X`, where prominence is defined as
    the number of elements in the array which are more than `sigma` standard
    deviations above the mean.
    """
    return np.count_nonzero(prominent_elements(X, sigma=sigma))

def prominence_frequencies(coll, *, sigma=1.0):
    return np.bincount(prominence(x, sigma=sigma) for x in coll)

def prominence_proportions(coll, *, sigma=1.0):
    freq = prominence_frequencies(coll, sigma=sigma)
    return freq / sum(freq)

def cosine_similarity(A, B):
    """
    Computes the cosine similarity of two vectors. If vectors are of different
    lengths, the shorter one is padded with zeroes.
    """
    A_norm = np.linalg.norm(A)
    B_norm = np.linalg.norm(B)

    size_difference = np.size(A) - np.size(B)

    if size_difference < 0:
        A = np.pad(A, (0, -size_difference), mode='constant', constant_values=0)
    elif size_difference > 0:
        B = np.pad(B, (0, size_difference),  mode='constant', constant_values=0)

    return np.dot(A, B) / (A_norm * B_norm)
