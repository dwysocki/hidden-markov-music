import numpy as np
import edn_format as edn

HMM              = edn.Keyword("HMM")
LogHMM           = edn.Keyword("LogHMM")

TYPE             = edn.Keyword("type")
STATES           = edn.Keyword("states")
OBSERVATIONS     = edn.Keyword("observations")
INITIAL_PROB     = edn.Keyword("initial-prob")
TRANSITION_PROB  = edn.Keyword("transition-prob")
OBSERVATION_PROB = edn.Keyword("observation-prob")

NEGATIVE_INFINITY = edn.Symbol("-Infinity")

def parse_model_from_filename(filename):
    with open(filename, 'r') as file:
        return parse_model_from_file(file)

def parse_model_from_file(file):
    return parse_model_from_str(file.read())

def parse_model_from_str(model_str):
    return parse_model_symbols(edn.loads(model_str))

def parse_symbol(symbol):
    if symbol == NEGATIVE_INFINITY:
        return np.NINF
    else:
        return symbol

def parse_dict_symbols(x):
    return {k : parse_symbol(x[k]) for k in x}

def parse_nested_dict_symbols(x):
    return {k : parse_dict_symbols(x[k]) for k in x}

def parse_model_symbols(model):
    return {
        TYPE             : model[TYPE],
        STATES           : model[STATES],
        OBSERVATIONS     : model[OBSERVATIONS],
        INITIAL_PROB     : parse_dict_symbols(model[INITIAL_PROB]),
        TRANSITION_PROB  : parse_nested_dict_symbols(model[TRANSITION_PROB]),
        OBSERVATION_PROB : parse_nested_dict_symbols(model[OBSERVATION_PROB])
    }
