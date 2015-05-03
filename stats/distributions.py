import model_io as mio
import numpy as np
import matplotlib as mpl
import matplotlib.mlab as mlab
import matplotlib.pyplot as plt

def max_observation_probs(model):
    states = model[mio.OBSERVATION_PROB]
    return {s : max(states[s].values()) for s in states}

def max_transition_probs(model):
    states = model[mio.TRANSITION_PROB]
    return {s : max(states[s].values()) for s in states}


def plot_sorted_distribution(distribution_map, transform=lambda x: x):
    figure = plt.figure()
    axis = figure.add_subplot(111)

    x = range(len(distribution_map))
    y = sorted(transform(v) for v in distribution_map.values())

    axis.plot(x, y, 'ro')

    axis.set_ylim(0, 1)

    return figure

def plot_hist(distribution_map, transform=lambda x: x):
    figure = plt.figure()
    axis = figure.add_subplot(111)

    x = np.fromiter((transform(v) for v in distribution_map.values()),
                     dtype=float)
    n, bins, patches = axis.hist(x, 50, normed=1, facecolor='green', alpha=0.75)

    mu, sigma = x.mean(), x.std()

    y = mlab.normpdf(bins, mu, sigma)
    l = axis.plot(bins, y, 'r--', linewidth=1)

    axis.set_xlabel("probability")
    axis.set_ylabel("frequency")
    axis.grid(True)

    return figure


if __name__ == "__main__":
    from sys import argv

    model = mio.parse_model_from_filename(argv[1])
    plot_hist(max_observation_probs(model), np.exp) \
        .savefig("./obs.png")
    plot_hist(max_transition_probs(model), np.exp) \
        .savefig("./tran.png")

    obs_states = model[mio.OBSERVATION_PROB]
    tran_states = model[mio.TRANSITION_PROB]
    for i, state in enumerate(obs_states):
        plot_hist(obs_states[state], np.exp) \
            .savefig("./obs/{0:02d}.png".format(i))
        plt.close("all")
    for i, state in enumerate(tran_states):
        plot_hist(tran_states[state], np.exp) \
            .savefig("./tran/{0:02d}.png".format(i))
        plt.close("all")
