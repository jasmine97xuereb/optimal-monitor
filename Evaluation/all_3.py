import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.transforms as transforms

import plot_2

SHIFT_VAL = 0.25


def _draw_plot(fig, df1, df2, df3):

    axes = fig.axes

    axes[0].plot(df1['Size'], df1['Time'], marker='o', label=r'$P_1(k)$')
    axes[0].plot(df2['Size'], df2['Time'], marker='x', label=r'$P_2(k)$')
    axes[0].legend(bbox_to_anchor=(0.37, 0.8), loc='center left', borderaxespad=0., prop={'size': 5})

    axes[1].plot(df3['Size'], df3['Time'], marker='x')
    
def _shift_x_labels(fig, ax, fact):

    # Create offset transform by 5 points in x direction
    dx = fact/72.; dy = 0/72.
    offset = transforms.ScaledTranslation(dx, dy, fig.dpi_scale_trans)

    # apply offset transform to all x ticklabels.
    for label in ax.xaxis.get_majorticklabels():
        label.set_transform(label.get_transform() + offset)
    ax.minorticks_off()

def _make_fig_latex():
    """ Creates a new figure pre set with axes.

    :return: The new figure.
    """

    # Configure figure style, size. It will have no title.
    plot_2.set_plot_style(plt)
    fig = plt.figure()
    plot_2.set_fig_style(fig, 4.5, 1.3)

    # Size without top no legend.
    fig.subplots_adjust(
        left=0.1, bottom=0.22, right=0.95, top=0.89, wspace=0.3, hspace=0.38
    )

    gs = fig.add_gridspec(1, 2)
    ax = fig.add_subplot(gs[0, 0])
    ax_1 = fig.add_subplot(gs[0, 1])


    # Configure axes for parametersized
    ax.set_title(r'Parametrized Formulae')
    # ax.set_yscale('symlog')
    ax.axis([0, 10000, 0, 1500])
    ax.set_xticks(np.arange(0, 10001, 2000))
    ax.set_yticks(np.arange(0, 1501, 500))
    ax.set(xlabel='Size ($n$)', ylabel='Time (s)')

    ax_1.set_title(r'Random Formulae')
    ax_1.axis([0, 10000, 0, 0.06])
    ax_1.set_yticks([0, 0.02, 0.04, 0.06], ['0', '0.02', '0.04', '0.06'])
    ax_1.set(xlabel='Size ($n$)')

    # Configure common style for axis.
    plot_2.set_axes_style(fig.axes)

    return fig


def _save_fig(fig, data_dir, file, ext):
    """ Save the figure to the specified directory.

    Figure title is used as the file name.

    :param fig: Figure to save.
    :param data_dir: Directory where the figure is saved.
    :param file: Name of file.
    :param ext: Type of file.
    :return: The figure file name.
    """

    # Retrieve figure title (hack) to use as the name of the file where plots are saved. Figure is saved to PDF.
    file = os.path.join(data_dir, file + '.' + ext)
    plt.savefig(file)

    return file


def main():

    # Create new figure and initialize it with default empty plots and styles.
    fig = _make_fig_latex()

    # Read file and save to a dataframe.
    data = pd.read_csv("results_parametrised_p1.csv")
    df1 = pd.DataFrame(data, columns = ['Index','Size', 'Time'])

    data = pd.read_csv("results_parametrised_p2.csv")
    df2 = pd.DataFrame(data, columns = ['Index','Size', 'Time'])

    data = pd.read_csv("results_random.csv")
    df3 = pd.DataFrame(data, columns = ['Size', 'Time'])

    _draw_plot(fig, df1, df2, df3)
    _save_fig(fig, '.', 'results', 'pgf')

    # This is the variant we chose to use in the final version of the paper. It
    # is exactly as 'all.py', with the only difference that the graphs have a
    # reduced height to fit in the paper better.


if __name__ == "__main__":
    main()

# python -m all_3
