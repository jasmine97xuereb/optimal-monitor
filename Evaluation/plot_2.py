import matplotlib
import pandas as pd
# import util

# Plot font sizes.
FONT_TINY = 5  # \tiny
FONT_SMALL = 7  # \scriptsize
# FONT_SMALL = 8
# FONT_MEDIUM = 10
FONT_MEDIUM = 8  # \footnotesize
FONT_LARGE = 8  # \footnotesize

MARKERS = ['v', 'D', '^', 's', 'o', '<', '>']

# Configure the use of PGF. Required pdflatex to be installed.
# Refer to https://timodenk.com/blog/exporting-matplotlib-plots-to-latex/
matplotlib.use('pgf')
matplotlib.rcParams.update({
    'pgf.texsystem': 'pdflatex',
    'font.family'  : 'serif',
    'text.usetex'  : True,
    'pgf.rcfonts'  : False,
})


def set_plot_style(plot):
    """ Sets the default style for the plot.

    :param plot: The plot to be styled.
    :return: The plot with style applied.
    """

    # Configure default font sizes.
    plot.rc('font', size=FONT_SMALL)  # Default text sizes.
    plot.rc('axes', titlesize=FONT_MEDIUM)  # Axes title font size.
    plot.rc('axes', labelsize=FONT_SMALL)  # X and Y label font size.
    plot.rc('xtick', labelsize=FONT_TINY)  # X Tick label font size.
    plot.rc('ytick', labelsize=FONT_TINY)  # Y Tick label font size.
    plot.rc('legend', fontsize=FONT_SMALL, title_fontsize=FONT_MEDIUM)  # Legend font size.
    plot.rc('figure', titlesize=FONT_LARGE)  # Figure title font size.

    # Configure default plot line style and markers.
    plot.rc('lines', linewidth=0.4, markersize='0.7')

    # Configure default padding between title and plot.
    plot.rc('axes', titlepad=5)

    # Configure legend.
    plot.rc(
        'legend', frameon=True, borderpad=0.2, handletextpad=0.4,
        labelspacing=0.4, handlelength=0.8, loc='upper left'
    )


def set_fig_style(fig, width, height):
    """ Sets the default figure style.

    :param fig: The figure to style.
    :param width: The figure width in inches.
    :param height: The figure height in inches.
    :return: The figure with style applied.
    """

    # Configure figure size.
    fig.set_size_inches(width, height, forward=True)


def set_axes_style(axes):
    """ Sets the default axes style.

    :param axes: The axes to style.
    :return: The axes with style applied.
    """
    for ax in axes:
        ax.grid(which='major', axis='both', linewidth=0.4, linestyle='--')
        ax.minorticks_on()
        ax.tick_params(which='major', axis='both', length=2)
        ax.tick_params(which='minor', axis='both', length=1)
        # ax.tick_params(axis='x', rotation=45)
        ax.set_facecolor('whitesmoke')
        ax.margins(x=0.01, y=0.02)
        ax.set_axisbelow(True)
        # ax.title_pad()
