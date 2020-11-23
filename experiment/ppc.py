# -*- coding: utf-8 -*-
"""
Version: 2019.02.0

The ppc module (ppc is short for "PsychoPy Course) contain some useful
methods to help you build and verify your experiment. Put the ppc.py
in the same folder as your script or in your PYTHONPATH. See these
functions in use in the ppc3_template.py and in ppc2_timing.py.

Jonas Lindeløv

TO DO:
 * add UTC times in csvWriter?
 * Remove sound or make it a dummy-one which drops back to psychopy
 * Use PEP8 names instead of camelCase
"""

import os
import time
import timeit
import math
import csv

# Check python version
import sys
python3 = sys.version_info[0] == 3


class Sound(object):
    """
    A windows-only low-latency replacement for psychopy.sound.
    It can only play wav files. Timing is unreliable if sound.play() is called
    before previous sound ends. Usage::

        beep = ppc.Sound('beep.wav')
        beep.play()

        # or generated beep:
        beep = ppc.Sound()
        beep.beep(1000, 0.2)  # 1000 Hz for 0.2 seconds
    """
    def __init__(self, filename=''):
        """ :filename: a .wav file"""
        self.sound = filename
        self._winsound = __import__('winsound')

    def play(self):
        """ plays the sound file with low latency"""
        self._winsound.PlaySound(self.sound,  self._winsound.SND_FILENAME | self._winsound.SND_ASYNC)

    def beep(self, frequency, duration):
        """ plays a beep with low latency"""
        self._winsound.Beep(frequency, duration / float(1000))


def timer(script, setup='', timeScale=False, runs=False):
    """
    Times code snippets and returns average duration in seconds.

    :script: a string to be timed
    :setup: a comma-separated string specifying methods and variables to be imported from __main__
    :timeScale: the unit for seconds. 10**-9 = nanoseconds. If False, the scale is automagically determined as s, ms, us or ns
    :runs: how many times to run the script. If False, the number of runs is automagically determine from 3 testruns, trying to keep the total test duration around a second but at least 10 runs and at most 10**6 runs.
    """
    if setup:
        setup = 'from __main__ import ' + setup

    timeit.timeit(number=10**7)  # get the computer's attention/ressources. First run is slower.

    # optional: determine appropriate number of runs from 3 test runs
    if not runs:
        result = timeit.timeit(script, setup=setup, number=3)
        runs = int(3 / result) if result > 0 else 10 ** 6
        if runs > 10 ** 6:
            runs = 10 ** 6  # a million at most
        if runs < 10:
            runs = 10  # ten at least

    # Actually do the timing
    baseline = timeit.timeit(setup=setup, number=runs)  # the time it takes to run an empty script
    result = timeit.timeit(script, setup=setup, number=runs)  # Run the test!
    mean = (result - baseline) / runs  # in seconds

    # Optional: determine appropriate timeScale for reporting
    if not timeScale:
        timeScale = 1 if mean > 1 else 10**-3 if mean > 10**-3 else 10**-6 if mean > 10**-6 else 10**-9
    unit = 's' if timeScale == 1 else 'ms' if timeScale == 10**-3 else 'us' if timeScale == 10**-6 else 'ns' if timeScale == 10**-9 else '*' + str(timeScale)

    # Print results
    print('\n\'', script, '\'')
    print('AVERAGE:', round(mean / timeScale, 3), unit, 'from', runs, 'runs')


def deg2cm(angle, distance):
    """
    Returns the size of a stimulus in cm given:
        :distance: ... to monitor in cm
        :angle: ... that stimulus extends as seen from the eye

    Use this function to verify whether your stimuli are the expected size.
    (there's an equivalent in psychopy.tools.monitorunittools.deg2cm)
    """
    return math.tan(math.radians(angle)) * distance  # trigonometry


class csv_writer(object):
    def __init__(self, filename_prefix='', folder='', column_order=[], add=['timestamp']):
        """
        Take a dictionary and write it to a csv file as a row.
        Writing is very fast - less than a microsecond.

        :filename_prefix: (str) would usually be the id of the participant
        :folder: (str) optionally use/create a folder.
        :column_order: (list) The columns to put first in the csv. Some or all.

        Use like:

            # Once towards the beginning of the script
            writer = csv_writer('participant1', folder='data', column_order=['id', 'condition'])

            # After each trial is completed
            trial = {'id': 'participant1', 'rt': 0.2323, 'condition': 'practice'}
            writer.write(trial)

            # Optional: forces save of hitherto collected data to disk.
            # writer.flush()
        """

        self.column_order = column_order
        self._header_written = False

        # Create folder if it doesn't exist
        if folder:
            folder += '/'
            if not os.path.isdir(folder):
                os.makedirs(folder)

        # Generate self.save_file and self.writer
        self.save_file = '%s%s (%s).csv' % (folder, filename_prefix, time.strftime('%Y-%m-%d %H-%M-%S', time.localtime()))  # Filename for csv. E.g. "myFolder/subj1_cond2 (2013-12-28 09-53-04).csv"
        self._setup_file()
        self.add = add

    def _setup_file(self):
        """Setting up the self.writer depends on python version."""
        if python3:
            self._file = open(self.save_file, 'a', newline='')
        else:
            self._file = open(self.save_file, 'wb')

        self.writer = csv.DictWriter(self._file, fieldnames=self.column_order)  # The writer function to csv. It appends a single row to file

    def _get_timestamp(self):
        """Get local timestamp"""
        return(int(round(time.time() * 1000)))

    def write(self, trial):
        """Saves a trial to buffer. :trial: a dictionary"""
		# Optionally add timestamp
        if 'timestamp' in self.add:
            trial.update({'timestamp': int(round(time.time() * 1000))})
		
        # Write header and add fieldnames on first trial
        if self.writer.fieldnames is None:
            self.writer.fieldnames = list(trial.keys())

        # Check that all column_order are present in the trial
        if len(set(self.column_order) - set(list(trial.keys()))) != 0:
            raise(ValueError('A column in column_order was not present in the trial dictionary'))

        # Enforce order on first columns. Then add the last in "random" order.
        if len(trial) > len(self.column_order):
            self.writer.fieldnames = self.column_order + list(set(list(trial.keys())) - set(self.column_order))

        # Write header if it hasn't been
        if not self._header_written:
            self.writer.writeheader()
            self._header_written = True

        # Now write data
        self.writer.writerow(trial)  # Works both in python2 and python3

    def flush(self):
        """Saves current content to file.
        This will happen automatically when the script terminates.
        Only do this if you fear a hard crash. It's mostly fast (< 1 ms) but can be slow (up to 30 ms)
        """
        self._file.close()
        self._setup_file()


def getActualFrameRate(frames=1000):
    """
    Measures the actual framerate of your monitor. It's not always as clean as
    you'd think. Prints various useful information.
        :frames: number of frames to do test on.
    """
    from psychopy import visual, core

    # Set stimuli up
    durations = []
    clock = core.Clock()
    win = visual.Window(color='pink')

    # Show a brief instruction / warning
    visual.TextStim(win, text='Now wait and \ndon\'t do anything', color='black').draw()
    win.flip()
    core.wait(1.5)

    # blank screen and synchronize clock to vertical blanks
    win.flip()
    clock.reset()

    # Run the test!
    for i in range(frames):
        win.flip()
        durations += [clock.getTime()]
        clock.reset()

    win.close()

    # Print summary
    import numpy as np
    print('average frame duration was', round(np.average(durations) * 1000, 3), 'ms (SD', round(np.std(durations), 5), ') ms')
    print('corresponding to a framerate of', round(1 / np.average(durations), 3), 'Hz')
    print('60 frames on your monitor takes', round(np.average(durations) * 60 * 1000, 3), 'ms')
    print('shortest duration was ', round(min(durations) * 1000, 3), 'ms and longest duration was ', round(max(durations) * 1000, 3), 'ms')


def dkl2rgb(dkl):
    """ takes a DKL color as input and returns the corresponding RGB color """
    from numpy import array
    from psychopy.misc import dkl2rgb
    return dkl2rgb(array(dkl))
