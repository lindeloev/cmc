# -*- coding: utf-8 -*-
"""
Six-way CMC for a study on individual difference in CMC. In particular, this
study examines CMCs between four visual features (shape, complexity, colour,
and lightnesss), and two auditory features (pitch and timbre).

Study concept by Hazel Anderson and Jonas Lindeløv.
Programmed by Jonas Kristoffer Lindeløv.
September, 2018
Updated with Chinese translations, Python3 and PEP8 in February, 2019

TO DO NEW:
 * Put rating response further down to avoid pressing center of scale
 * Fixation cross not behaving?

LOW PRIORITY:
 * Neutral square too loud?
 * Figure pointing direction (SNARK)
 * 1px gap between fill and shape for fill-shape figure. No idea why!

Instructions:
 * "Odd trial"

"""

# IMPORT MODULES
from __future__ import division
import random
import numpy as np
import copy
import itertools
import gettext
import inputs
import ppc
from collections import OrderedDict
from os import path

#import psychopy
#psychopy.useVersion('3.0.5')
from psychopy import visual, sound, event, core, gui

"""
DIALOGUE
"""
DIALOGUE = {
    'id': '',
    'gender': ['female', 'male'],
    'age': '',
    'simulation': ['no', 'yes'],
    'demo_and_practice': ['yes', 'no'],
    'section': ['test_CMC', 'rate_CMC'],
    'language': ['cn', 'dk', 'en'],
    'input': ['gamepad', 'keyboard'],
    'refresh_rate': [100, 60],
    'experimenter': ['Yue', 'person2', 'person3', 'other']
}
if not gui.DlgFromDict(DIALOGUE).OK:
    core.quit()

# Fix a bug in PsychoPy 1.90 where ints are converted to strings
DIALOGUE['refresh_rate'] = int(DIALOGUE['refresh_rate'])

# Fix a bug in PsychoPy3 where strings are lists of characters
for key, value in DIALOGUE.items():
    if isinstance(value, list):
        DIALOGUE[key] = ''.join(value)


# Just for testing
"""
DIALOGUE = {'id': 'jonas_test',
            'gender': 'male',
            'age': 33,
            'simulation': 'no',
            'demo_and_practice': 'no',
            'section': 'rate_CMC',
            'language': 'en',
            'input': 'keyboard',
            'refresh_rate': 60,
            'experimenter': 'Jonas'}
"""

# SET SETTINGS
# Durations
if DIALOGUE['refresh_rate'] == 60:
    FIGURE_DURATION = 7  # Number of frames. 116.667 ms.
    FIXATION_DURATIONS = [30, 45]  # List of fixation duration in Number of frames. Will be chosen at random. 500 ms or 750 ms
    FEEDBACK_DURATION = 30  # Number of frames of feedback in practice. 500 ms
elif DIALOGUE['refresh_rate'] == 100:
    FIGURE_DURATION = 12  # 120 ms
    FIXATION_DURATIONS = [50, 75]  # 500 or 750 ms
    FEEDBACK_DURATION = 50  # 500 ms

# Stimuli attributes
FIGURE_SIZE = 4  # Degrees visual angle
IMAGE_SIZE_PIX = 400  # pixels. Used to compute actual image size. Assumes square image
FIGURE_SIZE_PIX = 350  # pixels. Actual figure size in image.
DEMO_POSITIONS = ((-3, -1), (3, -1))
stuff = [[-2, -0.5], [-1, -0.5]], [[1, -0.5], [2, -0.5]]
RATING_POSITIONS = np.array(stuff) * FIGURE_SIZE
TEXT_HEIGHT_INSTRUCT = 0.5
TEXT_HEIGHT_REMINDER = 0.4
TEXT_POS = (0, 5)

# Conditions
TEST_REPETITIONS = 24  # Repetition of each trial type
RATE_REPETITIONS = 3  # Number of times to do each rating
PRACTICE_TRIALS = 8  # How long a retrospective window to evaluate practice trials
MINIMUM_PRACTICE_SCORE = 7  # Minimum number of correct answer out of PRACTICE_TRIALS
TEST_FEATURES = ['shape', 'lightness', 'timbre', 'pitch']  # Test these
RATE_FEATURES = ['lightness', 'hue', 'shape', 'complexity', 'timbre', 'pitch']  # Rate combinations of these

# Responses
RESPONSE_KEYS = ['left', 'right']
RESPONSE_TEXT = {'left': u'\u2190', 'right': u'\u2192'}  # Alternatives: ← →, ⬅ ➡, ◀ ▶, 'LEFT', 'RIGHT'
CATCH_TEXT = {'up': u'\u2191', 'down': u'\u2193'}
# RESPONSE_TEXT = {'left': u'\u2B05', 'right': u'\u27A1'}  # thick arrows
CATCH_KEYS = ['up', 'down']
QUIT_KEYS = ['escape']
CONTINUE_KEYS = ['return']
CONTINUE_TEXT = {'gamepad': 'X', 'keyboard': 'ENTER'}
KEYS_GAMEPAD = {'left': 'BTN_TL', 'right': 'BTN_TR', 'up': 'BTN_NORTH', 'down': 'BTN_SOUTH', 'return': 'BTN_WEST', 'escape': 'BTN_START'}
KEYS_GAMEPAD_REVERSE = {value: key for key, value in KEYS_GAMEPAD.items()}  # Invert keys and values

# Catch trials
CATCH_INTERVAL = 10  # How many trials average spacing between each catch trials
CATCH_JITTER = 4  # How many trials jitter uniformely added to each side of catch trial numbers (5 = -5 to 5))

# CMC ratings
RATING_WAIT = 5  # Number of seconds to wait after stimulus presentation before accepting responses

# Set language
if DIALOGUE['language'] != 'en':
    language = gettext.translation('CMC', localedir='locale', languages=[DIALOGUE['language']])
    if ppc.python3:
        language.install()
    else:
        language.install(unicode=True)
else:
    _ = gettext.gettext


"""
INSTRUCTIONS
"""

# General instructions
texts = {
    'continue': _(u'\n\nPress %s to continue...') % CONTINUE_TEXT[DIALOGUE['input']],
    'bottom_reminder': _(u' The reminder you see at the bottom will be present throughout the task. Press %(left)s or %(right)s a few times to see/hear more examples.') % RESPONSE_TEXT
}
texts.update({
    'break': _(u'%i blocks left! Feel free to take a moment\'s break if you need to.\n\nPress %s to show the instructions for the next task.'),
    'welcome': _(u'Welcome! In this experiment, you will be asked to respond to a particular aspect of an image or a sound. Let\'s get straight to it and practice how to do the task.') + texts['continue'],
    'practice_catch': _(u'Once in a while there\'s an odd trial. Let\'s practice that right now before we get to the main task.\n\nPress %(up)s if you see an upwards-pointing arrow or hear an upwards-sliding chirp. Press %(down)s if you see a downwards-arrow or hear a downwards-sliding chirp.\n\nYou will now get the chance to practice this with some feedback to help you. Don\'t worry about the other figures and sounds for now. You will be instructed how to respond to those momentarily.') % CATCH_TEXT + texts['continue'],
    'practice_exp': _(u'You will now get to practice the \"real\" task. During practice, you will get feedback whether you responded correctly or not. For now, try to be as accurate as possible.') + texts['continue'],
    'after_demo': _(u'Great! You are ready now. It is important that you respond as quickly as you possibly can without making errors.\n\nOn most trials you will respond as just practiced using %(left)s and %(right)s.') %RESPONSE_TEXT + _(u'But you will occasionally be presented with an arrow or a chirp to which you will need to press %(up)s or %(down)s to indicate the direction.') % CATCH_TEXT + texts['continue'],
    'small_break': _(u'You can take a small break if you need to. When you continue, the task will remain the same. Please make sure to respond as fast as possible without making errors.') + texts['continue'],
    'bye': _(u'The experiment has finished. Thank you! Press %s to finish.') % CONTINUE_TEXT[DIALOGUE['input']],
    'rate_intro1': _(u'In this block, you are going to see pairs of images which sometimes have accompanying sounds. There will be a pair of shapes on the left of the screen, and a pair of shapes on the right. All you need to do is decide which pairing feels correct to you. There will be a response bar at the bottom of the screen which ranges from strongly prefer the pairing on the left, to strongly prefer the pairing on the right, with no preference in the centre. If you feel the pair of images or images and sounds on the left of the screen is a better match, then indicate your preference to the left. If the pair of images or images and sounds on the right of the screen is a better match, then indicate your preference to the right. If you have no preference then choose that. Options between (slightly prefer and medium prefer) for either direction can also be made.  To make your choice, click on the response bar and a black triangle will appear there. If you want to change your selection, just click on a different point and the black triangle will move to there. To confirm your selection, click on the button on the bottom of the screen which lights up after making a choice.') + texts['continue'],
    'rate_intro2': _(u'On trials which include sounds, you need to click on each of the shapes to hear the pairings before making a selection or before the response bar is shown. You can click on the shapes to hear the sounds as many times as you like.\n\nPlease think about your choices, there is no time limit for this block and speed is not important. For this task, making sure you select your preference is all that matters.') + texts['continue'],
    'rate_instruct': _(u'Which side best matches %s and %s? Click on the options bar at the bottom of the screen to indicate your preference, a black triangle will appear on your choice response and a central number in the centre of the bar. Click on the central number to confirm your choice.'),
    'rate_click': _(u'\nUse the mouse to click all the shapes and listen.')
})

# Specific instructions.
task_instructions = {
    'shape': _(u'SHAPE: There are two types of shape in this task. The %s button is used to indicate shapes with smooth protrusions, and the %s button to indicate shapes with pointy protrusions.') + texts['bottom_reminder'],
    'complexity': _(u'COMPLEXITY: There are two types of complexity in this task. The %s button is used to indicate shapes with four protrusions, and the %s button to indicate shapes with seven protrusions.') + texts['bottom_reminder'],
    'hue': _(u'COLOUR: There are two types of colour in this task. The %s button is used to indicate shades of red and the %s button to indicate shades of blue.') + texts['bottom_reminder'],
    'lightness': _(u'LIGHTNESS: There are two levels of lightness in this task. The %s button is used to indicate dark gray and the %s button to indicate light gray.') + texts['bottom_reminder'],
    'timbre': _(u'SOUND: There are two types of tone sound in this task. The %s button is used to indicate soft sounding tones and the %s button to indicate rough sounding tones.') + texts['bottom_reminder'],
    'pitch': _(u'TONE PITCH: There are two types of tone pitch in this task. The %s button is used to indicate the high pitch tones, and the %s button to indicate the low pitch tones.') + texts['bottom_reminder']
}
# Human-readable specific feature attributes
feature_reminders = {
    'shape': [_(u'smooth'), _(u'pointy')],
    'complexity': [_(u'four protrusions'), _(u'seven protrusions')],
    'hue': [_(u'red'), _(u'blue')],
    'lightness': [_(u'dark colour'), _(u'light colour')],
    'timbre': [_(u'soft'), _(u'rough')],
    'pitch': [_(u'high'), _(u'low')]
}
# Human-readable general features
feature_reminders2 = {
    'shape': _(u'shape'),
    'complexity': _(u'complexity'),
    'hue': _(u'colour'),
    'lightness': _(u'lightness'),
    'timbre': _(u'sound'),
    'pitch': _(u'tone pitch')
}

# FOR INTERNAL USE. JUST LISTED HERE BECAUSE THEY NEED TO HAVE THE ABOVE STRUCTURE
# Just a practical translation from attribute dimension to possible values in the above
features = {
    'shape': ['smooth', 'pointy'],
    'complexity': ['simple', 'complex'],
    'hue': ['red', 'blue'],
    'lightness': ['dark', 'light'],
    'timbre': ['sine', 'square'],
    'pitch': ['high', 'low']
}


modalities = {
    'shape': 'visual',
    'complexity': 'visual',
    'hue': 'visual',
    'lightness': 'visual',
    'timbre': 'auditory',
    'pitch': 'auditory'
}

# Order of key-feature mappings. 1 = same as RESPONSE_KEYS. 2 = reversed(RESPONSE_KEYS).
# Used to counterbalance within subject. When a mapping is used, it is removed.
key_feature_maps = dict([(feature, random.sample([1, 2], 2)) for feature in features.keys()])

# Initial values to be changed during runtime
trial_no_global = 1


# For simulation
if DIALOGUE['simulation'] == 'yes':
    FIXATION_DURATIONS = [0]
    FIGURE_DURATION = 0


"""
PSYCHOPY STIMULI
"""
win_args = {'monitor': 'testMonitor',
            'size': [1200, 1000],  # Just for development in non-fullscreen
            'units': 'deg',
            'color': (119, 119, 119),  # CIE (50, 0, 0)
            'colorSpace': 'rgb255',
            'fullscr': True,
            'allowGUI': False,
            'waitBlanking': True}
win = visual.Window(**win_args)

# General stimuli
fixation = visual.TextStim(win, text='+', height=TEXT_HEIGHT_INSTRUCT, color=(171, 171, 171), colorSpace='rgb255')
instruction = visual.TextStim(win, height=TEXT_HEIGHT_INSTRUCT, pos=(0, 5))
feedback = visual.TextStim(win, height=TEXT_HEIGHT_INSTRUCT, pos=(0, 2))

key_demo = visual.TextStim(win, height=TEXT_HEIGHT_INSTRUCT*2)
key_reminders = {
    'left': visual.TextStim(win, height=TEXT_HEIGHT_REMINDER, pos=(-3, -6), color=(171, 171, 171), colorSpace='rgb255', alignHoriz='center'),
    'right': visual.TextStim(win, height=TEXT_HEIGHT_REMINDER, pos=(3, -6), color=(171, 171, 171), colorSpace='rgb255', alignHoriz='center')
}
clock = core.Clock()

# Placeholders for images (figures) with the correct attributes and all
figure_args = {'colorSpace': 'rgb255', 'size': FIGURE_SIZE * (IMAGE_SIZE_PIX / FIGURE_SIZE_PIX), 'interpolate': True}
figure_fill = visual.ImageStim(win, **figure_args)
figure_outline = visual.ImageStim(win, **figure_args)
figure_blank = visual.ImageStim(win)
figure_template = visual.ImageStim(win, **figure_args)
figure_template.flipped = 0


# Specify colors for the figure (color/lightness). Calculated using http://serennu.com/colour/hsltorgb.php
colors = {
    'blue': {
        'light': (158, 164, 249),  # CIE-LAB (70, 17.62, -43.06)
        'dark': (46, 65, 138),  # CIE-LAB (30, 17.62, -43.06)
        'neutral': (103, 113, 192)  # CIE-LAB (50, 17.62, -43.06)
    },
    'red': {
        'light': (250, 138, 152),  # CIE-LAB (30, 44.13, 11.55)
        'dark': (133, 31, 55),  # CIE-LAB (70, 44.14, 11.55)
        'neutral': (191, 85, 101)  # CIE-LAB (50, 44.14, 11.55)
    },
    'neutral': {
        'light': (171, 171, 171),  # CIE-LAB (70, 0, 0)
        'dark': (71, 71, 71),  # CIE-LAB (30, 0, 0)
        'neutral': (119, 119, 119)  # CIE-LAB (50, 0, 0)
    }
}

# Load appropriate sounds for timbre/complexity
beep_blank = sound.Sound(secs=0.1)
beep_blank.setVolume(0)
beeps = {
    'square': {
        'low': sound.Sound(path.join('stimuli', 'square_400hz_120ms.wav')),  # TO DO
        'high': sound.Sound(path.join('stimuli', 'square_1200hz_120ms.wav')),  # TO DO
        'neutral': sound.Sound(path.join('stimuli', 'square_693hz_120ms.wav'))
    },
    'sine': {
        'low': sound.Sound(path.join('stimuli', 'sine_400hz_120ms.wav')),  # TO DO
        'high': sound.Sound(path.join('stimuli', 'sine_1200hz_120ms.wav')),  # TO DO
        'neutral': sound.Sound(path.join('stimuli', 'sine_693hz_120ms.wav'))
    },
    'neutral': {
        'low': sound.Sound(path.join('stimuli', 'neutral_400hz_120ms.wav')),
        'high': sound.Sound(path.join('stimuli', 'neutral_1200hz_120ms.wav')),
        'neutral': sound.Sound(path.join('stimuli', 'neutral_693hz_120ms.wav'))
    }
}

# Catch trials
catches = {
    'auditory': {
        'up': sound.Sound(path.join('stimuli', 'catch_auditory_up.wav')),
        'down': sound.Sound(path.join('stimuli', 'catch_auditory_down.wav'))
    },
    'visual': {
        'up': visual.ImageStim(win, path.join('stimuli', 'catch_visual_small.png'), ori=180, size=FIGURE_SIZE/2, color=(171, 171, 171), colorSpace='rgb255'),
        'down': visual.ImageStim(win, path.join('stimuli', 'catch_visual_small.png'), ori=0, size=FIGURE_SIZE/2, color=(171, 171, 171), colorSpace='rgb255')
    }
}


# Stuff only relevant for ratings
if DIALOGUE['section'] == 'rate_CMC':
    mouse = event.Mouse(win=win)
    outline = visual.ImageStim(win, **figure_args)  # To be used for CMC ratings
    outline.color = (0, 0, 0)
    VAS = visual.RatingScale(
        win,
        scale=None,
        pos=(0, -0.8),
        stretch=2,
        textSize=TEXT_HEIGHT_REMINDER,
        # Translators: this is the rating scale
        labels=(_(u'I strongly\nprefer this match'), _(u'No preference'), _(u'I strongly\nprefer this match')),
        acceptPreText='Submit',
        acceptSize=1.5,
        acceptKeys=None,
        respKeys=None,
        markerColor='black')


"""
FUNCTIONS
"""
writer = ppc.csv_writer(u'CMC %s %s' % (DIALOGUE['id'], DIALOGUE['section']), folder='data')


def make_trials(task, inducer, phase='exp'):
    """Make a list of trials."""

    trials = []  # Will be returned for phase != 'practice_catch'
    trials_catch = []  # Will be returned for phase == 'practice_catch'. trials is used to generate this.

    combinations = itertools.product(features[task], features[inducer], range(TEST_REPETITIONS))
    # Loop through them
    for task_feature, inducer_feature, repetition in combinations:
        # Whether this trial contains visual and auditory elements
        any_visual = 'visual' in (modalities[task], modalities[inducer])
        any_auditory = 'auditory' in (modalities[task], modalities[inducer])

        # The trial data. Fill in as neutral trial. Then add actual stimulus features afterward
        trial = OrderedDict({
            # Figure properties
            'shape': 'neutral' if any_visual else '',
            'complexity': 'neutral' if any_visual else '',
            'hue': 'neutral' if any_visual else '',
            'lightness': 'neutral' if any_visual else '',
            # 'orientation': random.randint(0, 259),  # Random orientation
            'flip': random.randint(0, 1),  # Flip horizontally relative to source image?
            'fixation_duration': random.choice(FIXATION_DURATIONS),

            # Beep properties
            'timbre': 'neutral' if any_auditory else '',
            'pitch': 'neutral' if any_auditory else '',

            # Response
            'left': features[task][key_map['indices'][0]] if phase not in ('practice_catch', 'rate') else '',  # What feature the left key corresponds to
            'right': features[task][key_map['indices'][1]] if phase not in ('practice_catch', 'rate') else '',  # What feature the right key corresponds to

            'key': '',
            'rt': '',
            'score': '',

            'catch_key': '',
            'catch_rt': '',
            'catch_score': '',

            # General trial info
            'task': task,
            'inducer': inducer,
            'modality_task': modalities[task],  # Visual or Auditory
            'modality_inducer': modalities[inducer],
            'any_visual': any_visual,
            'any_auditory': any_auditory,
            'phase': phase,
            'catch': 0,  # Is this a catch trial? Default to false
            'catch_stim': '',  # Will be added later on catch trials only
            'superblock': superblock + 1 if phase in ('exp', 'practice_exp') else ''  # Start at 1
        })

        # Set the actual stimulus attributes to display/play
        trial[trial['task']] = task_feature
        trial[trial['inducer']] = inducer_feature
        trial['CMC_code'] = '%s:%s' % (task_feature, inducer_feature)

        trial.update(DIALOGUE)
        trials.append(trial)

    # Randomize order
    random.shuffle(trials)

    # INSERT CATCH TRIALS
    if phase in ('exp', 'practice_catch'):
        if phase == 'exp':
            # Which indices in trials to insert into. Ever so often in experiment...
            indices = np.linspace(start=CATCH_INTERVAL, 
                                  stop=len(trials) - CATCH_INTERVAL, 
                                  num=(len(trials) - CATCH_INTERVAL) / CATCH_INTERVAL
                                  ).astype(int)  # Equidistant integers
            # print 'actual catch interval is ', indices[1] - indices[0]  # First two, before adding jitter
            indices += np.random.randint(low=-CATCH_JITTER,
                                         high=CATCH_JITTER,
                                         size=int(np.floor(len(trials) / CATCH_INTERVAL - 1)))  # Add jitter
        # ... and just plenty for practice (as many as there are trials)
        if phase == 'practice_catch':
            indices = range(len(trials) - 1)

        # Actually insert. Reverse iteration so that shifting indices does not matter
        for catch_trial_index in reversed(indices):
            trial = copy.deepcopy(random.choice(trials))
            trial['catch'] = 1  # Yes, this is a catch trial

            # Alternate modalities during practice
            if phase == 'practice_catch':
                trial['modality_task'] = list(catches.keys())[catch_trial_index % 2]  # alternating modality

            # Insert auditory catch for visual trials
            if trial['modality_task'] == 'visual':
                # Pick an auditory catch stimulus
                trial['catch_stim'] = random.choice(list(catches['auditory'].keys()))

                # Remove auditory features.
                trial['timbre'] = ''
                trial['pitch'] = ''

            # Insert visual catch for auditory trials
            elif trial['modality_task'] == 'auditory':
                # Pick an auditory catch stimulus
                trial['catch_stim'] = random.choice(list(catches['visual'].keys()))

                # Remove visual features
                trial['shape'] = ''
                trial['complexity'] = ''
                #trial['hue'] = ''
                trial['lightness'] = ''
                #trial['orientation'] = ''
                trial['flip'] = ''

            # We have our catch trial! Now insert it in the appropriate list
            if phase == 'exp':
                trials.insert(catch_trial_index, trial)
            elif phase == 'practice_catch':
                trials_catch.extend([trial])

    # Finished! Return the appropriate list
    if phase == 'practice_catch':
        return trials_catch
    else:
        return trials


def prepare_figure(trial, pos=(0, 0)):
    """Prepare a figure given settings in a trial."""
    # A hack to not present figures on auditory-only trials
    if trial['any_visual']:
        # If the figure's current flip-state is not desired, flip it horizontally.
        figure_template.pos = pos
        figure_template.flipped = int(not figure_template.flipped)  # Now opposite

        # Draw fill
        figure_template.image = path.join('stimuli', '%(shape)s_%(complexity)s.png' %trial)
        figure_template.color = colors[trial['hue']][trial['lightness']]  # Select correct color
        figure_template.draw()

        # Draw outline
        figure_template.color = (255, 255, 255)  # Preserve original color
        figure_template.image = path.join('stimuli', '%(shape)s_%(complexity)s_outline.png' %trial)
        figure_template.draw()

        # Screenshot
        figure = visual.BufferImageStim(win)  # Screenshot

        # Clean up and return
        figure.draw()  # First draw is slower
        win.clearBuffer()  # Wipe screen
        figure_template.pos = (0, 0)  # back to center
        return figure
    else:
        return figure_blank


def prepare_beep(trial):
    """Prepare a beep given settings in trial."""
    if trial['any_auditory']:
        return beeps[trial['timbre']][trial['pitch']]  # Select the correct beep
    else:
        return beep_blank


def check_quit(key):
    """Quit gracefully on QUIT_KEYS. Necessary for some systems and psychopy versions."""
    if key in QUIT_KEYS:
        win.close()
        core.quit()


def waitGamepad(keyList=[], maxWait=float('inf'), timeStamped=True, state='down'):
    """
    Equivalent to psychopy.event.waitKeys(timeStamped=True) but for gamepads using the inputs module.

    maxWait: number of seconds to wait before returning None
    timeStamped: True, False or a core.Clock
    state: 'down' or 'release'. The state of the button.
    """
    if timeStamped in (True, False):
        clock.reset()
    else:
        pass  # Using the same clock

    while clock.getTime() < maxWait:
        presses = inputs.get_gamepad()
        for press in presses:
            if press.code in keyList:
                if (state == 'down' and press.state != 0) or (state == 'release' and press.state == 0):
                    return press.code, clock.getTime()

    else:
        return '', ''


def waitResponse(maxWait=float('inf'), keyList=[], timeStamped=True):
    """Abstraction layer to listen to either keyboard or gamepad"""

    if DIALOGUE['input'] == 'gamepad':
        keyList_gamepad = [KEYS_GAMEPAD[key] for key in keyList]
        key, rt = waitGamepad(keyList=keyList_gamepad, maxWait=maxWait, timeStamped=timeStamped, state='down')
        #print'line', key, rt, maxWait, keyList, keyList_gamepad
        if key:
            key = KEYS_GAMEPAD_REVERSE[key]

    elif DIALOGUE['input'] == 'keyboard':
        response = event.waitKeys(keyList=keyList, maxWait=maxWait, timeStamped=timeStamped)
        if response:
            key, rt = response[0]
        else:
            key, rt = '', ''

    return key, rt


def show_instruction(text, keyList = CONTINUE_KEYS, maxWait = float('inf'), pos=TEXT_POS):
    """ Show a text, wait for key press and return the key"""
    instruction.pos = pos
    instruction.text = text
    instruction.draw()
    win.flip()
    keyList.extend(QUIT_KEYS)

    if DIALOGUE['simulation'] == 'no':
        key, rt = waitResponse(keyList=keyList, maxWait=maxWait)
        check_quit(key)
    else:
        key = random.choice(CONTINUE_KEYS)
        rt = random.random()

    return key, rt


def make_instruction(trial):
    """Returns the instruction for a given trial, including the correct response keys"""
    final_instruction = task_instructions[trial['task']] % (key_map['chars'][0], key_map['chars'][1]) #keys_to_show  # Update the instruction with these characters
    return final_instruction


def demo_figure(task, inducer):
    """Show which key 'belongs' to which figure attribute."""
    # Loop until a key is pressed
    trials = make_trials(task, inducer, phase='demo')
    while True:
        for i, key in enumerate(RESPONSE_KEYS):
            # Prepare figure. Choose a random figure, fix the target attribute to the value that goes with this key, set position
            trial = random.choice(trials)  # Choose a random trial
            trial[task] = trial[key]  # Fix the target attribute (avoid randomness). E.g. trial['shape'] = 'pointy'
            figure = prepare_figure(trial, pos=DEMO_POSITIONS[i])  # Generate a figure using these settings
            figure.draw()

        # Show it. If response key is pressed, continue the loop. If continue key, break it and begin the task.
        key, rt = show_instruction(make_instruction(trial) + texts['continue'], keyList=CONTINUE_KEYS + RESPONSE_KEYS)
        if key in CONTINUE_KEYS:
            return


def demo_beep(task, inducer):
    """Show which key 'belongs' to which beep attribute."""
    # For code comments, see demo_figure function
    # Loop until a key is pressed
    trials = make_trials(task, inducer, phase='demo')
    while True:
        for i, key in enumerate(RESPONSE_KEYS):
            trial = random.choice(trials)
            trial[task] = trial[key]
            beep = prepare_beep(trial)
            win.callOnFlip(beep.play)  # When instruction is shown

            key_demo.pos = DEMO_POSITIONS[i]
            key_demo.text = RESPONSE_TEXT[RESPONSE_KEYS[i]]
            key_demo.draw()

            key, rt = show_instruction(make_instruction(trial) + texts['continue'], keyList=CONTINUE_KEYS + RESPONSE_KEYS)
            if key in CONTINUE_KEYS:
                return


def rate_CMC(feature1, feature2):
    """Show alternative CMCs and ask participants which they prefer"""
    # Instruction
    text = texts['rate_instruct'] %(feature_reminders2[feature1].upper(), feature_reminders2[feature2].upper())
    any_auditory = 'auditory' in (modalities[feature1], modalities[feature2])
    if any_auditory:
        text += texts['rate_click']

    # Get a simgle trial, just to get it's data structure
    trials = make_trials(task=feature1, inducer=feature2, phase='rate')
    trial = random.choice(trials)  # Use one base stimulus

    # Show a neutral placeholder, even if there is nothing visual.
    if not trial['any_visual']:
        trial['any_visual'] = 1
        trial['shape'] = trial['complexity'] = trial['hue'] = trial['lightness'] = 'neutral'

    # SCREENSHOT
    # Setting actual features and showing stimuli
    # We will address stimuli by their position[column][row]
    order = [[[0, 0], [1,1]], [[0, 1], [1,0]]]  # Order of the features in the format: order[column][row]
    groups = [0, 1]
    examples = [0, 1]
    for group in groups:
        for example in examples:
            # Set trial features
            trial[feature1] = features[feature1][order[group][example][0]]  # Some heavy indexing here!
            trial[feature2] = features[feature2][order[group][example][1]]
            trial['flip'] = 0  # So that figure and outline match

            # Prepare figure and show it
            figure = prepare_figure(trial, pos=RATING_POSITIONS[group][example])
            figure.draw()

    # Take screenshot before drawing
    instruction.pos = TEXT_POS
    instruction.text = text
    instruction.draw()
    rating_screen = visual.BufferImageStim(win)
    rating_screen.draw()  # First draw is slower
    win.clearBuffer()  # Preparation finished. Delete what's been drawn so far
    win.callOnFlip(clock.reset)  # Set time=0 on first draw hereafter

    # REGISTER RESPONSES WHEN AUDIO IS INVOLVED
    # Mouse clicks on trials with an auditory component
    if any_auditory:
        # Continue until all shapes are pressed
        figure_untouched = [[True, True], [True, True]]  # not_clicked[group][row]
        awaiting_beeps = True
        awaiting_keyboard = True
        while awaiting_beeps or awaiting_keyboard:
            # Show it. Only show VAS when all sounds have been played
            rating_screen.draw()
            if not awaiting_beeps:
                VAS.draw()
                press = event.getKeys(keyList=['escape'])
                if press:
                    core.quit()

                # Continue until there is a VAS response
                if not VAS.noResponse:
                    awaiting_keyboard = False  # Do not listen for responses anymore
            win.flip()

            # Virtually loop through figures and listen for mouse presses on them
            for group in groups:
                for example in examples:
                    figure_fill.pos = RATING_POSITIONS[group][example]

                    # Play sound and register
                    if mouse.isPressedIn(figure_fill):
                        # Play sound
                        trial[feature1] = features[feature1][order[group][example][0]]  # Some heavy indexing here!
                        trial[feature2] = features[feature2][order[group][example][1]]
                        beep = prepare_beep(trial)
                        beep.play()

                        # Register click
                        figure_untouched[group][example] = False

                        # Wait for mouse release
                        while mouse.isPressedIn(figure_fill):
                            pass

            # Status: how many touched now?
            awaiting_beeps = any(figure_untouched[0]) or any(figure_untouched[1])

            # Simulate responses prematurely
            if DIALOGUE['simulation'] == 'yes':
                figure_untouched = [[False, False], [False, False]]
                VAS.noResponse = False
                VAS.FINISHED = True
                VAS.decisionTime = random.random()

    # REGISTER RESPONSES FOR PURELY VISUAL CMCS
    else:
        # Simulate responses prematurely
        if DIALOGUE['simulation'] == 'yes':
            clock.reset(newT=RATING_WAIT+1)
            VAS.noResponse = False
            VAS.FINISHED = True
            VAS.decisionTime = random.random()

        # Get response: new example or rate it?
        while VAS.noResponse:
            rating_screen.draw()
            if clock.getTime() > RATING_WAIT:
                VAS.draw()
            win.flip()

    # SCORE TRIAL
    trial['key'] = VAS.getRating()
    trial['rt'] = VAS.getRT()
    VAS.reset()  # Ready for next rating

    # Trial info since we messed with these since make_block
    trial['left'] = features[feature1][order[0][0][0]] + ':' + features[feature2][order[0][0][1]]
    trial['right'] = features[feature1][order[1][0][0]] + ':' + features[feature2][order[1][0][1]]
    trial['CMC_code'] = '%s:%s' % (trial[feature1], trial[feature2])  # Update code to match
    writer.write(trial)
    writer.flush()

    # Optionally quit
    if event.getKeys(QUIT_KEYS):
        check_quit(QUIT_KEYS[0])  # Convoluted since we know that any response is a quit response; but consistently runs the same code on quit


def run_block(task, inducer, phase='exp'):
    """
    Present a list of trials and collect responses.
    Supports practice and a simulation.
    """
    global trial_no_global

    # Now loop through trials and collect dataI hope it's 
    trials = make_trials(task, inducer, phase)

    # Prepare continuous instruction for practice trials
    if phase == 'practice_exp':
        instruction.pos = TEXT_POS
        instruction.text = make_instruction(trials[0])
        instruction.autoDraw = True

    # Loop through trials
    for no_task, trial in enumerate(trials):
        # PREPARATION
        # Prepare stimuli for normal trials
        if not trial['catch']:
            # For practice
            if phase == 'practice_exp':
                # Make a balanced sequence of practice examples, demoing both response options.
                practice_block_no = no_task % PRACTICE_TRIALS  # Which number trial is this in the block of length PRACTICE_TRIALS?
                if practice_block_no == 0:
                    correct_responses = RESPONSE_KEYS*int(np.ceil(PRACTICE_TRIALS/2))  # List of keys
                    random.shuffle(correct_responses)  # Random order
                trial[task] = trial[correct_responses[practice_block_no]]  # Now fix this trial to show this

            # Always
            figure = prepare_figure(trial)
            beep = prepare_beep(trial)

        # Prepare stimuli for catch trials
        else:
            # Always
            if trial['modality_task'] == 'visual':
                figure = prepare_figure(trial)  # Same figure type as always
                beep = catches['auditory'][trial['catch_stim']]  # Catch beep
            elif trial['modality_task'] == 'auditory':
                figure = catches['visual'][trial['catch_stim']]  # Catch figure
                beep = prepare_beep(trial)  # Same beep type as always

        # STIMULATION
        # Fixation cross
        for frame in range(trial['fixation_duration']):
            fixation.draw()
            win.flip()

        # Stimulus!
        for frame in range(FIGURE_DURATION):
            figure.draw()
            win.flip()
            if frame == 0:
                clock.reset()
                beep.play()

        # Stimulus offset.
        fixation.draw()
        win.flip()

        # RESPONSES. Continue until a task-relevant answer has been collected
        while True:
            # Get response from regular trials. Simulate it otherwise
            if DIALOGUE['simulation'] == 'no':
                fixation.draw()
                key, rt = waitResponse(keyList=RESPONSE_KEYS + CATCH_KEYS + QUIT_KEYS, timeStamped=clock)
            else:
                key = random.choice(RESPONSE_KEYS + CATCH_KEYS)
                rt = random.random()

            # React to responses and save in the appropriate place in the data structure.
            # First responses only!
            check_quit(key)
            if key in RESPONSE_KEYS and not trial['key']:
                trial['key'] = key
                trial['rt'] = rt
                trial['score'] = int(trial[key] == trial[task])
                if not trial['catch']:
                    break
            if key in CATCH_KEYS and not trial['catch_key']:
                trial['catch_key'] = key
                trial['catch_rt'] = rt
                trial['catch_score'] = int(key == trial['catch_stim']) if trial['catch'] else 0
                if trial['catch']:
                    break

        # POST-PROCESSING
        # Save it. I would normally add trial numbers in make_trials,
        # but we don't want to do it for demo trials and skipped practice trials
        trial['no_task'] = no_task + 1  # Start at 1
        trial['no_global'] = trial_no_global
        trial_no_global += 1
        writer.write(trial)
        if not DIALOGUE['simulation']:
            writer.flush()  # save after each trial, just to make sure
        
        # Feedback during practice
        if phase in ('practice_exp', 'practice_catch'):
            target_score = 'catch_score' if phase == 'practice_catch' else 'score'
            feedback.text = _(u'Correct!') if trial[target_score] else _(u'Wrong!')

            if DIALOGUE['simulation'] == 'no':  # In simulation mode, we present nothing and let it run random answers until success!
                for frame in range(FEEDBACK_DURATION):
                    feedback.draw()
                    win.flip()

            # End practice if the performance over the last PRACTICE_TRIALS trials was satisfactory
            if trial['no_task'] >= PRACTICE_TRIALS:
                score_these = trials[trial['no_task']-PRACTICE_TRIALS:trial['no_task']]  # The last PRACTICE_TRIALS trials
                score_key = 'catch_score' if trial['catch'] else 'score'  # What is the column to score?
                scores = [previous_trial[score_key] for previous_trial in score_these]  # The last PRACTICE_TRIALS scores
                if sum(scores) >= MINIMUM_PRACTICE_SCORE:
                    instruction.autoDraw = False
                    return

    # Finally turn off
    key_reminders['left'].autoDraw = False
    key_reminders['right'].autoDraw = False


"""
RUN IT
"""
if DIALOGUE['section'] == 'rate_CMC':
    show_instruction(texts['rate_intro1'], pos=(0, 0))
    show_instruction(texts['rate_intro2'], pos=(0, 0))

    combinations = list(itertools.combinations(RATE_FEATURES, 2))  # Unique combinations
    random.shuffle(combinations)  # Random order
    for repetition in range(RATE_REPETITIONS):
        for feature1, feature2 in combinations:
            rate_CMC(feature1, feature2)

elif DIALOGUE['section'] == 'test_CMC':
    if DIALOGUE['demo_and_practice'] == 'yes':
        # Welcome
        show_instruction(texts['welcome'], pos=(0, 0))

        # Demo catch trials. Task doesn't matter here since all "real" trials are filtered out. 
        #show_instruction(texts['practice_catch'])
        #run_block('shape', 'pitch', phase='practice_catch')

    # The order to run the tasks in each superblock (random)
    random.shuffle(TEST_FEATURES)

    # Two super-blocks
    N_superblocks = len(key_feature_maps['shape'])  # Number of response orders
    for superblock in range(N_superblocks):  # Assuming that all have equal length
        # Run tasks in random order for each super-block
        for i, task in enumerate(TEST_FEATURES):

            # All other features as inducers - in random order
            inducers = copy.copy(TEST_FEATURES)
            inducers.remove(task)  # Only non-task features
            for inducer in inducers:  # Only inducers in the opposite sensory modality
                if modalities[task] == modalities[inducer]:
                    inducers.remove(inducer)
            random.shuffle(inducers)

            # Get ready. Show number of blocks left
            show_instruction(texts['break'] % (N_superblocks*len(TEST_FEATURES) - superblock*len(TEST_FEATURES) - i, CONTINUE_TEXT[DIALOGUE['input']]))

            # Get the response key mapping for this task and super-block
            response_order = key_feature_maps[task][superblock]  # Pick mapping for this repetition
            key_map = {}  # Will hold all information about key mapping
            key_map['feature_reminders'] = feature_reminders[task]  # Always presented in the same order. It's the keys that vary
            key_map['keys'] = RESPONSE_KEYS if response_order == 1 else list(reversed(RESPONSE_KEYS))
            key_map['indices'] = (0, 1) if key_map['keys'][0] == 'left' else (1, 0)
            key_map['chars'] = RESPONSE_TEXT[key_map['keys'][0]], RESPONSE_TEXT[key_map['keys'][1]]

            key_reminders['left'].text = u'%s %s' % (RESPONSE_TEXT['left'], key_map['feature_reminders'][key_map['indices'][0]])
            key_reminders['right'].text = u'%s %s' % (key_map['feature_reminders'][key_map['indices'][1]], RESPONSE_TEXT['right'])
            key_reminders['left'].autoDraw = True
            key_reminders['right'].autoDraw = True

            # Optionally skip demo and practice (only for development)
            if DIALOGUE['demo_and_practice'] == 'yes':
                # Demo of task. Demo using the first inducer
                if task in ('timbre', 'pitch'):
                    demo_beep(task, inducers[0])
                else:
                    demo_figure(task, inducers[0])

                # Practice it
                show_instruction(texts['practice_exp'])
                run_block(task, inducers[0], phase='practice_exp')

            # Run task!
            show_instruction(texts['after_demo'])

            for inducer in inducers:
                print('running task=%s and inducer=%s' % (task, inducer))
                key_reminders['left'].autoDraw = True
                key_reminders['right'].autoDraw = True
                run_block(task, inducer, phase='exp')
                writer.flush()


# Bye
show_instruction(texts['bye'])

# Needed on some computers!
win.close()
core.quit()
