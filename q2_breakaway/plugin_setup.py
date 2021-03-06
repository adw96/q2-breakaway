# ----------------------------------------------------------------------------
# Copyright (c) 2016-2018, QIIME 2 development team.
#
# Distributed under the terms of the Modified BSD License.
#
# The full license is in the file LICENSE, distributed with this software.
# ----------------------------------------------------------------------------
import importlib

import qiime2.plugin
from qiime2.plugin import Properties, Choices, Str

from q2_types.sample_data import SampleData, AlphaDiversity
from q2_types.feature_data import FeatureData, Sequence
from q2_types.feature_table import FeatureTable, Frequency

import q2_breakaway

plugin = qiime2.plugin.Plugin(
    name='breakaway',
    version=q2_breakaway.__version__,
    website='http://github.com/adw96/breakaway/',
    package='q2_breakaway',
    description=('baway'),
    short_description='ba.',
    citations=qiime2.plugin.Citations.load('citations.bib', package='q2_breakaway')
)

_METRIC_CHOICES = {'richness', 'chao_bunge'}

plugin.methods.register_function(
    function=q2_breakaway.breakaway,
    inputs={'table': FeatureTable[Frequency]},
    outputs=[('alpha_diversity', SampleData[AlphaDiversity % Properties(["StandardError", "LowerConfidence", "UpperConfidence"])])],
    parameters={'metric': Str % Choices(_METRIC_CHOICES)},
    input_descriptions={
        'table': ('The feature table containing the samples for which alpha '
                  'diversity should be computed.')
    },
    parameter_descriptions={
        'metric': 'An alpha diversity metric',
    },
    output_descriptions={
        'alpha_diversity': 'Vector containing per-sample alpha diversities.'
    },
    name='Richness, better',
    description='Amy\'s opinionated ideas about richness as a q2, '
                'plug-in.'
)
