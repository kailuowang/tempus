package tempus

import tempus.timeSeries.syntax

object implicits extends AllInstances with AllSyntax

trait AllSyntax extends syntax.TimeSeriesSyntax with syntax.TimeSyntax

trait AllInstances
    extends time.InstantInstances
    with time.DurationInstances
    with time.LocalDateInstances
