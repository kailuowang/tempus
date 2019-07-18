package tempus
package timeSeries

object implicits extends AllInstances with AllSyntax

trait AllSyntax extends syntax.TimeSeriesSyntax with syntax.TimeSyntax

trait AllInstances
    extends time.InstantInstances
    with time.DurationInstances
    with time.LocalDateInstances
