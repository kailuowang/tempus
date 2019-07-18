package tempus
package timeSeries
package time

import java.time.ZoneId

/**
 * Bring Zone to the type level
 */
sealed trait Zone extends Product with Serializable {
  def id: ZoneId
}

object Zone {

  implicit case object UTC extends Zone {
    val id = ZoneId.of("UTC")
  }

  implicit case object EST extends Zone {
    val id = ZoneId.of(ZoneId.SHORT_IDS.get("EST"))
  }

  type UTC = UTC.type
  type EST = EST.type
}
