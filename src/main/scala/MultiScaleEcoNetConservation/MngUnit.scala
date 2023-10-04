package MultiScaleEcoNetConservation

import MultiScaleAgrarianSES.LandscapeUnit

case class MngUnit(
                    id: Long,
                    composition: Vector[Long],
                    protectionLevel: Double
                  )
extends LandscapeUnit

