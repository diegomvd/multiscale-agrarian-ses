package MultiScaleAgrarianSES

/**
 * A base landscape is a landscape over which a Voronoi tesselation can be performed in a way that it becomes the base of
 * another landscape. Groups of units of the primitive landscape are the elementary units of the secondary landscape and
 * so on.
 */
trait BaseLandscape[T]extends Landscape[T]with VoronoiTesselation[T]
