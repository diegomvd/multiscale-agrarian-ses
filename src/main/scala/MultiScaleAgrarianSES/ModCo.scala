package MultiScaleAgrarianSES
/**
 * Implementation of hexagonal modulo coordinates. Defines all the functions needed to manipulate the modulo coordinates
 * in the hexagonal lattice. Used in EcoLandscape to create and locate the EcoUnits in different positions of an hexagonal
 * lattice and define their neighborhood using distance in this lattice to create the composition graph.
 */

object ModCo:
        /**
         * Builds all of the possible hexagonal modulo coordinates given the radius of the landscape.
         *
         * @constructor
         * @param r is the radius of the landscape
         * @return a List with each of the modulo coordinates
         * */
        def apply(
        r:Int):
        List[Int]=
        (0until area(r)).map(x=>x).toList

        /**
         * Calculates area of an hexagonal landscape given its radius
         *
         * @param r is the landscape's radius
         * @return the landscape's area
         * */
        def area(
        r:Int):
        Int=
        3*r*r+3*r+1

        /**
         * Transforms a modulo coordinate to a cubic coordinate of the hexagonal landscape
         *
         * @param mod the modulo coordinate
         * @param rad the landscape's radius
         * @return a tuple containing the cubic coordinates
         * */
        def toCubic(
        mod:Int,
        rad:Int):
        (Int,Int)={
        // helper values
        val shift=3*rad+2
        val ms=(mod+rad)/shift
        val mcs=(mod+2)*rad/(shift-1)

        // Need to make sure of this formula and check what is each coordinate
        // spatially
        val q=ms*(rad+1)+mcs*rad
        val r=ms+ms*(-2*rad-1)+mcs*(-rad-1)
        (q,r)
        }

        /**
         * Transforms a cubic coordinate to a modulo coordinate of the hexagonal landscape
         *
         * @param cub the cubic coordinate
         * @param rad the landscape's radius
         * @return the modulo coordinate
         * */
        def toModulo(
        cub:(Int,Int),
        rad:Int):
        Int={
        // helper values
        val area=3*rad*rad+3*rad+1
        val shift=3*rad+2
        val div=cub(0)+shift*cub(1)
        // "%" operator is the remainder operator in scala, with the function below we
        // obtain the modulo. Need to make sure this is right
        ((div%area)+area)%area
        }

        /**
         * Calculates the hexagonal cells in the neighborhood of a cell.
         *
         * @param mod the modulo coordinate.
         * @param rad the landscape's radius.
         * @param thr the threshold distance at which cells are considered neighbors.
         * @return a List with the modulo coordinates of the neighbors.
         * */
        def neighbors(
        mod:Int,
        rad:Int,
        thr:Int):
        List[Int]= {
                val cub = toCubic(mod, rad)

                val q_from = cub(0) - thr
                val qto = cub(0) + thr
                val r_from = cub(1) - thr
                val rto = cub(1) + thr

                (q_from to qto).flatMap { q =>
                        (r_from to rto).map(r => toModulo((q, r), rad))
                }.toList
        }

end ModCo
