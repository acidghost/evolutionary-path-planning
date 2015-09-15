package nl.vu.ai.acidghost.ec.ga

import com.typesafe.config.ConfigFactory

/**
 * Created by acidghost on 13/09/15.
 */
object Configuration {

    private val conf = ConfigFactory.load()

    def getBoolean(path: String): Boolean = conf.getBoolean(s"params.$path")
    def getDouble(path: String): Double = conf.getDouble(s"params.$path")
    def getInt(path: String): Int = conf.getInt(s"params.$path")

    val mapSize = (conf.getInt("map.m"), conf.getInt("map.n"))

    val minInitial = mapSize._1 / 2
    val maxInitial = minInitial * 2

}
