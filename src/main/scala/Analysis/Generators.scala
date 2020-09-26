package Analysis

import java.io.{BufferedWriter, FileWriter}

import Shapez.Shape
import au.com.bytecode.opencsv.CSVWriter

import scala.collection.JavaConverters._


object Generators extends App {
  override def main(args: Array[String]): Unit = {
    createCategorizedShapesCSV()
    //    bruteForcedShapesCSV()
  }

  def createCategorizedShapesCSV(): Unit = {
    val outputFile = new BufferedWriter(new FileWriter("GeneratedFiles/allStructuresAnalysis.csv"))
    val csvWriter = new CSVWriter(outputFile)
    val outputContents =
      Array("Shape", "Possible?", "Freeplay?", "Simple Floating?", "Multilayer Scaffolding?", "Drop Through?") +:
        Shape().listAllStructures.map(shape =>
          Array(
            shape.toString,
            Detectors.isPossible(shape).toString,
            Detectors.isFreeplay(shape).toString,
            Detectors.isSimpleFloating(shape).toString,
            Detectors.isMultilayerScaffolding(shape).toString,
            Detectors.isDropThrough(shape).toString))
    csvWriter.writeAll(outputContents.toList.asJava)
    outputFile.close()
  }

  def bruteForcedShapesCSV(): Unit = {
    val outputFile = new BufferedWriter(new FileWriter("GeneratedFiles/bruteForcedShapes.csv"))
    val csvWriter = new CSVWriter(outputFile)
    val outputContents: List[Array[String]] = Shape().bruteForceProducableShapes.map(shape => Array(shape.toString)).toList
    csvWriter.writeAll(outputContents.asJava)
    outputFile.close()
  }
}
