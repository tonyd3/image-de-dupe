package com.td.dedupe

import java.io.File
import java.awt.image.BufferedImage
import java.awt.Color
import javax.imageio.ImageIO

object ImageDeDupe {

  def main(args: Array[String]) {
    println("Begin Scanning given directories")
    val files = args flatMap {
      dirName => new File(dirName) match {
        case file: File => if (file.exists && file.isDirectory) file.listFiles() else List.empty
      }
    }
    val imageMap = files.map {
      file => println("Reading: " + file.getAbsolutePath);
        (file.getAbsolutePath, getLuminance(resizeImage(file.getAbsolutePath, 32, 32)))
    }
    println("Comparing Images")
    imageMap.foreach {
      file1 =>
        imageMap.splitAt(imageMap.indexOf(file1))._2.foreach {
          file2 => if (file1._1 != file2._1 && compareImages(file1._2, file2._2))
            println("Duplicate: " + file1._1 + " and " + file2._1)

        }
    }
    println("Done")
  }

  def compareImages(file1: Seq[Double], file2: Seq[Double]): Boolean = {
    val mean1 = file1.sum / file1.size
    val mean2 = file2.sum / file2.size
    val denom = Math.sqrt(file1.map(x => Math.pow((x - mean1), 2)).sum * file2.map(x => Math.pow((x - mean2), 2)).sum);
    file1.zip(file2).map {
      case (lum1: Double, lum2: Double) => (lum1 - mean1) * (lum2 - mean2)
    }.sum / denom > 0.95

  }

  def getLuminance(image: BufferedImage): Seq[Double] = {
    (for (i <- 0 to image.getHeight - 1; j <- 0 to (image.getWidth - 1)) yield {
      val c = new Color(image.getRGB(i, j));
      0.299 * c.getRed + 0.587 * c.getGreen + 0.114 * c.getBlue
    }).toList
  }

  def resizeImage(path: String, width: Integer, height: Integer): BufferedImage = {
    val originalImage = ImageIO.read(new File(path));
    val resizedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
    val g = resizedImage.createGraphics();
    g.drawImage(originalImage, 0, 0, width, height, null);
    g.dispose();

    resizedImage;
  }
}
