package asm

import java.io.File
import java.util
import java.util.jar.{JarEntry, JarFile}

/**
  *
  * 描述: asm utils
  *
  * @author hz.lei
  * @date 2018年04月03日 上午11:06
  */
object Util {
  /**
    * 获取 goods-api_2.12.jar 下的 所有 .class
    *
    * Map( "com.maple.UserService.class"
    */
  def getJarFileClass(path: String): collection.mutable.Map[String, Array[Byte]] = {
    val inputStreams = collection.mutable.Map[String, Array[Byte]]()
    val jarFile = new JarFile(new File(path))
    val entries: util.Enumeration[JarEntry] = jarFile.entries()
    while (entries.hasMoreElements) {
      val path = entries.nextElement().getName
      if (path.endsWith(".class")) {
        val is = jarFile.getInputStream(entries.nextElement())
        val content = new Array[Byte](is.available())
        is.read(content, 0, is.available())
        inputStreams += (path -> content)
      }
    }
    inputStreams
  }

  /**
    * classes 下
    *
    * @param path
    * @return
    */
  def getFileClass(path: String): List[String] = {
    val classFiles = subdirs3(new File(path))
    classFiles.toList.map(_.getPath).filter(_.endsWith(".class"))
  }


  def subdirs3(dir: File): Iterator[File] = {
    val d = dir.listFiles.filter(_.isDirectory)
    val f = dir.listFiles.toIterator
    f ++ d.toIterator.flatMap(subdirs3 _)
  }


}
