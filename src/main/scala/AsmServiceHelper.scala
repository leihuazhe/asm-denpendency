package asm

import java.io.FileInputStream

import org.objectweb.asm.ClassReader

import scala.collection.mutable

/**
  *
  * 描述:
  *
  * @author hz.lei
  * @date 2018年04月03日 上午11:09
  */
object AsmServiceHelper {


  val flags = ClassReader.SKIP_DEBUG

  /**
    * 第一步，找到所有的Service服务类，存到 Map 中
    */
  def getServiceAndClient(classIs: collection.mutable.Map[String, Array[Byte]], services: collection.mutable.Set[String]): Unit = {
    classIs.foreach(is => {
      try {
        val cr: ClassReader = new ClassReader(is._2)
        cr.accept(new ServiceClassVisitor(services), flags)
      } catch {
        case e: Exception => /*System.err.println(is._1)*/
      }
    })
    println(s"services size:  ${services.size}")

    classIs.foreach(is => {
      try {
        val cr2: ClassReader = new ClassReader(is._2)

        cr2.accept(new ClientClassVisitor(services), flags)
      } catch {
        case e: Exception => /*System.err.println(is._1)*/
      }
    })
    println(s"services and client size:  ${services.size}")

  }

  /**
    * 第二步，查找这些service和client的方法
    *
    * @param classIs
    * @param services
    * @param methods
    */

  val path = "/data/classes/com/today/commons/GoodsUtil$.class";

  def getServiceMethod(classIs: collection.mutable.Map[String, Array[Byte]], services: collection.mutable.Set[String], methods: collection.mutable.Map[String, collection.mutable.Set[String]]): Unit = {
    classIs.foreach(is => {
      try {
        //        val cr2: ClassReader = new ClassReader(is._2)
        val cr2: ClassReader = new ClassReader(new FileInputStream(path))
        cr2.accept(new MethodClassVisitor(services, methods), flags)
      } catch {
        case e: Exception => /* System.err.println(is._1)*/
      }

    })
    //    methods.values.foreach(k => println(k))
    methods.foreach(k => println(s"${k._1} -> ${k._2.size}"))
  }

  /**
    * 第三步，扫描所有类，找到有Client字段的类.
    * String path3 = "/data/classes/com/today/commons/GoodsUtil$.class";
    *
    * @param classService
    * @param callClientMap
    * classIs: mutable.Map[String, Array[Byte]]1
    * classService.foreach(is => {
    * try {
    * val cr2: ClassReader = new ClassReader(is._2)
    * cr2.accept(new FieldsClassVisitor(services), flags)
    * } catch {
    * case e: Exception =>
    * }
    * })
    *
    *
    */

  def getFieldIncludeClient(classService: List[String], callClientMap: collection.mutable.Set[String]): Unit = {
    classService.foreach(is => {
      try {
        val cr2: ClassReader = new ClassReader(new FileInputStream(is))
        cr2.accept(new FieldsClassVisitor(callClientMap), flags)
      } catch {
        case e: Exception =>
      }
    })
  }


  def getCalledMethod(classIsService: List[String], callClientSet: mutable.Set[String], callAssociated: collection.mutable.Map[String, mutable.Set[String]]): Unit = {
    classIsService/*.filter(x => x == "/data/classes/com/today/service/goods/query/ListSkuByConditionsQuery.class")*/.foreach(is => {
      try {
        val cr2: ClassReader = new ClassReader(new FileInputStream(is))
        cr2.accept(new CalledClassVisitor(callClientSet,callAssociated), flags)
      } catch {
        case e: Exception =>
      }
    })
  }


}
