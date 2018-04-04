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
        val cr2: ClassReader = new ClassReader(is._2)
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
    classIsService /*.filter(x => x == "/data/classes/com/today/service/goods/query/ListSkuByConditionsQuery.class")*/ .foreach(is => {
      try {
        val cr2: ClassReader = new ClassReader(new FileInputStream(is))
        cr2.accept(new CalledClassVisitor(callClientSet, callAssociated), flags)
      } catch {
        case e: Exception =>
      }
    })
  }

  /**
    * 递归遍历
    *
    * @param callAssociated
    */
  def getCalledMethodSecond(classIsService: List[String], callAssociated: mutable.Map[String, mutable.Set[String]]): Unit = {

    classIsService /*.filter(x => x == "/data/classes/com/today/service/goods/action/sql/BarcodeSql$.class")*/ .foreach(is => {
      try {
        val cr2: ClassReader = new ClassReader(new FileInputStream(is))
        cr2.accept(new CalledSecondClassVisitor(callAssociated), flags)
      } catch {
        case e: Exception =>
      }
    })


  }

  /**
    * format
    *
    * @param callAssociated
    */
  def formatMap(callAssociated: mutable.Map[String, mutable.Set[String]], callMap: mutable.Map[String, mutable.Set[String]]): Unit = {
    val actionMaps: mutable.Map[String, mutable.Set[String]] = callAssociated.filter(callMap => callMap._1.split("\\.")(0).endsWith("Action"))
    actionMaps.map(action => {
      getCalledHierarchy(callAssociated, action._1, action._1, callMap)
    })
    //    callMap.foreach(x => println(s"key:  ${x._1} \nvalue:  ${x._2} \n"))
  }

  def getCalledHierarchy(callAssociated: mutable.Map[String, mutable.Set[String]], key: String, rootKey: String, callMap: mutable.Map[String, mutable.Set[String]]): Unit = {

    if (callAssociated.get(key).nonEmpty) {
      val values: mutable.Set[String] = callAssociated.get(key).get


      if (callMap.get(rootKey).nonEmpty) {
        callMap.get(rootKey).get ++= (values)
      } else {
        callMap += (rootKey -> values)
      }

      values.filter(v => v != key).foreach(value => {
        getCalledHierarchy(callAssociated, value, rootKey, callMap)
      })
    }
  }

  def main(args: Array[String]): Unit = {
    val callAssociated = mutable.Map[String, mutable.Set[String]]()
    callAssociated += ("a" -> mutable.Set("b", "c"))
    callAssociated += ("b" -> mutable.Set("d", "e"))
    callAssociated += ("c" -> mutable.Set("f", "g"))
    callAssociated += ("d" -> mutable.Set("x", "z"))
    callAssociated += ("g" -> mutable.Set("x", "z"))
    val callmap = mutable.Map[String, mutable.Set[String]]()
    val res = getCalledHierarchy(callAssociated, "a", "a", callmap)

    println(callmap)

  }

  /**
    *
    * 服务端，拿到service实现类
    * 拿到service 实现类
    *
    * @param classIsServiceFile
    * @param serviceImpl
    */
  /**
    * 第一步，找到所有的Service服务类，存到 Map 中
    */
  def getServiceImpl(classService: List[String], servicesApi: collection.mutable.Set[String], servicesImpl: collection.mutable.Set[String]): Unit = {
    classService.foreach(is => {
      try {
        val cr: ClassReader = new ClassReader(new FileInputStream(is))
        cr.accept(new ServiceImplClassVisitor(servicesApi, servicesImpl), flags)
      } catch {
        case e: Exception => /*System.err.println(is._1)*/
      }
    })
    println(s"servicesImpl size:  ${servicesImpl.size}")
  }

  /**
    * 第二步，查找这些service和client的方法
    *
    * @param services
    * @param methods
    */
  def getServiceImplMethod(classService: List[String], services: collection.mutable.Set[String], methods: collection.mutable.Map[String, collection.mutable.Set[String]]): Unit = {
    classService.foreach(is => {
      try {
        val cr2: ClassReader = new ClassReader(new FileInputStream(is))
        cr2.accept(new MethodClassVisitor(services, methods), flags)
      } catch {
        case e: Exception => /* System.err.println(is._1)*/
      }
    })
  }


  def getServiceImplActionMethod(classIsService: List[String], serviceAction: mutable.Map[String, String], methodsImpl: collection.mutable.Map[String, collection.mutable.Set[String]]): Unit = {
    classIsService.foreach(is => {
      try {
        val cr2: ClassReader = new ClassReader(new FileInputStream(is))
        cr2.accept(new ServiceActionClassVisitor(serviceAction,methodsImpl), flags)
      } catch {
        case e: Exception =>
      }
    })
  }


}
