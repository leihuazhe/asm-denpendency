package asm

import org.objectweb.asm.Opcodes.ASM5
import org.objectweb.asm._
import scala.collection.mutable.ListBuffer


object GoodsAsmTest {
  // goods-api 包
  val classIs: collection.mutable.Map[String, Array[Byte]] = Util.getJarFileClass("/data/goods-api_2.12.jar")
  //goods-service 包
  val classIsService: collection.mutable.Map[String, Array[Byte]] = Util.getJarFileClass("/data/goods_service.jar")

  val classIsServiceFile = Util.getFileClass("/data/classes")


  val flags = ClassReader.SKIP_DEBUG

  val services = collection.mutable.Set[String]()

  val methods = collection.mutable.Map[String, collection.mutable.Set[String]]()
  //  val isCached = collection.mutable.Map[String, InputStream]()
  val callClientSet = collection.mutable.Set[String]()

  /**
    *
    * @param args
    */
  def main(args: Array[String]): Unit = {
    //第一步，找到所有的 Service 和 Client 类，存到 Map 中
    AsmServiceHelper.getServiceAndClient(classIs, services)
    //第二步，查找这些service和client的方法
    AsmServiceHelper.getServiceMethod(classIs, services, methods)
    //第三步，扫描所有类，找到有Client字段的类.
    AsmServiceHelper.getFieldIncludeClient(classIsServiceFile, callClientSet)
    callClientSet.foreach(println)
  }

  /*



      /**
        * 第三步， 查找这些服务的方法
        */


      classIs.foreach(is => {
        try {
          val cr2: ClassReader = new ClassReader(is._2)

          cr2.accept(new MethodClassVisitor(services, methods), flags)
          println()
        } catch {
          case e: Exception => System.err.println(is._1)
        }

      })
      methods.values.foreach(k => println(k))


      /**
        * 第四步， 扫描A包下的所有的类，查找所有对services 下的方法的调用集合， 设为 calls: List<Method -> Method>
        */
      classIsService.foreach(is => {
        try {
          val cr2: ClassReader = new ClassReader(is._2)

          cr2.accept(new CalledClassVisitor(services, methods), flags)
          println()
        } catch {
          case e: Exception => System.err.println(is._1)
        }

      })
      //    /Users/maple/ideaspace/product/goods/goods-service/target/classes/com/today/service/goods/GoodsServiceImpl.class

      try {
        //GoodsServiceImpl
        //            val cr2: ClassReader = new ClassReader(new FileInputStream("/Users/maple/ideaspace/product/goods/goods-service/target/classes/com/today/service/goods/GoodsServiceImpl.class"))
        val cr2: ClassReader = new ClassReader(new FileInputStream("/Users/maple/ideaspace/product/goods/goods-service/target/classes/com/today/service/goods/query/ListSkuByDetailByNosQuery.class"))

        //      val cr2: ClassReader = new ClassReader(new FileInputStream("/Users/maple/ideaspace/product/goods/goods-service/target/classes/com/today/commons/GoodsUtil.class"))
        //      val cr2: ClassReader = new ClassReader(new FileInputStream("/Users/maple/ideaspace/product/goods/goods-service/target/classes/com/today/service/skuprice/action/sql/SkuPriceActionSql$.class"))

        //      val cr2: ClassReader = new ClassReader(new FileInputStream("/Users/maple/ideaspace/product/goods/goods-service/target/classes/com/today/service/skuprice/SkuPriceServiceImpl.class"))


        //      /Users/maple/ideaspace/product/goods/goods-service/target/classes/com/today/commons/GoodsUtil.class


        cr2.accept(new CalledClassVisitor(services, methods), flags)
        println()
      } catch {
        case e: Exception => System.err.println("exception")


      }


    }
  }*/


  private class MyFieldVisitor() extends FieldVisitor(ASM5) {
    override def visitAttribute(attr: Attribute): Unit = {
      if (fv != null) fv.visitAttribute(attr)
    }
  }


  /**
    *
    * called visitor <--->
    *
    * @param services
    */
  private class CalledClassVisitor(val services: ListBuffer[String], val methods: collection.mutable.Map[String, collection.mutable.Set[String]]) extends ClassVisitor(ASM5) {
    var className: String = _

    /**
      * order -> 1
      */
    override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]): Unit = {
      super.visit(version, access, name, signature, superName, interfaces)
      this.className = name
    }

    /**
      *
      * @param access
      * @param name service 方法名称
      * @param desc service 方法上的参数
      * @param signature
      * @param exceptions
      * @return
      */
    override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor = {
      if (cv != null) return cv.visitMethod(access, name, desc, signature, exceptions)
      println(s"【第一步】:service's method: name: $name, desc: $desc,signature $signature exceptions $exceptions ")
      println(s"【第二步】:服务类： ${this.className}    方法名称: name: $name")

      new CalledMethodVisitor(this, name, signature, methods)
    }


    /**
      * service method visitor
      * 改变方法里的逻辑，获取方法内部的逻辑
      */


  }

  /**
    * called method visitor
    *
    * @param clsVisitor
    * @param name
    * @param signature
    */
  private class CalledMethodVisitor(var clsVisitor: CalledClassVisitor, var parentMethodName: String, var signature: String, val methods: collection.mutable.Map[String, collection.mutable.Set[String]]) extends MethodVisitor(ASM5) {

    /**
      * visitor method
      *
      * @param opcode
      * @param owner com/github/dapeng/service/user/action/RegisterAction
      * @param name  调用的方法
      * @param desc  调用的方法里的参数
      * @param itf   该方法是不是属于一个接口类
      *
      *  clsVisitor.name 查询是在哪个类里调用了这个服务，这个方法
      *
      */
    override def visitMethodInsn(opcode: Int, owner: String, name: String, desc: String, itf: Boolean): Unit = {

      methods.get(owner) match {
        case Some(x) =>
        case None =>
      }


      println("【第三步】: owner:" + owner + ", name:" + name + ", desc:" + desc)

      println(s"【第四步】: parentMethodName  $parentMethodName ,  className: ${clsVisitor.className}  ,methodName:  $name , signature : $signature")

      println("<------------------------------分割线----------------------------------------->")
      println()
    }


  }

}


