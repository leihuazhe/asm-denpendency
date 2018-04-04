package asm

import org.objectweb.asm.Opcodes.ASM5
import org.objectweb.asm._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


object GoodsAsmTest {
  // goods-api 包
  val classIs: collection.mutable.Map[String, Array[Byte]] = Util.getJarFileClass("/data/goods-api_2.12.jar")
  //goods-service 包
  val classIsService: collection.mutable.Map[String, Array[Byte]] = Util.getJarFileClass("/data/goods_service.jar")

  val classIsServiceFile = Util.getFileClass("/data/classes")


  val flags = ClassReader.SKIP_DEBUG
  // api
  val services = collection.mutable.Set[String]()
  // serviceImpl
  val servicesImpl = collection.mutable.Set[String]()
  //api
  val methods = collection.mutable.Map[String, collection.mutable.Set[String]]()
  // impl
  val methodsImpl = collection.mutable.Map[String, collection.mutable.Set[String]]()
  //  val isCached = collection.mutable.Map[String, InputStream]()
  val callClientSet = collection.mutable.Set[String]()

  val callAssociated = collection.mutable.Map[String, mutable.Set[String]]()


  val callMap = mutable.Map[String, mutable.Set[String]]()

  /**
    *
    * @param args
    */
  def main(args: Array[String]): Unit = {
    val begin = System.currentTimeMillis()
    //第一步，找到所有的 Service 和 Client 类，存到 Map 中
    AsmServiceHelper.getServiceAndClient(classIs, services)
    //第二步，查找这些service和client的方法
    AsmServiceHelper.getServiceMethod(classIs, services, methods)
    //第三步，扫描所有类，找到有Client字段的类.
    AsmServiceHelper.getFieldIncludeClient(classIsServiceFile, callClientSet)
    callClientSet.foreach(println)
    //第四步，扫描goods_services 下所有class，遍历，如果方法里面有某个方法调用，而调用者是
    AsmServiceHelper.getCalledMethod(classIsServiceFile, callClientSet, callAssociated)
    //    callAssociated.foreach(println)

    //第五步，查询哪些方法调用了第四步查询出来的方法。
    AsmServiceHelper.getCalledMethodSecond(classIsServiceFile, callAssociated)
    println("第五步 size: " + callAssociated.size)

    //第六步，查询哪些方法调用了第四步查询出来的方法。
    AsmServiceHelper.getCalledMethodSecond(classIsServiceFile, callAssociated)
    println("第六步 size: " + callAssociated.size)

    //第七步，查询哪些方法调用了第四步查询出来的方法。
    AsmServiceHelper.getCalledMethodSecond(classIsServiceFile, callAssociated)
    println("第七步 size: " + callAssociated.size)


    //第八步，查询哪些方法调用了第四步查询出来的方法。
    AsmServiceHelper.getCalledMethodSecond(classIsServiceFile, callAssociated)

    println((System.currentTimeMillis() - begin))

    /*callAssociated.foreach(x => {
      println(s"key: ${x._1}   value: ${x._2}")
    })*/
    println("第九步")
    //第九步 format
    AsmServiceHelper.formatMap(callAssociated, callMap)
    callMap.size

    //第十步1，找到所有的 Service 和 Client 类，存到 Map 中
    AsmServiceHelper.getServiceImpl(classIsServiceFile, services, servicesImpl)
    //第十步2，查找这些service和client的方法
    AsmServiceHelper.getServiceImplMethod(classIsServiceFile, servicesImpl, methodsImpl)
    println(methodsImpl)


  }

 /*


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
*/
}


