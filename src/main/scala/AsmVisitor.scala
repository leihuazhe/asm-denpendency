package asm

import org.objectweb.asm._
import org.objectweb.asm.Opcodes.ASM5

/**
  * fetch 接口类 and Client
  *
  * @param services
  */
private class ServiceClassVisitor(val services: collection.mutable.Set[String]) extends ClassVisitor(ASM5) {
  var isService = false
  var className: String = _

  /**
    * @param desc
    * @param visible
    * @return
    */
  override def visitAnnotation(desc: String, visible: Boolean): AnnotationVisitor = {
    if (desc == "Lcom/github/dapeng/core/Service;") {
      services += className
      isService = true
      //      System.out.println("find  a service:    " + name)
    }
    super.visitAnnotation(desc, visible)
  }

  /**
    *
    * @param version
    * @param access
    * @param name
    * @param signature
    * @param superName
    * @param interfaces
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




    //    println(s"service's method: name: $name, desc: $desc,signature $signature exceptions $exceptions ")
    //    println(s"【第一步】：服务类： ${this.name}    方法名称: name: $name")
    new ServiceMethodVisitor(this, name, signature)
  }
}

/**
  * service method visitor
  * 改变方法里的逻辑，获取方法内部的逻辑
  */
private class ServiceMethodVisitor(var clsVisitor: ServiceClassVisitor, var name: String, var signature: String) extends MethodVisitor(ASM5) {

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
    //    if (name == "sayHello")
    //    System.out.println("<-------> owner:" + owner + ", name:" + name + ", desc:" + desc)

    //    if (owner == "com/github/dapeng/hello/JavaSyncHelloServiceClient")
    //    System.out.println(clsVisitor.name + ":" + name + ":" + signature + "-> " + "method:" + name)
    //    System.out.println(s"className: ${clsVisitor.name} , name: $name  , signature $signature   method: $name")
  }
}

/**
  * 找到实现了服务的类 ,例如 Client
  *
  * @param services
  * @param
  */
private class ClientClassVisitor(val services: collection.mutable.Set[String]) extends ClassVisitor(ASM5) {
  var isService = false
  var name: String = _

  /**
    *
    * @param version
    * @param access
    * @param name
    * @param signature
    * @param superName
    * @param interfaces
    */
  override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]): Unit = {
    //    println(s"superName:  $superName")
    super.visit(version, access, name, signature, superName, interfaces)
    this.name = name
    var isServiceSubClass = false

    services.foreach(key => {
      interfaces.foreach(interface => {
        if (interface.equals(key)) {
          isServiceSubClass = true
          services += name
        }
      })
    })

  }
}


private class MethodClassVisitor(val services: collection.mutable.Set[String], val methods: collection.mutable.Map[String, collection.mutable.Set[String]]) extends ClassVisitor(ASM5) {
  var isService = false
  var className: String = _

  /**
    *
    * val methods: collection.mutable.Map[String, ListBuffer[String]]
    *
    * @param version
    * @param access
    * @param name
    * @param signature
    * @param superName
    * @param interfaces
    */
  override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]): Unit = {
    super.visit(version, access, name, signature, superName, interfaces)
    this.className = name

  }


  override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor = {

    if (services.contains(className)) {
      //
      methods.get(className) match {

        case Some(x) => x += name
        case None => methods += (className -> collection.mutable.Set(name))
      }

    }

    super.visitMethod(access, name, desc, signature, exceptions)
  }
}


/**
  * 找到有client fieled 字段 的类
  *
  * @param callClientMap
  */
private class FieldsClassVisitor(val callClientMap: collection.mutable.Set[String]) extends ClassVisitor(ASM5) {
  var className: String = _

  /**
    *
    * @param version
    * @param access
    * @param name
    * @param signature
    * @param superName
    * @param interfaces
    */
  override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]): Unit = {
    super.visit(version, access, name, signature, superName, interfaces)
    this.className = name
  }

  override def visitField(access: Int, name: String, desc: String, signature: String, value: scala.Any): FieldVisitor = {
    val fieldName = desc.substring(1, desc.length - 1)
    //    println(s"$className  --->  $name ----->  ${fieldName}")
    if (fieldName.endsWith("Client")) {
      callClientMap += s"${className}.${fieldName}"
    }
    super.visitField(access, name, desc, signature, value)
  }

}


