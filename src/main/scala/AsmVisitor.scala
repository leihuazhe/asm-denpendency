package asm

import org.objectweb.asm._
import org.objectweb.asm.Opcodes.ASM5

import scala.collection.mutable

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
    //    println(s"superName:  $superName")
    super.visit(version, access, name, signature, superName, interfaces)
    this.className = name
    var isServiceSubClass = false
    if (className == "com.today.service.goods.GoodsServiceImpl") {
      println(name)
    }
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

/**
  * called
  * 哪些方法里面调用了 client的方法
  */
private class CalledClassVisitor(val callClientMap: collection.mutable.Set[String], val callAssociated: collection.mutable.Map[String, mutable.Set[String]]) extends ClassVisitor(ASM5) {
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

  override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor = {
    cv match {
      case null => new CalledMethodVisitor(this, name, signature, callClientMap, callAssociated)
      case _ => cv.visitMethod(access, name, desc, signature, exceptions)
    }
  }

}

/**
  * service method visitor
  * 改变方法里的逻辑，获取方法内部的逻辑
  */
private class CalledMethodVisitor(var clsVisitor: CalledClassVisitor, var name: String, var signature: String, val callClientSet: collection.mutable.Set[String], val callAssociated: collection.mutable.Map[String, mutable.Set[String]]) extends MethodVisitor(ASM5) {

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
    /*
    owner:com/today/commons/GoodsUtil$, name:categoryServiceClient, desc:()Lcom/today/api/category/scala/CategoryServiceClient;
    className: com/today/service/goods/query/ListSkuByConditionsQuery , name: categoryServiceClient  , signature null   method: categoryServiceClient
     */

    callClientSet.foreach(cc => {
      val callList = cc.split("\\.").toList
      //owner
      if (callList(1).equals(owner)) {
        //        System.out.println(s"<-------> owner: ${owner} , name: ${name} , desc: ${desc}, methodName: ${this.name}, className: ${clsVisitor.className}")
        //形式如下 com/today/service/goods/query/ListSkuByConditionsQuery.action
        val mapKey = s"${clsVisitor.className}.${this.name}"
        val value = s"${owner}.${name}"
        callAssociated.get(mapKey) match {
          case Some(x) => x += value
          case None => callAssociated += (mapKey -> mutable.Set(value))
        }
      }

    })
  }
}


/**
  * called second classVisitor
  * 哪些方法里面调用了 client的方法
  */
private class CalledSecondClassVisitor(val callAssociated: collection.mutable.Map[String, mutable.Set[String]]) extends ClassVisitor(ASM5) {
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

  override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor = {
    cv match {
      case null => new CalledSecondMethodVisitor(this, name, signature, callAssociated)
      case _ => cv.visitMethod(access, name, desc, signature, exceptions)
    }
  }

}

/**
  * service method visitor
  * 改变方法里的逻辑，获取方法内部的逻辑
  */
private class CalledSecondMethodVisitor(var clsVisitor: CalledSecondClassVisitor, var name: String, var signature: String, val callAssociated: collection.mutable.Map[String, mutable.Set[String]]) extends MethodVisitor(ASM5) {

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
    val ownerKey = s"${owner}.${name}"
    if (callAssociated.contains(ownerKey)) {

      val mapKey = s"${clsVisitor.className}.${this.name}"
      val value = ownerKey
      callAssociated.get(mapKey) match {
        case Some(x) => x += value
        case None => callAssociated += (mapKey -> mutable.Set(value))
      }
      //      System.out.println(s" owner: ${owner} , name: ${name} , desc: ${desc}, methodName: ${this.name}, className: ${clsVisitor.className}")
    }

  }
}


/**
  * 得到实现类，和实现方法
  *
  * @param services
  */
private class ImplClassVisitor(val services: collection.mutable.Map[String, String], val methods: collection.mutable.Map[String, collection.mutable.Set[String]]) extends ClassVisitor(ASM5) {
  var isService = false
  var className: String = _

  override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]): Unit = {


    super.visit(version, access, name, signature, superName, interfaces)
    this.className = name
    //   com/today/api/goods/scala/service/GoodsAdminService

    if (className == "com/today/service/goods/GoodsServiceImpl") {
      println("interfaces" + interfaces + "className" + className + "superName" + superName)
    }
    methods.foreach(method => {
      if (method._1.contains(interfaces)) {
        println("xiao pang")
      }

    })

  }

  /**
    * @param name service 方法名称
    * @param desc service 方法上的参数
    */
  override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor = {
    if (cv != null) return cv.visitMethod(access, name, desc, signature, exceptions)


    //    println(s"service's method: name: $name, desc: $desc,signature $signature exceptions $exceptions ")
    //    println(s"【第一步】：服务类： ${this.name}    方法名称: name: $name")
    new ImplMethodVisitor(this, name, signature)
  }
}

/**
  * service method visitor
  * 改变方法里的逻辑，获取方法内部的逻辑
  */
private class ImplMethodVisitor(var clsVisitor: ImplClassVisitor, var name: String, var signature: String) extends MethodVisitor(ASM5) {

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
private class ServiceImplClassVisitor(val servicesApi: collection.mutable.Set[String], val servicesImpl: collection.mutable.Set[String]) extends ClassVisitor(ASM5) {
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
    //    println(s"superName:  $superName")
    super.visit(version, access, name, signature, superName, interfaces)
    this.className = name
    var isServiceSubClass = false
    if (className == "com.today.service.goods.GoodsServiceImpl") {
      println(name)
    }
    servicesApi.foreach(key => {
      interfaces.foreach(interface => {
        if (interface.equals(key)) {
          isServiceSubClass = true
          servicesImpl += name
        }
      })
    })

  }
}

/**
  * 查找出实现类的 方法 调用 的 action
  */
private class ServiceActionClassVisitor(val serviceAction: mutable.Map[String, String], val methodsImpl: collection.mutable.Map[String, collection.mutable.Set[String]]) extends ClassVisitor(ASM5) {
  var className: String = _


  override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]): Unit = {
    super.visit(version, access, name, signature, superName, interfaces)
    this.className = name
  }

  override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor = {
    cv match {
      case null => new ServiceActionMethodVisitor(this, name, signature, serviceAction, methodsImpl)
      case _ => cv.visitMethod(access, name, desc, signature, exceptions)
    }
  }

}

/**
  * service method visitor
  * 改变方法里的逻辑，获取方法内部的逻辑
  */
private class ServiceActionMethodVisitor(var clsVisitor: ServiceActionClassVisitor, var name: String, var signature: String, val serviceAction: mutable.Map[String, String], val methodsImpl: collection.mutable.Map[String, collection.mutable.Set[String]]) extends MethodVisitor(ASM5) {

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

    methodsImpl.foreach(method => {
      println("what are you doing ?")
    })


    /*callClientSet.foreach(cc => {
      val callList = cc.split("\\.").toList
      //owner
      if (callList(1).equals(owner)) {
        //        System.out.println(s"<-------> owner: ${owner} , name: ${name} , desc: ${desc}, methodName: ${this.name}, className: ${clsVisitor.className}")
        //形式如下 com/today/service/goods/query/ListSkuByConditionsQuery.action
        val mapKey = s"${clsVisitor.className}.${this.name}"
        val value = s"${owner}.${name}"
        callAssociated.get(mapKey) match {
          case Some(x) => x += value
          case None => callAssociated += (mapKey -> mutable.Set(value))
        }
      }

    })*/
  }
}