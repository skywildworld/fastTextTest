import java.io._
import scala.language.reflectiveCalls

object Util {
  def using[A <: {def close(): Unit }, B](resource: A)(f: A => B) : B =
    try {
      f(resource)
    } finally {
      resource.close()
    }
}


object Test {
  def main(args: Array[String]) : Unit = {
    var out : Option[DataOutputStream] = None

    try {
      out = Some(new DataOutputStream(new FileOutputStream("abc.txt")))
      val intV = 100
      val doubleV = 11.0
      val stringV = "hello"
      val arrInt = Array[Int](100)
      val arrStr = Array[String]("hello")
      out.get.writeInt(intV)
      //out.get.writeDouble(doubleV)
      out.get.writeBytes(stringV)
    } catch {
      case e: Exception => e.printStackTrace
    } finally {
      if(out.isEmpty) out.get.close
    }
  }
}

