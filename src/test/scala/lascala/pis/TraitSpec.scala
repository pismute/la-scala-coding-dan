package la.scala.pis

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import scala.annotation.tailrec
import util.control.Breaks._

class TraitSpec extends FunSpec with ShouldMatchers{
  describe("Lieariation") {
    it("is form pis book") {
      abstract class Animal { def cry():Unit }
      trait Furry extends Animal {
        abstract override def cry() { println("furry");super.cry()}}
      trait HasLegs extends Animal {
        abstract override  def cry() { println("hasLegs");super.cry()} }
      trait FourLegged extends HasLegs {
        abstract override  def cry() { println("fourLegged");super.cry()} }
      class Doggy extends Animal {
        override def cry() { println("wuff!");}}
      class DetailDoggy extends Doggy with Furry with FourLegged {
        override def cry() { println("detail");super.cry()}}

      // DetailDoggy -> FourLegged -> HasLegs -> Furry -> Doggy
      new DetailDoggy ().cry()
    }

    it("is from kingori") {
      abstract class Value { def value():Int  }
      class ValueImpl(vall:Int) extends Value {
        override def value():Int = { println("valueimpl");vall } }
      trait Add extends Value {
        abstract override def value():Int = { println("add");super.value() + 3} }
      trait Mult extends Value {
        abstract override def value():Int = { println("mult");super.value() *2} }

      val a = new ValueImpl(3) with Add with Mult

      println("--------------")
      //  ( 3 + 3 ) * 2  : Mult -> Add -> ValueImpl
      a.value

      println("--------------")
      class ConcreteImpl(vall:Int) extends ValueImpl(vall) with Add with Mult {
        override def value():Int = { println("concreteImpl");super.value() } }

      //  ( 3 + 3 ) * 2  : ConcreteIpml -> Mult -> Add -> ValueImpl
      new ConcreteImpl(3).value

    }
  }
}
