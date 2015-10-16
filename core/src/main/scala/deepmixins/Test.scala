package deepmixins

trait Mixin {
  trait C {
    def f: String
  }  
}

trait Mixin2 {
  trait C {
    def f: String = "Test"
  }    
}

@deepmixin
object Test extends Mixin with Mixin2 {
  
  //class C extends super[Mixin].C with super[Mixin2].C
  def main(args: Array[String]) = {
    println((new C).f)
  }
}