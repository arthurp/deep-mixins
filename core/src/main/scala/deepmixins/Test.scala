package deepmixins

trait Mixin {
  trait C {
    def f: String = "nope"
  }  
}

trait Mixin2 extends Mixin {
  trait C extends super.C {
    override def f: String = super.f + " Test2"
  }    
}

trait Mixin3 extends Mixin {
  trait C extends super.C {
    override def f: String = super.f + " Test3"
  }    
}

trait Mixin4 extends Mixin {
  trait C {
    def g: String = " Test4"
  }    
}

@deepmixin
object Test extends Mixin3 with Mixin4 {
  
  //class C extends super[Mixin].C with super[Mixin2].C
  def main(args: Array[String]) = {
    println((new C).f)
  }
}