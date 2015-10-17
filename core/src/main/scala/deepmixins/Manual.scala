package deepmixins

/**
 * @author amp
 */
object Manual extends App {
  trait Mixin {
    def f = C().f
    
    // Define the abstract constructor and type for C and the base 
    // class of "C".
    def C(): C
    type C <: CImpl
    trait CImpl {
      def f: String = "B"
    }
  }

  trait Mixin2 extends Mixin {
    // In each mixin we refine the bound on C to force subclasses to 
    // include this mixins version of CImpl.
    type C <: CImpl
    trait CImpl extends super.CImpl {
      override def f: String = super.f + " 2"
    }
  }

  trait Mixin3 extends Mixin {
    type C <: CImpl
    trait CImpl extends super.CImpl {
      override def f: String = super.f + " 3"
    }
  }

  class Done extends Mixin2 with Mixin3 {
    // Finally provide concrete instances of the members allowing super
    // classes to have the correct types in members and construct the
    // correct classes.
    type C = CImpl
    def C() = new CImpl
    class CImpl extends super[Mixin2].CImpl with super[Mixin3].CImpl
  }
  
  println((new Done).f)
  (new Done).C() : Done#C
}
/*
  trait Mixin {
    def f = C().f // C() is a magically generated constructor for C.
    trait C {
      def f: String = "B"
    }
  }

  trait Mixin2 extends Mixin {
    trait C extends super.C {
      override def f: String = super.f + " 2"
    }
  }

  trait Mixin3 extends Mixin {
    trait C extends super.C {
      override def f: String = super.f + " 3"
    }
  }

  class Done extends Mixin2 with Mixin3
  
  println((new Done).f)
  
  (new Done).C() : Done#C
*/