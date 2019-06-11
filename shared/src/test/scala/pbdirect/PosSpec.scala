package pbdirect

import org.scalatest.{ Matchers, WordSpecLike }

class PosSpec extends WordSpecLike with Matchers {

  import Pos._

  val pos = List(
    new _1{}, new _2{}, new _3{}, new _4{}, new _5{}, new _6{}, new _7{}, new _8{}, new _9{}, new _10{},
    new _20{}, new _19{}, new _18{}, new _17{}, new _16{}, new _15{}, new _14{}, new _13{}, new _12{}, new _11{},
    new _21{}, new _22{}, new _23{}, new _24{}, new _25{}, new _26{}, new _27{}, new _28{}, new _29{}, new _30{},
    new _40{}, new _39{}, new _38{}, new _37{}, new _36{}, new _35{}, new _34{}, new _33{}, new _32{}, new _31{},
    new _41{}, new _42{}, new _43{}, new _44{}, new _45{}, new _46{}, new _47{}, new _48{}, new _49{}, new _50{},
    new _60{}, new _59{}, new _58{}, new _57{}, new _56{}, new _55{}, new _54{}, new _53{}, new _52{}, new _51{},
    new _61{}, new _62{}, new _63{}, new _64{}, new _65{}, new _66{}, new _67{}, new _68{}, new _69{}, new _70{},
    new _80{}, new _79{}, new _78{}, new _77{}, new _76{}, new _75{}, new _74{}, new _73{}, new _72{}, new _71{},
    new _81{}, new _82{}, new _83{}, new _84{}, new _85{}, new _86{}, new _87{}, new _88{}, new _89{}, new _90{},
    new _99{}, new _98{}, new _97{}, new _96{}, new _95{}, new _94{}, new _93{}, new _92{}, new _91{}, new _0{}
  )

  "Pos" should {
    "be sortable" in {
      pos.sorted.map(_._pos) shouldBe (0 to 99)
    }
  }

}
