import org.specs2.mutable._
import scala.xml._

class RfmParserSpec extends Specification {

  import RfmParser._

  val url = getClass.getResource("h.xml")
  val xml = XML.load(url)

  "RfmParser.parseXml" should {
    "parse example file" in {
      val output = Seq(
        Generation("0x36", Seq(
          Variant("0xFF", Seq(
            Revision("0x02", support = true),
            Revision("0x01", support = true),
            Revision("0x03", support = false),
            Revision("0x04", support = false),
            Revision("0x05", support = false)
          )),
          Variant("0xEE", Seq(
            Revision("0x00", support = false),
            Revision("0x04", support = false),
            Revision("0x02", support = false),
            Revision("0x03", support = false)
          ))
        ))
      )

      parseXml(xml) must beEqualTo(output)
    }
  }

  "RfmParser.parse" should {
    "parse example file" in {
      val output = "Gen 0x36 eq Var 0xFF eq E 0x01 ge E 0x02 le and and andg"

      parse(xml) must beEqualTo(output)
    }
  }

}
