package za.co.absa.cobrix.cobol.parser.extract

import org.scalatest.FunSuite
import za.co.absa.cobrix.cobol.parser.CopybookParser
import za.co.absa.cobrix.cobol.parser.ast.{BinaryProperties, Group, Primitive}
import za.co.absa.cobrix.cobol.parser.encoding.EBCDIC
import za.co.absa.cobrix.cobol.parser.ast.datatype.{AlphaNumeric, CobolType}

class BinaryExtractorSpec extends FunSuite {

  val copyBookContents: String =
    """       01  RECORD.
      |           05  ID                        PIC S9(4)  COMP.
      |           05  COMPANY.
      |               10  SHORT-NAME            PIC X(10).
      |               10  COMPANY-ID-NUM        PIC 9(5) COMP-3.
      |               10  COMPANY-ID-STR
      |			         REDEFINES  COMPANY-ID-NUM PIC X(3).
      |           05  METADATA.
      |               10  CLIENTID              PIC X(15).
      |               10  REGISTRATION-NUM      PIC X(10).
      |               10  NUMBER-OF-ACCTS       PIC 9(03) COMP-3.
      |               10  ACCOUNT.
      |                   12  ACCOUNT-DETAIL    OCCURS 80
      |                                         DEPENDING ON NUMBER-OF-ACCTS.
      |                      15  ACCOUNT-NUMBER     PIC X(24).
      |                      15  ACCOUNT-TYPE-N     PIC 9(5) COMP-3.
      |                      15  ACCOUNT-TYPE-X     REDEFINES
      |                           ACCOUNT-TYPE-N  PIC X(3).
      |
      |""".stripMargin

  //  Data sample:
  //  JSON:
  //
  //  {
  //    "ID": 6,
  //    "COMPANY": {
  //      "SHORT_NAME": "EXAMPLE4",
  //      "COMPANY_ID_NUM": 0,
  //      "COMPANY_ID_STR": ""
  //    },
  //    "METADATA": {
  //      "CLIENTID": "",
  //      "REGISTRATION_NUM": "",
  //      "NUMBER_OF_ACCTS": 3,
  //      "ACCOUNT": {
  //      "ACCOUNT_DETAIL": [
  //    {
  //      "ACCOUNT_NUMBER": "000000000000002000400012",
  //      "ACCOUNT_TYPE_N": 0,
  //      "ACCOUNT_TYPE_X": ""
  //    },
  //    {
  //      "ACCOUNT_NUMBER": "000000000000003000400102",
  //      "ACCOUNT_TYPE_N": 1,
  //      "ACCOUNT_TYPE_X": ""
  //    },
  //    {
  //      "ACCOUNT_NUMBER": "000000005006001200301000",
  //      "ACCOUNT_TYPE_N": 2,
  //      "ACCOUNT_TYPE_X": ""
  //    }
  //      ]
  //    }
  //    }
  //  }
  //
  //  Binary:
  //  00 06 C5 E7 C1 D4 D7 D3 C5 F4 40 40 00 00
  //  0F 40 40 40 40 40 40 40 40 40 40 40 40 40 40 40
  //  40 40 40 40 40 40 40 40 40 40 00 3F F0 F0 F0 F0
  //  F0 F0 F0 F0 F0 F0 F0 F0 F0 F0 F2 F0 F0 F0 F4 F0
  //  F0 F0 F1 F2 00 00 0F F0 F0 F0 F0 F0 F0 F0 F0 F0
  //  F0 F0 F0 F0 F0 F3 F0 F0 F0 F4 F0 F0 F1 F0 F2 00
  //  00 1F F0 F0 F0 F0 F0 F0 F0 F0 F5 F0 F0 F6 F0 F0
  //  F1 F2 F0 F0 F3 F0 F1 F0 F0 F0 00 00 2F

   val bytes: Array[Byte]  = Array[Byte](
     0x00.toByte, 0x06.toByte, 0xC5.toByte, 0xE7.toByte, 0xC1.toByte, 0xD4.toByte, 0xD7.toByte, 0xD3.toByte,
     0xC5.toByte, 0xF4.toByte, 0x40.toByte, 0x40.toByte, 0x00.toByte, 0x00.toByte, 0x0F.toByte, 0x40.toByte,
     0x40.toByte, 0x40.toByte, 0x40.toByte, 0x40.toByte, 0x40.toByte, 0x40.toByte, 0x40.toByte, 0x40.toByte,
     0x40.toByte, 0x40.toByte, 0x40.toByte, 0x40.toByte, 0x40.toByte, 0x40.toByte, 0x40.toByte, 0x40.toByte,
     0x40.toByte, 0x40.toByte, 0x40.toByte, 0x40.toByte, 0x40.toByte, 0x40.toByte, 0x40.toByte, 0x40.toByte,
     0x00.toByte, 0x3F.toByte, 0xF0.toByte, 0xF0.toByte, 0xF0.toByte, 0xF0.toByte, 0xF0.toByte, 0xF0.toByte,
     0xF0.toByte, 0xF0.toByte, 0xF0.toByte, 0xF0.toByte, 0xF0.toByte, 0xF0.toByte, 0xF0.toByte, 0xF0.toByte,
     0xF2.toByte, 0xF0.toByte, 0xF0.toByte, 0xF0.toByte, 0xF4.toByte, 0xF0.toByte, 0xF0.toByte, 0xF0.toByte,
     0xF1.toByte, 0xF2.toByte, 0x00.toByte, 0x00.toByte, 0x0F.toByte, 0xF0.toByte, 0xF0.toByte, 0xF0.toByte,
     0xF0.toByte, 0xF0.toByte, 0xF0.toByte, 0xF0.toByte, 0xF0.toByte, 0xF0.toByte, 0xF0.toByte, 0xF0.toByte,
     0xF0.toByte, 0xF0.toByte, 0xF0.toByte, 0xF3.toByte, 0xF0.toByte, 0xF0.toByte, 0xF0.toByte, 0xF4.toByte,
     0xF0.toByte, 0xF0.toByte, 0xF1.toByte, 0xF0.toByte, 0xF2.toByte, 0x00.toByte, 0x00.toByte, 0x1F.toByte,
     0xF0.toByte, 0xF0.toByte, 0xF0.toByte, 0xF0.toByte, 0xF0.toByte, 0xF0.toByte, 0xF0.toByte, 0xF0.toByte,
     0xF5.toByte, 0xF0.toByte, 0xF0.toByte, 0xF6.toByte, 0xF0.toByte, 0xF0.toByte, 0xF1.toByte, 0xF2.toByte,
     0xF0.toByte, 0xF0.toByte, 0xF3.toByte, 0xF0.toByte, 0xF1.toByte, 0xF0.toByte, 0xF0.toByte, 0xF0.toByte,
     0x00.toByte, 0x00.toByte, 0x2F.toByte
   )

  val copybook = CopybookParser.parseTree(EBCDIC(), copyBookContents)
  val startOffset: Int = 0

  test("Test extract primitive field") {

    // using getFieldByName
    val statement = copybook.getFieldByName("ID")
    val field: Primitive = statement.asInstanceOf[Primitive]
    val result: Any = copybook.extractPrimitiveField(field, bytes, startOffset)
    assert(result.asInstanceOf[Int] === 6)

    // traverse AST and extract all primitives to map
    var extractedData = scala.collection.mutable.Map[String,Any]()
    def traverseAst(group: Group): Unit = {
      for (child <- group.children) {
        if (child.isInstanceOf[Primitive]) {
          extractedData += (child.name -> copybook.extractPrimitiveField(child.asInstanceOf[Primitive],
            bytes, startOffset))
        } else {
          assert(child.isInstanceOf[Group] === true)
          traverseAst(child.asInstanceOf[Group])
        }
      }
    }

    // check extracted values in the map
    traverseAst(copybook.ast.head)
    assert(extractedData("ID").asInstanceOf[Int] === 6)
    assert(extractedData("SHORT_NAME").asInstanceOf[String] === "EXAMPLE4")
    assert(extractedData("NUMBER_OF_ACCTS").asInstanceOf[Int] === 3)
    assert(extractedData.size === 10)

    // Test custom Primitive
    val level: Int = 10
    val name: String = "SHORT-NAME"
    val lineNumber: Int = 4
    val dataType: CobolType = AlphaNumeric(10, None, Some(EBCDIC()))
    val redefines: Option[String] = None
    val isRedefined: Boolean = false
    val occurs: Option[Int] = None
    val to: Option[Int] = None
    val dependingOn: Option[String] = None
    val isDependee: Boolean = false
    val binaryProperties: BinaryProperties = BinaryProperties(2*8, 10*8, 10*8)

    val primitive: Primitive = new Primitive(level, name, lineNumber, dataType, redefines, isRedefined,
      occurs, to, dependingOn, isDependee, binaryProperties)(None)
    val result2: Any = copybook.extractPrimitiveField(primitive, bytes, startOffset)
    assert(result2.asInstanceOf[String] === "EXAMPLE4")
  }

  test("Test get field value by name"){
    val fieldName0: String = "ID"
    val result0: Any = copybook.getFieldValueByName(fieldName0, bytes, startOffset)
    assert(result0.asInstanceOf[Int] === 6)

    val fieldName1: String = "COMPANY.SHORT-NAME"
    val result1: Any = copybook.getFieldValueByName(fieldName1, bytes, startOffset)
    assert(result1.asInstanceOf[String] === "EXAMPLE4")

    val fieldName2: String = "METADATA.NUMBER-OF-ACCTS"
    val result2: Any = copybook.getFieldValueByName(fieldName2, bytes, startOffset)
    assert(result2.asInstanceOf[Int] === 3)

    val notExistingName: String = "NOT-EXISTING-FIELD"
    val thrown = intercept[IllegalStateException] {
      val resultImpossible: Any = copybook.getFieldValueByName(notExistingName, bytes, startOffset)
    }
    assert(thrown.getMessage.contains("not found in the copybook")
      === true)

    val emptyName: String = ""
    val thrown2 = intercept[IllegalStateException] {
      val resultImpossible2: Any = copybook.getFieldValueByName(emptyName, bytes, startOffset)
    }
    assert(thrown2.getMessage.contains("not found in the copybook")
      === true)

    val notPrimitiveName: String = "METADATA"
    val thrown3 = intercept[IllegalStateException] {
      val resultImpossible3: Any = copybook.getFieldValueByName(notPrimitiveName, bytes, startOffset)
    }
    assert(thrown3.getMessage.contains("is not a primitive field") === true)

    //  TODO
    // Currently, if a filed name corresponding to a Group Statement (instead of a Primitive) is passed,
    // 'Field not found in a copybook' exception is thrown. Should be 'Not a primitive field'

    //    val notPrimitiveName2: String = "METADATA.ACCOUNT"
    //    val thrown4 = intercept[IllegalStateException] {
    //      val resultImpossible4: Any = copybook.getFieldValueByName(notPrimitiveName2, bytes, startOffset)
    //    }
    //    assert(thrown4.getMessage === s"$notPrimitiveName2 is not a primitive field," +
    //      " cannot extract it's value.")

  }
}