/*
 * Copyright 2018-2019 ABSA Group Limited
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package za.co.absa.cobrix.cobol.parser.headerparsers

/**
  * This is a parser for fixed length record files.
  */
class RecordHeaderParserFixedLen(recordSize: Int,
                                 fileHeaderBytes: Int,
                                 fileFooterBytes: Int) extends Serializable with RecordHeaderParser {

  /** This type of files do not have record headers */
  override def getHeaderLength: Int = 0

  override def isHeaderDefinedInCopybook: Boolean = false

  /**
    * Given a raw values of a record header returns metadata sufficient to parse the record.
    *
    * @param header A record header as an array of bytes
    * @param offset An offset from the beginning of the underlying file
    * @param size   A size of the underlying file
    *
    * @return A parsed record metadata
    */
  override def getRecordMetadata(header: Array[Byte], offset: Long = 0L, size: Long = 0L): RecordMetadata = {
    if (fileHeaderBytes > 0 && offset == 0L) {
      RecordMetadata(fileHeaderBytes, isValid = false)
    } else if (size > 0L && fileFooterBytes > 0 && size - offset <= fileFooterBytes) {
      RecordMetadata((size - offset).toInt, isValid = false)
    } else if (size - offset >= recordSize) {
      RecordMetadata(recordSize, isValid = true)
    } else {
      RecordMetadata(-1, isValid = false)
    }
  }

}