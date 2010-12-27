/**
 * Copyright (c) 2007-2010 Eric Torreborre <etorreborre@yahoo.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of
 * the Software. Neither the name of specs nor the names of its contributors may be used to endorse or promote
 * products derived from this software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package org.specs.runner
import org.specs.io._
import org.specs.util._
import org.specs.log._
import org.specs._
import scala.xml.{Elem, PrettyPrinter, NodeSeq}
import org.specs.specification._
import org.specs.util.ExtendedThrowable._
import scala.xml.{Elem, PrettyPrinter}
import org.specs.execute._
import ExtendedThrowable._

/**
 * Concrete class for the SurefireXml trait. It allows to select a specification to run and an output path
 * Usage: <code>object runner extends SurefireXmlRunner("./results/specs", mySpec)</code>
 *
 * The name of the generated file is specification.name by default but can be overriden:<pre>
 * object runner extends SurefireXmlRunner(mySpec, "./results/specs"){ override def fileName(s: Specification)="spec-report.xml" }</pre>
 */
case class SurefireXmlRunner(val specs: Seq[Specification], outputDirPath: String, fName: BaseSpecification => String) extends
  FileReporter(outputDirPath, fName) with Xml {
  
  /** 
   * Alternative constructor with no specific specifications and default values.
   * This runner can report specifications using the report method.
   * This capacity is used in conjunction with the Runner class:<pre>
   * object runner extends Runner(spec1 :: spec2, HtmlRunner() :: XmlRunner())
   * </pre>
   */
  def this() = this(Nil, "./target", XmlNamingFunction.default)
  
  /** alternate constructor with the specification only. The output dir is the current directory */
  def this(specifications: Specification*) = this(specifications, "./target", XmlNamingFunction.default)

  /** alternate constructor with one specification only. The output dir is the current directory */
  def this(spec: Specification, outputDirPath: String) = this(List(spec), outputDirPath, XmlNamingFunction.default)
  
  /** alternate constructor with one specification only. */
  def this(spec: Specification, outputDirPath: String, fName: BaseSpecification => String) = this(List(spec), outputDirPath, fName)

  /** definition of the file name of a specification. */
  override def fileName(spec: BaseSpecification): String = fName(spec) 

  /** definition of the output directory of the report. */
  override def outputDir = normalize(outputDirPath)
}
/**
 * The SurefireXmlSuite is the same class as SurefireXmlRunner but can be extended with the JUnit trait.
 */
case class SurefireXmlSuite(val specs: Seq[Specification], outputDirPath: String, fName: BaseSpecification => String) extends
  FileSuite(outputDirPath, fName) with SurefireXml {

  /**
   * Alternative constructor with no specific specifications and default values.
   * This runner can report specifications using the report method.
   * This capacity is used in conjunction with the Runner class:<pre>
   * object runner extends Runner(spec1 :: spec2, HtmlSuite() :: SurefireXmlSuite())
   * </pre>
   */
  def this() = this(Nil, "./target", XmlNamingFunction.default)

  /** alternate constructor with the specification only. The output dir is the current directory */
  def this(specifications: Specification*) = this(specifications, ".", XmlNamingFunction.default)

  /** alternate constructor with one specification only. The output dir is the current directory */
  def this(spec: Specification, outputDirPath: String) = this(List(spec), outputDirPath, XmlNamingFunction.default)

  /** alternate constructor with one specification only. */
  def this(spec: Specification, outputDirPath: String, fName: BaseSpecification => String) = this(List(spec), outputDirPath, fName)

  /** definition of the file name of a specification. */
  override def fileName(spec: BaseSpecification): String = fName(spec)

  /** definition of the output directory of the report. */
  override def outputDir = normalize(outputDirPath)
}
/**
 * The <code>SurefireXml</code> trait is used to create an Maven Surefire compatible xml file, in a specified output directory
 * with the results of a specification execution.
 *
 * If the output directory is not specified <pre>object runner extends SurefireXmlRunner(mySpec)</pre> then the
 * current directory will be used
 *
 * Usage:<code>
 * class mySpecRunner extends Runner(mySpec) with SurefireXml
 * <code>
 *
 * The output directory can be overriden if necessary:<pre>
 * class mySpecRunner extends Runner(mySpec) with SurefireXml { override def outputDir = "./results/specs" }</pre>
 */
trait SurefireXml extends File {
  /** definition of the file name of a specification. */
  override def fileName(spec: BaseSpecification): String = XmlNamingFunction.default(spec)

  /** definition of the output directory of the report. */
  override def outputDir = "./target"

  /** definition of xml output. */
  def specOutput(spec: Specification): String = new PrettyPrinter(200, 2).format(asXml(spec))

  /**
   * @returns the specification results translated as to xml (including subspecifications)
   */
  def asXml(s: Specification): Elem = {
    val numTests = if (planOnly()) 0 else s.examples.length
    val numErrors = if (planOnly()) 0 else s.examples.filter(_.failureAndErrors.nonEmpty).size
    <testsuite name={s.name}
               description={s.description}
               tests={numTests.toString}
               errors={numErrors.toString}>
      <properties/>
      {s.examples map (asXml(s, (_: Example)))}
    </testsuite>
  }

  /**
   * @returns the example results translated as to xml (including sub-examples)
   */
  def asXml(s: Specification, e: Example): Elem = {
    <testcase classname={s.getClass.getName} name={e.description}>{
      e.failureAndErrors match {
        case Nil =>
        case Seq(e: SkippedException) => <skipped/>
        case es => <failure>{es.map(_.printStackTraceToString).mkString("\n\n")}</failure>
      }
    }
    </testcase>
  }
//
//  /**
//   * @returns an error translated as to xml
//   */
//  def asXml(error: Throwable): Elem = <error location={error.location}>{error.getMessage}</error>
//
//  /**
//   * @returns a failure translated as to xml
//   */
//  def asXml(failure: FailureException): Elem = <failure location={failure.location}>{failure.message}</failure>
//
//  /**
//   * @returns a skipped example translated as to xml
//   */
//  def asXml(skipped: SkippedException): Elem = <skipped location={skipped.location}>{skipped.message}</skipped>
}
