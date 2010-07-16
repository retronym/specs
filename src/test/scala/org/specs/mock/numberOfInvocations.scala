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
package org.specs.mock
import org.specs._

object numberOfInvocations extends numberOfInvocations
class numberOfInvocations extends HtmlSpecification("Number of invocations") with MockitoSpecification {
<wiki>
{"""import org.specs.Specification
  import org.specs.mock.Mockito
  import org.mockito.Mock
  import java.util.List
  import java.util.LinkedList""" prelude it shh }
h3. Number of invocations

The number of invocations can be checked with different methods on the @called@ matcher: {"""

  class s4 extends Specification with Mockito {
    val m = mock[List[String]]

    m.add("one")

    1 to 2 foreach { i => m.add("two") } 
    1 to 3 foreach { i => m.add("three") } 
  } 
""" prelude it }

<ex>@there was one(mock).call()@ checks if the mock has been called only one time</ex>:

{ "new s4 { there was one(m).add(\"one\") }.isOk" snip it }
{ >("true") }

<ex>@there was two(mock).call()@ checks if the mock has been called twice</ex>:

{ "new s4 { there was two(m).add(\"two\") }.isOk" snip it }
{ >("true") }

<ex>It is also possible to check that a method was called at least a number of times</ex>:

{ "new s4 { there was atLeastOne(m).add(\"two\") }.isOk" snip it }
{ >("true") }

<ex>If the method wasn't called the expected number of times, there must be a @FailureException@</ex>:
  
{ "new s4 { there was two(m).add(\"one\") }.failures" snip it }
{ >("Wanted 2 times") }

<ex>@no(m).call()@ checks that the method wasn't called at all (never in Mockito)</ex>:
  
{ "new s4 { there was no(m).add(\"one\") }.failures" snip it }
{ >("Never wanted here") }

<ex>It is also possible to check that there are no unexpected calls on a mock</ex>:
  
{ """  new s4 { 
    there was one(m).add("one")
    there were noMoreCallsTo(m)
  }.failures.head""" snip it }
{ 
  execute(it) must (include("No interactions wanted here") and 
    include("But found this interaction:"))
}

</wiki> isSus
}
