package parser

/**
 * Created by marpiec on 1/9/15.
 */
class InputIterator(input: String) {
   val inputLength = input.length
   var index = 0
   def next = {
     val ch = input.charAt(index)
     index += 1
     ch
   }
   def hasNext = index < inputLength
 }
