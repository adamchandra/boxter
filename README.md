
Boxes: Now, in Scala.
--------------------

A port of the boxes library, written by Brent Yorgey under the attached copyright[1].

- Todo
  - Create utility functions to ease the process of using blocks of text with the boxes
    formatting library, i.e., convert a newline sep string into a seq[string] without '\n's, 
  
  - Integrate colorizing lib

  - Create 'border' functions

- Preliminary notes on translation of haskell to Scala: 

  - Record syntax approximation using case classes + lenses to provide accessor functions
    (Why not just use lifted methods, e.g. (_.getField)? Scala type inferencer woes, for one...)
    
  - When to preserve ordering of function composition in haskell (f . g . h), vs.
    scala's reversed ordering (f andThen g andThen h). Reversal helps scala's inference, 
    and can be as clear as regular composition. Problems arise when the two styles are freely
    mixed (especially in a single expression), such the order of function application is not
    strictly left to right or right to left.
    
    

~~~~ haskell
  data Para = Para { paraWidth   :: Int
                   , paraContent :: ParaContent
                   }
  data ParaContent = Block { fullLines :: [Line]
                           , lastLine  :: Line
                           }
~~~~~  

~~~~ scala
  sealed trait ParaContent

  case class Para(paraWidth : Int, paraContent : ParaContent)
  val paraWidth: Lens[Para, Int] = Lens(_.paraWidth, (obj, v) => obj copy (paraWidth = v))
  val paraContent: Lens[Para, ParaContent] = Lens(_.paraContent, (obj, v) => obj copy (paraContent = v))
~~~~~  




fn1.
    
    Copyright (c) Brent Yorgey 2008
    
    All rights reserved.
    
    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:
    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.
    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.
    3. Neither the name of the author nor the names of other contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.
    
    THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND
    ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
    ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
    FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
    OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
    SUCH DAMAGE.
    

The original boxes.hs file header is:

    -- Module      :  Text.PrettyPrint.Boxes
    -- Copyright   :  (c) Brent Yorgey 2009
    -- License     :  BSD-style (see LICENSE)
    -- Maintainer  :  byorgey@cis.upenn.edu
    -- Stability   :  experimental
    -- Portability :  portable
    --
    -- A pretty-printing library for laying out text in two dimensions,
    -- using a simple box model.

