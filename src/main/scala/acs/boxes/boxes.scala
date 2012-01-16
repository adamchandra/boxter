package acs.boxes

object Commons {
  def head[A](a:Seq[A]) = a.head
  def head[A](a:List[A]) = a.head
  def tail[A](a:Seq[A]) = a.tail
  def tail[A](a:List[A]) = a.tail

  def csv = (s:String) => (s.split(",") map (_.trim)).toList
  def wsv = (s:String) => (s.split(" ") map (_.trim)).toList
}

import Commons._

object Boxes {
  import scalaz._
  import Scalaz._

  // -- | The basic data type.  A box has a specified size and some sort of
  // --   contents.
  // data Box = Box { rows    :: Int
  //                , cols    :: Int
  //                , content :: Content
  //                }
  //   deriving (Show)

  case class Box(rows:Int, cols:Int, content: Content) {
    // -- | Paste two boxes together horizontally, using a default (top)
    // --   alignment.
    // (<>) :: Box -> Box -> Box
    // l <> r = hcat top [l,r]
    def <> : Box => Box = beside
    def beside : Box => Box = 
      r => hcat(top) (List(this,r))

    // -- | Paste two boxes together horizontally with a single intervening
    // --   column of space, using a default (top) alignment.
    // (<+>) :: Box -> Box -> Box
    // l <+> r = hcat top [l, emptyBox 0 1, r]
    def <+> : Box => Box = besideS
    def besideS: Box => Box = 
      r => hcat(top)(List(this, emptyBox(0)(1), r))

    // -- | Paste two boxes together vertically, using a default (left)
    // --   alignment.
    // (\/\/) :: Box -> Box -> Box
    // t \/\/ b = vcat left [t,b]
    def <-> : Box => Box = atop
    def atop(b: Box): Box = 
      vcat(left)(List(this,b))


    // -- | Paste two boxes together vertically with a single intervening row
    // --   of space, using a default (left) alignment.
    // (/+/) :: Box -> Box -> Box
    // t /+/ b = vcat left [t, emptyBox 1 0, b]
    def <=> : Box => Box = atopS
    def atopS : Box => Box = 
      b => vcat(left)(List(this,emptyBox(1)(0), b))
  }

  object Box {
    val rows : Lens[Box, Int] = Lens(_.rows, (obj, v) => obj copy (rows = v))
    val cols : Lens[Box, Int] = Lens(_.cols, (obj, v) => obj copy (cols = v))
    val content : Lens[Box, Content] = Lens(_.content, (obj, v) => obj copy (content = v))
  }

  // -- | Convenient ability to use bare string literals as boxes.
  // instance IsString Box where
  //   fromString = text
  implicit def str2box(s:String): Box = text(s)

  // -- | Data type for specifying the alignment of boxes.
  // data Alignment = AlignFirst    -- ^ Align at the top/left.
  //                | AlignCenter1  -- ^ Centered, biased to the top/left.
  //                | AlignCenter2  -- ^ Centered, biased to the bottom/right.
  //                | AlignLast     -- ^ Align at the bottom/right.
  //   deriving (Eq, Read, Show)
  
  sealed trait Alignment

  case object AlignFirst extends Alignment
  case object AlignLast extends Alignment
  case object AlignCenter1 extends Alignment
  case object AlignCenter2 extends Alignment

  // -- | Align boxes along their tops.
  // top :: Alignment
  // top        = AlignFirst
  def top = AlignFirst

  // -- | Align boxes along their bottoms.
  // bottom :: Alignment
  // bottom     = AlignLast
  def bottom = AlignLast

  // -- | Align boxes to the left.
  // left :: Alignment
  // left       = AlignFirst
  def left = AlignFirst

  // -- | Align boxes to the right.
  // right :: Alignment
  // right      = AlignLast
  def right = AlignLast

  // -- | Align boxes centered, but biased to the left/top in case of
  // --   unequal parities.
  // center1 :: Alignment
  // center1    = AlignCenter1
  def center1    = AlignCenter1

  // -- | Align boxes centered, but biased to the right/bottom in case of
  // --   unequal parities.
  // center2 :: Alignment
  // center2    = AlignCenter2
  def center2    = AlignCenter2

  // -- | Contents of a box.
  // data Content = Blank        -- ^ No content.
  //              | Text String  -- ^ A raw string.
  //              | Row [Box]    -- ^ A row of sub-boxes.
  //              | Col [Box]    -- ^ A column of sub-boxes.
  //              | SubBox Alignment Alignment Box
  //                             -- ^ A sub-box with a specified alignment.
  //   deriving (Show)
  sealed trait Content

  case object Blank extends Content
  case class Text(s:String) extends Content
  case class Row(bs:List[Box]) extends Content
  case class Col(bs:List[Box]) extends Content
  case class SubBox(a1: Alignment, a2: Alignment, b:Box) extends Content


  // -- | The null box, which has no content and no size.  It is quite
  // --   useless.
  // nullBox :: Box
  // nullBox = emptyBox 0 0
  def nullBox = emptyBox(0)(0)

  // -- | @emptyBox r c@ is an empty box with @r@ rows and @c@ columns.
  // --   Useful for effecting more fine-grained positioning of other
  // --   boxes, by inserting empty boxes of the desired size in between
  // --   them.
  // emptyBox :: Int -> Int -> Box
  // emptyBox r c = Box r c Blank
  def emptyBox: Int => Int => Box = 
    r => c => Box(r, c, Blank)

  // -- | A @1x1@ box containing a single character.
  // char :: Char -> Box
  // char c = Box 1 1 (Text [c])
  def char: Char => Box =
    c => Box(1, 1, Text(c.toString))

  // -- | A (@1 x len@) box containing a string of length @len@.
  // text :: String -> Box
  // text t = Box 1 (length t) (Text t)
  def text: String => Box =
    s => Box(1, s.length, Text(s))


  // -- | Glue a list of boxes together horizontally, with the given alignment.
  // hcat :: Alignment -> [Box] -> Box
  // hcat a bs = Box h w (Row $ map (alignVert a h) bs)
  //   where h = maximum . (0:) . map rows $ bs
  //         w = sum . map cols $ bs
  def hcat: Alignment => List[Box] => Box =
    a => bs => {
      def h = (0 :: (bs ∘ (_.rows))) max
      def w = (bs ∘ (_.cols)) sum
      val aligned = alignVert(a)(h)
      Box(h, w, Row(bs ∘ aligned))
    }

  // -- | @hsep sep a bs@ lays out @bs@ horizontally with alignment @a@,
  // --   with @sep@ amount of space in between each.
  // hsep :: Int -> Alignment -> [Box] -> Box
  // hsep sep a bs = punctuateH a (emptyBox 0 sep) bs
  def hsep: Int => Alignment => List[Box] => Box =
    sep => a => bs => punctuateH(a)(emptyBox(0)(sep))(bs)


  // -- | Glue a list of boxes together vertically, with the given alignment.
  // vcat :: Alignment -> [Box] -> Box
  // vcat a bs = Box h w (Col $ map (alignHoriz a w) bs)
  //   where h = sum . map rows $ bs
  //         w = maximum . (0:) . map cols $ bs
  def vcat: Alignment => List[Box] => Box =
    a => bs => {
      def h = (bs ∘ (_.rows)).sum
      def w = (0 :: (bs ∘ (_.cols))) max
      val aligned = alignHoriz(a)(w)
      Box(h, w, Col(bs ∘ aligned))
    }


  // -- | @vsep sep a bs@ lays out @bs@ vertically with alignment @a@,
  // --   with @sep@ amount of space in between each.
  // vsep :: Int -> Alignment -> [Box] -> Box
  // vsep sep a bs = punctuateV a (emptyBox sep 0) bs
  def vsep: Int => Alignment => List[Box] => Box =
    sep => a => bs => punctuateV(a)(emptyBox(sep)(0))(bs)


  // -- | @punctuateH a p bs@ horizontally lays out the boxes @bs@ with a
  // --   copy of @p@ interspersed between each.
  // punctuateH :: Alignment -> Box -> [Box] -> Box
  // punctuateH a p bs = hcat a (intersperse p bs)
  def punctuateH: Alignment => Box => List[Box] => Box =
    a => p => bs => hcat(a)(bs intersperse p)

  // -- | A vertical version of 'punctuateH'.
  // punctuateV :: Alignment -> Box -> [Box] -> Box
  // punctuateV a p bs = vcat a (intersperse p bs)
  def punctuateV: Alignment => Box => List[Box] => Box =
    a => p => bs => vcat(a)(bs intersperse p)

  // --------------------------------------------------------------------------------
  // --  Paragraph flowing  ---------------------------------------------------------
  // --------------------------------------------------------------------------------

  // -- | @para algn w t@ is a box of width @w@, containing text @t@,
  // --   aligned according to @algn@, flowed to fit within the given
  // --   width.
  // para :: Alignment -> Int -> String -> Box
  // para a n t = (\ss -> mkParaBox a (length ss) ss) $ flow n t

  def para: Alignment => Int => String => Box = 
    a => n => t => 
      flow(n)(t) |> (ss => mkParaBox(a) (ss.length) (ss))
      // ((ss:List[String]) => mkParaBox(a) (ss.length) (ss)) (flow(n)(t))



  // -- | @columns w h t@ is a list of boxes, each of width @w@ and height
  // --   at most @h@, containing text @t@ flowed into as many columns as
  // --   necessary.
  // columns :: Alignment -> Int -> Int -> String -> [Box]
  // columns a w h t = map (mkParaBox a h) . chunk h $ flow w t
  def columns : (Alignment, Int, Int, String) => List[Box] = 
    (a, w, h, t) =>  flow(w)(t) ∘ (_.grouped(h).toList) ∘ (mkParaBox(a)(h))

  // -- | @mkParaBox a n s@ makes a box of height @n@ with the text @s@
  // --   aligned according to @a@.
  // mkParaBox :: Alignment -> Int -> [String] -> Box
  // mkParaBox a n = alignVert top n . vcat a . map text
  def mkParaBox : Alignment => Int => List[String] => Box = 
    a => n => alignVert(top)(n) compose vcat(a) compose (_.map(text))


  def words = wsv
  def unwords = (ws:List[String]) => ws.mkString(" ")


  // -- | Flow the given text into the given width.
  // flow :: Int -> String -> [String]
  // flow n t = map (take n)
  //          . getLines
  //          $ foldl' addWordP (emptyPara n) (map mkWord . words $ t)

  def flow2 : Int => String => List[String] = 
    n => t => {
      val wrds = words(t) ∘ mkWord
      val para = wrds.foldl (emptyPara(n)) (addWordP)
      para |> getLines |> (_.map(_.take(n)))
    }

  def flow : Int => String => List[String] = 
    n => t => {
      val wrds = words(t) ∘ mkWord
      val para = wrds.foldl (emptyPara(n)) { addWordP }
      para |> getLines |> (_.map(_.take(n)))
    }



  // data Para = Para { paraWidth   :: Int
  //                  , paraContent :: ParaContent
  //                  }
  // data ParaContent = Block { fullLines :: [Line]
  //                          , lastLine  :: Line
  //                          }
  sealed trait ParaContent

  case class Para(paraWidth : Int, paraContent : ParaContent)
  val paraWidth: Lens[Para, Int] = Lens(_.paraWidth, (obj, v) => obj copy (paraWidth = v))
  val paraContent: Lens[Para, ParaContent] = Lens(_.paraContent, (obj, v) => obj copy (paraContent = v))

  case class Block(fullLines : List[Line], lastLine  : Line) extends ParaContent
  val fullLines: Lens[Block, List[Line]] = Lens(_.fullLines, (obj, v) => obj copy (fullLines = v))
  val lastLine: Lens[Block, Line] = Lens(_.lastLine, (obj, v) => obj copy (lastLine = v))

  // emptyPara :: Int -> Para
  // emptyPara pw = Para pw (Block [] (Line 0 []))
  def emptyPara(pw: Int) : Para = 
    Para(pw, (Block(nil, (Line(0, nil)))))

  // getLines :: Para -> [String]
  // getLines (Para _ (Block ls l))
  //   | lLen l == 0 = process ls
  //   | otherwise   = process (l:ls)
  //   where process = map (unwords . reverse . map getWord . getWords) . reverse
  def getLines : Para => List[String] = 
    p => {
      def process =  (l:List[Line]) => l.reverse ∘ Line.getWords ∘ (_.map(Word.getWord)) ∘ (_.reverse) ∘ unwords

      p match {
        case Para(_, (Block(ls, l))) => 
          if (l.len == 0) process(ls)
          else            process(l::ls)
      }
    }

  // data Line = Line { lLen :: Int, getWords :: [Word] }
  case class Line(len: Int, words: List[Word])
  object Line {
    val lenL: Lens[Line, Int] = Lens(_.len, (obj, v) => obj copy (len = v))
    val wordsL: Lens[Line, List[Word]] = Lens(_.words, (obj, v) => obj copy (words = v))
    val getLen = lenL.apply _
    val getWords = wordsL.apply _
  }

  // mkLine :: [Word] -> Line
  // mkLine ws = Line (sum (map wLen ws) + length ws - 1) ws
  def mkLine : List[Word] => Line = 
    ws => Line((ws ∘ Word.getLen).sum + ws.length - 1, ws)

  // startLine :: Word -> Line
  // startLine = mkLine . (:[])
  def startLine : Word => Line = 
    w => mkLine(w :: Nil)

  // data Word = Word { wLen :: Int, getWord  :: String }
  case class Word(len:Int, word:String)
  object Word {
    val lenL: Lens[Word, Int] = Lens(_.len, (obj, v) => obj copy (len = v))
    val wordL: Lens[Word, String] = Lens(_.word, (obj, v) => obj copy (word = v))
    val getLen = lenL.apply _
    val getWord = wordL.apply _
  }

  // mkWord :: String -> Word
  // mkWord w = Word (length w) w
  def mkWord : String => Word = 
    w => Word(w.length, w)

  // addWordP :: Para -> Word -> Para
  // addWordP (Para pw (Block fl l)) w
  //   | wordFits pw w l = Para pw (Block fl (addWordL w l))
  //   | otherwise       = Para pw (Block (l:fl) (startLine w))
  def addWordP : (Para, Word) => Para = 
    (p, w) => {
      p match {
        case Para(pw, (Block(fl,l))) =>
          if (wordFits(pw,w,l))
            Para(pw, Block(fl, addWordL(w, l)))
          else
            Para(pw, Block((l::fl), startLine(w)))
      }
    }


  // addWordL :: Word -> Line -> Line
  // addWordL w (Line len ws) = Line (len + wLen w + 1) (w:ws)
  def addWordL : (Word, Line) => Line = 
    (w, l) => l match {
      case Line(len, ws) => Line((len + w.len + 1), (w::ws))
    }


  // wordFits :: Int -> Word -> Line -> Bool
  // wordFits pw w l = lLen l == 0 || lLen l + wLen w + 1 <= pw
  def wordFits : (Int, Word, Line) => Boolean = 
    (pw, w, l) => 
      l.len == 0 || l.len + w.len + 1 <= pw


  // --------------------------------------------------------------------------------
  // --  Alignment  -----------------------------------------------------------------
  // --------------------------------------------------------------------------------

  // -- | @alignHoriz algn n bx@ creates a box of width @n@, with the
  // --   contents and height of @bx@, horizontally aligned according to
  // --   @algn@.
  // alignHoriz :: Alignment -> Int -> Box -> Box
  // alignHoriz a c b = Box (rows b) c $ SubBox a AlignFirst b
  def alignHoriz: Alignment => Int => Box => Box = 
    a => c => b => {
      Box(b.rows, c, SubBox(a, AlignFirst, b))
    }

  // -- | @alignVert algn n bx@ creates a box of height @n@, with the
  // --   contents and width of @bx@, vertically aligned according to
  // --   @algn@.
  // alignVert :: Alignment -> Int -> Box -> Box
  // alignVert a r b = Box r (cols b) $ SubBox AlignFirst a b
  def alignVert: Alignment => Int => Box => Box = 
    a => r => b => 
      Box(r, (b.cols), SubBox(AlignFirst, a, b))


  // -- | @align ah av r c bx@ creates an @r@ x @c@ box with the contents
  // --   of @bx@, aligned horizontally according to @ah@ and vertically
  // --   according to @av@.
  // align : Alignment -> Alignment -> Int -> Int -> Box -> Box
  // align ah av r c = Box r c . SubBox ah av
  def align : (Alignment, Alignment, Int, Int, Box) => Box =
    (ah, av, r, c, bx) => Box(r, c, SubBox(ah, av, bx))

  // -- | Move a box \"up\" by putting it in a larger box with extra rows,
  // --   aligned to the top.  See the disclaimer for 'moveLeft'.
  // moveUp : Int -> Box -> Box
  // moveUp n b = alignVert top (rows b + n) b
  def moveUp : Int => Box => Box = 
    n => b => alignVert(top)(b.rows + n)(b)


  // -- | Move a box down by putting it in a larger box with extra rows,
  // --   aligned to the bottom.  See the disclaimer for 'moveLeft'.
  // moveDown : Int -> Box -> Box
  // moveDown n b = alignVert bottom (rows b + n) b
  def moveDown : Int => Box => Box = 
    n => b => alignVert(bottom)(b.rows + n)(b)

  // -- | Move a box left by putting it in a larger box with extra columns,
  // --   aligned left.  Note that the name of this function is
  // --   something of a white lie, as this will only result in the box
  // --   being moved left by the specified amount if it is already in a
  // --   larger right-aligned context.
  // moveLeft : Int -> Box -> Box
  // moveLeft n b = alignHoriz left (cols b + n) b
  def moveLeft : Int => Box => Box = 
    n => b => alignHoriz(left)(b.cols + n)(b)


  // -- | Move a box right by putting it in a larger box with extra
  // --   columns, aligned right.  See the disclaimer for 'moveLeft'.
  // moveRight : Int -> Box -> Box
  // moveRight n b = alignHoriz right (cols b + n) b
  def moveRight : Int => Box => Box = 
    n => b => alignHoriz(right)(b.cols + n)(b)


  // --------------------------------------------------------------------------------
  // --  Implementation  ------------------------------------------------------------
  // --------------------------------------------------------------------------------

  // -- | Render a 'Box' as a String, suitable for writing to the screen or
  // --   a file.
  // render : Box -> String
  // render = unlines . renderBox
  def render : Box => String = 
    b => renderBox(b) |> (_.mkString("\n")) 

  // -- XXX make QC properties for takeP

  // -- | \"Padded take\": @takeP a n xs@ is the same as @take n xs@, if @n
  // --   <= length xs@; otherwise it is @xs@ followed by enough copies of
  // --   @a@ to make the length equal to @n@.
  // takeP : a -> Int -> [a] -> [a]
  // takeP _ n _      | n <= 0 = []
  // takeP b n []              = replicate n b
  // takeP b n (x:xs)          = x : takeP b (n-1) xs


  def takeP[A] : A => Int => List[A] => List[A] = 
    a => n => aas => {
      val pad = if (n <= aas.length) 0 else n - aas.length
      // aas.take(n) ::: a.repeat[List].take(pad)
      aas.take(n) ::: a.replicate[List](pad)
    }

  // -- | @takePA @ is like 'takeP', but with alignment.  That is, we
  // --   imagine a copy of @xs@ extended infinitely on both sides with
  // --   copies of @a@, and a window of size @n@ placed so that @xs@ has
  // --   the specified alignment within the window; @takePA algn a n xs@
  // --   returns the contents of this window.
  // takePA : Alignment -> a -> Int -> [a] -> [a]
  // takePA c b n = glue . (takeP b (numRev c n) *** takeP b (numFwd c n)) . split
  //   where split t = first reverse . splitAt (numRev c (length t)) $ t
  //         glue    = uncurry (++) . first reverse
  //         numFwd AlignFirst    n = n
  //         numFwd AlignLast     _ = 0
  //         numFwd AlignCenter1  n = n `div` 2
  //         numFwd AlignCenter2  n = (n+1) `div` 2
  //         numRev AlignFirst    _ = 0
  //         numRev AlignLast     n = n
  //         numRev AlignCenter1  n = (n+1) `div` 2
  //         numRev AlignCenter2  n = n `div` 2

  def takePA[A] : Alignment => A => Int => List[A] => List[A] = 
    c => b => n => aas => {
      def numFwd(a:Alignment, n:Int): Int = a match {
        case AlignFirst    => n
        case AlignLast     => 0
        case AlignCenter1  => n / 2
        case AlignCenter2  => (n+1) / 2
      }

      def numRev(n:Int) = (_:Alignment) match {
        case AlignFirst    => 0
        case AlignLast     => n
        case AlignCenter1  => (n+1) / 2
        case AlignCenter2  => n / 2
      }

      def split: List[A] => (List[A], List[A]) = 
        as => ((_:List[A]) reverse).first apply as.splitAt(numRev(as.length)(c)) 

      def pad = (takeP(b)(numRev(n)(c)) *** takeP(b)(numFwd(c,n)));

      pad apply split(aas) fold (_ ++ _)
    }


  // -- | Generate a string of spaces.
  // blanks : Int -> String
  // blanks = flip replicate ' '
  def blanks : Int => String = 
    n => " " * n

  // -- | Render a box as a list of lines.
  // renderBox : Box -> [String]
  // renderBox (Box r c Blank)            = resizeBox r c [""]
  // renderBox (Box r c (Text t))         = resizeBox r c [t]
  // 
  // renderBox (Box r c (Col bs))         = resizeBox r c
  //                                        . concatMap (renderBoxWithCols c)
  //                                        $ bs
  //
  // renderBox (Box r c (SubBox ha va b)) = resizeBoxAligned r c ha va
  //                                        . renderBox
  //                                        $ b
  // renderBox (Box r c (Row bs))         = resizeBox r c
  //                                        . merge
  //                                        . map (renderBoxWithRows r)
  //                                        $ bs
  //                                      where merge = foldr (zipWith (++)) (repeat [])

  def renderBox : Box => List[String] = 
    box => box match {
      case Box(r, c, Blank)             => resizeBox(r, c, List("")) 
      case Box(r, c, Text(t))           => resizeBox(r, c, List(t))
      case Box(r, c, Col(bs))           => (bs >>= renderBoxWithCols(c)) |> (resizeBox(r, c, _))
      case Box(r, c, SubBox(ha, va, b)) => resizeBoxAligned(r, c, ha, va)(renderBox(b))
      case Box(r, c, Row(bs))           => {
        def merge: List[List[String]] => List[String] = 
          sss => sss.foldr("".repeat[Stream]) ({
            case (a, b) => (a.toStream zip b) map {case (x, y) => x+y}
          }).toList
        bs ∘ renderBoxWithRows(r) |> merge |> (resizeBox(r, c, _))
      }
    }


  // -- | Render a box as a list of lines, using a given number of rows.
  // renderBoxWithRows : Int => Box => [String]
    //r => b => renderBox (b{rows = r})
  def renderBoxWithRows : Int => Box => List[String] = 
    r => b => renderBox (Box.rows.set(b, r))

  // -- | Render a box as a list of lines, using a given number of columns.
  // renderBoxWithCols : Int => Box => [String]
  // renderBoxWithCols c b = renderBox (b{cols = c})
  def renderBoxWithCols : Int => Box => List[String] = 
    c => b => renderBox (Box.cols.set(b, c))

  // -- | Resize a rendered list of lines.
  // resizeBox : Int => Int => [String] => [String]
  // resizeBox r c = takeP (blanks c) r . map (takeP ' ' c)
  def resizeBox : (Int, Int, List[String]) => List[String] =
    (r, c, ss) => {
      val taker = takeP(" "*c)(r)
      val takec = ss map (s => (takeP(' ')(c)(s.toList)).mkString(""))
      taker(takec)
    }

  // -- | Resize a rendered list of lines, using given alignments.
  // resizeBoxAligned : Int => Int => Alignment => Alignment => [String] => [String]
  // resizeBoxAligned r c ha va = takePA va (blanks c) r . map (takePA ha ' ' c)
  def resizeBoxAligned : (Int, Int, Alignment, Alignment) => List[String] => List[String] = 
     (r, c, ha, va) => ss => 
       takePA(va)(blanks(c))(r){
         (ss.map (_.toList)) ∘ (takePA(ha)(' ')(c)) ∘ (_.mkString(""))
       }

  // -- | A convenience function for rendering a box to stdout.
  def printBox : Box => Unit = 
    box => println(render(box))

}
