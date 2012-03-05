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

  // The basic data type.  A box has a specified size and some sort of
  //   contents.
  case class Box(rows:Int, cols:Int, content: Content) {
    // Paste two boxes together horizontally, using a default (top) alignment.
    def + : Box => Box = beside
    def beside : Box => Box = 
      r => hcat(top) (List(this,r))

    // Paste two boxes together horizontally with a single intervening
    //   column of space, using a default (top) alignment.
    def +| : Box => Box = besideS
    def besideS: Box => Box = 
      r => hcat(top)(List(this, emptyBox(0)(1), r))

    // Paste two boxes together vertically, using a default (left)
    //   alignment.
    def % : Box => Box = atop
    def atop(b: Box): Box = 
      vcat(left)(List(this,b))


    // Paste two boxes together vertically with a single intervening row
    //   of space, using a default (left) alignment.
    def %| : Box => Box = atopS
    def atopS : Box => Box = 
      b => vcat(left)(List(this,emptyBox(1)(0), b))
  }

  object Box {
    val rows : Lens[Box, Int] = Lens(_.rows, (obj, v) => obj copy (rows = v))
    val cols : Lens[Box, Int] = Lens(_.cols, (obj, v) => obj copy (cols = v))
    val content : Lens[Box, Content] = Lens(_.content, (obj, v) => obj copy (content = v))
  }

  // Convenient ability to use bare string literals as boxes.
  // implicit def str2box(s:String): Box = text(s)

  // Data type for specifying the alignment of boxes.
  sealed trait Alignment

  case object AlignFirst extends Alignment
  case object AlignLast extends Alignment
  case object AlignCenter1 extends Alignment
  case object AlignCenter2 extends Alignment

  // Align boxes along their top/bottom/left/right
  def top = AlignFirst
  def bottom = AlignLast
  def left = AlignFirst
  def right = AlignLast

  // Align boxes centered, but biased to the left/top (center1) or 
  //  right/bottom (center2) in the case of unequal parities.
  def center1    = AlignCenter1
  def center2    = AlignCenter2

  // Contents of a box.
  sealed trait Content

  case object Blank extends Content
  case class Text(s:String) extends Content
  case class Row(bs:List[Box]) extends Content
  case class Col(bs:List[Box]) extends Content
  case class SubBox(a1: Alignment, a2: Alignment, b:Box) extends Content
  case class AnnotatedBox(props:Map[String, String], b:Box) extends Content


  // The null box, which has no content and no size.  
  def nullBox = emptyBox(0)(0)

  // @emptyBox r c@ is an empty box with @r@ rows and @c@ columns.
  //   Useful for effecting more fine-grained positioning of other
  //   boxes, by inserting empty boxes of the desired size in between
  //   them.
  def emptyBox: Int => Int => Box = 
    r => c => Box(r, c, Blank)

  // A @1x1@ box containing a single character.
  def char: Char => Box =
    c => Box(1, 1, Text(c.toString))

  // A (@1 x len@) box containing a string of length @len@.
  def text: String => Box =
    s => Box(1, s.length, Text(s))


  // Glue a list of boxes together horizontally, with the given alignment.
  def hcat: Alignment => List[Box] => Box =
    a => bs => {
      def h = (0 :: (bs ∘ (_.rows))) max
      def w = (bs ∘ (_.cols)) sum
      val aligned = alignVert(a)(h)
      Box(h, w, Row(bs ∘ aligned))
    }

  // @hsep sep a bs@ lays out @bs@ horizontally with alignment @a@,
  //   with @sep@ amount of space in between each.
  def hsep: Int => Alignment => List[Box] => Box =
    sep => a => bs => punctuateH(a)(emptyBox(0)(sep))(bs)


  // Glue a list of boxes together vertically, with the given alignment.
  def vcat: Alignment => List[Box] => Box =
    a => bs => {
      def h = (bs ∘ (_.rows)).sum
      def w = (0 :: (bs ∘ (_.cols))) max
      val aligned = alignHoriz(a)(w)
      Box(h, w, Col(bs ∘ aligned))
    }


  // @vsep sep a bs@ lays out @bs@ vertically with alignment @a@,
  //   with @sep@ amount of space in between each.
  def vsep: Int => Alignment => List[Box] => Box =
    sep => a => bs => punctuateV(a)(emptyBox(sep)(0))(bs)


  // @punctuateH a p bs@ horizontally lays out the boxes @bs@ with a
  //   copy of @p@ interspersed between each.
  def punctuateH: Alignment => Box => List[Box] => Box =
    a => p => bs => hcat(a)(bs intersperse p)

  // A vertical version of 'punctuateH'.
  def punctuateV: Alignment => Box => List[Box] => Box =
    a => p => bs => vcat(a)(bs intersperse p)

  //------------------------------------------------------------------------------
  //  Paragraph flowing  ---------------------------------------------------------
  //------------------------------------------------------------------------------

  // @para algn w t@ is a box of width @w@, containing text @t@,
  //   aligned according to @algn@, flowed to fit within the given
  //   width.
  def para: Alignment => Int => String => Box = 
    a => n => t => 
      flow(n)(t) |> (ss => mkParaBox(a) (ss.length) (ss))
      // ((ss:List[String]) => mkParaBox(a) (ss.length) (ss)) (flow(n)(t))



  // @columns w h t@ is a list of boxes, each of width @w@ and height
  //   at most @h@, containing text @t@ flowed into as many columns as
  //   necessary.
  def columns : (Alignment, Int, Int, String) => List[Box] = 
    (a, w, h, t) =>  flow(w)(t) ∘ (_.grouped(h).toList) ∘ (mkParaBox(a)(h))

  // @mkParaBox a n s@ makes a box of height @n@ with the text @s@
  //   aligned according to @a@.
  def mkParaBox : Alignment => Int => List[String] => Box = 
    a => n => alignVert(top)(n) compose vcat(a) compose (_.map(text))


  def words = wsv
  def unwords = (ws:List[String]) => ws.mkString(" ")


  // Flow the given text into the given width.
  def flow : Int => String => List[String] = 
    n => t => {
      val wrds = words(t) ∘ mkWord
      val para = wrds.foldl (emptyPara(n)) { addWordP }
      para |> getLines |> (_.map(_.take(n)))
    }

  sealed trait ParaContent

  case class Para(paraWidth : Int, paraContent : ParaContent)
  val paraWidth: Lens[Para, Int] = Lens(_.paraWidth, (obj, v) => obj copy (paraWidth = v))
  val paraContent: Lens[Para, ParaContent] = Lens(_.paraContent, (obj, v) => obj copy (paraContent = v))

  case class Block(fullLines : List[Line], lastLine  : Line) extends ParaContent
  val fullLines: Lens[Block, List[Line]] = Lens(_.fullLines, (obj, v) => obj copy (fullLines = v))
  val lastLine: Lens[Block, Line] = Lens(_.lastLine, (obj, v) => obj copy (lastLine = v))

  def emptyPara(pw: Int) : Para = 
    Para(pw, (Block(nil, (Line(0, nil)))))

  def getLines : Para => List[String] = 
    p => {
      def process =  (l:List[Line]) => l.reverse ∘ Line.getWords ∘ (_.map(Word.getWord)) ∘ (_.reverse) ∘ unwords

      p match {
        case Para(_, (Block(ls, l))) => 
          if (l.len == 0) process(ls)
          else            process(l::ls)
      }
    }

  case class Line(len: Int, words: List[Word])
  object Line {
    val lenL: Lens[Line, Int] = Lens(_.len, (obj, v) => obj copy (len = v))
    val wordsL: Lens[Line, List[Word]] = Lens(_.words, (obj, v) => obj copy (words = v))
    val getLen = lenL.apply _
    val getWords = wordsL.apply _
  }

  // 
  def mkLine : List[Word] => Line = 
    ws => Line((ws ∘ Word.getLen).sum + ws.length - 1, ws)

  def startLine : Word => Line = 
    w => mkLine(w :: Nil)

  case class Word(len:Int, word:String)
  object Word {
    val lenL: Lens[Word, Int] = Lens(_.len, (obj, v) => obj copy (len = v))
    val wordL: Lens[Word, String] = Lens(_.word, (obj, v) => obj copy (word = v))
    val getLen = lenL.apply _
    val getWord = wordL.apply _
  }

  def mkWord : String => Word = 
    w => Word(w.length, w)

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


  def addWordL : (Word, Line) => Line = 
    (w, l) => l match {
      case Line(len, ws) => Line((len + w.len + 1), (w::ws))
    }


  def wordFits : (Int, Word, Line) => Boolean = 
    (pw, w, l) => 
      l.len == 0 || l.len + w.len + 1 <= pw


  //------------------------------------------------------------------------------
  //  Alignment  -----------------------------------------------------------------
  //------------------------------------------------------------------------------

  // @alignHoriz algn n bx@ creates a box of width @n@, with the
  //   contents and height of @bx@, horizontally aligned according to
  //   @algn@.
  def alignHoriz: Alignment => Int => Box => Box = 
    a => c => b => {
      Box(b.rows, c, SubBox(a, AlignFirst, b))
    }

  // @alignVert algn n bx@ creates a box of height @n@, with the
  //   contents and width of @bx@, vertically aligned according to
  //   @algn@.
  def alignVert: Alignment => Int => Box => Box = 
    a => r => b => 
      Box(r, (b.cols), SubBox(AlignFirst, a, b))


  // @align ah av r c bx@ creates an @r@ x @c@ box with the contents
  //   of @bx@, aligned horizontally according to @ah@ and vertically
  //   according to @av@.
  def align : (Alignment, Alignment, Int, Int, Box) => Box =
    (ah, av, r, c, bx) => Box(r, c, SubBox(ah, av, bx))

  // Move a box \"up\" by putting it in a larger box with extra rows,
  //   aligned to the top.  See the disclaimer for 'moveLeft'.
  def moveUp : Int => Box => Box = 
    n => b => alignVert(top)(b.rows + n)(b)


  // Move a box down by putting it in a larger box with extra rows,
  //   aligned to the bottom.  See the disclaimer for 'moveLeft'.
  def moveDown : Int => Box => Box = 
    n => b => alignVert(bottom)(b.rows + n)(b)

  // Move a box left by putting it in a larger box with extra columns,
  //   aligned left.  Note that the name of this function is
  //   something of a white lie, as this will only result in the box
  //   being moved left by the specified amount if it is already in a
  //   larger right-aligned context.
  def moveLeft : Int => Box => Box = 
    n => b => alignHoriz(left)(b.cols + n)(b)


  // Move a box right by putting it in a larger box with extra
  //   columns, aligned right.  See the disclaimer for 'moveLeft'.
  def moveRight : Int => Box => Box = 
    n => b => alignHoriz(right)(b.cols + n)(b)


  //------------------------------------------------------------------------------
  //  Implementation  ------------------------------------------------------------
  //------------------------------------------------------------------------------

  // Render a 'Box' as a String, suitable for writing to the screen or
  //   a file.
  def render : Box => String = 
    b => renderBox(b) |> (_.mkString("\n")) 

  // \"Padded take\": @takeP a n xs@ is the same as @take n xs@, if @n
  //   <= length xs@; otherwise it is @xs@ followed by enough copies of
  //   @a@ to make the length equal to @n@.
  def takeP[A] : A => Int => List[A] => List[A] = 
    a => n => aas => {
      val pad = if (n <= aas.length) 0 else n - aas.length
      // aas.take(n) ::: a.repeat[List].take(pad)
      aas.take(n) ::: a.replicate[List](pad)
    }

  // @takePA @ is like 'takeP', but with alignment.  That is, we
  //   imagine a copy of @xs@ extended infinitely on both sides with
  //   copies of @a@, and a window of size @n@ placed so that @xs@ has
  //   the specified alignment within the window; @takePA algn a n xs@
  //   returns the contents of this window.
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


  // Generate a string of spaces.
  def blanks : Int => String = 
    n => " " * n

  // Render a box as a list of lines.
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


  // Render a box as a list of lines, using a given number of rows.
  def renderBoxWithRows : Int => Box => List[String] = 
    r => b => renderBox (Box.rows.set(b, r))

  // Render a box as a list of lines, using a given number of columns.
  def renderBoxWithCols : Int => Box => List[String] = 
    c => b => renderBox (Box.cols.set(b, c))

  // Resize a rendered list of lines.
  def resizeBox : (Int, Int, List[String]) => List[String] =
    (r, c, ss) => {
      val taker = takeP(" "*c)(r)
      val takec = ss map (s => (takeP(' ')(c)(s.toList)).mkString(""))
      taker(takec)
    }

  // Resize a rendered list of lines, using given alignments.
  def resizeBoxAligned : (Int, Int, Alignment, Alignment) => List[String] => List[String] = 
     (r, c, ha, va) => ss => 
       takePA(va)(blanks(c))(r){
         (ss.map (_.toList)) ∘ (takePA(ha)(' ')(c)) ∘ (_.mkString(""))
       }

  // A convenience function for rendering a box to stdout.
  def printBox : Box => Unit = 
    box => println(render(box))

}
