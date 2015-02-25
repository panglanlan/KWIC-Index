import java.io._
import scala.io.Source
import scala.swing.FileChooser
import scala.collection.mutable.Map

object Keyword {
  def getWordsFromText(text: String): List[String] = {
    //(?=.*\w)^(\w|')+$
    ("""[a-zA-Z|a-zA-Z'a-zA-Z]+""".r findAllIn text).toList
  }

  def addlist (elm:String, lst:List[String]) =lst match {
    case h::t => elm::lst
    case _ => elm::List()
  }

  def addMap (key:String, line:Int, map:scala.collection.mutable.Map[String, List[Int]], stop:List[String]) :scala.collection.mutable.Map[String, List[Int]] ={
    if (stop.exists{ a=> a == key}) return map
    else if (map.contains(key) == false) {
      val linelst = line::List()
      map += (key -> linelst)
      return map
    }
    else {
      val newlinelst = line::map(key)
      map(key) = newlinelst
      return map
    }
  }
  def main(args: Array[String]) {
    val file_stopword = new File("stop_word.txt")
    val stoplist = Source.fromFile(file_stopword).getLines.toList
    val outfile = new File("kwic_index.txt")
    val writer = new PrintWriter(outfile)

    //    val sfile = choosePlainFile()
    //    val file = sfile match {case Some(sfile) => sfile;case None=>null}
    var input = true
    println("specify file directory or file path.")
    var fileList:List[java.io.File] = List()
    while (input) {
      var fline = readLine()
      if (fline == "") {
        input = false
      }
      else {
        var ifile = new File(fline)
        if (ifile.isDirectory == true) {
          fileList = ifile.listFiles.filter(_.getName.endsWith(".txt")).toList ++ fileList
        }
        else
          fileList = ifile::fileList
      }
    }
    //fileList = fileList.tail
    if (fileList.isEmpty) {print("error:no file specified."); return}
    fileList.foreach{println}
    for (file<-fileList) {
      writer.write("File Name: "+file+"\r\n")
      val linelist = Source.fromFile(file).getLines().foldRight(List("")) {(elm, lst)=>addlist(elm, lst)}
      //linelist.foreach{println}
      val keywordMap = scala.collection.mutable.Map[String, List[Int]]()
      val numlineMap = scala.collection.mutable.Map[Int, String]()
      var x = ""

      val line_numlist = 1 to linelist.length
      val line_vs_num = (linelist zip line_numlist)
      for ( (lines, line_num) <- line_vs_num) {
        numlineMap += (line_num -> lines)
        //a list of satisfied words
        getWordsFromText(lines.toLowerCase).foreach {
          (x) => addMap(x, line_num, keywordMap, stoplist)
        }
      }
      
      for ((wd,ln) <- keywordMap.toSeq.sortBy(_._1)) {
        val line_num_list = ln.reverse
        for (l <- line_num_list) {
          val index = numlineMap(l).indexOf(wd)
          val (f, b) = numlineMap(l).splitAt(index)
          val res = b.length
          var bsub = b.substring(wd.length, res);
          var fsub = f;
          if (bsub.length >= 30)
            bsub = bsub.substring(0, 30)
          if (f.length >= 30)  fsub = f.substring(f.length-30, f.length)
          writer.write("%-6d".format(l)+" "+"%30s".format(fsub)+"  "+wd+"%-30s".format(bsub)+"\r\n")
        }
      }
    }
    writer.close
  }
}
