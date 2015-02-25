import java.io.File
import scala.io.Source
import scala.collection.mutable.Map
import org.scalatest.FunSuite

class keywordTest extends FunSuite {
  def addMap (key:String, line_num:Int, map:scala.collection.mutable.Map[String, List[Int]], stop:List[String]) :scala.collection.mutable.Map[String, List[Int]] ={
    if (stop.exists{ a=> a == key}) return map
    else if (map.contains(key) == false) {
      val linelst = line_num::List()
      map += (key -> linelst)
      return map
    }
    else {
      val newlinelst = line_num::map(key)
      map(key) = newlinelst
      return map
    }
  }
  test ("key-value") {
    val file_stopword = new File("stop_word.txt")
    val stoplist = Source.fromFile(file_stopword).getLines.toList
    val keywordMap = scala.collection.mutable.Map[String, List[Int]]()
    val keyword="Example"
    val line_num = 7
    addMap(keyword, line_num, keywordMap, stoplist)
    assert(keywordMap(keyword) == List(line_num))
    expect(List(7)){keywordMap(keyword)}
  }

  test ("key-multi-line number list") {
    val file_stopword = new File("stop_word.txt")
    val stoplist = Source.fromFile(file_stopword).getLines.toList
    val keywordMap = scala.collection.mutable.Map[String, List[Int]]()
    val keyword = "Example"
    val line_num0 = 7
    addMap(keyword, line_num0, keywordMap, stoplist)
    val line_num1 = 8
    addMap(keyword, line_num1, keywordMap, stoplist)
    val line_num2 = 10
    addMap(keyword, line_num2, keywordMap, stoplist)
    val line_num3 = 16
    addMap(keyword, line_num3, keywordMap, stoplist)    
    assert(keywordMap(keyword) == List(16,10,8,7))
  }

  test ("avoid stop words") {
    val file_stopword = new File("stop_word.txt")
    val stoplist = Source.fromFile(file_stopword).getLines.toList
    val keywordMap = scala.collection.mutable.Map[String, List[Int]]()
    val keyword0 = "on"
    val line_num0 = 7
    addMap(keyword0, line_num0, keywordMap, stoplist)
    val keyword1 = "the"    
    val line_num1 = 8
    addMap(keyword1, line_num1, keywordMap, stoplist)
    val keyword2 = "skyline"        
    val line_num2 = 10
    addMap(keyword2, line_num2, keywordMap, stoplist)
    val keyword3 = "above"            
    val line_num3 = 16
    addMap(keyword3, line_num3, keywordMap, stoplist)
    assert(keywordMap.size == 1)    
    assert(keywordMap(keyword2) == List(10))
  }    
}
