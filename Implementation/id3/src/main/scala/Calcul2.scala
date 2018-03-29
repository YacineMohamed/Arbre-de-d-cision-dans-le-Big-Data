import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Calcul2 {
  def calculeEntropieClasses(x: Map[String, String]): Double = {
    var entropieC = 0.0
    var total = 0
    var cpt = 0
    val map_ = mutable.HashMap[String, Int]()
    for ((k, v) <- x) {
      if (cpt == 0) {
        total = v.split(",").toList.size
        val l: List[String] = v.split(",").toList
        for (i <- 0 to l.size - 1) {
          if (map_.contains(l(i).split("_")(1))) {
            map_(l(i).split("_")(1)) = map_(l(i).split("_")(1)) + 1
          } else {
            map_ += (l(i).split("_")(1) -> 1)
          }
        }
      }
      cpt += 1
    }
    // println("Total : "+totall)
    //println("map_ :"+map_)
    for ((k, v) <- map_) {
      println(v)
      entropieC += -(v.toDouble / total.toDouble) * (calcLog2(v.toDouble / total.toDouble))
    }
    return entropieC
  }
  def gainn(x:List[String]) : Double={
    val total = x.size
    val map = mutable.HashMap[String,Int]()
    for(a<-x){
      if(map.contains(a.toString)){
        map(a.toString) = map(a.toString)+1
      }else{
        map += (a.toString->1)
      }
    }
    //println(map)
    val hh = ListBuffer[String]()
    for((k,v)<- map){
      // println(k+" : "+v)
      hh += k+" = "+v
    }
    //println("hh : "+hh)
    val uu = ListBuffer[String]()
    val monMap = new mutable.HashMap[String,String]()
    for(i<-0 to (hh.size-1)){
      var t = hh(i).split("=")(0).split("_")(0)
      var test=false
      var kk=""
      for(j<-0 to hh.size-1){
        if(i!=j){
          if(hh(j).split("=")(0).split("_")(0).equals(t)){
            test=true
            if(kk.equals("")){
              kk=""+hh(j)
            }else{
              if(kk.compareTo(hh(j))>0){
                kk=kk+":"+hh(j)
              }else{
                kk=hh(j)+":"+kk
              }
            }
          }
        }
      }
      if(test){
        if(hh(i).compareTo(kk)>0){
          monMap += (hh(i)+":"+kk -> "1")
        }else{
          monMap += (kk+":"+hh(i) -> "1")
        }
      }else{
        monMap += (hh(i) -> "1")
      }
      kk=""
    }
    // println("Monmap : "+monMap)
    for((k,v)<-monMap){
      uu += ""+k
    }
    //println("uu : "+uu)
    var entr = 0.0
    for(i<-0 to uu.size-1){
      var entropie=0.0
      val tab = uu(i).split(":")
      if(tab.length>1){
        var sum=0
        for(k<-0 to tab.length-1){
          sum=sum+(tab(k).split("=")(1)).replaceAll("\\s","").toInt
        }
        for(j<-0 to tab.length-1){
          entropie +=(-(tab(j).split("=")(1).replaceAll("\\s","").toInt).toDouble /
            sum.toDouble) * calcLog2((tab(j).split("=")(1).replaceAll("\\s","").toInt / sum.toDouble))
        }
        entropie = entropie.toDouble * (sum.toDouble/total.toDouble)
        entr += entropie.toDouble
      }
    }
    return entr
  }
  def calcLog2(num:Double):Double={
    return(Math.log(num) / Math.log(2))
  }
}