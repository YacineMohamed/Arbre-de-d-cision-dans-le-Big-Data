import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

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

  def gainn(entopieC:Double,x: List[String]): Double = {
    val total = x.size
    val map = mutable.HashMap[String, Int]()
    for (a <- x) {
      if (map.contains(a.toString)) {
        map(a.toString) = map(a.toString) + 1
      } else {
        map += (a.toString -> 1)
      }
    }
    //println(map)
    val hh = ListBuffer[String]()
    for ((k, v) <- map) {
      // println(k+" : "+v)
      hh += k + " = " + v
    }
    //println("hh : "+hh)
    val uu = ListBuffer[String]()
    val monMap = new mutable.HashMap[String, String]()
    for (i <- 0 to (hh.size - 1)) {
      var t = hh(i).split("=")(0).split("_")(0)
      var test = false
      var kk = ""
      for (j <- 0 to hh.size - 1) {
        if (i != j) {
          if (hh(j).split("=")(0).split("_")(0).equals(t)) {
            test = true
            if (kk.equals("")) {
              kk = "" + hh(j)
            } else {
              if (kk.compareTo(hh(j)) > 0) {
                kk = kk + ":" + hh(j)
              } else {
                kk = hh(j) + ":" + kk
              }
            }
          }
        }
      }
      if (test) {
        if (hh(i).compareTo(kk) > 0) {
          monMap += (hh(i) + ":" + kk -> "1")
        } else {
          monMap += (kk + ":" + hh(i) -> "1")
        }
      } else {
        monMap += (hh(i) -> "1")
      }
      kk = ""
    }
    // println("Monmap : "+monMap)
    for ((k, v) <- monMap) {
      uu += "" + k
    }
    //println("uu : "+uu)
    var entr = 0.0
    for (i <- 0 to uu.size - 1) {
      var entropie = 0.0
      val tab = uu(i).split(":")
      if (tab.length > 1) {
        var sum = 0
        for (k <- 0 to tab.length - 1) {
          sum = sum + (tab(k).split("=")(1)).replaceAll("\\s", "").toInt
        }
        for (j <- 0 to tab.length - 1) {
          entropie += (-(tab(j).split("=")(1).replaceAll("\\s", "").toInt).toDouble /
            sum.toDouble) * calcLog2((tab(j).split("=")(1).replaceAll("\\s", "").toInt / sum.toDouble))
        }
        entropie = entropie * (sum.toDouble / total.toDouble)
        entr += entropie
      }
    }
    return entopieC.toDouble-entr
  }


  def gainRatio(entropieC:Double,x:List[String]): Double ={
    val ratio= (gainn(entropieC.toDouble,x).toDouble) / (infoSplit(x)).toDouble
     //   val gain = gainn(entropieC.toDouble,x).toDouble
    return ratio.toDouble
  }
def infoSplit(x:List[String]):Double={
  var InfoSplit = 0.0
  val total = x.size
  val hashM:mutable.HashMap[String,Int] = new mutable.HashMap[String,Int]()
  for(a <- x){
    if(hashM.contains(a.toString.split("_")(0))){
      hashM(a.toString.split("_")(0)) = hashM(a.toString.split("_")(0))+1
    }else{
      hashM += (a.toString.split("_")(0)->1)
    }
  }
  for((k,v)<- hashM){
    InfoSplit = InfoSplit+ (- (v.toDouble / total.toDouble) * calcLog2(v.toDouble / total.toDouble))
  }
  return InfoSplit
}

  def calcLog2(num: Double): Double = {
    return (Math.log(num) / Math.log(2))
  }


  def verifNumeric(x:ListBuffer[String]): mutable.HashMap[Int,Int] = {
    var hash_Verif_num = new mutable.HashMap[Int, Int]()
    for (i <- 0 to x.size - 1) {
      val haha: Array[String] = x(i).split(" ")
      for (j <- 0 to haha.size - 1) {
        if (haha(j).forall(_.isDigit)) {
          if (hash_Verif_num.contains(j)) {
            hash_Verif_num(j) = hash_Verif_num(j) + 1
          } else {
            hash_Verif_num += (j -> 1)
          }
        }
      }
    }
    return hash_Verif_num
  }

  def TraitDisc(x:ListBuffer[String], hh:mutable.HashMap[Int,Int],pos:Int): ListBuffer[String] ={
    // println("POSTITIONNNNNNNNNNNNNNNNNNNNN : "+pos)
    var vMin=0
    var pMin=0
    val kk3:ListBuffer[Double] = ListBuffer[Double]()
    //  val zazazaza= scala.io.StdIn.readLine()
    val arrayBuffer:ArrayBuffer[String] = ArrayBuffer[String]()
    var array: Array[String] = x.toArray
    var kk: ArrayBuffer[Int] = mutable.ArrayBuffer[Int]()
    for ((k, v) <- hh) {
      kk += k
    }
    val kk2: Array[Int] = kk.toArray
    scala.util.Sorting.quickSort(kk2)

    for(i<-0 to array.size-1) {
      var ss = ""
      val gg: Array[String] = array(i).split(" ")
      gg.toBuffer

      for (z <- 0 to kk2.size - 1) {
      }
      for (p <- 0 to kk2.length - 2) {
        kk3 += (kk2(p).toDouble + kk2(p + 1).toDouble) / 2
      }
      val listSeuil: ListBuffer[Double] = ListBuffer[Double]()
      for (s <- 0 to kk3.size - 1) {
        listSeuil += Calcul2.blabla(x, kk3(s), pos)
      }
      val arr: Array[Double] = listSeuil.toArray.sorted
      var min = arr(arr.size - 1)
      var posMIN = 0
      for (s <- 0 to listSeuil.size - 1) {
        if (listSeuil(s) < min) {
          min = listSeuil(s)
          posMIN = s
        }
      }
      pMin = posMIN

    }

    val valMin= kk3(pMin)

    for(i<-0 to array.size-1){
      val jj:ArrayBuffer[String] = array(i).split(" ").to[ArrayBuffer]
      //println(" JJJ 2 "+jj.toList)

      if(jj(pos).toDouble<valMin.toDouble){
        jj(pos)="<"+valMin
      }else{
        jj(pos) = ">"+valMin
      }

      // println(" JJJ "+jj.toList)
      // val kkkkkkkkkk = scala.io.StdIn.readLine()

      var ss=""
      for(j<-0 to jj.size-1){
        if(ss.equals("")){
          ss = jj(j)
        }else{
          ss = ss+" "+jj(j)
        }
      }
      //  println("ss : "+ss)
      array(i) = ss
    }




    val res:ListBuffer[String] = array.to[ListBuffer]
    //println("RESSSSSSSSSSSSSSSSSS : "+res)
    // val kqkq = scala.io.StdIn.readLine() ><
    return res
  }


  def blabla(x:ListBuffer[String],y:Double, pos:Int): Double ={
    // println("x: "+x)
    //println("pos : "+pos)
    // println("y : "+y)
    var res:ListBuffer[String] = ListBuffer[String]()

    for(i<-0 to x.size-1){
      val hh:ArrayBuffer[String] = x(i).split(" ").to[ArrayBuffer]
      if(hh(pos).toDouble < y){
        hh(pos) = ">"+y
      }else{
        hh(pos) = "<"+y
      }
      var ss=""
      for(j<-0 to hh.size-1){
        if(ss.equals("")){
          ss = hh(j)
        }else{
          ss = ss +" "+hh(j)
        }
      }
      res += ss

    }
    //println(" Res : "+res) blabla

    //println("RES : "+res)
    // val uu = scala.io.StdIn.rea
    val hash:mutable.HashMap[String,Int] = new mutable.HashMap[String,Int]()
    for(i<-0 to res.size-1){
      val kk:Array[String] = res(i).split(" ")
      if(hash.contains(kk(pos)+"_"+kk(kk.size-1))){
        hash(kk(pos)+"_"+kk(kk.size-1)) = hash(kk(pos)+"_"+kk(kk.size-1)) + 1
      }else{
        hash += ( (kk(pos)+"_"+kk(kk.size-1)) -> 1 )
      }

      //   println(res)
    }

    //println("hassh ::::: "+hash)

    var hh = ListBuffer[String]()
    for((k,v)<- hash){
      // println(k+" : "+v)
      hh += k+" = "+v
    }

    // println("hh !!!!: "+hh)
    //val jajajaja = scala.io.StdIn.readLine()

    val uu = ListBuffer[String]()
    val monMap = new mutable.HashMap[String,String]()


    for(i<-0 to hh.size-1){
      val aa = hh(i).split("=")(0).split("_")(0)

      if(monMap.contains(aa)){
        val gg:Array[String] = monMap(aa).split(":")
        var test = false
        for(j<- 0 to gg.size-1){
          if(gg(j).equals(hh(i).split("_")(1))){
            test = true
          }
        }
        if(!test){
          monMap(aa) = monMap(aa) +":"+ hh(i).split("_")(1)
        }
      } else{
        monMap += (aa -> (hh(i).split("_")(1)))
      }

    }

    var total = 0
    var uuu = ListBuffer[String]()
    for ((k,v)<- monMap){
      val jiji:Array[String] = v.split(":")
      var ss=""
      for(i<-0 to jiji.size-1){
        if(ss.equals("")){
          ss = k+"_"+jiji(i)
        }else{
          ss=ss+":"+k+"_"+jiji(i)
        }

        total = total + jiji(i).split("=")(1).replaceAll("\\s","").toInt
      }
      uuu += ss
    }

    //println("TOTAL : "+total)
    //println("uuu : "+uuu)

    //val jffdsds = scala.io.StdIn.readLine()

    var entr = 0.0
    var entropie=0.0

    for(i<-0 to uuu.size-1){
      val tab = uuu(i).split(":")
      if(tab.length>1){
        var sum=0
        for(k<-0 to tab.length-1){
          sum=sum+(tab(k).split("=")(1)).replaceAll("\\s","").toInt
        }
        var compte =0
        for(j<-0 to tab.length-1){
          compte = compte +tab(j).split("=")(1).replaceAll("\\s","").toInt
          entropie = entropie.toDouble + (- (tab(j).split("=")(1).replaceAll("\\s","").toInt).toDouble / sum.asInstanceOf[Double]) * calcLog2((tab(j).split("=")(1).replaceAll("\\s","").toInt / sum.asInstanceOf[Double]).toDouble)
        }
        entropie = entropie.toDouble * (sum.toDouble/total.toDouble)
        entr = entr.toDouble + entropie.toDouble
        entropie=0.0
      }
    }

    //println("ENTROPIE : "+entr)
    //scala.io.StdIn.readLine()
    // println("list :: "+uuu)



    return entr
  }



}
