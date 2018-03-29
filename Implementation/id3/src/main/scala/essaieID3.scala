import org.apache.spark.{SparkConf, SparkContext}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.io.Path

object essaieID3 {

  val conf = new SparkConf().setAppName("ID3").setMaster("local")
  val sc = new SparkContext(conf)
  val inputPath = "fichier/texte4.txt"
  val outputPath = "fichier/Resultat"
  var niveau = 0
  var resultat: ListBuffer[String] = ListBuffer[String]()
  var noeud = "root"

  def main(args: Array[String]): Unit = {
    print("test")
    /*  val conf = new SparkConf().setAppName("Word Count").setMaster("local")
      val sc = new SparkContext(conf)
  */
    //sc.textFile(inputPath).flatMap(_.split(" ")).map((_,1)).reduceByKey(_+_).map(rec=> rec._1+ "\t" +rec._2).saveAsTextFile(outputPath)
    //println
    val texte = sc.textFile(inputPath)
    val listTexte: List[String] = texte.collect().toList
    // traitement(0,noeud,listTexte)
    preparation(listTexte)
  }

  def preparation(listTexte:List[String]):Unit={
    println("__________________________________________________")
    val nombreAttribut =
      (listTexte.take(1).toList.flatMap(line=>line.split(" ")).size)-1

    val jj = ListBuffer[String]()
    val liste2 = ListBuffer[String]()
    for(i<-0 to (listTexte.size-1)){
      var l:Array[String] =listTexte(i).split(" ")
      for(j<-0 to (l.length-2)){
        liste2+=j+":"+l(j)+"_"+l(l.length-1)
      }
    }
    val liste3 = liste2.toList

    val rdd= sc.parallelize(liste3)
    val rdd2 = rdd.map(word=>(word.split(":")(0),word.split(":")(1)))
    val rdd3 = rdd2.reduceByKey(_+","+_)
    val myMAP= rdd3.collect.toMap.toSeq.sortBy(_._1).toMap
    //      toMap[String,String]
    //println("MyMAP"+myMAP)
    trat("Root","-1",myMAP)
  }
  def trat(noeud:String,cls:String,x:Map[String,String]):Unit={
   // println("teste !!!!!!!!")
    // println("entrant : "+x)
    // println(x)
    val map_verif:mutable.HashMap[String,Int]=mutable.HashMap[String,Int]()
    for((k,v)<-x){
      val l:List[String] = v.split(",").toList
      for(j<-0 to l.size-1){
        map_verif += (l(j).split("_")(1) -> 1)
      }
    }
    //println(map_verif)
    //println(map_verif.size)
    if(map_verif.size>1){
      var listGain = ListBuffer[Double]()
      // val entropieC = Calcul.EntropieClasses(x)
      val entropieC = Calcul2.calculeEntropieClasses(x)
      var listGainn:mutable.HashMap[Int,Double] = mutable.HashMap[Int,Double]()
      //Calcul
      for((k,v) <-x){
        val bb:List[String] = v.split(",").toList
        // listGain += entropieC.toDouble - Calcul.gainn(bb).toDouble
        listGainn += (k.toInt -> (entropieC.toDouble - Calcul2.gainn(bb).toDouble))
      }
      var max=0.0
      var pos =0
      for((k,v)<-listGainn){
        if(max < v){
          max = v
          pos = k
        }
      }
      // println("max  : "+max +" pos  : "+pos)
      //if(att== -1){
      div(noeud,pos,x)
      // }
    }else{

      var a=""
      for((k,v)<-map_verif){
        a=k
      }
      //println(""+noeud+"="+"->["+a+"]")


      resultat += ""+noeud+"="+"->["+a+"]"
      // println(""+noeud+"---> ["+cls+"]")
      val rdddd = sc.parallelize(resultat)

      if(scala.reflect.io.File(scala.reflect.io.Path(outputPath)).exists){
        val jj:Path = Path(outputPath)
        jj.deleteRecursively()
        rdddd.saveAsTextFile(outputPath)
      }else{
        rdddd.saveAsTextFile(outputPath)
      }

    }
  }
  def div(noeud:String,pos:Int,x:Map[String,String]):Unit={
   // println("test 2 !!!!!! ")
    val suppAtt:mutable.HashMap[String,String] = mutable.HashMap[String,String]()
    val valDeCl:mutable.HashMap[String,String] = mutable.HashMap[String,String]()
    val g:List[String] = x(pos.toString).split(",").toList

    for(i<-0 to g.size-1){
      if(valDeCl.contains(g(i).split("_")(0)))
      {
        valDeCl(g(i).split("_")(0)) = i+","+valDeCl(g(i).split("_")(0))
      }else{
        valDeCl.put(g(i).split("_")(0),i+"")
      }
    }
    for((k,v)<-x){
      if(!k.equals(pos.toString)){
        suppAtt.put(k,v)
      }
    }

   for((k,v)<- valDeCl) {
      val o:ListBuffer[String] = ListBuffer[String]()
      val lesPos: List[String] = v.split(",").toList
      //println("ss : " + lesPos)
      val mapR:mutable.HashMap[String,String] = mutable.HashMap[String,String]()
      for(i<-0 to lesPos.size-1){

        // println(k+" "+lesPos(i))
        for((kk,vv)<- suppAtt){
          // val po:List[String] = vv.split(",").toList
          //o += kk + " : " + vv.split(",")(lesPos(i).toInt)
          if(mapR.contains(kk+"")){
            mapR(kk+"") = mapR(kk+"")+ ","+vv.split(",")(lesPos(i).toInt)
          }else{
            mapR.put(kk+"",vv.split(",")(lesPos(i).toInt))
          }
        }
      }
      trat(noeud+":"+pos+"="+k,k,mapR.toMap)

    }


  }
}
