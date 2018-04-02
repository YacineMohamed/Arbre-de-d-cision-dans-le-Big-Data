package Traitement

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Calcul2 {
  //print
  //Methoe utilisé afin d eviter d ecrire la formule a cahque calcul
  def calcLog2(num: Double): Double = {
    return (Math.log(num) / Math.log(2))
  }

  //%ethode utilisé pour verifier l'existance d'un attribut numerique,
  /*
  il doit contneir ds valeur numerique au niveau de TOUTES les instance,
  si pour 10 000 instance, dans 9 999 il est numerique, il est considéré comme att discret
  car sinon sa discretisation va generé une exception
  autre sol : interagir avec l'user dans de telles situation? supprimé l'anomalie,..? reste a creuser.
   */
  def verifNumeric(x: ListBuffer[String]): mutable.HashMap[Int, Int] = {

    var hash_Verif_num = new mutable.HashMap[Int, Int]()
    for (i <- 0 to x.size - 1) {
      val haha: Array[String] = x(i).split(" ")
      for (j <- 0 to haha.size - 1) {
       // if (haha(j).forall(_.isDigit)) {
          if (isParsableAsDouble(haha(j))) {
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
  /*/
  Traitement d'un attribut discret, pour ce :
  1) chacune de ses valeur est récupéré, puis l'ensemble est trié par ordre croissant.
  2) création d'un seuil pour chaque valeur consécutive, tel que S = (Ai+A(i+1)) / 2
  3) pour chaque seui:
     A) modification des valeur initiales de l'attribut, tel quel chaque valeur poura etre :
          '>seuil'  OU '<seuil' .... pourquoi pas le cas  '=' ?  : car les seuil on été crée
          suivant en calculant la moyenne de chaque att consécutive, donc il est impossbile qu'un
          attribut soit egal a un seuil !
     B)  calcul de l'entropie de l'attribut avec ces nouvelles valeurs
     C) le seuil qui aura la plus basse entropie (ou bien le plus grand gain) sera choisi
  4) modification de l'ensemble initial selon le seuil choisi
  Retourner le nouvel ensemble discrétisé
  FIN
   */
  def TraitAttDisc(x: ListBuffer[String], hh: mutable.HashMap[Double, Int], pos: Int): ListBuffer[String] = {
    var vMin = 0.0
    var pMin = 0
    val kk3: ListBuffer[Double] = ListBuffer[Double]()
    val arrayBuffer: ArrayBuffer[String] = ArrayBuffer[String]()
    var array: Array[String] = x.toArray
    var kk: ArrayBuffer[Double] = mutable.ArrayBuffer[Double]()
    for ((k, v) <- hh) {
      kk += k
    }
    val kk2: Array[Double] = kk.toArray
    scala.util.Sorting.quickSort(kk2)

    for (i <- 0 to array.size - 1) {
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
        listSeuil += creerSeuil(x, kk3(s), pos)
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

    val valMin = kk3(pMin)

    for (i <- 0 to array.size - 1) {
      val jj: ArrayBuffer[String] = array(i).split(" ").to[ArrayBuffer]
      //println(" JJJ 2 "+jj.toList)

      if (jj(pos).toDouble < valMin.toDouble) {
        jj(pos) = "<" + valMin
      } else {
        jj(pos) = ">" + valMin
      }

      var ss = ""
      for (j <- 0 to jj.size - 1) {
        if (ss.equals("")) {
          ss = jj(j)
        } else {
          ss = ss + " " + jj(j)
        }
      }
      array(i) = ss
    }


    val res: ListBuffer[String] = array.to[ListBuffer]


    return res
  }

  /*
*Cette fonction calcul l'entropie du noeud pere
* Entré : La list contenant la valeur de classe de toutes les instance de données du noeud.
* Sortie : Entropie.
* */
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
     // println(v)
      entropieC += -(v.toDouble / total.toDouble) * (calcLog2(v.toDouble / total.toDouble))
    }
    return entropieC
  }

  /*
  * Cette fonction calcul le gain d'infomration apporté par chaque attribut
  * Entre :
  * 1-L'entropie du noeud pere
  * 2-La liste des valeur de cet attrinut ainsi que les catégorie de classe, afin de calculer son entropie
  * Sortie : gain = entropie du neud pere - netropie de l'attibut
  * */

  def gain(entopieC: Double, x: List[String]): Double = {
    val total = x.size
    val map = mutable.HashMap[String, Int]()
    for (a <- x) {
      if (map.contains(a.toString)) {
        map(a.toString) = map(a.toString) + 1
      } else {
        map += (a.toString -> 1)
      }
    }
    val hh = ListBuffer[String]()
    for ((k, v) <- map) {
      hh += k + " = " + v
    }
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
    for ((k, v) <- monMap) {
      uu += "" + k
    }
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
    return entopieC.toDouble - entr
  }


  def infoSplit(x: List[String]): Double = {
    var InfoSplit = 0.0
    val total = x.size
    val hashM: mutable.HashMap[String, Int] = new mutable.HashMap[String, Int]()
    for (a <- x) {
      if (hashM.contains(a.toString.split("_")(0))) {
        hashM(a.toString.split("_")(0)) = hashM(a.toString.split("_")(0)) + 1
      } else {
        hashM += (a.toString.split("_")(0) -> 1)
      }
    }
    for ((k, v) <- hashM) {
      InfoSplit = InfoSplit + (-(v.toDouble / total.toDouble) * calcLog2(v.toDouble / total.toDouble))
    }
    return InfoSplit
  }


  /*
* Cette fct calcul le rapport de gain de chaque attrbut
* Entre :
*   1-Entripie du noeud pere
*   2-list des valeur de l'attribut ainsi que les categori e classe coreespondante
*   (1+2) :permette de calculer le gain
*   2 : permiet de calculer InfoSplit
*   Sortie : Rapport de gain = Gain / InfoSplit
* */

  def gainRatio(entropieC: Double, x: List[String]): Double = {
    val ratio = (gain(entropieC.toDouble, x)) / (infoSplit(x))
    return ratio.toDouble
  }


  def creerSeuil(x: ListBuffer[String], y: Double, pos: Int): Double = {
    /*
    Ou :
    Param1 : l'ensemble de données
    param2 : le seuil qui sera utilisé dans la discretisation
    param3 : la position de l'att a discretiser

     */

    var res: ListBuffer[String] = ListBuffer[String]()

    for (i <- 0 to x.size - 1) {
      val hh: ArrayBuffer[String] = x(i).split(" ").to[ArrayBuffer]
      if (hh(pos).toDouble < y) {
        hh(pos) = ">" + y
      } else {
        hh(pos) = "<" + y
      }
      var ss = ""
      for (j <- 0 to hh.size - 1) {
        if (ss.equals("")) {
          ss = hh(j)
        } else {
          ss = ss + " " + hh(j)
        }
      }
      res += ss

    }

    val hash: mutable.HashMap[String, Int] = new mutable.HashMap[String, Int]()
    for (i <- 0 to res.size - 1) {
      val kk: Array[String] = res(i).split(" ")
      if (hash.contains(kk(pos) + "_" + kk(kk.size - 1))) {
        hash(kk(pos) + "_" + kk(kk.size - 1)) = hash(kk(pos) + "_" + kk(kk.size - 1)) + 1
      } else {
        hash += ((kk(pos) + "_" + kk(kk.size - 1)) -> 1)
      }

    }

    //println("hassh ::::: "+hash)

    var hh = ListBuffer[String]()
    for ((k, v) <- hash) {
      // println(k+" : "+v)
      hh += k + " = " + v
    }

    // println("hh !!!!: "+hh)
    //val jajajaja = scala.io.StdIn.readLine()

    val uu = ListBuffer[String]()
    val monMap = new mutable.HashMap[String, String]()


    for (i <- 0 to hh.size - 1) {
      val aa = hh(i).split("=")(0).split("_")(0)

      if (monMap.contains(aa)) {
        val gg: Array[String] = monMap(aa).split(":")
        var test = false
        for (j <- 0 to gg.size - 1) {
          if (gg(j).equals(hh(i).split("_")(1))) {
            test = true
          }
        }
        if (!test) {
          monMap(aa) = monMap(aa) + ":" + hh(i).split("_")(1)
        }
      } else {
        monMap += (aa -> (hh(i).split("_")(1)))
      }

    }

    var total = 0
    var uuu = ListBuffer[String]()
    for ((k, v) <- monMap) {
      val jiji: Array[String] = v.split(":")
      var ss = ""
      for (i <- 0 to jiji.size - 1) {
        if (ss.equals("")) {
          ss = k + "_" + jiji(i)
        } else {
          ss = ss + ":" + k + "_" + jiji(i)
        }

        total = total + jiji(i).split("=")(1).replaceAll("\\s", "").toInt
      }
      uuu += ss
    }

    //println("TOTAL : "+total)
    //println("uuu : "+uuu)

    //val jffdsds = scala.io.StdIn.readLine()

    var entr = 0.0
    var entropie = 0.0

    for (i <- 0 to uuu.size - 1) {
      val tab = uuu(i).split(":")
      if (tab.length > 1) {
        var sum = 0
        for (k <- 0 to tab.length - 1) {
          sum = sum + (tab(k).split("=")(1)).replaceAll("\\s", "").toInt
        }
        var compte = 0
        for (j <- 0 to tab.length - 1) {
          compte = compte + tab(j).split("=")(1).replaceAll("\\s", "").toInt
          entropie = entropie.toDouble + (-(tab(j).split("=")(1).replaceAll("\\s", "").toInt).toDouble / sum.asInstanceOf[Double]) * calcLog2((tab(j).split("=")(1).replaceAll("\\s", "").toInt / sum.asInstanceOf[Double]).toDouble)
        }
        entropie = entropie.toDouble * (sum.toDouble / total.toDouble)
        entr = entr.toDouble + entropie.toDouble
        entropie = 0.0
      }
    }

    //println("ENTROPIE : "+entr)
    //scala.io.StdIn.readLine()
    // println("list :: "+uuu)


    return entr
  }


  def isParsableAsDouble(s: String) = try {
    s.toDouble
    true
  } catch {
    case numberFormatException: NumberFormatException =>
      false
  }

}
