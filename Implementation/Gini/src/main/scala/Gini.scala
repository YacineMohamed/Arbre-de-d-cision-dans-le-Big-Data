import org.apache.spark.{SparkConf, SparkContext}
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.io.Path

object Gini {
  var separateur=""
  val conf = new SparkConf().setAppName("C4.5").setMaster("local")
  val sc = new SparkContext(conf)
  val inputPath = "fichier/texte2.txt"
  val outputPath = "fichier/Resultat"
  var resultat: ListBuffer[String] = ListBuffer[String]()
  def main(args: Array[String]): Unit = {

    val texte = sc.textFile(inputPath)

    val ensembleTraitée:List[String] = preparerEnsemble(texte.collect().toList)

    val ensembreStructuré:Map[String,String] = structureEnsemble(ensembleTraitée)

    println(""+ensembreStructuré)
    constructionArbre("root",ensembreStructuré)
    enregistreArbre()

  }

  def enregistreArbre(): Unit ={
    if (scala.reflect.io.File(scala.reflect.io.Path(outputPath)).exists) {
      val jj: Path = Path(outputPath)
      jj.deleteRecursively()
      sc.parallelize(resultat).saveAsTextFile(outputPath)
    }else{
      sc.parallelize(resultat).saveAsTextFile(outputPath)
    }

  }

  def constructionArbre(noeud: String,  ensemble: Map[String, String]): Unit = {

    val verifiHomogeneite: mutable.HashMap[String, Int] = mutable.HashMap[String, Int]()

    for ((k, v) <- ensemble) {
        verifiHomogeneite += ""+k.split("_")(1)->1
    }
    if (verifiHomogeneite.size == 1) {
      var valeurClasse = ""
      for ((k, v) <- verifiHomogeneite) {
        valeurClasse= k
      }
      resultat += noeud+ "["+valeurClasse+"]"
    }
    else {
      val t: mutable.HashMap[String, Int] = mutable.HashMap[String, Int]()
      var total = 0


      val listGini: mutable.HashMap[String, Double] = mutable.HashMap[String, Double]()

      for ((k, v) <- ensemble) {
        val a = k.split("_")(0)
        var compteur = 0
        for ((kk, vv) <- ensemble) {
          if (!k.equals(kk)) {
            if (kk.split("_")(0).equals(a)) {
              compteur += vv.split(",").toList.size
            }
          }
        }
        listGini += k -> Calcul2.Gini(v, compteur)

      }
      var min = 1.0
      var test = ""
      var nombreInstance=0
      for ((k, v) <- listGini) {
        if (v <= min) {
          if(v<min){
          min = v
          test = k
        }
        }
      }
    division(noeud, test,ensemble)
    }
  }

  def listContien(element:String, list:List[String]): Int ={
    var test = 0

    for(i<-0 to list.size-1){
      if(list(i).equals(element)) test = 1
    }

return test
  }

def division(noeud:String, test:String, ensemble:Map[String,String]): Unit ={
  var l = ""
for((k,v)<-ensemble){
  if(k.equals(test)){
    l = v
  }
}
  println(" v :"+l)
  val ListVrai:List[String] = l.split(",").toList

  val ttt:ListBuffer[String] = ListBuffer[String]()
  for(index<-0 to 1) {
    var n=""
    val sousEnsemble:mutable.HashMap[String,String] = mutable.HashMap[String,String]()
    for ((k, v) <- ensemble) {
      val g: List[String] = v.split(",").toList
      for (i <- 0 to g.size - 1) {
        if (listContien(g(i), ListVrai)==index) {
          if (sousEnsemble.contains(k)) {
            sousEnsemble(k) = sousEnsemble(k) + "," + g(i)
          } else {
            if (!k.equals(test)) {
              sousEnsemble += k -> g(i)
            }
          }
        }
      }
    }

    if(index==0){
      n=noeud+":"+test.split("_")(0)+"=Faux"
    }else{
      n=noeud+":"+test.split("_")(0)+"=Vrai"
    }
   // println(index +" :  " + sousEnsemble)
    constructionArbre(n,sousEnsemble.toMap)
  }
}


  def structureEnsemble(listTexte: List[String]): Map[String,String] = {
    println("__________________________________________________")
    val nombreAttribut =
      (listTexte.take(1).toList.flatMap(line => line.split(" ")).size) - 1

    val nombreInstance = listTexte.size-1

    val map:mutable.HashMap[String,String] = mutable.HashMap[String,String]()


    for(i<-0 to nombreInstance){
      var l: Array[String] = listTexte(i).split(" ")
      for(j<-0 to nombreAttribut-1){

        if(map.contains(j+":"+l(j)+"_"+l(nombreAttribut))){

         map(j+":"+l(j)+"_"+l(nombreAttribut)) = map(j+":"+l(j)+"_"+l(nombreAttribut))+","+i
        }else{
          map += (j+":"+l(j)+"_"+l(nombreAttribut))->(i+"")
        }
      }
    }


    return map.toMap

  }



  def preparerEnsemble(listTexte:List[String]): List[String]={

    println("******** L'ensemble d'apprentissage contient une entete ******** ?\n Oui : 1\n Non : 0")
    println("************************ Reponse : ")
    var entete = scala.io.StdIn.readInt()

    if(entete!=1 && entete != 0){
      println("Reponse éronée, veuillez saisir un des choix proposés")
      preparerEnsemble((listTexte))
    }

    var listRes:ListBuffer[String] = ListBuffer[String]()
    // List qui va contenir les résultat des modification au fur et a mesure des tests

    separateur = sep()

    if(separateur.equals("Innexistance")){
      println("Choix innexistant !! veuillez reessayer")
      separateur = sep()
    }else{
      var listB:ListBuffer[String] = ListBuffer[String]()
      for(i<-entete to listTexte.size-1) {
        listB += listTexte(i).toString().replaceAll(separateur, " ")
      }
      listRes=listB
    }

    //TraitAttDisc

    val attNumPos:mutable.HashMap[Int,Int] = Calcul2.verifNumeric(listRes)
    /*
    Une map qui va contenir le résultat de la fonction : verifiNumeric
    Cette fonction aura comme tache de dire si oui ou non il existe un attribut continus
    Entré : lise de données
    Sortie : Une Map de type clé valeur, ou
      la clé : correspond a la position de l'attribut continu detecté
      valeur : le nombre d'instance ou la valeur de cette attribut est numérique :
        PS : si l'attribut n'est pas numérique ne serait ce qu'au nieau d'une seul instance
          il sera considéré comme discret !!!! CAR :
              Sa discretisation generera une erreur (au niveau de la ligne non numérique)
     */
    var listeIntermediaire:ListBuffer[String] = listRes
    //Le cas ou il existe des attributs continus
    if(attNumPos.size>0){

      var array: Array[String] = listRes.toArray
      val arrayBuffer:ArrayBuffer[String] =ArrayBuffer[String]()
      for ((k, v) <- attNumPos) {
        //parcours de la map contenant les position de l'att numerique(comme clé, et comme valeur elle contein
        //le nombe d instance ou cet att est num

        //verification que l'attr a bien eté num au niv de toutes les instances !!!
        if (v == listTexte.size) {


          var list2:ListBuffer[String] = ListBuffer[String]()
          var hh = new mutable.HashMap[Int, Int]()
          for (i <- 0 to array.size - 1) {
            val u: Array[String] = array(i).split(" ")
            hh += (u(k).toInt -> 1)
          }
          list2 = Calcul2.TraitAttDisc(listeIntermediaire, hh,k)
          listeIntermediaire.clear()
          listeIntermediaire=list2
        }
      }
      listRes.clear()
      listRes = listeIntermediaire
    }

    val listTexte2:List[String] = listRes.toList
    return listTexte2
  }


  def sep(): String ={
    var sep=""

    println("* Veuillez tapper le N° correspondant au séparateur d'attributs utilisé:")
    println("* 1 : L'espace( )\n* 2 : La virgule( , )\n* 3 : Le tiré ( - )\n* 4 : L'underscore ( _ )\n* 5 : Autres")
    println("************************ Reponse : ")
    val rep = scala.io.StdIn.readLine()
    rep match {
      case "1" => sep=" "
      case "2" => sep = ","
      case "3" => sep = "-"
      case "4" => sep = "_"
      case "5" => {
        println("************************ Veuillez saisir votre séparateur :")
        sep = scala.io.StdIn.readLine()
      }
      case default => sep ="Innexistance"
    }
    return sep
  }




}