package Traitement

import org.apache.spark.SparkContext

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object TraitementDonnees {

  def structureEnsemble(sc:SparkContext,listTexte: List[String]): Map[String,String] = {
    val nombreAttribut =
      (listTexte.take(1).toList.flatMap(line => line.split(" ")).size) - 1
    val nombreInstance = listTexte.size-1
    val jj = ListBuffer[String]()
    val listConcat = ListBuffer[String]()
    for (i <- 0 to (nombreInstance)) {
      var l: Array[String] = listTexte(i).split(" ")
      for (j <- 0 to (nombreAttribut-1)) {
        listConcat += j + ":" + l(j) + "_" + l(l.length - 1)
      }
    }
    /*
    * listConcat conteniens la concaténation de toutes les valeur d'attribut, leurs N° de lignes
    * ainsi que la catégoriee de classe qui leurs est associée, dans la structure suivante :
    * (numero_de_ligne:valeur_attribut_catégorie_de_classe)
    *
    */
    val myMAP = sc.parallelize(listConcat.toList).
      map(word => (word.split(":")(0), word.split(":")(1))).
      reduceByKey(_ + "," + _).collect.toMap.toSeq.sortBy(_._1).toMap

    /*
    Une MAP(clé,valeur) qui va etre chargé a partir e la liste préceente (listConcat)
    Clé : Le numero de lignes
    Valeur : valeurAttribut_categorieClasse
    CEtte map est trié par rapport a la clé, afin e garntir que les attributs sont tiés selon
    leur vériable possition dans le jeux de données !!

     */

    // trat("Root", "-1", myMAP)
    return myMAP
  }

  /****preparerEnsemble(param1,param2,param3,param4)****/
      /*Cette methode assure les traitments suivants :
            1)discrétisation des attributs continus (int ou double).
            2)Remplacement de du sépérateur utilisé en <espace>.
            3)Suppression de l entete si elle existe.
            4)Suppression des identificateur des instances si ils éxistent.
         Elle prend en entré la liste de données tel qu'elle a été ajouté par l'user
         et elle renvoie une liste néttoyee prete a un traitement
       */
  def preparerEnsemble(listTexte:List[String],separateur:String,entete:Int,ID:Int): List[String]={

    var listRes:ListBuffer[String] = ListBuffer[String]()
    listRes = traitementEnteteID(listTexte,entete,ID,separateur)
    /*
    * ListRes: une liste qui va contenir le resultat des transformation :
    *   Suppression de l'entete / ID
    *   Remplacment du symbole séparateur par un espace
     */

    val attNumPos:mutable.HashMap[Int,Int] = Calcul2.verifNumeric(listRes)
    /*
    Une map qui va contenir le résultat de la fonction : verifiNumeric
    Cette fonction aura comme tache de tester si oui ou non il existe un attribut continus
    Entré : lise de données
    Sortie : Une Map de type clé valeur, ou
      la clé : correspond a la position de l'attribut continu detecté
      valeur : le nombre d'instance ou la valeur de cette attribut est numérique :
        PS : si l'attribut n'est pas numérique ne serait ce qu'au nieau d'une seule instance
          il sera considéré comme discret !!!! CAR :
              Sa discretisation generera une erreur (au niveau de la ligne non numérique)
     */
    var listeIntermediaire:ListBuffer[String] = listRes


    //Le cas ou il existe des attributs continus
    if(attNumPos.size>0){
      var array: Array[String] = listRes.toArray
      val arrayBuffer:ArrayBuffer[String] =ArrayBuffer[String]()
      for ((k, v) <- attNumPos-1) {
        //parcours de la map contenant les position de l'att numerique(comme clé, et comme valeur elle contein
        //le nombe d instance ou cet att est num
        //verification que l'attr a bien eté num au niv de toutes les instances !!!
        if (v == listRes.size) {
          var list2:ListBuffer[String] = ListBuffer[String]()
          var hh = new mutable.HashMap[Double, Int]()
          for (i <- 0 to array.size - 1) {
            val u: Array[String] = array(i).split(" ")
            hh += (u(k).toDouble -> 1)
          }
          list2 = Calcul2.TraitAttDisc(listeIntermediaire, hh,k)
          listeIntermediaire.clear()
          listeIntermediaire=list2
        }
      }
      listRes.clear()
      listRes = listeIntermediaire
    }
    // val listTexte2:List[String] = listRes.toList
    // println("listR"+listRes)
    return listRes.toList
  }







  /****TraitementEnteteID****/
  /*cette methode prend comme entré la list fourni par l'user, et renvois une liste
  traité, qui ne contien ni entete, ni ID de ligne, avec un séparateur <espace>
  */
  def traitementEnteteID(listTexte:List[String],entete:Int,ID:Int,sepateur:String): ListBuffer[String] ={
    val listR:ListBuffer[String] = ListBuffer[String]()
    for(i<-entete to listTexte.size-1){
      //parcours de la list initail (vercalement) de la valeur entete
      //si elle existe, cette valeur sera egal a 1, on va dnc la supprimé, sinn le parcours
      //commence de zero.
      var a = ""
      val aa:List[String] = listTexte(i).split(sepateur).toList
      for(j<-ID to aa.size-1){
        //meme principe pr l'ID lors du parcours horizontal
        if(a.equals("")){
          a = aa(j)
        }
        else{
          a +=" "+aa(j)}
      }
      listR+=a
    }
    return listR
  }

}
