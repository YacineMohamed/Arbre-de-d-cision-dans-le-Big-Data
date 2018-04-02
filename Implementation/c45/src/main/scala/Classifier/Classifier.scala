package Classifier

import org.apache.spark.{SparkConf, SparkContext}

import scala.collection.mutable.ListBuffer

object Classifier {
  def main(sc:SparkContext,args:Array[String]): Unit = {
/****Recuperation des parametres****/
    val input = args(0) // le model (arbre de decision)
    val modelList = sc.textFile(input).collect().toList
    val classif = args(1) // les nouvelles instances a classer
    val classifList = sc.textFile(classif).collect().toList
/****Fin****/

    val listNoeud: ListBuffer[Noeud] = TraitementNoeud.NoeudCreation(modelList)
    //clreation d'une liste de noeud apartir du model

    //bouche afin de classer chaque instance de notre nouveau ensemble de donnée
    for(i<-0 to classifList.size-1){
      classification(classifList(i),listNoeud)
    }
  }

  /****classification(param1,param2)****/
  /*
  *Cette methode permet de transformer le model d une liste de String, en un ensemble
  *de noeud, afin de faciliter la tache de classficiation (en parcourant les noeuds)
  * ion
  */
  /****FIN****/
  def classification(instance: String, listNoeud: ListBuffer[Noeud]): Unit = {
    var ID = ""
    /*Cette variable va contenir l'identifant du noeud racine,
      qui sera utilisé afin de lancer la recherhce afin de lancer la recherche
    */

    val ins: List[String] = instance.split(" ").toList
    for (i <- 0 to listNoeud.size - 1) {
      if (listNoeud(i).TYPE.equals("racine")) ID = listNoeud(i).ID
    }
    /*uen fois que l'identifiant corespondant au noeud racine est determiner,
     La recherche de noeud est entamé
     */
    chercherNoeud(ID, ins, listNoeud)
  }


  def chercherNoeud(id: String, istance: List[String], listNoeud: ListBuffer[Noeud]): Unit = {

    /*
     L'identifiant d'un neud peut etre :
         01) un nombre : La position de l'attribut
         02) une valeur de classe
       Donc cette methode sera executé autant de fois qu'il y a un ID numérique
       jusqu'a troubé un id NON numerique => retourner la valeur de la classe
      */
    if(Traitement.Calcul2.isParsableAsDouble(id)){
     //Cas ou l'id est un numerique => noeud interne

     var valeur = istance(id.toInt)
      /*
      recuperation de la valeur de l'instance correspondante a l'attribut de division
      au niveau du noeud actuel(qui est son ID)
      */

      //recherche de la position du noeud (dans la list de noeud) afin de pouvoir acceder
      // a ses fils par la suite.
     var pos = 0
     for (i <- 0 to listNoeud.size - 1) {
       if (listNoeud(i).ID.equals(id)) {
         pos=i
       }
     }

      /*
      Si la valeur d'attribut de l'instance est numérique, cca veut dire que l'attribut
      en question est continu et à été discrétisé durant la création du model
      du coup, il faudra modifier la vleur acttuel de l'acttuel suivant le seuil utilisé
       */
      if(Traitement.Calcul2.isParsableAsDouble(valeur)){
        val lis:List[String] = listNoeud(pos).LIEN.keySet.toList
        var n=0.0
        if(lis(0).contains("<")) {
          n = lis(0).split("<")(1).toDouble
        }else{
          n = lis(0).split(">")(1).toDouble
        }
        if(valeur.toDouble> n){
          valeur = ">"+n
        }else{
          valeur = "<"+n
        }

      }

      /*une fois que la valeur d'attribut est modifié (ou pas), nous accedon
      Au noeud (via pos), ensuite nous accedons au champ LIEN, etnt donné qu'un noeud
      a plusieurs fils, nous devons donc spcifié quel fils correpond a sa valeur d'attribut
      */
      val idFils  = listNoeud(pos).LIEN.toMap.get(valeur).last
     chercherNoeud(idFils,istance,listNoeud)
      //réexecution de la meme methde, tant que les id sont des noeud interne



    }else{
      /*dans le ca ou l'id du noeud n'est pas numerique, le alcule s'arrete et la valeurs de la classe
      sera retoutnée
      */
      TraitementNoeud.afficheRésult(id,istance)
   }
  }
}
