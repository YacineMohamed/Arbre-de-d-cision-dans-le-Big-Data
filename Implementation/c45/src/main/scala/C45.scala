import org.apache.spark.{SparkConf, SparkContext}
import java.util.logging.Logger
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.io.Path
import org.slf4j.{Logger, LoggerFactory}
import com.typesafe.scalalogging._

object C45  extends LazyLogging {

  var separateur=""
  val conf = new SparkConf().setAppName("C4.5").setMaster("local")
  val sc = new SparkContext(conf)
  val inputPath = "fichier/texte.txt"
  val outputPath = "fichier/Resultat"
  var resultat: ListBuffer[String] = ListBuffer[String]()

  /**
    * Fonction appellée au lancement du programme
    */
  def main(args: Array[String]): Unit = {

    val texte = sc.textFile(inputPath) // un RDD qui contient l ensemble de données tel quel
    val listTexte: List[String] = texte.collect().toList // tranformation de l'nsemble sous forme d'une liste


    val ensembleTraitée:List[String] = preparerEnsemble(texte.collect().toList)
        /*préparation de l'ensemble de données :
             1)discrétisation des attributs continus.
             2)Remplacement de du sépérateur utilisé en <espace>.
             3)suppression de l entete si elle existe.
        */

    val ensembreStructuré:Map[String,String] = structureEnsemble(ensembleTraitée)
    /*
    /StrictireEnseùbme :
             Une fonction qui va transformé notre ensemble de données, d'une liste a une MAP(clé,valeur)
             ou :
             1) La clé : represente le numero de ligne
             2) La valeur : la valeur de chaque attribut concaténé a la catégorie de classe
                         lui correspond
         ... Les details du comment faire commenté à l'interieur de la fonction ...
     */


      constructionArbre("Root", ensembreStructuré)
    /*
    *Une fois que les données son structuré sous forme de map(c,v),
    * Avec comme parmetre d'entré :
    *       1) Le noeud courant (qui est Root vu que c'est la premiere iteration)
    *       2) L'ensemble de données structuré sous forme d'une map(c,v)
    * C'est une methode itérative, les deux scenario possible sont les suivant :
    *       1) Enseùbme (3eme parametre) est homogene (cas d'une feuille) : le libelé de la classe
    *          sera attribué au noeud, et son developpenet est arreté.
    *       2) L'ensemble heterogene, dans ce cas, une procedure de dvision sera lancé, e
    *          la fonction constrictionArbre sera executé pour chacun des sous ensemble résultat
    *          avec comme parametre leurs noeud ainsi que la position de l'attribut de division.
    *
     */

    enregistreArbre()
  }


  // TODO: commenter cette fonction: que fait-elle, quels sont ses arguments, que renvoie-t-elle ?
  def structureEnsemble(listTexte: List[String]): Map[String,String] = {
    println("__________________________________________________")
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

  // TODO: commenter cette fonction: que fait-elle, quels sont ses arguments, que renvoie-t-elle ?
  def constructionArbre(noeud: String,  ensemble: Map[String, String]): Unit = {

    val verifiHomogeneite: mutable.HashMap[String, Int] = mutable.HashMap[String, Int]()

    for ((k, v) <- ensemble) {
      val l: List[String] = v.split(",").toList
      for (j <- 0 to l.size - 1) {
        verifiHomogeneite += (l(j).split("_")(1) -> 1)
      }
    }
    /*  verifHomogeinité :
    *   Une map(C,V) qui est utilisé afin de verifier l'homogeneité de l'ensemble en entré
    *   Etant donnée que la MAP ne peu contenir deux clé ayant la meme valeur, du coup :
    *   La clé utilisé est la catégorie de classe de chaque instance,
    *   Si au finale, la MAP est de longueur=1, ça veut dire qu'il n y a qu'une seule classe pour
    *   toutes les instance => ensemble HOMOGENE !!!
    */

    //Le cas ou l'ensembe est pas homogene :
       if (verifiHomogeneite.size == 1) {
         /*nous recuperons la valeur de classe, qui, une fois dans cette condition,
         est certainement la meme pour tous les attributs
         */
         var valeurClasse = ""
         for ((k, v) <- verifiHomogeneite) {
           valeurClasse= k
         }

         resultat += "" + noeud + "=" + "->[" + valeurClasse + "]"
         //resultat est une List global
         /*
         A laquelle sera ajouté chaque acheminement menant de la racine a la feuille.
          */
       }//Le cas ou l'ensemble est heterogene
       else {
         val entropieNoeud = Calcul2.calculeEntropieClasses(ensemble)
         //Calcule de l'entropie de l'enseùbme de donnée relatif au noeud actuel

         var listRatio: mutable.HashMap[Int, Double] = mutable.HashMap[Int, Double]()
         for ((k, v) <- ensemble) {
           val valeurAttribut: List[String] = v.split(",").toList
           listRatio += (k.toInt -> (Calcul2.gainRatio(entropieNoeud.toDouble,valeurAttribut).toDouble))
         }
         /*
        Map(C,V) qui contenient :
          - Clé : position de l'attribut
          - Valeur : son rapport ed gain de l'attribut
         */
         var max = 0.0
         var pos = 0
         for ((k, v) <- listRatio) {
           if (max < v) {
             max = v
             pos = k
           }
         }
         /*
              Le parcours de la map listRatio en recherchant la valeur maximal,
              qui correpsond a l'attribut de division
          */

         divisionEnsemble(noeud, pos, ensemble)
         /*Diviser l'ensemble de donnéé actuel par rapport a l'attribut de division choisi
         grace au rapport de gain
         La méthode divisionEnsemble prend en entré trois parametres :
            1) Noeud : afin de garder la trace du noeud (partant de la valeur 'root'!)
            2) pos : la position de l'attribut de division choisi,
            3) ensemble : l'ensemble actuel

         PS: Les deux methode, construireArbre et divisionEnsemble vont se generer autant de fois
         qu'il y a d'ensemble heterogene !
         */
    }
  }

  // TODO: commenter cette fonction: que fait-elle, quels sont ses arguments, que renvoie-t-elle ?
  def enregistreArbre(): Unit ={
    if (scala.reflect.io.File(scala.reflect.io.Path(outputPath)).exists) {
      val jj: Path = Path(outputPath)
      jj.deleteRecursively()
      sc.parallelize(resultat).saveAsTextFile(outputPath)
    }else{
      sc.parallelize(resultat).saveAsTextFile(outputPath)
    }

  }


  // TODO: commenter cette fonction: que fait-elle, quels sont ses arguments, que renvoie-t-elle ?
  def divisionEnsemble(noeud: String, pos: Int, ensemble: Map[String, String]): Unit = {
    val nouvelEnsemble: mutable.HashMap[String, String] = mutable.HashMap[String, String]()
    for ((k, v) <- ensemble) {
      if (!k.equals(pos.toString)) {
        nouvelEnsemble.put(k, v)
      }
    }
    /*
    nouvelEnsemble :
    Une map(c,v) tout a fait identique (par sa structure) a la map entrente, sauf qu'elle
    ne contient plus l'attribut choisi en diviison, car ca ne va servir a rien de le calculer
    une autre fois etant donnée que nous avons diviser pra rapport a lui!!! il aura la meme valeur
    d attribut dans tous les sous ensemble resultant
     */

    val valAttDivision: List[String] = ensemble(pos.toString).split(",").toList
//  cette liste contiendra toutes les valeur de l'attribur de division, elle servira a la map suivante:

    val valeurEtLigne: mutable.HashMap[String, String] = mutable.HashMap[String, String]()
    for (i <- 0 to valAttDivision.size - 1) {
      if (valeurEtLigne.contains(valAttDivision(i).split("_")(0))) {
        valeurEtLigne(valAttDivision(i).split("_")(0)) = i + "," + valeurEtLigne(valAttDivision(i).split("_")(0))
      } else {
        valeurEtLigne.put(valAttDivision(i).split("_")(0), i + "")
      }
    }
    /*
      valeurEtPos est une MAP(c,v) qui comme son nom l'indique, elle contient toutes
      la valeur ainsi que le **NUMERO** de ligne de chacune des valeur de l'attribur,  why ? :
        Elle servira à stocker la ligne de chaqune des valeur distincte, afin de pouvoir créer
        les sous ensmble suivant chacun des ligne correspondante a une valeur d'attribut,
        Elle aura comme valeur :
        Clé : catégorie de classe
        Valeur :  Les lignes separées par une virgule.
       */

    //Creation des sous ensembles, Rappel(par rapport au N°ligne correspondant a chaque valeur d'attribur)
    for ((k, v) <- valeurEtLigne) {
      //Les commentaires à l'inteireur de la boucle a eviter, utilises juste pr expliquer pr une premeire fois.

      val lesLigne: List[String] = v.split(",").toList
      /* lesLigne: une liste qui contiendra toutes les ligne de la valeur d'attribut k
         on les a spliter par rapport a la virgule pour créer la liste
      */

      val sousEnsemble: mutable.HashMap[String, String] = mutable.HashMap[String, String]()
      /*
       sousEnsemble est la map(c,v) qui contiendra chacun des sous ensemble correspondant
      à la valeur d'attribut k
      elle aura la meme structure que toute les map utilisé pour stocker les donnée structuré:
          Clé : numero de ligne
          Valeur: valeurAttribut_CatégorieDeClass
      */
      for (i <- 0 to lesLigne.size - 1) {
          // parcours des valeur de ligne que nous allons attribuer a ce sous enseùbme
        for ((kk, vv) <- nouvelEnsemble) {
          // parcours de notre ensemble de données actuel

          if (sousEnsemble.contains(kk + "")) {
            sousEnsemble(kk + "") = sousEnsemble(kk + "") + "," + vv.split(",")(lesLigne(i).toInt)
          } else {
            sousEnsemble.put(kk + "", vv.split(",")(lesLigne(i).toInt))
          }
        }
      }

      constructionArbre(noeud + ":" + pos + "=" + k, sousEnsemble.toMap)
      /*reExecution de la methode constructionArbre avec comme parametres:
        1) Noeud, que nous avons recuperé en entré, auquel nous ajoutant la position de l'attribur de division
          de l'ensemble d'etnré, ainsi que la valeur qui pour laquelle a été crée ce sous ensmemble !!
        2) Le sous ensemble de données

       PS : ça sera éxecuté k fois (suivant les k valeur de notre attribut de division!!
      */

    }
  }

  // TODO: commenter cette fonction: que fait-elle, quels sont ses arguments, que renvoie-t-elle ?
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

  // TODO: commenter cette fonction: que fait-elle, quels sont ses arguments, que renvoie-t-elle ?
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
//TraitAttDisc
}
