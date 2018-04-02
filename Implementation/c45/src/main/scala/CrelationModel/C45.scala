package CrelationModel

import Traitement.{Calcul2, TraitementDonnees}
import com.typesafe.scalalogging._
import org.apache.spark.{SparkConf, SparkContext}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.io.Path
object C45  extends LazyLogging{

  var resultat: ListBuffer[String] = ListBuffer[String]()
  def main(sc:SparkContext,args: Array[String]): Unit= {

    /****Recupération des parametres ****/
    val input=args(0) // fichier d entre
    var output=args(1) // dossier de sortie
    var separateur=args(2) // separateur d attribut utilise
    var entete=args(3) // l existance d une entete (1 si oui, 0 sinon)
    var ID=args(4) // l existance d un ID d instance (1 si oui, 0 sinon)


    /***Fin***/

    val texte = sc.textFile(input)
    // un RDD qui contient l ensemble de données tel quel


    val ensembleTraitée:List[String] = TraitementDonnees.preparerEnsemble(texte.collect()
                                        .toList,separateur,entete.toInt,ID.toInt)

    val ensembreStructuré:Map[String,String] =
                        TraitementDonnees.structureEnsemble(sc, ensembleTraitée)
    /*
    /StructureEnseùbme :
             Une fonction qui va transformé notre ensemble de données, d'une liste a une
             MAP(clé,valeur), ou :
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

    enregistreArbre(sc,output)
    /*
    *       Une fois que les calculs s'arrete (situation ou tous les sous enesmble
    *       sont homogene, la methode enregistreAbre est appelé afn d'ernetgistré le
    *       resultat dans un fichier texte (précisé par l'utilisateur at ravers le
    *       parametre 'output' )
      */
  }


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

    //Le cas ou l'ensembe est homogene :
       if (verifiHomogeneite.size == 1) {
         /*nous recuperons la valeur de classe, qui, une fois dans cette condition,
         est certainement la meme pour tous les attributs
         */
         var valeurClasse = ""
         for ((k, v) <- verifiHomogeneite) {
           valeurClasse= k
         }
         resultat += "" + noeud +"#"+valeurClasse
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


  def enregistreArbre(sc:SparkContext,output:String): Unit ={
    if (scala.reflect.io.File(scala.reflect.io.Path(output)).exists) {
      val jj: Path = Path(output)
      jj.deleteRecursively()
      sc.parallelize(resultat).saveAsTextFile(output)
    }else{
      sc.parallelize(resultat).saveAsTextFile(output)
    }

  }



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


}