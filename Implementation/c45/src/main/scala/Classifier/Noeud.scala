package Classifier

import scala.collection.mutable

class Noeud {

  var ID="" // Identifiant du noeud
  var TYPE = "" // Le type du noeud (Racine ou Local)
  var LIEN:mutable.HashMap[String,String]= new mutable.HashMap[String,String]()
  /*HashMap afin de stocker pour chaque noeud :
      Le noeud fils (Valeur)
      La lien qui mene vers ce noeud ! (Clé)
  */

}
