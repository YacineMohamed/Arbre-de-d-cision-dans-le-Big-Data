package Classifier

import scala.collection.mutable.ListBuffer

object TraitementNoeud {


/****NoeudCreation(param) :****/
/*/
Cette methode va transformer notre modele d'une liste de texte en une liste de noeud
ayant une structur permetant la classification
 */
/****FIN****/
  def NoeudCreation(listTexte:List[String]): ListBuffer[Noeud]={
    val listNoeud:ListBuffer[Noeud] = ListBuffer[Noeud]()
/*
Chaque ligne de notre modele contient une acheminement de la racine a la feuille
nous allons donc parcourir les different noeud en parsant.
le premier element de chaque ligne represente la racine, le dernier reposente uen feuille
 */
    for(i<-0 to listTexte.size-1) {

      val feuille = listTexte(i).split("#")(1)
      val a: List[String] = listTexte(i).split("#")(0).split(":").toList
      for (j <- 1 to a.size - 1) {
        var typee =""
        if(j==1){
          //si nous parcourant la premier part du split => c est une racine
          typee = "racine"
        }else{ // sinon si ce n'est pas le premeir parcours,  cest un noeud interee
          typee = "local"
        }
        val n = new Noeud()
        val k = j + 1
        var fils =""

        n.ID = a(j).split("=")(0)
        n.TYPE = typee
        val lienn = a(j).split("=")(1)

        if (k <= a.size - 1) {
          fils = a(k).split("=")(0)
        } else {
          fils = feuille
        }
        n.LIEN += lienn -> fils
        // listNoeud += n

        if(NoeudVerif(n.ID,listNoeud)){
          /*verification si le neodu existe deja (a travers son ID)
          si c'est oui, il sera pas crée mais sera plutot modifié, en ajoutant le noeud filse
          a sa hashMap LIEN
          */
          NoeudUpdate(n.ID,lienn,fils,listNoeud)
        }else{
          //si le noedu n'existe pas, il sear ajouté a la liste des noeud !!!
          listNoeud += n
        }

      }
    }


    return listNoeud
  }



  def afficheRésult(libele:String,instance:List[String]): Unit ={
    //  println("La classe de l'instance : ")
    for(i<-0 to instance.size-1){
      print(instance(i)+" ")
    }
    print("["+libele+"]")
    println()
  }



  def NoeudVerif(ID:String, listNoeud:ListBuffer[Noeud]): Boolean ={
    var test = false
    for(i <- 0 to listNoeud.size-1){
      if(listNoeud(i).ID.equals(ID)){
        test = true
      }
    }

    return test
  }



  def NoeudUpdate(ID:String, lien:String, fils:String, listNoeud:ListBuffer[Noeud]): Unit ={

    var pos = 0
    for(i<-0 to listNoeud.size-1){
      if(listNoeud(i).ID.equals(ID)) pos = i
    }

    listNoeud(pos).LIEN += lien -> fils

  }


  def NoeudAffiche(listNoeud:ListBuffer[Noeud]): Unit ={
    for(i<-0 to listNoeud.size-1) {
      print(".. ID ="+listNoeud(i).ID)
      print(".. TYPE ="+listNoeud(i).TYPE)
      print(".. LIEN ="+listNoeud(i).LIEN)
      println()
    }
  }

}
