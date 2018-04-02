import java.io.IOException
import java.lang.Exception

import org.apache.log4j.Logger
import Classifier.Classifier
import CrelationModel.C45
import com.typesafe.scalalogging.{Logger, slf4j}
import org.apache.spark.{SparkConf, SparkContext}

import scala.util.control.Exception

object Accueil {
val conf = new SparkConf().setAppName("C4.5").setMaster("local")
  val sc = new SparkContext(conf)

 def main (args:Array[String]): Unit ={
   /****Creation du model****/

      val fichierEntee = "fichier/texte.txt"
      val dossierSortie = "fichier/Resultat"
      val separateurAttribut = " "
      val entete ="1"
      val ID = "1"
      val params:Array[String] =
        Array(fichierEntee,dossierSortie,separateurAttribut,entete,ID)
            //Les parametre seront recupéré a travers l'interface graphique!
      try{
            C45.main(sc,params) // lancement de la classe C45 afin de crée le model
            println("Model crée avec succes !! ")
      }catch {
            case e:Exception=> e.printStackTrace()
      }
    /*****FIN*****/
   println("cliquer sur n importe quel touche afin de classer les nouvelles instances")
   scala.io.StdIn.readLine()
   //Cet interaction avec l'user est temporaire juste pour séparer les deux traitement


   /****Analyser de  nouvelles instances ****/

      val fichierModel=dossierSortie+"/part-00000"
      val fichierClassif="fichier/texteClassif.txt"
      val paramss:Array[String] = Array(fichierModel,fichierClassif)
        //La aussi les parametre sont momentanément statique, il eront récupéré via l'interface graphique.
      Classifier.main(sc,paramss) //
       // lancement de la classe Classifier afin de classer de nouvelles isntances!

   /****FIN***/

 }



}
