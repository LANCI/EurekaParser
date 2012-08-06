package ca.uqam.lanci.eurekaparser

import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.InputStreamReader
import java.io.OutputStreamWriter
import java.io.PrintWriter

import scala.Array.canBuildFrom

import javax.swing.text.html.HTML.Attribute
import javax.swing.text.html.HTML.Tag
import javax.swing.text.html.HTMLEditorKit
import javax.swing.text.MutableAttributeSet

case class Article(
  val fileName: String,
  val noDocument: String,
  val source: String,
  val dtPublication: String,
  val auteurs: String,
  val titre: String,
  val texte: String) {
  override def toString = Seq(
    "Fichier: " + fileName,
    "No:      " + noDocument,
    "Source:  " + source,
    "Date:    " + dtPublication,
    "Titre:   " + titre,
    "Auteurs: " + auteurs,
    "Texte:   " + texte)
    .mkString("\n")
}

class Parser(val file: java.io.File) extends HTMLEditorKit.ParserCallback {
  private val _articles = collection.mutable.Buffer[Article]()
  def articles = Seq(_articles: _*)
  private val sep = ';'
  abstract sealed class State
  case object Source extends State
  case object Titre extends State
  case object Texte extends State
  case object Auteurs extends State
  case object NoDocument extends State
  case object Ignore extends State
  case object Neant extends State
  var currentState: State = Neant
  private val builders = Map[State, StringBuilder](
    Titre -> new StringBuilder,
    Source -> new StringBuilder,
    Texte -> new StringBuilder,
    Auteurs -> new StringBuilder,
    NoDocument -> new StringBuilder)
  def append(str: String) {
    val s = str.trim + (if (currentState == Auteurs) sep else " ")
    builders.get(currentState).foreach(_.append(s))
  }
  private def reset() { builders.values.foreach(_.clear()) }
  private def article: Article = Article(
    fileName = file.getPath,
    titre = builders(Titre).result.trim,
    source = builders(Source).result.trim,
    texte = builders(Texte).result.trim,
    auteurs = builders(Auteurs).result.trim.dropRight(1), // drop the final ';'
    dtPublication = {
      val D = ".*([0-9]{4})([0-9]{2})([0-9]{2}).*".r
      builders(NoDocument).result match {
        case D(y, m, d) => y + "-" + m + "-" + d
        case _ => throw new Exception("Pas capable d'extraire la date!")
      }
    },
    noDocument = builders(NoDocument).result.trim)
  def pushCurrentArticle() { _articles += article; reset }

  val ignoreCues = Set("À bien y penser", "[À BIEN Y PENSER]", "Réplique", "Dialogue", "Éditoriaux",
    "Éditorial", "Appel à tous", "Lettre de la semaine", "La boîte aux lettres",
    "Chronique", "Rencontre", "Critique", "Opinion", "Télévision",
    "Radio", "Analyse", "Sondage CROP - La Presse", "Post-scriptum",
    "Entracte", "Éducation", "Libre opinion", "En bref...", "Lettres",
    "En bref", "Société", "Perspectives", "Livres", "Médias",
    "Technologie", "Essais québécois", "Théâtre", "Tête-à-tête",
    "Essais", "En couverture", "Humour", "PC", "Presse Canadienne",
    "AFP", "Reuters", "France-Presse, Agence")

  private var lastTagSeen: Option[Tag] = None

  override def handleStartTag(tag: Tag, attributes: MutableAttributeSet, position: Int) {
    lastTagSeen = Some(tag)
    def hasClass(c: String) = attributes.containsAttribute(Attribute.CLASS, c)
    currentState = tag match {
      case Tag.SPAN =>
        if (hasClass("DocPublicationName")) Source
        else if (hasClass("DocHeader")) Neant
        else if (hasClass("TitreArticleVisu")) Titre
        else currentState
      case Tag.P if currentState == Auteurs => Texte // il y a toujours <p></p> avant le texte
      case _ if currentState == NoDocument => pushCurrentArticle(); Neant
      case _ => currentState
    }
  }

  override def handleSimpleTag(tag: Tag, attributes: MutableAttributeSet, position: Int) {
    if (currentState == Titre && tag == Tag.BR && lastTagSeen == Some(Tag.BR)) // deux <BR> avant les auteurs
      currentState = Auteurs
    lastTagSeen = Some(tag)
  }

  override def handleText(text: Array[Char], position: Int) {
    val NoDoc = "Numéro de document : (.*)".r
    val Copyright = ".*©.*".r
    val Email = "(?i)^[A-Z0-9._%+-]+@[A-Z0-9.-]+(\\.[A-Z]{2,4})?$".r
    val strToAppend = new String(text) match {
      case _ if currentState == Ignore => None
      case str if ignoreCues contains str => currentState = Ignore; reset; None
      case NoDoc(no) => currentState = NoDocument; Some(no)
      case "Collaboration spéciale" => None
      case Copyright() => currentState = Neant; None
      case Email() => currentState = Neant; None
      case "Illustration(s) :" => currentState = Neant; None
      case str => Some(str)
    }
    strToAppend.foreach(append)
  }

  new javax.swing.text.html.parser.ParserDelegator()
    .parse(new InputStreamReader(new FileInputStream(file), "ISO-8859-1"), this, true)
}

object EurekaParserApp {
  val files = new java.io.File("data/").listFiles
    .filter(_.getName().toLowerCase().endsWith(".html"))
  def main(args: Array[String]) {
    val articles =
      files.flatMap(new Parser(_).articles)
        .filter(_.texte.nonEmpty)
        .filter(_.auteurs.length < 50)
    val out = new PrintWriter(new OutputStreamWriter(new FileOutputStream("etudiants.txt"), "UTF-8"))
    try { out.print(articles.mkString("\n===\n")) }
    finally { out.close }

    println("Total: " + articles.size)
  }
}
