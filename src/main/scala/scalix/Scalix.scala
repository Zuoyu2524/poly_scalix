package scalix

import org.json4s.*
import org.json4s.native.JsonMethods.*

import java.io.File
import scala.io.Source
import java.net.URLEncoder

implicit val formats: Formats = DefaultFormats
import java.io.PrintWriter

object Scalix extends App {
  val api_key = "8ccb201db499a824723ee857c02288f1"
  val url1 = s"https://api.themoviedb.org/3/movie/157336?api_key=$api_key"
  val url2 = s"https://api.themoviedb.org/3/search/person?query=Tom+Cruise&api_key=$api_key"
  val source1 = Source.fromURL(url1)
  val source2 = Source.fromURL(url2)
  val contents = source2.mkString
  source2.close()
  //println(contents)
  val json = parse(contents)
  val id = findActorId(name = "Tom", surname = "Cruise")
  //print(id)
  val movies = findActorMovies(actorId = 500)
  //print(movies)
  val directors = findMovieDirector(343611)
  //print(directors)
  val col = collaboration(FullName("Scarlett", "Johansson"), FullName("Chris", "Evans"))
  //println(col)
  findActorMovies_v2(500)

  def findActorId(name: String, surname: String): Option[Int] =
    val query = s"${URLEncoder.encode(surname, "UTF-8")},${URLEncoder.encode(name, "UTF-8")}"
    val url = s"https://api.themoviedb.org/3/search/person?api_key=$api_key&query=$query"
    val source = Source.fromURL(url)
    val contents = source.mkString
    source.close()
    val json = parse(contents)
    // Examinez les résultats de la recherche
    val results = (json \ "results").children

    if (results.nonEmpty) {
      // Prenez le premier résultat de la recherche
      val actorId = (results.head \ "id").extract[Int]
      Some(actorId)
    } else {
      None
    }

  def findActorMovies(actorId:Int):Set[(Int,String)] =
    val url = s"https://api.themoviedb.org/3/person/$actorId/movie_credits?api_key=$api_key"
    val source = Source.fromURL(url)
    val contents = source.mkString
    source.close()
    val json = parse(contents)

    val movieCredits = (json \ "cast").children
    //println(movieCredits)

    val movieSet = movieCredits.map { movie =>
      val movieId = (movie \ "id").extract[Int]
      val movieTitle = (movie \ "title").extract[String]
      (movieId, movieTitle)
    }.toSet
    movieSet

  def findMovieDirector(movieId: Int): Option[(Int, String)] = {
    val url = s"https://api.themoviedb.org/3/movie/$movieId/credits?api_key=$api_key"
    val source = Source.fromURL(url)
    val contents = source.mkString
    source.close()
    val json = parse(contents)
    // extract the value for "job" parameter
    val directors = (json \ "crew").children.filter{ crew =>
      (crew \ "job").extractOpt[String].contains("Director")
    }
    //println(directors)
    val directorSet = directors.headOption.map { director =>
      val directorId = (director \ "id").extract[Int]
      val directorName = (director \ "name").extract[String]
      (directorId, directorName)
    }
    directorSet}

  case class FullName(name: String, surname: String)

  def collaboration(actor1: FullName, actor2: FullName): Set[(String, String)] =
    val actorId1 = findActorId(actor1.name, actor1.surname)
    val actorId2 = findActorId(actor2.name, actor2.surname)
    val actorId1Value: Int = actorId1.getOrElse {
      throw new IllegalArgumentException("actor dose not exist")
    }
    val actorId2Value: Int = actorId2.getOrElse {
      throw new IllegalArgumentException("actor dose not exist")
    }
    val collaborationSet = for(
      movie1 <- findActorMovies(actorId1Value);
      movie2 <- findActorMovies(actorId2Value);
      if(movie1._1 == movie2._1);
        director <- findMovieDirector(movie1._1)
    ) yield(director._2, movie1._2)
    collaborationSet

  //def findActorId_v2(name: String, surname: String): Option[Int] =


  def findActorMovies_v2(actorId:Int): Set[(Int,String)] =
    //un check if actor already in data
    //if no, find actor id, and add it to the data
    //if yes, read and return it
    val filename = s"src/data/actors/actor$actorId.json"
    val file = new File(filename)

    if(file.exists()){
      val contents2 = Source.fromFile(filename).mkString
      new Gson().fromJson(contents2, Set[(Int,String)])
    }
    else{
      val url = s"https://api.themoviedb.org/3/person/$actorId/movie_credits?api_key=$api_key"
      val source = Source.fromURL(url)
      val contents = source.mkString
      source.close()
      val json = parse(contents)

      val movieCredits = (json \ "cast").children
      //println(movieCredits)

      val movieSet = movieCredits.map { movie =>
        val movieId = (movie \ "id").extract[Int]
        val movieTitle = (movie \ "title").extract[String]
        (movieId, movieTitle)
      }.toSet
      val out = new PrintWriter(filename)
      out.write(json)
      out.close()
      movieSet}







}

//code -> reformatage