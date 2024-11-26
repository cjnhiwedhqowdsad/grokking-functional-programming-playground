import AttractionOrdering.{ByLocationPopulation, ByName}

import scala.util.Random
import cats.effect.IO

import scala.languageFeature.existentials
import cats.implicits.toTraverseOps

import scala.math.BigDecimal.RoundingMode
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.TimeUnit
import cats.effect.*

import scala.jdk.javaapi.CollectionConverters.asScala

def increment(x: Int): Int = {
  x + 1
}

def wordScore(word: String): Int = {
  word.length()
}

def tipCalculator(names: List[String]): Int = {
  names.size match {
    case x if x > 5 => 20
    case x if x > 0 => 5
    case _ => 0
  }
}

def abbreviate(name: String): String = {
  val initial = name.substring(0, 1)
  val separatorIndex = name.indexOf(' ')
  val remainder = name.substring(separatorIndex + 1)
  initial + ". " + remainder
}

def firstTwo(elements: List[String]): List[String] = {
  elements.slice(0, 2)
}

def lastTwo(elements: List[String]): List[String] = {
  elements.slice(elements.size - 2, elements.size)
}

def movedFirstTwoToTheEnd(elements: List[String]): List[String] = {
  val firstTwo = elements.slice(0, 2)
  val remainder = elements.slice(2, elements.size)
  remainder.appendedAll(firstTwo)
}

def insertedBeforeLast(elements: List[String], element: String): List[String] = {
  val first = elements.slice(0, elements.size - 1)
  val last = elements.slice(elements.size - 1, elements.size)
  first.appended(element).appendedAll(last)
}

def sortAscending(elements: List[String]): List[String] = {
  elements.sortBy(word => word.length)
}

def sortAscendingByLetterS(elements: List[String]): List[String] = {
  elements.sortBy(word => word.count(char => char == 's'))
}

def sortIntDescending(elements: List[Int]): List[Int] = {
  elements.sortBy(int => -int)
}

def sortDescendingByLetterS(elements: List[String]): List[String] = {
  elements.sortBy(word => -word.count(char => char == 's'))
}

def score(word: String): Int = word.replaceAll("a", "").length

def bonus(word: String): Int = if (word.contains("c")) 5 else 0

def penalty(word: String): Int = if word.contains("s") then -7 else 0

def rankedWords(wordScore: String => Int, words: List[String]): List[String] = {
  words.sortBy(wordScore).reverse
}

def filterShorterThan5(words: List[String]): List[String] = {
  words.filter(word => word.length < 5)
}

def filterSCountMoreThan2(words: List[String]): List[String] = {
  words.filter(word => word.count(c => c == 's') > 2)
}

def filterOddNum(numbers: List[Int]): List[Int] = {
  numbers.filter(num => num % 2 != 0)
}

def filterLarger4(numbers: List[Int]): List[Int] = {
  numbers.filter(num => num > 4)
}

def largerThan(number: Int)(value: Int): Boolean = {
  value > number
}

def divisibleBy(number: Int)(value: Int): Boolean = {
  value % number == 0
}

def shorterThan(number: Int)(word: String): Boolean = {
  word.length < number
}

def containsSTimes(number: Int)(word: String): Boolean = {
  word.count(c => c == 's') > number
}

def filterNumbersFn[T](numbers: List[T], filter: T => Boolean): List[T] = {
  numbers.filter(filter)
}

def highScoringWords(wordScore: String => Int)(higherThan: Int)(words: List[String]): List[String] = {
  words.filter(word => wordScore(word) > higherThan)
}

def sum(numbers: List[Int]): Int = {
  numbers.foldLeft(0)((sum, number) => sum + number)
}

def totalLength(words: List[String]): Int = {
  words.foldLeft(0)((sum, word) => sum + word.length)
}

def totalS(words: List[String]): Int = {
  words.foldLeft(0)((sum, word) => sum + word.count(c => c == 's'))
}

def maximum(numbers: List[Int]): Int = {
  numbers.foldLeft(0)((maximum, number) => if maximum > number then maximum else number)
}

case class Book(str: String, authors: List[String]);

def recommendedBooks(friend: String): List[Book] = {
  val scala = List(
    Book("FP in Scala", List("Chiusano", "Bjarnason")),
    Book("Get Programming with Scala", List("Sfregola")))
  val fiction = List(
    Book("Harry Potter", List("Rowling")),
    Book("The Lord of the Rings", List("Tolkien")))
  if(friend == "Alice") scala
  else if(friend == "Bob") fiction
  else List.empty
}

case class Point(x: Int, y: Int)

case class Point3d(x: Int, y: Int, z: Int)

def isInside(point: Point, radius: Int): Boolean = {
 radius * radius >= point.x * point.x + point.y * point.y
}

def validateName(name: String): Option[String] = {
  if (name.size > 0) Some(name) else None
}

def validateEnd(end: Int): Option[Int] = {
  if (end < 3000) Some(end) else None
}

def validateStart(start: Int, end: Int): Option[Int] = {
  if (start <= end) Some(start) else None
}

def validateLength(start: Int, end: Int, minLength: Int): Option[Int] = {
  if end - start >= minLength then Some(end - start) else None
}

case class Event(name: String, start: Int, end: Int)

def parse(name: String, start: Int, end: Int): Option[Event] = {
  for {
    validName <- validateName(name)
    validEnd <- validateEnd(end)
    validStart <- validateStart(start, end)
  } yield Event(validName, validStart, validEnd)
}

def parseLongEvent(name: String, start: Int, end: Int, minLength: Int): Option[Event] = {
    for {
    validName <- validateName(name)
    validEnd <- validateEnd(end)
    validStart <- validateStart(start, end)
    validLength <- validateLength(start, end, minLength)
  } yield Event(validName, validStart, validEnd)
}

case class TvShow(title: String, start: Int, end: Int)

def sortShows(shows: List[TvShow]): List[TvShow] = {
   shows
    .sortBy(tvShow => tvShow.end - tvShow.start)
    .reverse
}

def extractName(rawShow: String): Either[String, String] = {
  val bracketOpen = rawShow.indexOf('(')

  if (bracketOpen != -1) {
    Right(rawShow.substring(0, bracketOpen).trim)
  } else {
    Left(s"Can't extract name from $rawShow")
  }
}

def extractSingleYear(rawShow: String): Either[String, Int] = {
  val dash = rawShow.indexOf('-')
  val bracketOpen = rawShow.indexOf('(')
  val bracketClose = rawShow.indexOf(')')

  for {
    yearStr <- if (dash == -1 && bracketOpen != -1 && bracketClose > bracketOpen + 1) {
      Right(rawShow.substring(bracketOpen + 1, bracketClose))
    } else {
      Left(s"Can't extract single year from $rawShow")
    }
    year <- yearStr.toIntOption.toRight(s"Can't parse $yearStr")
  } yield year
}

def extractYearStart(rawShow: String): Either[String, Int] = {
  val bracketOpen = rawShow.indexOf('(')
  val dash = rawShow.indexOf('-')

  for {
    yearStartString <- if (bracketOpen != -1 && dash > bracketOpen + 1) {
      Right(rawShow.substring(bracketOpen + 1, dash))
    } else {
      Left(s"Can't extract start year from $rawShow")
    }
    yearStart <- yearStartString.toIntOption.toRight(s"Can't parse $yearStartString")
  } yield yearStart
}

def extractYearEnd(rawShow: String): Either[String, Int] = {
  val bracketClose = rawShow.indexOf(')')
  val dash = rawShow.indexOf('-')

  for {
    yearEndString <- if (bracketClose != -1 && bracketClose > dash + 1) {
      Right(rawShow.substring(dash + 1, bracketClose))
    } else {
      Left(s"Can't extract year end from $rawShow")
    }
    yearEnd <- yearEndString.toIntOption.toRight(s"Can't parse $yearEndString")
  } yield yearEnd
}

def parseShow(rawShow: String): Either[String, TvShow]  = {
  for {
    name <- extractName(rawShow)
    yearStart <- extractYearStart(rawShow).orElse(extractSingleYear(rawShow))
    yearEnd <- extractYearEnd(rawShow).orElse(extractSingleYear(rawShow))
  } yield TvShow(name, yearStart, yearEnd)
}

def parseShows(rawShows: List[String]): Either[String, List[TvShow]] = {
  rawShows
    .map(parseShow)
    .foldLeft(Right(List.empty))(addOrResign)
}

def addOrResign(parsedShows: Either[String, List[TvShow]], newParsedShow: Either[String, TvShow]): Either[String, List[TvShow]] = {
  for {
    parsedShow <- newParsedShow
    parsedShows <- parsedShows
  } yield parsedShows.appended(parsedShow)
}

enum YearsActive {
 case StillActive(since: Int)
 case ActiveBetween(start: Int, end: Int)
} 

enum MusicGenre {
 case HeavyMetal
 case Pop
 case HardRock
}

object model {
  opaque type Location = String
  object Location {
    def apply(value: String): Location = value
    extension(a: Location) def name: String = a
  }

  case class Artist(
    name: String, 
    genre: MusicGenre, 
    origin: Location, 
    yearsActive: Set[YearsActive],
  )
}

import model._

def searchArtists(
  artists: List[Artist], 
  genres: List[MusicGenre],
  locations: List[String], 
  searchByActiveYears: Boolean,
  activeAfter: Int, 
  activeBefore: Int
): List[Artist] = {
  artists.filter(artist =>
    (genres.isEmpty || genres.contains(artist.genre)) &&
    (locations.isEmpty || locations.contains(artist.origin.name)) &&
    (!searchByActiveYears || wasArtistActive(artist, activeAfter, activeBefore))
  )
}

case class User(
  name: String, 
  city: Option[String], 
  favoriteArtists: List[String]
)

def userf1(users: List[User]): List[String] = {
  users.filter(_.city.forall(city => city == "Melbourne")).map(_.name)
}

def userf2(users: List[User]): List[String] = {
  users.filter(_.city.exists(city => city == "Lagos")).map(_.name)
}

def userf3(users: List[User]): List[String] = {
  users.filter(_.favoriteArtists.contains("Bee Gees")).map(_.name)
}

def userf4(users: List[User]): List[String] = {
  users.filter(_.city.exists(_.startsWith("T"))).map(_.name)
}

def userf5(users: List[User]): List[String] = {
  users.filter(_.favoriteArtists.forall(_.length > 8)).map(_.name)
}

def userf6(users: List[User]): List[String] = {
  users.filter(_.favoriteArtists.exists(_.startsWith("M"))).map(_.name)
}

enum PlaylistMusicGenre {
  case Funk
  case House
  case HardRock
}

enum PlaylistKind {
  case ForUser(name: String)
  case ForArtist(artist: String)
  case ForGenre(genre: Set[PlaylistMusicGenre])
}

case class Song(name: String, artist: String)

case class Playlist(
  name: String,
  kind: PlaylistKind,
  songs: List[Song]
)

def gatherSongs(playlists: List[Playlist], artist: String, genre: PlaylistMusicGenre): List[Song] = {
  playlists.flatMap(p => p.kind match {
    case PlaylistKind.ForArtist(a) => if a == artist then p.songs else List.empty
    case PlaylistKind.ForGenre(g) => if g.contains(genre) then p.songs else List.empty
    case PlaylistKind.ForUser(name) => p.songs.filter(s => s.artist == artist)
  })
}

enum SearchCondition {
 case SearchByGenre(genres: List[MusicGenre])
 case SearchByOrigin(locations: List[Location])
 case SearchByActiveYears(start: Int, end: Int)
}

def wasArtistActive(artist: Artist, yearStart: Int, yearEnd: Int): Boolean = {
  true
}

def searchArtists2(artists: List[Artist], requiredConditions: List[SearchCondition]): List[Artist] = {
  artists.filter(artist => requiredConditions.forall(condition => condition match {
    case SearchCondition.SearchByGenre(genres) => genres.contains(artist.genre)
    case SearchCondition.SearchByOrigin(locations) => locations.contains(artist.origin)
    case SearchCondition.SearchByActiveYears(start, end) => wasArtistActive(artist, start, end)
  }))
}

case class MeetingTime(startHour: Int, endHour: Int)

def calendarEntriesApiCall(name: String): List[MeetingTime] = {
  val rand = Random();
  if (rand.nextFloat() < 0.25)
    throw RuntimeException("Connection error")

  if (name.equals("Alice"))
    return List(MeetingTime(8, 10), MeetingTime(11, 12));
  else if (name.equals("Bob"))
    return List(MeetingTime(9, 10));
  else
    return List(MeetingTime(rand.nextInt(5) + 8, rand.nextInt(4) + 13));
}

def createMeetingApiCall(names: List[String], meetingTime: MeetingTime): Unit = {
  val rand = Random();
  if (rand.nextFloat() < 0.25) {
    throw RuntimeException("Oof")
  }
  println("Side effect")
}

def calendarMeeting(names: List[String], meetingTime: MeetingTime): IO[Unit] = {
  IO.delay(createMeetingApiCall(names, meetingTime))
}

def calendarEntries(name: String): IO[List[MeetingTime]] = {
  IO.delay(calendarEntriesApiCall(name))
}

def retry[A](action: IO[A], maxRetries: Int): IO[A] = {
  List.range(0, maxRetries)
    .map(_ => action)
    .foldLeft(action)((program, retryAction) => {
      program.orElse(retryAction)
    })
}

def scheduledMeetings(attendees: List[String]): IO[List[MeetingTime]] = {
  attendees
    .flatTraverse(q => retry(calendarEntries(q), 10))
}

def possibleMeetingOverlaps(startHourt: Int, endHour: Int, meeting: MeetingTime): Boolean = {
  endHour > meeting.startHour && startHourt < meeting.endHour
}

def possibleMeetings(
  existingMeetings: List[MeetingTime], 
  startHour: Int, 
  endHour: Int,
  lengthHours: Int
): List[MeetingTime] = {
  List.range(startHour, endHour - lengthHours + 1)
    .filter(start => existingMeetings.forall(!possibleMeetingOverlaps(startHour, startHour + lengthHours, _)))
    .map(start => MeetingTime(start, start + lengthHours))
}

def schedule(attendees: List[String], lengthHours: Int): IO[Option[MeetingTime]] = {
  for {
    existingMeetings <- scheduledMeetings(attendees)
      .orElse(scheduledMeetings(attendees))
      .orElse(IO.pure(List.empty))
    meetings = possibleMeetings(existingMeetings, 8, 16, lengthHours)    
    _ = meetings.headOption match {
      case Some(meeting) => calendarMeeting(attendees, meeting)
      case None => IO.unit
    }
  } yield meetings.headOption
}

def f01[A, B](x: IO[A], f: A => B): IO[B] = x.map(f)
def f02[A](x: IO[IO[A]]): IO[A] = x.flatten
def f03[A, B](x: IO[A], f: A => IO[B]): IO[B] = x.flatMap(f)
def f04[A](x: A): IO[A] = IO.pure(x)
def f05[A](impureAction: () => A): IO[A] = IO.delay(impureAction())
def f06[A](x: IO[A], alternative: IO[A]): IO[A] = x.orElse(alternative)
def f07[A](x: List[IO[A]]): IO[List[A]] = x.sequence
def f08[A](x: Option[IO[A]]): IO[Option[A]] = x.sequence
def f09[A, B](x: List[A], y: List[A]): List[A] = x.appendedAll(y)
def f10[A](x: List[A], f: A => Boolean): List[A] = x.filter(f)
def f11[A](x: List[A], zero: A, f: (A, A) => A): A = x.foldLeft(zero)(f)
def f12[A](x: List[List[A]]): List[A] = x.flatten
def f13[A, B](x: List[A], f: A => List[B]): List[B] = x.flatMap(f)
def f14[A](x: List[A], f: A => Boolean): Boolean = x.forall(f)
def f15[A, B](x: Set[A], f: A => B): Set[B] = x.map(f)
def f16[A](x: Set[A], f: A => Boolean): Set[A] = x.filter(f)
def f17[A](x: Set[A], zero: A, f: (A, A) => A): A = x.foldLeft(zero)(f)
def f18[A](x: Set[Set[A]]): Set[A] = x.flatten
def f19[A, B](x: Set[A], f: A => Set[B]): Set[B] = x.flatMap(f)
def f20[A](x: Set[A], f: A => Boolean): Boolean = x.forall(f)

import cats.implicits.catsSyntaxEither

def f21[A, B](x: Option[A], f: A => B): Option[B] = x.map(f)
def f22[A](x: Option[A], f: A => Boolean): Option[A] = x.filter(f)
def f23[A](x: Option[A], zero: A, f: (A, A) => A): A = x.foldLeft(zero)(f)
def f24[A](x: Option[Option[A]]): Option[A] = x.flatten
def f25[A, B](x: Option[A], f: A => Option[B]): Option[B] = x.flatMap(f)
def f26[A](x: Option[A], f: A => Boolean): Boolean = x.forall(f)
def f27(x: String): Option[Int] = x.toIntOption
def f28[A](x: Option[A], alternative: Option[A]): Option[A] = x.orElse(alternative)
def f29[A, B](x: Option[A], y: B): Either[B, A] = x.toRight(y)
def f30[A, B](x: Option[A], y: B): Either[A, B] = x.toLeft(y)
def f31[A](x: List[Option[A]]): Option[List[A]] = x.sequence
def f32[A, B, C](x: Either[A, B], f: B => C): Either[A, C] = x.map(f)
def f33[A, B, C](x: Either[A, B], zero: C, f: (C, B) => C): C = x.foldLeft(zero)(f)
def f34[A, B](x: Either[A, Either[A, B]]): Either[A, B] = x.flatten
def f35[A, B, C](x: Either[A, B], f: B => Either[A, C]): Either[A, C] = x.flatMap(f)
def f36[A, B](x: Either[A, B], f: B => Boolean): Boolean = x.forall(f)
def f37[A, B](x: Either[A, B], alternative: Either[A, B]): Either[A, B] = x.orElse(alternative)
def f38[A, B](x: Either[A, B]): Option[B] = x.toOption
def f39[A, B](x: List[Either[A, B]]): Either[A, List[B]] = x.sequence
def f40[A, B](x: Either[A, List[B]]): List[Either[A, B]] = x.sequence

def exchangeRatesTableApiCall(currency: String): Map[String, BigDecimal] = {
  val rand = new Random();
  if (rand.nextFloat() < 0.25) 
    throw new RuntimeException("Connection error");

  var result = Map();
  if (currency.equals("USD")) {
    val eurv = BigDecimal.valueOf(0.81 + (rand.nextGaussian() / 100)).setScale(2, RoundingMode.FLOOR)
    val jpyv = BigDecimal.valueOf(103.25 + (rand.nextGaussian())).setScale(2, RoundingMode.FLOOR)
    return Map("EUR" -> eurv, "JPY" -> jpyv);
  }

  throw new RuntimeException("Rate not available");
}

def exchangeTable(from: String): IO[Map[String, BigDecimal]] = {
  IO.delay(exchangeRatesTableApiCall(from)).map(table =>
    table.map(kv =>
      kv match {
        case (currencyName, rate) => (String(currencyName), rate)
      }
    )
  )
}

import fs2._

def castTheDieImpure(): IO[Int] = {
  println("The die is cast");
  val rand = new Random();
  return IO.pure(rand.nextInt(6) + 1);
}

def lastRates(from: String, to: String, n: Int): IO[List[BigDecimal]] = {
  List.range(0, n).map(_ => currencyRate(from, to)).sequence
}

def currencyRate(from: String, to: String): IO[BigDecimal] = {
  for {
    table <- retry(exchangeTable(from), 10)
    result <- extractSingleCurrencyRate(to)(table) match {
      case Some(rate) => IO.pure(rate)
      case None => currencyRate(from, to)
    }
  } yield result
}

def extractSingleCurrencyRate(currencyToExtract: String)(table: Map[String, BigDecimal]): Option[BigDecimal] = {
  table.get(currencyToExtract)
}

def trending(rates: List[BigDecimal]): Boolean = {
  rates.size > 1 && rates.zip(rates.drop(1)).forall(pair => pair(1) > pair(0))
}

def exchangeIfTrending(amount: BigDecimal, from: String, to: String): IO[BigDecimal] = {
  for {
    rates <- lastRates(from, to, 3)
    result <- if (trending(rates)) 
                IO.pure(amount * rates.last) 
              else 
                exchangeIfTrending(amount, from, to)
  } yield result
}

import cats.effect.unsafe.implicits.global
import cats.implicits.catsSyntaxApplicativeError

def rates(from: String, to: String): Stream[IO, BigDecimal] = {
  Stream.eval(exchangeTable(from))
    .repeat
    .map(extractSingleCurrencyRate(to))
    .unNone
    .orElse(rates(from, to))
}

def exchangeIfTrending2(amount: BigDecimal, from: String, to: String): IO[BigDecimal] = {
  val delay = FiniteDuration(1, TimeUnit.SECONDS)
  val ticks = Stream.fixedRate[IO](delay)

  rates(from, to)
    .sliding(3)
    .zipLeft(ticks)
    .map(_.toList)
    .filter(trending)
    .map(_.last)
    .take(1)
    .compile
    .lastOrError
    .map(_ * amount)
}

case class CityStats(city: String, checkIns: Int)

def topCities(cityCheckIns: Map[String, Int]): List[CityStats] = {
  cityCheckIns.toList
    .map(_ match {
        case (city, checkIns) => CityStats(city, checkIns)
      })
    .sortBy(_.checkIns)
    .reverse
    .take(3)
}

def updateRanking(
 storedCheckIns: Ref[IO, Map[String, Int]],
 storedRanking: Ref[IO, List[CityStats]]
): IO[Nothing] = {
  for {
    newRanking <- storedCheckIns.get.map(topCities)
    _ <- storedRanking.set(newRanking)
    result <- updateRanking(storedCheckIns, storedRanking)
  } yield result
}

import cats.implicits.catsSyntaxParallelSequence1
import scala.concurrent.duration.DurationInt

def storeCheckIn(storedCheckIns: Ref[IO, Map[String, Int]])(city: String): IO[Unit] = {
  storedCheckIns.update(_.updatedWith(city) {
    case None => Some(1)
    case Some(checkIns) => Some(checkIns + 1)
  })
}

case class ProcessingCheckIns(currentRanking: IO[List[CityStats]], stop: IO[Unit])

def processCheckIns(checkIns: Stream[IO, String]): IO[ProcessingCheckIns] = {
  for {
    storedCheckIns <- Ref.of[IO, Map[String, Int]](Map.empty)
    storedRanking <- Ref.of[IO, List[CityStats]](List.empty)
    rankingProgram = updateRanking(storedCheckIns, storedRanking)
    checkInsProgram = checkIns.evalMap(storeCheckIn(storedCheckIns))
      .compile.drain
    fiber <- List(rankingProgram, checkInsProgram).parSequence.start
  } yield ProcessingCheckIns(storedRanking.get, fiber.cancel)
}

import cats.effect.unsafe.implicits.global

case class Attraction(name: String, description: Option[String], location: Location)

enum PopCultureSubject {
  case Artist(name: String, followers: Int)
  case Movie(name: String, boxOffice: Int)
}

case class TravelGuide(attraction: Attraction, subjects: List[PopCultureSubject])

case class Location(id: String, name: String, population: Int)

enum AttractionOrdering {
  case ByLocationPopulation
  case ByName
}

trait DataAccess {
  def findAttractions(name: String, ordering: AttractionOrdering, limit: Int): IO[List[Attraction]]
  def findArtistsFromLocation(locationId: String, limit: Int): IO[List[PopCultureSubject.Artist]]
  def findMoviesAboutLocation(locationId: String, limit: Int): IO[List[PopCultureSubject.Movie]]
}

def travelGuide(data: DataAccess, attractionName: String): IO[Option[TravelGuide]] = {
  for {
    attractions <- data.findAttractions(attractionName, ByLocationPopulation, 1)
    guide <- attractions.headOption match {
      case None => IO.pure(None)
      case Some(attraction) =>
        for {
          artists <- data.findArtistsFromLocation(attraction.location.id, 2)
          movies <- data.findMoviesAboutLocation(attraction.location.id, 2)
        } yield Some(TravelGuide(attraction, artists.appendedAll(movies)))
    }
  } yield guide
}

import org.apache.jena.query._
import org.apache.jena.rdfconnection._

def execQuery(getConnection: IO[RDFConnection], query: String): IO[List[QuerySolution]] = {
  getConnection.flatMap(c => IO.delay(
    asScala(c.query(QueryFactory.create(query)).execSelect()).toList
  ))
}

def parseAttraction(s: QuerySolution): IO[Attraction] = {
  println(s)

  IO.delay(
    Attraction(
      name = s.getLiteral("tourist_attractionLabel").getString,
      description =
        if (s.contains("description"))
          Some(s.getLiteral("description").getString)
        else None,
      location = Location(
        id = s.getResource("location").getLocalName,
        name = s.getLiteral("locationLabel").getString,
        population = s.getLiteral("population").getInt
      )
    )
  )
}

def findAttractions(name: String, ordering: AttractionOrdering, limit: Int): IO[List[Attraction]] = {
  val getConnection: IO[RDFConnection] = IO.delay(
    RDFConnectionRemote.create
      .destination("https://query.wikidata.org/")
      .queryEndpoint("sparql")
      .build
  )

  val orderBy = ordering match {
    case ByName => "?attractionLabel"
    case ByLocationPopulation => "DESC(?population)"
  }

  val query =
    s"""
      PREFIX wd: <http://www.wikidata.org/entity/>
      PREFIX wdt: <http://www.wikidata.org/prop/direct/>
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX wikibase: <http://wikiba.se/ontology#>
      PREFIX bd: <http://www.bigdata.com/rdf#>

      SELECT ?tourist_attraction ?tourist_attractionLabel ?located_in_the_administrative_territorial_entity ?located_in_the_administrative_territorial_entityLabel ?population WHERE {
      SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],mul,en". }
      ?tourist_attraction wdt:P31 wd:Q570116.
      OPTIONAL { ?tourist_attraction wdt:P131 ?located_in_the_administrative_territorial_entity. }
      OPTIONAL { ?tourist_attraction wdt:P1082 ?population. }
   } ORDER BY $orderBy LIMIT $limit"""

  for {
    solutions <- execQuery(getConnection, query)
    attractions <- solutions.traverse(parseAttraction)
  } yield attractions
}

@main
def main(): Unit = {
  println(findAttractions("Mount Everest", AttractionOrdering.ByName, 3).unsafeRunSync())

  val example: IO[Int] = for {
    counter <- Ref.of[IO, Int](0)
    _ <- counter.update(_ + 3)
    result <- counter.get
  } yield result

  val checkIns: Stream[IO, String] = Stream(
    "Sydney",
    "Sydney",
    "Cape Town",
    "Singapore",
    "Cape Town",
    "Sydney"
    ).covary[IO]

  // val diceCasts = Stream.eval(castTheDieImpure())
  //   .repeat
  //   .filter(number => number % 2 == 0)
  //   .take(3)
  //   .compile
  //   .toList

  // val diceCasts2 = Stream.eval(castTheDieImpure())
  //   .repeat
  //   .map(number => number * 2)
  //   .take(5)
  //   .compile
  //   .toList

  // val diceCasts3 = Stream.eval(castTheDieImpure())
  //   .repeat
  //   .take(3)
  //   .reduce((a, b) => a + b)
  //   .compile
  //   .toList

  // val diceCasts4 = Stream.eval(castTheDieImpure())
  //   .repeat
  //   .filter(x => x == 5)
  //   .take(1)
  //   .append(Stream.eval(castTheDieImpure()).take(2))
  //   .compile
  //   .toList

  val usdExchangeTables = List(
    Map("EUR" -> BigDecimal(0.88)),
    Map("EUR" -> BigDecimal(0.89), "JPY" -> BigDecimal(114.62)),
    Map("JPY" -> BigDecimal(114))
  )

  val p = Playlist(
    "This is Foo Fighters", 
    PlaylistKind.ForArtist("Foo Fighters"),
    List(
      Song("Breakout", "Foo Fighters"), 
      Song("Learn To Fly", "Foo Fighters")
    )
  )

  val p2 = Playlist(
    "Deep Focus", 
    PlaylistKind.ForGenre(Set(PlaylistMusicGenre.Funk, PlaylistMusicGenre.House)),
    List(
      Song("One More Time", "Daft Punk"), 
      Song("Hey Boy Hey Girl", " The Chemical Brothers")
    )
  )

//  val artists = List(
//    Artist("Metallica", MusicGenre.HeavyMetal, Location("U.S."), Set(YearsActive.StillActive(1981))),
//    Artist("Led Zeppelin", MusicGenre.HardRock, Location("England"), Set(YearsActive.ActiveBetween(1968, 1980))),
//    Artist("Bee Gees", MusicGenre.Pop, Location("England"), Set(YearsActive.ActiveBetween(1958, 2003)))
//  )

  val users = List(
    User("Alice", Some("Melbourne"), List("Bee Gees")),
    User("Bob", Some("Lagos"), List("Bee Gees")),
    User("Eve", Some("Tokyo"), List.empty),
    User("Mallory", None, List("Metallica", "Bee Gees")),
    User("Trent", Some("Buenos Aires"), List("Led Zeppelin"))
  )

  // assert(parseShows(List("The Wire (2002-2008)", "[2019]")) == Left("Can't extract name from [2019]"))
  // assert(parseShows(List("The Wire (-)", "Chernobyl (2019)")) == Left("Can't extract single year from The Wire (-)"))
  // assert(parseShows(List("The Wire (2002-2008)", "Chernobyl (2019)")) == 
  //     Right(List(TvShow("The Wire", 2002, 2008),
  //     TvShow("Chernobyl", 2019, 2019))))

  // assert(addOrResign(Some(List.empty), Some(TvShow("Chernobyl", 2019, 2019))) == Some(List(TvShow("Chernobyl", 2019, 2019))))
  
  // assert(addOrResign(Some(List(TvShow("Chernobyl", 2019, 2019))), Some(TvShow("The Wire", 2002, 2008))) == 
  //   Some(List(TvShow("Chernobyl", 2019, 2019), TvShow("The Wire", 2002, 2008))))

  // assert(addOrResign(Some(List(TvShow("Chernobyl", 2019, 2019))), None) == None)
  // assert(addOrResign(None, Some(TvShow("Chernobyl", 2019, 2019))) == None)
  // assert(addOrResign(None, None) == None)

  // assert(parseShow("Chernobyl (2019)") == Some(TvShow("Chernobyl", 2019, 2019)))
  // assert(parseShow("Breaking Bad (2008-2013)") ==  Some(TvShow("Breaking Bad", 2008, 2013)))
  // assert(parseShow("Mad Men (-2015)") == None)

  val shows = List(
    TvShow("Breaking Bad", 2008, 2013),
    TvShow("The Wire", 2002, 2008), 
    TvShow("Mad Men", 2007, 2015)
  )

  assert(sortShows(shows) == 
    List(TvShow("Mad Men", 2007, 2015), 
    TvShow("The Wire", 2002, 2008), 
    TvShow("Breaking Bad", 2008, 2013))
  )

  assert(parseLongEvent("Apollo Program", 1961, 1972, 10) == Some(Event("Apollo Program", 1961, 1972)))
  assert(parseLongEvent("World War II", 1939, 1945, 10) == None)
  assert(parseLongEvent("", 1939, 1945, 10) == None)
  assert(parseLongEvent("Apollo Program", 1972, 1961, 10) == None)

  assert(parse("Apollo Program", 1961, 1972) == Some(Event("Apollo Program", 1961, 1972))) 
  assert(parse("", 1961, 1972) == None)

  assert((for {
    x <- List(1, 2, 3)
    y <- Set(1)
    } yield x * y) == List(1, 2, 3))

  assert((for {
    x <- Set(1, 2, 3)
    y <- List(1)
    } yield x * y) == Set(1, 2, 3))

  assert((for {
    x <- List(1, 2, 3)
    y <- Set(1)
    z <- Set(0)
    } yield x * y * z) == List(0, 0, 0))

  val pointsa = List(Point(5, 2), Point(1, 1))
  val riskyRadiuses = List(-10, 0, 2)

  val points11 = for {
    r <- riskyRadiuses.filter(r => r > 0)
    p <- pointsa.filter(point => isInside(point, r))
  } yield s"$p is within a radius of $r"
  assert(points11 ==  List("Point(1,1) is within a radius of 2"))

  val points12 = for {
    r <- riskyRadiuses if r > 0
    p <- pointsa if isInside(p, r)
  } yield s"$p is within a radius of $r"
  assert(points12 ==  List("Point(1,1) is within a radius of 2"))

  val points13 = for {
    r <- riskyRadiuses
    r2 <- if r > 0 then List(r) else List.empty
    p <- pointsa
    p2 <- if isInside(p, r2) then List(p) else List.empty
  } yield s"$p is within a radius of $r"
  assert(points13 ==  List("Point(1,1) is within a radius of 2"))

  val xs2 = List(1)
  val ys2 = List(-2, 7)
  val zs2 = List(3, 4)

  val points4 = xs2.flatMap(x => ys2.flatMap(y => zs2.map(z => Point3d(x, y, z))))
  assert(points4 == List(Point3d(1, -2, 3), Point3d(1, -2, 4),
    Point3d(1, 7, 3), Point3d(1, 7, 4)))

  val points3 = for {
    x <- xs2
    y <- ys2
    z <- zs2
  } yield Point3d(x, y, z)
  assert(points3 == List(Point3d(1, -2, 3), Point3d(1, -2, 4),
    Point3d(1, 7, 3), Point3d(1, 7, 4)))

  val xs = List(1)
  val ys = List(-2, 7)

  val points2 = for {
    x <- xs
    y <- ys
  } yield Point(x, y)
  assert(points2 == List(Point(1, -2), Point(1, 7)))

  val points = xs.flatMap(x =>
    ys.map(y =>
      Point(x, y)
    )
  )
  assert(points == List(Point(1, -2), Point(1, 7)))

  val friends = List("Alice", "Bob", "Charlie")

  val recommendations = friends.flatMap(recommendedBooks)
  assert(recommendations == List(
    Book("FP in Scala", List("Chiusano", "Bjarnason")),
    Book("Get Programming with Scala", List("Sfregola")),
    Book("Harry Potter", List("Rowling")),
    Book("The Lord of the Rings", List("Tolkien"))
  ))

  val recommendations2 = friends.flatMap(friend => recommendedBooks(friend).flatMap(_.authors))
  assert(recommendations2 == List("Chiusano", "Bjarnason", "Sfregola", "Rowling", "Tolkien"))

  assert(sum(List(5, 1, 2, 4, 100)) == 112)
  assert(totalLength(List("scala", "rust", "ada")) == 12)
  assert(totalS(List("scala", "haskell", "rust", "ada")) == 3)
  assert(maximum(List(5, 1, 2, 4, 15)) == 15)

  assert(filterNumbersFn(List(5, 1, 2, 4, 0), largerThan(4)) == List(5))
  assert(filterNumbersFn(List(5, 1, 2, 4, 0), largerThan(1)) == List(5, 2, 4))

  assert(filterNumbersFn(List(5, 1, 2, 4, 15), divisibleBy(5)) == List(5, 15))
  assert(filterNumbersFn(List(5, 1, 2, 4, 15), divisibleBy(2)) == List(2, 4))

  assert(filterNumbersFn(List("scala", "ada"), shorterThan(4)) == List("ada"))
  assert(filterNumbersFn(List("scala", "ada"), shorterThan(7)) == List("scala", "ada"))

  assert(filterNumbersFn(List("rust", "ada"), containsSTimes(2)) == List.empty)
  assert(filterNumbersFn(List("rust", "ada"), containsSTimes(0)) == List("rust"))

  assert(filterShorterThan5(List("scala", "rust", "ada")) == List("rust", "ada"))
  assert(filterSCountMoreThan2(List("rust", "ada")) == List.empty)
  assert(filterOddNum(List(5, 1, 2, 4, 0)) == List(5, 1))
  assert(filterLarger4(List(5, 1, 2, 4, 0)) == List(5))

  assert(rankedWords(word => score(word) + bonus(word) + penalty(word), List("java", "scala", "ada", "haskell", "rust")) ==
    List("java", "ada", "scala", "haskell", "rust"))

  assert(sortDescendingByLetterS(List("ada", "rust")) == List("rust", "ada"))
  assert(sortIntDescending(List(5, 1, 2, 4, 3)) == List(5, 4, 3, 2, 1))
  assert(sortAscending(List("scala", "rust", "ada")) == List("ada", "rust", "scala"))
  assert(sortAscendingByLetterS(List("rust", "ada")) == List("ada", "rust"))

  assert(firstTwo(List("a", "b", "c")) == List("a", "b"))
  assert(lastTwo(List("a", "b", "c")) == List("b", "c"))
  assert(movedFirstTwoToTheEnd(List("a", "b", "c")) == List("c", "a", "b"))
  assert(insertedBeforeLast(List("a", "b"), "c") == List("a", "c", "b"))

  assert(tipCalculator(List("One", "Two", "One", "Two", "One", "Two")) == 20)
  assert(tipCalculator(List.empty) == 0)
}
