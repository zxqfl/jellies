package jellies.game

import scala.collection.immutable
import scala.collection.mutable

sealed trait JellyColour {
  def mergesWith(other: JellyColour): Boolean
}
final case class MergeableColour(identifier: Int) extends JellyColour {
  def mergesWith(other: JellyColour): Boolean = (other == this)
}
final case class UnmergeableColour(identifier: Int) extends JellyColour {
  def mergesWith(other: JellyColour): Boolean = (other == this)
}

private[game] sealed trait RawTile

private[game] final case class JellyPart(colour: JellyColour) extends RawTile
private[game] final case object RawOpenSpace extends RawTile

final class State private (
    private[game] val objects: immutable.Map[Location, RawTile]) {
  
  override def equals(o: Any) =
    o.isInstanceOf[State] && o.asInstanceOf[State].objects == objects

  override def toString = State.toASCIIArt(this)
  
  override def hashCode = objects.hashCode
  
  sealed trait Tile
  
  case object OpenSpace extends Tile
  case object Wall extends Tile

  final case class JellyRef(
      private[game] id: Int,
      colour: JellyColour) extends Tile

  private val (
      privateJellies: Set[JellyRef],
      tiles: Map[Location, Tile]) = {
    val seenObjects = mutable.Set[Location]()
    val tiles = mutable.Map[Location, Tile]()
    val jellies = mutable.Set[JellyRef]()
    var nextStartIndex = 0
    for {
      (location, tile) <- objects
      if !seenObjects.contains(location)
    } tile match {
      case part @ JellyPart(colour) => {
        val newRef = JellyRef(nextStartIndex, colour)
        nextStartIndex += 1
        jellies += newRef
        dfs(location, part, newRef)
      }
      case RawOpenSpace => tiles(location) = OpenSpace
    }
    def dfs(location: Location, colour: JellyPart, id: JellyRef): Unit = {
      if (objects.contains(location) &&
          !seenObjects.contains(location) &&
          objects(location) == colour) {
        seenObjects += location
        tiles(location) = id
        for (adj <- location.adjacent) {
          dfs(adj, colour, id)
        }
      }
    }
    (Set(jellies.toSeq: _*), Map(tiles.toSeq: _*))
  }
  
  val jellies: Set[JellyRef] = privateJellies
  
  def at(p: Location): Tile = tiles.getOrElse(p, Wall)
  def allLocations: Set[Location] = tiles.keySet
  def jellyParts(j: JellyRef): Set[Location] =
    allLocations.filter(tiles(_) == j)
  def anyTileOf(j: JellyRef): Location = {
    tiles.find(_._2 == j).get._1
  }
  
  val isLevelSolved: Boolean = {
    val mergeableJellies = jellies
        .filter(_.colour.isInstanceOf[MergeableColour])
    val distinctColours = mergeableJellies
        .map(_.colour.asInstanceOf[MergeableColour])
    mergeableJellies.size == distinctColours.size
  }
    
  private[game] def selectByColour(colour: JellyColour): JellyRef = {
    val candidates = jellies.filter(_.colour == colour)
    require(candidates.size == 1)
    candidates.head
  }
  
  final case class Move(
      jelly: JellyRef,
      desiredDirection: Direction,
      perspective: Perspective)
  
  private def checkDirection(m: Move): Either[MoveFailureReason, Unit] = {
    if (m.desiredDirection == m.perspective.left ||
        m.desiredDirection == m.perspective.right)
      Right(Unit)
    else
      Left(InvalidDirectionFromPerspective)
  }
  
  def hasPermission(jelly: JellyRef, perspective: Perspective): Boolean = {
    val field = new PhysicsField
    field.findPermittedJellies(perspective) contains jelly
  }
  
  private def checkPermission(m: Move): Either[MoveFailureReason, Unit] = {
    if (hasPermission(m.jelly, m.perspective))
      Right(Unit)
    else
      Left(JellyPermissionFailure)
  }
  
  private def checkBlocked(m: Move): Either[MoveFailureReason, Unit] = {
    val field = new PhysicsField
    val effects =
      field.applyHorizontalMove(
          Set(m.jelly),
          m.desiredDirection,
          m.perspective)
    if (effects.isEmpty)
      Left(DirectionBlocked)
    else
      Right(Unit)
  }
  
  private def applyLegalMove(m: Move): MoveResult = {
    val field = new PhysicsField
    val effects = field.applyMove(m)
    val resultingState = new State(field.extractRawTiles)
    val refMap = field.remapJellies(resultingState)
    MoveResult(this, resultingState)(effects, refMap)
  }
      
  def applyMove(m: Move): Either[MoveFailureReason, MoveResult] = {
    for {
      _ <- checkDirection(m)
      _ <- checkPermission(m)
      _ <- checkBlocked(m)
    } yield applyLegalMove(m)
  }
  
  def emptyMoveResult: MoveResult = {
    val refMap = {
      for (j <- jellies) yield (j -> j)
    }.toMap
    MoveResult(this, this)(Seq(), refMap)
  }
  
  def tileBoundingBox: BoundingBox = new BoundingBox(allLocations.toSeq: _*)
  
  sealed trait MoveEffect {
    def jelliesAffected: Set[JellyRef]
    def oldLocations: Set[Location]
  }
  
  final case class JellyMerge private (
      merges: Set[Set[JellyRef]],
      oldLocations: Set[Location]) extends MoveEffect {
    def jelliesAffected = merges.flatten
  }
      
  final case class JellyMove(
      jelliesAffected: Set[JellyRef],
      oldLocations: Set[Location],
      direction: Direction) extends MoveEffect
  
  // Mutable.
  private class PhysicsField {
    private val boundingBox = tileBoundingBox.expand(1)
    private val field: Array[Array[Tile]] =
      Array.tabulate(
          boundingBox.right - boundingBox.left + 1,
          boundingBox.top - boundingBox.bottom + 1)(
              (x, y) => at(
                  new Location(
                      boundingBox.left + x,
                      boundingBox.bottom + y)))
    private val jellyTiles: Array[Set[Location]] = {
      val jellyTiles = Array.fill(jellies.size)(Set[Location]())
      for (t <- allLocations) {
        tiles(t) match {
          case JellyRef(id, _) => jellyTiles(id) += t
          case _ =>
        }
      }
      jellyTiles
    }
    private val dsuParent: Array[Int] = (0 until jellies.size).toArray
    
    def representative(j: JellyRef): JellyRef = {
      def rep(x: Int): Int = {
        if (dsuParent(x) == x) {
          x
        } else {
          val y = rep(dsuParent(x))
          dsuParent(x) = y
          y
        }
      }
      JellyRef(rep(j.id), j.colour)
    }
    
    def apply(loc: Location): Tile =
      field(loc.x - boundingBox.left)(loc.y - boundingBox.bottom)
      
    private def update(loc: Location, newValue: Tile) = {
      assert(!(
          this(loc).isInstanceOf[JellyRef] &&
          newValue.isInstanceOf[JellyRef]))
          
      this(loc) match {
        case JellyRef(id, _) => jellyTiles(id) -= loc
        case _ =>
      }
      newValue match {
        case JellyRef(id, _) => jellyTiles(id) += loc
        case _ =>
      }
      field(loc.x - boundingBox.left)(loc.y - boundingBox.bottom) = newValue
    }
    private def apply(j: JellyRef): Set[Location] = {
      assert(dsuParent(j.id) == j.id)
      jellyTiles(j.id)
    }
    
    private def inDirection(locs: Set[Location], direction: Direction) =
      locs.map(_ + direction)
    
    private def applyGravity(
        downwardCandidates: Set[JellyRef],
        perspective: Perspective) : Seq[MoveEffect] = {
      val downwardMovers: Set[JellyRef] = downwardCandidates.filter { j =>
        inDirection(this(j), perspective.down).forall { otherTile =>
          val atOther = this(otherTile)
          atOther match {
            case OpenSpace => true
            case Wall => false
            case oj: JellyRef => downwardCandidates contains oj
          }
        }
      }
      if (downwardMovers.isEmpty) {
        Seq()
      } else if (downwardMovers.size == downwardCandidates.size) {
        val effect = forceMove(downwardMovers, perspective.down)
        effect +: applyGravity(downwardMovers, perspective)
      } else {
        applyGravity(downwardMovers, perspective)
      }
    }
    
    private def locationsOf(js: Set[JellyRef]) =
      js map representative flatMap apply
    
    private def forceMove(js: Set[JellyRef], direction: Direction): JellyMove = {
      val currentLocations = js.map(j => (j, jellyTiles(j.id)))
      val originalLocations = currentLocations flatMap (_._2)
      for {
        (j, locs) <- currentLocations
        loc <- locs
      } this(loc) = OpenSpace
      for {
        (j, locs) <- currentLocations
        loc <- locs
      } this(loc.translate(direction)) = j
      JellyMove(jellies.filter(x => js contains representative(x)),
                originalLocations, direction)
    }
    
    def applyHorizontalMove(
        candidates: Set[JellyRef],
        direction: Direction,
        perspective: Perspective): Option[JellyMove] = {
      require(direction == perspective.left || direction == perspective.right)
      
      var failure = false
      
      val movers: Set[JellyRef] = for {
        j <- candidates
        loc <- this(j)
        x <- {
          this(loc.translate(direction)) match {
            case o: JellyRef => List(j, o)
            case Wall => failure = true; List()
            case OpenSpace => List(j)
          }
        }
      } yield x
      
      if (failure)
        None
      else if (movers.size == candidates.size)
        Some(forceMove(movers, direction))
      else
        applyHorizontalMove(movers, direction, perspective)
    }
    
    private def merge(a: JellyRef, b: JellyRef): Unit = {
      require(a.colour mergesWith b.colour)
      val ra = representative(a)
      val rb = representative(b)
      if (ra != rb) {
        val absorbedTiles = this(rb)
        for (t <- absorbedTiles) {
          this(t) = OpenSpace
          this(t) = ra
        }
        dsuParent(rb.id) = ra.id
      }
    }
    
    private def families: Set[Set[JellyRef]] = {
      var result = mutable.Map[JellyRef, Set[JellyRef]]()
      for (j <- jellies) {
        val rj = representative(j)
        if (!result.contains(rj))
          result(rj) = Set[JellyRef]()
        result(rj) += j
      }
      result.values.toSet
    }
    
    def mergeWherePossible(): Seq[MoveEffect] = {
      val js = remainingJellies
      val initialFamilies = families
      for {
        j <- js
        loc <- this(representative(j))
        adjLoc <- loc.adjacent
      } this(adjLoc) match {
        case oj: JellyRef if j.colour mergesWith oj.colour => merge(j, oj)
        case _ =>
      }
      val mergedFamilies = families
      val changes = mergedFamilies.filter(!initialFamilies.contains(_))
      if (changes.isEmpty)
        Seq()
      else
        Seq(JellyMerge(changes, locationsOf(changes.flatten)))
    }
    
    private def remainingJellies: Set[JellyRef] =
      jellies.toSet.map(representative)
    
    def findPermittedJellies(perspective: Perspective): Set[JellyRef] = {
      def helper(
          candidates: Set[JellyRef],
          members: Set[JellyRef]): Set[JellyRef] = {
        var newCandidates = Set[JellyRef]()
        var newMembers = members
        for {
          j <- candidates
          loc <- inDirection(this(j), perspective.down)
        } this(loc) match {
          case oj: JellyRef if members contains oj => newMembers += j
          case Wall => newMembers += j
          case _ => newCandidates += j
        }
        if (candidates == newCandidates && members == newMembers) {
          members
        } else {
          helper(newCandidates, newMembers)
        }
      }
      helper(remainingJellies, Set())
    }
    
    private def supportedBy(
        js: Set[JellyRef], perspective: Perspective): Set[JellyRef] = {
      var result = js
      for (j <- js) {
        for (otherLoc <- inDirection(this(j), perspective.up)) {
          this(otherLoc) match {
            case oj: JellyRef => result += oj
            case _ =>
          }
        }
      }
      if (result == js)
        js
      else
        supportedBy(result, perspective)
    }
    
    def updateFallSet(
        fallSet: Set[JellyRef],
        unaffected: Set[JellyRef],
        perspective: Perspective): Set[JellyRef] = {
      var newFallSet = Set[JellyRef]()
      for (j <- fallSet) {
        if (!unaffected.contains(j)) {
          newFallSet += j
          for (otherLoc <- inDirection(this(j), perspective.down) ++
                           Set()) {
//                           inDirection(this(j), perspective.up)) {
            this(otherLoc) match {
              case oj: JellyRef if !unaffected.contains(oj) => newFallSet += oj
              case _ =>
            }
          }
        }
      }
      if (newFallSet == fallSet) {
        fallSet
      } else {
        updateFallSet(newFallSet, unaffected, perspective)
      }
    }
    
    def applyMove(m: Move): Seq[MoveEffect] = {
//      val supported = supportedBy(
//          Set(m.jelly),
//          m.perspective)
      val supported = findPermittedJellies(m.perspective)
      val initialEffect = applyHorizontalMove(
          Set(m.jelly),
          m.desiredDirection,
          m.perspective).get // legality already checked
      var fallSet = supported
      var result: Seq[MoveEffect] = Seq(initialEffect)
      while (!fallSet.isEmpty) {
        result ++= applyGravity(fallSet, m.perspective)
        val unaffected = findPermittedJellies(m.perspective)
        fallSet = fallSet map representative
        fallSet = updateFallSet(fallSet, unaffected, m.perspective)
      }
      result ++= mergeWherePossible()
      result
    }
    
    def remapJellies(newState: State): Map[JellyRef, newState.JellyRef] = {
      for (j <- jellies) yield {
        j -> (newState at this(representative(j)).head).asInstanceOf[newState.JellyRef]
      }
    }.toMap
    
    def extractRawTiles: Map[Location, RawTile] = {
      for {
        (loc, _) <- objects
      } yield this(loc) match {
        case JellyRef(_, colour) => loc -> JellyPart(colour)
        case OpenSpace => loc -> RawOpenSpace
        case _ => throw new AssertionError
      }
    }
  }
}

object State {
  // For easily readable tests.
  def fromASCIIArt(s: String): State = {
    val lines = s.trim.split("""[\r\n]+""").map(_.trim)
    val height = lines.size
    val width = lines(0).size
    lines.foreach(x => require(x.size == width))
    val objMap: Map[Location, RawTile] = {
      for {
        x <- 1 to width
        y <- 1 to height
        mapping <- {
          val loc = Location(x, y)
          val i = height - y
          val j = x - 1
          lines(i)(j) match {
            case '.' => Some(loc -> RawOpenSpace)
            case 'X' => None
            case digit if '1' <= digit && digit <= '9' => {
              Some(loc -> JellyPart(MergeableColour(digit.toString.toInt)))
            }
            case _ => throw new IllegalArgumentException
          }
        }: Option[(Location, RawTile)]
      } yield mapping
    }.toMap
    new State(objMap)
  }
  
  def toASCIIArt(state: State): String = {
    val boundingBox = state.tileBoundingBox
    val arr = Array.fill(
        boundingBox.top - boundingBox.bottom + 1,
        boundingBox.right - boundingBox.left + 1)('X')
    for (loc <- state.allLocations) {
      arr(boundingBox.top - loc.y)(loc.x - boundingBox.left) = {
        state.at(loc) match {
          case state.OpenSpace => '.'
          case state.JellyRef(_, MergeableColour(x)) => {
            val digit = x.toString
            assert(digit.length == 1)
            digit(0)
          }
          case state.JellyRef(_, UnmergeableColour(_)) => ???
          case state.Wall => throw new AssertionError
        }
      }
    }
    val lines = for (chars <- arr) yield chars.mkString("", "", "\n")
    lines.mkString
  }
}