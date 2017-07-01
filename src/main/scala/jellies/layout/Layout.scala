package jellies.layout

import jellies.game
import jellies.game.MoveResult
import jellies.game.Location
import jellies.game.Perspective
import jellies.game.State
import jellies.game.Direction
import jellies.utility.Util

class Layout(val moveInfo: MoveResult, val perspective: Perspective) {
      
  private def initialInterimState = {
    val locMap = {
      for {
        loc <- moveInfo.initialState.allLocations
        if moveInfo.initialState.at(loc).isInstanceOf[moveInfo.initialState.JellyRef]
      } yield (loc -> moveInfo.initialState.at(loc).asInstanceOf[moveInfo.initialState.JellyRef])
    }.toMap
    val jellyMap = {
      for (j <- moveInfo.initialState.jellies) yield (j -> j)
    }.toMap
    InterimState(locMap, jellyMap)
  }
  
  val boundingBox = moveInfo.initialState.tileBoundingBox.expand(1)
  
  sealed trait Tile {
    def finalLocation: Location
  }
  
  final case class Wall(
      location: Location,
      sameInfo: SameInfo) extends Tile {
    def finalLocation = location
  }
  final case class Jelly(
      location: Location,
      sameInfo: SameInfo,
      refA: moveInfo.initialState.JellyRef,
      refB: moveInfo.resultingState.JellyRef,
      onGroundNow: Boolean,
      onGroundNext: Boolean) extends Tile {
    def finalLocation = moveInfo.resultingState.anyTileOf(refB)
  }
  
  val length = moveInfo.effects.length + 1
      
  val tiles: Seq[Seq[Tile]] = {
    val states = moveInfo.effects.scanLeft(initialInterimState)(_ >>> _)
    states.zipWithIndex.map { case (interimState, index) =>
      val nextIndex = {
        if (index + 1 == states.size) {
          index
        } else {
          var nextIndex = index + 1
          while (nextIndex + 1 < states.size &&
                 moveInfo.effects(nextIndex)
                 .isInstanceOf[moveInfo.initialState.JellyMerge]) {
            nextIndex += 1
          }
          nextIndex
        }
      }
      val nextState = states(nextIndex)
      val elts = for {
        loc <- boundingBox.allTiles
        here <- interimState.at(loc) match {
          case jelly: moveInfo.initialState.JellyRef => {
            Some(Jelly(loc, interimState.getSameInfo(loc, jelly),
                  jelly, moveInfo.refMap(jelly),
                  interimState.isOnGround(jelly),
                  nextState.isOnGround(jelly)))
          }
          case moveInfo.initialState.OpenSpace => None
          case x @ moveInfo.initialState.Wall => {
            Some(Wall(loc, interimState.getSameInfo(loc, x))) 
          }
        }
      } yield here
      elts.toSeq
    }
  }
  
  val directions: Seq[Map[moveInfo.initialState.JellyRef, Direction]] = {
    val maps = moveInfo.effects.map {
      case _: moveInfo.initialState.JellyMerge => {
        Map[moveInfo.initialState.JellyRef, Direction]()
      }
      case m: moveInfo.initialState.JellyMove => {
        for (j <- m.jelliesAffected) yield (j -> m.direction)
      }.toMap
    }
    maps :+ Map()
  }
  
  private case class InterimState(
      locMap: Map[Location, moveInfo.initialState.JellyRef],
      jellyMap: Map[moveInfo.initialState.JellyRef,
                    moveInfo.initialState.JellyRef]) {
    
    def >>> (effect: moveInfo.initialState.MoveEffect): InterimState = {
      effect match {
        case m: moveInfo.initialState.JellyMove => {
          var newLocs = locMap
          for (loc <- m.oldLocations) {
            newLocs -= loc
          }
          for (loc <- m.oldLocations) {
            newLocs += (loc + m.direction) -> locMap(loc)
          }
          val s2 = newLocs.size
          InterimState(newLocs, jellyMap)
        }
        case m: moveInfo.initialState.JellyMerge => {
          var newJellies = jellyMap
          for (jelly <- m.merges) {
            val repr = jelly.head
            for (part <- jelly) {
              newJellies += (part -> repr)
            }
          }
          InterimState(locMap, newJellies)
        }
      }
    }
    
    def at(loc: Location) = {
      locMap.get(loc) match {
        case Some(j: moveInfo.initialState.JellyRef) => {
          jellyMap(j)
        }
        case Some(x) => x
        case None => moveInfo.initialState.at(loc) match {
          case _: moveInfo.initialState.JellyRef => {
            moveInfo.initialState.OpenSpace
          }
          case x => x
        }
      }
    }
    
    def getSameInfo(
        loc: Location, 
        tile: moveInfo.initialState.Tile): SameInfo = {
      SameInfo(
          leftSame = (tile == at(loc + perspective.left)),
          rightSame = (tile == at(loc + perspective.right)),
          upSame = (tile == at(loc + perspective.up)),
          downSame = (tile == at(loc + perspective.down)))
    }
    
    private lazy val invertedLocs = Util.invert(locMap)
    private lazy val invertedJellies = Util.invert(jellyMap)
    
    private def locationsOf(
        jelly: moveInfo.initialState.JellyRef): Set[Location] = {
      invertedJellies(jellyMap(jelly)).flatMap(invertedLocs)
    }
    
    // This is a reimplementation of findPermittedJellies
    // from State#PhysicsField.
    private lazy val grounded: Set[moveInfo.initialState.JellyRef] = {
      def helper(
          candidates: Set[moveInfo.initialState.JellyRef],
          members: Set[moveInfo.initialState.JellyRef])
            : Set[moveInfo.initialState.JellyRef] = {
        var newCandidates = Set[moveInfo.initialState.JellyRef]()
        var newMembers = members
        for {
          j <- candidates
          loc <- locationsOf(j).map(_ + perspective.down)
        } at(loc) match {
          case oj: moveInfo.initialState.JellyRef if members contains oj => {
            newMembers += j
          }
          case moveInfo.initialState.Wall => {
            newMembers += j
          }
          case _ => {
            newCandidates += j
          }
        }
        if (candidates == newCandidates && members == newMembers) {
          members
        } else {
          helper(newCandidates, newMembers)
        }
      }
      helper(jellyMap.values.toSet, Set())
    }
    
    def isOnGround(jelly: moveInfo.initialState.JellyRef): Boolean = {
      grounded contains jellyMap(jelly)
    }
  }
}