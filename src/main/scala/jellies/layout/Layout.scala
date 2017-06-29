package jellies.layout

import jellies.game
import jellies.game.MoveResult
import jellies.game.Location
import jellies.game.Perspective
import jellies.game.State
import jellies.game.Direction

class Layout(val moveInfo: MoveResult, val perspective: Perspective) {
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
  }
      
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
      refB: moveInfo.resultingState.JellyRef) extends Tile {
    def finalLocation = moveInfo.resultingState.anyTileOf(refB)
  }
      
  val tiles: Seq[Seq[Tile]] = {
    moveInfo.effects
        .scanLeft(initialInterimState)(_ >>> _)
        .map { interimState =>
      val elts = for {
        loc <- boundingBox.allTiles
        here <- interimState.at(loc) match {
          case jelly: moveInfo.initialState.JellyRef => {
            Some(Jelly(loc, interimState.getSameInfo(loc, jelly),
                  jelly, moveInfo.refMap(jelly)))
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
  
  assert(tiles.length == directions.length)
}