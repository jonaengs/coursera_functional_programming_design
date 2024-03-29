class Pouring(capacities: Vector[Int]) {
  // States
  type State = Vector[Int]
  val initialState = capacities map (_ => 0)

  // Moves
  trait Move {
    def change(state: State): State
  }
  case class Empty(glass: Int) extends Move {
    def change(state: State): State = state updated(glass, 0)
  }
  case class Fill(glass: Int) extends Move {
    def change(state: State): State = state updated(glass, capacities(glass))
  }
  case class Pour(from: Int, to: Int) extends Move {
    def change(state: State): State = {
      val amount = state(from) min (capacities(to) - state(to))
      state updated (from, state(from) - amount) updated(to, state(to) + amount)
    }
  }

  val glasses = 0 until capacities.length
  val moves =
    (for (g <- glasses) yield Empty(g)) ++
      (for (g <- glasses) yield Fill(g)) ++  // if stmnt below added by me
      (for (from <- glasses; to <- glasses; if from != to) yield Pour(from, to))

  // Paths
  class Path(history: List[Move], val endState: State) {  // "val" in before parameter name makes member public
    def extend(move: Move) = new Path(move :: history, move change endState)
    override def toString = (history.reverse mkString " ") + "--> " + endState
  }

  val initialPath = new Path(Nil, initialState)

  def from(paths: Set[Path], explored: Set[State]): LazyList[Set[Path]] =
    if (paths.isEmpty) LazyList.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
        if !(explored contains next.endState)
      } yield next
      paths #:: from(more, explored ++ (more map (_.endState)))
    }

  val pathSets = from(Set(initialPath), Set(initialState))

  // Solution
  def solution(target: Int): LazyList[Path] =
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState contains target
    } yield path
}


val problem = new Pouring(Vector(4, 9))
problem.solution(6).take(1).toList

/*
// Old Path implementation with endState foldRight:
  class Path(history: List[Move]) {
    def endState: State = (history foldRight initialState) (_ change _)  // applies each move to the initial state
    def extend(move: Move) = new Path(move :: history)
    override def toString = (history.reverse mkString " ") + "--> " + endState
  }
*/


