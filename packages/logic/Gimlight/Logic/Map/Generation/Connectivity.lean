module

public import Gimlight.Logic.Map.Generation.Tiles
public import Gimlight.Logic.Map.Generation.CandidateRoom

namespace Gimlight.MapGeneration

private inductive BoundedPath (dimensions : Dimensions) (source : Position) : Position → Type where
  | refl (sourceInBounds : source.x < dimensions.width ∧ source.y < dimensions.height) :
      BoundedPath dimensions source source
  | step {current target : Position}
      (path : BoundedPath dimensions source current)
      (direction : Direction)
      (stepped : current.step direction = some target)
      (targetInBounds : target.x < dimensions.width ∧ target.y < dimensions.height) :
      BoundedPath dimensions source target

private theorem BoundedPath.sourceInBounds
    (path : BoundedPath dimensions source target) :
    source.x < dimensions.width ∧ source.y < dimensions.height := by
  induction path with
  | refl sourceInBounds => exact sourceInBounds
  | step _ _ _ _ sourceInBounds => exact sourceInBounds

private theorem BoundedPath.targetInBounds
    (path : BoundedPath dimensions source target) :
    target.x < dimensions.width ∧ target.y < dimensions.height := by
  cases path with
  | refl sourceInBounds => exact sourceInBounds
  | step _ _ _ targetInBounds => exact targetInBounds

private def BoundedPath.trans
    (first : BoundedPath dimensions source middle) {target : Position} :
    BoundedPath dimensions middle target → BoundedPath dimensions source target
  | .refl _ => first
  | .step path direction stepped targetInBounds =>
      .step (first.trans path) direction stepped targetInBounds

private def BoundedPath.symm
    (path : BoundedPath dimensions source target) :
    BoundedPath dimensions target source :=
  match path with
  | .refl sourceInBounds => .refl sourceInBounds
  | .step path direction stepped targetInBounds =>
      let reversedStep : BoundedPath dimensions _ _ :=
        .step (.refl targetInBounds) direction.opposite
          (Position.step_opposite stepped) path.targetInBounds
      reversedStep.trans path.symm

private def BoundedPath.horizontalRight (dimensions : Dimensions) (x y distance : Nat)
    (targetInBounds : x + distance < dimensions.width ∧ y < dimensions.height) :
    BoundedPath dimensions { x, y } { x := x + distance, y } :=
  match distance with
  | 0 => .refl (by simpa using targetInBounds)
  | distance + 1 =>
      .step (BoundedPath.horizontalRight dimensions x y distance (by omega)) .right
        (by simp [Position.step, Nat.add_assoc]) targetInBounds

private def BoundedPath.horizontal (dimensions : Dimensions) (y sourceX targetX : Nat)
    (sourceInBounds : sourceX < dimensions.width ∧ y < dimensions.height)
    (targetInBounds : targetX < dimensions.width ∧ y < dimensions.height) :
    BoundedPath dimensions { x := sourceX, y } { x := targetX, y } :=
  if ordered : sourceX ≤ targetX then
    by
      simpa [Nat.add_sub_of_le ordered] using
        BoundedPath.horizontalRight dimensions sourceX y (targetX - sourceX) (by omega)
  else
    by
      have targetBeforeSource : targetX ≤ sourceX := by omega
      simpa [Nat.add_sub_of_le targetBeforeSource] using
        (BoundedPath.horizontalRight dimensions targetX y (sourceX - targetX) (by omega)).symm

private def BoundedPath.verticalDown (dimensions : Dimensions) (x y distance : Nat)
    (targetInBounds : x < dimensions.width ∧ y + distance < dimensions.height) :
    BoundedPath dimensions { x, y } { x, y := y + distance } :=
  match distance with
  | 0 => .refl (by simpa using targetInBounds)
  | distance + 1 =>
      .step (BoundedPath.verticalDown dimensions x y distance (by omega)) .down
        (by simp [Position.step, Nat.add_assoc]) targetInBounds

private def BoundedPath.vertical (dimensions : Dimensions) (x sourceY targetY : Nat)
    (sourceInBounds : x < dimensions.width ∧ sourceY < dimensions.height)
    (targetInBounds : x < dimensions.width ∧ targetY < dimensions.height) :
    BoundedPath dimensions { x, y := sourceY } { x, y := targetY } :=
  if ordered : sourceY ≤ targetY then
    by
      simpa [Nat.add_sub_of_le ordered] using
        BoundedPath.verticalDown dimensions x sourceY (targetY - sourceY) (by omega)
  else
    by
      have targetBeforeSource : targetY ≤ sourceY := by omega
      simpa [Nat.add_sub_of_le targetBeforeSource] using
        (BoundedPath.verticalDown dimensions x targetY (sourceY - targetY) (by omega)).symm

private def BoundedPath.tunnel (dimensions : Dimensions) (source target : Position)
    (horizontalFirst : Bool)
    (sourceInBounds : source.x < dimensions.width ∧ source.y < dimensions.height)
    (targetInBounds : target.x < dimensions.width ∧ target.y < dimensions.height) :
    BoundedPath dimensions source target :=
  if horizontalFirst then
    let horizontal := BoundedPath.horizontal dimensions source.y source.x target.x sourceInBounds
      ⟨targetInBounds.1, sourceInBounds.2⟩
    let vertical := BoundedPath.vertical dimensions target.x source.y target.y
      ⟨targetInBounds.1, sourceInBounds.2⟩ targetInBounds
    horizontal.trans vertical
  else
    let vertical := BoundedPath.vertical dimensions source.x source.y target.y sourceInBounds
      ⟨sourceInBounds.1, targetInBounds.2⟩
    let horizontal := BoundedPath.horizontal dimensions target.y source.x target.x
      ⟨sourceInBounds.1, targetInBounds.2⟩ targetInBounds
    vertical.trans horizontal

private theorem boundedPath_reverse_exists
    (path : BoundedPath dimensions source target) :
    Nonempty (BoundedPath dimensions target source) :=
  ⟨path.symm⟩

private theorem boundedPath_append_exists
    (first : BoundedPath dimensions source middle)
    (second : BoundedPath dimensions middle target) :
    Nonempty (BoundedPath dimensions source target) :=
  ⟨first.trans second⟩

private theorem MapGenerationParameters.widthPositive
    (parameters : MapGenerationParameters) : 0 < parameters.dimensions.width := by
  have valid := parameters.valid
  omega

private theorem Position.eq_of_rowMajorIndex_eq (dimensions : Dimensions)
    (widthPositive : 0 < dimensions.width) (source target : Position)
    (sourceInBounds : source.x < dimensions.width)
    (targetInBounds : target.x < dimensions.width)
    (indexEq : source.y * dimensions.width + source.x =
      target.y * dimensions.width + target.x) :
    source = target := by
  have xEq : source.x = target.x := by
    have modEq := congrArg (fun index => index % dimensions.width) indexEq
    simpa [Nat.add_mod, Nat.mod_eq_of_lt sourceInBounds, Nat.mod_eq_of_lt targetInBounds]
      using modEq
  have rowsEq : source.y * dimensions.width = target.y * dimensions.width := by
    omega
  have yEq := Nat.mul_right_cancel widthPositive rowsEq
  cases source
  cases target
  simp_all

private theorem SizedTiles.setFloorAt_preservesReachable (tiles : SizedTiles parameters)
    (position : Position)
    (positionInBounds : position.x < parameters.dimensions.width ∧
      position.y < parameters.dimensions.height)
    (path : FloorReachable tiles.tiles parameters.dimensions source target) :
    FloorReachable (tiles.setFloorAt position).tiles parameters.dimensions source target :=
  path.mono fun pathPosition _ pathFloor =>
    tiles.setFloorAt_preservesFloor position pathPosition positionInBounds pathFloor

public structure ConnectedTiles (parameters : MapGenerationParameters) where private mk ::
  public tiles : SizedTiles parameters
  public start : Position
  public startInBounds : start.x < parameters.dimensions.width ∧
    start.y < parameters.dimensions.height
  public startFloor : tiles.tiles[start.y * parameters.dimensions.width + start.x]? = some .floor
  private reachable : ∀ target,
    target.x < parameters.dimensions.width ∧ target.y < parameters.dimensions.height →
    tiles.tiles[target.y * parameters.dimensions.width + target.x]? = some .floor →
    FloorReachable tiles.tiles parameters.dimensions start target

private def ConnectedTiles.startAt (parameters : MapGenerationParameters) (start : Position)
    (startInBounds : start.x < parameters.dimensions.width ∧
      start.y < parameters.dimensions.height) : ConnectedTiles parameters :=
  let walls := SizedTiles.walls parameters
  let tiles := walls.setFloorAt start
  { tiles
    start
    startInBounds
    startFloor := walls.setFloorAt_at start startInBounds
    reachable := by
      intro target targetInBounds targetFloor
      have sameIndex : start.y * parameters.dimensions.width + start.x =
          target.y * parameters.dimensions.width + target.x := by
        by_cases sameIndex : start.y * parameters.dimensions.width + start.x =
            target.y * parameters.dimensions.width + target.x
        · exact sameIndex
        · have wallFloor := walls.floor_of_setFloorAt_floor_of_index_ne start target sameIndex
            targetFloor
          exact (SizedTiles.walls_not_floor parameters target wallFloor).elim
      have samePosition := Position.eq_of_rowMajorIndex_eq parameters.dimensions
        (MapGenerationParameters.widthPositive parameters) start target startInBounds.1
        targetInBounds.1 sameIndex
      subst target
      exact .refl startInBounds targetFloor }

private def ConnectedTiles.extend (connected : ConnectedTiles parameters)
    {previous target : Position}
    (previousReachable : FloorReachable connected.tiles.tiles parameters.dimensions
      connected.start previous)
    (direction : Direction) (stepped : previous.step direction = some target)
    (targetInBounds : target.x < parameters.dimensions.width ∧
      target.y < parameters.dimensions.height) : ConnectedTiles parameters :=
  let tiles := connected.tiles.setFloorAt target
  let preservedPrevious := connected.tiles.setFloorAt_preservesReachable target targetInBounds
    previousReachable
  let targetFloor := connected.tiles.setFloorAt_at target targetInBounds
  let targetReachable : FloorReachable tiles.tiles parameters.dimensions connected.start target :=
    .step preservedPrevious direction stepped targetInBounds targetFloor
  { tiles
    start := connected.start
    startInBounds := connected.startInBounds
    startFloor := connected.tiles.setFloorAt_preservesFloor target connected.start targetInBounds
      connected.startFloor
    reachable := by
      intro position positionInBounds positionFloor
      by_cases sameIndex : target.y * parameters.dimensions.width + target.x =
          position.y * parameters.dimensions.width + position.x
      · have samePosition := Position.eq_of_rowMajorIndex_eq parameters.dimensions
          (MapGenerationParameters.widthPositive parameters) target position targetInBounds.1
          positionInBounds.1 sameIndex
        subst position
        exact targetReachable
      · have oldFloor := connected.tiles.floor_of_setFloorAt_floor_of_index_ne target position
          sameIndex positionFloor
        exact connected.tiles.setFloorAt_preservesReachable target targetInBounds
          (connected.reachable position positionInBounds oldFloor) }

public theorem ConnectedTiles.allFloorsConnected (connected : ConnectedTiles parameters)
    (source target : Position)
    (sourceInBounds : source.x < parameters.dimensions.width ∧
      source.y < parameters.dimensions.height)
    (targetInBounds : target.x < parameters.dimensions.width ∧
      target.y < parameters.dimensions.height)
    (sourceFloor : connected.tiles.tiles[
      source.y * parameters.dimensions.width + source.x]? = some .floor)
    (targetFloor : connected.tiles.tiles[
      target.y * parameters.dimensions.width + target.x]? = some .floor) :
    FloorReachable connected.tiles.tiles parameters.dimensions source target :=
  (connected.reachable source sourceInBounds sourceFloor).symm.trans
    (connected.reachable target targetInBounds targetFloor)

private structure PathCarvingResult (parameters : MapGenerationParameters)
    (source target : Position) where
  connected : ConnectedTiles parameters
  sourceFloor : connected.tiles.tiles[
    source.y * parameters.dimensions.width + source.x]? = some .floor
  targetFloor : connected.tiles.tiles[
    target.y * parameters.dimensions.width + target.x]? = some .floor

private def ConnectedTiles.carvePath (connected : ConnectedTiles parameters)
    {source target : Position}
    (sourceFloor : connected.tiles.tiles[
      source.y * parameters.dimensions.width + source.x]? = some .floor)
    (path : BoundedPath parameters.dimensions source target) :
    PathCarvingResult parameters source target :=
  match path with
  | .refl _ =>
      { connected
        sourceFloor
        targetFloor := sourceFloor }
  | .step path direction stepped targetInBounds =>
      let carved := connected.carvePath sourceFloor path
      let previousReachable := carved.connected.reachable _ path.targetInBounds carved.targetFloor
      let updated := carved.connected.extend previousReachable direction stepped targetInBounds
      { connected := updated
        sourceFloor := carved.connected.tiles.setFloorAt_preservesFloor target source targetInBounds
          carved.sourceFloor
        targetFloor := carved.connected.tiles.setFloorAt_at target targetInBounds }

private theorem ConnectedTiles.carvePath_target_reachable
    (connected : ConnectedTiles parameters) (sourceFloor : connected.tiles.tiles[
      source.y * parameters.dimensions.width + source.x]? = some .floor)
    (path : BoundedPath parameters.dimensions source target) :
    let carved := connected.carvePath sourceFloor path
    FloorReachable carved.connected.tiles.tiles parameters.dimensions
      carved.connected.start target := by
  simp only
  let carved := connected.carvePath sourceFloor path
  exact carved.connected.reachable target path.targetInBounds carved.targetFloor

public structure RootedConnectedTiles (parameters : MapGenerationParameters)
    (root : Position) where private mk ::
  public connected : ConnectedTiles parameters
  public rootInBounds : root.x < parameters.dimensions.width ∧
    root.y < parameters.dimensions.height
  public rootFloor : connected.tiles.tiles[
    root.y * parameters.dimensions.width + root.x]? = some .floor

private def ConnectedTiles.rootedAtStart (connected : ConnectedTiles parameters) :
    RootedConnectedTiles parameters connected.start :=
  { connected
    rootInBounds := connected.startInBounds
    rootFloor := connected.startFloor }

private def RootedConnectedTiles.carvePathTo
    (rooted : RootedConnectedTiles parameters root) {target : Position}
    (path : BoundedPath parameters.dimensions root target) :
    RootedConnectedTiles parameters root :=
  let carved := rooted.connected.carvePath rooted.rootFloor path
  { connected := carved.connected
    rootInBounds := rooted.rootInBounds
    rootFloor := carved.sourceFloor }

private def RootedConnectedTiles.carveTunnel
    (rooted : RootedConnectedTiles parameters source) (target : Position)
    (horizontalFirst : Bool)
    (targetInBounds : target.x < parameters.dimensions.width ∧
      target.y < parameters.dimensions.height) :
    RootedConnectedTiles parameters target :=
  let path := BoundedPath.tunnel parameters.dimensions source target horizontalFirst
    rooted.rootInBounds targetInBounds
  let carved := rooted.connected.carvePath rooted.rootFloor path
  { connected := carved.connected
    rootInBounds := targetInBounds
    rootFloor := carved.targetFloor }

private theorem RootedConnectedTiles.carveTunnel_connected
    (rooted : RootedConnectedTiles parameters source) (target : Position)
    (horizontalFirst : Bool)
    (targetInBounds : target.x < parameters.dimensions.width ∧
      target.y < parameters.dimensions.height) :
    let carved := rooted.carveTunnel target horizontalFirst targetInBounds
    ∀ position,
      position.x < parameters.dimensions.width ∧
        position.y < parameters.dimensions.height →
      carved.connected.tiles.tiles[
        position.y * parameters.dimensions.width + position.x]? = some .floor →
      FloorReachable carved.connected.tiles.tiles parameters.dimensions
        carved.connected.start position := by
  simp only
  intro position positionInBounds positionFloor
  exact (rooted.carveTunnel target horizontalFirst targetInBounds).connected.reachable
    position positionInBounds positionFloor

private def RootedConnectedTiles.carveRoom
    {candidate : CandidateRoom parameters}
    (rooted : RootedConnectedTiles parameters candidate.center) :
    RootedConnectedTiles parameters candidate.center :=
  (List.finRange candidate.1.height).foldl (fun carved offsetY =>
    (List.finRange candidate.1.width).foldl (fun carved offsetX =>
      let target : Position :=
        { x := candidate.1.x + offsetX.val
          y := candidate.1.y + offsetY.val }
      have targetInBounds : target.x < parameters.dimensions.width ∧
          target.y < parameters.dimensions.height := by
        have valid := candidate.2
        change CandidateRoomValid parameters candidate.1 at valid
        simp only [CandidateRoomValid] at valid
        change candidate.1.x + offsetX.val < parameters.dimensions.width ∧
          candidate.1.y + offsetY.val < parameters.dimensions.height
        have offsetXInBounds := offsetX.isLt
        have offsetYInBounds := offsetY.isLt
        omega
      let path := BoundedPath.tunnel parameters.dimensions candidate.center target true
        candidate.centerInBounds targetInBounds
      carved.carvePathTo path) carved) rooted

private theorem RootedConnectedTiles.carveRoom_connected
    {candidate : CandidateRoom parameters}
    (rooted : RootedConnectedTiles parameters candidate.center) :
    let carved := rooted.carveRoom
    ∀ position,
      position.x < parameters.dimensions.width ∧
        position.y < parameters.dimensions.height →
      carved.connected.tiles.tiles[
        position.y * parameters.dimensions.width + position.x]? = some .floor →
      FloorReachable carved.connected.tiles.tiles parameters.dimensions
        carved.connected.start position := by
  simp only
  intro position positionInBounds positionFloor
  exact rooted.carveRoom.connected.reachable position positionInBounds positionFloor

public def ConnectedTiles.initialRoom (candidate : CandidateRoom parameters) :
    RootedConnectedTiles parameters candidate.center :=
  let connected := ConnectedTiles.startAt parameters candidate.center candidate.centerInBounds
  connected.rootedAtStart.carveRoom

public def RootedConnectedTiles.addRoom
    (rooted : RootedConnectedTiles parameters source)
    (candidate : CandidateRoom parameters) (horizontalFirst : Bool) :
    RootedConnectedTiles parameters candidate.center :=
  (rooted.carveTunnel candidate.center horizontalFirst candidate.centerInBounds).carveRoom

public theorem RootedConnectedTiles.addRoom_allFloorsConnected
    (rooted : RootedConnectedTiles parameters source)
    (candidate : CandidateRoom parameters) (horizontalFirst : Bool) :
    let added := rooted.addRoom candidate horizontalFirst
    ∀ first second,
      first.x < parameters.dimensions.width ∧
        first.y < parameters.dimensions.height →
      second.x < parameters.dimensions.width ∧
        second.y < parameters.dimensions.height →
      added.connected.tiles.tiles[
        first.y * parameters.dimensions.width + first.x]? = some .floor →
      added.connected.tiles.tiles[
        second.y * parameters.dimensions.width + second.x]? = some .floor →
      FloorReachable added.connected.tiles.tiles parameters.dimensions first second := by
  simp only
  exact (rooted.addRoom candidate horizontalFirst).connected.allFloorsConnected

end Gimlight.MapGeneration
