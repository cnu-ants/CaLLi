module type S = 
  sig
    type memty
    val transfer : Basicblock.t ->  memty ->  memty
  end
