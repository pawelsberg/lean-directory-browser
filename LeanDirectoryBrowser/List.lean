universe u
namespace List
  def splitAt {α : Type u} (l :List α) (n : Nat)  : (List α) × (List α) :=
    ⟨l.take n, l.drop n⟩
