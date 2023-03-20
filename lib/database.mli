module type EntitySet = sig
  type 'a t
  val contains : 'a -> bool
  val add : 'a -> 'a t
  val remove : 'a -> 'a t
end

module type RelationSet = sig
  type 'a t

end
module type Database = sig
end