import scala.collection.mutable._;

// ASSUMPTION: permanent COI, once you access an object, you can from then on never access any other objects with the same COI label

object ChineseWall {

  type U = String; // User
  type O = String; // Object
  type L = String; // COI label

  var users = Set[U]();
  var objects = Set[O]();

  var object_label = Map[O,L]();
  var user_labels = Map[U,Set[L]]();
  var user_objects = Map[U,Set[O]]();

  def access(u:U, o:O): Unit = {
    require(user_objects(u).contains(o) | !user_labels.contains(object_label(o)));
    user_labels(u) += object_label(o);
    user_objects(u) += o;
  }

  // Non permanent? -> Just undo what access did

}
