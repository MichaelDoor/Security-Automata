import scala.collection.mutable._;
object LBACPolicy {

  type U = String; // User
  type O = String; // Object
  type L = String; // Label
  type S = Object; // Session

  // Stable
  var users = Set[U]("alice", "bob");
  var label_users = Map[U,L]("alice" -> "A", "bob" -> "B");

  // Dynamic
  var objects = Set[O]();
  var sessions = Set[S]();

  var object_labels = Map[O,L]();
  var session_labels = Map[S,L]();

  // Functions
  def read(s:S, o:O) {require(session_labels(s) >= object_labels(o));}
  def write(s:S, o:O) {require(session_labels(s) <= object_labels(o));}

  def startSession(u:U, l:L): Unit = {
    require(l <= label_users(u));
    var s = new S();
    sessions += s;
    session_labels += s -> l;
  }

  def addObject(s:S, o:O, l:L): Unit = {
    require(!objects.contains(o) & l >= session_labels(s));
    objects += o;
    object_labels += o -> l;
  }
}
