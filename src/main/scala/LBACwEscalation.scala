import scala.collection.mutable._;

// Start session at chosen level, escalate when necessary and possible
object LBACwEscalation {

  type U = String; // User
  type O = String; // Object
  type L = String; // Label
  type S = Object; // Session

  // Stable
  var users = Set[U]();
  var objects = Set[O]();
  var user_label = Map[U,L]();

  // Dynamic
  var sessions = Set[S]();
  var session_label = Map[S,L]();
  var object_label = Map[O,L]();
  var session_user = Map[S,U]();

  def createSession(u:U, l:L): Unit = {
    require(l <= user_label(u));
    var s = new S();
    sessions += s;
    session_label += s -> l;
  }

  def delSession(s:S): Unit = {
    require(sessions.contains(s));
    sessions -= s;
    session_label -= s;
    session_user -= s;
  }

  def read(s:S, o:O): Unit = {
    require(object_label(o) <= user_label(session_user(s)));
    if (session_label(s) < object_label(o)) {
      createSession(session_user(s), object_label(o))
      delSession(s);
    };
  }

  def write(s:S, o:O, l:L): Unit = {
    require(object_label(o) >= session_label(s));
  }

  def createObject(s:S, o:O, l:L): Unit = {
    require(!objects.contains(o) & session_label(s) <= l);
    objects += o;
    object_label += o -> l;
  }

}
