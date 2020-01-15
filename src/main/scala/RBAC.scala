object RBACPolicy {

  type U = String; // User
  type R = String; // Role
  type P = String; // Permission
  type S = Object; // Session

  var users = Set[U]("alice", "bob");
  var roles = Set[R]("researcher", "teacher");
  var perms = Set[P]("accessLab", "accessClass");
  var sessions = Set[S]();

  var ur = Map[U,Set[R]]("alice" -> Set("researcher"), "bob" -> Set("researcher", "teacher"));
  var rp = Map[R,Set[P]]("researcher" -> Set("accessLab", "accessClass"), "teacher" -> Set("accessClass"));
  var sr = Map[S,Set[R]]();
  var su = Map[S,U]();

  def startSession(u:U, r:Set[R]): Unit = {
    require(!su.contains(u) & r.subsetOf(ur(u)));
    var s = new S();
    sessions += s;
    sr += s -> r;
    su += s -> u;
  }

  def checkAccess(s:S, p:P): Unit = {
    require(sr.contains((r:R) => rp(r).contains(p)));
  }

  def activateRole(s:S, r:R): Unit = {
    require(!sr(s).contains(r) & ur(su(s)).contains(r));
    sr(s) += r;
  }

  def dropRole(s:S, r:R): Unit = {
    require(sr(s).contains(r));
    sr(s) -= r;
  }
}
