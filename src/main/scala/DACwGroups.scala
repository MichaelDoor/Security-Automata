import scala.collection.mutable._;

// Discretionary access control with groups

object DACwGroupsPolicy {

  type U = String; // User
  type O = String; // Object
  type A = String; // Action
  type G = Set[U]; // Group
  type R = (G,O,A); // Right

  var users = Set[U]();
  var objects = Set[O]();
  var rights = Set[R]();
  var groups = Set[G]();

  var ownerOf = Map[O,U]();

  def read(u:U, o:O): Unit = {
    require(groups.filter((g:G) => g.contains(u)).exists((g: G) => rights.contains(g, o, "read")));
  }

  def write(u:U, o:O): Unit = {
    require(groups.filter((g:G) => g.contains(u)).exists((g:G) => rights.contains((g, o, "write"))));
  }

  def addObject(u:U, o:O): Unit = {
    require(!objects.contains(o));
    objects += o;
    ownerOf += o -> u;
  }

  def removeObject(u:U, o:O): Unit = {
    require(objects.contains(o) & ownerOf(o) == u);
    objects -= o;
    ownerOf -= o;
    rights.retain({case (s,o2,a) => o2 != o});
  }

  def addRight(u:U, r:R): Unit = {
    r match {
      case (g, o, a) =>
        require(groups.contains(g) & ownerOf(o) == u);
        rights += r;
    }
  }

  def delRight(u:U, r:R): Unit = {
    r match {
      case (g, o, a) =>
        require(ownerOf(o) == u);
        rights -= r;
    }
  }


  // Admin operations
  def addGroup(g:G): Unit = {
    require(!groups.contains(g) & g.subsetOf(users))
    groups += g;
  }

  def delGroup(g:G): Unit = {
    require(groups.contains(g));
    groups -= g;
  }

  def addUser(u:U): Unit = {
    require(!users.contains(u));
    users += u;
  }

}
