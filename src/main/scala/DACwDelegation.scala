import scala.collection.mutable._;
object DACwDelegation {

  type U = String; // User
  type O = String; // Object
  type A = String; // Action
  type R = (U,O,A); // Right

  var users = Set[U]();
  var objects = Set[O]();
  var rights = Set[R]();

  var ownerOf = Map[O,U]();
  var delegateOf = Set[(O,U)]();

  def read(u:U, o:O): Unit = {
    require(rights.contains((u,o,"read")));
  }

  def write(u:U, o:O): Unit = {
    require(rights.contains((u,o,"write")));
  }

  def addObject(u:U, o:O): Unit = {
    require(!objects.contains(o));
    objects += o;
    ownerOf += o -> u;
  }

  def delObject(u:U, o:O): Unit = {
    require(objects.contains(o) & ownerOf(o) == u);
    objects -= o;
    ownerOf -= o;
    rights.retain({case (s,o2,a) => o2 != o});
    delegateOf.retain({case (o2,s) => o2 != o});
  }

  def addRight(u:U, r:R): Unit = {
    r match {
      case (s,o,a) =>
        require(ownerOf(o) == u | delegateOf.contains((o,u)));
        rights += r;
    }
  }

  def delRight(u:U, r:R): Unit = {
    r match {
      case (s,o,a) =>
        require(ownerOf(o) == u | delegateOf.contains((o,u)));
        rights -= r;
    }
  }

  def addDelegate(u:U, d:U, o:O): Unit = {
    require(ownerOf(o) == u);
    var delRel = (o,d);
    delegateOf += delRel;
  }

  def delDelegate(u:U, d:U, o:O): Unit = {
    require(ownerOf(o) == u);
    var delRel = (o,d);
    delegateOf -= delRel;
  }

  // Admin
  def addUser(u:U): Unit = {
    require(!users.contains(u));
    users += u;
  }
}
