import scala.collection.mutable._;
object DACwMulOwner {

  type U = String; // User
  type O = String; // Object
  type A = String; // Action
  type R = (U,O,A); // Rights

  var users = Set[U]();
  var objects = Set[O]();
  var rights = Set[R]();

  var ownerOf = Map[O,Set[U]]();

  def read(u:U, o:O): Unit = {
    require(rights.contains((u,o,"read")));
  }

  def write(u:U,o:O): Unit = {
    require(rights.contains((u,o,"write")));
  }

  def addObject(u:U, o:O): Unit = {
    require(!objects.contains(o));
    objects += o;
    ownerOf(o) += u;
  }

  def delObject(u:U, o:O): Unit = {
    require(objects.contains(o) & ownerOf(o).contains(u));
    objects -= o;
    rights.retain({case (s,o2,a) => o2 != o});
    ownerOf -= o;
  }

  def addRight(u:U, r:R): Unit = {
    r match {
      case (s,o,a) =>
        require(ownerOf(o).contains(u));
        rights += r;
    }
  }

  def delRight(u:U, r:R): Unit = {
    r match {
      case (s,o,a) =>
        require(ownerOf(o).contains(u));
        rights -= r;
    }
  }

  def addOwner(u:U, s:U, o:O): Unit = {
    require(ownerOf(o).contains(u));
    ownerOf(o) += s;
  }

  def removeOwner(u:U, s:U, o:O): Unit = {
    require(ownerOf(o).contains(s) & ownerOf(o).contains(u));
    ownerOf(o) -= s;
  }

  // Admin
  def addUser(u:U): Unit = {
    require(!users.contains(u));
    users += u;
  }

}
