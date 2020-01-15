import scala.collection.mutable._;
object DACPolicy {
  type U = String; // Users
  type O = String; // Objects
  type A = String; // Actions
  type R = (U,O,A); // Rights

  // INVARIANT: voor een right zijn de user en object aanwezig in hun respectievelijke sets.

  var users = Set[U]();
  var objects = Set[O]();
  var rights = Set[R]();
  var ownerOf = Map[O,U]();

  def read(u:U, o:O) {require(rights.contains((u,o,"Read")));}
  def write(u:U, o:O) {require(rights.contains((u,o,"Write")));}

  def addRight(s:U, r:R): Unit = {
    r match {case (u,o,a) =>
      require(users.contains(u) & objects.contains(o) & ownerOf(o) == s);
      rights += r;
    }
  }

  def deleteRight(s:U, r:R): Unit = {
    r match {case (u,o,a) =>
      require(ownerOf(o) == s);
      rights -= r;
    }
  }

  def addObject(u:U, o:O): Unit = {
    require(!objects.contains(o));
    objects += o;
    ownerOf += (o -> u);
  }

  def delObject(u:U, o:O): Unit = {
    require(objects.contains(o) & ownerOf(o) == u);
    objects -= o;
    ownerOf -= o;
    rights.retain({case(s,o2,a) => o2 != o});
  }

  // admin functions
  def addUser(u:U): Unit = {
    require(!users.contains(u)); // admin rights
    users += u;
  }

}
