object ResourceAC {

  type U = String; // User
  type R = String; // Resource

  // Stable
  var users = Set[U]();
  var resources = Set[R]();

  // Dynamic
  var subs = Map[(U,R),Int]();

  def pay(u:U, r:R): Unit = {
    subs((u,r)) += 10;
  }

  def use(u:U, r:R): Unit = {
    require(subs((u,r)) > 0);
    subs((u,r)) -= 1;
  }

  def transfer(u:U, n:Int, s:U, r:R): Unit = {
    require(n > 0 & subs((u,r)) >= n);
    subs((u,r)) -= n;
    subs((s,r)) += n;
  }

  // Admin
  def addResource(r:R): Unit = {
    require(!resources.contains(r));
    resources += r;
    subs ++ users.map((x:U) => (x,r) -> 0);
  }

  def addUser(u:U): Unit = {
    require(!users.contains(u));
    users += u;
  }
}
