import scala.collection.mutable._;
object ACwFriend {

  type U = String; // Users
  type O = String; // Objects

  var users = Set[U]();
  var objects = Set[O]();

  var ownerOf = Map[O,U]();
  var friendOf = Map[U,Set[U]]();
  var frequestsOf = Map[U,Set[U]]();

  def read(u:U, o:O): Unit = {
    require(ownerOf(o) == u | friendOf(ownerOf(o)).contains(u));
  }

  def write(u:U, o:O): Unit = {
    require(ownerOf(o) == u);
  }

  def sendFRequest(u:U, f:U): Unit = {
    require(f != u & !friendOf(f).contains(u) & !frequestsOf(f).contains(u));
    frequestsOf(f) += u;
  }

  def acceptFRequest(u:U, f:U): Unit = {
    friendOf(u) += f;
    frequestsOf(u) -= f;
  }

  def rejectFRequest(u:U, f:U): Unit = {
    frequestsOf(u) -= f;
  }

}
