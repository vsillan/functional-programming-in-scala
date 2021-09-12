import scala_book.State

sealed trait BookAction
case object Open extends BookAction
case object TurnNextPage extends BookAction
case object TurnPrevPage extends BookAction

case class Book(open: Boolean, pages: Int, currentPage: Int) {
  def update = (a: BookAction) => (s: Book) =>
    (a, s) match {
      case (Open, Book(false, pages, curPage)) => Book(true, pages, curPage)
    }

  def use(action: BookAction): State[Book, Int] = for {
  // s <- State.modify[Book](update(action))
    s <- State.get[Book]
  } yield (s.currentPage)
}