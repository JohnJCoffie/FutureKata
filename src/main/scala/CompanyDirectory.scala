import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

case class Employee(name: String, department: String, yearsOfExperience: Int, manager: Option[Employee])

trait CompanyDirectoryRepository {
  def fetchDepartments(): Future[List[String]]
  def fetchEmployees(department: String): Future[List[Employee]]
}

trait CompanyDirectoryService {
  def employeeNames(department: String): Future[List[String]]
  def departmentTotalExperience: String => Future[Int]
  def totalExperience: Future[Int]
}

class AcmeCorpDirectoryService(directoryRepo: CompanyDirectoryRepository) extends CompanyDirectoryService {

  def employeeNames(department: String): Future[List[String]] = ???

  def departmentTotalExperience: (String) => Future[Int] = ???

  def totalExperience: Future[Int] = ???

}