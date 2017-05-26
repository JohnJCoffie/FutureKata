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

  def employeeNames(department: String): Future[List[String]] = for {
    employees <- directoryRepo.fetchEmployees(department)
    names = for {
      employee <- employees
    } yield employee.name
  } yield names

  def departmentTotalExperience: String => Future[Int] =
    department => for {
      employees <- directoryRepo.fetchEmployees(department)
      experience = for {
        employee <- employees
      } yield employee.yearsOfExperience
    } yield experience.sum

  def totalExperience: Future[Int] =
    for {
      departmentNames <- directoryRepo.fetchDepartments
      departmentsFutureExperience = for {
        departmentName <- departmentNames
        departmentExperience = for {
          departmentEmployees <- directoryRepo.fetchEmployees(departmentName)
          departmentExperience = for {
            employee <- departmentEmployees
          } yield employee.yearsOfExperience
        } yield departmentExperience.sum
      } yield departmentExperience
      futureDepartmentsExperience <- Future.sequence(departmentsFutureExperience)
    } yield futureDepartmentsExperience.sum
}