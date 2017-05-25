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

  //alt impls
  def futureExperience2: Future[Int] = for {
    departmentNames <- directoryRepo.fetchDepartments
    departmentsExperience <- Future.sequence(
      for {
        departmentName <- departmentNames
      } yield departmentTotalExperience(departmentName))
  } yield departmentsExperience.sum

  def futureExperience3: Future[Int] =
    for {
      departmentNames <- directoryRepo.fetchDepartments
      departmentsExperience <- Future.sequence(
        departmentNames.map(departmentTotalExperience(_)))
    } yield departmentsExperience.sum

  def futureExperience4: Future[Int] =
    for {
      departmentNames <- directoryRepo.fetchDepartments
      departmentsExperience <- Future.sequence(
        for {
          departmentName <- departmentNames
        } yield for {
          employees <- directoryRepo.fetchEmployees(departmentName)
          experience = for {
            employee <- employees
          } yield employee.yearsOfExperience
        } yield experience.sum)
    } yield departmentsExperience.sum

  def futureExperience5: Future[Int] =
    for {
      departmentNames <- directoryRepo.fetchDepartments
      departmentsExperience <- Future.sequence(
        departmentNames.map(departmentName => for {
          employees <- directoryRepo.fetchEmployees(departmentName)
          experience = employees.map(_.yearsOfExperience)
        } yield experience.sum))
    } yield departmentsExperience.sum

  def futureExperience6: Future[Int] =
    for {
      departmentNames <- directoryRepo.fetchDepartments
      departmentsExperience <- Future.traverse(departmentNames)(departmentName => for {
        employees <- directoryRepo.fetchEmployees(departmentName)
        experience = employees.map(_.yearsOfExperience)
      } yield experience.sum)
    } yield departmentsExperience.sum

  def futureExperience7: Future[Int] =
    for {
      departmentNames <- directoryRepo.fetchDepartments
      employees: List[List[Employee]] <- Future.traverse(departmentNames)(directoryRepo.fetchEmployees)
      experience: List[Int] <- Future.traverse(employees.flatten)(e => Future {e.yearsOfExperience})
    } yield experience.sum

  def futureExperience8: Future[Int] =
    for {
      departmentNames <- directoryRepo.fetchDepartments
      employees: List[List[Employee]] <- Future.traverse(departmentNames)(directoryRepo.fetchEmployees)
      experience: List[Int] = employees.flatten.map(_.yearsOfExperience)
    } yield experience.sum

  def futureExperience9: Future[Int] =
    for {
      departmentNames <- directoryRepo.fetchDepartments
      employees: List[List[Employee]] <- Future.traverse(departmentNames)(directoryRepo.fetchEmployees)
    } yield employees.flatten.map(_.yearsOfExperience).sum
}