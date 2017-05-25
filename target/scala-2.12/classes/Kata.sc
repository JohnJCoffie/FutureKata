import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps




/*

With the above functions, solve the following problems

1.) Create a Future of a List the names of each employee in HR

2.) Compute the Future total yearsOfExperience of the Marketing department

3.) Create a Map of Department Name -> Future total yearsOfExperience of department

4.) Create a Future total yearsOfExperience of all employees
*/

def bosses: Future[List[Employee]] = for {
  hrEmployees <- CompanyDirectoryRepositoryDouble.fetchEmployees("Marketing")
  names = for {
    employee <- hrEmployees
    if !employee.manager.isEmpty
  } yield employee.manager.get
} yield names

val bossList = Await.result(bosses, 1 seconds)

def futureHRNames: Future[List[String]] = for {
  hrEmployees <- CompanyDirectoryRepositoryDouble.fetchEmployees("HR")
  names = for {
    employee <- hrEmployees
  } yield employee.name
} yield names

val namesInHR = Await.result(futureHRNames, 1 seconds)

def futureMarketingExperience = for {
  hrEmployees <- CompanyDirectoryRepositoryDouble.fetchEmployees("Marketing")
  experience = for {
    employee <- hrEmployees
  } yield employee.yearsOfExperience
} yield experience.sum

val totalExperienceInHR = Await.result(futureMarketingExperience, 1 seconds)


def futureDepartmentExperience: String => Future[Int] =
  department => for {
    hrEmployees <- CompanyDirectoryRepositoryDouble.fetchEmployees(department)
    experience = for {
      employee <- hrEmployees
    } yield employee.yearsOfExperience
  } yield experience.sum

Await.result(futureDepartmentExperience("IT"), 1 seconds)

def futureExperience1: Future[Int] =
  for {
    departmentNames <- CompanyDirectoryRepositoryDouble.fetchDepartments
    departmentsFutureExperience = for {
      departmentName <- departmentNames
      departmentExperience = for {
        departmentEmployees <- CompanyDirectoryRepositoryDouble.fetchEmployees(departmentName)
        departmentExperience = for {
          employee <- departmentEmployees
        } yield employee.yearsOfExperience
      } yield departmentExperience.sum
    } yield departmentExperience
    futureDepartmentsExperience <- Future.sequence(departmentsFutureExperience)
  } yield futureDepartmentsExperience.sum

val totalExperience1 = Await.result(futureExperience1, 1 seconds)

def futureExperience2: Future[Int] =
  for {
    departmentNames <- CompanyDirectoryRepositoryDouble.fetchDepartments
    departmentsExperience <- Future.sequence(
      for {
        departmentName <- departmentNames
      } yield futureDepartmentExperience(departmentName))
  } yield departmentsExperience.sum

val totalExperience2 = Await.result(futureExperience2, 1 seconds)

def futureExperience3: Future[Int] =
  for {
    departmentNames <- CompanyDirectoryRepositoryDouble.fetchDepartments
    departmentsExperience <- Future.sequence(
      departmentNames.map(futureDepartmentExperience(_)))
  } yield departmentsExperience.sum

val totalExperience3 = Await.result(futureExperience3, 1 seconds)

def futureExperience4: Future[Int] =
  for {
    departmentNames <- CompanyDirectoryRepositoryDouble.fetchDepartments
    departmentsExperience <- Future.sequence(
      for {
        departmentName <- departmentNames
      } yield for {
        hrEmployees <- CompanyDirectoryRepositoryDouble.fetchEmployees(departmentName)
        experience = for {
          employee <- hrEmployees
        } yield employee.yearsOfExperience
      } yield experience.sum)
  } yield departmentsExperience.sum

val totalExperience4 = Await.result(futureExperience4, 1 seconds)


def futureExperience5: Future[Int] =
  for {
    departmentNames <- CompanyDirectoryRepositoryDouble.fetchDepartments
    departmentsExperience <- Future.sequence(
      departmentNames.map(departmentName => for {
        hrEmployees <- CompanyDirectoryRepositoryDouble.fetchEmployees(departmentName)
        experience = hrEmployees.map(_.yearsOfExperience)
      } yield experience.sum))
  } yield departmentsExperience.sum

val totalExperience5 = Await.result(futureExperience5, 1 seconds)

def futureExperience6: Future[Int] =
  for {
    departmentNames <- CompanyDirectoryRepositoryDouble.fetchDepartments
    departmentsExperience <- Future.traverse(departmentNames)(departmentName => for {
      hrEmployees <- CompanyDirectoryRepositoryDouble.fetchEmployees(departmentName)
      experience = hrEmployees.map(_.yearsOfExperience)
    } yield experience.sum)
  } yield departmentsExperience.sum

val totalExperience6 = Await.result(futureExperience6, 1 seconds)

def futureExperience7: Future[Int] =
  for {
    departmentNames <- CompanyDirectoryRepositoryDouble.fetchDepartments
    employees: List[List[Employee]] <- Future.traverse(departmentNames)(CompanyDirectoryRepositoryDouble.fetchEmployees)
    experience: List[Int] <- Future.traverse(employees.flatten)(e => Future {e.yearsOfExperience})
  } yield experience.sum

val totalExperience7 = Await.result(futureExperience7, 1 seconds)

def futureExperience8: Future[Int] =
  for {
    departmentNames <- CompanyDirectoryRepositoryDouble.fetchDepartments
    employees: List[List[Employee]] <- Future.traverse(departmentNames)(CompanyDirectoryRepositoryDouble.fetchEmployees)
    experience: List[Int] = employees.flatten.map(_.yearsOfExperience)
  } yield experience.sum

val totalExperience8 = Await.result(futureExperience8, 1 seconds)


def futureExperience9: Future[Int] =
  for {
    departmentNames <- CompanyDirectoryRepositoryDouble.fetchDepartments
    employees: List[List[Employee]] <- Future.traverse(departmentNames)(CompanyDirectoryRepositoryDouble.fetchEmployees)
  } yield employees.flatten.map(_.yearsOfExperience).sum

val totalExperience9 = Await.result(futureExperience9, 1 seconds)
