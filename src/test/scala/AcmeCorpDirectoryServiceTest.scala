import org.scalatest.{FunSuite, Matchers}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class AcmeCorpDirectoryServiceTest extends FunSuite with Matchers {

  test("Fetch names of all HR employees") {
    new TestContext {
      val directoryService = new AcmeCorpDirectoryService(directoryRepoDouble)
      Await.result(
        directoryService.employeeNames("HR"), 1 seconds) shouldBe List("James")
    }
  }

  test("Compute total experience of the Accounting department") {
    new TestContext {
      val directoryService = new AcmeCorpDirectoryService(directoryRepoDouble)
      Await.result(
        directoryService.departmentTotalExperience("Accounting"), 1 seconds) shouldBe 15
    }
  }

  test("Compute total experience of the entire company") {
    new TestContext {
      val directoryService = new AcmeCorpDirectoryService(directoryRepoDouble)
      Await.result(
        directoryService.totalExperience, 1 seconds) shouldBe 44
    }
  }

  trait TestContext {
    lazy val directoryRepoDouble = new CompanyDirectoryRepository {
      val patricia = Employee("Patricia", "Accounting", 12, None)
      val robert = Employee("Robert", "Marketing", 8, None)
      val james = Employee("James", "HR", 5, None)
      val john = Employee("John", "Accounting", 3, Option(patricia))
      val mary = Employee("Mary", "IT", 7, None)
      val jennifer = Employee("Jennifer", "Marketing", 9, Option(robert))

      val employeeTable: List[Employee] = List(
        james,
        john,
        robert,
        mary,
        patricia,
        jennifer
      )

      def fetchDepartments(): Future[List[String]] =
        Future {
          employeeTable.map(_.department).distinct
        }

      def fetchEmployees(department: String): Future[List[Employee]] =
        Future {
          employeeTable.filter(e => e.department == department)
        }
    }

  }
}
