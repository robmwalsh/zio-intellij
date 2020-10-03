package zio.intellij.inspections

import com.intellij.codeInspection.ProblemsHolder
import com.intellij.psi.PsiElement
import javax.swing.JComponent
import org.jetbrains.plugins.scala.codeInspection.collections.OperationOnCollectionInspectionBase.SimplifiableExpression
import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.util.IntentionAvailabilityChecker
import zio.intellij.utils.Version

abstract class ZInspection(simplifiers: SimplificationType*) extends OperationOnCollectionInspection {
  def availableSince: Version = Version.ZIO.`1.0.0`

  final override def getLikeCollectionClasses: Array[String] = Array("zio.ZIO")

  final override def createOptionsPanel(): JComponent = null // god help me

  final override def possibleSimplificationTypes: Array[SimplificationType] = simplifiers.toArray

  protected override def actionFor(implicit
    holder: ProblemsHolder,
    isOnTheFly: Boolean
  ): PartialFunction[PsiElement, Any] = {
    case SimplifiableExpression(expr) if IntentionAvailabilityChecker.checkInspection(this, expr) =>
      simplifications(expr).foreach {
        case s @ Simplification(toReplace, _, hint, rangeInParent) =>
          val quickFix = OperationOnCollectionQuickFix(s)
          holder.registerProblem(toReplace.getElement, hint, highlightType, rangeInParent, quickFix)
      }
  }

  private def simplifications(expr: ScExpression): Array[Simplification] = {
    def simplificationTypes = for {
      (st, idx) <- possibleSimplificationTypes.zipWithIndex
      if simplificationTypesEnabled(idx)
    } yield st

    simplificationTypes.flatMap(st => st.getSimplifications(expr) ++ st.getSimplification(expr))
  }

}
