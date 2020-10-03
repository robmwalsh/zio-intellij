package zio.intellij.inspections

import com.intellij.codeInspection.InspectionProfileEntry
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.util.IntentionAvailabilityChecker
import zio.intellij.utils.ModuleSyntax
import zio.intellij.utils.TypeCheckUtils.fromZioLike

class AvailabilityChecker extends IntentionAvailabilityChecker {
  override def isInspectionAvailable(inspectionProfileEntry: InspectionProfileEntry, element: PsiElement): Boolean =
    (for {
      inspection <- Option(inspectionProfileEntry).collect { case z: ZInspection => z }
      module     <- Option(ScalaPsiUtil.getModule(element))
      zioVersion <- module.zioVersion
      if zioVersion >= inspection.availableSince
    } yield true).getOrElse(false)

  override def canCheck(element: PsiElement): Boolean =
    element match {
      case expr: ScExpression if fromZioLike(expr) => true
      case tpe: ScType if fromZioLike(tpe)         => true
      case _                                       => false
    }
}
