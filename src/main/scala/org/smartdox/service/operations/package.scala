package org.smartdox.service

import scala.util.matching.Regex
import java.io.File
import org.goldenport.context.Consequence
import org.goldenport.cli.Request
import org.goldenport.cli.spec
import org.goldenport.tree.TreeTransformer
import org.smartdox.doxsite.DoxSite

/*
 * @since   Jun.  3, 2025
 * @version Jun. 16, 2025
 * @author  ASAMI, Tomoharu
 */

package object operations {
  trait Command {
  }

  trait Result {
  }

  case class SiteParameters(
    in: File,
    strategy: Option[DoxSite.Strategy],
    outputScopePolicy: Option[TreeTransformer.Config.Scope.Policy],
    target: Option[List[Regex]]
  )
  object SiteParameters {
    trait Holder {
      def siteParameters: SiteParameters

      def in: File = siteParameters.in
      def strategy: Option[DoxSite.Strategy] = siteParameters.strategy
      def outputScopePolicy: Option[TreeTransformer.Config.Scope.Policy] = siteParameters.outputScopePolicy
      def target: Option[List[Regex]] = siteParameters.target
    }

    trait Specification {
      val in = spec.Parameter.argumentFile("in")
      val strategy = spec.Parameter.propertyPowertypeOption(DoxSite.Strategy, "strategy")
      val outputScopePolicy = spec.Parameter.propertyPowertypeOption(TreeTransformer.Config.Scope.Policy, "output.scope.policy")
      val target = spec.Parameter.propertyRegexSequence("target")
    }

    object params extends Specification

    def createC(req: Request): Consequence[SiteParameters] =
      for {
        in <- req.cFile(params.in)
        strategy <- req.cPowertypeOption(params.strategy)
        outputscopepolicy <- req.cPowertypeOption(params.outputScopePolicy)
        target <- req.cRegexListOption(params.target)
      } yield {
        SiteParameters(in, strategy, outputscopepolicy, target)
      }

    def request: spec.Request = spec.Request(
      params.in,
      params.strategy,
      params.outputScopePolicy,
      params.target
    )
  }
}
