package org.smartdox.service

import scala.util.matching.Regex
import java.io.File
import org.goldenport.context.Consequence
import org.goldenport.cli.Request
import org.goldenport.cli.spec
import org.goldenport.realm.Realm
import org.goldenport.realm.Realm.FileData
import org.goldenport.tree.TreeTransformer
import org.goldenport.util.StringUtils
import org.smartdox.doxsite.DoxSite

/*
 * @since   Jun.  3, 2025
 *  version Apr.  9, 2026
 * @version May. 13, 2026
 * @author  ASAMI, Tomoharu
 */

package object operations {
  trait Command {
  }

  trait Result {
  }

  case class SiteParameters(
    in: File,
    publish: Option[File],
    strategy: Option[DoxSite.Strategy],
    outputScopePolicy: Option[TreeTransformer.Config.Scope.Policy],
    target: Option[List[Regex]]
  )
  object SiteInputRealm {
    def create(p: SiteParameters.Holder): Realm =
      create(p.in)

    def create(p: SiteParameters): Realm =
      create(p.in)

    def create(in: File): Realm =
      if (in.isDirectory)
        Realm.create(DoxSite.realmConfig, in)
      else
        _from_file(in)

    private def _from_file(in: File): Realm = {
      val b = Realm.Builder()
      val suffix = StringUtils.toSuffix(in.getName.toLowerCase)
      if (DoxSite.realmConfig.textSuffixes.contains(suffix))
        b.set(in.getName, scala.io.Source.fromFile(in).mkString)
      else
        b.cursor.set(in.getName, FileData(in))
      b.build()
    }
  }

  object SiteParameters {
    trait Holder {
      def siteParameters: SiteParameters

      def in: File = siteParameters.in
      def publish: Option[File] = siteParameters.publish
      def strategy: Option[DoxSite.Strategy] = siteParameters.strategy
      def outputScopePolicy: Option[TreeTransformer.Config.Scope.Policy] = siteParameters.outputScopePolicy
      def target: Option[List[Regex]] = siteParameters.target
    }

    trait Specification {
      val in = spec.Parameter.argumentFile("in")
      val publish = spec.Parameter.propertyFileOption("publish")
      val strategy = spec.Parameter.propertyPowertypeOption(DoxSite.Strategy, "strategy")
      val outputScopePolicy = spec.Parameter.propertyPowertypeOption(TreeTransformer.Config.Scope.Policy, "output.scope.policy")
      val target = spec.Parameter.propertyRegexSequence("target")
    }

    object params extends Specification

    def createC(req: Request): Consequence[SiteParameters] =
      for {
        in <- req.cFile(params.in)
        publish <- req.cFileOption(params.publish)
        strategy <- req.cPowertypeOption(params.strategy)
        outputscopepolicy <- req.cPowertypeOption(params.outputScopePolicy)
        target <- req.cRegexListOption(params.target)
      } yield {
        SiteParameters(in, _effective_publish(publish), strategy, outputscopepolicy, target)
      }

    def request: spec.Request = spec.Request(
      params.in,
      params.publish,
      params.strategy,
      params.outputScopePolicy,
      params.target
    )

    private def _effective_publish(p: Option[File]): Option[File] =
      p.orElse {
        val default = new File("publish.d")
        if (default.exists) Some(default) else None
      }
  }
}
