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
 * @version Jun. 19, 2026
 * @author  ASAMI, Tomoharu
 */

package object operations {
  trait Command {
  }

  trait Result {
  }

  case class SiteParameters(
    in: File,
    publication: Option[File],
    strategy: Option[DoxSite.Strategy],
    outputScopePolicy: Option[TreeTransformer.Config.Scope.Policy],
    target: Option[List[Regex]],
    publicationRepository: Option[File] = None,
    publicationRdfMissingPolicy: Option[String] = None
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
      def publication: Option[File] = siteParameters.publication
      def strategy: Option[DoxSite.Strategy] = siteParameters.strategy
      def outputScopePolicy: Option[TreeTransformer.Config.Scope.Policy] = siteParameters.outputScopePolicy
      def target: Option[List[Regex]] = siteParameters.target
      def publicationRepository: Option[File] = siteParameters.publicationRepository
      def publicationRdfMissingPolicy: Option[String] = siteParameters.publicationRdfMissingPolicy
    }

    trait Specification {
      val in = spec.Parameter.argumentFile("in")
      val publication = spec.Parameter.propertyFileOption("publication")
      val strategy = spec.Parameter.propertyPowertypeOption(DoxSite.Strategy, "strategy")
      val outputScopePolicy = spec.Parameter.propertyPowertypeOption(TreeTransformer.Config.Scope.Policy, "output.scope.policy")
      val target = spec.Parameter.propertyRegexSequence("target")
      val publicationRepository = spec.Parameter.propertyFileOption("publication.repository")
      val publicationRdfMissingPolicy = spec.Parameter.property("publication.rdf.missing.policy")
    }

    object params extends Specification

    def createC(req: Request): Consequence[SiteParameters] =
      for {
        in <- req.cFile(params.in)
        publication <- req.cFileOption(params.publication)
        strategy <- req.cPowertypeOption(params.strategy)
        outputscopepolicy <- req.cPowertypeOption(params.outputScopePolicy)
        target <- req.cRegexListOption(params.target)
        publicationrepository <- req.cFileOption(params.publicationRepository)
      } yield {
        SiteParameters(in, _effective_publication(publication), strategy, outputscopepolicy, target, publicationrepository, _string_option(req, params.publicationRdfMissingPolicy.name))
      }

    def request: spec.Request = spec.Request(
      params.in,
      params.publication,
      params.strategy,
      params.outputScopePolicy,
      params.target,
      params.publicationRepository,
      params.publicationRdfMissingPolicy
    )


    private def _string_option(req: Request, name: String): Option[String] =
      req.properties.find(_.name == name).map(_.asString).map(_.trim).filter(_.nonEmpty)

    private def _effective_publication(p: Option[File]): Option[File] =
      p.orElse {
        val default = new File("src/main/publication")
        if (default.exists) Some(default) else None
      }
  }
}
