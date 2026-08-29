package org.smartdox.doxsite

import io.circe._
import org.goldenport.config.ConfigLoader
import org.goldenport.context.Consequence
import org.goldenport.i18n.I18NContext
import org.goldenport.realm.Realm
import org.goldenport.tree.TreeTransformer
import org.smartdox.semanticweb.Site.SiteMetadata
import DoxSite._

/*
 * @since   Aug. 30, 2026
 * @version Aug. 30, 2026
 * @author  ASAMI, Tomoharu
 */
private[doxsite] object DoxSiteConfigLoader {
  def config(
    inconfig: Config,
    realm: Realm,
    configname: Option[String]
  )(implicit ctx: I18NContext): DoxSite.Config = {
    config(realm, configname) + inconfig
  }

  def config(
    realm: Realm,
    configname: Option[String]
  )(implicit ctx: I18NContext): DoxSite.Config =
    configname match {
      case Some("site") => _named_config(realm, "site")
      case Some(n) => _named_config(realm, "site") + _named_config(realm, n)
      case None => DoxSite.Config.default
    }

  private def _named_config(
    realm: Realm,
    name: String
  )(implicit ctx: I18NContext): DoxSite.Config = {
      val c = for {
        json <- ConfigLoader.loadConfigJson(realm, name)
        simplemodelingorg <- _simplemodeling_org(json)
        output <- _tree_transformer_config(json.hcursor.downField("output").focus)
        sitemetadata <- _site_metadata(json.hcursor.downField("site").downField("metadata").focus)
        sitenavigation <- _site_navigation(json.hcursor.downField("site").downField("navigation").focus)
        siteoutput <- _site_output(json.hcursor.downField("site").downField("output").focus)
        siteheader <- _site_header(json.hcursor.downField("site").downField("header").focus)
      } yield {
        val config = Config(
          None,
          None,
          output,
          siteMetadata = sitemetadata,
          siteNavigation = sitenavigation,
          siteOutput = siteoutput,
          siteHeader = siteheader,
          simplemodelingOrg = simplemodelingorg,
          origin = realm.origin
        )
        if (simplemodelingorg)
          config.withSimpleModelingOrgCompatibility
        else
          config
      }
      c.take
    }

  private def _simplemodeling_org(json: Json): Consequence[Boolean] =
    json.hcursor.downField("simplemodelingorg").focus match {
      case Some(s) => Consequence run {
        s.as[Boolean] match {
          case Right(r) => Consequence.success(r)
          case Left(l) => Consequence.syntaxErrorFault(l.toString)
        }
      }
      case None => Consequence.success(false)
    }

  private def _site_metadata(json: Option[Json]): Consequence[SiteMetadata] =
    json match {
      case Some(s) => _site_metadata(s)
      case None => Consequence.success(SiteMetadata.empty)
    }

  private def _site_metadata(json: Json): Consequence[SiteMetadata] =
    Consequence run {
      json.as[SiteMetadata] match {
        case Right(r) => Consequence.success(r)
        case Left(l) => Consequence.syntaxErrorFault(l.toString)
      }
    }

  private def _site_navigation(json: Option[Json]): Consequence[Config.SiteNavigation] =
    json match {
      case Some(s) => _site_navigation(s)
      case None => Consequence.success(Config.SiteNavigation.default)
    }

  private def _site_navigation(json: Json): Consequence[Config.SiteNavigation] =
    Consequence run {
      val cursor = json.hcursor
      cursor.downField("mode").as[Option[Config.SiteNavigation.Mode]] match {
        case Right(mode) => Consequence.success(Config.SiteNavigation(mode.getOrElse(Config.SiteNavigation.default.mode)))
        case Left(l) => Consequence.syntaxErrorFault(l.toString)
      }
    }

  private def _site_output(json: Option[Json]): Consequence[Config.SiteOutput] =
    json match {
      case Some(s) => _site_output(s)
      case None => Consequence.success(Config.SiteOutput.default)
    }

  private def _site_output(json: Json): Consequence[Config.SiteOutput] =
    Consequence run {
      val cursor = json.hcursor
      val r = for {
        localemode <- cursor.downField("locale_mode").as[Option[Config.SiteOutput.LocaleMode]]
        defaultlocale <- cursor.downField("default_locale").as[Option[String]]
      } yield Config.SiteOutput(
        localemode.getOrElse(Config.SiteOutput.default.localeMode),
        defaultlocale.getOrElse(Config.SiteOutput.default.defaultLocale)
      )
      r match {
        case Right(s) => Consequence.success(s)
        case Left(l) => Consequence.syntaxErrorFault(l.toString)
      }
    }

  private def _site_header(json: Option[Json]): Consequence[Config.SiteHeader] =
    json match {
      case Some(s) => _site_header(s)
      case None => Consequence.success(Config.SiteHeader.default)
    }

  private def _site_header(json: Json): Consequence[Config.SiteHeader] =
    Consequence run {
      json.hcursor.downField("language_toggle").as[Option[Boolean]] match {
        case Right(s) => Consequence.success(Config.SiteHeader(s.getOrElse(Config.SiteHeader.default.languageToggle)))
        case Left(l) => Consequence.syntaxErrorFault(l.toString)
      }
    }

  private def _tree_transformer_config(json: Option[Json]): Consequence[Option[TreeTransformer.Config]] =
    json match {
      case Some(s) => _tree_transformer_config(s)
      case None => Consequence.success(None)
    }

  private def _tree_transformer_config(json: Json): Consequence[Option[TreeTransformer.Config]] = {
    json.hcursor.downField("scope").downField("policy").focus match {
      case Some(s) => s.as[TreeTransformer.Config.Scope.Policy] match {
        case Right(r) => Consequence.success(Some(TreeTransformer.Config(TreeTransformer.Config.Scope(r))))
        case Left(l) => Consequence.success(None)
      }
      case None => Consequence.success(None)
    }
  }

  private def _doxsite_transformer_config(json: Option[Json]): Consequence[DoxSiteTransformer.Config] =
    json match {
      case Some(s) => _doxsite_transformer_config(s)
      case None => Consequence.success(DoxSiteTransformer.Config.default)
    }

  private def _doxsite_transformer_config(json: Json): Consequence[DoxSiteTransformer.Config] =
    Consequence run {
      json.as[DoxSiteTransformer.Config] match {
        case Right(r) => Consequence.success(r)
        case Left(l) => Consequence.syntaxErrorFault(l.toString)
      }
    }
    // configname.fold(DoxSiteTransformer.Config.default) { n =>
    //   val c = for {
    //     json <- ConfigLoader.loadConfigJson(realm, n)
    //     r <- Consequence run {
    //       json.as[DoxSiteTransformer.Config] match {
    //         case Right(r) => Consequence.success(r)
    //         case Left(l) => Consequence.syntaxErrorFault(l.toString)
    //       }
    //     }
    //   } yield r
    //   c.take
    // }
}
