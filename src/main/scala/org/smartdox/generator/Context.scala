package org.smartdox.generator

import java.util.Locale
import org.goldenport.context.ContextFoundation
import org.goldenport.i18n.I18NContext
import org.goldenport.recorder.{ForwardRecorder, Recorder}
import org.goldenport.cli.Environment
import org.goldenport.tree.TreeTransformer
import org.goldenport.realm.Realm
import org.smartdox.Dox
import org.smartdox.service

/*
 * @since   Jul.  5, 2020
 *  version Oct. 11, 2020
 *  version Nov. 23, 2020
 *  version Apr. 30, 2025
 *  version May.  2, 2025
 * @version Jun.  7, 2025
 * @author  ASAMI, Tomoharu
 */
case class Context(
  environment: Environment,
  config: Config,
  contextFoundation: ContextFoundation,
  targetI18NContextOption: Option[I18NContext] = None
) extends Environment.AppEnvironment with ForwardRecorder with ContextFoundation.Holder {
  protected def forward_Recorder: Recorder = recorder
  private def recorder = environment.recorder

  def targetI18NContext = targetI18NContextOption getOrElse contextFoundation.i18NContext

  val realmContext: TreeTransformer.Context[Realm.Data] =
    TreeTransformer.Context.default[Realm.Data].
      withI18NContext(targetI18NContext)
  val doxContext: TreeTransformer.Context[Dox] =
    TreeTransformer.Context.default[Dox].
      withI18NContext(targetI18NContext)
  val unitContext: TreeTransformer.Context[Unit] =
    TreeTransformer.Context.default[Unit].
      withI18NContext(targetI18NContext)

  def withTargetI18NContext(p: Locale) = copy(targetI18NContextOption = Some(contextFoundation.i18NContext.withLocale(p)))
}

object Context {
  trait Holder {
    def context: Context

    protected final def context_realm = context.realmContext
    protected final def context_dox = context.doxContext
  }

  def create(): Context = create(Array[String]())

  def create(args: Array[String]): Context = create(Environment.create(args))

  def create(p: Environment): Context = new Context(p, Config(p.config), p.contextFoundation)

  def create(p: service.Context): Context = new Context(p.environment, Config.create(p.config), p.contextFoundation)
}
