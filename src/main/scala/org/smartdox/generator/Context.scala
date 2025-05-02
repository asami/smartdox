package org.smartdox.generator

import org.goldenport.context.ContextFoundation
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
 * @version May.  2, 2025
 * @author  ASAMI, Tomoharu
 */
class Context(
  val environment: Environment,
  val config: Config,
  val contextFoundation: ContextFoundation
) extends Environment.AppEnvironment with ForwardRecorder with ContextFoundation.Holder {
  protected def forward_Recorder: Recorder = recorder
  private def recorder = environment.recorder

  val realmContext: TreeTransformer.Context[Realm.Data] = TreeTransformer.Context.default[Realm.Data]
  val doxContext: TreeTransformer.Context[Dox] = TreeTransformer.Context.default[Dox]
  val unitContext: TreeTransformer.Context[Unit] = TreeTransformer.Context.default[Unit]
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
