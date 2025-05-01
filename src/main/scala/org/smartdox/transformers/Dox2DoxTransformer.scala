package org.smartdox.transformers

import scalaz._, Scalaz._
import org.goldenport.parser._
import org.smartdox._
import org.smartdox.generator.Context
import org.smartdox.transformer._

/*
 * @since   Feb.  2, 2021
 * @version Feb.  3, 2021
 * @author  ASAMI, Tomoharu
 */
class Dox2DoxTransformer(
  context: Context,
  rule: Dox2DoxTransformer.Rule
) {
  import Dox2DoxTransformer._

  def transform(in: Dox): ParseResult[Dox] = for {
    root <- ParseResult(_get_root(in))
    r <- _transform(root)
  } yield r

  private def _transform(in: Option[Dox]): ParseResult[Dox] = in match {
    case Some(s) => _transform(s)
    case None => EmptyParseResult()
  }

  private def _transform(in: Dox): ParseResult[Dox] = 
    rule.resolve(in) match {
      case NoneResolved => _resolve(in, in.elements)
      case Resolved(r) => ParseResult(r)
      case Unresolved(r, cs) => _resolve(r, cs)
    }

  private def _get_root(p: Dox): Option[Dox] = rule.getRoot(p)

  private def _resolve(dox: Dox, ps: Seq[Dox]): ParseResult[Dox] = for {
    rs <- _resolve(ps)
    r <- ParseResult.create(dox.copyV(rs))
  } yield r

  private def _resolve(ps: Seq[Dox]): ParseResult[List[Dox]] =
    ps.toList.traverse(_transform)
}

object Dox2DoxTransformer {
  sealed trait RootStrategy {
    def getRoot(p: Dox): Option[Dox]

    protected final def to_div(p: Dox): Div = Div(p.elements)

    protected final def find_node(pf: PartialFunction[Dox, Dox])(p: Dox): Option[Dox] =
      if (pf.isDefinedAt(p))
        Some(pf.apply(p))
      else
        p.elements.toStream.flatMap(find_node(pf)).headOption
  }
  case object BodyDivRootStrategy extends RootStrategy {
    def getRoot(p: Dox): Option[Dox] = p match {
      case m: Document => getRoot(m.body)
      case m: Body => Some(to_div(m))
      case m => find_node({
        case m: Body => m
      })(p)
    }
  }

  sealed trait Resolver {
    def resolve(p: Dox): ResolveResult
  }
  case object NoneResolver extends Resolver {
    def resolve(p: Dox): ResolveResult = NoneResolved
  }

  sealed trait ResolveResult
  case object NoneResolved extends ResolveResult
  case class Resolved(resolved: Dox) extends ResolveResult
  case class Unresolved(unresolved: Dox, children: List[Dox]) extends ResolveResult

  case class Rule(
    rootStrategy: RootStrategy,
    resolver: Resolver
  ) {
    def getRoot(p: Dox): Option[Dox] = rootStrategy.getRoot(p)
    def resolve(p: Dox): ResolveResult = resolver.resolve(p)
  }
  object Rule {
    def apply(rootstrategy: RootStrategy): Rule = Rule(rootstrategy, NoneResolver)
  }

  def transform(rule: Rule, p: Dox): ParseResult[Dox] = {
    val ctx = Context.create()
    val tx = new Dox2DoxTransformer(ctx, rule)
    tx.transform(p)
  }
}
