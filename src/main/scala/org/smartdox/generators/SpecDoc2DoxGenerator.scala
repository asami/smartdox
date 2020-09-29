package org.smartdox.generators

// import java.io.{InputStream, OutputStream, IOException}
// import scala.collection.mutable.{ArrayBuffer, HashMap}
// import org.goldenport.entity.content._
// import org.goldenport.entity.GEntityContext
// import org.goldenport.entities.graphviz._
// import org.simplemodeling.SimpleModeler.entity._
// import org.simplemodeling.SimpleModeler.entity.flow._
// import org.simplemodeling.SimpleModeler.entity.business._
// import org.simplemodeling.dsl.SExecutionStep
// import org.simplemodeling.dsl.SStep
// import org.goldenport.Strings

import com.asamioffice.goldenport.text.UJavaString
import org.goldenport.realm.Realm
import org.goldenport.tree.TreeNode
import org.smartdox._
import org.smartdox.specdoc._
import org.smartdox.generator._
import org.smartdox.builder.DoxRealmBuilder

/*
 * Derived from SpecDoc2SmartDocRealmGenerator.
 *
 * @since   Oct.  6, 2008
 *  version Jul. 15, 2010
 *  version Jun. 21, 2020
 *  version Jul. 26, 2020
 * @version Aug. 13, 2020
 * @author  ASAMI, Tomoharu
 */
class SpecDoc2DoxGenerator(
  val context: Context
) extends GeneratorBase {
  protected val title_summary = "概要" // XXX
  protected val title_feature = "特性" // XXX
  protected val title_description = "説明" // XXX
  protected val title_history = "履歴" // XXX

  class Generator(val result: Realm = Realm.createEmpty()) extends Realm.Visitor {

    override def enter(p: TreeNode[Realm.Data]): Unit = {
      p.content match {
        case m: SpecDoc => _generate(p, m)
        case m => // do nothing
      }
    }

    private def _generate(n: TreeNode[Realm.Data], p: SpecDoc) {
      val pathname = n.parent.pathname
      val view = Realm.View(n.parent)
      val t = new SpecDoc2Dox(p, view)
      val realm = t.apply()
      result.merge(pathname, realm)
    }
  }

  def generate(p: Realm): Realm = {
    val g = new Generator()
    p.traverse(g)
    g.result
  }

  class SpecDoc2Dox(specdoc: SpecDoc, view: Realm.View) {
    private val _builder = new DoxRealmBuilder(context)
    private val _cursor = _builder.createCursor

    def apply(): Realm = {
      transform_with_category
      _builder.build()
    }

    protected final def transform_with_category {
      for (pkg <- specdoc.packages) {
        _cursor.enterTopic(pkg.title, UJavaString.pathname2className(pkg.pathname))
        build_package_prologue(pkg)
        for (entity <- pkg.entities) {
	  _cursor.enterPage(entity.effectiveTitle, entity.name)
	  build_entity_prologue(entity)
	  for (category <- entity.categories) {
	    _cursor.enterDivision(category.label)
	    build_category_prologue(category)
	    for (subEntity <- entity.subEntities(category)) {
	      _cursor.enterDivision(subEntity.effectiveTitle)
	      build_sub_entity_prologue(subEntity)
	      _cursor.leaveDivision()
	    }
	    _cursor.leaveDivision()
	  }
	  _cursor.enterDivision(title_history)
	  build_history_prologue(entity)
	  _cursor.leaveDivision()
	  _cursor.leavePage()
        }
        // for (summary <- pkg.epilogues) {
        //   _cursor.enterPage(summary.effectiveTitle, summary.name)
        //   build_summary_prologue(summary, pkg.entities)
        //   _cursor.leavePage()
        // }
        _cursor.leaveTopic()
      }
    }

    protected final def build_package_prologue(aPackage: SDPackage) {
      _cursor.setResume(aPackage.resume)
      _cursor.addContent(aPackage.overview)
      _cursor.addTable(title_feature, aPackage.featureTable)
      for (category <- aPackage.categories) {
        _cursor.addTable(category.label, aPackage.entitiesTable(category))
      }
      _cursor.addContent(aPackage.specification)
      _cursor.addContent(aPackage.explanation)
    }

    private def build_entity_prologue(anEntity: SDEntity) {
      _cursor.setResume(anEntity.resume)
      _cursor.addContent(anEntity.overview)
      _cursor.addTable(title_feature, anEntity.featureTable)
      for (category <- anEntity.categories) {
        _cursor.addTable(category.label, anEntity.subEntitiesTable(category))
      }
      _cursor.addContent(anEntity.specification)
      _cursor.enterDivision(title_description)
      _cursor.addContent(anEntity.explanation)
      _cursor.leaveDivision()
    }

    private def build_category_prologue(aCategory: SDCategory) {
    }

    private def build_sub_entity_prologue(anEntity: SDEntity) {
      _cursor.setResume(anEntity.resume)
      _cursor.addContent(anEntity.overview)
      _cursor.addTable(title_feature, anEntity.featureTable)
      _cursor.addContent(anEntity.specification)
      _cursor.enterDivision(title_description)
      _cursor.addContent(anEntity.explanation)
      _cursor.leaveDivision()
    }

    private def build_history_prologue(anEntity: SDEntity) {
      if (anEntity.history.isEmpty) return
        _cursor.addTable(title_history, anEntity.history.toTable)
    }

    // private def build_summary_prologue(aSummary: SDSummary, theEntities: Seq[SDEntity]) {
    //   _cursor.addDescription(aSummary.effectiveSummary)
    //   _cursor.addDescription(aSummary.overview)
    //   _cursor.addTable(aSummary.summaryTable(theEntities))
    // }
  }
}

// class SpecDoc2SmartDocRealmGenerator(val specdoc: SpecDocEntity) {
//   private val builder = new SmartDocRealmBuilder(specdoc.entityContext)
//   private val cursor = builder.getCursor
//   private val title_summary = "概要" // XXX
//   private val title_feature = "特性" // XXX
//   private val title_description = "説明" // XXX
//   private val title_history = "履歴" // XXX

//   final def transform: SmartDocRealmEntity = {
//     transform_with_category
//     builder.make ensuring (_.isOpened)
//   }

//   private def transform_with_category {
//     for (pkg <- specdoc.entityPackages) {
//       cursor.enterTopic(pkg.title, UJavaString.pathname2className(pkg.pathname))
//       build_package_prologue(pkg)
//       for (entity <- pkg.entities) {
// 	cursor.enterPage(entity.effectiveTitle, entity.name)
// 	build_entity_prologue(entity)
// 	for (category <- entity.categories) {
// 	  cursor.enterDivision(category.label)
// 	  build_category_prologue(category)
// 	  for (subEntity <- entity.subEntities(category)) {
// 	    cursor.enterDivision(subEntity.effectiveTitle)
// 	    build_sub_entity_prologue(subEntity)
// 	    cursor.leaveDivision()
// 	  }
// 	  cursor.leaveDivision()
// 	}
// 	cursor.enterDivision(title_history)
// 	build_history_prologue(entity)
// 	cursor.leaveDivision()
// 	cursor.leavePage()
//       }
//       for (summary <- pkg.summaries) {
// 	cursor.enterPage(summary.effectiveTitle, summary.name)
// 	build_summary_prologue(summary, pkg.entities)
// 	cursor.leavePage()
//       }
//       cursor.leaveTopic()
//     }
//   }

//   private def build_package_prologue(aPackage: SDPackage) {
//     cursor.addDescription(aPackage.effectiveSummary)
//     cursor.addDescription(aPackage.overview)
//     cursor.addTable(aPackage.featureTable) caption_is title_feature
//     for (category <- aPackage.categories) {
//       cursor.addTable(aPackage.entitiesTable(category)) caption_is category.label
//     }
//     cursor.addDescription(aPackage.specification)
//   }

//   private def build_entity_prologue(anEntity: SDEntity) {
//     cursor.addDescription(anEntity.effectiveSummary)
//     cursor.addDescription(anEntity.overview)
//     cursor.addTable(anEntity.featureTable) caption_is title_feature
//     for (category <- anEntity.categories) {
//       cursor.addTable(anEntity.subEntitiesTable(category)) caption_is category.label
//     }
//     cursor.addDescription(anEntity.specification)
//     cursor.enterDivision(title_description)
//     cursor.addDescription(anEntity.description)
//     cursor.leaveDivision()
//   }

//   private def build_category_prologue(aCategory: SDCategory) {
//   }

//   private def build_sub_entity_prologue(anEntity: SDEntity) {
//     cursor.addDescription(anEntity.effectiveSummary)
//     cursor.addDescription(anEntity.overview)
//     cursor.addTable(anEntity.featureTable) caption_is title_feature
//     cursor.addDescription(anEntity.specification)
//     cursor.enterDivision(title_description)
//     cursor.addDescription(anEntity.description)
//     cursor.leaveDivision()
//   }

//   private def build_history_prologue(anEntity: SDEntity) {
//     if (anEntity.history.isEmpty) return
//     cursor.addTable(anEntity.history.toTable)
//   }

//   private def build_summary_prologue(aSummary: SDSummary, theEntities: Seq[SDEntity]) {
//     cursor.addDescription(aSummary.effectiveSummary)
//     cursor.addDescription(aSummary.overview)
//     cursor.addTable(aSummary.summaryTable(theEntities))
//   }
// }
