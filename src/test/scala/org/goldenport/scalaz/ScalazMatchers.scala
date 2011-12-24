package org.goldenport.scalaz

import scalaz._
import Scalaz._
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher

/*
 * @since   Dec. 24, 2011
 * @version Dec. 24, 2011
 * @author  ASAMI, Tomoharu
 */
trait ScalazMatchers {
  object success extends ValidationSuccessMatcher  
  def fail(messages: List[String]) = {  
    ValidationFailMatcher(messages)  
  }
}
  
case class ValidationSuccessMatcher() extends Matcher[Validation[NonEmptyList[String], _]] {  
  def apply(value: Validation[NonEmptyList[String], _]) = {  
    val result = value.isSuccess  
    MatchResult(result, "failure", "success")  
  }  
}  
  
case class ValidationFailMatcher(messages: List[String]) extends Matcher[Validation[NonEmptyList[String], _]] {  
  def apply(value: Validation[NonEmptyList[String], _]) = {  
    value match {  
      case Success(a) => MatchResult(false, "incorrect success", "")  
      case Failure(e) => if (e.all(messages.contains)) { 
        MatchResult(true, "", "correct failure")  
      } else {  
        MatchResult(false, "incorrect failure", "")  
      }  
    }  
  }  
}  
