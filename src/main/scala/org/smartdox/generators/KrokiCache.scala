package org.smartdox.generators

import java.io.File
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import org.goldenport.io.IoUtils
import org.goldenport.context.Consequence
import org.smartdox.generator.Context

/*
 * Kroki diagram cache manager.
 *
 * This class provides a unified caching mechanism for diagrams generated
 * via Kroki (e.g., PlantUML, Mermaid, Graphviz). It stores generated diagram
 * files (e.g., SVG, PNG) based on a hash of the source diagram definition.
 *
 * - The cache directory can be either:
 *   - a single directory (shared by all languages)
 *   - or separated by language subdirectories, depending on configuration.
 *
 * @since   Oct. 11, 2025
 * @version Oct. 11, 2025
 * @author  ASAMI
 */
class KrokiCache(
  context: Context,
  baseDir: File,
  useLanguageSubdir: Boolean
) {
  import KrokiCache._

  if (!baseDir.exists())
    baseDir.mkdirs()

  //
  // Public API
  //

  /** Returns a cached diagram file if it exists, without specifying language. */
  def get(source: String, format: String): Option[File] =
    get(None, source, format)

  /** Returns a cached diagram file for a specific language, if it exists. */
  def get(language: Option[String], source: String, format: String): Option[File] = {
    val file = _cache_file(language, source, format)
    if (file.exists()) Some(file) else None
  }

  /** Checks if a cached file exists (without language specification). */
  def exists(source: String, format: String): Boolean =
    exists(None, source, format)

  /** Checks if a cached file exists for a specific language. */
  def exists(language: Option[String], source: String, format: String): Boolean =
    _cache_file(language, source, format).exists()

  /** Saves diagram content (byte array) to the cache without language. */
  def set(source: String, format: String, content: Array[Byte]): Unit =
    set(None, source, format, content)

  /** Saves diagram content (byte array) to the cache for a specific language. */
  def set(language: Option[String], source: String, format: String, content: Array[Byte]): Unit = {
    val file = _cache_file(language, source, format)
    try {
      val parent = file.getParentFile
      if (!parent.exists()) parent.mkdirs()
      IoUtils.save(file, content)
    } catch {
      case e: Throwable =>
        context.log.error(s"Failed to save Kroki cache: ${file.getPath}: ${e.getMessage}")
        IoUtils.moveFileWithErrorSuffix(file)
    }
  }

  /** Loads a cached diagram (as byte array) without language specification. */
  def load(source: String, format: String): Option[Array[Byte]] =
    load(None, source, format)

  /** Loads a cached diagram (as byte array) for a specific language. */
  def load(language: Option[String], source: String, format: String): Option[Array[Byte]] =
    Consequence {
      IoUtils.toBytes(_cache_file(language, source, format))
    }.toOption

  /** Removes the entire cache directory. */
  def clear(): Unit = IoUtils.removeDirectory(baseDir)

  /** Removes cache files for a specific language (if using language subdirectories). */
  def clear(language: String): Unit = {
    val dir = new File(baseDir, language)
    if (dir.exists()) IoUtils.removeDirectory(dir)
  }

  //
  // Internal
  //

  /** Computes the full cache file path for a given source and format. */
  private def _cache_file(language: Option[String], source: String, format: String): File = {
    val hash = _hash(source)
    val dir =
      if (useLanguageSubdir)
        language.map(lang => new File(baseDir, lang)).getOrElse(baseDir)
      else
        baseDir
    new File(dir, s"$hash.$format")
  }

  /** Generates a SHA-1 hash string from the source diagram definition. */
  private def _hash(source: String): String = {
    val md = MessageDigest.getInstance("SHA-1")
    val bytes = source.getBytes(StandardCharsets.UTF_8)
    md.digest(bytes).map("%02x".format(_)).mkString
  }
}

object KrokiCache {
  /** Default configuration: single cache directory shared by all languages. */
  def apply(context: Context): KrokiCache =
    new KrokiCache(context, new File("kroki-cache.d"), false)

  /** Creates a KrokiCache with a specified base directory (single-dir mode). */
  def apply(baseDir: File, context: Context): KrokiCache =
    new KrokiCache(context, baseDir, false)

  /** Creates a KrokiCache with optional language subdirectory mode. */
  def apply(baseDir: File, useLanguageSubdir: Boolean, context: Context): KrokiCache =
    new KrokiCache(context, baseDir, useLanguageSubdir)
}

