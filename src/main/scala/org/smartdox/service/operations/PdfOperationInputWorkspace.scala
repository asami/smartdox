package org.smartdox.service.operations

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import org.goldenport.io.IoUtils
import org.smartdox.{Dox, ReferenceImg}
import org.smartdox.parser.Dox2Parser

/*
 * @since   Sep.  8, 2026
 * @version Sep.  8, 2026
 * @author  ASAMI, Tomoharu
 */
private[operations] object PdfOperationInputWorkspace {
  def parse(in: File): PdfOperationClass.ParsedPdfInput = {
    val file = in.getCanonicalFile
    val root = Option(file.getParentFile).getOrElse(
      throw new IllegalArgumentException(s"PDF input has no parent directory: ${file.getPath}")
    )
    val text = scala.io.Source.fromFile(file, "UTF-8").mkString
    val config = Dox2Parser.Config.default.withResourceRoot(root.toPath)
    PdfOperationClass.ParsedPdfInput(
      Dox2Parser.parseWithFilename(config, file.getPath, text),
      root
    )
  }

  def prepare(
    renderer: PdfOperationClass.PdfRenderer,
    filename: String,
    content: String,
    dox: Dox,
    resourceRoot: File
  ): PdfOperationClass.RendererWorkspace = {
    val workspace = Files.createTempDirectory(PdfRendererInvocation.tempPrefix(filename))
    try {
      val input = workspace.resolve(filename)
      Files.write(input, content.getBytes(StandardCharsets.UTF_8))
      _stage_root_relative_images(renderer, workspace, dox, resourceRoot)
      PdfOperationClass.RendererWorkspace(workspace, input)
    } catch {
      case e: Throwable =>
        dispose(workspace)
        throw e
    }
  }

  def createRendererDirectory(filename: String): Path =
    Files.createTempDirectory(PdfRendererInvocation.tempPrefix(filename))

  def writeRendererInput(dir: Path, filename: String, content: String): org.goldenport.bag.FileBag = {
    val path = dir.resolve(filename)
    val bag = org.goldenport.bag.FileBag.create(path.toFile)
    bag.write(content, StandardCharsets.UTF_8)
    bag
  }

  def dispose(directory: Path): Unit =
    IoUtils.removeDirectory(directory.toFile)

  private def _stage_root_relative_images(
    renderer: PdfOperationClass.PdfRenderer,
    workspace: Path,
    dox: Dox,
    resourceroot: File
  ): Unit = {
    val rootpath = resourceroot.getCanonicalFile.toPath
    _root_relative_reference_images(dox).foreach { image =>
      val sourcepath = _root_relative_image_source(image, rootpath)
      val targetpath = workspace.resolve(_renderer_image_target(renderer, image)).normalize
      if (!targetpath.startsWith(workspace))
        throw new IllegalArgumentException(
          s"image.local.outside-resource-root: ${_image_context(image, targetpath)} root=$workspace"
        )
      Option(targetpath.getParent).foreach(parent => Files.createDirectories(parent))
      Files.copy(sourcepath, targetpath, StandardCopyOption.REPLACE_EXISTING)
    }
  }

  private def _root_relative_reference_images(dox: Dox): Vector[ReferenceImg] = {
    val here = dox match {
      case image: ReferenceImg if _is_root_relative_reference_image(image) => Vector(image)
      case _ => Vector.empty
    }
    here ++ dox.elements.toVector.flatMap(_root_relative_reference_images)
  }

  private def _is_root_relative_reference_image(image: ReferenceImg): Boolean = {
    val uri = image.src
    !uri.isAbsolute &&
    uri.getRawAuthority == null &&
    uri.getRawQuery == null &&
    uri.getRawFragment == null &&
    Option(uri.getPath).exists(_.nonEmpty)
  }

  private def _root_relative_image_source(image: ReferenceImg, rootpath: Path): Path = {
    val sourcepath = try {
      Paths.get(Option(image.src.getPath).getOrElse(""))
    } catch {
      case _: java.nio.file.InvalidPathException =>
        throw new IllegalArgumentException(s"image.local.invalid-resource: ${_image_context(image, rootpath)}")
    }
    if (sourcepath.isAbsolute)
      throw new IllegalArgumentException(
        s"image.local.outside-resource-root: ${_image_context(image, sourcepath)} root=$rootpath"
      )
    val candidate = rootpath.resolve(sourcepath).normalize
    if (!candidate.startsWith(rootpath))
      throw new IllegalArgumentException(
        s"image.local.outside-resource-root: ${_image_context(image, candidate)} root=$rootpath"
      )
    if (!Files.isRegularFile(candidate))
      _missing_image_resource(image, candidate)
    val canonical = candidate.toRealPath()
    if (!canonical.startsWith(rootpath))
      throw new IllegalArgumentException(
        s"image.local.outside-resource-root: ${_image_context(image, canonical)} root=$rootpath"
      )
    canonical
  }

  private def _renderer_image_target(
    renderer: PdfOperationClass.PdfRenderer,
    image: ReferenceImg
  ): Path =
    renderer match {
      case PdfOperationClass.PdfRenderer.ChromeHeadless => Paths.get(image.src.getPath)
      case PdfOperationClass.PdfRenderer.Asciidoc => _asciidoc_image_target(image)
      case PdfOperationClass.PdfRenderer.Latex =>
        throw new IllegalArgumentException(s"PDF renderer workspace is unsupported for: $renderer")
    }

  private def _asciidoc_image_target(image: ReferenceImg): Path = {
    val source = image.src.toString
    val target =
      if (source.startsWith("images/")) {
        val rest = source.substring("images/".length)
        val i = rest.indexOf('/')
        if (i > 0)
          s"${rest.substring(0, i)}:${rest.substring(i + 1)}"
        else
          rest
      } else {
        source
      }
    Paths.get(target)
  }

  private def _missing_image_resource(image: ReferenceImg, path: Path): Nothing =
    throw new IllegalArgumentException(s"image.local.missing-resource: ${_image_context(image, path)}")

  private def _image_context(image: ReferenceImg, path: Path): String = {
    val location = image.location.map(_.toString).getOrElse("<absent>")
    s"location=$location source=${image.src} path=$path"
  }
}
