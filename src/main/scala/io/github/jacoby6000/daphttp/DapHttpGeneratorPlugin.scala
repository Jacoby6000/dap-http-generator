package io.github.jacoby6000.daphttp

import software.amazon.smithy.build.{PluginContext, SmithyBuildPlugin}

final class DapHttpGeneratorPlugin extends SmithyBuildPlugin {
  override def getName: String = "dap-http-generator"

  override def execute(context: PluginContext): Unit = {
    val outputFile = context.getSettings.getStringMemberOrDefault("outputFile", "dap-http/struct-manifest.json")
    val result = DapStructManifestGenerator.generateWithDiagnostics(context.getModel)

    result.warnings.foreach { warning =>
      System.err.println(s"[dap-http-generator][warning] ${warning.shapeId}: ${warning.message}")
    }

    context.getFileManifest.writeFile(outputFile, result.json)

    if (result.errors.nonEmpty) {
      val errorSummary = result.errors
        .map(error => s"${error.shapeId}: ${error.message}")
        .mkString("\n")
      throw new IllegalArgumentException(s"dap-http-generator validation failed:\n$errorSummary")
    }
  }
}
