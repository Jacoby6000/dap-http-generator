package io.github.jacoby6000.daphttp

import software.amazon.smithy.build.{PluginContext, SmithyBuildPlugin}

final class DapHttpGeneratorPlugin extends SmithyBuildPlugin {
  override def getName: String = "dap-http-generator"

  override def execute(context: PluginContext): Unit = {
    val outputFile = context.getSettings.getStringMemberOrDefault("outputFile", "dap-http/struct-manifest.json")
    val manifest = DapStructManifestGenerator.generate(context.getModel)
    context.getFileManifest.writeFile(outputFile, manifest)
  }
}
