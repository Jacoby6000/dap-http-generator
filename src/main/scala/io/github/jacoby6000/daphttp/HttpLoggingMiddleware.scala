package io.github.jacoby6000.daphttp

import cats.data.Kleisli
import cats.effect.IO
import org.http4s.HttpApp
import org.http4s.Request
import org.http4s.Response

object HttpLoggingMiddleware {
  def apply(httpApp: HttpApp[IO]): HttpApp[IO] =
    Kleisli[IO, Request[IO], Response[IO]] { request =>
      val startedAt = System.nanoTime()
      val path = request.uri.path.renderString
      val method = request.method
      httpApp(request).flatTap { response =>
        IO.delay {
          val elapsedMs = (System.nanoTime() - startedAt) / 1000000L
          DapHttpLoggers.http.info(
            "{} {} -> {} ({} ms)",
            method,
            path,
            Integer.valueOf(response.status.code),
            Long.box(elapsedMs)
          )
        }
      }
    }
}
