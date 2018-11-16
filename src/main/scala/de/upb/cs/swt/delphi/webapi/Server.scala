// Copyright (C) 2018 The Delphi Team.
// See the LICENCE file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package de.upb.cs.swt.delphi.webapi

import akka.http.scaladsl.server.{HttpApp, Route}
import de.upb.cs.swt.delphi.instancemanagement.InstanceRegistry

/**
  * Web server configuration for Delphi web API.
  */
object Server extends HttpApp with JsonSupport with AppLogging {
  val delphiRoutes = DelphiRoutes()

  override def routes: Route = {
    delphiRoutes
  }


  def main(args: Array[String]): Unit = {
    sys.addShutdownHook({
      log.warning("Received shutdown signal.")
      InstanceRegistry.handleInstanceStop(configuration)
    })

    StartupCheck.check(configuration)
    Server.startServer(configuration.bindHost, configuration.bindPort, system)

    val terminationFuture = system.terminate()

    terminationFuture.onComplete {
      sys.exit(0)
    }
  }


}


