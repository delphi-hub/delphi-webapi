package de.upb.cs.swt.delphi.webapi

import akka.actor.{Actor, ActorLogging, Props}
import com.sksamuel.elastic4s.http.ElasticDsl._
import de.upb.cs.swt.delphi.webapi.ElasticActorManager.{Enqueue, Retrieve}
import de.upb.cs.swt.delphi.webapi.ElasticActor.GetSource

class ElasticActorManager(configuration: Configuration) extends Actor with ActorLogging{

  private val index = "delphi" / "project"

  override def preStart(): Unit = log.info("Actor manager started")
  override def postStop(): Unit = log.info("Actor manager shut down")

  override def receive = {
    case Retrieve(id) => getSource(id)
    case Enqueue(id) =>  getSource(id)
  }

  private def getSource(id: String) = {
    log.info("Creating actor to search for entry {}", id)
    val retrieveActor = context.actorOf(ElasticActor.props(configuration))
    retrieveActor forward GetSource(id, index)
  }
}

object ElasticActorManager{
  def props(configuration: Configuration) : Props = Props(new ElasticActorManager(configuration))
    .withMailbox("es-priority-mailbox")

  final case class Retrieve(id: String)
  final case class Enqueue(id: String)
}