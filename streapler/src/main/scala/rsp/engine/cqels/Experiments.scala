package rsp.engine.cqels

import akka.actor.{ActorSystem, Props}
import com.hp.hpl.jena.graph.Triple
import com.typesafe.config.ConfigFactory
import org.deri.cqels.engine.ConstructListener
import rsp.engine.trowl.TrowlReasoner
import rsp.engine.{RspListener, RspReasoner}
import rsp.vocab.Ssn

import scala.collection.JavaConversions._
import scala.concurrent.duration._
import scala.language.reflectiveCalls

object Experiments extends CommonPrefixes {
  type TripleList = java.util.List[Triple]
  val exStreams = "http://example.org/streams/"

  val config = ConfigFactory.load.getConfig("experiments.rsp")
  val engine = config.getString("engine")
  val inputConfig = ConfigFactory.load.getConfig("experiments.rsp.input")
  val timeSpan = config.getLong("timeSpan")
  val registerQuery = config.getBoolean("registerQuery")
  val queryIds = config.getIntList("queryIds")
  val enableRewriting = config.getBoolean("enableRewriting")
  val ontology = config.getString("ontology")
  var reas = 0

  def main(args: Array[String]): Unit = {

    val system = ActorSystem.create("rspSystem")

    val reasoner = engine match {
      case "cqels" => new CqelsReasoner(ontology)
      case "trowl" =>
        val trowl = new TrowlReasoner(ontology)
        schedReasoner(system, trowl)
        trowl
    }

    val ssw = system.actorOf(Props(new SsnStream(reasoner, exStreams + "s1", inputConfig)))
    val listener = createListener(reasoner)

    if (registerQuery) {
      val registered_queries: Unit = reasoner.registerQuery(queries(queryIds(0) - 1), listener, enableRewriting)
    }
    ssw ! StartStream
    Thread.sleep(timeSpan)
    println("time: " + timeSpan)
    println("queries: " + reasoner.regQueries)
    println("input: " + reasoner.inputCount)
    println("output: " + listener.count)
    println("reasoning: " + reas)

    ssw ! StopStream
    system.shutdown
    reasoner.stop
    //System.exit(0)
  }

  val rate = inputConfig.getInt("rate")

  def schedReasoner(system: ActorSystem, trowl: TrowlReasoner) = {
    import concurrent.ExecutionContext.Implicits.global
    val sched = system.scheduler.schedule(0 seconds, rate milliseconds) {
      reas += 1
      trowl.reason
    }
  }


  def createListener(reasoner: RspReasoner) = reasoner match {
    case cqels: CqelsReasoner =>
      val lis = new ConstructListener(cqels.engine) {
        var count = 0L

        def update(triples: TripleList): Unit = {
          count += triples.size
        }
      }
      new RspListener(lis) {
        override def count = lis.count
      }
    case _ =>
      new RspListener(null) {
        override def count = 123L
      }
  }


  def queries = {

    def template(bgp: String, construct: String = null) = {
      val const = if (construct == null) s"?s <${met}found> <triple> ."
      else construct
      s"""CONSTRUCT { $const }
       WHERE { 
         STREAM <${exStreams}s1> [RANGE 1ms]  {
           $bgp
       }
      }"""
    }

    val ssn1 = template(
      s"""?s  a <${met}TemperatureObservation>.""")

    val ssn2 = template(
      s"""?s <${Ssn.iri}observedBy> ?po .
          ?po a <${aws}TemperatureSensor>.
      """)

    val ssn3 = template(
      s"""?s  a <${met}AirTemperatureObservation>.        
      """)

    val ssn4 = template(
      s"""   

          ?s a <${met}HumidityObservation>.             
      """, s"?s <${met}found> <triple> .")

    val ssn5 = template(
      s""" ?s <${ssn}featureOfInterest> ?f .   
          ?f a <${met}AirMedium> .               
      """)

    val ssn6 = template(
      s"""?s <${Ssn.iri}observedBy> ?po .
          ?po a <${aws}Thermistor>.           
      """)

    val ssn7 = template(
      s"""?s  a <${ssn}Observation>.        
      """)
    val ssn8 = template(
      s"""?s  <${ssn}observedBy> ?g. ?g a <${ssn}Sensor>        
      """)


    val ssn9 = template(
      s"""?s  <${ssn}observedProperty> ?g. ?g a <${ssn}Property>        
      """)
    val ssn10 = template(
      s"""
?s   <${ssn}observedProperty> ?o.
?o  a <${qudim}Temperature> .   
?s     <${ssn}featureOfInterest> ?f. ?f a <${met}AirMedium>. 
?s     <${ssn}observedBy> ?c. ?c a <${aws}Thermistor>. 
      """)
    List(ssn1, ssn2, ssn3, ssn4, ssn5, ssn6, ssn7, ssn8, ssn9, ssn10)
  }
}

trait CommonPrefixes {
  val met = "http://purl.org/env/meteo#"
  val ssn = "http://purl.oclc.org/NET/ssnx/ssn#"
  val aws = "http://purl.oclc.org/NET/ssnx/meteo/aws#"
  val qudim = "http://purl.oclc.org/NET/ssnx/qu/dim#"
  val qu = "http://purl.oclc.org/NET/ssnx/qu/qu#"
  val cff = "http://purl.oclc.org/NET/ssnx/cf/cf-feature#"
  val owl = "http://www.w3.org/2002/07/owl#"
}



