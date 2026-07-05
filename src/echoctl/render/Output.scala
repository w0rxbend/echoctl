package com.worxbend.echoctl.render

import fansi.Str
import pprint.pprintln
import upickle.default.write

import scala.util.Try

final class Output(json: Boolean, verboseEnabled: Boolean):
  private def paint(message: String, style: Str => Str): String =
    style(Str(message)).render

  val isJson: Boolean = json

  def withJson(raw: String, pretty: => String): Unit =
    if json then
      println(raw)
    else
      Try(ujson.read(raw)) match
        case scala.util.Success(parsed) =>
          pprintln(parsed)
        case scala.util.Failure(_) =>
          println(pretty)

  def short(message: String): Unit =
    println(message)

  def ok(message: String): Unit =
    if json then
      println(message)
    else
      println(paint(s"[ok] $message", _.green))

  def warn(message: String): Unit =
    if json then
      println(message)
    else
      println(paint(s"[warn] $message", _.yellow))

  def error(message: String): Unit =
    if json then
      System.err.println(message)
    else
      System.err.println(paint(s"[err] $message", _.red))

  def verbose(message: String): Unit =
    if verboseEnabled then
      if json then
        System.err.println(message)
      else
        System.err.println(paint(s"[verbose] $message", _.blue))

  def prettyPrint[T: upickle.default.Writer](value: T): Unit =
    if json then
      println(write(value, indent = 2))
    else
      pprintln(value)

  def asJson[T: upickle.default.Writer](value: T): String =
    write[T](value)
