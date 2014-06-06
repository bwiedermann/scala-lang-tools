package edu.hmc.langtools

abstract class Interpreter[IR, SD] {
  def eval(ast: IR): SD
}