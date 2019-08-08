package com.example.helloworld

import io.circe.generic.JsonCodec
import net.reactivecore.fhttp.{ ApiBuilder, Input, Output }

@JsonCodec
case class Numbers(
    a: Int,
    b: Int
)

@JsonCodec
case class NumberResult(
    result: Int
)

object HelloWorldApi extends ApiBuilder {

  val sum = add {
    post("sum")
      .expecting(Input.circe[Numbers]())
      .responding(Output.circe[NumberResult]())
  }

  val version = add {
    get("version")
      .responding(Output.text())
  }

  def divide = add {
    post("divide")
      .expecting(
        Input.circe[Numbers]()
      )
      .responding(
        Output.ErrorSuccess(
          Output.text(),
          Output.circe[NumberResult]()
        )
      )
  }
}
