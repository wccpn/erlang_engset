import gleam/float
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam_community/maths
import lustre
import lustre/attribute
import lustre/dev/simulate
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

type Erlang {
  Erlang(
    lines: Option(Int),
    traffic: Option(Float),
    blocking_rate: Option(Float),
  )
}

fn calculate_erlang(erlang: Erlang) -> Float {
  case erlang.lines, erlang.traffic, erlang.blocking_rate {
    Some(l), Some(t), None -> erlang_b(l, t)
    Some(l), None, Some(b) -> erlang_t(l, b, 0.0)
    None, Some(t), Some(b) -> erlang_l(t, b, 1) |> int.to_float()
    // TODO implement error handling
    _, _, _ -> 0.0
  }
}

// For calculating blocking rate
fn erlang_b(lines: Int, traffic: Float) -> Float {
  {
    { float.power(traffic, int.to_float(lines)) |> result.unwrap(0.0) }
    /. { maths.factorial(lines) |> result.unwrap(1) |> int.to_float }
    /. int.range(from: 0, to: lines + 1, with: 0.0, run: fn(acc, i) {
      acc
      +. { { float.power(traffic, int.to_float(i)) |> result.unwrap(0.0) } }
      /. { { maths.factorial(i) |> result.unwrap(0) } |> int.to_float() }
    })
  }
}

fn erlang_l(traffic: Float, blocking_rate: Float, lines_guess: Int) -> Int {
  // calculate correct lines by recursively guessing and comparing with tolerance for floating point number precision
  case
    float.loosely_equals(blocking_rate, erlang_b(lines_guess, traffic), 0.1)
  {
    True -> lines_guess
    False -> erlang_l(traffic, blocking_rate, lines_guess + 1)
  }
}

fn erlang_t(lines: Int, blocking_rate: Float, traffic_guess: Float) -> Float {
  case
    float.loosely_equals(blocking_rate, erlang_b(lines, traffic_guess), 0.1)
  {
    True -> traffic_guess
    False -> erlang_t(lines, blocking_rate, traffic_guess +. 0.1)
  }
}

type Engset {
  Engset(
    servers: Option(Int),
    sources: Option(Int),
    arrival_rate: Option(Float),
    holding_time: Option(Float),
    blocking_rate: Option(Float),
  )
}

fn calculate_engset(engset: Engset) -> Float {
  case
    engset.servers,
    engset.sources,
    engset.arrival_rate,
    engset.holding_time,
    engset.blocking_rate
  {
    Some(servers), Some(sources), Some(arrival_rate), Some(holding_time), None
    -> engset_blocking_rate(servers, sources, arrival_rate, holding_time)
    None,
      Some(sources),
      Some(arrival_rate),
      Some(holding_time),
      Some(blocking_rate)
    ->
      engset_servers(0, sources, arrival_rate, holding_time, blocking_rate)
      |> int.to_float
    Some(servers),
      None,
      Some(arrival_rate),
      Some(holding_time),
      Some(blocking_rate)
    ->
      engset_sources(servers, 0, arrival_rate, holding_time, blocking_rate)
      |> int.to_float()
    Some(servers), Some(sources), None, Some(holding_time), Some(blocking_rate)
    -> engset_arrival_rate(servers, sources, 0.0, holding_time, blocking_rate)
    Some(servers), Some(sources), Some(arrival_time), None, Some(blocking_rate)
    -> engset_arrival_rate(servers, sources, arrival_time, 0.0, blocking_rate)
    // TODO implement error handling
    _, _, _, _, _ -> 0.0
  }
}

fn engset_blocking_rate(
  servers: Int,
  sources: Int,
  arrival_rate: Float,
  holding_time: Float,
) -> Float {
  {
    {
      maths.combination(sources - 1, servers)
      |> result.unwrap(0)
      |> int.to_float()
    }
    *. {
      float.power(arrival_rate *. holding_time, { servers |> int.to_float() })
      |> result.unwrap(0.0)
    }
    /. {
      int.range(from: 0, to: servers + 1, with: 0.0, run: fn(acc, i) {
        acc
        +. {
          {
            maths.combination(sources - 1, i)
            |> result.unwrap(0)
            |> int.to_float
          }
          *. {
            float.power(arrival_rate *. holding_time, int.to_float(i))
            |> result.unwrap(0.0)
          }
        }
      })
    }
  }
}

fn engset_servers(
  servers_guess: Int,
  sources: Int,
  arrival_rate: Float,
  holding_time: Float,
  blocking_rate: Float,
) -> Int {
  case
    float.loosely_equals(
      blocking_rate,
      engset_blocking_rate(servers_guess, sources, arrival_rate, holding_time),
      0.1,
    )
  {
    True -> servers_guess
    False ->
      engset_servers(
        servers_guess + 1,
        sources,
        arrival_rate,
        holding_time,
        blocking_rate,
      )
  }
}

fn engset_sources(
  servers: Int,
  sources_guess: Int,
  arrival_rate: Float,
  holding_time: Float,
  blocking_rate: Float,
) -> Int {
  case
    float.loosely_equals(
      blocking_rate,
      engset_blocking_rate(servers, sources_guess, arrival_rate, holding_time),
      0.1,
    )
  {
    True -> sources_guess
    False ->
      engset_servers(
        servers,
        sources_guess + 1,
        arrival_rate,
        holding_time,
        blocking_rate,
      )
  }
}

fn engset_arrival_rate(
  servers: Int,
  sources: Int,
  arrival_rate_guess: Float,
  holding_time: Float,
  blocking_rate: Float,
) -> Float {
  case
    float.loosely_equals(
      blocking_rate,
      engset_blocking_rate(servers, sources, arrival_rate_guess, holding_time),
      0.1,
    )
  {
    True -> arrival_rate_guess
    False ->
      engset_arrival_rate(
        servers,
        sources,
        arrival_rate_guess +. 0.1,
        holding_time,
        blocking_rate,
      )
  }
}

fn engset_holding_time(
  servers: Int,
  sources: Int,
  arrival_rate: Float,
  holding_time_guess: Float,
  blocking_rate: Float,
) -> Float {
  case
    float.loosely_equals(
      blocking_rate,
      engset_blocking_rate(servers, sources, arrival_rate, holding_time_guess),
      0.1,
    )
  {
    True -> holding_time_guess
    False ->
      engset_arrival_rate(
        servers,
        sources,
        arrival_rate,
        holding_time_guess +. 0.1,
        blocking_rate,
      )
  }
}

type Model {
  Model(
    erlang: Option(Erlang),
    engset: Option(Engset),
    message: Option(Message),
  )
}

type Message {
  ErlangChoice
  EngsetChoice
  Lines
  Traffic
  BlockingRateErlang
  BlockingRateErlangUpdateLines(String)
  BlockingRateErlangUpdateTraffic(String)
  LinesErlangUpdateBlockingRate(String)
  LinesErlangUpdateTraffic(String)
  TrafficErlangUpdateLines(String)
  TrafficErlangUpdateBlockingRate(String)
  Servers
  Sources
  ArrivalRate
  HoldingTime
  BlockingRateEngset
  BlockingRateEngsetUpdateServers(String)
}

fn init(_args) -> Model {
  Model(None, None, None)
}

fn view(model: Model) -> Element(Message) {
  case model.erlang, model.engset, model.message {
    None, None, None -> init_view(model)
    _, _, Some(message) ->
      case message {
        ErlangChoice -> erlang_choice_view(model)
        EngsetChoice -> engset_choice_view(model)
        BlockingRateErlang -> erlang_blocking_rate_choice_view(model)
        Lines -> erlang_lines_choice_view(model)
        Traffic -> erlang_lines_choice_view(model)
        _ -> init_view(model)
      }
    _, _, _ -> todo
  }
}

fn init_view(model: Model) -> Element(Message) {
  html.div([], [
    html.button([event.on_click(ErlangChoice)], [html.text("Erlang")]),
    html.button([event.on_click(EngsetChoice)], [html.text("Engset")]),
  ])
}

fn erlang_choice_view(model: Model) -> Element(Message) {
  html.div([], [
    html.h1([], [html.text("What do you want to calculate?")]),
    html.button([event.on_click(BlockingRateErlang)], [
      html.text("Blocking rate"),
    ]),
    html.button([event.on_click(Lines)], [
      html.text("Number of servers/telephone lines"),
    ]),
    html.button([event.on_click(Traffic)], [html.text("Traffic")]),
  ])
}

fn erlang_blocking_rate_choice_view(model: Model) {
  let assert Some(erlang) = model.erlang
  html.div([], [
    html.h1([], [html.text("Enter the values: ")]),
    html.h2([], [html.text("Number of servers/telephone lines: ")]),
    html.input([
      attribute.value(erlang.lines |> option.unwrap(0) |> int.to_string),
      event.on_input(BlockingRateErlangUpdateLines),
    ]),
    html.h2([], [html.text("Traffic: ")]),
    html.input([
      attribute.value(erlang.traffic |> option.unwrap(0.0) |> float.to_string),
      event.on_input(BlockingRateErlangUpdateTraffic),
    ]),
    html.h2([], [
      html.text(
        "Blocking rate: "
        <> {
          calculate_erlang(erlang) |> float.to_precision(3) |> float.to_string
        },
      ),
    ]),
  ])
}

fn erlang_lines_choice_view(model: Model) {
  let assert Some(erlang) = model.erlang
  html.div([], [
    html.h1([], [html.text("Enter the values: ")]),
    html.h2([], [html.text("Blocking rate: ")]),
    html.input([
      attribute.value(
        erlang.blocking_rate |> option.unwrap(0.0) |> float.to_string,
      ),
      event.on_input(LinesErlangUpdateBlockingRate),
    ]),
    html.h2([], [html.text("Traffic: ")]),
    html.input([
      attribute.value(erlang.traffic |> option.unwrap(0.0) |> float.to_string),
      event.on_input(LinesErlangUpdateTraffic),
    ]),
    html.h2([], [
      html.text(
        "Number of servers/telephone lines: "
        <> {
          calculate_erlang(erlang) |> float.to_precision(3) |> float.to_string
        },
      ),
    ]),
  ])
}

fn erlang_traffic_choice_view(model: Model) {
  let assert Some(erlang) = model.erlang
  html.div([], [
    html.h1([], [html.text("Enter the values: ")]),
    html.h2([], [html.text("Blocking rate: ")]),
    html.input([
      attribute.value(
        erlang.blocking_rate |> option.unwrap(0.0) |> float.to_string,
      ),
      event.on_input(LinesErlangUpdateBlockingRate),
    ]),
    html.h2([], [html.text("Number of servers/telephone lines: ")]),
    html.input([
      attribute.value(erlang.lines |> option.unwrap(0) |> int.to_string),
      event.on_input(LinesErlangUpdateTraffic),
    ]),
    html.h2([], [
      html.text(
        "Traffic: "
        <> {
          calculate_erlang(erlang) |> float.to_precision(3) |> float.to_string
        },
      ),
    ]),
  ])
}

fn engset_choice_view(model: Model) {
  html.div([], [
    html.h1([], [html.text("What do you want to calculate?")]),
    html.button([event.on_click(BlockingRateEngset)], [
      html.text("Blocking rate"),
    ]),
    html.button([event.on_click(Servers)], [html.text("Number of servers")]),
    html.button([event.on_click(Sources)], [
      html.text("Number of sources of traffic"),
    ]),
    html.button([event.on_click(ArrivalRate)], [
      html.text("Idle source arrival rate"),
    ]),
    html.button([event.on_click(HoldingTime)], [
      html.text("Average holding time"),
    ]),
  ])
}

fn update(model: Model, message: Message) -> Model {
  case model.erlang, model.engset, model.message, message {
    _, _, _, ErlangChoice ->
      Model(Some(Erlang(None, None, None)), None, Some(ErlangChoice))
    _, _, _, EngsetChoice -> Model(None, None, Some(EngsetChoice))
    Some(e), _, _, BlockingRateErlang ->
      Model(Some(e), None, Some(BlockingRateErlang))
    Some(e), _, _, BlockingRateErlangUpdateLines(l) ->
      case int.parse(l) {
        Ok(l) ->
          Model(
            Some(Erlang(..e, lines: Some(l))),
            None,
            Some(BlockingRateErlang),
          )
        Error(_) -> Model(Some(e), None, Some(BlockingRateErlang))
      }
    Some(e), _, _, BlockingRateErlangUpdateTraffic(t) ->
      case float.parse(t) {
        Ok(t) ->
          Model(
            Some(Erlang(..e, traffic: Some(t))),
            None,
            Some(BlockingRateErlang),
          )
        Error(_) -> Model(Some(e), None, Some(BlockingRateErlang))
      }
    Some(e), _, _, Lines -> Model(Some(e), None, Some(Lines))
    Some(e), _, _, LinesErlangUpdateBlockingRate(b) ->
      case float.parse(b) {
        Ok(b) ->
          Model(Some(Erlang(..e, blocking_rate: Some(b))), None, Some(Lines))
        Error(_) -> Model(Some(e), None, Some(Lines))
      }
    Some(e), _, _, LinesErlangUpdateTraffic(t) ->
      case float.parse(t) {
        Ok(t) -> Model(Some(Erlang(..e, traffic: Some(t))), None, Some(Lines))
        Error(_) -> Model(Some(e), None, Some(Lines))
      }
    Some(e), _, _, Traffic -> Model(Some(e), None, Some(Traffic))
    Some(e), _, _, TrafficErlangUpdateBlockingRate(t) ->
      case float.parse(t) {
        Ok(t) ->
          Model(Some(Erlang(..e, blocking_rate: Some(t))), None, Some(Lines))
        Error(_) -> Model(Some(e), None, Some(Lines))
      }
    Some(e), _, _, TrafficErlangUpdateLines(l) ->
      case int.parse(l) {
        Ok(l) -> Model(Some(Erlang(..e, lines: Some(l))), None, Some(Lines))
        Error(_) -> Model(Some(e), None, Some(Lines))
      }
    _, _, _, _ -> todo
  }
}

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}
