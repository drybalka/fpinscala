package fpinscala.exercises.parsing


enum JSON:
  case JNull
  case JNumber(get: Double)
  case JString(get: String)
  case JBool(get: Boolean)
  case JArray(get: IndexedSeq[JSON])
  case JObject(get: Map[String, JSON])


object JSON:
  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] =
    import P.*
    def jNullParser: Parser[JSON] =
      string("null").scope("jNull").map(_ => JSON.JNull)
    def jBoolParser: Parser[JBool] =
      boolean.scope("jBool").map(b => JSON.JBool(b))
    def jNumberParser: Parser[JNumber] =
      number.scope("jNumber").map(d => JSON.JNumber(d))
    def jStringParser: Parser[JString] =
      quotedString
        .map(_.stripPrefix("\"").stripSuffix("\""))
        .scope("jString")
        .map(s => JSON.JString(s))

    def jArrayParser: Parser[JArray] =
      (char('[').trim skipLeft
        jSomething.manyWithSeparator(char(',')) skipRight
        char(']')).scope("jArray").map(list => JSON.JArray(list.toIndexedSeq))

    def jObjectEntry: Parser[(String, JSON)] =
      quotedString
        .map(_.stripPrefix("\"").stripSuffix("\""))
        .trim
        .skipRight(char(':'))
        .product(jSomething)
        .scope("jObjectEntry")
    def jObjectParser: Parser[JObject] =
      (char('{') skipLeft
        jObjectEntry.manyWithSeparator(char(',')) skipRight
        char('}')).scope("jObject").map(list => JSON.JObject(list.toMap))

    def jSomething =
      (jNullParser | jBoolParser | jNumberParser | jStringParser | jArrayParser | jObjectParser).trim

    (jObjectParser | jArrayParser).trim


@main def runJsonParser() =
  // val p = MyParsers.char('{')
  val p = JSON.jsonParser(MyParsers)

  println(p.run("""[ 2 2 ]"""))
