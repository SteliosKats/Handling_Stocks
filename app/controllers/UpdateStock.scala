package controllers

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import javax.inject._

import play.api.http.HttpEntity
import akka.util.ByteString
import play.api.mvc._

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.reflect.internal.util.Statistics.Quantity
import java.time._


import scala.collection.mutable.ListBuffer


case class ProductsNew (products: Products, itemsSold:Int)
case class Products (id: String, timestamp :ZonedDateTime, productid: String, quantity :Int)
case class Stock (id: String, timestamp :ZonedDateTime , quantity: Int)
case class ExtractedProduct (id: String, requested_timestamp :ZonedDateTime, stock :Stock)
case class TopSelProducts (productId :String, itemsSold :Int)
case class ProductStats (requestTimestamp :ZonedDateTime,range:String, topavailableproducts :Products, topsellingproducts :TopSelProducts )



/**
  * Get Time in UCT time format
  */
object UTCtimeNow {
  def get: ZonedDateTime = {
    val timeNow = Instant.now
    val utcTz = ZoneId.of("UTC")
    timeNow.atZone(utcTz)
  }
}





object Products {
  var list :ListBuffer[ProductsNew] = ListBuffer.empty
  var max :Int = 0
  lazy val top3HighestAvailthisMonth = list.filter(x => x.products.timestamp.getDayOfMonth == UTCtimeNow.get.getDayOfMonth).sortWith((pr1 , pr2) => pr1.products.quantity >= pr2.products.quantity).take(3)
  lazy val top3HighestAvailLastMonth = list.filter(x => x.products.timestamp.getDayOfMonth -1 == UTCtimeNow.get.getDayOfMonth -1).sortWith((pr1 , pr2) => pr1.products.quantity >= pr2.products.quantity).take(3)

  lazy val top3sellingLastMonth  =  list.filter(x => x.products.timestamp.getDayOfMonth == UTCtimeNow.get.getDayOfMonth).sortWith((pr1 , pr2) => pr1.itemsSold >= pr2.itemsSold).take(3)
  lazy val top3sellingthisMonth  = list.filter(x => x.products.timestamp.getDayOfMonth == UTCtimeNow.get.getDayOfMonth).sortWith((pr1 , pr2) => pr1.itemsSold >= pr2.itemsSold).take(3)
  
  def insert(newProduct: ProductsNew):Either[Unit, ListBuffer[ProductsNew]] = {

    if(list.contains(newProduct.products.id)) Left ("\nCannot update stock because product already exists in the database\n")

    else Right(list += newProduct)

  }//list += products


  def update(products: Products):Either[Unit,ListBuffer[ProductsNew]] = {

    val productExists =list.filter(_.products.id == products.id)

    if(productExists.isEmpty) {                          //If product doesn't exist in the stock then
      Left ("\nCannot update stock because product does not exist in the database\n")   // print the appropriate message
    }
    else if(products.timestamp.isAfter(productExists.head.products.timestamp)) {   // else check the datetime in UTC and if it's a newer stock, update it
      list = list.-=(productExists.head)
      val updatedProduct= ProductsNew (products, productExists.head.itemsSold)
      Right(list += updatedProduct)
    }
    else
      Left(println("\nCannot update stock because a newer stock value is already in the database\n"))

  }


  def search(productId : String): Option[ProductsNew] ={
    list.find (x => x.products.productid == productId )
  }


  def getstats(time :String): Either[Unit, Tuple2[ListBuffer[ProductsNew],ListBuffer[ProductsNew]]] ={
    time match {

      case ci"today" => Right(Tuple2(top3HighestAvailthisMonth,top3sellingthisMonth) ) // UTCtimeNow.get.getDayOfMonth


      case ci"lastMonth" => Right(Tuple2(top3HighestAvailLastMonth,top3sellingLastMonth) )

      case _ => Left("\nWrong 'time' value provided in order to get the statistics returned. Valid ones are [today,lastMonth]\n")

    }

  }


  /**
    * Make each String pattern case insensitive for pattern matching
    */
  implicit class CaseInsensitiveRegex(sc: StringContext) {
    def ci = ( "(?i)" + sc.parts.mkString ).r
  }

}



/**
  * This controller creates an `Action` to handle HTTP POST requests to the
  * application's /updatestock controller (Actor).
  */

@Singleton
class UpdateStock @Inject()(cc: ControllerComponents) extends AbstractController(cc) {

  implicit val productsNewWrites : Writes[ProductsNew] = (
    (JsPath \ "products").write[Products] and
      (JsPath \ "itemsSold").write[Int]
    )(unlift(ProductsNew.unapply))

  implicit val productsNewReads : Reads[ProductsNew] = (
    (JsPath \ "products").read[Products] and
      (JsPath \ "itemsSold").read[Int]
    )(ProductsNew.apply _)

  implicit val productsWrites : Writes[Products] = (
    (JsPath \ "id").write[String] and
      (JsPath \ "timestamp").write[ZonedDateTime] and
      (JsPath \ "productid").write[String] and
      (JsPath \ "quantity").write[Int]
    )(unlift(Products.unapply))

  implicit val productsReads : Reads[Products] = (
    (JsPath \ "id").read[String] and
      (JsPath \ "timestamp").read[ZonedDateTime] and
    (JsPath \ "productid").read[String] and
      (JsPath \ "quantity").read[Int]
  )(Products.apply _)


  implicit val extractedPorductWrites : Writes[ExtractedProduct] = (
    (JsPath \ "productId").write[String] and
      (JsPath \ "requestedTimestamp").write[ZonedDateTime] and
      (JsPath \ "stock").write[Stock]
    )(unlift(ExtractedProduct.unapply))

  implicit val stocksWrites : Writes[Stock] = (
    (JsPath \ "id").write[String] and
      (JsPath \ "timestamp").write[ZonedDateTime] and
      (JsPath \ "quantity").write[Int]
    )(unapply(Stock.unapply))

  def validateJson[A : Reads] = parse.json.validate(
    _.validate[A].asEither.left.map(e => BadRequest(JsError.toJson(e)))
  )



  def insert_product = Action(validateJson[ProductsNew]) { request =>
    val product = request.body
    Products.insert(product) match {

      case Right (s) =>

        Result(
          header = ResponseHeader(201, Map.empty)
          ,
          body = HttpEntity.Strict(ByteString("Everything went well and the new entry for stock was accepted!\n "), Some("text/plain"))
        )

      case Left (s) =>

        Result(
          header = ResponseHeader(204, Map.empty)
          ,
          body = HttpEntity.Strict(ByteString("\nCannot update stock because a newer stock value is already in the database!\n"), Some("text/plain"))
        )

    }

  }


/**
  * Validate JSON using the implicit `producteReads`
  * above, returning errors if the parsed json fails validation.
  */
  def prod_update = Action(validateJson[Products]) { request =>
    val products = request.body
    Products.update(products) match {

      case Right (s) =>

        Result(
          header = ResponseHeader(201, Map.empty)
          ,
          body = HttpEntity.Strict(ByteString("Everything went well and the new entry for stock was accepted!\n "), Some("text/plain"))
        )

      case Left (s) =>

        Result(
          header = ResponseHeader(204, Map.empty)
          ,
          body = HttpEntity.Strict(ByteString("\nCannot update stock because a newer stock value is already in the database!\n"), Some("text/plain"))
        )

    }

  }

  def request_prod (productId :String) = Action {

    Products.search(productId) match {

      case Some(x) =>


        val stockIsntance :Stock = Stock(x.products.id, x.products.timestamp,x.products.quantity)
        Json.toJson(stockIsntance)
        val extractedlist :ExtractedProduct = ExtractedProduct(x.products.productid,UTCtimeNow.get, stockIsntance)
        println(Json.toJson(extractedlist))
        val extracted_product = Json.toJson(extractedlist)

        Ok(extracted_product)
      //val extracted_product = Json.toJson(stockIsntance)


      case None =>

        Result(
          header = ResponseHeader(404, Map.empty)
          ,
          body = HttpEntity.Strict(ByteString(s"Cannot find product with produtctID=$productId in the database!"), Some("text/plain"))
        )
    }
  }


  def get_statistics(time:String) = Action {

    Products.getstats(time) match {

      case Right (s) =>



      case Left (s) =>


  }

}
