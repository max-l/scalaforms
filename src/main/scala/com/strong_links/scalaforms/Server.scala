package com.strong_links.scalaforms

import com.strong_links.core._
import com.strong_links.scalaforms._

import unfiltered.request._
import unfiltered.response._
import javax.servlet.http._
import java.io._
import java.sql.Driver
import org.slf4j.LoggerFactory
import unfiltered.{ Cookie => UCookie }

object Server {
  def cookiesAsSeq(m: Map[String, Option[UCookie]]) =
    m.values.filterNot(_ == None).map(_.get).toSeq
}

trait Server extends Logging {

  def activeRoles: Seq[Role]

  /**
   * The admin role is no different than others, except that generally permission giving
   * actions should be exclusive to this role, it is exposed in this (Server) trait
   * mainly to force applicative code to designate a role as being 'administrative'.
   */
  def adminRole: Role

  def anonymousRole: Role

  private val _adminRole = adminRole

  private val _anonymousRole = anonymousRole

  val allRoles = activeRoles ++ Seq(_adminRole) ++ Seq(_anonymousRole)

  def maxSecureSessionIdleTimeInSeconds = 5 * 60

  def serverSecretKey = "huhuhs439875c3p4k05-02598vk9430tv9i34ti40-ti04it".getBytes("UTF-8")

  /**
   * This is used as the 'path' for cookies emited by this server instance
   */
  def applicationRootPath = intWebroot

  private val applicationCookiePath = applicationRootPath + "/"

  def retrieveSslSessionId(request: HttpRequest[_]) =
    Some("uryte54jyteo9jyf89oy5tf8e4f8")

  private def isDev = true

  def authenticatingUris: Seq[(IdentityTrustLevel, Uri)]

  private val _authenticatingUris =
    authenticatingUris.map(t => (t._1, t._2.uri)).toMap

  private def getAuthenticatingUriFor(l: IdentityTrustLevel) =
    _authenticatingUris.get(l).getOrElse(Errors.fatal("No authenticating interaction for level _" << l))

  private object InteractionHandler extends unfiltered.filter.Plan {

    def intent: PartialFunction[HttpRequest[HttpServletRequest], ResponseFunction[HttpServletResponse]] = {
      case httpRequest: HttpRequest[_] =>
        val (isPost, originalPath, params, cookieMap) = httpRequest match {
          case GET(Path(p) & Params(params) & Cookies(cookies)) => (false, p, params, cookies)
          case POST(Path(p) & Params(params) & Cookies(cookies)) => (true, p, params, cookies)
          case _ => Errors.fatal("Unsupported request.")
        }

        logInfo("Intercepting URI _." << originalPath)

        val cookies = Server.cookiesAsSeq(cookieMap)

        if (isDev && originalPath.endsWith("/ClearCookies"))
          SetCookies(SessionCookieManager.resetAllCookies(applicationCookiePath): _*) ~> Redirect("/int/DumpCookies")
        else if (isDev && originalPath.endsWith("/DumpCookies"))
          ResponseString(dumpCookies(cookies))
        else
          processUri(isPost, originalPath, params, cookies, httpRequest)
    }
  }

  private def dumpCookies(c: Seq[UCookie]) =
    "Cookies : " + c.mkString("[\n", "\n", "\n]")

  def processUri(isPost: Boolean, originalPath: String, params: Map[String, Seq[String]], inputCookies: Seq[UCookie], httpRequest: HttpRequest[javax.servlet.http.HttpServletRequest]) = {

    val uriExtracter = new UriExtracter(originalPath)

    def createStreamer(renderFunc: (ServerOutputStream) => Unit) =
      new ResponseStreamer {
        def stream(os: OutputStream) {
          val out = new ServerOutputStream(os)

          DebugTls.using(httpRequest, params, inputCookies) {
            uriExtracter.module.surroundResponseStreamer(out, renderFunc(out))
          }

          out.flush
          os.close
        }
      }

    def prependCookies[R](r: ResponseFunction[R], cookies: Seq[UCookie]) = {
      logDebug("Outgoing " + dumpCookies(cookies))
      SetCookies(cookies: _*) ~> r
    }

    def invalidRequest = Errors.fatal("Invalid request _." << originalPath)

    val interactionDefinition = uriExtracter.interactionDefinition
    val sslSessionId = retrieveSslSessionId(httpRequest)
    val isSecureConnection = sslSessionId.isDefined
    val cm = new SessionCookieManager(
      applicationCookiePath,
      inputCookies,
      serverSecretKey,
      interactionDefinition.requiredTrustLevel,
      sslSessionId)

    //... modal forms ?

    val ic = new InteractionContext(
      interactionDefinition.requiredTrustLevel,
      cm.anonymousNonSecureSessionId,
      cm.userCorrelationId,
      cm.secureSessionToken.map(_.userId),
      inputCookies)

    def surroundCall[A](a: => A): A =
      uriExtracter.module.surroundServerCall(ic, a)

    val interaction = interactionDefinition.createInteraction(ic)

    val maxTrustLevelUpgradeIfAuthenticationForm =
      interaction match {
        case LoginInteraction(l) => Some(l)
        case _ => None
      }

    val isAuthentication =
      maxTrustLevelUpgradeIfAuthenticationForm.isDefined

    def processLoginResult(i: Interaction, res: (LoginResult[IdentityTrustLevel], (ServerOutputStream) => Unit)) = res match {
      case (LoginSuccess(userId, newTrustLevel, Uri(defaultNextUri)), onFailRenderer) => {
        val nextUri = cm.nextUriAfterAuth.getOrElse(defaultNextUri)
        // Send a new authenticator cookie :
        prependCookies(Redirect(nextUri), Seq(cm.createStrongAuthenticator(userId, newTrustLevel)))
      }
      case (LoginFailure(errorMessage, _), onFailRenderer) =>
        createStreamer(onFailRenderer)
      case result =>
        Errors.fatal("Invalid post result _ for LoginInteraction _." << (result, i))
    }

    val renderFuncOrRedirect =
      if (isAuthentication && sslSessionId.isEmpty)
        Errors.fatal("Cannot authenticate on a non TLS connection.")
      else if (cm.secureSessionToken.isDefined && cm.secureSessionTokenStatusIfExists.get == ToughCookieStatus.Invalid)
        Redirect("/securityBreachAttempt")
      else if (cm.needsToAuthenticate)
        prependCookies(Redirect(getAuthenticatingUriFor(interactionDefinition.requiredTrustLevel)), Seq(cm.cookieForNextUriAfterAuth(originalPath)))
      else interaction match {
        case i @ LoginInteraction(_) if isPost =>
          processLoginResult(i, surroundCall(i.processPost(params)))
        case fi: FormInteraction if isPost => {
          surroundCall(fi.processPost(params)) match {
            case (FormPostResult(true, _, Some(Uri(nextUri)), _), onFailRenderer) =>
              Redirect(nextUri)
            case (FormPostResult(false, message, _, _), onFailRenderer) =>
              createStreamer(onFailRenderer)
            case _ => invalidRequest
          }
        }
        case i: LogoutInteraction =>
          prependCookies(createStreamer(surroundCall(i.processGet(_))), cm.seqOfStrongAuthTerminationCookiesIfAuthenticated)
        case i @ LoginGetInteraction(_) =>
          processLoginResult(i, surroundCall(i.processLoginGet(params)))
        case i: Interaction =>
          createStreamer(surroundCall(i.processGet(_)))
        case _ => invalidRequest
      }

    val outCookies = cm.outgoingAnonymousNonSecureSessionIdCookieIfRequired ++
      ic.outgoingCookies.map(t => {
        val (value, formater) = t
        UCookie(formater.cookieName, value).copy(maxAge = Some(formater.maxAgeInSeconds))
      }).toSeq

    prependCookies(renderFuncOrRedirect, outCookies)
  }

  def start(port: Int, host: String, staticResourceNodes: Seq[StaticResourceNode]): Unit = {

    Logging.setLogger(LoggerFactory.getLogger)

    logInfo("Starting server on port: _" <<< port)
    logInfo("Static resource nodes: _" <<< staticResourceNodes)

    // Create basic server that can serve static resources
    val server = Unfiltered.makeServer(port, host, staticResourceNodes)

    // Add the ability to serve "Interaction" requests.
    server.context(intWebroot) { ctx =>
      ctx.filter(InteractionHandler)
    }

    // Add the ability to serve "Comet" requests.
    server.context(cometWebroot) { ctx =>
      ComedDServerImpl.createBayeuxHandlerInto(ctx.current)
    }

    server.run
  }
}

object DebugTls extends ThreadLocalStack[(HttpRequest[HttpServletRequest], Map[String, Seq[String]], Seq[UCookie])] {

  private def headers(req: HttpRequest[HttpServletRequest]) = {

    import scala.collection.JavaConversions._
    import req.underlying._

    val hNames = getHeaderNames.asInstanceOf[java.util.Enumeration[String]].toSeq

    for (n <- hNames)
      yield (n, getHeader(n))
  }

  def dumpAll(out: ServerOutputStream) = map { x =>
    val (req, args, cookies) = x
    out << ("Http Method: _\n" << req.underlying.getMethod())
    out << "Headers : \n"

    for ((hName, hValue) <- headers(req)) {
      out << ("  _ -> _\n" << (hName, hValue))
    }

    out << ("URL: _\n" << req.underlying.getRequestURL())
    out << cookies.sortBy(_.name).map(c => "_ -> _" <<< (c.name, c.value)).mkString("Cookies : \n  ", "\n  ", "")

    if (!args.isEmpty)
      out << "\nParams :"

    for (keyWithSeqOfValues <- args.toSeq.sortBy(_._1)) {
      out << ("\n  '_' -> _" << (keyWithSeqOfValues._1, keyWithSeqOfValues._2.map("'" + _ + "'").mkString("[", ",", "]")))
    }
  }
}
