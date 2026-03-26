/*
 * Copyright (C) 2014, The OpenURP Software.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.openurp.edu.cert.neea

import org.beangle.commons.net.http.{HttpUtils, Request, Response}

import java.io.File
import java.net.{URLDecoder, URLEncoder}
import java.nio.charset.StandardCharsets.UTF_8
import java.time.Duration
import java.util.Base64
import javax.crypto.Cipher
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}

/** Parsed CET login form (passport.neea.edu.cn) hidden fields and POST target. */
final case class NeeaLoginPage(postUrl: String, hiddenFields: Map[String, String]) {

  def requestVerificationToken: String = {
    hiddenFields.getOrElse("__RequestVerificationToken", throw new IllegalStateException("missing __RequestVerificationToken"))
  }

  def hiddenPublicKeyExponent: String = {
    hiddenFields.getOrElse("HiddenPublicKeyExponent", throw new IllegalStateException("missing HiddenPublicKeyExponent"))
  }

  def hiddenPublicKeyModulus: String = {
    hiddenFields.getOrElse("HiddenPublicKeyModulus", throw new IllegalStateException("missing HiddenPublicKeyModulus"))
  }

}

/**
 * 使用 [[org.beangle.commons.net.http.HttpUtils]] 访问通行证。
 */
final class NeeaCetClient(val http: HttpUtils, val passportOrigin: String = NeeaCetClient.PassportOrigin) {

  import NeeaCetClient.*

  /** @return 解析结果与**整页** HTML 文本（便于落盘调试）。 */
  def fetchLoginPage(loginPageUrl: String = DefaultCetLoginUrl): (NeeaLoginPage, String) = {
    val resolvedUrl = normalizeCetLoginUrl(loginPageUrl)
    val getReq = Request.noBody
      .header("Accept", "text/html, */*")
    val res = http.get(resolvedUrl, getReq)
    if !res.isOk then
      throw new IllegalStateException(
        s"login page GET failed: HTTP ${res.status}${responseBodyHint(res)}")
    val html = res.getText
    val formHtml = extractLoginForm(html).getOrElse(
      throw new IllegalStateException("could not find form#loginForm in HTML"))
    (NeeaLoginPage(resolvedUrl, parseHiddenInputs(formHtml)), html)
  }

  def fetchCaptchaImageBytes(loginPageUrl: String): Array[Byte] = {
    val url = requestCaptchaImageUrl(loginPageUrl)
    getBytesDirect(url, Some(loginPageUrl))
  }

  def requestCaptchaImageUrl(loginPageUrl: String): String = {
    val postReq = Request.asForm("")
      .header("Referer", loginPageUrl)
      .header("Origin", passportOrigin)
    val res = http.post(s"$passportOrigin/CheckImage/LoadCheckImage", postReq)
    if !res.isOk then
      throw new IllegalStateException(
        s"LoadCheckImage failed: HTTP ${res.status}${responseBodyHint(res)}")
    val raw = res.getText.trim
    if raw.length >= 2 && raw.startsWith("\"") && raw.endsWith("\"") then raw.substring(1, raw.length - 1)
    else raw
  }

  /** GET 二进制（如验证码图）；已设浏览器 UA，可选 Referer。 */
  def getBytesDirect(uri: String, referer: Option[String] = None): Array[Byte] = {
    val b = Request.noBody
      .header("Accept", "*/*")
    referer.foreach(r => b.header("Referer", r))
    val r = http.get(uri, b)
    if !r.isOk then
      throw new IllegalStateException(
        s"GET failed: HTTP ${r.status}${responseBodyHint(r)}")
    r.getBytes
  }

  /**
   * @param forceLogin 默认 true：等同浏览器勾选「强制登录」，提交 `chkForce` 且 `HiddenSafe=1`，挤掉其它端通行证会话。若需与非强制行为一致可传 false。
   */
  def loginWithCaptcha(
                        page: NeeaLoginPage,
                        account: String,
                        plainPassword: String,
                        captchaFourDigits: String,
                        forceLogin: Boolean = true): Response = {
    require(captchaFourDigits != null && captchaFourDigits.length == 4,
      "captcha must be 4 characters (same as browser check)")

    val encPwd = encrypt(plainPassword, page.hiddenPublicKeyExponent, page.hiddenPublicKeyModulus)

    val hiddenSafe = if forceLogin then "1" else page.hiddenFields.getOrElse("HiddenSafe", "0")

    val fields = List.newBuilder[(String, Any)]
    page.hiddenFields.foreach { case (k, v) =>
      if k != "txtPassword" && k != "txtName" && k != "txtCheckImageValue" && k != "chkForce" then
        val value = if k == "HiddenSafe" then hiddenSafe else v
        fields += k -> value
    }
    fields += "txtName" -> account
    fields += "txtPassword" -> encPwd
    fields += "txtCheckImageValue" -> captchaFourDigits
    if forceLogin then fields += "chkForce" -> ""

    val body = Request.asForm(fields.result())
    val postReq = body
      .header("Referer", page.postUrl)
      .header("Origin", passportOrigin)
      .header("Accept", "text/html, */*")
    http.post(page.postUrl, postReq)
  }

  /**
   * 登录 POST 成功后，与浏览器一致完成考务会话：
   *   - 若响应体为通行证返回的 **自动提交表单**（`POST` → `…/Home/VerifyPassport/`），则解析字段并提交；
   *   - 否则回退为对 `ReturnUrl` 的 GET（旧行为）。
   */
  def establishCetSiteSessionAfterLogin(page: NeeaLoginPage, loginResponse: Response): Response = {
    val html = loginResponse.getText
    NeeaCetClient.extractVerifyPassportPostForm(html) match
      case Some((actionUrl, fields)) =>
        val body = Request.asForm(fields)
        val postReq = body
          .header("Referer", page.postUrl)
          .header("Origin", passportOrigin)
          .header("Accept", "text/html, */*")
        http.post(actionUrl, postReq)
      case None =>
        val target = NeeaCetClient.returnUrlFromLoginPageOrDefault(page.postUrl)
        val req = Request.noBody
          .header("Accept", "text/html, */*")
          .header("Referer", page.postUrl)
          .header("Origin", passportOrigin)
        http.get(target, req)
  }

  /**
   * 使用当前 [[http]] 会话（Cookie）GET HTML，用于通行证登录后访问考务站等。
   *
   * @param referer 可选，建议为 `https://cet-kw.neea.edu.cn/` 或实际上一跳，便于站点校验
   */
  def getHtml(url: String, referer: Option[String] = None): Response = {
    val b = Request.noBody
      .header("Accept", "text/html, */*")
    referer.foreach(r => b.header("Referer", r))
    http.get(url, b)
  }

  /** GET 任意二进制或未知类型（`Accept` 为通配），便于读取 `Content-Disposition` 等响应头。 */
  def getBinary(url: String, file: File): Option[File] = {
    if (http.download(url, file)) {
      Some(file)
    } else {
      None
    }
  }

  /** 考务站同会话 POST（`application/x-www-form-urlencoded`）。 */
  def postCetKwForm(url: String, fields: List[(String, Any)], referer: Option[String] = None,
                    accept: String = "text/html, */*"): Response = {
    val body = Request.asForm(fields)
    val b = body
      .header("Accept", accept)
      .header("Origin", CetKwOrigin)
    referer.foreach(r => b.header("Referer", r))
    http.post(url, b)
  }

  /** 考务站 POST JSON（成绩导出第一步等；响应多为 JSON 而非文件）。 */
  def postCetKwJson(url: String, jsonBody: String, referer: Option[String] = None): Response = {
    val body = Request.build(jsonBody, "application/json; charset=UTF-8")
    val b = body
      .header("Accept", "application/json, text/plain, */*")
      .header("Origin", CetKwOrigin)
    referer.foreach(r => b.header("Referer", r))
    http.post(url, b)
  }

  private def responseBodyHint(res: Response): String = {
    val t = res.getText
    if t == null || t.isEmpty then ""
    else
      val clip = if t.length > 400 then t.substring(0, 400) + "…" else t
      s"; body: $clip"
  }

}

object NeeaCetClient {

  val PassportOrigin: String = "https://passport.neea.edu.cn"

  /** 四六级报名网（考务）站点根，用于 Referer 等。 */
  val CetKwOrigin: String = "https://cet-kw.neea.edu.cn"

  /** 登录成功后后台管理首页 URL（cet-kw.neea.edu.cn）。 */
  val CetKwWelcomeManageIndexUrl: String = CetKwOrigin + "/WelcomeManage/Index"

  /** 考务站退出登录：`https://cet-kw.neea.edu.cn/logout`。 */
  val CetKwLogoutUrl: String = CetKwOrigin + "/logout"

  /** 成绩管理首页：`/SCOREMANAGE/Index`。 */
  val CetKwScoreManageIndexUrl: String = CetKwOrigin + "/SCOREMANAGE/Index"

  /**
   * 成绩导出**第一步**：`POST application/x-www-form-urlencoded` 后响应为 **JSON**（非文件流）。成功时 `Message` 为临时 `guid`。
   * 浏览器对应 `$.ajax({ type:'post', url: 'GetDownExportSearchScoreInfo', data: formToJson(...) })`：`formToJson` 返回对象，jQuery 默认按表单编码提交，不是 JSON body。
   */
  val CetKwScoreDownExportUrl: String = CetKwOrigin + "/SCOREMANAGE/GetDownExportSearchScoreInfo"

  /**
   * 成绩导出**第二步**：用第一步 JSON 里的 `guid` 请求实际文件（`GET …?guid=`），与页面里 `iframe.src = GetDownExportSearchScoreInfoFile?guid=` 一致。
   */
  val CetKwScoreDownExportFileUrl: String = CetKwOrigin + "/SCOREMANAGE/GetDownExportSearchScoreInfoFile"

  val BrowserLikeUserAgent: String =
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36"

  private val AesCbc = "AES/CBC/PKCS5Padding"

  def encrypt(plainPassword: String, keyUtf8: String, ivUtf8: String): String = {
    val keyBytes = keyUtf8.getBytes(UTF_8)
    val ivBytes = ivUtf8.getBytes(UTF_8)
    require(keyBytes.length == 16 || keyBytes.length == 24 || keyBytes.length == 32,
      s"AES key must be 16/24/32 bytes, got ${keyBytes.length}")
    require(ivBytes.length == 16, s"AES CBC IV must be 16 bytes, got ${ivBytes.length}")

    val cipher = Cipher.getInstance(AesCbc)
    cipher.init(
      Cipher.ENCRYPT_MODE,
      new SecretKeySpec(keyBytes, "AES"),
      new IvParameterSpec(ivBytes)
    )
    val encrypted = cipher.doFinal(plainPassword.getBytes(UTF_8))
    Base64.getEncoder.encodeToString(encrypted)
  }

  /** `HttpUtils.session` + 内存 Cookie + [[BrowserLikeUserAgent]]，供 [[NeeaCetClient]] 使用。 */
  def newHttpSession(trustAll: Boolean = true): HttpUtils =
    val h = HttpUtils.session(trustAll, Duration.ofMinutes(5))
    h.setUserAgent(BrowserLikeUserAgent)
    h

  val DefaultCetReturnUrl: String =
    "https://cet-kw.neea.edu.cn/Home/VerifyPassport/?LoginType=1&Safe=1"

  val DefaultCetLoginUrl: String = cetLoginUrl(DefaultCetReturnUrl)

  def cetLoginUrl(returnUrlUnencoded: String): String =
    PassportOrigin + "/CETLogin?ReturnUrl=" + URLEncoder.encode(returnUrlUnencoded, UTF_8)

  def cetKwScoreExportDownloadUrl(guid: String): String =
    CetKwScoreDownExportFileUrl + "?guid=" + URLEncoder.encode(guid, UTF_8)

  /** 从通行证 `CETLogin?ReturnUrl=...` 登录页 URL 中解析出解码后的 ReturnUrl；若无则空。 */
  def returnUrlFromLoginPageOrDefault(loginPageUrl: String): String =
    extractReturnUrlFromPassportCetLoginUrl(loginPageUrl).getOrElse(DefaultCetReturnUrl)

  private def extractReturnUrlFromPassportCetLoginUrl(loginPageUrl: String): Option[String] = {
    val q = loginPageUrl.indexOf('?')
    if q < 0 then None
    else
      loginPageUrl
        .substring(q + 1)
        .split('&')
        .flatMap { seg =>
          seg.split("=", 2) match
            case Array(k, v) if k.equalsIgnoreCase("ReturnUrl") =>
              Some(URLDecoder.decode(v, UTF_8))
            case _ => None
        }
        .headOption
  }

  def normalizeCetLoginUrl(url: String): String = {
    val marker = "/cetlogin?returnurl="
    val lower = url.toLowerCase
    val idx = lower.indexOf(marker)
    if idx < 0 then url
    else
      val value = url.substring(idx + marker.length)
      val encoded =
        if value.startsWith("https%3A") || value.startsWith("http%3A") then value
        else URLEncoder.encode(value, UTF_8)
      url.substring(0, idx + marker.length) + encoded
  }

  private def extractLoginForm(html: String): Option[String] = {
    val lower = html.toLowerCase
    val key = """id="loginform""""
    val i = lower.indexOf(key)
    if i < 0 then None
    else
      val formStart = html.lastIndexOf("<form", i)
      if formStart < 0 then None
      else
        val j = html.indexOf("</form>", i)
        if j < 0 then None
        else Some(html.substring(formStart, j + "</form>".length))
  }

  /**
   * 通行证登录成功后返回的 HTML：`form` `POST` 到考务 `VerifyPassport`，`onload` 自动 submit。
   * 解析 `action` 与全部 `input` 的 name/value（支持 `value="…"` 与 `value='…'`），顺序与文档一致。
   */
  private def extractVerifyPassportPostForm(html: String): Option[(String, List[(String, String)])] = {
    if html == null || html.isEmpty then None
    else
      extractFormContainingVerifyPassport(html) match
        case None => None
        case Some(formHtml) =>
          parseFormActionUrl(formHtml).map { action =>
            (action, parseInputFieldsInOrder(formHtml))
          }
  }

  private def extractFormContainingVerifyPassport(html: String): Option[String] = {
    val lower = html.toLowerCase

    def loop(from: Int): Option[String] = {
      val formIdx = lower.indexOf("<form", from)
      if formIdx < 0 then None
      else
        val formEnd = lower.indexOf("</form>", formIdx)
        if formEnd < 0 then None
        else
          val fragment = html.substring(formIdx, formEnd + "</form>".length)
          val fl = fragment.toLowerCase
          if fl.contains("verifypassport") && fl.contains("post") then Some(fragment)
          else loop(formEnd + "</form>".length)
    }

    loop(0)
  }

  private val FormAction = """(?i)\baction\s*=\s*["']([^"']+)["']""".r

  private def parseFormActionUrl(formHtml: String): Option[String] =
    FormAction.findFirstMatchIn(formHtml).map(_.group(1).trim).filter(_.nonEmpty)

  /** 表单内每个 `<input` 一段，取 name 与 value（双引号或单引号 value）。 */
  private def parseInputFieldsInOrder(formHtml: String): List[(String, String)] = {
    val NameDQ = """(?i)name\s*=\s*"([^"]*)"""".r
    val NameSQ = """(?i)name\s*=\s*'([^']*)'""".r
    val ValueDQ = """(?i)value\s*=\s*"([^"]*)"""".r
    val ValueSQ = """(?i)value\s*=\s*'([^']*)'""".r
    formHtml.split("(?i)<input").drop(1).flatMap { chunk =>
      val nameOpt =
        NameDQ.findFirstMatchIn(chunk).map(_.group(1))
          .orElse(NameSQ.findFirstMatchIn(chunk).map(_.group(1)))
      nameOpt.map { n =>
        val v =
          ValueDQ.findFirstMatchIn(chunk).map(_.group(1))
            .orElse(ValueSQ.findFirstMatchIn(chunk).map(_.group(1)))
            .getOrElse("")
        (n, v)
      }
    }.toList
  }

  private def parseHiddenInputs(formHtml: String): Map[String, String] = {
    parseInputFieldsInOrder(formHtml).toMap
  }

}
