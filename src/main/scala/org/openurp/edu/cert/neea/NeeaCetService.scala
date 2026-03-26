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

import org.beangle.commons.net.http.Response
import org.openurp.edu.cert.LogSink
import org.openurp.edu.cert.neea.NeeaCetService.*

import java.io.File
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

/**
 * 考务 CET（通行证登录 → 回签 VerifyPassport → WelcomeManage → 成绩导出两步下载）整合入口。
 *
 * 构造时即建立 TLS 会话、拉取 CET 登录页并下载验证码图片到 [[workDir]]；对外用 [[captchaImage]]、登录成功后的 [[testIds]] 即可。
 *
 * @param workDir 工作目录：验证码、快照、`log.txt` 在根目录；按 TestID 下载的成绩表在子目录 `{TestID}/`（不存在则创建）。每次新建本类会清空并重建 `log.txt`。
 */
final class NeeaCetService(val workDir: Path, logSink: LogSink) {

  private val root: Path = workDir.toAbsolutePath.normalize

  private var client: NeeaCetClient = _
  private var loginPage: NeeaLoginPage = _
  private var captchaImageFile: File = _
  private var loggedIn: Boolean = false
  /**
   * 最近一次成功拉取的 WelcomeManage 首页中 `changeTest(…)` 解析出的考试任务 ID 列表（HTML 出现顺序、去重保序）。
   * 在 [[login]] 成功拉取首页后更新；[[downloadFiles]] 再次拉取首页时也会刷新。[[logout]]、[[refreshCaptcha]] 后为 `Nil`。
   */
  private var testIdSet: Set[String] = Set.empty
  private var subjectMap: Map[String, String] = Map.empty
  //code and name
  private var examOrg: (String, String) = _

  locally {
    Files.createDirectories(root)
    client = new NeeaCetClient(NeeaCetClient.newHttpSession(trustAll = true))
  }

  def isLoggedIn: Boolean = loggedIn

  /** 构造或 [[refreshCaptcha]] 后得到的验证码 JPG（位于 [[workDir]]）。 */
  def captchaImage: File = captchaImageFile

  def testIds: List[String] = {
    require(loggedIn)
    testIdSet.toList.sorted
  }

  def subjects: Map[String, String] = {
    require(loggedIn)
    subjectMap
  }

  /** 笔试报名机构：`_1` = `searchRegOrgCode`，`_2` = 成绩页 `searchRegOrg` 展示文本（如 `(31084)上海…`）。 */
  def regOrg: (String, String) = {
    require(loggedIn && null != examOrg)
    examOrg
  }

  /**
   * 丢弃当前登录状态，换新会话并重新拉取登录页与验证码（验证码错误、过期等）。
   *
   * @return 新的验证码 [[java.io.File]]
   */
  def refreshCaptcha(): File = {
    testIdSet = Set.empty
    loggedIn = false
    examOrg = null
    subjectMap = Map.empty
    val (p, f) = openCetLoginAndCaptcha()
    loginPage = p
    captchaImageFile = f
    f
  }

  /**
   * 使用构造时拉取的登录页提交账号、密码、四位验证码；回签考务并完成 WelcomeManage 首屏拉取。
   *
   * @return 是否判定为登录成功（回签 HTTP 成功且 WelcomeManage 拉取成功）
   */
  def login(account: String, password: String, captchaFourDigits: String): Boolean = {
    require(captchaFourDigits != null && captchaFourDigits.trim.length == 4, "验证码须为4位")
    loggedIn = false
    log("提交登录（强制登录 chkForce）…")
    val res = client.loginWithCaptcha(loginPage, account, password, captchaFourDigits.trim)
    log(s"登录 POST HTTP 状态: ${res.status}")
    saveHtmlIfLikelyHtml(HtmlSnapshots.PassportLoginPost, res)

    if !isPassportLoginPostSuccess(res) then
      log("登录 POST 未通过通行证校验（账号、密码或验证码错误），已跳过回签与 WelcomeManage。")
      return false

    log("回签考务会话（VerifyPassport 表单或 GET ReturnUrl）…")
    val verifyRes = client.establishCetSiteSessionAfterLogin(loginPage, res)
    log(s"VerifyPassport/ReturnUrl HTTP 状态: ${verifyRes.status}")
    saveHtmlIfLikelyHtml(HtmlSnapshots.CetKwVerifyPassport, verifyRes)

    log(s"拉取 WelcomeManage: ${NeeaCetClient.CetKwWelcomeManageIndexUrl}")
    val manageRes =
      client.getHtml(NeeaCetClient.CetKwWelcomeManageIndexUrl, Some(NeeaCetClient.CetKwOrigin + "/"))
    log(s"WelcomeManage HTTP 状态: ${manageRes.status}")
    saveHtmlIfLikelyHtml(HtmlSnapshots.WelcomeManage, manageRes)
    if manageRes.isOk then
      testIdSet = NeeaCetHtmlParse.parseChangeTestIds(manageRes.getText).toSet
      log(s"WelcomeManage 解析 TestID 列表: ${testIdSet.mkString(", ")}")

    val ok = verifyRes.isOk && manageRes.isOk
    if ok then {
      log("登录流程判定成功。")
      val extraOrgOk = extractOrgAndSubject()
      if (extraOrgOk) {
        loggedIn = ok
      }
    } else
      log("登录流程判定失败（回签或 WelcomeManage 非成功状态）。")
    ok
  }

  /**
   * 在已登录前提下，按 WelcomeManage → 指定 `testId` POST → 成绩页 → 各科两步导出。
   * 导出的 **非 HTML 文件**（成绩 xlsx）写入 `[[workDir]]/{TestID}/`（目录名经净化）；HTML 快照仍在 `workDir` 根目录。
   * 文件名为 `{testId}_{考点展示名}_{科目名}.xlsx`（各段经文件名净化）。
   *
   * @param testId 考试任务 ID（与页面 `changeTest(testId)` 一致，可先查看 `snap-welcome-manage.html`）
   * @return 成功写入的导出文件列表（不含 HTML 快照）；未登录时返回空列表并记日志。
   */
  def downloadFiles(testId: String, subjectId: String): List[File] = {
    require(loggedIn, "downloadFiles：当前未处于登录成功状态，跳过。")
    require(null != testId && testId.nonEmpty)
    require(testIds.contains(testId))
    log(s"downloadFiles：使用 TestID=$testId")
    if !changeTestId(testId) then return List.empty
    val exportDir = root.resolve(safeFileComponent(testId))
    Files.createDirectories(exportDir)
    log(s"导出目录: $exportDir")
    val acc = List.newBuilder[File]
    val filterSubjects = this.subjects.filter(x => subjectId == "*" || subjectId == x._1)
    for ((subCode, subName) <- filterSubjects) {
      val exportFields =
        NeeaCetHtmlParse.scoreManageExportFormFields(subCode.trim, this.examOrg._1, "0,1,2")
      val init =
        client.postCetKwForm(
          NeeaCetClient.CetKwScoreDownExportUrl,
          exportFields,
          Some(NeeaCetClient.CetKwScoreManageIndexUrl),
          accept = "application/json, text/plain, */*")
      val initBody = Option(init.getText).getOrElse("").trim()
      log(s"科目 $subCode (${subName.take(120)}) GetDownExportSearchScoreInfo HTTP ${init.status} 响应(JSON): $initBody")
      NeeaCetHtmlParse.parseDownExportSearchScoreInfoResult(initBody) match
        case None =>
          log("  无法解析为 JSON 对象或缺少 ExceuteResultType。")
        case Some((1, guid)) if guid != null && guid.nonEmpty =>
          val fileUrl = NeeaCetClient.cetKwScoreExportDownloadUrl(guid)
          val outName =
            s"${testId}_${safeFileComponent(this.examOrg._2)}_${safeFileComponent(subName)}.xlsx"
          val outFile = exportDir.resolve(outName).toFile
          val fileRes = client.getBinary(fileUrl, outFile)
          if fileRes.nonEmpty then
            log(s"  已写入: $outFile")
            acc += outFile
          else
            log(s"  下载文件失败")
        case Some((t, msg)) =>
          log(s"  ExceuteResultType=$t Message=${Option(msg).map(_.take(120)).getOrElse("")}")
    }
    acc.result()
  }

  /** 请求考务 `/logout`；不关闭 JVM 时可释放服务端会话。 */
  def logout(): Unit = {
    log(s"退出考务: ${NeeaCetClient.CetKwLogoutUrl}")
    val r = client.getHtml(NeeaCetClient.CetKwLogoutUrl, Some(NeeaCetClient.CetKwWelcomeManageIndexUrl))
    log(s"Logout HTTP 状态: ${r.status}")
    saveHtmlIfLikelyHtml(HtmlSnapshots.Logout, r)
    loggedIn = false
    testIdSet = Set.empty
    subjectMap = Map.empty
    examOrg = null
  }

  private def changeTestId(testId: String): Boolean = {
    require(loggedIn, "downloadFiles：当前未处于登录成功状态，跳过。")
    require(null != testId && testId.nonEmpty)
    require(testIds.contains(testId))
    log(s"拉取 WelcomeManage（用于校验 changeTest 列表）: ${NeeaCetClient.CetKwWelcomeManageIndexUrl}")
    val manageRes = client.getHtml(NeeaCetClient.CetKwWelcomeManageIndexUrl, Some(NeeaCetClient.CetKwOrigin + "/"))
    if !manageRes.isOk then
      log(s"downloadFiles WelcomeManage HTTP ${manageRes.status}，中止。")
      return false

    log(s"POST TestID=$testId → ${NeeaCetClient.CetKwWelcomeManageIndexUrl}")

    // 与页面 form#form1 一致，仅 POST TestID。若日后站点启用 ASP.NET MVC 防伪，可再解析 HTML 中的 __RequestVerificationToken 一并提交。
    val postTestRes =
      client.postCetKwForm(NeeaCetClient.CetKwWelcomeManageIndexUrl, List("TestID" -> testId), Some(NeeaCetClient.CetKwWelcomeManageIndexUrl))
    log(s"POST TestID HTTP 状态: ${postTestRes.status}")
    saveHtmlIfLikelyHtml(HtmlSnapshots.WelcomeManagePostTestId, postTestRes)
    if !postTestRes.isOk then
      log("POST TestID 未成功，结束导出。")
      false
    else true
  }

  /** 通过成绩查询页面获取组织机构和科目代码名称
   *
   * @return
   */
  private def extractOrgAndSubject(): Boolean = {
    log(s"拉取成绩管理页: ${NeeaCetClient.CetKwScoreManageIndexUrl}")
    val scoreIdxRes =
      client.getHtml(NeeaCetClient.CetKwScoreManageIndexUrl, Some(NeeaCetClient.CetKwWelcomeManageIndexUrl))
    log(s"SCOREMANAGE/Index HTTP 状态: ${scoreIdxRes.status}")
    saveHtmlIfLikelyHtml(HtmlSnapshots.ScoreManageIndex, scoreIdxRes)
    if !scoreIdxRes.isOk then
      log("成绩管理页未成功，结束导出。")
      return false

    val scoreHtml = scoreIdxRes.getText
    val orgCode = NeeaCetHtmlParse.parseInputValueByName(scoreHtml, "searchRegOrgCode")
    if (orgCode.isEmpty) {
      log(s"解析考点代码失败")
      return false
    }
    val orgDisplay = NeeaCetHtmlParse.parseInputValueByName(scoreHtml, "searchRegOrg")
      .map(_.trim).filter(_.nonEmpty).getOrElse("--")
    this.examOrg = (orgCode.get.trim, orgDisplay)
    log(s"searchRegOrgCode=${examOrg._1}, searchRegOrg=${examOrg._2}")
    val subjectOptions = NeeaCetHtmlParse.parseScoreSubjectSelectOptions(scoreHtml)
    subjectOptions.foreach { case (v, n) => log(s"  科目 value=$v => $n") }

    this.subjectMap = subjectOptions.filter((v, _) => v != null && v.trim.nonEmpty).map(x => (x._1, stripCode(x._1, x._2))).toMap
    if subjectMap.isEmpty then
      log("无有效科目")
      return false
    log(s"按 ${subjectMap.size} 个科目导出（JSON → guid → 文件）…")
    true
  }

  private def stripCode(code: String, name: String): String = {
    val prefix = s"（${code}）"
    if (name.startsWith(prefix)) {
      name.substring(prefix.length)
    } else {
      name
    }
  }

  /**
   * 通行证登录 POST 成功时返回自动 POST 到考务 `VerifyPassport` 的中间页（见 `snap-passport-login-post_success.html`）；
   * 失败时仍返回完整 CET 登录页 HTML（见 `snap-passport-login-post.html`），二者无需解析具体错误文案即可区分。
   */
  private def isPassportLoginPostSuccess(res: Response): Boolean = {
    val t = Option(res.getText).getOrElse("")
    t.contains("document.forms[0].submit()") ||
      (t.contains("id=\"form1\"") && t.contains("/Home/VerifyPassport/"))
  }

  private def saveHtmlIfLikelyHtml(fileName: String, res: Response): Unit = {
    if !isLikelyHtml(res) then return
    val bytes = res.getBytes
    if bytes == null || bytes.length == 0 then return
    Files.write(root.resolve(fileName), bytes)
    log(s"HTML 快照已覆盖: $fileName")
  }

  private def isLikelyHtml(res: Response): Boolean = {
    val ct = contentTypeOf(res).toLowerCase
    if ct.contains("json") then false
    else if ct.contains("text/html") || ct.contains("application/xhtml") then true
    else looksLikeMarkup(res)
  }

  private def looksLikeMarkup(res: Response): Boolean = {
    val t = res.getText
    t != null && {
      val s = t.stripLeading()
      s.startsWith("<!") || s.toLowerCase.startsWith("<html") || s.startsWith("<!--")
    }
  }

  private def contentTypeOf(res: Response): String =
    res.headers.iterator
      .find((k, _) => k.equalsIgnoreCase("Content-Type"))
      .flatMap((_, vs) => vs.headOption)
      .getOrElse("")

  private def safeFileComponent(name: String): String =
    if name == null || name.trim.isEmpty then "export"
    else name.trim.replaceAll("[\\\\/:*?\"<>|]", "_").take(200)

  /** 追加一行经 [[logSink]] 输出（默认即 `log.txt` + 标准输出）。 */
  def log(message: String): Unit = {
    val line = s"${LocalDateTime.now().format(LogTsFmt)} $message\n"
    logSink.accept(line)
  }

  private def saveHtmlString(fileName: String, html: String): Unit = {
    if html == null || html.isEmpty then return
    val bytes = html.getBytes(UTF_8)
    Files.write(root.resolve(fileName), bytes)
    log(s"HTML 快照已覆盖: $fileName")
  }

  /** 新建 HTTP 会话、拉登录页、存快照并下载验证码。 */
  private def openCetLoginAndCaptcha(): (NeeaLoginPage, File) = {
    log("创建 TLS+Cookie 会话并获取 CET 登录页…")
    val (page, cetLoginHtml) = client.fetchLoginPage()
    saveHtmlString(HtmlSnapshots.PassportCetLogin, cetLoginHtml)
    log(s"登录页 URL: ${page.postUrl}")
    log("请求验证码图片…")
    val captchaUrl = client.requestCaptchaImageUrl(page.postUrl)
    log(s"验证码图片 URL: $captchaUrl")
    val bytes = client.getBytesDirect(captchaUrl, Some(page.postUrl))
    val name = "captcha.jpg"
    val path = root.resolve(name)
    Files.write(path, bytes)
    log(s"验证码已保存: $path")
    (page, path.toFile)
  }

}

object NeeaCetService {
  val LogFileName: String = "log.txt"

  /** 固定文件名，每次运行覆盖；便于对照每一步 HTML。 */
  object HtmlSnapshots {
    val PassportCetLogin: String = "snap-passport-cetlogin.html"
    val PassportLoginPost: String = "snap-passport-login-post.html"
    val CetKwVerifyPassport: String = "snap-cetkw-verify-passport.html"
    val WelcomeManage: String = "snap-welcome-manage.html"
    val WelcomeManagePostTestId: String = "snap-welcome-manage-post-testid.html"
    val ScoreManageIndex: String = "snap-score-manage-index.html"
    val Logout: String = "snap-logout.html"
  }

  private val LogTsFmt = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS")
}
