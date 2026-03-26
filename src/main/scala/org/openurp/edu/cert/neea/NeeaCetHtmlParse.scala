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

import org.beangle.commons.json.Json

import java.util.regex.Pattern

/** 考务 kw 后台页 HTML 中的 `changeTest`、隐藏域、`select` 等解析（无网络依赖）。
 *
 * 成绩页（如 `SCOREMANAGE/Index`）中「报考科目」为 `<select id="searchSubjectCode">`，通常 **无 `name`**，须按 `id` 解析；
 * 导出时浏览器对 `GetDownExportSearchScoreInfo` 提交的是 `formSerachScoreInfo` 各字段的 **表单编码**（`application/x-www-form-urlencoded`），见 [[scoreManageExportFormFields]]。
 */
object NeeaCetHtmlParse {

  /** 与 WelcomeManage 页一致，如 `<a href="#" onclick="changeTest(261)">`（`=` 两侧可有空格，引号可为单/双）。 */
  private val ChangeTestRe = """(?i)onclick\s*=\s*["']changeTest\s*\(\s*(\d+)\s*\)""".r

  /**
   * 解析 `changeTest(…)` 中的考试/任务 TestID（数字串，与页面一致）。
   *
   * 顺序为 **HTML 中出现顺序**（与页面链接一致）；去重时保留首次出现。勿对结果排序后再取 `head`，否则会变成数值最小的 ID（如样例页首条为 261，最小为 171）。
   */
  def parseChangeTestIds(html: String): List[String] =
    if html == null || html.isEmpty then Nil
    else ChangeTestRe.findAllMatchIn(html).map(_.group(1)).toList.distinct

  /** 全文扫描 `<input …>`，按 `name` 取 `value`（双引号或单引号）。 */
  def parseInputValueByName(html: String, inputName: String): Option[String] = {
    parseAllInputPairs(html).collectFirst { case (n, v) if n == inputName => v }
  }

  private def parseAllInputPairs(html: String): List[(String, String)] = {
    if html == null || html.isEmpty then Nil
    else
      val NameDQ = """(?i)name\s*=\s*"([^"]*)"""".r
      val NameSQ = """(?i)name\s*=\s*'([^']*)'""".r
      val ValueDQ = """(?i)value\s*=\s*"([^"]*)"""".r
      val ValueSQ = """(?i)value\s*=\s*'([^']*)'""".r
      html.split("(?i)<input").drop(1).flatMap { chunk =>
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

  /** `select name="…"` 下各 `option` 的 `(value, 可见文本)`。 */
  def parseSelectOptionsByName(html: String, selectName: String): List[(String, String)] =
    parseSelectBlock(html, """name\s*=\s*["']""" + Pattern.quote(selectName) + """["']""")

  /** `select id="…"`（成绩页科目 `searchSubjectCode` 仅有 id）。 */
  def parseSelectOptionsById(html: String, elementId: String): List[(String, String)] =
    parseSelectBlock(html, """id\s*=\s*["']""" + Pattern.quote(elementId) + """["']""")

  /** 先按 `name="searchSubjectCode"`，若无则按 `id="searchSubjectCode"`（与考务成绩首页一致）。 */
  def parseScoreSubjectSelectOptions(html: String): List[(String, String)] = {
    val byName = parseSelectOptionsByName(html, "searchSubjectCode")
    if byName.nonEmpty then byName
    else parseSelectOptionsById(html, "searchSubjectCode")
  }

  private def parseSelectBlock(html: String, attrPatternInSelectTag: String): List[(String, String)] = {
    if html == null || html.isEmpty then Nil
    else
      val selectRe = ("""(?is)<select[^>]*""" + attrPatternInSelectTag + """[^>]*>(.*?)</select>""").r
      selectRe.findFirstMatchIn(html) match
        case None => Nil
        case Some(m) =>
          val inner = m.group(1)
          """(?i)<option\s+([^>]*?)>(.*?)</option>""".r.findAllMatchIn(inner).map { om =>
            val attrs = om.group(1)
            val text = stripHtmlTags(om.group(2)).trim
            val value =
              """(?i)value\s*=\s*["']([^"']*)["']""".r.findFirstMatchIn(attrs).map(_.group(1)).getOrElse("")
            (value, text)
          }.toList
  }

  /**
   * 成绩导出第一步：与页面 `formSerachScoreInfo` 中各 `input name` 一致（`public.js` 的 `formToJson` 先拼成对象再交给 `$.ajax`）。
   * jQuery 对普通对象默认按 **`application/x-www-form-urlencoded`** 提交，不是 JSON 请求体。
   * 这里的参数，虽然有的值是空的，但不要删除
   */
  def scoreManageExportFormFields(subjectCode: String, regOrgCode: String, candidateStatus: String): List[(String, String)] = {
    List(
      "SubjectCode" -> subjectCode,
      "RegOrgCode" -> regOrgCode,
      "Name" -> "",
      "IDNumber" -> "",
      "StudentNumber" -> "",
      "WrittenTestTicket" -> "",
      "SpokenTestTicket" -> "",
      "Absent" -> "",
      "Violation" -> "",
      "College" -> "",
      "Major" -> "",
      "EduLevel" -> "",
      "EduYear" -> "",
      "EnrollmentYear" -> "",
      "Grade" -> "",
      "Class" -> "",
      "CertificateNumber" -> "",
      "WrittenScoreStart" -> "",
      "WrittenScoreEnd" -> "",
      "Obstruction" -> "",
      "SpokenScore" -> "",
      "TranscriptType" -> "",
      "CandidateStatus" -> candidateStatus)
  }

  /**
   * 解析成绩导出**第一步**的响应 JSON（服务端字段名 `ExceuteResultType`，常见拼写如此）。
   * `ExceuteResultType==1` 时 `Message` 为第二步下载用的 `guid`；`-1` 等为错误信息。
   */
  def parseDownExportSearchScoreInfoResult(body: String): Option[(Int, String)] = {
    if body == null || body.isBlank || !body.trim().startsWith("{") then None
    else
      try
        val o = Json.parseObject(body.trim)
        val missing = -999999
        val t = o.getInt("ExceuteResultType", missing)
        if t == missing then None
        else Some((t, o.getString("Message", "")))
      catch {
        case _: Exception => None
      }
  }

  private def stripHtmlTags(s: String): String =
    s.replaceAll("(?s)<[^>]+>", "").trim

}
