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

import org.beangle.commons.lang.Consoles
import org.openurp.edu.cert.LogSink

import java.nio.file.{Files, Path, Paths, StandardOpenOption}

/** 命令行试跑：委托 [[NeeaCetService]]；构造时已拉取验证码，全程日志见工作目录 `log.txt`。
 *
 * 参数：可选工作目录；可选第二个参数为 **TestID 字符串**（与 `snap-welcome-manage.html` 中 `changeTest` 一致，通常为数字）。未给时在终端询问。
 */
object Main {

  def main(args: Array[String]): Unit = {
    val outDir = workDirFromArgs(args)
    Files.createDirectories(outDir)

    /** 丢弃旧日志并创建空文件（构造时调用一次）。 */
    val logPath = outDir.toAbsolutePath.normalize.resolve(NeeaCetService.LogFileName)
    Files.writeString(logPath, "", StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
    val logSink = LogSink.fileAndStdout(logPath)
    val svc = new NeeaCetService(outDir, logSink)

    svc.log(s"工作目录: ${svc.workDir.toAbsolutePath}，日志: ${logPath.toAbsolutePath}")

    while (!svc.isLoggedIn) {
      val account = Consoles.prompt("证件号/手机/邮箱: ", null)
      val password = readPasswordOrLine("密码: ")
      svc.refreshCaptcha()
      svc.log(s"请查看验证码文件: ${svc.captchaImage.getAbsolutePath}")
      //默认大写验证码
      val code = Consoles.prompt("验证码（4 位）: ", null)(x => x.length == 4).toUpperCase()

      if !svc.login(account, password, code) then
        svc.log("登录未成功，请重试。")
    }

    Consoles.shell(svc.regOrg._2 + "> ", Set("exit", "quit", "q")) {
      case "list" =>
        println(s"可选考试任务编号 : ${svc.testIds.mkString(", ")}")
        println(s"可选科目编号 : ${svc.subjects.toList.sortBy(_._1).map(x => x._1 + " " + x._2).mkString(", ")}")
      case "export" =>
        val testId = Consoles.prompt("考试任务编号 : ", null) { x =>
          val matched = svc.testIds.contains(x)
          if (!matched) println(s"可选 : ${svc.testIds.mkString(", ")}")
          matched
        }
        val subjectId = Consoles.prompt("科目编号 :", null) { x =>
          val matched = svc.subjects.contains(x) || x == "*"
          if (!matched) println(s"可选 : ${svc.subjects.toList.sortBy(_._1).map(x => x._1 + " " + x._2).mkString(", ")}")
          matched
        }
        val exported = svc.downloadFiles(testId, subjectId)
        svc.log(s"本次导出文件数: ${exported.size}")
        exported.foreach(f => svc.log(s"  - ${f.getAbsolutePath}"))
      case _ => println("use list or export")
    }
    svc.logout()
  }

  private def workDirFromArgs(args: Array[String]): Path = {
    val raw = nonFlagArgs(args).headOption
    raw match
      case Some(dir) => Paths.get(dir).toAbsolutePath.normalize
      case None =>
        Paths.get(System.getProperty("user.dir"), "target", "neea-captcha").toAbsolutePath.normalize
  }

  private def nonFlagArgs(args: Array[String]): List[String] =
    args.filter(a => !a.startsWith("-")).toList

  /** 第二个非 `-` 参数为 TestID 字符串（首尾空白会去掉）。 */
  private def testIdFromArgs(args: Array[String]): Option[String] =
    nonFlagArgs(args).lift(1).map(_.trim).filter(_.nonEmpty)

  private def readLineTrim(prompt: String): String = {
    print(prompt)
    val line = scala.io.StdIn.readLine()
    if line == null then "" else line.trim
  }

  private def readPasswordOrLine(prompt: String): String = {
    val c = System.console()
    if c != null then
      print(prompt)
      val arr = c.readPassword()
      if arr == null then "" else new String(arr).trim
    else
      println("(当前无 Console，密码将以明文行输入)")
      readLineTrim(prompt)
  }

}
