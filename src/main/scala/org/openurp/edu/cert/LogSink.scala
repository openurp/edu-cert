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

package org.openurp.edu.cert

import java.io.{OutputStream, OutputStreamWriter, Writer}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardOpenOption}
import java.util.concurrent.BlockingQueue

/**
 * 流程日志的观察者：每条为带时间戳的整行（含末尾 `\n`），与写入 `log.txt` 的片段一致。
 *
 * Web 场景可用 [[LogSink.utf8Stream]] / [[LogSink.writer]] 接到响应流，或用 [[LogSink.chain]] 组合「写文件 + 推流」。
 */
trait LogSink {

  def accept(formattedLine: String): Unit
}

object LogSink {

  /** 丢弃日志（占位或未连接 SSE 时使用）。 */
  def ignore: LogSink = _ => ()

  /** 将每条整行日志投入阻塞队列，由独立线程（如 SSE 响应）取出后格式化为 `data:` 事件。 */
  def offerToQueue(q: BlockingQueue[String]): LogSink = (formattedLine: String) => { val _ = q.offer(formattedLine); () }

  /** 与原先 CLI 行为一致：追加到 `logFile` 并 `System.out.print`（无额外换行，行内已含 `\n`）。 */
  def fileAndStdout(logFile: Path): LogSink = (formattedLine: String) => {
    Files.writeString(logFile, formattedLine, StandardOpenOption.CREATE, StandardOpenOption.APPEND)
    System.out.print(formattedLine)
  }

  def chain(parts: LogSink*): LogSink = (formattedLine: String) =>
    parts.foreach(_.accept(formattedLine))

  def stringBuilder(target: StringBuilder): LogSink = (formattedLine: String) =>
    target.append(formattedLine)

  /** `flushEachLine = true` 时适合 chunked / SSE，便于客户端逐步看到日志。 */
  def writer(w: Writer, flushEachLine: Boolean = true): LogSink = (formattedLine: String) => {
    w.write(formattedLine)
    if flushEachLine then w.flush()
  }

  def utf8Stream(out: OutputStream, flushEachLine: Boolean = true): LogSink =
    writer(new OutputStreamWriter(out, UTF_8), flushEachLine)
}
