module System.Log.HTML (
    defaultTimeLocale,

    htmlFmt, html
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import System.Locale
import System.Log.Base
import System.Log.Text

htmlFmt :: String -> String -> Converter Text
htmlFmt fmt title = Converter header c where
    c (Message tm l p msg) = T.unlines [
        T.pack $ "<div class=\"common " ++ typeClass l ++ "\">",
        T.pack $ "<span class=\"date\">" ++ formatTm tm ++ "</span>",
        T.pack $ "<b>" ++ classMsg l ++ "</b>.",
        T.concat [T.pack "<i>", T.intercalate (T.pack "/") (reverse p), T.pack "&gt;</i>"],
        msg,
        T.pack "</div>"]

    typeClass Trace = "trace"
    typeClass Debug = "except debug"
    typeClass Info = "div1 except info"
    typeClass Warning = "div0 except warning"
    typeClass Error = "div0 except error"
    typeClass Fatal = "div0 critical"

    classMsg Trace = "Trace info"
    classMsg Debug = "Debug info"
    classMsg Info = "Information"
    classMsg Warning = "Warning"
    classMsg Error = "Error"
    classMsg Fatal = "Critical error"

    formatTm tm = formatTime defaultTimeLocale fmt tm
    header = T.unlines $ map T.pack [
        "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"",
        "\"http://www.w3.org/TR/html4/loose.dtd\">",
        "<html>",
        "<head>",
        "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">",
        "<title>" ++ title ++ "</title>",
        "",
        "<style type=\"text/css\">",
        "body { font-family:Arial, Helvetica, sans-serif; font-size:13px; }",
        "",
        ".title { font-size:18px; }",
        "",
        ".date { font-size:10px; padding-right:5px; padding-left:20px; float: right; }",
        "",
        ".common {\npadding-left:20px;\npadding-top:5px;\nmargin-top:1px;\npadding-bottom:10px;\nmargin-bottom:10px;\n}",
        "",
        ".except {\nborder-width:3px;\nborder-style:solid;\ncolor:#000000;\n}",
        "",
        ".logfail { background-color:#1469E1; color:#FFFFFF; }",
        ".critical { background:#DD4400; color:#FFFFFF; }",
        ".error { border-color:#FF0033; }",
        ".warning { border-color:#FF9900; }",
        ".info { border-color:#339933; }",
        ".debug { border-color:#999966; }",
        ".trace { padding-bottom: 0px; padding-top: 0px; color: #666666; }",
        ".div0 { background-color: #F7F7FF; padding-top:10px; padding-bottom:5px; margin-top:15px; margin-bottom:10px; }",
        ".div1 { background-color: #F7FFF7; padding-top:10px; padding-bottom:5px; margin-top:15px; margin-bottom:10px; }",
        "</style>",
        "",
        "</head>",
        "<body>",
        "<p align=\"center\" class=\"title\">" ++ title ++ "</p>"]

html :: String -> Converter Text
html = htmlFmt defaultTimeFormat
