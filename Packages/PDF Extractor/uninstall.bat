@echo off

SET DLLFILE=Bytescout.PDFExtractor.dll 
SET TLBFILE=Bytescout.PDFExtractor.tlb

"C:\windows\Microsoft.NET\Framework64\v4.0.30319\regasm.exe" "%DLLFILE%" /tlb:"%TLBFILE%" /unregister 
