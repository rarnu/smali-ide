{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit allsynedit;

{$warn 5023 off : no warning about unused units}
interface

uses
  LazSynEditMouseCmdsTypes, LazSynEditText, LazSynTextArea, SynBeautifier, 
  SynCompletion, SynEdit, SynEditAutoComplete, SynEditExport, 
  SynEditFoldedView, SynEditHighlighter, SynEditHighlighterFoldBase, 
  SynEditHighlighterXMLBase, SynEditKeyCmds, SynEditLines, SynEditMarks, 
  SynEditMarkup, SynEditMarkupBracket, SynEditMarkupCtrlMouseLink, 
  SynEditMarkupGutterMark, SynEditMarkupHighAll, SynEditMarkupIfDef, 
  SynEditMarkupSelection, SynEditMarkupSpecialChar, SynEditMarkupSpecialLine, 
  SynEditMarkupWordGroup, SynEditMiscClasses, SynEditMiscProcs, 
  SynEditMouseCmds, SynEditPlugins, SynEditPointClasses, SynEditRegexSearch, 
  SynEditSearch, SynEditStrConst, SynEditTextBase, SynEditTextBidiChars, 
  SynEditTextBuffer, SynEditTextDoubleWidthChars, SynEditTextSystemCharWidth, 
  SynEditTextTabExpander, SynEditTextTrimmer, SynEditTypes, SynExportHTML, 
  SynGutter, SynGutterBase, SynGutterChanges, SynGutterCodeFolding, 
  SynGutterLineNumber, SynGutterLineOverview, SynGutterMarks, 
  SynHighlighterAny, SynHighlighterBat, SynHighlighterCpp, SynHighlighterCss, 
  SynHighlighterDiff, SynHighlighterHashEntries, SynHighlighterHTML, 
  SynHighlighterIni, SynHighlighterJava, SynHighlighterJScript, 
  SynHighlighterLFM, SynHighlighterMulti, SynHighlighterPas, 
  SynHighlighterPerl, SynHighlighterPHP, synhighlighterpike, SynHighlighterPo, 
  SynHighlighterPosition, SynHighlighterPython, SynHighlighterSQL, 
  SynHighlighterTeX, synhighlighterunixshellscript, SynHighlighterVB, 
  SynHighlighterXML, SynMacroRecorder, SynMemo, SynPluginMultiCaret, 
  SynPluginSyncroEdit, SynPluginSyncronizedEditBase, SynPluginTemplateEdit, 
  SynRegExpr, SynTextDrawer, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('SynEdit', @SynEdit.Register);
end;

initialization
  RegisterPackage('bs_SynEdit', @Register);
end.
