unit synhighlightersmali;

{$I SynEdit.inc}

interface

uses
  Graphics,
  GraphType, 
  SynEditTypes,
  SynEditHighlighter,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSecondKey, tkThirdKey,
    tkSpace, tkString, tkSymbol, tkVariable, tkUnknown);

  TRangeState = (rsUnknown, rsAnsi, rsPasStyle, rsCStyle);

  TProcTableProc = procedure of object;

type

  { TSynSmaliSyn }

  TSynSmaliSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fLine: PChar;
    fProcTable: array[#0..#255] of TProcTableProc;
    FThirdKeyAttri: TSynHighlighterAttributes;
    FThirdKeyWords: TStrings;
    Run: LongInt;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    fLineNumber: Integer;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSecondKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fVarAttri: TSynHighlighterAttributes;
    fKeyWords: TStrings;
    fSecondKeys: TStrings;
    procedure BraceOpenProc;
    procedure PointCommaProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure RoundOpenProc;
    procedure SetThirdKeyWords(Value: TStrings);
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure MakeMethodTables;
    procedure AnsiProc;
    procedure PasStyleProc;
    procedure CStyleProc;
    procedure DollarProc;
    procedure DotProc;
    procedure SetSecondKeys(const Value: TStrings);
  protected
    function GetIdentChars: TSynIdentChars; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(const NewValue: String; LineNumber:Integer); override;
    function IsKeyword(const AKeyword: string): boolean; override;
    function IsSecondKeyWord(aToken: string): Boolean;
    function IsThirdKeyWord(aToken: string): Boolean;
    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property SecondKeyAttri: TSynHighlighterAttributes read fSecondKeyAttri write fSecondKeyAttri;
    property SecondKeyWords: TStrings read fSecondKeys write SetSecondKeys;
    property ThirdKeyAttri: TSynHighlighterAttributes read FThirdKeyAttri write FThirdKeyAttri;
    property ThirdKeyWords: TStrings read FThirdKeyWords write SetThirdKeyWords;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
    property VarAttri: TSynHighlighterAttributes read fVarAttri write fVarAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  ShellScriptKeysCount = 45;
  ShellScriptKeys: array[1..ShellScriptKeysCount] of string = (
    'super', 'class', 'implements', 'source', 'method', 'end', 'prologue', 'local', 'catch', 'catchall',
    'sparse', 'switch', 'packed', 'array', 'data', 'field', 'annotation', 'subannotation',
    'enum', 'restart', 'line', 'param', 'registers', 'locals', 'parameter', 'interface', 'public', 'protected',
    'private', 'abstract', 'static', 'final', 'synchronized', 'transient', 'volatile', 'native', 'strictfp', 'synthetic',
    'bridge', 'varargs', 'declared', 'build', 'runtime', 'system', 'constructor');

  ShellScriptSecondKeysCount = 55;
  ShellScriptSecondKeys: array[1..ShellScriptSecondKeysCount] of string = (
    'init', 'clinit', 'const', 'wide', 'string', 'check', 'cast', 'new', 'instance',
    'nop', 'throw', 'move', 'result', 'object', 'exception', 'array', 'length', 'neg', 'not',
    'invoke', 'direct', 'virtual', 'iget', 'iput', 'return', 'void', 'monitor', 'aget', 'aput',
    'enter', 'exit', 'if', 'mul', 'add', 'sub', 'rsub', 'div', 'rem', 'and', 'or', 'xor',
    'shl', 'shr', 'ushr', 'addr', 'instance', 'of', 'sget', 'sput', 'goto', 'cmp', 'cmpl', 'cmpg',
    'fill', 'filled'
    );
  ShellScriptThirdKeyCount = 20;
  ShellScriptThirdKeys: array[1..ShellScriptThirdKeyCount] of string = (
    'int', 'long', 'float', 'double', 'to', 'byte', 'char', 'short', 'ne', 'lt', 'ge', 'gt', 'le', 'eq',
    'nez', 'ltz', 'gez', 'gtz', 'lez', 'eqz'
  );

var
  Identifiers: array[#0..#255] of ByteBool;

procedure MakeIdentTable;
var
  I: Char;
begin
  for I := #0 to #255 do
  begin
    case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z':
        Identifiers[I] := True;
      else
        Identifiers[I] := False;
    end;
  end;
end;

function TSynSmaliSyn.IsKeyword(const AKeyword: string): boolean;
begin
  Result :=  (fKeyWords.IndexOf(AKeyword) <> -1);
end; { IsKeyWord }

function TSynSmaliSyn.IsSecondKeyWord(aToken: string): Boolean;
begin
  Result := (fSecondKeys.IndexOf(aToken) <> -1);
end; { IsSecondKeyWord }

function TSynSmaliSyn.IsThirdKeyWord(aToken: string): Boolean;
begin
  Result := (FThirdKeyWords.IndexOf(aToken) <> -1);
end;

procedure TSynSmaliSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      '#': fProcTable[I] := @SlashProc;
      '{': fProcTable[I] := @BraceOpenProc;
      ';', '.': fProcTable[I] := @PointCommaProc;
      '/': fProcTable[I] := @SlashProc;
      #13: fProcTable[I] := @CRProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := @IdentProc;
      #10: fProcTable[I] := @LFProc;
      #0: fProcTable[I] := @NullProc;
      '0'..'9': fProcTable[I] := @NumberProc;
      '(': fProcTable[I] := @RoundOpenProc;
      '$': fProcTable[i] := @DollarProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := @SpaceProc;
      #34, #39: fProcTable[I] := @StringProc;
      else fProcTable[I] := @UnknownProc;
    end;
end;

constructor TSynSmaliSyn.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited Create(AOwner);
  fKeyWords := TStringList.Create;
  TStringList(fKeyWords).Sorted := True;
  TStringList(fKeyWords).Duplicates := dupIgnore;
  fSecondKeys := TStringList.Create;
  TStringList(fSecondKeys).Sorted := True;
  TStringList(fSecondKeys).Duplicates := dupIgnore;
  FThirdKeyWords := TStringList.Create;
  TStringList(FThirdKeyWords).Sorted:= True;
  TStringList(FThirdKeyWords).Duplicates:= dupIgnore;

  for i := 1 to ShellScriptKeysCount do fKeyWords.Add(ShellScriptKeys[i]);
  for i := 1 to ShellScriptSecondKeysCount do fSecondKeys.Add(ShellScriptSecondKeys[i]);
  for i := 1 to ShellScriptThirdKeyCount do FThirdKeyWords.Add(ShellScriptThirdKeys[i]);

  fCommentAttri := TSynHighlighterAttributes.Create(@SYNS_AttrComment, SYNS_XML_AttrComment);
  fCommentAttri.Foreground := clGreen;
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(@SYNS_AttrIdentifier, SYNS_XML_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(@SYNS_AttrReservedWord, SYNS_XML_AttrReservedWord);
  fKeyAttri.Foreground := clBlue;
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fSecondKeyAttri := TSynHighlighterAttributes.Create(@SYNS_AttrSecondReservedWord, SYNS_XML_AttrSecondReservedWord);
  fSecondKeyAttri.Foreground:= clNavy;
  AddAttribute(fSecondKeyAttri);
  FThirdKeyAttri := TSynHighlighterAttributes.Create(@SYNS_AttrThirdReservedWord, SYNS_XML_AttrThirdReservedWord);
  FThirdKeyAttri.Foreground:= clPurple;
  AddAttribute(FThirdKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(@SYNS_AttrNumber, SYNS_XML_AttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(@SYNS_AttrSpace, SYNS_XML_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(@SYNS_AttrString, SYNS_XML_AttrString);
  fStringAttri.Foreground := clMaroon;
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(@SYNS_AttrSymbol, SYNS_XML_AttrSymbol);
  fSymbolAttri.Foreground := clGray;
  AddAttribute(fSymbolAttri);
  fVarAttri := TSynHighlighterAttributes.Create(@SYNS_AttrVariable, SYNS_XML_AttrVariable);
  AddAttribute(fVarAttri);
  SetAttributesOnChange(@DefHighlightChange);

  MakeMethodTables;
  fRange := rsUnknown;
  fDefaultFilter := SYNS_FilterUNIXShellScript;
end; { Create }

procedure TSynSmaliSyn.SetLine(
  const NewValue: String;
  LineNumber:Integer);
begin
  inherited;
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end; { SetLine }

destructor TSynSmaliSyn.Destroy;
begin
  fKeyWords.Free;
  fSecondKeys.Free;
  FThirdKeyWords.Free;
  inherited Destroy;
end; { Destroy }

procedure TSynSmaliSyn.AnsiProc;
begin
  fTokenID := tkComment;
  case FLine[Run] of
    #0:
      begin
        NullProc;
        exit;
      end;
    #10:
      begin
        LFProc;
        exit;
      end;

    #13:
      begin
        CRProc;
        exit;
      end;
  end;

  while fLine[Run] <> #0 do
    case fLine[Run] of
      '*':
        if fLine[Run + 1] = ')' then
        begin
          fRange := rsUnKnown;
          inc(Run, 2);
          break;
        end else inc(Run);
      #10: break;

      #13: break;
    else inc(Run);
    end;
end;

procedure TSynSmaliSyn.PasStyleProc;
begin
  fTokenID := tkComment;
  case FLine[Run] of
    #0:
      begin
        NullProc;
        exit;
      end;
    #10:
      begin
        LFProc;
        exit;
      end;

    #13:
      begin
        CRProc;
        exit;
      end;
  end;

  while FLine[Run] <> #0 do
    case FLine[Run] of
      '}':
        begin
          fRange := rsUnKnown;
          inc(Run);
          break;
        end;
      #10: break;

      #13: break;
    else inc(Run);
    end;
end;

procedure TSynSmaliSyn.CStyleProc;
begin
  case fLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkComment;
    repeat
      if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then
      begin
        fRange := rsUnKnown;
        Inc(Run, 2);
        break;
      end;
      Inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end;
end;

procedure TSynSmaliSyn.DollarProc;
var
  cc: Char;
begin
  inc(Run);
  fTokenID := tkVariable;
  if FLine[Run] = #0 then Exit;
  cc := FLine[Run];
  inc(Run);
  if (cc = '{') then begin
    // ${var}
    while FLine[Run] in IdentChars do begin
      case FLine[Run] of
        #0, #10, #13: Break;
      end;
      inc(Run);
    end;
    if FLine[Run] = '}' then Inc(Run);
  end else
    // $var
    while FLine[Run] in IdentChars do
      inc(Run);
end;

procedure TSynSmaliSyn.DotProc;
  function TestDot: boolean;
  var
    i: integer;
  begin
    result := false;
    i := run;
    inc(i);
    while (FLine[i] in ['a'..'z', 'A'..'Z']) do
      inc(i);
    if i > (run + 1) then
      result := true;
    if result then
      run := i;
  end;
begin
  if TestDot then
    fTokenID := tkIdentifier
  else begin
    inc(Run);
    fTokenID := tkSymbol;
  end;
end;

procedure TSynSmaliSyn.BraceOpenProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSmaliSyn.PointCommaProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSmaliSyn.CRProc;
begin
  fTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: inc(Run, 2);
  else inc(Run);
  end;
end;

procedure TSynSmaliSyn.IdentProc;
begin
  while Identifiers[fLine[Run]] do inc(Run);
  if IsKeyWord(GetToken) then begin
    fTokenId := tkKey;
    Exit;
  end
  else fTokenId := tkIdentifier;
  if IsSecondKeyWord(GetToken) then
    fTokenId := tkSecondKey
  else if IsThirdKeyWord(GetToken) then
    FTokenID:= tkThirdKey
  else
    FTokenID:= tkIdentifier;
end;

procedure TSynSmaliSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynSmaliSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynSmaliSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', 'e', 'E'] do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
end;

procedure TSynSmaliSyn.RoundOpenProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynSmaliSyn.SetThirdKeyWords(Value: TStrings);
var
  i: Integer;
begin
  FThirdKeyWords.Assign(Value);
  DefHighLightChange(nil);
end;

procedure TSynSmaliSyn.SlashProc;
begin
  if FLine[Run] = '#' then begin
    inc(Run);
    fTokenID := tkComment;
    while FLine[Run] <> #0 do
    begin
      case FLine[Run] of
        #10, #13: break;
      end;
      inc(Run);
    end;
  end else begin
    inc(Run);
    fTokenID := tkSymbol;
  end;
end;

procedure TSynSmaliSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynSmaliSyn.StringProc;
var
  QuoteChar: Char;
begin

  fTokenID := tkString;
  QuoteChar := FLine[Run];
  if (FLine[Run + 1] = QuoteChar) and (FLine[Run + 2] = QuoteChar)
    then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = QuoteChar;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynSmaliSyn.UnknownProc;
begin
  inc(Run);
  while (fLine[Run] in [#128..#191]) OR
   ((fLine[Run]<>#0) and (fProcTable[fLine[Run]] = @UnknownProc)) do inc(Run);
  fTokenID := tkUnKnown;
end;

procedure TSynSmaliSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsAnsi: AnsiProc;
    rsPasStyle: PasStyleProc;
    rsCStyle: CStyleProc;
  else
    fProcTable[fLine[Run]];
  end;
end;

function TSynSmaliSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_NUMBER: Result := fNumberAttri;
    SYN_ATTR_VARIABLE: Result := fVarAttri;
  else
    Result := nil;
  end;
end;

function TSynSmaliSyn.GetEol: Boolean;
begin
  Result := False;
  if fTokenId = tkNull then Result := True;
end;

function TSynSmaliSyn.GetRange: Pointer;
begin
  Result := Pointer(PtrInt(fRange));
end;

function TSynSmaliSyn.GetToken: string;
var
  Len: LongInt;
begin
  Result := '';
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

procedure TSynSmaliSyn.GetTokenEx(out TokenStart: PChar;
  out TokenLength: integer);
begin
  TokenLength:=Run-fTokenPos;
  TokenStart:=FLine + fTokenPos;
end;

function TSynSmaliSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynSmaliSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkSecondKey: Result := fSecondKeyAttri;
    tkThirdKey: Result := FThirdKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkVariable: Result := fVarAttri;
    tkUnknown: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynSmaliSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynSmaliSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynSmaliSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynSmaliSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(PtrUInt(Value));
end;

procedure TSynSmaliSyn.SetSecondKeys(const Value: TStrings);
begin
  fSecondKeys.Assign(Value);
  DefHighLightChange(nil);
end;

function TSynSmaliSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', '0'..'9', 'a'..'z', 'A'..'Z'];
end;

resourcestring
  LangName = 'Smali';

class function TSynSmaliSyn.GetLanguageName: string;
begin
  Result := LangName;
end;

initialization
  MakeIdentTable;
  RegisterPlaceableHighlighter(TSynSmaliSyn);

end.

