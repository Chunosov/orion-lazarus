unit ObjectFilter;

interface

type
  TFilterOp = (fopAnd, fopOr, fopAndNot, fopOrNot);

  TFilterCondition = class
  private
    Operation: TFilterOp;
  public
    function Check(Obj: TObject): Boolean; virtual; abstract;
  end;

  TFilterConditions = array of TFilterCondition;

  TObjectFilter = class(TFilterCondition)
  private
    FConditions: TFilterConditions;
    FVolatile: Boolean;
  public
    constructor Create; overload;
    constructor Create(Volatile: Boolean); overload;
    constructor Create(const Conditions: array of const; Volatile: Boolean = False); overload;
    destructor Destroy; override;
    procedure Add(AOperation: TFilterOp; Condition: TFilterCondition);
    function Append(AOperation: TFilterOp; Condition: TFilterCondition): TObjectFilter;
    procedure Insert(Index: Integer; AOperation: TFilterOp; Condition: TFilterCondition);
    function Check(Obj: TObject): Boolean; override;

    // Свойство может указывать тем процедурам, в которые передается фильтр,
    // что "снаружи" этот фильтр не используется и может быть уничтожен.
    property Volatile: Boolean read FVolatile write FVolatile;
  end;

  TFilterByClass = class(TFilterCondition)
  private
    FClass: TClass;
  public
    constructor Create(AClass: TClass);
    function Check(Obj: TObject): Boolean; override;
  end;

  TFilterByInterface = class(TFilterCondition)
  private
    FIntf: TGUID;
  public
    constructor Create(const IID: TGUID);
    function Check(Obj: TObject): Boolean; override;
  end;

{
  Функции для "быстрого" создания фильтров.
  Позволяют записать фильтр в более коротком и "человекопонятном" виде.

  Например вызов некоторой процедуры, фильтрующей список:

  SomeFilteringProc(ListBeingFiltered,
      TObjectFilter.Create(True)
        .Append(fopAnd, TFilterByClass.Create(TSomeClass))
        .Append(fopAnd, TFilterByInterface.Create(ISomeInterface)));

  можно записать как:

  SomeFilteringProc(ListBeingFiltered,
    FilterBy([ClassIs(TSomeClass), 'and', HasInterface(ISomeInterface)]));

  что читается гораздо проще, чем первый вызов.
}
function FilterBy(const Conditions: array of const): TObjectFilter;
function ClassIs(AClass: TClass): TFilterCondition; inline;
function HasInterface(const IID: TGUID): TFilterCondition; inline;

implementation

{$region 'TObjectFilter'}
constructor TObjectFilter.Create;
begin
end;

constructor TObjectFilter.Create(Volatile: Boolean);
begin
  FVolatile := Volatile;
end;

constructor TObjectFilter.Create(const Conditions: array of const; Volatile: Boolean);

  function FilterOpFromStr(const Op: String): TFilterOp;
  begin
    if Op = 'or' then Result := fopAnd
    else if Op = 'and not' then Result := fopAndNot
    else if Op = 'or not' then Result := fopOrNot
    else Result := fopAnd;
  end;

var
  I: Integer;
  Oper: TFilterOp;
begin
  Oper := fopAnd;
  for I := 0 to Length(Conditions)-1 do
    if Conditions[I].VType = vtInteger then
      Oper := TFilterOp(Conditions[I].VInteger)
    else if Conditions[I].VType = vtString then
      Oper := FilterOpFromStr(String(Conditions[I].VString))
    else if Conditions[I].VType = vtObject then
      if Conditions[I].VObject is TFilterCondition then
        Add(Oper, TFilterCondition(Conditions[I].VObject));
  FVolatile := Volatile;
end;

destructor TObjectFilter.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FConditions)-1 do FConditions[I].Free;
  inherited;
end;

// Добавляет новое условие фильтрации в конец списка.
procedure TObjectFilter.Add(AOperation: TFilterOp; Condition: TFilterCondition);
var
  L: Integer;
begin
  Condition.Operation := AOperation;

  L := Length(FConditions);
  SetLength(FConditions, L+1);
  FConditions[L] := Condition;
end;

// Добавляет новое условие фильтрации. Тоже самое, что и Add, но возвращает результат.
// Синтаксический сахар, для использования в разных "пижонских" конструкциях типа:
//   SomeProcedure(TObjectFilter.Create
//     .Append(fopAnd, TFilterByClass.Create(TSomeClass))
//     .Append(fopAnd, TFilterBySmth.Create(Something))
//     .Append(fopAnd, TFilterBySmthElse.Create(SomethingElse))
//   )
function TObjectFilter.Append(AOperation: TFilterOp; Condition: TFilterCondition): TObjectFilter;
begin
  Add(AOperation, Condition);
  Result := Self;
end;

// Вставляет новое условие фильтрации в заданноую позицию списка.
procedure TObjectFilter.Insert(Index: Integer; AOperation: TFilterOp; Condition: TFilterCondition);
var
  I: Integer;
begin
  if Index >= Length(FConditions) then
  begin
    Add(AOperation, Condition);
    Exit;
  end;
  SetLength(FConditions, Length(FConditions)+1);
  for I := Index to Length(FConditions)-2 do
    FConditions[I+1] := FConditions[I];
  FConditions[Index] := Condition;

  Condition.Operation := AOperation;
end;

// Проверяет, удовлетворяет ли объект всем заданным условиям.
function TObjectFilter.Check(Obj: TObject): Boolean;
var
  I: Integer;
  F: Boolean;
begin
  if Length(FConditions) > 0 then
  begin
    Result := FConditions[0].Check(Obj);
    for I := 1 to Length(FConditions) - 1 do
    begin
      F := FConditions[I].Check(Obj);
      case FConditions[I].Operation of
        fopAnd: Result := Result and F;
        fopOr: Result := Result or F;
        fopAndNot: Result := Result and not F;
        fopOrNot: Result := Result or not F;
      end;
    end;
  end
  else Result := True;
end;
{$endregion}

{$region 'Quick-filter functions'}
function FilterBy(const Conditions: array of const): TObjectFilter;
begin
  Result := TObjectFilter.Create(Conditions, True);
end;

function ClassIs(AClass: TClass): TFilterCondition; inline;
begin
  Result := TFilterByClass.Create(AClass);
end;

function HasInterface(const IID: TGUID): TFilterCondition; inline;
begin
  Result := TFilterByInterface.Create(IID);
end;
{$endregion}

{$region 'TFilterByClass'}
constructor TFilterByClass.Create(AClass: TClass);
begin
  FClass := AClass;
end;

function TFilterByClass.Check(Obj: TObject): Boolean;
begin
  Result := Obj is FClass;
end;
{$endregion}

{$region 'TFilterByInterface'}
constructor TFilterByInterface.Create(const IID: TGUID);
begin
  FIntf := IID;
end;

function TFilterByInterface.Check(Obj: TObject): Boolean;
var
  Intf: IInterface;
begin
  Result := Obj.GetInterface(FIntf, Intf);
end;
{$endregion}

end.
