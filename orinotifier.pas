{***************************************************************************
 *
 * Orion-project.org Lazarus Helper Library
 * Copyright (C) 2016-2017 by Nikolay Chunosov
 * 
 * This file is part of the Orion-project.org Lazarus Helper Library
 * https://github.com/Chunosov/orion-lazarus
 *
 * This Library is free software: you can redistribute it and/or modify it 
 * under the terms of the MIT License. See enclosed LICENSE.txt for details.
 *
 ***************************************************************************}

unit OriNotifier;

interface

uses
  Classes, Contnrs;

type
  TPublisher = class
  strict private
    FSubscribers: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    class procedure Unsubscribe(AEvent: TNotifyEvent);
    class procedure Subscribe(const ATopic: String; AEvent: TNotifyEvent);
    class procedure Publish(Sender: TObject; const ATopic: String);
  end;

implementation

uses
  SysUtils;

var
  Publisher: TPublisher;

function HashOf(const Key: string): Cardinal; inline;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(Key) do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result)*8 - 2))) xor Ord(Key[I]);
end;

{$region 'TSubscriber'}
type
  TSubscriber = class
  strict private
  public
    Topic: Cardinal;
    Event: TNotifyEvent;
    constructor Create(ATopic: Cardinal; AEvent: TNotifyEvent);
  end;

constructor TSubscriber.Create(ATopic: Cardinal; AEvent: TNotifyEvent);
begin
  Topic := ATopic;
  Event := AEvent;
end;
{$endregion}

{$region 'TPublisher'}
constructor TPublisher.Create;
begin
  FSubscribers := TObjectList.Create(True);
end;

destructor TPublisher.Destroy;
begin
  FSubscribers.Free;
end;

class procedure TPublisher.Subscribe(const ATopic: String; AEvent: TNotifyEvent);
var
	I: Integer;
  T: Cardinal;
begin
  if not Assigned(Publisher) then
    Publisher := TPublisher.Create;
  T := HashOf(ATopic);
  for I := 0 to Publisher.FSubscribers.Count-1 do
    with TSubscriber(Publisher.FSubscribers[I]) do
      if (Topic = T) and (Addr(AEvent) = Addr(Event)) then
        Exit;
  Publisher.FSubscribers.Add(TSubscriber.Create(T, AEvent));
end;

class procedure TPublisher.Publish(Sender: TObject; const ATopic: String);
var
	I: Integer;
  T: Cardinal;
begin
  if Assigned(Publisher) then
  begin
  	T := HashOf(ATopic);
    for I := 0 to Publisher.FSubscribers.Count-1 do
      with TSubscriber(Publisher.FSubscribers[I]) do
        if (Topic = T) and Assigned(Event) then
          Event(Sender);
  end;
end;

class procedure TPublisher.Unsubscribe(AEvent: TNotifyEvent);
var
	I: Integer;
begin
  for I := Publisher.FSubscribers.Count-1 downto 0 do
    if Addr(TSubscriber(Publisher.FSubscribers[I]).Event) = Addr(AEvent) then
    begin
      Publisher.FSubscribers[I].Free;
      Publisher.FSubscribers.Delete(I);
    end;
end;
{$endregion}

initialization

finalization
  if Assigned(Publisher) then Publisher.Free;

end.
