%% -*- coding: utf-8 -*-
-module(lwm2m_PzBeWLCJj_epaqI6).

-vsn(<<"0.1.0">>).

-manufacturer(<<"china-unicom">>).
-protocol(<<"lwm2m">>).
-tenant_id(<<"PzBeWLCJj">>).
-product_id(<<"epaqI6">>).

-ifndef(TEST).
-on_load(on_load/0).
-endif.

-export([parse/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(KEY(MsgType, Key), utf8((MsgType) ++ ":" ++ (Key))).

utf8(Str) when is_list(Str) ->
  unicode:characters_to_binary(Str);
utf8(Str) -> Str.

-ifndef(TEST).
on_load() ->
  emqx_actorcloud_parser_cli:on_parser_loaded(?MODULE).
-endif.

-spec(parse(Topic::binary(), Data :: binary()) -> map()).
parse(_Topic, <<ProtoType:8, BusinessNO:16, Reserved:2, BusinessType:2, MsgType:12, Remain/binary>>) ->
  {Content, Remain2} = parse_content(Remain),
  <<CRC:16>> = Remain2,
  _Data = #{proto_type => ProtoType,
            business_no => BusinessNO,
            reserved => Reserved,
            business_type => BusinessType,
            msg_type => MsgType,
            content => Content,
            crc => CRC},
  jsx:encode(Content).

-spec(parse_content(Data :: binary()) -> {[map()], Remain :: binary()}).
parse_content(Data) ->
  parse_content(Data, []).

parse_content(<<T:16, L:16, Data/binary>>, ContentList) when L > 0,
                                                             Data =/= <<>> ->
  case parse_tlv(T, L, Data) of
    {ok, Content, Remain} ->
      parse_content(Remain, [Content| ContentList]);
    {error, Reason, Remain} ->
      error_logger:error_msg("~s, Parse TLV failed: ~p", [?MODULE, Reason]),
      parse_content(Remain, ContentList)
  end;
parse_content(Remain, ContentList) -> {ContentList, Remain}.

parse_tlv(16#D000 = T, Len, Data) ->
  <<Val:Len/binary, Remain/binary>> = Data,
  <<UTC:4/integer-unit:8,
    DataSrc:1/integer-unit:8,
    Enable:1/integer-unit:8,
    ActTh:2/integer-unit:8,
    ActTime:2/integer-unit:8,
    InaTh:2/integer-unit:8,
    InaTime:2/integer-unit:8,
    AlertPeriodMin:2/integer-unit:8,
    AlertNum:2/integer-unit:8,
    CalculateTimeMs:4/integer-unit:8>> = Val,
  MsgType = msg_type(T),
  {ok, #{ ?KEY(MsgType, "UTC时间") => UTC,
          ?KEY(MsgType, "数据原因") => DataSrc,
          ?KEY(MsgType, "是否使能报警") => bool(Enable),
          ?KEY(MsgType, "震动阈值") => ActTh,
          ?KEY(MsgType, "震动时间") => ActTime,
          ?KEY(MsgType, "静止阈值") => InaTh,
          ?KEY(MsgType, "静止时间") => InaTime,
          ?KEY(MsgType, "报警周期") => AlertPeriodMin,
          ?KEY(MsgType, "累计报警次数") => AlertNum,
          ?KEY(MsgType, "累计解算时间") => CalculateTimeMs
        }, Remain};

parse_tlv(16#D001 = T, Len, Data) ->
  <<Val:Len/binary, Remain/binary>> = Data,
  <<ActTh:2/integer-unit:8,
    ActTime:2/integer-unit:8,
    InaTh:2/integer-unit:8,
    InaTime:2/integer-unit:8,
    WakeNum:2/integer-unit:8>> = Val,
  MsgType = msg_type(T),
  {ok, #{ ?KEY(MsgType, "震动阈值") => ActTh,
          ?KEY(MsgType, "震动时间") => ActTime,
          ?KEY(MsgType, "静止阈值") => InaTh,
          ?KEY(MsgType, "静止时间") => InaTime,
          ?KEY(MsgType, "累计唤醒次数") => WakeNum}, Remain};

parse_tlv(16#D002 = T, Len, Data) ->
  <<Val:Len/binary, Remain/binary>> = Data,
  <<UTC:4/integer-unit:8,
    DataSrc:1/integer-unit:8,
    Enable:1/integer-unit:8,
    AlertState:1/integer-unit:8,
    YawBase:2/integer-unit:8,
    RollBase:2/integer-unit:8,
    PitchBase:2/integer-unit:8,
    HAngleBase:2/integer-unit:8,
    YawCur:2/integer-unit:8,
    RollCur:2/integer-unit:8,
    PitchCur:2/integer-unit:8,
    HAngleCur:2/integer-unit:8,
    AlertAngle:2/integer-unit:8,
    AlertPeriodMin:2/integer-unit:8,
    AlertNum:2/integer-unit:8,
    CalculateTimeMs:4/integer-unit:8
  >> = Val,
  MsgType = msg_type(T),
  {ok, #{ ?KEY(MsgType, "UTC时间") => UTC,
          ?KEY(MsgType, "数据原因") => DataSrc,
          ?KEY(MsgType, "是否使能报警") => bool(Enable),
          ?KEY(MsgType, "是否告警") => AlertState,
          ?KEY(MsgType, "基准偏航角") => YawBase,
          ?KEY(MsgType, "基准横滚角") => RollBase,
          ?KEY(MsgType, "基准俯仰角") => PitchBase,
          ?KEY(MsgType, "基准水平倾角") => HAngleBase,
          ?KEY(MsgType, "当前偏航角") => YawCur,
          ?KEY(MsgType, "当前横滚角") => RollCur,
          ?KEY(MsgType, "当前俯仰角") => PitchCur,
          ?KEY(MsgType, "当前水平倾角") => HAngleCur,
          ?KEY(MsgType, "报警角度") => AlertAngle,
          ?KEY(MsgType, "报警周期") => AlertPeriodMin,
          ?KEY(MsgType, "累计报警次数") => AlertNum,
          ?KEY(MsgType, "累计解算时间") => CalculateTimeMs}, Remain};

parse_tlv(16#D003 = T, Len, Data) ->
  <<Val:Len/binary, Remain/binary>> = Data,
  <<UTC:4/integer-unit:8,
    DataSrc:1/integer-unit:8,
    Type:1/integer-unit:8,
    Capacity:4/integer-unit:8,
    VoltageMv:2/integer-unit:8,
    RemainPower:1/integer-unit:8,
    Temperature:2/integer-unit:8,
    AlertVoltageMv:2/integer-unit:8,
    AlertRemain:1/integer-unit:8,
    AlertPeriodMin:2/integer-unit:8,
    AlertNum:2/integer-unit:8,
    SampleTimeMs:4/integer-unit:8
  >> = Val,
  MsgType = msg_type(T),
  {ok, #{ ?KEY(MsgType, "UTC时间") => UTC,
          ?KEY(MsgType, "数据原因") => DataSrc,
          ?KEY(MsgType, "电池类型") => Type,
          ?KEY(MsgType, "电池容量") => Capacity,
          ?KEY(MsgType, "电池电压数据") => VoltageMv,
          ?KEY(MsgType, "剩余电量") => RemainPower,
          ?KEY(MsgType, "电池温度") => Temperature,
          ?KEY(MsgType, "报警电压") => AlertVoltageMv,
          ?KEY(MsgType, "报警电量") => AlertRemain,
          ?KEY(MsgType, "报警周期") => AlertPeriodMin,
          ?KEY(MsgType, "累计报警次数") => AlertNum,
          ?KEY(MsgType, "累计采样时间") => SampleTimeMs}, Remain};

parse_tlv(16#D004 = T, Len, Data) ->
  <<Val:Len/binary, Remain/binary>> = Data,
  <<DataSrc:1/integer-unit:8,
    State:1/integer-unit:8,
    ChangeTimeUTC:4/integer-unit:8,
    UndefenceTimeMin:2/integer-unit:8
  >> = Val,
  MsgType = msg_type(T),
  {ok, #{ ?KEY(MsgType, "数据原因") => DataSrc,
          ?KEY(MsgType, "当前状态") => State,
          ?KEY(MsgType, "状态改变时间") => ChangeTimeUTC,
          ?KEY(MsgType, "撤防超时时间") => UndefenceTimeMin}, Remain};

parse_tlv(16#D005 = T, Len, Data) ->
  <<Val:Len/binary, Remain/binary>> = Data,
  <<CellId:4/integer-unit:8,
    PCI:2/integer-unit:8,
    ECL:1/integer-unit:8,
    RSRQ:2/integer-unit:8,
    ASU:2/integer-unit:8,
    SNR:2/integer-unit:8,
    RSRP:2/integer-unit:8,
    Chan:2/integer-unit:8,
    APN:20/binary,
    IP1:8/integer,IP2:8/integer,IP3:8/integer,IP4:8/integer
  >> = Val,
  MsgType = msg_type(T),
  {ok, #{ ?KEY(MsgType, "基站ID") => CellId,
          ?KEY(MsgType, "小区号") => PCI,
          ?KEY(MsgType, "信号等级") => ECL,
          ?KEY(MsgType, "信号电平") => RSRQ,
          ?KEY(MsgType, "接收信号强度") => ASU,
          ?KEY(MsgType, "信噪比") => SNR,
          ?KEY(MsgType, "接收信号质量") => RSRP,
          ?KEY(MsgType, "信道") => Chan,
          ?KEY(MsgType, "APN") => APN,
          ?KEY(MsgType, "IP地址") => inet:ntoa({IP1,IP2,IP3,IP4}) %% convert list() to ipaddr
        }, Remain};

parse_tlv(16#D006 = T, Len, Data) ->
  <<Val:Len/binary, Remain/binary>> = Data,
  <<Flag:8/bits,
    UEName:16/binary,
    UEIMEI:16/binary,
    UEIMSI:16/binary,
    UEICCID:21/binary>> = Val,
  MsgType = msg_type(T),
  Results = filter_by_flags(Flag, [?KEY(MsgType, "通信设备名称"),
                                   ?KEY(MsgType, "电话卡号"),
                                   ?KEY(MsgType, "模组串号"),
                                   ?KEY(MsgType, "电话卡物理号")],
                                  [UEName, UEIMEI, UEIMSI, UEICCID], #{}),
  <<FlagInt:8/integer>> = Flag,
  {ok, Results#{?KEY(MsgType, "选项字节") => FlagInt}, Remain};

parse_tlv(16#D007 = T, Len, Data) ->
  <<Val:Len/binary, Remain/binary>> = Data,
  <<Flag:8/bits,
    String/binary>> = Val,
  MsgType = msg_type(T),
  Results = filter_by_flags(Flag, [?KEY(MsgType, "硬件版本号"),
                                   ?KEY(MsgType, "软件版本号"),
                                   ?KEY(MsgType, "设备序列号")],
                                   null_terminated_strings(String), #{}),
  <<FlagInt:8/integer>> = Flag,
  {ok, Results#{?KEY(MsgType, "选项字节") => FlagInt}, Remain};

parse_tlv(16#D008 = T, Len, Data) ->
  <<Val:Len/binary, Remain/binary>> = Data,
  <<Flag:8/bits,
    RstNum:2/integer-unit:8,
    NetPwrTimeMs:4/integer-unit:8>> = Val,
  MsgType = msg_type(T),
  Results = filter_by_flags(Flag, [?KEY(MsgType, "复位次数"),
                                   ?KEY(MsgType, "网络模块累计通信时间")],
                                  [RstNum, NetPwrTimeMs], #{}),
  <<FlagInt:8/integer>> = Flag,
  {ok, Results#{?KEY(MsgType, "选项字节") => FlagInt}, Remain};

parse_tlv(16#D009 = T, Len, Data) ->
  <<Val:Len/binary, Remain/binary>> = Data,
  <<UTC:4/integer-unit:8,
    Log/binary
  >> = Val,
  MsgType = msg_type(T),
  {ok, #{ ?KEY(MsgType, "UTC时间") => UTC,
          ?KEY(MsgType, "log信息") => hd(null_terminated_strings(Log))}, Remain};

parse_tlv(16#D00A = T, Len, Data) ->
  <<ENVs:Len/binary, Remain/binary>> = Data,
  MsgType = msg_type(T),
  {ok, #{ ?KEY(MsgType, "ENV数据") => hd(null_terminated_strings(ENVs))}, Remain};

parse_tlv(16#D00B = T, Len, Data) ->
  <<OnlineStatus:Len/binary, Remain/binary>> = Data,
  MsgType = msg_type(T),
  {ok, #{ ?KEY(MsgType, "在线状态") => bool(OnlineStatus)}, Remain};

parse_tlv(Type, Len, Data) ->
  <<_:Len/binary, Remain/binary>> = Data,
  {error, {unknown_type, Type}, Remain}.

bool(0) -> false;
bool(_) -> true.

null_terminated_strings(String) ->
  [Str || Str <- binary:split(String, <<0>>, [global]), Str =/= <<>>].

filter_by_flags(Flags, Ks, Vs, ResultMap)
    when Flags =:= <<>>;
         Ks =:= [];
         Vs =:= [] ->
  ResultMap;
filter_by_flags(<<0:1, RFlags/bits>>, [_K|Ks], [_V|Vs], ResultMap) ->
  filter_by_flags(RFlags, Ks, Vs, ResultMap);
filter_by_flags(<<1:1, RFlags/bits>>, [K|Ks], [V|Vs], ResultMap) ->
  filter_by_flags(RFlags, Ks, Vs, ResultMap#{K => V}).

msg_type(16#D000) -> "震动数据";
msg_type(16#D001) -> "震动唤醒数据";
msg_type(16#D002) -> "倾角数据";
msg_type(16#D003) -> "电池数据";
msg_type(16#D004) -> "防御数据";
msg_type(16#D005) -> "基站数据";
msg_type(16#D006) -> "物联网模组数据";
msg_type(16#D007) -> "设备数据";
msg_type(16#D008) -> "运行信息";
msg_type(16#D009) -> "日志信息";
msg_type(16#D00A) -> "ENV配置信息";
msg_type(16#D00B) -> "上线信息";
msg_type(_) -> "Unknow".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(EUNIT).
filter_by_flags_test_() ->
  [
    ?_assertEqual(#{a=>v1, c=>v3},
                  filter_by_flags(<<1:1, 0:1, 1:1, 0:1>>,
                                  [a, b, c, d],
                                  [v1, v2, v3, v4], #{})),
    ?_assertEqual(#{},
                  filter_by_flags(<<0:1, 0:1, 0:1, 0:1>>,
                                  [a, b, c, d],
                                  [v1, v2, v3, v4], #{})),
    ?_assertEqual(#{a=>v1, b=>v2, c=>v3, d=>v4},
                  filter_by_flags(<<1:1, 1:1, 1:1, 1:1>>,
                                  [a, b, c, d],
                                  [v1, v2, v3, v4], #{}))
  ].

null_terminated_strings_test_() ->
  [
    ?_assertEqual([], null_terminated_strings(<<0, 0>>)),
    ?_assertEqual([], null_terminated_strings(<<0>>)),
    ?_assertEqual([<<"aaa">>], null_terminated_strings(<<"aaa">>)),
    ?_assertEqual([<<"aaa">>], null_terminated_strings(<<"aaa",0>>)),
    ?_assertEqual([<<"aaa">>, <<"bbb">>], null_terminated_strings(<<"aaa", 0, "bbb", 0>>)),
    ?_assertEqual([<<"aaa">>, <<"bbb">>, <<"ccc">>], null_terminated_strings(<<"aaa", 0, "bbb", 0, "ccc">>)),
    ?_assertEqual([<<"aaa">>, <<"bbb">>], null_terminated_strings(<<0, "aaa", 0, "bbb">>))
  ].

parse_tlv_d00_test_() ->
  UTC = 1544715898,
  DataSrc = 1,
  Enable = 1,
  ActTh = 5443,
  ActTime = 354,
  InaTh = 2331,
  InaTime = 11,
  AlertPeriodMin = 20,
  AlertNum = 220,
  CalculateTimeMs = 22312,
  [
    ?_assertEqual({ok, #{ utf8("震动数据:UTC时间") => UTC,
                          utf8("震动数据:数据原因") => DataSrc,
                          utf8("震动数据:是否使能报警") => bool(Enable),
                          utf8("震动数据:震动阈值") => ActTh,
                          utf8("震动数据:震动时间") => ActTime,
                          utf8("震动数据:静止阈值") => InaTh,
                          utf8("震动数据:静止时间") => InaTime,
                          utf8("震动数据:报警周期") => AlertPeriodMin,
                          utf8("震动数据:累计报警次数") => AlertNum,
                          utf8("震动数据:累计解算时间") => CalculateTimeMs
                        }, <<>>},
                  parse_tlv(16#D000, 22, <<UTC:4/integer-unit:8,
                                          DataSrc:1/integer-unit:8,
                                          Enable:1/integer-unit:8,
                                          ActTh:2/integer-unit:8,
                                          ActTime:2/integer-unit:8,
                                          InaTh:2/integer-unit:8,
                                          InaTime:2/integer-unit:8,
                                          AlertPeriodMin:2/integer-unit:8,
                                          AlertNum:2/integer-unit:8,
                                          CalculateTimeMs:4/integer-unit:8>>)),
    ?_assertEqual({ok, #{ utf8("震动数据:UTC时间") => UTC,
                          utf8("震动数据:数据原因") => DataSrc,
                          utf8("震动数据:是否使能报警") => bool(Enable),
                          utf8("震动数据:震动阈值") => ActTh,
                          utf8("震动数据:震动时间") => ActTime,
                          utf8("震动数据:静止阈值") => InaTh,
                          utf8("震动数据:静止时间") => InaTime,
                          utf8("震动数据:报警周期") => AlertPeriodMin,
                          utf8("震动数据:累计报警次数") => AlertNum,
                          utf8("震动数据:累计解算时间") => CalculateTimeMs
                        }, <<"extra">>},
                  parse_tlv(16#D000, 22, <<UTC:4/integer-unit:8,
                                          DataSrc:1/integer-unit:8,
                                          Enable:1/integer-unit:8,
                                          ActTh:2/integer-unit:8,
                                          ActTime:2/integer-unit:8,
                                          InaTh:2/integer-unit:8,
                                          InaTime:2/integer-unit:8,
                                          AlertPeriodMin:2/integer-unit:8,
                                          AlertNum:2/integer-unit:8,
                                          CalculateTimeMs:4/integer-unit:8, "extra">>))
  ].

parse_tlv_d06_test_() ->
  Flag1 = <<1:1, 1:1, 1:1, 1:1, 0:4>>,
  Flag2 = <<1:1, 0:1, 1:1, 0:1, 0:4>>,
  UEName = <<"Shawn'sCellPhone">>,
  UEIMEI = <<"1234567890123456">>,
  UEIMSI = <<"1234567890123456">>,
  UEICCID = <<"123456789012345678901">>,
[
  ?_assertEqual({ok, #{ utf8("物联网模组数据:选项字节") => 240,
                        utf8("物联网模组数据:通信设备名称") => UEName,
                        utf8("物联网模组数据:电话卡号") => UEIMEI,
                        utf8("物联网模组数据:模组串号") => UEIMSI,
                        utf8("物联网模组数据:电话卡物理号") => UEICCID}, <<>>},
                  parse_tlv(16#D006, 70,
                            <<Flag1:8/bits,
                              UEName:16/binary,
                              UEIMEI:16/binary,
                              UEIMSI:16/binary,
                              UEICCID:21/binary>>)),
  ?_assertEqual({ok, #{ utf8("物联网模组数据:选项字节") => 160,
                        utf8("物联网模组数据:通信设备名称") => UEName,
                        utf8("物联网模组数据:模组串号") => UEIMSI}, <<>>},
                  parse_tlv(16#D006, 70,
                            <<Flag2:8/bits,
                              UEName:16/binary,
                              UEIMEI:16/binary,
                              UEIMSI:16/binary,
                              UEICCID:21/binary>>))
].

parse_test_() ->
  UTC = 1544715898,
  DataSrc = 1,
  Enable = 1,
  ActTh = 5443,
  ActTime = 354,
  InaTh = 2331,
  InaTime = 11,
  AlertPeriodMin = 20,
  AlertNum = 220,
  CalculateTimeMs = 22312,
  Payload = <<16#D000:2/integer-unit:8, 22:2/integer-unit:8,
              UTC:4/integer-unit:8,
              DataSrc:1/integer-unit:8,
              Enable:1/integer-unit:8,
              ActTh:2/integer-unit:8,
              ActTime:2/integer-unit:8,
              InaTh:2/integer-unit:8,
              InaTime:2/integer-unit:8,
              AlertPeriodMin:2/integer-unit:8,
              AlertNum:2/integer-unit:8,
              CalculateTimeMs:4/integer-unit:8>>,
  Data = <<1:8, 155:16, 0:2, 2:2, 16#D000:12, Payload/binary, 3415:16>>,
  [?_assertEqual(jsx:encode([#{
                              utf8("震动数据:UTC时间") => UTC,
                              utf8("震动数据:数据原因") => DataSrc,
                              utf8("震动数据:是否使能报警") => bool(Enable),
                              utf8("震动数据:震动阈值") => ActTh,
                              utf8("震动数据:震动时间") => ActTime,
                              utf8("震动数据:静止阈值") => InaTh,
                              utf8("震动数据:静止时间") => InaTime,
                              utf8("震动数据:报警周期") => AlertPeriodMin,
                              utf8("震动数据:累计报警次数") => AlertNum,
                              utf8("震动数据:累计解算时间") => CalculateTimeMs
                            }]), parse(<<"ha">>, Data))].

-endif.
