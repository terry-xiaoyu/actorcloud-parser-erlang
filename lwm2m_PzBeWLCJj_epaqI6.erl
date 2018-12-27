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

-export([parse/2, unparse/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(CRC16Def,[16#0000, 16#C0C1, 16#C181, 16#0140, 16#C301,
                  16#03C0, 16#0280, 16#C241, 16#C601, 16#06C0,
                  16#0780, 16#C741, 16#0500, 16#C5C1, 16#C481,
                  16#0440, 16#CC01, 16#0CC0, 16#0D80, 16#CD41,
                  16#0F00, 16#CFC1, 16#CE81, 16#0E40, 16#0A00,
                  16#CAC1, 16#CB81, 16#0B40, 16#C901, 16#09C0,
                  16#0880, 16#C841, 16#D801, 16#18C0, 16#1980,
                  16#D941, 16#1B00, 16#DBC1, 16#DA81, 16#1A40,
                  16#1E00, 16#DEC1, 16#DF81, 16#1F40, 16#DD01,
                  16#1DC0, 16#1C80, 16#DC41, 16#1400, 16#D4C1,
                  16#D581, 16#1540, 16#D701, 16#17C0, 16#1680,
                  16#D641, 16#D201, 16#12C0, 16#1380, 16#D341,
                  16#1100, 16#D1C1, 16#D081, 16#1040, 16#F001,
                  16#30C0, 16#3180, 16#F141, 16#3300, 16#F3C1,
                  16#F281, 16#3240, 16#3600, 16#F6C1, 16#F781,
                  16#3740, 16#F501, 16#35C0, 16#3480, 16#F441,
                  16#3C00, 16#FCC1, 16#FD81, 16#3D40, 16#FF01,
                  16#3FC0, 16#3E80, 16#FE41, 16#FA01, 16#3AC0,
                  16#3B80, 16#FB41, 16#3900, 16#F9C1, 16#F881,
                  16#3840, 16#2800, 16#E8C1, 16#E981, 16#2940,
                  16#EB01, 16#2BC0, 16#2A80, 16#EA41, 16#EE01,
                  16#2EC0, 16#2F80, 16#EF41, 16#2D00, 16#EDC1,
                  16#EC81, 16#2C40, 16#E401, 16#24C0, 16#2580,
                  16#E541, 16#2700, 16#E7C1, 16#E681, 16#2640,
                  16#2200, 16#E2C1, 16#E381, 16#2340, 16#E101,
                  16#21C0, 16#2080, 16#E041, 16#A001, 16#60C0,
                  16#6180, 16#A141, 16#6300, 16#A3C1, 16#A281,
                  16#6240, 16#6600, 16#A6C1, 16#A781, 16#6740,
                  16#A501, 16#65C0, 16#6480, 16#A441, 16#6C00,
                  16#ACC1, 16#AD81, 16#6D40, 16#AF01, 16#6FC0,
                  16#6E80, 16#AE41, 16#AA01, 16#6AC0, 16#6B80,
                  16#AB41, 16#6900, 16#A9C1, 16#A881, 16#6840,
                  16#7800, 16#B8C1, 16#B981, 16#7940, 16#BB01,
                  16#7BC0, 16#7A80, 16#BA41, 16#BE01, 16#7EC0,
                  16#7F80, 16#BF41, 16#7D00, 16#BDC1, 16#BC81,
                  16#7C40, 16#B401, 16#74C0, 16#7580, 16#B541,
                  16#7700, 16#B7C1, 16#B681, 16#7640, 16#7200,
                  16#B2C1, 16#B381, 16#7340, 16#B101, 16#71C0,
                  16#7080, 16#B041, 16#5000, 16#90C1, 16#9181,
                  16#5140, 16#9301, 16#53C0, 16#5280, 16#9241,
                  16#9601, 16#56C0, 16#5780, 16#9741, 16#5500,
                  16#95C1, 16#9481, 16#5440, 16#9C01, 16#5CC0,
                  16#5D80, 16#9D41, 16#5F00, 16#9FC1, 16#9E81,
                  16#5E40, 16#5A00, 16#9AC1, 16#9B81, 16#5B40,
                  16#9901, 16#59C0, 16#5880, 16#9841, 16#8801,
                  16#48C0, 16#4980, 16#8941, 16#4B00, 16#8BC1,
                  16#8A81, 16#4A40, 16#4E00, 16#8EC1, 16#8F81,
                  16#4F40, 16#8D01, 16#4DC0, 16#4C80, 16#8C41,
                  16#4400, 16#84C1, 16#8581, 16#4540, 16#8701,
                  16#47C0, 16#4680, 16#8641, 16#8201, 16#42C0,
                  16#4380, 16#8341, 16#4100, 16#81C1, 16#8081, 16#4040]).

-define(KEY(MsgType, Key), utf8((MsgType) ++ ":" ++ (Key))).

utf8(Str) when is_list(Str) ->
  unicode:characters_to_binary(Str);
utf8(Str) -> Str.

-ifndef(TEST).
on_load() ->
  emqx_actorcloud_parser_cli:on_parser_loaded(?MODULE).
-endif.

-spec(parse(Object::binary(), Bin::binary()) -> map() | string()).
parse(_Object, <<ProtoType:8, BusinessNO:16, Reserved:2, BusinessType:2, FuncCode:12, Remain/binary>>) ->
  {Content, CRC} = parse_content(FuncCode, Remain),
  _Data = #{proto_type => ProtoType,
            business_no => BusinessNO,
            reserved => Reserved,
            business_type => BusinessType,
            func_code => FuncCode,
            content => Content,
            crc => CRC},
  Content.

-spec(unparse(Object::binary(), Json::map()) -> binary()).
unparse(_Object, Json) ->
  #{<<"version">> := Version,
    <<"req_no">> := ReqNo,
    <<"msg_type">> := MsgType,
    <<"func_no">> := FuncCode
    } = Json,
  Data = <<Version:8, ReqNo:16, 0:2, MsgType:2, FuncCode:12>>,
  CRC16 = crc16(Data),
  <<Data/binary, CRC16:16>>.

-spec(parse_content(FunCode::integer(), Data :: binary()) -> {string(), CRC16 :: integer()}).
parse_content(FunCode, Data) ->
  ContentLen = byte_size(Data) - 2,
  <<Content:ContentLen/binary, CRC16:16/integer>> = Data,
  {do_parse_content(FunCode, Content), CRC16}.

do_parse_content(16#D00, Data) ->
  parse_tlv_content(Data, []);
do_parse_content(16#D01, Json) ->
  Json;
do_parse_content(16#D02, Text) ->
  Text;
do_parse_content(16#C0E, LockResult) ->
  #{ok => bool(LockResult)};
do_parse_content(16#C0F, UnLockResult) ->
  #{ok => bool(UnLockResult)};
do_parse_content(_, Data) ->
  Data.

parse_tlv_content(<<T:16, L:16, Data/binary>>, ContentList) when L > 0,
                                                             Data =/= <<>> ->
  case parse_tlv(T, L, Data) of
    {ok, Content, Remain} ->
      parse_tlv_content(Remain, [Content| ContentList]);
    {error, Reason, Remain} ->
      error_logger:error_msg("~s, Parse TLV failed: ~p", [?MODULE, Reason]),
      parse_tlv_content(Remain, ContentList)
  end;
parse_tlv_content(<<>>, ContentList) -> ContentList.

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
    String/binary>> = Val,
  MsgType = msg_type(T),
  Results = filter_by_flags(Flag, [?KEY(MsgType, "通信设备名称"),
                                   ?KEY(MsgType, "电话卡号"),
                                   ?KEY(MsgType, "模组串号"),
                                   ?KEY(MsgType, "电话卡物理号")],
                                   null_terminated_strings(String), #{}),
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

crc16(Data) ->
   crc16(Data, 16#FFFF).

crc16(<<>>, CRC) ->
   CRC;
crc16(<<Value:8, Rest/binary>>, CRC) when Value =< 255->
   Index = (CRC bxor Value) band 255,
   NewCRC = (CRC bsr 8) bxor crc_index(Index),
   crc16(Rest,NewCRC).

crc_index(N) ->
   lists:nth(N+1, ?CRC16Def).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TEST
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(EUNIT).
crc16_test() ->
	?assertEqual(50380, crc16(<<1,0,1,12,14>>)).

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
  UEName = <<"Shawn'sCellPhone", 0>>,
  UEIMEI = <<"1234567890123456", 0>>,
  UEIMSI = <<"1234567890123456", 0>>,
  UEICCID = <<"123456789012345678901">>,
  Len = 1 + byte_size(UEName) + byte_size(UEIMEI) + byte_size(UEIMSI) + byte_size(UEICCID),
[
  ?_assertEqual({ok, #{ utf8("物联网模组数据:选项字节") => 240,
                        utf8("物联网模组数据:通信设备名称") => <<"Shawn'sCellPhone">>,
                        utf8("物联网模组数据:电话卡号") => <<"1234567890123456">>,
                        utf8("物联网模组数据:模组串号") => <<"1234567890123456">>,
                        utf8("物联网模组数据:电话卡物理号") => <<"123456789012345678901">>}, <<>>},
                  parse_tlv(16#D006, Len,
                            <<Flag1:8/bits,
                              UEName/binary,
                              UEIMEI/binary,
                              UEIMSI/binary,
                              UEICCID/binary>>)),
  ?_assertEqual({ok, #{ utf8("物联网模组数据:选项字节") => 160,
                        utf8("物联网模组数据:通信设备名称") => <<"Shawn'sCellPhone">>,
                        utf8("物联网模组数据:模组串号") => <<"1234567890123456">>}, <<>>},
                  parse_tlv(16#D006, Len,
                            <<Flag2:8/bits,
                              UEName/binary,
                              UEIMEI/binary,
                              UEIMSI/binary,
                              UEICCID/binary>>))
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
  Data = <<1:8, 155:16, 0:2, 2:2, 16#D00:12, Payload/binary, 3415:16>>,
  [?_assertEqual([#{  utf8("震动数据:UTC时间") => UTC,
                      utf8("震动数据:数据原因") => DataSrc,
                      utf8("震动数据:是否使能报警") => bool(Enable),
                      utf8("震动数据:震动阈值") => ActTh,
                      utf8("震动数据:震动时间") => ActTime,
                      utf8("震动数据:静止阈值") => InaTh,
                      utf8("震动数据:静止时间") => InaTime,
                      utf8("震动数据:报警周期") => AlertPeriodMin,
                      utf8("震动数据:累计报警次数") => AlertNum,
                      utf8("震动数据:累计解算时间") => CalculateTimeMs
                    }],
                    parse(<<"ha">>, Data))].

unparse_test() ->
  CmdLock = #{<<"version">> => 1,
              <<"req_no">> => 21,
              <<"msg_type">> => 0,
              <<"func_no">> => 16#c0e}, 
  [
    ?_assertEqual(<<1,0,1,12,14,50380:16>>, unparse(<<"ha">>, CmdLock))
  ].

-endif.
