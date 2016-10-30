-module(esmpp_lib_latin1_to_gsm).

-include("esmpp_lib.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([latin1_to_gsm/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec latin1_to_gsm(binary(), binary()) ->
    binary().
latin1_to_gsm(<<>>, Bin2) ->
    revert_bin_data(Bin2, <<>>);
latin1_to_gsm(Bin1, Bin2) ->
    <<OldSymb:1/binary, Tail/binary>> = Bin1,
    <<LatinCode>> = OldSymb,
    NewCode = get_gsm_code(LatinCode),
    case NewCode > 255 of
        false ->
            latin1_to_gsm(Tail, <<NewCode:8/integer, Bin2/binary>>);
        true -> 
            RevertCode = revert_bin_data(<<NewCode:16/integer>>, <<>>),
            latin1_to_gsm(Tail, <<RevertCode:2/binary, Bin2/binary>>)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec get_gsm_code(number()) ->
    number().
get_gsm_code(LatinCode) ->
    case LatinCode of 
        64  -> 0;       % symbol @
        163 -> 1;       % symbol £
        36  -> 2;       % symbol $
        165 -> 3;       % symbol ¥  
        232 -> 4;       % symbol è
        233 -> 5;       % symbol é
        249 -> 6;       % symbol ù
        236 -> 7;       % symbol ì
        242 -> 8;       % symbol ò
        199 -> 9;       % symbol Ç
        216 -> 11;      % symbol Ø
        248 -> 12;      % symbol ø
        197 -> 14;      % symbol Å
        229 -> 15;      % symbol å
        95  -> 17;      % symbol _
        18  -> 6922;    %        FORM FEED
        94  -> 6932;    % symbol ^
        123 -> 6952;    % symbol {
        125 -> 6953;    % symbol }
        92  -> 6959;    % symbol \
        91  -> 6972;    % symbol [
        126 -> 6973;    % symbol ~
        93  -> 6974;    % symbol ]
        124 -> 6976;    % symbol |
        164 -> 7013;    % symbol €
        198 -> 28;      % symbol Æ
        230 -> 29;      % symbol æ
        223 -> 30;      % symbol ß
        201 -> 31;      % symbol É
        161 -> 64;      % symbol ¡
        196 -> 91;      % symbol Ä
        214 -> 92;      % symbol Ö
        209 -> 93;      % symbol Ñ
        220 -> 94;      % symbol Ü
        167 -> 95;      % symbol §
        191 -> 96;      % symbol ¿ 
        228 -> 123;     % symbol ä
        246 -> 124;     % symbol ö
        241 -> 125;     % symbol ñ
        252 -> 126;     % symbol ü
        224 -> 127;     % symbol à
        Any -> Any
    end. 

-spec revert_bin_data(binary(), binary()) ->
    binary().
revert_bin_data(<<>>, Bin) ->
    Bin;
revert_bin_data(<<H:1/binary, Tail/binary>>, Bin) ->
    revert_bin_data(Tail, <<H:1/binary, Bin/binary>>).
