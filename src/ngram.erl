%% N-Gram language detection implementation
%%
%% Based on the ruby version found at
%% http://github.com/feedbackmine/language_detector Thanks to yong!
%% 
%% See the wikipedia article for more information about this method.
%% http://en.wikipedia.org/wiki/Ngram
%%
%% To run tests use eunit  eunit:test('ngram')
-module(ngram).
-author("Nathan Zorn <nathan@collecta.com>").

%% use this include after generating the hrl 
%% -include("ngram.hrl").
-define(PROFILE, []).  %% defined by ngram.hrl when generated

-export([init/0,
         ngrams/1,
         detect/1,
         compute_distance/3, 
	 profile_to_hrl/1,
         train/1]).

-include_lib("eunit/include/eunit.hrl").
-define(punctuation,".,\n\t\r!?\"()+_[]{}#$%&'*/0123456789:;<=>@\\^_`|~").
-define(LIMIT, 2000).
-define(CHAR_LIMIT, 500).
-define(ETS_TABLE, ngrametstable).

%% Used to create database for language profiles.
-spec init() ->
    atom().
init() ->
    case ets:info(?ETS_TABLE) of
        undefined ->
            ets:new(?ETS_TABLE, [named_table, public]);
        _ ->
            ok
    end,
    true.

%% write profile to table
write_profile(Key, P) ->
    Ret = ets:insert(?ETS_TABLE, {{Key}, P}),
    ?debugFmt("~p~n",[Ret]),
    Ret.
    

%% return all language profiles
read_profiles() ->
    ets:foldl(fun(A, Acc) ->
                      Acc ++ [A]
              end,
              [],
              ?ETS_TABLE).

%% read in directory of language files to store as ngram profiles
%% sample wikipedia data is provied in this repository
-spec train(string()) ->
    atom().
train(Directory) ->
    %% init Database
    init(),
    TrainingData = [
                    %% af (afrikaans)
                    [ "ar", "ar-utf8.txt", "utf8", "arabic" ],
                    [ "bg", "bg-utf8.txt", "utf8", "bulgarian" ],
                    %% bs (bosnian )
                    %% ca (catalan)
                    [ "cs", "cs-utf8.txt", "utf8", "czech" ],
                    %% cy (welsh)
                    [ "da", "da-iso-8859-1.txt", "iso-8859-1", "danish" ],
                    [ "de", "de-utf8.txt", "utf8", "german" ],
                    [ "el", "el-utf8.txt", "utf8", "greek" ],
                    [ "en", "en-iso-8859-1.txt", "iso-8859-1", "english" ],
                    [ "et", "et-utf8.txt", "utf8", "estonian" ],
                    [ "es", "es-utf8.txt", "utf8", "spanish" ],
                    [ "fa", "fa-utf8.txt", "utf8", "farsi" ],
                    [ "fi", "fi-utf8.txt", "utf8", "finnish" ],
                    [ "fr", "fr-utf8.txt", "utf8", "french" ],
                    [ "fy", "fy-utf8.txt", "utf8", "frisian" ],
                    [ "ga", "ga-utf8.txt", "utf8", "irish" ],
                    %% gd (gaelic)
                    %% haw (hawaiian)
                    [ "he", "he-utf8.txt", "utf8", "hebrew" ],
                    [ "hi", "hi-utf8.txt", "utf8", "hindi" ],
                    [ "hr", "hr-utf8.txt", "utf8", "croatian" ],
                    %% id (indonesian)
                    [ "io", "io-utf8.txt", "utf8", "ido" ],
                    [ "is", "is-utf8.txt", "utf8", "icelandic" ],
                    [ "it", "it-utf8.txt", "utf8", "italian" ],
                    [ "ja", "ja-utf8.txt", "utf8", "japanese" ],
                    [ "ko", "ko-utf8.txt", "utf8", "korean" ],
                    %% ku (kurdish)
                    %% la ?
                    %% lb ?
                    %% lt (lithuanian)
                    %% lv (latvian)
                    [ "hu", "hu-utf8.txt", "utf8", "hungarian" ],
                    %% mk (macedonian)
                    %% ms (malay)
                    %% my (burmese)
                    [ "nl", "nl-iso-8859-1.txt", "iso-8859-1", "dutch" ],
                    [ "no", "no-utf8.txt", "utf8", "norwegian" ],
                    [ "pl", "pl-utf8.txt", "utf8", "polish" ],
                    [ "pt", "pt-utf8.txt", "utf8", "portuguese" ],
                    [ "ro", "ro-utf8.txt", "utf8", "romanian" ],
                    [ "ru", "ru-utf8.txt", "utf8", "russian" ],
                    [ "sl", "sl-utf8.txt", "utf8", "slovenian" ],
                    %% sr (serbian)
                    [ "sv", "sv-iso-8859-1.txt", "iso-8859-1", "swedish" ],
                     %% [ "sv", "sv-utf8.txt", "utf8", "swedish" ],
                    [ "th", "th-utf8.txt", "utf8", "thai" ],
                    %% tl (tagalog)
                    %% ty (tahitian)
                    [ "uk", "uk-utf8.txt", "utf8", "ukraninan" ],
                    [ "vi", "vi-utf8.txt", "utf8", "vietnamese" ],
                    %% wa (walloon)
                    %% yi (yidisih)
                    [ "zh", "zh-utf8.txt", "utf8", "chinese" ]
                   ],    
    train_from_files(Directory, TrainingData).
%% compile the language profiles into a header for better performance
%% Pass in the directory name where the training data lives.
-spec profile_to_hrl(string()) ->
    atom().
profile_to_hrl(D) ->
    train(D),
    P = read_profiles(),
    FileData = lists:flatten(
		 io_lib:format("-define(PROFILE, ~w).~n", 
			       [P])
		),
    file:write_file("./ngram.hrl", FileData),
    ok.


%% read in each file and create profile
train_from_files(_, []) ->
    ok;
train_from_files(Directory, [FileInfo|Files]) when is_list(Directory) ->
    [BiName, File, _CharSet, _Name] = FileInfo,
    %% read from file
    ?debugFmt("~p/~p~n", [Directory, File]),
    {ok,B} = file:read_file(Directory++"/"++File),    
    ?debugMsg("finished reading file"),
    PL = ngrams(binary_to_list(B)),
    P = dict:from_list(PL),
    ?debugFmt("writing ~p to Db",[BiName]),
    write_profile(BiName, P),
    train_from_files(Directory, Files).

%% ngrams(Text) return a list of ngrams for given text
-spec ngrams(string()) ->
    tuple().
ngrams(Text) ->
    ngrams(Text, length(Text)).
ngrams([], _) ->
    [];
ngrams(Text, Length) ->
    Tokens = tokenize(Text, Length),
    Rest = case length(Tokens) of
               1 ->
                   Tokens;
               _ ->
                   [_| R] = lists:reverse(Tokens),
                   R
           end,
    P1 = lists:reverse(lists:keysort(2, ngram_each([2, 3, 4, 5], 
                                                   lists:reverse(Rest), 
                                                   []))),
    NFold = ngrams_count(P1),
    lists:reverse(NFold).

ngrams_count(L) ->
    ngrams_count(L, dict:new(), 0).
ngrams_count([], Pl, _Acc) -> dict:to_list(Pl);
ngrams_count(_, Pl, Acc) when Acc > ?LIMIT ->
    dict:to_list(Pl);
ngrams_count([{Key,_}|T], Pl, Acc) ->
    Val = Acc + 1,
    D = dict:store(Key, Val, Pl),
    ngrams_count(T, D, Val).

%% count the ngram in the given string
ngram(N, PL, L) when length(L) < N ->
    PL;
ngram(N, PL, [_|R]=L) ->
    {Key, _} = lists:split(N, L),
    PL2 = incr_pl(Key, PL),
    ngram(N, PL2, R).

%% iterate through list of integers and call ngram_count with given integer
ngram_each(_N, [], []) ->
    []; %% handle empty list and accumulator
ngram_each([], _Text, PL) ->
    dict:to_list(PL);
ngram_each(NL, Text, PL) when is_list(PL) ->
    ngram_each(NL, Text, dict:from_list(PL));
ngram_each([N|R], Text, PL) ->
    X = ngram_count(N, Text, PL),
    ngram_each(R, Text, X).

%% calls ngram() on each token 
ngram_count(_N, [], PL) ->
    PL;
ngram_count(N, [T|R], PL) ->
    T0 = case length(T) >= N of
             true ->
                 U = case N of 
                         1 ->
                             "";
                         _ ->
                             "_"
                     end,
                 U ++ T ++ string:chars($_, N - 1);
             false ->
                 T
         end,  
    X = ngram(N, PL, T0),
    ngram_count(N, R, X).
    
incr_pl(Key, D) ->
    dict:update_counter(Key, 1, D).
    
%% remove punctuation characters
strip_punctuation(Input) ->
    strip_punctuation(Input, []).

strip_punctuation([], OK) -> lists:reverse(OK);
strip_punctuation([H|T], OK) ->
    case lists:member(H, ?punctuation) of
        true -> strip_punctuation(T, OK);
        false -> strip_punctuation(T, [H|OK])
    end.

%% tokenize , split up strings
tokenize(Text) ->
    tokenize(Text, length(Text)).
tokenize(Text, Length) ->
    string:tokens(strip_punctuation(
                    string:to_lower(string:substr(Text,
                                                  1,
                                                  Length))),
                  " ").

%% compute the distance between two ngram profiles Compare the list of
%% ngrams to the dictionary of language profiles.  Best is used to
%% keep track of the best distance. This ends the function when Best
%% is less than the computed distance.  The last parameter is the
%% distance accumulator.
-spec compute_distance(list(), dict(), integer(), integer()) ->
    tuple().
compute_distance(P1, P2, Best) ->
    compute_distance(P1, P2, Best, 0).
compute_distance([], _P2, _Best, X) ->
    X;
compute_distance(_P1, _P2, Best, Dist) when Best < Dist ->
    Dist;
compute_distance([{K, Val}|R], P2, Best, Dist) ->    
    X = case dict:find(K, P2) of
            {ok, Value} ->
                Value;
            _ ->
                ?LIMIT
        end,
    D = case X of 
            ?LIMIT ->
                ?LIMIT + Dist;                
            _ ->
                abs(Val - X) + Dist
        end,
    compute_distance(R, P2, Best, D).
    

%% Return the most likely langauge of a given text.  Given the text
%% string this function looks at all trained profiles and returns the
%% best match (closest profile). A tuple is returned { "en" , 24000}
%% with the two letter code for the language and the distance
%% calculation.
-spec detect(string()) ->
    tuple().
detect(Text) ->
    Profiles = case length(?PROFILE) of
		   0 ->
		       read_profiles();
		   _ ->
		       ?PROFILE
	       end,
    TextP = ngrams(Text, ?CHAR_LIMIT),
    Dist = lists:foldl(fun(Rec, Acc) ->
                               LangBest = case length(Acc) of
                                              0 ->
                                                  {nil,?LIMIT * ?LIMIT};
                                              _ ->
                                                  [B | _] = Acc,
                                                  B
                                          end,
                               {{L}, P1} = Rec,
                               {_, Best} = LangBest,
                               D = compute_distance(TextP, P1, Best),
                               lists:keysort(2, [LangBest] ++ [{L, D}])
                       end,
                       [],
                       Profiles),
    [Best | _Rest ] = Dist,
    Best.
                                 
    
%% Tests for this ngram module. The Makefile contains a rule to build
%% and run the tests.

%% test strip punctuation
strip_punctuation_test() ->
    Data = "test .,\n\t\r!?\"()+_ test",
    Data0 = strip_punctuation(Data),
    ?assertEqual("test  test", Data0).

%% test tokenize function
tokenize_test() ->
    Data = "crazy stuf in here ,+_ A \t 123 test",
    X = tokenize(Data),
    ?assertEqual(X, ["crazy", "stuf", "in" , "here", "a", "test"]).

%% test count ngram
ngram_count_test() ->
    P = dict:to_list(ngram_count(1, ["words"], dict:new())),
    ?assertEqual(P, [{"r",1},
                     {"o",1},
                     {"s",1},
                     {"w",1},
                     {"d",1}]),
    P0 = dict:to_list(ngram_count(2, ["words"], dict:new())),
    ?assertEqual(P0, [{"or",1},
                      {"rd",1},
                      {"s_",1},
                      {"ds",1},
                      {"wo",1},
                      {"_w",1}]),
    P1 = dict:to_list(ngram_count(3, ["words"], dict:new())),
    ?assertEqual(P1, [{"ord",1},
                      {"s__",1},
                      {"rds",1},
                      {"wor",1},
                      {"_wo",1},
                      {"ds_",1}]),
    P2 = dict:to_list(ngram_count(4, ["words"], dict:new())),
    ?assertEqual(P2, [{"s___",1},
                      {"rds_",1},
                      {"ords",1},
                      {"ds__",1},
                      {"_wor",1},
                      {"word",1}]),
    P3 = dict:to_list(ngram_count(5, ["words"], dict:new())),
    ?assertEqual(P3, [{"s____",1},
                      {"rds__",1},
                      {"ords_",1},
                      {"_word",1},
                      {"words",1},
                      {"ds___",1}]),
    P4 = dict:to_list(ngram_count(6, ["words"], dict:new())),
    ?assertEqual(P4, []).

ngrams_test() ->
    %% simple test first
    ngrams("para poner este importante proyecto en práctica"),
    ngrams("what the what"),
    ngrams("what"),
    ?assertEqual(true, true).

ngram_test() ->
    Ret = ngram(2, dict:new(), "_what_"),
    Expected = [{"ha",1},{"t_",1},{"at",1},{"wh",1},{"_w",1}],
    ?assertEqual(Expected, dict:to_list(Ret)).

%% Test the distance computation
compute_distance_test() ->
    P1 = ngrams("what the"),
    P2 = ngrams("This is english it is"),
    Ret = compute_distance(P1, dict:from_list(P2), ?LIMIT*?LIMIT),
    ?assertEqual(28020, Ret),
    P3 = ngrams("this is a test"),
    P4 = ngrams("this is a test"),
    ?assertEqual(compute_distance(P4, dict:from_list(P3), ?LIMIT*?LIMIT), 0),
    P5 = ngrams("this is ,+_  A \t 123 test"),
    P6 = ngrams("xxxx"),
    ?assertEqual(compute_distance(P6, dict:from_list(P5), ?LIMIT*?LIMIT), 
                 24000).

detect_speed_test() ->
    TestStr = lists:flatten(
		lists:flatmap(fun(_) ->
				      "this is a test of the Emergency text categorizing system."
			      end,
			      lists:seq(1, 1000))),
    lists:foreach(fun(_) ->
			  ?debugVal(statistics(wall_clock)),
			  {En, _} = detect(TestStr),
			  ?debugVal(statistics(wall_clock)),
			  ?assertEqual("en", En)
		  end,
		  lists:seq(1, 10)),
    ok.

detect_en_es_test() ->
    fprof:trace(start, "ngram.fprof"),
    init(), %% initialize database
    {Es, _P} = detect("para poner este importante proyecto en práctica"),
    ?assertEqual("es", Es), 
    {En, _} = detect("this is a test of the Emergency text categorizing system."),
    fprof:trace(stop),
    ?assertEqual("en", En).

detect_fr_it_test() ->
    {Fr, _} = detect("serait désigné peu après PDG d'Antenne 2 et de FR 3. Pas même lui ! Le"),
    ?assertEqual("fr", Fr),
    {It, _} = detect("studio dell'uomo interiore? La scienza del cuore umano, che"),
    ?assertEqual("it", It).
detect_ro_pl_test() ->
    {Ro, _} = detect("taiate pe din doua, in care vezi stralucind brun  sau violet cristalele interioare"),
    ?assertEqual("ro", Ro),
    {Pl, _} = detect("na porozumieniu, na ³±czeniu si³ i ¶rodków. Dlatego szukam ludzi, którzy"),
    ?assertEqual("pl", Pl).

detect_lang_test() ->
    {De, _} = detect("sagt Hühsam das war bei Über eine Annonce in einem Frankfurter der Töpfer ein. Anhand von gefundenen gut kennt, hatte ihm die wahren Tatsachen Sechzehn Adorno-Schüler erinnern und daß ein Weiterdenken der Theorie für ihre Festlegung sind drei Jahre Erschütterung Einblick in die Abhängigkeit der Bauarbeiten sei"),
    ?assertEqual("de",De),
    {Fi, _} = detect("koulun arkistoihin pölyttymään, vaan nuoret saavat itse vaikuttaa ajatustensa eteenpäinviemiseen esimerkiksi"),
    ?assertEqual("fi", Fi),
    {Hu, _} = detect("esôzéseket egy kissé túlméretezte, ebbôl kifolyólag a Földet egy hatalmas árvíz mosta el"),
    ?assertEqual("hu", Hu),
    {Fi, _} = detect("koulun arkistoihin pölyttymään, vaan nuoret saavat itse vaikuttaa ajatustensa eteenpäinviemiseen esimerkiksi"),
    ?assertEqual("fi", Fi),
    {Nl, _} = detect("tegen de kabinetsplannen. Een speciaal in het leven geroepen Landelijk"), 
    ?assertEqual("nl", Nl),
    {Da, _} = detect("viksomhed, 58 pct. har et arbejde eller er under uddannelse, 76 pct. forsørges ikke længere af Kolding"),
    ?assertEqual("da", Da), 
    {Cs, _} = detect("datují rokem 1862.  Naprosto zakázán byl v pocitech smutku, beznadìje èi jiné"),
    ?assertEqual("cs", Cs), 
    {No, _} = detect("hånd på den enda hvitere restaurant-duken med en bevegelse så forfinet"),
    ?assertEqual("no", No), 
    {Pt, _} = detect("popular. Segundo o seu biógrafo, a Maria Adelaide auxiliava muita gente"),
    ?assertEqual("pt", Pt),
    {Ar, _} = detect("تكبّد المدنيون خسائر في الأرواح إبّان الحرب العالمية الثانية أكثر من أي حرب عرفها التاريخ، ويعزى السبب لتقليعة القصف الجوي على [[مدينة|المد"),
    ?assertEqual("ar", Ar),
    {Jp, _} = detect("このうちアジア・太平洋地域における戦局、すなわち日本が米英蘭に対して戦端を開いた["),
    ?assertEqual("ja", Jp),
    {En0, _} = detect("TaffyDB finders looking nice so far!"),
    ?assertEqual("en", En0).
     
    
