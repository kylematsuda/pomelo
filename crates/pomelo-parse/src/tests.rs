use crate::Parser;
use expect_test::{expect, Expect};

pub(crate) fn check(should_error: bool, src: &str, expect: Expect) {
    let parser = Parser::new(src);
    let tree = parser.parse();

    let actual: String = format!("{}", tree);
    expect.assert_eq(&actual);

    assert_eq!(tree.has_errors(), should_error);
}

pub(crate) fn check_with_f<F>(should_error: bool, parse_function: F, src: &str, expect: Expect)
where
    F: FnMut(&mut Parser),
{
    let parser = Parser::new(src);
    let tree = parser.parse_inner(parse_function);

    let actual: String = format!("{}", tree);
    expect.assert_eq(&actual);

    assert_eq!(tree.has_errors(), should_error);
}

#[test]
// #[ignore]
fn aocprogram() {
    // This won't work until we figure out how to correctly
    // glue ":" and ":" into "::". And implement it everywhere!
    let src = r#"
val input = "../../input/day6/input.txt"

fun readlines (infile : string) = let
  val ins = TextIO.openIn infile 
  fun loop ins = 
    case TextIO.inputLine ins of
         SOME line => line :: (loop ins)
       | NONE      => []
  in 
    loop ins before TextIO.closeIn ins 
  end

val initial = List.mapPartial Int.fromString
  (String.tokens (fn c => c = #",") (hd (readlines input)))

val numAtTimer = 
  (fn t => (length o (List.filter (fn x => x = t))) initial)

val initial_timers = List.tabulate(9, numAtTimer) 

fun advance timers =
  let 
    val at_zero = List.nth(timers, 0)
    fun next 8 = at_zero 
      | next 6 = at_zero + List.nth(timers, 7)
      | next n = List.nth(timers, n + 1)
  in 
    List.tabulate(9, next)
  end

fun simulate num_days initial_timers = 
  let 
    fun loop count ts = 
      if count = num_days then
        ts 
      else
        loop (count + 1) (advance ts)
  in 
    foldl (op +) 0 (loop 0 initial_timers)
  end

val part_one = simulate 80 initial_timers
val part_two = simulate 256 initial_timers
"#;
    check(
        false,
        src,
        expect![[r##"
            FILE@0..1048
              WHITESPACE@0..1
              DEC@1..1047
                SEQ_DEC@1..1047
                  DEC@1..41
                    VAL_DEC@1..41
                      VAL_KW@1..4 "val"
                      WHITESPACE@4..5
                      VAL_BIND@5..41
                        PAT@5..10
                          AT_PAT@5..10
                            VID_PAT@5..10
                              LONG_VID@5..10
                                VID@5..10 "input"
                        WHITESPACE@10..11
                        EQ@11..12 "="
                        WHITESPACE@12..13
                        EXP@13..41
                          AT_EXP@13..41
                            SCON_EXP@13..41
                              STRING@13..41 "\"../../input/day6/inp ..."
                  WHITESPACE@41..43
                  DEC@43..285
                    FUN_DEC@43..285
                      FUN_KW@43..46 "fun"
                      WHITESPACE@46..47
                      TY_VAR_SEQ@47..47
                      FVAL_BIND@47..285
                        FVAL_BIND_ROW@47..283
                          VID@47..56 "readlines"
                          WHITESPACE@56..57
                          AT_PAT@57..74
                            L_PAREN@57..58 "("
                            PAT@58..73
                              TY_PAT@58..73
                                PAT@58..64
                                  AT_PAT@58..64
                                    VID_PAT@58..64
                                      LONG_VID@58..64
                                        VID@58..64 "infile"
                                WHITESPACE@64..65
                                COLON@65..66 ":"
                                WHITESPACE@66..67
                                TY@67..73
                                  LONG_TY_CON@67..73
                                    TY_CON@67..73 "string"
                            R_PAREN@73..74 ")"
                          WHITESPACE@74..75
                          EQ@75..76 "="
                          WHITESPACE@76..77
                          EXP@77..283
                            AT_EXP@77..283
                              LET_DEC@77..283
                                LET_KW@77..80 "let"
                                WHITESPACE@80..83
                                DEC@83..234
                                  SEQ_DEC@83..234
                                    DEC@83..113
                                      VAL_DEC@83..113
                                        VAL_KW@83..86 "val"
                                        WHITESPACE@86..87
                                        VAL_BIND@87..113
                                          PAT@87..90
                                            AT_PAT@87..90
                                              VID_PAT@87..90
                                                LONG_VID@87..90
                                                  VID@87..90 "ins"
                                          WHITESPACE@90..91
                                          EQ@91..92 "="
                                          WHITESPACE@92..93
                                          EXP@93..113
                                            UNRES_INFIX_APP_EXP@93..113
                                              EXP@93..106
                                                AT_EXP@93..106
                                                  VID_EXP@93..106
                                                    LONG_VID@93..106
                                                      STRID@93..99 "TextIO"
                                                      DOT@99..100 "."
                                                      VID@100..106 "openIn"
                                              WHITESPACE@106..107
                                              EXP@107..113
                                                AT_EXP@107..113
                                                  VID_EXP@107..113
                                                    LONG_VID@107..113
                                                      VID@107..113 "infile"
                                    WHITESPACE@113..117
                                    DEC@117..234
                                      FUN_DEC@117..234
                                        FUN_KW@117..120 "fun"
                                        WHITESPACE@120..121
                                        TY_VAR_SEQ@121..121
                                        FVAL_BIND@121..234
                                          FVAL_BIND_ROW@121..231
                                            VID@121..125 "loop"
                                            WHITESPACE@125..126
                                            AT_PAT@126..129
                                              VID_PAT@126..129
                                                LONG_VID@126..129
                                                  VID@126..129 "ins"
                                            WHITESPACE@129..130
                                            EQ@130..131 "="
                                            WHITESPACE@131..137
                                            EXP@137..231
                                              CASE_MATCH_EXP@137..231
                                                CASE_KW@137..141 "case"
                                                WHITESPACE@141..142
                                                EXP@142..162
                                                  UNRES_INFIX_APP_EXP@142..162
                                                    EXP@142..158
                                                      AT_EXP@142..158
                                                        VID_EXP@142..158
                                                          LONG_VID@142..158
                                                            STRID@142..148 "TextIO"
                                                            DOT@148..149 "."
                                                            VID@149..158 "inputLine"
                                                    WHITESPACE@158..159
                                                    EXP@159..162
                                                      AT_EXP@159..162
                                                        VID_EXP@159..162
                                                          LONG_VID@159..162
                                                            VID@159..162 "ins"
                                                WHITESPACE@162..163
                                                OF_KW@163..165 "of"
                                                WHITESPACE@165..175
                                                MATCH@175..231
                                                  MRULE@175..206
                                                    PAT@175..184
                                                      UNRES_INFIX_APP_PAT@175..184
                                                        PAT@175..179
                                                          AT_PAT@175..179
                                                            VID_PAT@175..179
                                                              LONG_VID@175..179
                                                                VID@175..179 "SOME"
                                                        WHITESPACE@179..180
                                                        PAT@180..184
                                                          AT_PAT@180..184
                                                            VID_PAT@180..184
                                                              LONG_VID@180..184
                                                                VID@180..184 "line"
                                                    WHITESPACE@184..185
                                                    THICK_ARROW@185..187 "=>"
                                                    WHITESPACE@187..188
                                                    EXP@188..206
                                                      UNRES_INFIX_APP_EXP@188..206
                                                        EXP@188..192
                                                          AT_EXP@188..192
                                                            VID_EXP@188..192
                                                              LONG_VID@188..192
                                                                VID@188..192 "line"
                                                        WHITESPACE@192..193
                                                        EXP@193..195
                                                          AT_EXP@193..195
                                                            VID_EXP@193..195
                                                              LONG_VID@193..195
                                                                VID@193..195 "::"
                                                        WHITESPACE@195..196
                                                        EXP@196..206
                                                          AT_EXP@196..206
                                                            L_PAREN@196..197 "("
                                                            EXP@197..205
                                                              UNRES_INFIX_APP_EXP@197..205
                                                                EXP@197..201
                                                                  AT_EXP@197..201
                                                                    VID_EXP@197..201
                                                                      LONG_VID@197..201
                                                                        VID@197..201 "loop"
                                                                WHITESPACE@201..202
                                                                EXP@202..205
                                                                  AT_EXP@202..205
                                                                    VID_EXP@202..205
                                                                      LONG_VID@202..205
                                                                        VID@202..205 "ins"
                                                            R_PAREN@205..206 ")"
                                                  WHITESPACE@206..214
                                                  PIPE@214..215 "|"
                                                  WHITESPACE@215..216
                                                  MRULE@216..231
                                                    PAT@216..220
                                                      AT_PAT@216..220
                                                        VID_PAT@216..220
                                                          LONG_VID@216..220
                                                            VID@216..220 "NONE"
                                                    WHITESPACE@220..226
                                                    THICK_ARROW@226..228 "=>"
                                                    WHITESPACE@228..229
                                                    EXP@229..231
                                                      AT_EXP@229..231
                                                        LIST_EXP@229..231
                                                          L_BRACKET@229..230 "["
                                                          R_BRACKET@230..231 "]"
                                          WHITESPACE@231..234
                                IN_KW@234..236 "in"
                                WHITESPACE@236..242
                                EXP@242..276
                                  UNRES_INFIX_APP_EXP@242..276
                                    EXP@242..246
                                      AT_EXP@242..246
                                        VID_EXP@242..246
                                          LONG_VID@242..246
                                            VID@242..246 "loop"
                                    WHITESPACE@246..247
                                    EXP@247..250
                                      AT_EXP@247..250
                                        VID_EXP@247..250
                                          LONG_VID@247..250
                                            VID@247..250 "ins"
                                    WHITESPACE@250..251
                                    EXP@251..257
                                      AT_EXP@251..257
                                        VID_EXP@251..257
                                          LONG_VID@251..257
                                            VID@251..257 "before"
                                    WHITESPACE@257..258
                                    EXP@258..272
                                      AT_EXP@258..272
                                        VID_EXP@258..272
                                          LONG_VID@258..272
                                            STRID@258..264 "TextIO"
                                            DOT@264..265 "."
                                            VID@265..272 "closeIn"
                                    WHITESPACE@272..273
                                    EXP@273..276
                                      AT_EXP@273..276
                                        VID_EXP@273..276
                                          LONG_VID@273..276
                                            VID@273..276 "ins"
                                WHITESPACE@276..280
                                END_KW@280..283 "end"
                        WHITESPACE@283..285
                  DEC@285..389
                    VAL_DEC@285..389
                      VAL_KW@285..288 "val"
                      WHITESPACE@288..289
                      VAL_BIND@289..389
                        PAT@289..296
                          AT_PAT@289..296
                            VID_PAT@289..296
                              LONG_VID@289..296
                                VID@289..296 "initial"
                        WHITESPACE@296..297
                        EQ@297..298 "="
                        WHITESPACE@298..299
                        EXP@299..389
                          UNRES_INFIX_APP_EXP@299..389
                            EXP@299..314
                              AT_EXP@299..314
                                VID_EXP@299..314
                                  LONG_VID@299..314
                                    STRID@299..303 "List"
                                    DOT@303..304 "."
                                    VID@304..314 "mapPartial"
                            WHITESPACE@314..315
                            EXP@315..329
                              AT_EXP@315..329
                                VID_EXP@315..329
                                  LONG_VID@315..329
                                    STRID@315..318 "Int"
                                    DOT@318..319 "."
                                    VID@319..329 "fromString"
                            WHITESPACE@329..332
                            EXP@332..389
                              AT_EXP@332..389
                                L_PAREN@332..333 "("
                                EXP@333..388
                                  UNRES_INFIX_APP_EXP@333..388
                                    EXP@333..346
                                      AT_EXP@333..346
                                        VID_EXP@333..346
                                          LONG_VID@333..346
                                            STRID@333..339 "String"
                                            DOT@339..340 "."
                                            VID@340..346 "tokens"
                                    WHITESPACE@346..347
                                    EXP@347..365
                                      AT_EXP@347..365
                                        L_PAREN@347..348 "("
                                        EXP@348..364
                                          FN_EXP@348..364
                                            FN_KW@348..350 "fn"
                                            WHITESPACE@350..351
                                            MATCH@351..364
                                              MRULE@351..364
                                                PAT@351..352
                                                  AT_PAT@351..352
                                                    VID_PAT@351..352
                                                      LONG_VID@351..352
                                                        VID@351..352 "c"
                                                WHITESPACE@352..353
                                                THICK_ARROW@353..355 "=>"
                                                WHITESPACE@355..356
                                                EXP@356..364
                                                  UNRES_INFIX_APP_EXP@356..364
                                                    EXP@356..357
                                                      AT_EXP@356..357
                                                        VID_EXP@356..357
                                                          LONG_VID@356..357
                                                            VID@356..357 "c"
                                                    WHITESPACE@357..358
                                                    EXP@358..359
                                                      AT_EXP@358..359
                                                        VID_EXP@358..359
                                                          LONG_VID@358..359
                                                            VID@358..359 "="
                                                    WHITESPACE@359..360
                                                    EXP@360..364
                                                      AT_EXP@360..364
                                                        SCON_EXP@360..364
                                                          CHAR@360..364 "#\",\""
                                        R_PAREN@364..365 ")"
                                    WHITESPACE@365..366
                                    EXP@366..388
                                      AT_EXP@366..388
                                        L_PAREN@366..367 "("
                                        EXP@367..387
                                          UNRES_INFIX_APP_EXP@367..387
                                            EXP@367..369
                                              AT_EXP@367..369
                                                VID_EXP@367..369
                                                  LONG_VID@367..369
                                                    VID@367..369 "hd"
                                            WHITESPACE@369..370
                                            EXP@370..387
                                              AT_EXP@370..387
                                                L_PAREN@370..371 "("
                                                EXP@371..386
                                                  UNRES_INFIX_APP_EXP@371..386
                                                    EXP@371..380
                                                      AT_EXP@371..380
                                                        VID_EXP@371..380
                                                          LONG_VID@371..380
                                                            VID@371..380 "readlines"
                                                    WHITESPACE@380..381
                                                    EXP@381..386
                                                      AT_EXP@381..386
                                                        VID_EXP@381..386
                                                          LONG_VID@381..386
                                                            VID@381..386 "input"
                                                R_PAREN@386..387 ")"
                                        R_PAREN@387..388 ")"
                                R_PAREN@388..389 ")"
                  WHITESPACE@389..391
                  DEC@391..469
                    VAL_DEC@391..469
                      VAL_KW@391..394 "val"
                      WHITESPACE@394..395
                      VAL_BIND@395..469
                        PAT@395..405
                          AT_PAT@395..405
                            VID_PAT@395..405
                              LONG_VID@395..405
                                VID@395..405 "numAtTimer"
                        WHITESPACE@405..406
                        EQ@406..407 "="
                        WHITESPACE@407..411
                        EXP@411..469
                          AT_EXP@411..469
                            L_PAREN@411..412 "("
                            EXP@412..468
                              FN_EXP@412..468
                                FN_KW@412..414 "fn"
                                WHITESPACE@414..415
                                MATCH@415..468
                                  MRULE@415..468
                                    PAT@415..416
                                      AT_PAT@415..416
                                        VID_PAT@415..416
                                          LONG_VID@415..416
                                            VID@415..416 "t"
                                    WHITESPACE@416..417
                                    THICK_ARROW@417..419 "=>"
                                    WHITESPACE@419..420
                                    EXP@420..468
                                      UNRES_INFIX_APP_EXP@420..468
                                        EXP@420..460
                                          AT_EXP@420..460
                                            L_PAREN@420..421 "("
                                            EXP@421..459
                                              UNRES_INFIX_APP_EXP@421..459
                                                EXP@421..427
                                                  AT_EXP@421..427
                                                    VID_EXP@421..427
                                                      LONG_VID@421..427
                                                        VID@421..427 "length"
                                                WHITESPACE@427..428
                                                EXP@428..429
                                                  AT_EXP@428..429
                                                    VID_EXP@428..429
                                                      LONG_VID@428..429
                                                        VID@428..429 "o"
                                                WHITESPACE@429..430
                                                EXP@430..459
                                                  AT_EXP@430..459
                                                    L_PAREN@430..431 "("
                                                    EXP@431..458
                                                      UNRES_INFIX_APP_EXP@431..458
                                                        EXP@431..442
                                                          AT_EXP@431..442
                                                            VID_EXP@431..442
                                                              LONG_VID@431..442
                                                                STRID@431..435 "List"
                                                                DOT@435..436 "."
                                                                VID@436..442 "filter"
                                                        WHITESPACE@442..443
                                                        EXP@443..458
                                                          AT_EXP@443..458
                                                            L_PAREN@443..444 "("
                                                            EXP@444..457
                                                              FN_EXP@444..457
                                                                FN_KW@444..446 "fn"
                                                                WHITESPACE@446..447
                                                                MATCH@447..457
                                                                  MRULE@447..457
                                                                    PAT@447..448
                                                                      AT_PAT@447..448
                                                                        VID_PAT@447..448
                                                                          LONG_VID@447..448
                                                                            VID@447..448 "x"
                                                                    WHITESPACE@448..449
                                                                    THICK_ARROW@449..451 "=>"
                                                                    WHITESPACE@451..452
                                                                    EXP@452..457
                                                                      UNRES_INFIX_APP_EXP@452..457
                                                                        EXP@452..453
                                                                          AT_EXP@452..453
                                                                            VID_EXP@452..453
                                                                              LONG_VID@452..453
                                                                                VID@452..453 "x"
                                                                        WHITESPACE@453..454
                                                                        EXP@454..455
                                                                          AT_EXP@454..455
                                                                            VID_EXP@454..455
                                                                              LONG_VID@454..455
                                                                                VID@454..455 "="
                                                                        WHITESPACE@455..456
                                                                        EXP@456..457
                                                                          AT_EXP@456..457
                                                                            VID_EXP@456..457
                                                                              LONG_VID@456..457
                                                                                VID@456..457 "t"
                                                            R_PAREN@457..458 ")"
                                                    R_PAREN@458..459 ")"
                                            R_PAREN@459..460 ")"
                                        WHITESPACE@460..461
                                        EXP@461..468
                                          AT_EXP@461..468
                                            VID_EXP@461..468
                                              LONG_VID@461..468
                                                VID@461..468 "initial"
                            R_PAREN@468..469 ")"
                  WHITESPACE@469..471
                  DEC@471..520
                    VAL_DEC@471..520
                      VAL_KW@471..474 "val"
                      WHITESPACE@474..475
                      VAL_BIND@475..520
                        PAT@475..489
                          AT_PAT@475..489
                            VID_PAT@475..489
                              LONG_VID@475..489
                                VID@475..489 "initial_timers"
                        WHITESPACE@489..490
                        EQ@490..491 "="
                        WHITESPACE@491..492
                        EXP@492..520
                          UNRES_INFIX_APP_EXP@492..520
                            EXP@492..505
                              AT_EXP@492..505
                                VID_EXP@492..505
                                  LONG_VID@492..505
                                    STRID@492..496 "List"
                                    DOT@496..497 "."
                                    VID@497..505 "tabulate"
                            EXP@505..520
                              AT_EXP@505..520
                                TUPLE_EXP@505..520
                                  L_PAREN@505..506 "("
                                  EXP@506..507
                                    AT_EXP@506..507
                                      SCON_EXP@506..507
                                        INT@506..507 "9"
                                  COMMA@507..508 ","
                                  WHITESPACE@508..509
                                  EXP@509..519
                                    AT_EXP@509..519
                                      VID_EXP@509..519
                                        LONG_VID@509..519
                                          VID@509..519 "numAtTimer"
                                  R_PAREN@519..520 ")"
                  WHITESPACE@520..523
                  DEC@523..743
                    FUN_DEC@523..743
                      FUN_KW@523..526 "fun"
                      WHITESPACE@526..527
                      TY_VAR_SEQ@527..527
                      FVAL_BIND@527..743
                        FVAL_BIND_ROW@527..741
                          VID@527..534 "advance"
                          WHITESPACE@534..535
                          AT_PAT@535..541
                            VID_PAT@535..541
                              LONG_VID@535..541
                                VID@535..541 "timers"
                          WHITESPACE@541..542
                          EQ@542..543 "="
                          WHITESPACE@543..546
                          EXP@546..741
                            AT_EXP@546..741
                              LET_DEC@546..741
                                LET_KW@546..549 "let"
                                WHITESPACE@549..555
                                DEC@555..705
                                  SEQ_DEC@555..705
                                    DEC@555..588
                                      VAL_DEC@555..588
                                        VAL_KW@555..558 "val"
                                        WHITESPACE@558..559
                                        VAL_BIND@559..588
                                          PAT@559..566
                                            AT_PAT@559..566
                                              VID_PAT@559..566
                                                LONG_VID@559..566
                                                  VID@559..566 "at_zero"
                                          WHITESPACE@566..567
                                          EQ@567..568 "="
                                          WHITESPACE@568..569
                                          EXP@569..588
                                            UNRES_INFIX_APP_EXP@569..588
                                              EXP@569..577
                                                AT_EXP@569..577
                                                  VID_EXP@569..577
                                                    LONG_VID@569..577
                                                      STRID@569..573 "List"
                                                      DOT@573..574 "."
                                                      VID@574..577 "nth"
                                              EXP@577..588
                                                AT_EXP@577..588
                                                  TUPLE_EXP@577..588
                                                    L_PAREN@577..578 "("
                                                    EXP@578..584
                                                      AT_EXP@578..584
                                                        VID_EXP@578..584
                                                          LONG_VID@578..584
                                                            VID@578..584 "timers"
                                                    COMMA@584..585 ","
                                                    WHITESPACE@585..586
                                                    EXP@586..587
                                                      AT_EXP@586..587
                                                        SCON_EXP@586..587
                                                          INT@586..587 "0"
                                                    R_PAREN@587..588 ")"
                                    WHITESPACE@588..593
                                    DEC@593..705
                                      FUN_DEC@593..705
                                        FUN_KW@593..596 "fun"
                                        WHITESPACE@596..597
                                        TY_VAR_SEQ@597..597
                                        FVAL_BIND@597..705
                                          FVAL_BIND_ROW@597..613
                                            VID@597..601 "next"
                                            WHITESPACE@601..602
                                            AT_PAT@602..603
                                              SCON_PAT@602..603
                                                INT@602..603 "8"
                                            WHITESPACE@603..604
                                            EQ@604..605 "="
                                            WHITESPACE@605..606
                                            EXP@606..613
                                              AT_EXP@606..613
                                                VID_EXP@606..613
                                                  LONG_VID@606..613
                                                    VID@606..613 "at_zero"
                                          WHITESPACE@613..621
                                          FVAL_BIND_ROW@621..661
                                            PIPE@621..622 "|"
                                            WHITESPACE@622..623
                                            VID@623..627 "next"
                                            WHITESPACE@627..628
                                            AT_PAT@628..629
                                              SCON_PAT@628..629
                                                INT@628..629 "6"
                                            WHITESPACE@629..630
                                            EQ@630..631 "="
                                            WHITESPACE@631..632
                                            EXP@632..661
                                              UNRES_INFIX_APP_EXP@632..661
                                                EXP@632..639
                                                  AT_EXP@632..639
                                                    VID_EXP@632..639
                                                      LONG_VID@632..639
                                                        VID@632..639 "at_zero"
                                                WHITESPACE@639..640
                                                EXP@640..641
                                                  AT_EXP@640..641
                                                    VID_EXP@640..641
                                                      LONG_VID@640..641
                                                        VID@640..641 "+"
                                                WHITESPACE@641..642
                                                EXP@642..650
                                                  AT_EXP@642..650
                                                    VID_EXP@642..650
                                                      LONG_VID@642..650
                                                        STRID@642..646 "List"
                                                        DOT@646..647 "."
                                                        VID@647..650 "nth"
                                                EXP@650..661
                                                  AT_EXP@650..661
                                                    TUPLE_EXP@650..661
                                                      L_PAREN@650..651 "("
                                                      EXP@651..657
                                                        AT_EXP@651..657
                                                          VID_EXP@651..657
                                                            LONG_VID@651..657
                                                              VID@651..657 "timers"
                                                      COMMA@657..658 ","
                                                      WHITESPACE@658..659
                                                      EXP@659..660
                                                        AT_EXP@659..660
                                                          SCON_EXP@659..660
                                                            INT@659..660 "7"
                                                      R_PAREN@660..661 ")"
                                          WHITESPACE@661..668
                                          FVAL_BIND_ROW@668..702
                                            PIPE@668..669 "|"
                                            WHITESPACE@669..670
                                            VID@670..674 "next"
                                            WHITESPACE@674..675
                                            AT_PAT@675..676
                                              VID_PAT@675..676
                                                LONG_VID@675..676
                                                  VID@675..676 "n"
                                            WHITESPACE@676..677
                                            EQ@677..678 "="
                                            WHITESPACE@678..679
                                            EXP@679..702
                                              UNRES_INFIX_APP_EXP@679..702
                                                EXP@679..687
                                                  AT_EXP@679..687
                                                    VID_EXP@679..687
                                                      LONG_VID@679..687
                                                        STRID@679..683 "List"
                                                        DOT@683..684 "."
                                                        VID@684..687 "nth"
                                                EXP@687..702
                                                  AT_EXP@687..702
                                                    TUPLE_EXP@687..702
                                                      L_PAREN@687..688 "("
                                                      EXP@688..694
                                                        AT_EXP@688..694
                                                          VID_EXP@688..694
                                                            LONG_VID@688..694
                                                              VID@688..694 "timers"
                                                      COMMA@694..695 ","
                                                      WHITESPACE@695..696
                                                      EXP@696..701
                                                        UNRES_INFIX_APP_EXP@696..701
                                                          EXP@696..697
                                                            AT_EXP@696..697
                                                              VID_EXP@696..697
                                                                LONG_VID@696..697
                                                                  VID@696..697 "n"
                                                          WHITESPACE@697..698
                                                          EXP@698..699
                                                            AT_EXP@698..699
                                                              VID_EXP@698..699
                                                                LONG_VID@698..699
                                                                  VID@698..699 "+"
                                                          WHITESPACE@699..700
                                                          EXP@700..701
                                                            AT_EXP@700..701
                                                              SCON_EXP@700..701
                                                                INT@700..701 "1"
                                                      R_PAREN@701..702 ")"
                                          WHITESPACE@702..705
                                IN_KW@705..707 "in"
                                WHITESPACE@707..713
                                EXP@713..735
                                  UNRES_INFIX_APP_EXP@713..735
                                    EXP@713..726
                                      AT_EXP@713..726
                                        VID_EXP@713..726
                                          LONG_VID@713..726
                                            STRID@713..717 "List"
                                            DOT@717..718 "."
                                            VID@718..726 "tabulate"
                                    EXP@726..735
                                      AT_EXP@726..735
                                        TUPLE_EXP@726..735
                                          L_PAREN@726..727 "("
                                          EXP@727..728
                                            AT_EXP@727..728
                                              SCON_EXP@727..728
                                                INT@727..728 "9"
                                          COMMA@728..729 ","
                                          WHITESPACE@729..730
                                          EXP@730..734
                                            AT_EXP@730..734
                                              VID_EXP@730..734
                                                LONG_VID@730..734
                                                  VID@730..734 "next"
                                          R_PAREN@734..735 ")"
                                WHITESPACE@735..738
                                END_KW@738..741 "end"
                        WHITESPACE@741..743
                  DEC@743..963
                    FUN_DEC@743..963
                      FUN_KW@743..746 "fun"
                      WHITESPACE@746..747
                      TY_VAR_SEQ@747..747
                      FVAL_BIND@747..963
                        FVAL_BIND_ROW@747..961
                          VID@747..755 "simulate"
                          WHITESPACE@755..756
                          AT_PAT@756..764
                            VID_PAT@756..764
                              LONG_VID@756..764
                                VID@756..764 "num_days"
                          WHITESPACE@764..765
                          AT_PAT@765..779
                            VID_PAT@765..779
                              LONG_VID@765..779
                                VID@765..779 "initial_timers"
                          WHITESPACE@779..780
                          EQ@780..781 "="
                          WHITESPACE@781..785
                          EXP@785..961
                            AT_EXP@785..961
                              LET_DEC@785..961
                                LET_KW@785..788 "let"
                                WHITESPACE@788..794
                                DEC@794..909
                                  FUN_DEC@794..909
                                    FUN_KW@794..797 "fun"
                                    WHITESPACE@797..798
                                    TY_VAR_SEQ@798..798
                                    FVAL_BIND@798..909
                                      FVAL_BIND_ROW@798..906
                                        VID@798..802 "loop"
                                        WHITESPACE@802..803
                                        AT_PAT@803..808
                                          VID_PAT@803..808
                                            LONG_VID@803..808
                                              VID@803..808 "count"
                                        WHITESPACE@808..809
                                        AT_PAT@809..811
                                          VID_PAT@809..811
                                            LONG_VID@809..811
                                              VID@809..811 "ts"
                                        WHITESPACE@811..812
                                        EQ@812..813 "="
                                        WHITESPACE@813..821
                                        EXP@821..906
                                          IF_EXP@821..906
                                            IF_KW@821..823 "if"
                                            WHITESPACE@823..824
                                            EXP@824..840
                                              UNRES_INFIX_APP_EXP@824..840
                                                EXP@824..829
                                                  AT_EXP@824..829
                                                    VID_EXP@824..829
                                                      LONG_VID@824..829
                                                        VID@824..829 "count"
                                                WHITESPACE@829..830
                                                EXP@830..831
                                                  AT_EXP@830..831
                                                    VID_EXP@830..831
                                                      LONG_VID@830..831
                                                        VID@830..831 "="
                                                WHITESPACE@831..832
                                                EXP@832..840
                                                  AT_EXP@832..840
                                                    VID_EXP@832..840
                                                      LONG_VID@832..840
                                                        VID@832..840 "num_days"
                                            WHITESPACE@840..841
                                            THEN_KW@841..845 "then"
                                            WHITESPACE@845..854
                                            EXP@854..856
                                              AT_EXP@854..856
                                                VID_EXP@854..856
                                                  LONG_VID@854..856
                                                    VID@854..856 "ts"
                                            WHITESPACE@856..864
                                            ELSE_KW@864..868 "else"
                                            WHITESPACE@868..877
                                            EXP@877..906
                                              UNRES_INFIX_APP_EXP@877..906
                                                EXP@877..881
                                                  AT_EXP@877..881
                                                    VID_EXP@877..881
                                                      LONG_VID@877..881
                                                        VID@877..881 "loop"
                                                WHITESPACE@881..882
                                                EXP@882..893
                                                  AT_EXP@882..893
                                                    L_PAREN@882..883 "("
                                                    EXP@883..892
                                                      UNRES_INFIX_APP_EXP@883..892
                                                        EXP@883..888
                                                          AT_EXP@883..888
                                                            VID_EXP@883..888
                                                              LONG_VID@883..888
                                                                VID@883..888 "count"
                                                        WHITESPACE@888..889
                                                        EXP@889..890
                                                          AT_EXP@889..890
                                                            VID_EXP@889..890
                                                              LONG_VID@889..890
                                                                VID@889..890 "+"
                                                        WHITESPACE@890..891
                                                        EXP@891..892
                                                          AT_EXP@891..892
                                                            SCON_EXP@891..892
                                                              INT@891..892 "1"
                                                    R_PAREN@892..893 ")"
                                                WHITESPACE@893..894
                                                EXP@894..906
                                                  AT_EXP@894..906
                                                    L_PAREN@894..895 "("
                                                    EXP@895..905
                                                      UNRES_INFIX_APP_EXP@895..905
                                                        EXP@895..902
                                                          AT_EXP@895..902
                                                            VID_EXP@895..902
                                                              LONG_VID@895..902
                                                                VID@895..902 "advance"
                                                        WHITESPACE@902..903
                                                        EXP@903..905
                                                          AT_EXP@903..905
                                                            VID_EXP@903..905
                                                              LONG_VID@903..905
                                                                VID@903..905 "ts"
                                                    R_PAREN@905..906 ")"
                                      WHITESPACE@906..909
                                IN_KW@909..911 "in"
                                WHITESPACE@911..917
                                EXP@917..955
                                  UNRES_INFIX_APP_EXP@917..955
                                    EXP@917..922
                                      AT_EXP@917..922
                                        VID_EXP@917..922
                                          LONG_VID@917..922
                                            VID@917..922 "foldl"
                                    WHITESPACE@922..923
                                    EXP@923..929
                                      AT_EXP@923..929
                                        L_PAREN@923..924 "("
                                        EXP@924..928
                                          AT_EXP@924..928
                                            VID_EXP@924..928
                                              OP_KW@924..926 "op"
                                              WHITESPACE@926..927
                                              LONG_VID@927..928
                                                VID@927..928 "+"
                                        R_PAREN@928..929 ")"
                                    WHITESPACE@929..930
                                    EXP@930..931
                                      AT_EXP@930..931
                                        SCON_EXP@930..931
                                          INT@930..931 "0"
                                    WHITESPACE@931..932
                                    EXP@932..955
                                      AT_EXP@932..955
                                        L_PAREN@932..933 "("
                                        EXP@933..954
                                          UNRES_INFIX_APP_EXP@933..954
                                            EXP@933..937
                                              AT_EXP@933..937
                                                VID_EXP@933..937
                                                  LONG_VID@933..937
                                                    VID@933..937 "loop"
                                            WHITESPACE@937..938
                                            EXP@938..939
                                              AT_EXP@938..939
                                                SCON_EXP@938..939
                                                  INT@938..939 "0"
                                            WHITESPACE@939..940
                                            EXP@940..954
                                              AT_EXP@940..954
                                                VID_EXP@940..954
                                                  LONG_VID@940..954
                                                    VID@940..954 "initial_timers"
                                        R_PAREN@954..955 ")"
                                WHITESPACE@955..958
                                END_KW@958..961 "end"
                        WHITESPACE@961..963
                  DEC@963..1004
                    VAL_DEC@963..1004
                      VAL_KW@963..966 "val"
                      WHITESPACE@966..967
                      VAL_BIND@967..1004
                        PAT@967..975
                          AT_PAT@967..975
                            VID_PAT@967..975
                              LONG_VID@967..975
                                VID@967..975 "part_one"
                        WHITESPACE@975..976
                        EQ@976..977 "="
                        WHITESPACE@977..978
                        EXP@978..1004
                          UNRES_INFIX_APP_EXP@978..1004
                            EXP@978..986
                              AT_EXP@978..986
                                VID_EXP@978..986
                                  LONG_VID@978..986
                                    VID@978..986 "simulate"
                            WHITESPACE@986..987
                            EXP@987..989
                              AT_EXP@987..989
                                SCON_EXP@987..989
                                  INT@987..989 "80"
                            WHITESPACE@989..990
                            EXP@990..1004
                              AT_EXP@990..1004
                                VID_EXP@990..1004
                                  LONG_VID@990..1004
                                    VID@990..1004 "initial_timers"
                  WHITESPACE@1004..1005
                  DEC@1005..1047
                    VAL_DEC@1005..1047
                      VAL_KW@1005..1008 "val"
                      WHITESPACE@1008..1009
                      VAL_BIND@1009..1047
                        PAT@1009..1017
                          AT_PAT@1009..1017
                            VID_PAT@1009..1017
                              LONG_VID@1009..1017
                                VID@1009..1017 "part_two"
                        WHITESPACE@1017..1018
                        EQ@1018..1019 "="
                        WHITESPACE@1019..1020
                        EXP@1020..1047
                          UNRES_INFIX_APP_EXP@1020..1047
                            EXP@1020..1028
                              AT_EXP@1020..1028
                                VID_EXP@1020..1028
                                  LONG_VID@1020..1028
                                    VID@1020..1028 "simulate"
                            WHITESPACE@1028..1029
                            EXP@1029..1032
                              AT_EXP@1029..1032
                                SCON_EXP@1029..1032
                                  INT@1029..1032 "256"
                            WHITESPACE@1032..1033
                            EXP@1033..1047
                              AT_EXP@1033..1047
                                VID_EXP@1033..1047
                                  LONG_VID@1033..1047
                                    VID@1033..1047 "initial_timers"
              WHITESPACE@1047..1048
        "##]],
    )
}
