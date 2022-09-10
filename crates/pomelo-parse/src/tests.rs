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
#[ignore]
fn aocprogram() {
    // This won't work until we figure out how to correctly
    // glue ":" and ":" into "::". And implement it everywhere!
    let src = r#"
val input = "../../input/day6/input.txt"

fun readlines (infile : string) = let
  val ins = TextIO.openIn infile 
  fun loop ins = 
    case TextIO.inputLine ins of
        0 => 0
      | 1 => 1
    (*
         SOME line => line :: (loop ins)
       | NONE      => []
    *)
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
            FILE@0..1092
              WHITESPACE@0..1
              DEC@1..402
                SEQ_DEC@1..402
                  DEC@1..41
                    VAL_DEC@1..41
                      VAL_KW@1..4 "val"
                      WHITESPACE@4..5
                      VAL_BIND@5..41
                        PAT@5..10
                          AT_PAT@5..10
                            VID_PAT@5..10
                              LONG_VID@5..10
                                IDENT@5..10 "input"
                        WHITESPACE@10..11
                        EQ@11..12 "="
                        WHITESPACE@12..13
                        EXP@13..41
                          AT_EXP@13..41
                            SCON_EXP@13..41
                              STRING@13..41 "\"../../input/day6/inp ..."
                  WHITESPACE@41..43
                  DEC@43..329
                    FUN_DEC@43..329
                      FUN_KW@43..46 "fun"
                      WHITESPACE@46..47
                      TY_VAR_SEQ@47..47
                      FVAL_BIND@47..329
                        FVAL_BIND_ROW@47..327
                          VID@47..56
                            IDENT@47..56 "readlines"
                          WHITESPACE@56..57
                          AT_PAT@57..74
                            L_PAREN@57..58 "("
                            PAT@58..73
                              TY_PAT@58..73
                                PAT@58..64
                                  AT_PAT@58..64
                                    VID_PAT@58..64
                                      LONG_VID@58..64
                                        IDENT@58..64 "infile"
                                WHITESPACE@64..65
                                COLON@65..66 ":"
                                WHITESPACE@66..67
                                TY@67..73
                                  LONG_TY_CON@67..73
                                    IDENT@67..73 "string"
                            R_PAREN@73..74 ")"
                          WHITESPACE@74..75
                          EQ@75..76 "="
                          WHITESPACE@76..77
                          EXP@77..327
                            AT_EXP@77..327
                              LET_DEC@77..327
                                LET_KW@77..80 "let"
                                WHITESPACE@80..83
                                DEC@83..278
                                  SEQ_DEC@83..278
                                    DEC@83..113
                                      VAL_DEC@83..113
                                        VAL_KW@83..86 "val"
                                        WHITESPACE@86..87
                                        VAL_BIND@87..113
                                          PAT@87..90
                                            AT_PAT@87..90
                                              VID_PAT@87..90
                                                LONG_VID@87..90
                                                  IDENT@87..90 "ins"
                                          WHITESPACE@90..91
                                          EQ@91..92 "="
                                          WHITESPACE@92..93
                                          EXP@93..113
                                            UNRES_INFIX_APP_EXP@93..113
                                              EXP@93..106
                                                AT_EXP@93..106
                                                  VID_EXP@93..106
                                                    LONG_VID@93..106
                                                      IDENT@93..99 "TextIO"
                                                      DOT@99..100 "."
                                                      IDENT@100..106 "openIn"
                                              WHITESPACE@106..107
                                              EXP@107..113
                                                AT_EXP@107..113
                                                  VID_EXP@107..113
                                                    LONG_VID@107..113
                                                      IDENT@107..113 "infile"
                                    WHITESPACE@113..117
                                    DEC@117..278
                                      FUN_DEC@117..278
                                        FUN_KW@117..120 "fun"
                                        WHITESPACE@120..121
                                        TY_VAR_SEQ@121..121
                                        FVAL_BIND@121..278
                                          FVAL_BIND_ROW@121..195
                                            VID@121..125
                                              IDENT@121..125 "loop"
                                            WHITESPACE@125..126
                                            AT_PAT@126..129
                                              VID_PAT@126..129
                                                LONG_VID@126..129
                                                  IDENT@126..129 "ins"
                                            WHITESPACE@129..130
                                            EQ@130..131 "="
                                            WHITESPACE@131..137
                                            EXP@137..195
                                              CASE_MATCH_EXP@137..195
                                                CASE_KW@137..141 "case"
                                                WHITESPACE@141..142
                                                EXP@142..162
                                                  UNRES_INFIX_APP_EXP@142..162
                                                    EXP@142..158
                                                      AT_EXP@142..158
                                                        VID_EXP@142..158
                                                          LONG_VID@142..158
                                                            IDENT@142..148 "TextIO"
                                                            DOT@148..149 "."
                                                            IDENT@149..158 "inputLine"
                                                    WHITESPACE@158..159
                                                    EXP@159..162
                                                      AT_EXP@159..162
                                                        VID_EXP@159..162
                                                          LONG_VID@159..162
                                                            IDENT@159..162 "ins"
                                                WHITESPACE@162..163
                                                OF_KW@163..165 "of"
                                                WHITESPACE@165..174
                                                MATCH@174..195
                                                  MRULE@174..180
                                                    PAT@174..175
                                                      AT_PAT@174..175
                                                        SCON_PAT@174..175
                                                          INT@174..175 "0"
                                                    WHITESPACE@175..176
                                                    THICK_ARROW@176..178 "=>"
                                                    WHITESPACE@178..179
                                                    EXP@179..180
                                                      AT_EXP@179..180
                                                        SCON_EXP@179..180
                                                          INT@179..180 "0"
                                                  WHITESPACE@180..187
                                                  PIPE@187..188 "|"
                                                  WHITESPACE@188..189
                                                  MRULE@189..195
                                                    PAT@189..190
                                                      AT_PAT@189..190
                                                        SCON_PAT@189..190
                                                          INT@189..190 "1"
                                                    WHITESPACE@190..191
                                                    THICK_ARROW@191..193 "=>"
                                                    WHITESPACE@193..194
                                                    EXP@194..195
                                                      AT_EXP@194..195
                                                        SCON_EXP@194..195
                                                          INT@194..195 "1"
                                          WHITESPACE@195..200
                                          COMMENT@200..275 "(*\n         SOME line ..."
                                          WHITESPACE@275..278
                                IN_KW@278..280 "in"
                                WHITESPACE@280..286
                                EXP@286..320
                                  UNRES_INFIX_APP_EXP@286..320
                                    EXP@286..290
                                      AT_EXP@286..290
                                        VID_EXP@286..290
                                          LONG_VID@286..290
                                            IDENT@286..290 "loop"
                                    WHITESPACE@290..291
                                    EXP@291..294
                                      AT_EXP@291..294
                                        VID_EXP@291..294
                                          LONG_VID@291..294
                                            IDENT@291..294 "ins"
                                    WHITESPACE@294..295
                                    EXP@295..301
                                      AT_EXP@295..301
                                        VID_EXP@295..301
                                          LONG_VID@295..301
                                            IDENT@295..301 "before"
                                    WHITESPACE@301..302
                                    EXP@302..316
                                      AT_EXP@302..316
                                        VID_EXP@302..316
                                          LONG_VID@302..316
                                            IDENT@302..308 "TextIO"
                                            DOT@308..309 "."
                                            IDENT@309..316 "closeIn"
                                    WHITESPACE@316..317
                                    EXP@317..320
                                      AT_EXP@317..320
                                        VID_EXP@317..320
                                          LONG_VID@317..320
                                            IDENT@317..320 "ins"
                                WHITESPACE@320..324
                                END_KW@324..327 "end"
                        WHITESPACE@327..329
                  DEC@329..402
                    VAL_DEC@329..402
                      VAL_KW@329..332 "val"
                      WHITESPACE@332..333
                      VAL_BIND@333..402
                        PAT@333..340
                          AT_PAT@333..340
                            VID_PAT@333..340
                              LONG_VID@333..340
                                IDENT@333..340 "initial"
                        WHITESPACE@340..341
                        EQ@341..342 "="
                        WHITESPACE@342..343
                        EXP@343..402
                          UNRES_INFIX_APP_EXP@343..402
                            EXP@343..358
                              AT_EXP@343..358
                                VID_EXP@343..358
                                  LONG_VID@343..358
                                    IDENT@343..347 "List"
                                    DOT@347..348 "."
                                    IDENT@348..358 "mapPartial"
                            WHITESPACE@358..359
                            EXP@359..373
                              AT_EXP@359..373
                                VID_EXP@359..373
                                  LONG_VID@359..373
                                    IDENT@359..362 "Int"
                                    DOT@362..363 "."
                                    IDENT@363..373 "fromString"
                            WHITESPACE@373..376
                            EXP@376..402
                              AT_EXP@376..402
                                L_PAREN@376..377 "("
                                EXP@377..402
                                  UNRES_INFIX_APP_EXP@377..402
                                    EXP@377..390
                                      AT_EXP@377..390
                                        VID_EXP@377..390
                                          LONG_VID@377..390
                                            IDENT@377..383 "String"
                                            DOT@383..384 "."
                                            IDENT@384..390 "tokens"
                                    WHITESPACE@390..391
                                    EXP@391..402
                                      AT_EXP@391..402
                                        L_PAREN@391..392 "("
                                        EXP@392..401
                                          FN_EXP@392..401
                                            FN_KW@392..394 "fn"
                                            WHITESPACE@394..395
                                            MATCH@395..401
                                              MRULE@395..401
                                                PAT@395..396
                                                  AT_PAT@395..396
                                                    VID_PAT@395..396
                                                      LONG_VID@395..396
                                                        IDENT@395..396 "c"
                                                WHITESPACE@396..397
                                                THICK_ARROW@397..399 "=>"
                                                WHITESPACE@399..400
                                                EXP@400..401
                                                  AT_EXP@400..401
                                                    VID_EXP@400..401
                                                      LONG_VID@400..401
                                                        IDENT@400..401 "c"
                                        WHITESPACE@401..402
                                        ERROR@402..402 ""
                                ERROR@402..402 ""
              EQ@402..403 "="
              WHITESPACE@403..404
              CHAR@404..408 "#\",\""
              R_PAREN@408..409 ")"
              WHITESPACE@409..410
              L_PAREN@410..411 "("
              IDENT@411..413 "hd"
              WHITESPACE@413..414
              L_PAREN@414..415 "("
              IDENT@415..424 "readlines"
              WHITESPACE@424..425
              IDENT@425..430 "input"
              R_PAREN@430..431 ")"
              R_PAREN@431..432 ")"
              R_PAREN@432..433 ")"
              WHITESPACE@433..435
              DEC@435..498
                VAL_DEC@435..498
                  VAL_KW@435..438 "val"
                  WHITESPACE@438..439
                  VAL_BIND@439..498
                    PAT@439..449
                      AT_PAT@439..449
                        VID_PAT@439..449
                          LONG_VID@439..449
                            IDENT@439..449 "numAtTimer"
                    WHITESPACE@449..450
                    EQ@450..451 "="
                    WHITESPACE@451..455
                    EXP@455..498
                      AT_EXP@455..498
                        L_PAREN@455..456 "("
                        EXP@456..498
                          FN_EXP@456..498
                            FN_KW@456..458 "fn"
                            WHITESPACE@458..459
                            MATCH@459..498
                              MRULE@459..498
                                PAT@459..460
                                  AT_PAT@459..460
                                    VID_PAT@459..460
                                      LONG_VID@459..460
                                        IDENT@459..460 "t"
                                WHITESPACE@460..461
                                THICK_ARROW@461..463 "=>"
                                WHITESPACE@463..464
                                EXP@464..498
                                  AT_EXP@464..498
                                    L_PAREN@464..465 "("
                                    EXP@465..498
                                      UNRES_INFIX_APP_EXP@465..498
                                        EXP@465..471
                                          AT_EXP@465..471
                                            VID_EXP@465..471
                                              LONG_VID@465..471
                                                IDENT@465..471 "length"
                                        WHITESPACE@471..472
                                        EXP@472..473
                                          AT_EXP@472..473
                                            VID_EXP@472..473
                                              LONG_VID@472..473
                                                IDENT@472..473 "o"
                                        WHITESPACE@473..474
                                        EXP@474..498
                                          AT_EXP@474..498
                                            L_PAREN@474..475 "("
                                            EXP@475..498
                                              UNRES_INFIX_APP_EXP@475..498
                                                EXP@475..486
                                                  AT_EXP@475..486
                                                    VID_EXP@475..486
                                                      LONG_VID@475..486
                                                        IDENT@475..479 "List"
                                                        DOT@479..480 "."
                                                        IDENT@480..486 "filter"
                                                WHITESPACE@486..487
                                                EXP@487..498
                                                  AT_EXP@487..498
                                                    L_PAREN@487..488 "("
                                                    EXP@488..497
                                                      FN_EXP@488..497
                                                        FN_KW@488..490 "fn"
                                                        WHITESPACE@490..491
                                                        MATCH@491..497
                                                          MRULE@491..497
                                                            PAT@491..492
                                                              AT_PAT@491..492
                                                                VID_PAT@491..492
                                                                  LONG_VID@491..492
                                                                    IDENT@491..492 "x"
                                                            WHITESPACE@492..493
                                                            THICK_ARROW@493..495 "=>"
                                                            WHITESPACE@495..496
                                                            EXP@496..497
                                                              AT_EXP@496..497
                                                                VID_EXP@496..497
                                                                  LONG_VID@496..497
                                                                    IDENT@496..497 "x"
                                                    WHITESPACE@497..498
                                                    ERROR@498..498 ""
                                            ERROR@498..498 ""
                                    ERROR@498..498 ""
                        ERROR@498..498 ""
              EQ@498..499 "="
              WHITESPACE@499..500
              IDENT@500..501 "t"
              R_PAREN@501..502 ")"
              R_PAREN@502..503 ")"
              R_PAREN@503..504 ")"
              WHITESPACE@504..505
              IDENT@505..512 "initial"
              R_PAREN@512..513 ")"
              WHITESPACE@513..515
              DEC@515..874
                SEQ_DEC@515..874
                  DEC@515..564
                    VAL_DEC@515..564
                      VAL_KW@515..518 "val"
                      WHITESPACE@518..519
                      VAL_BIND@519..564
                        PAT@519..533
                          AT_PAT@519..533
                            VID_PAT@519..533
                              LONG_VID@519..533
                                IDENT@519..533 "initial_timers"
                        WHITESPACE@533..534
                        EQ@534..535 "="
                        WHITESPACE@535..536
                        EXP@536..564
                          UNRES_INFIX_APP_EXP@536..564
                            EXP@536..549
                              AT_EXP@536..549
                                VID_EXP@536..549
                                  LONG_VID@536..549
                                    IDENT@536..540 "List"
                                    DOT@540..541 "."
                                    IDENT@541..549 "tabulate"
                            EXP@549..564
                              AT_EXP@549..564
                                TUPLE_EXP@549..564
                                  L_PAREN@549..550 "("
                                  EXP@550..551
                                    AT_EXP@550..551
                                      SCON_EXP@550..551
                                        INT@550..551 "9"
                                  COMMA@551..552 ","
                                  WHITESPACE@552..553
                                  EXP@553..563
                                    AT_EXP@553..563
                                      VID_EXP@553..563
                                        LONG_VID@553..563
                                          IDENT@553..563 "numAtTimer"
                                  R_PAREN@563..564 ")"
                  WHITESPACE@564..567
                  DEC@567..787
                    FUN_DEC@567..787
                      FUN_KW@567..570 "fun"
                      WHITESPACE@570..571
                      TY_VAR_SEQ@571..571
                      FVAL_BIND@571..787
                        FVAL_BIND_ROW@571..785
                          VID@571..578
                            IDENT@571..578 "advance"
                          WHITESPACE@578..579
                          AT_PAT@579..585
                            VID_PAT@579..585
                              LONG_VID@579..585
                                IDENT@579..585 "timers"
                          WHITESPACE@585..586
                          EQ@586..587 "="
                          WHITESPACE@587..590
                          EXP@590..785
                            AT_EXP@590..785
                              LET_DEC@590..785
                                LET_KW@590..593 "let"
                                WHITESPACE@593..599
                                DEC@599..749
                                  SEQ_DEC@599..749
                                    DEC@599..632
                                      VAL_DEC@599..632
                                        VAL_KW@599..602 "val"
                                        WHITESPACE@602..603
                                        VAL_BIND@603..632
                                          PAT@603..610
                                            AT_PAT@603..610
                                              VID_PAT@603..610
                                                LONG_VID@603..610
                                                  IDENT@603..610 "at_zero"
                                          WHITESPACE@610..611
                                          EQ@611..612 "="
                                          WHITESPACE@612..613
                                          EXP@613..632
                                            UNRES_INFIX_APP_EXP@613..632
                                              EXP@613..621
                                                AT_EXP@613..621
                                                  VID_EXP@613..621
                                                    LONG_VID@613..621
                                                      IDENT@613..617 "List"
                                                      DOT@617..618 "."
                                                      IDENT@618..621 "nth"
                                              EXP@621..632
                                                AT_EXP@621..632
                                                  TUPLE_EXP@621..632
                                                    L_PAREN@621..622 "("
                                                    EXP@622..628
                                                      AT_EXP@622..628
                                                        VID_EXP@622..628
                                                          LONG_VID@622..628
                                                            IDENT@622..628 "timers"
                                                    COMMA@628..629 ","
                                                    WHITESPACE@629..630
                                                    EXP@630..631
                                                      AT_EXP@630..631
                                                        SCON_EXP@630..631
                                                          INT@630..631 "0"
                                                    R_PAREN@631..632 ")"
                                    WHITESPACE@632..637
                                    DEC@637..749
                                      FUN_DEC@637..749
                                        FUN_KW@637..640 "fun"
                                        WHITESPACE@640..641
                                        TY_VAR_SEQ@641..641
                                        FVAL_BIND@641..749
                                          FVAL_BIND_ROW@641..657
                                            VID@641..645
                                              IDENT@641..645 "next"
                                            WHITESPACE@645..646
                                            AT_PAT@646..647
                                              SCON_PAT@646..647
                                                INT@646..647 "8"
                                            WHITESPACE@647..648
                                            EQ@648..649 "="
                                            WHITESPACE@649..650
                                            EXP@650..657
                                              AT_EXP@650..657
                                                VID_EXP@650..657
                                                  LONG_VID@650..657
                                                    IDENT@650..657 "at_zero"
                                          WHITESPACE@657..665
                                          FVAL_BIND_ROW@665..705
                                            PIPE@665..666 "|"
                                            WHITESPACE@666..667
                                            VID@667..671
                                              IDENT@667..671 "next"
                                            WHITESPACE@671..672
                                            AT_PAT@672..673
                                              SCON_PAT@672..673
                                                INT@672..673 "6"
                                            WHITESPACE@673..674
                                            EQ@674..675 "="
                                            WHITESPACE@675..676
                                            EXP@676..705
                                              UNRES_INFIX_APP_EXP@676..705
                                                EXP@676..683
                                                  AT_EXP@676..683
                                                    VID_EXP@676..683
                                                      LONG_VID@676..683
                                                        IDENT@676..683 "at_zero"
                                                WHITESPACE@683..684
                                                EXP@684..685
                                                  AT_EXP@684..685
                                                    VID_EXP@684..685
                                                      LONG_VID@684..685
                                                        IDENT@684..685 "+"
                                                WHITESPACE@685..686
                                                EXP@686..694
                                                  AT_EXP@686..694
                                                    VID_EXP@686..694
                                                      LONG_VID@686..694
                                                        IDENT@686..690 "List"
                                                        DOT@690..691 "."
                                                        IDENT@691..694 "nth"
                                                EXP@694..705
                                                  AT_EXP@694..705
                                                    TUPLE_EXP@694..705
                                                      L_PAREN@694..695 "("
                                                      EXP@695..701
                                                        AT_EXP@695..701
                                                          VID_EXP@695..701
                                                            LONG_VID@695..701
                                                              IDENT@695..701 "timers"
                                                      COMMA@701..702 ","
                                                      WHITESPACE@702..703
                                                      EXP@703..704
                                                        AT_EXP@703..704
                                                          SCON_EXP@703..704
                                                            INT@703..704 "7"
                                                      R_PAREN@704..705 ")"
                                          WHITESPACE@705..712
                                          FVAL_BIND_ROW@712..746
                                            PIPE@712..713 "|"
                                            WHITESPACE@713..714
                                            VID@714..718
                                              IDENT@714..718 "next"
                                            WHITESPACE@718..719
                                            AT_PAT@719..720
                                              VID_PAT@719..720
                                                LONG_VID@719..720
                                                  IDENT@719..720 "n"
                                            WHITESPACE@720..721
                                            EQ@721..722 "="
                                            WHITESPACE@722..723
                                            EXP@723..746
                                              UNRES_INFIX_APP_EXP@723..746
                                                EXP@723..731
                                                  AT_EXP@723..731
                                                    VID_EXP@723..731
                                                      LONG_VID@723..731
                                                        IDENT@723..727 "List"
                                                        DOT@727..728 "."
                                                        IDENT@728..731 "nth"
                                                EXP@731..746
                                                  AT_EXP@731..746
                                                    TUPLE_EXP@731..746
                                                      L_PAREN@731..732 "("
                                                      EXP@732..738
                                                        AT_EXP@732..738
                                                          VID_EXP@732..738
                                                            LONG_VID@732..738
                                                              IDENT@732..738 "timers"
                                                      COMMA@738..739 ","
                                                      WHITESPACE@739..740
                                                      EXP@740..745
                                                        UNRES_INFIX_APP_EXP@740..745
                                                          EXP@740..741
                                                            AT_EXP@740..741
                                                              VID_EXP@740..741
                                                                LONG_VID@740..741
                                                                  IDENT@740..741 "n"
                                                          WHITESPACE@741..742
                                                          EXP@742..743
                                                            AT_EXP@742..743
                                                              VID_EXP@742..743
                                                                LONG_VID@742..743
                                                                  IDENT@742..743 "+"
                                                          WHITESPACE@743..744
                                                          EXP@744..745
                                                            AT_EXP@744..745
                                                              SCON_EXP@744..745
                                                                INT@744..745 "1"
                                                      R_PAREN@745..746 ")"
                                          WHITESPACE@746..749
                                IN_KW@749..751 "in"
                                WHITESPACE@751..757
                                EXP@757..779
                                  UNRES_INFIX_APP_EXP@757..779
                                    EXP@757..770
                                      AT_EXP@757..770
                                        VID_EXP@757..770
                                          LONG_VID@757..770
                                            IDENT@757..761 "List"
                                            DOT@761..762 "."
                                            IDENT@762..770 "tabulate"
                                    EXP@770..779
                                      AT_EXP@770..779
                                        TUPLE_EXP@770..779
                                          L_PAREN@770..771 "("
                                          EXP@771..772
                                            AT_EXP@771..772
                                              SCON_EXP@771..772
                                                INT@771..772 "9"
                                          COMMA@772..773 ","
                                          WHITESPACE@773..774
                                          EXP@774..778
                                            AT_EXP@774..778
                                              VID_EXP@774..778
                                                LONG_VID@774..778
                                                  IDENT@774..778 "next"
                                          R_PAREN@778..779 ")"
                                WHITESPACE@779..782
                                END_KW@782..785 "end"
                        WHITESPACE@785..787
                  DEC@787..874
                    FUN_DEC@787..874
                      FUN_KW@787..790 "fun"
                      WHITESPACE@790..791
                      TY_VAR_SEQ@791..791
                      FVAL_BIND@791..874
                        FVAL_BIND_ROW@791..874
                          VID@791..799
                            IDENT@791..799 "simulate"
                          WHITESPACE@799..800
                          AT_PAT@800..808
                            VID_PAT@800..808
                              LONG_VID@800..808
                                IDENT@800..808 "num_days"
                          WHITESPACE@808..809
                          AT_PAT@809..823
                            VID_PAT@809..823
                              LONG_VID@809..823
                                IDENT@809..823 "initial_timers"
                          WHITESPACE@823..824
                          EQ@824..825 "="
                          WHITESPACE@825..829
                          EXP@829..874
                            AT_EXP@829..874
                              LET_DEC@829..874
                                LET_KW@829..832 "let"
                                WHITESPACE@832..838
                                DEC@838..874
                                  FUN_DEC@838..874
                                    FUN_KW@838..841 "fun"
                                    WHITESPACE@841..842
                                    TY_VAR_SEQ@842..842
                                    FVAL_BIND@842..874
                                      FVAL_BIND_ROW@842..874
                                        VID@842..846
                                          IDENT@842..846 "loop"
                                        WHITESPACE@846..847
                                        AT_PAT@847..852
                                          VID_PAT@847..852
                                            LONG_VID@847..852
                                              IDENT@847..852 "count"
                                        WHITESPACE@852..853
                                        AT_PAT@853..855
                                          VID_PAT@853..855
                                            LONG_VID@853..855
                                              IDENT@853..855 "ts"
                                        WHITESPACE@855..856
                                        EQ@856..857 "="
                                        WHITESPACE@857..865
                                        EXP@865..874
                                          IF_EXP@865..874
                                            IF_KW@865..867 "if"
                                            WHITESPACE@867..868
                                            EXP@868..873
                                              AT_EXP@868..873
                                                VID_EXP@868..873
                                                  LONG_VID@868..873
                                                    IDENT@868..873 "count"
                                            WHITESPACE@873..874
                                            ERROR@874..874 ""
                                            EXP@874..874
                                              AT_EXP@874..874
                                                ERROR@874..874 ""
                                            ERROR@874..874 ""
                                            EXP@874..874
                                              AT_EXP@874..874
                                                ERROR@874..874 ""
                                ERROR@874..874 ""
                                EXP@874..874
                                  AT_EXP@874..874
                                    ERROR@874..874 ""
                                ERROR@874..874 ""
              EQ@874..875 "="
              WHITESPACE@875..876
              IDENT@876..884 "num_days"
              WHITESPACE@884..885
              THEN_KW@885..889 "then"
              EXP@889..900
                UNRES_INFIX_APP_EXP@889..900
                  EXP@889..889
                    AT_EXP@889..889
                      ERROR@889..889 ""
                  WHITESPACE@889..898
                  EXP@898..900
                    AT_EXP@898..900
                      VID_EXP@898..900
                        LONG_VID@898..900
                          IDENT@898..900 "ts"
              WHITESPACE@900..908
              ELSE_KW@908..912 "else"
              EXP@912..950
                UNRES_INFIX_APP_EXP@912..950
                  EXP@912..912
                    AT_EXP@912..912
                      ERROR@912..912 ""
                  WHITESPACE@912..921
                  EXP@921..925
                    AT_EXP@921..925
                      VID_EXP@921..925
                        LONG_VID@921..925
                          IDENT@921..925 "loop"
                  WHITESPACE@925..926
                  EXP@926..937
                    AT_EXP@926..937
                      L_PAREN@926..927 "("
                      EXP@927..936
                        UNRES_INFIX_APP_EXP@927..936
                          EXP@927..932
                            AT_EXP@927..932
                              VID_EXP@927..932
                                LONG_VID@927..932
                                  IDENT@927..932 "count"
                          WHITESPACE@932..933
                          EXP@933..934
                            AT_EXP@933..934
                              VID_EXP@933..934
                                LONG_VID@933..934
                                  IDENT@933..934 "+"
                          WHITESPACE@934..935
                          EXP@935..936
                            AT_EXP@935..936
                              SCON_EXP@935..936
                                INT@935..936 "1"
                      R_PAREN@936..937 ")"
                  WHITESPACE@937..938
                  EXP@938..950
                    AT_EXP@938..950
                      L_PAREN@938..939 "("
                      EXP@939..949
                        UNRES_INFIX_APP_EXP@939..949
                          EXP@939..946
                            AT_EXP@939..946
                              VID_EXP@939..946
                                LONG_VID@939..946
                                  IDENT@939..946 "advance"
                          WHITESPACE@946..947
                          EXP@947..949
                            AT_EXP@947..949
                              VID_EXP@947..949
                                LONG_VID@947..949
                                  IDENT@947..949 "ts"
                      R_PAREN@949..950 ")"
              WHITESPACE@950..953
              IN_KW@953..955 "in"
              WHITESPACE@955..961
              IDENT@961..966 "foldl"
              WHITESPACE@966..967
              L_PAREN@967..968 "("
              OP_KW@968..970 "op"
              WHITESPACE@970..971
              IDENT@971..972 "+"
              R_PAREN@972..973 ")"
              WHITESPACE@973..974
              INT@974..975 "0"
              WHITESPACE@975..976
              L_PAREN@976..977 "("
              IDENT@977..981 "loop"
              WHITESPACE@981..982
              INT@982..983 "0"
              WHITESPACE@983..984
              IDENT@984..998 "initial_timers"
              R_PAREN@998..999 ")"
              WHITESPACE@999..1002
              END_KW@1002..1005 "end"
              WHITESPACE@1005..1007
              DEC@1007..1091
                SEQ_DEC@1007..1091
                  DEC@1007..1048
                    VAL_DEC@1007..1048
                      VAL_KW@1007..1010 "val"
                      WHITESPACE@1010..1011
                      VAL_BIND@1011..1048
                        PAT@1011..1019
                          AT_PAT@1011..1019
                            VID_PAT@1011..1019
                              LONG_VID@1011..1019
                                IDENT@1011..1019 "part_one"
                        WHITESPACE@1019..1020
                        EQ@1020..1021 "="
                        WHITESPACE@1021..1022
                        EXP@1022..1048
                          UNRES_INFIX_APP_EXP@1022..1048
                            EXP@1022..1030
                              AT_EXP@1022..1030
                                VID_EXP@1022..1030
                                  LONG_VID@1022..1030
                                    IDENT@1022..1030 "simulate"
                            WHITESPACE@1030..1031
                            EXP@1031..1033
                              AT_EXP@1031..1033
                                SCON_EXP@1031..1033
                                  INT@1031..1033 "80"
                            WHITESPACE@1033..1034
                            EXP@1034..1048
                              AT_EXP@1034..1048
                                VID_EXP@1034..1048
                                  LONG_VID@1034..1048
                                    IDENT@1034..1048 "initial_timers"
                  WHITESPACE@1048..1049
                  DEC@1049..1091
                    VAL_DEC@1049..1091
                      VAL_KW@1049..1052 "val"
                      WHITESPACE@1052..1053
                      VAL_BIND@1053..1091
                        PAT@1053..1061
                          AT_PAT@1053..1061
                            VID_PAT@1053..1061
                              LONG_VID@1053..1061
                                IDENT@1053..1061 "part_two"
                        WHITESPACE@1061..1062
                        EQ@1062..1063 "="
                        WHITESPACE@1063..1064
                        EXP@1064..1091
                          UNRES_INFIX_APP_EXP@1064..1091
                            EXP@1064..1072
                              AT_EXP@1064..1072
                                VID_EXP@1064..1072
                                  LONG_VID@1064..1072
                                    IDENT@1064..1072 "simulate"
                            WHITESPACE@1072..1073
                            EXP@1073..1076
                              AT_EXP@1073..1076
                                SCON_EXP@1073..1076
                                  INT@1073..1076 "256"
                            WHITESPACE@1076..1077
                            EXP@1077..1091
                              AT_EXP@1077..1091
                                VID_EXP@1077..1091
                                  LONG_VID@1077..1091
                                    IDENT@1077..1091 "initial_timers"
              WHITESPACE@1091..1092
        "##]],
    )
}
