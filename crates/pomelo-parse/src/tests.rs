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
"#;
    let _src = r#"val input = "../../input/day6/input.txt"

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
        expect![[r#"
            FILE@0..284
              WHITESPACE@0..1
              DEC@1..226
                SEQ_DEC@1..226
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
                  DEC@43..226
                    FUN_DEC@43..226
                      FUN_KW@43..46 "fun"
                      WHITESPACE@46..47
                      TY_VAR_SEQ@47..47
                      FVAL_BIND@47..226
                        FVAL_BIND_ROW@47..226
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
                          EXP@77..226
                            AT_EXP@77..226
                              LET_DEC@77..226
                                LET_KW@77..80 "let"
                                WHITESPACE@80..83
                                DEC@83..226
                                  SEQ_DEC@83..226
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
                                    DEC@117..226
                                      FUN_DEC@117..226
                                        FUN_KW@117..120 "fun"
                                        WHITESPACE@120..121
                                        TY_VAR_SEQ@121..121
                                        FVAL_BIND@121..226
                                          FVAL_BIND_ROW@121..206
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
                                            EXP@137..206
                                              TY_EXP@137..206
                                                EXP@137..194
                                                  CASE_MATCH_EXP@137..194
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
                                                    WHITESPACE@165..175
                                                    MATCH@175..194
                                                      MRULE@175..194
                                                        PAT@175..184
                                                          UNRES_INFIX_APP_PAT@175..184
                                                            PAT@175..179
                                                              AT_PAT@175..179
                                                                VID_PAT@175..179
                                                                  LONG_VID@175..179
                                                                    IDENT@175..179 "SOME"
                                                            WHITESPACE@179..180
                                                            PAT@180..184
                                                              AT_PAT@180..184
                                                                VID_PAT@180..184
                                                                  LONG_VID@180..184
                                                                    IDENT@180..184 "line"
                                                        WHITESPACE@184..185
                                                        THICK_ARROW@185..187 "=>"
                                                        WHITESPACE@187..188
                                                        EXP@188..194
                                                          TY_EXP@188..194
                                                            EXP@188..192
                                                              AT_EXP@188..192
                                                                VID_EXP@188..192
                                                                  LONG_VID@188..192
                                                                    IDENT@188..192 "line"
                                                            WHITESPACE@192..193
                                                            COLON@193..194 ":"
                                                COLON@194..195 ":"
                                                WHITESPACE@195..196
                                                TY@196..206
                                                  L_PAREN@196..197 "("
                                                  TY@197..205
                                                    TY_CON_EXP@197..205
                                                      TY@197..201
                                                        LONG_TY_CON@197..201
                                                          IDENT@197..201 "loop"
                                                      WHITESPACE@201..202
                                                      TY@202..205
                                                        LONG_TY_CON@202..205
                                                          IDENT@202..205 "ins"
                                                  R_PAREN@205..206 ")"
                                          WHITESPACE@206..214
                                          FVAL_BIND_ROW@214..226
                                            PIPE@214..215 "|"
                                            WHITESPACE@215..216
                                            VID@216..220
                                              IDENT@216..220 "NONE"
                                            WHITESPACE@220..226
                                            AT_PAT@226..226
                                              ERROR@226..226 ""
                                            ERROR@226..226 ""
                                            EXP@226..226
                                              AT_EXP@226..226
                                                ERROR@226..226 ""
                                ERROR@226..226 ""
                                EXP@226..226
                                  AT_EXP@226..226
                                    ERROR@226..226 ""
                                ERROR@226..226 ""
              THICK_ARROW@226..228 "=>"
              WHITESPACE@228..229
              L_BRACKET@229..230 "["
              R_BRACKET@230..231 "]"
              WHITESPACE@231..234
              IN_KW@234..236 "in"
              WHITESPACE@236..242
              IDENT@242..246 "loop"
              WHITESPACE@246..247
              IDENT@247..250 "ins"
              WHITESPACE@250..251
              IDENT@251..257 "before"
              WHITESPACE@257..258
              IDENT@258..264 "TextIO"
              DOT@264..265 "."
              IDENT@265..272 "closeIn"
              WHITESPACE@272..273
              IDENT@273..276 "ins"
              WHITESPACE@276..280
              END_KW@280..283 "end"
              WHITESPACE@283..284
        "#]],
    )
}
