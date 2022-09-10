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
fn aocprogram() {
    let src = r#"
val input = "../../input/day6/input.txt"

fun readlines (infile : string) = let
  val ins = TextIO.openIn infile 
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
            FILE@0..167
              WHITESPACE@0..1
              DEC@1..167
                SEQ_DEC@1..167
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
                  DEC@43..167
                    FUN_DEC@43..167
                      FUN_KW@43..46 "fun"
                      WHITESPACE@46..47
                      TY_VAR_SEQ@47..47
                      FVAL_BIND@47..167
                        FVAL_BIND_ROW@47..167
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
                          EXP@77..166
                            AT_EXP@77..166
                              LET_DEC@77..166
                                LET_KW@77..80 "let"
                                WHITESPACE@80..83
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
                                IN_KW@117..119 "in"
                                WHITESPACE@119..125
                                EXP@125..159
                                  UNRES_INFIX_APP_EXP@125..159
                                    EXP@125..129
                                      AT_EXP@125..129
                                        VID_EXP@125..129
                                          LONG_VID@125..129
                                            IDENT@125..129 "loop"
                                    WHITESPACE@129..130
                                    EXP@130..133
                                      AT_EXP@130..133
                                        VID_EXP@130..133
                                          LONG_VID@130..133
                                            IDENT@130..133 "ins"
                                    WHITESPACE@133..134
                                    EXP@134..140
                                      AT_EXP@134..140
                                        VID_EXP@134..140
                                          LONG_VID@134..140
                                            IDENT@134..140 "before"
                                    WHITESPACE@140..141
                                    EXP@141..155
                                      AT_EXP@141..155
                                        VID_EXP@141..155
                                          LONG_VID@141..155
                                            IDENT@141..147 "TextIO"
                                            DOT@147..148 "."
                                            IDENT@148..155 "closeIn"
                                    WHITESPACE@155..156
                                    EXP@156..159
                                      AT_EXP@156..159
                                        VID_EXP@156..159
                                          LONG_VID@156..159
                                            IDENT@156..159 "ins"
                                WHITESPACE@159..163
                                END_KW@163..166 "end"
                          WHITESPACE@166..167
        "#]],
    )
}
