(* Copyright (c) 1998-2007 INRIA *)
(* Copyright (c) 2018-2019 Geneanet *)

let nbc c =
  if Char.code c < 0b10000000 then 1
  else if Char.code c < 0b11000000 then -1
  else if Char.code c < 0b11100000 then 2
  else if Char.code c < 0b11110000 then 3
  else if Char.code c < 0b11111000 then 4
  else if Char.code c < 0b11111100 then 5
  else if Char.code c < 0b11111110 then 6
  else -1

let decode fns fnc unsupported s i len =
  let c = String.unsafe_get s i in
  let nbc = nbc c in
  if nbc < 0 || i + nbc > len
  then (fnc c ; i + 1)
  else
    let () =
      match Char.code c with

      (* A..Z *)
      |0x41|0x42|0x43|0x44|0x45|0x46|0x47|0x48|0x49|0x4A|0x4B|0x4C|0x4D|0x4E|0x4F
      |0x50|0x51|0x52|0x53|0x54|0x55|0x56|0x57|0x58|0x59|0x5A

      (* a..z *)
      |0x61|0x62|0x63|0x64|0x65|0x66|0x67|0x68|0x69|0x6A|0x6B|0x6C|0x6D|0x6E|0x6F
      |0x70|0x71|0x72|0x73|0x74|0x75|0x76|0x77|0x78|0x79|0x7A

      (* 0..9 *)
      |0x30|0x31|0x32|0x33|0x34|0x35|0x36|0x37|0x38|0x39
      as c -> fnc (Char.unsafe_chr c)

      | 0xC2 -> unsupported s i nbc
      | 0xC3 ->
        begin match Char.code @@ String.unsafe_get s (i+1) with
          | 0x80 | 0x81 | 0x82 | 0x83 | 0x84 | 0x85 -> fnc 'A'
          | 0x86 -> fns "AE" 0 2
          | 0x87 -> fnc 'C'
          | 0x88 | 0x89 | 0x8A | 0x8B -> fnc 'E'
          | 0x8C | 0x8D | 0x8E | 0x8F -> fnc 'I'
          | 0x90 -> fnc 'D'
          | 0x91 -> fnc 'N'
          | 0x92 | 0x93 | 0x94 | 0x95 | 0x96 | 0x98 -> fnc 'O'
          | 0x99 | 0x9A | 0x9B | 0x9C -> fnc 'U'
          | 0x9D -> fnc 'Y'
          | 0x9E -> fns "TH" 0 2
          | 0x9F -> fns "sz" 0 2
          | 0xA0 | 0xA1 | 0xA2 | 0xA3 | 0xA4 | 0xA5 -> fnc 'a'
          | 0xA6 -> fns "ae" 0 2
          | 0xA7 -> fnc 'c'
          | 0xA8 | 0xA9 | 0xAA | 0xAB -> fnc 'e'
          | 0xAC | 0xAD | 0xAE | 0xAF -> fnc 'i'
          | 0xB0 -> fnc 'd'
          | 0xB1 -> fnc 'n'
          | 0xB2 | 0xB3 | 0xB4 | 0xB5 | 0xB6 | 0xB8 -> fnc 'o'
          | 0xB9 | 0xBA | 0xBB | 0xBC -> fnc 'u'
          | 0xBD | 0xBF -> fnc 'y'
          | 0xBE -> fns "th" 0 2
          | _ -> unsupported s i nbc
        end

      | 0xC4 ->
        begin match Char.code @@ String.unsafe_get s (i+1) with
          | 0x80 | 0x82 | 0x84 -> fnc 'A'
          | 0x81 | 0x83 | 0x85 -> fnc 'a'
          | 0x86 | 0x88 | 0x8A | 0x8C -> fnc 'C'
          | 0x87 | 0x89 | 0x8B | 0x8D -> fnc 'c'
          | 0x8E | 0x90 -> fnc 'D'
          | 0x8F | 0x91 -> fnc 'd'
          | 0x92 | 0x94 | 0x96 | 0x98 | 0x9A -> fnc 'E'
          | 0x93 | 0x95 | 0x97 | 0x99 | 0x9B -> fnc 'e'
          | 0x9C | 0x9E | 0xA0 | 0xA2 -> fnc 'G'
          | 0x9D | 0x9F | 0xA1 | 0xA3 -> fnc 'g'
          | 0xA4 | 0xA6 -> fnc 'H'
          | 0xA5 | 0xA7 -> fnc 'h'
          | 0xA8 | 0xAA | 0xAC | 0xAE | 0xB0 -> fnc 'I'
          | 0xA9 | 0xAB | 0xAD | 0xAF | 0xB1 -> fnc 'i'
          | 0xB2 -> fns "IJ" 0 2
          | 0xB3 -> fns "ij" 0 2
          | 0xB4 -> fnc 'J'
          | 0xB5 -> fnc 'j'
          | 0xB6 -> fnc 'K'
          | 0xB7 | 0xB8 -> fnc 'k'
          | 0xB9 | 0xBB | 0xBD | 0xBF -> fnc 'L'
          | 0xBA | 0xBC | 0xBE -> fnc 'l'
          | _ -> unsupported s i nbc
        end

      | 0xC5 ->
        begin match Char.code @@ String.unsafe_get s (i+1) with
          | 0x80 | 0x82 -> fnc 'l'
          | 0x81 -> fnc 'L'
          | 0x83 | 0x85 | 0x87 | 0x8A -> fnc 'N'
          | 0x84 | 0x86 | 0x88 | 0x89 | 0x8B -> fnc 'n'
          | 0x8C | 0x8E | 0x90 -> fnc 'O'
          | 0x8D | 0x8F | 0x91 -> fnc 'o'
          | 0x92 -> fns "OE" 0 2
          | 0x93 -> fns "oe" 0 2
          | 0x94 | 0x96 | 0x98 -> fnc 'R'
          | 0x95 | 0x97 | 0x99 -> fnc 'r'
          | 0x9A | 0x9C | 0x9E | 0xA0 -> fnc 'S'
          | 0x9B | 0x9D | 0x9F | 0xA1 -> fnc 's'
          | 0xA2 | 0xA4 | 0xA6 -> fnc 'T'
          | 0xA3 | 0xA5 | 0xA7 -> fnc 't'
          | 0xA8 | 0xAA | 0xAC | 0xAE | 0xB0 | 0xB2 -> fnc 'U'
          | 0xA9 | 0xAB | 0xAD | 0xAF | 0xB1 | 0xB3 -> fnc 'u'
          | 0xB4 -> fnc 'W'
          | 0xB5 -> fnc 'w'
          | 0xB6 | 0xB8 -> fnc 'Y'
          | 0xB7 -> fnc 'y'
          | 0xB9 | 0xBB | 0xBD -> fnc 'Z'
          | 0xBA | 0xBC | 0xBE -> fnc 'z'
          | _ -> unsupported s i nbc
        end

      | 0xC6 ->
        begin match Char.code @@ String.unsafe_get s (i+1) with
          | 0x86 | 0x9F | 0xA0 -> fnc 'O'
          | 0x90 -> fnc 'E'
          | 0x96 | 0x97 -> fnc 'I'
          | 0xA1 -> fnc 'o'
          | 0xAF | 0xB1 -> fnc 'U'
          | 0xB0 -> fnc 'u'
          | 0xB3 -> fnc 'Y'
          | 0xB4 -> fnc 'y'
          | _ -> unsupported s i nbc
        end

      | 0xC7 ->
        begin match Char.code @@ String.unsafe_get s (i+1) with
          | 0x8D | 0x9E | 0xA0 | 0xBA -> fnc 'A'
          | 0x8E | 0x9F | 0xA1 | 0xBB -> fnc 'a'
          | 0x8F -> fnc 'I'
          | 0x90 -> fnc 'i'
          | 0x91 | 0xAA | 0xAC | 0xBE -> fnc 'O'
          | 0x92 | 0xAB | 0xAD | 0xBF -> fnc 'o'
          | 0x93 | 0x95 | 0x97 | 0x99 | 0x9B -> fnc 'U'
          | 0x94 | 0x96 | 0x98 | 0x9A | 0x9C -> fnc 'u'
          | 0xBC | 0xA2 -> fns "AE" 0 2
          | 0xBD | 0xA3 -> fns "ae" 0 2
          | _ -> unsupported s i nbc
        end

      | 0xC8 ->
        begin match Char.code @@ String.unsafe_get s (i+1) with
          | 0x80 | 0x82 | 0xA6 | 0xBA -> fnc 'A'
          | 0x81 | 0x83 | 0xA7 -> fnc 'a'
          | 0x84 | 0x86 | 0xA8 -> fnc 'E'
          | 0x85 | 0x87 | 0xA9 -> fnc 'e'
          | 0x88 | 0x8A -> fnc 'I'
          | 0x89 | 0x8B -> fnc 'i'
          | 0x8C | 0x8E | 0xAA | 0xAC | 0xAE | 0xB0 -> fnc 'O'
          | 0x8D | 0x8F | 0xAB | 0xAD | 0xAF | 0xB1 -> fnc 'o'
          | 0x94 | 0x96 -> fnc 'U'
          | 0x95 | 0x97 -> fnc 'u'
          | 0xA2 -> fns "OU" 0 2
          | 0xA3 -> fns "ou" 0 2
          | 0xB2 -> fnc 'Y'
          | 0xB3 -> fnc 'y'
          | _ -> unsupported s i nbc
        end

      | 0xC9 ->
        begin match Char.code @@ String.unsafe_get s (i+1) with
          | 0x84 -> fnc 'U'
          | 0x86 -> fnc 'E'
          | 0x87 | 0x9B -> fnc 'e'
          | 0x8E -> fnc 'Y'
          | 0x8F -> fnc 'y'
          | 0x94 -> fnc 'o'
          | 0xA8 | 0xAE -> fnc 'i'
          | _ -> unsupported s i nbc
        end

      | 0xCA ->
        begin match Char.code @@ String.unsafe_get s (i+1) with
          | 0x84 -> fnc 'u'
          | _ -> unsupported s i nbc
        end

      | 0xCE ->
        (* Greek *)
        begin match Char.code @@ String.unsafe_get s (i+1) with
          | 0x91 -> fnc 'A'
          | 0x92 -> fnc 'B'
          | 0x93 -> fnc 'G'
          | 0x94 -> fnc 'D'
          | 0x95 -> fnc 'E'
          | 0x96 -> fns "DZ" 0 2
          | 0x97 -> fnc 'E'
          | 0x98 -> fns "TH" 0 2
          | 0x99 -> fnc 'I'
          | 0x9A -> fnc 'K'
          | 0x9B -> fnc 'L'
          | 0x9C -> fnc 'M'
          | 0x9D -> fnc 'N'
          | 0x9E -> fnc 'X'
          | 0x9F -> fnc 'O'
          | 0xA0 -> fnc 'P'
          | 0xA1 -> fnc 'R'
          | 0xA2 | 0xA3 -> fnc 'S'
          | 0xA4 -> fnc 'T'
          | 0xA5 -> fnc 'U'
          | 0xA6 -> fns "PH" 0 2
          | 0xA7 -> fns "KH" 0 2
          | 0xA8 -> fns "PS" 0 2
          | 0xA9 -> fnc 'O'
          | 0xB1 -> fnc 'a'
          | 0xB2 -> fnc 'b'
          | 0xB3 -> fnc 'g'
          | 0xB4 -> fnc 'd'
          | 0xB5 -> fnc 'e'
          | 0xB6 -> fns "dz" 0 2
          | 0xB7 -> fnc 'e'
          | 0xB8 -> fns "th" 0 2
          | 0xB9 -> fnc 'i'
          | 0xBA -> fnc 'k'
          | 0xBB -> fnc 'l'
          | 0xBC -> fnc 'm'
          | 0xBD -> fnc 'n'
          | 0xBE -> fnc 'x'
          | 0xBF -> fnc 'o'
          | _ -> unsupported s i nbc
        end

      | 0xCF ->
        begin match Char.code @@ String.unsafe_get s (i+1) with
          | 0x80 -> fnc 'p'
          | 0x81 -> fnc 'r'
          | 0x82 | 0x83 -> fnc 's'
          | 0x84 -> fnc 't'
          | 0x85 -> fnc 'u'
          | 0x86 -> fns "ph" 0 2
          | 0x87 -> fns "kh" 0 2
          | 0x88 -> fns "ps" 0 2
          | 0x89 -> fnc 'o'
          | _ -> unsupported s i nbc
        end

      | 0xD0 ->
        (* Cyrillic *)
        begin match Char.code @@ String.unsafe_get s (i+1) with
          | 0x81 -> fnc 'E'
          | 0x86 -> fnc 'I'
          | 0x90 -> fnc 'A'
          | 0x91 -> fnc 'B'
          | 0x92 -> fnc 'V'
          | 0x93 -> fnc 'G'
          | 0x94 -> fnc 'D'
          | 0x95 -> fnc 'E'
          | 0x96 -> fnc 'J'
          | 0x97 -> fnc 'Z'
          | 0x98 | 0x99 -> fnc 'I'
          | 0x9A -> fnc 'K'
          | 0x9B -> fnc 'L'
          | 0x9C -> fnc 'M'
          | 0x9D -> fnc 'N'
          | 0x9E -> fnc 'O'
          | 0x9F -> fnc 'P'
          | 0xA0 -> fnc 'R'
          | 0xA1 -> fnc 'S'
          | 0xA2 -> fnc 'T'
          | 0xA3 -> fns "OU" 0 2
          | 0xA4 -> fnc 'F'
          | 0xA5 -> fns "KH" 0 2
          | 0xA6 -> fns "TS" 0 2
          | 0xA7 -> fns "TCH" 0 2
          | 0xA8 -> fns "CH" 0 2
          | 0xA9 -> fns "CHT" 0 2
          | 0xAB -> fnc 'Y'
          | 0xAC -> ()
          | 0xAD -> fnc 'E'
          | 0xAE -> fns "YOU" 0 3
          | 0xAF -> fns "YA" 0 2
          | 0xB0 -> fnc 'a'
          | 0xB1 -> fnc 'b'
          | 0xB2 -> fnc 'v'
          | 0xB3 -> fnc 'g'
          | 0xB4 -> fnc 'd'
          | 0xB5 -> fnc 'e'
          | 0xB6 -> fnc 'j'
          | 0xB7 -> fnc 'z'
          | 0xB8 | 0xB9 -> fnc 'i'
          | 0xBA -> fnc 'k'
          | 0xBB -> fnc 'l'
          | 0xBC -> fnc 'm'
          | 0xBD -> fnc 'n'
          | 0xBE -> fnc 'o'
          | 0xBF -> fnc 'p'
          | _ -> unsupported s i nbc
        end

      | 0xD1 ->
        begin match Char.code @@ String.unsafe_get s (i+1) with
          | 0x80 -> fnc 'r'
          | 0x81 -> fnc 's'
          | 0x82 -> fnc 't'
          | 0x83 -> fns "ou" 0 2
          | 0x84 -> fnc 'f'
          | 0x85 -> fns "kh" 0 2
          | 0x86 -> fns "ts" 0 2
          | 0x87 -> fns "tch" 0 3
          | 0x88 -> fns "ch" 0 2
          | 0x89 -> fns "cht" 0 3
          | 0x8B -> fnc 'y'
          | 0x8C -> ()
          | 0x8D -> fnc 'e'
          | 0x8E -> fns "you" 0 3
          | 0x8F -> fns "ya" 0 2
          | 0x91 -> fnc 'e'
          | _ -> unsupported s i nbc
        end

      | 0xD3 ->
        begin match Char.code @@ String.unsafe_get s (i+1) with
          | 0x80 -> fnc 'I'
          | 0x95 -> fns "ae" 0 2
          | _ -> unsupported s i nbc
        end

      | 0xD4 ->
        (* Armenian *)
        begin match Char.code @@ String.unsafe_get s (i+1) with
          | 0xB1 -> fnc 'A'
          | 0xB2 -> fnc 'B'
          | 0xB3 -> fnc 'G'
          | 0xB4 -> fnc 'D'
          | 0xB5 -> fnc 'E'
          | 0xB6 -> fnc 'Z'
          | 0xB7 -> fnc 'E'
          | 0xB8 -> fnc 'E'
          | 0xB9 -> fnc 'T'
          | 0xBA -> fnc 'Z'
          | 0xBB -> fnc 'I'
          | 0xBC -> fnc 'L'
          | 0xBD -> fnc 'X'
          | 0xBE -> fnc 'C'
          | 0xBF -> fnc 'K'
          | _ -> unsupported s i nbc
        end

      | 0xD5 ->
        begin match Char.code @@ String.unsafe_get s (i+1) with
          | 0x80 -> fnc 'H'
          | 0x81 -> fnc 'J'
          | 0x82 -> fnc 'L'
          | 0x83 -> fnc 'C'
          | 0x84 -> fnc 'M'
          | 0x85 -> fnc 'Y'
          | 0x86 -> fnc 'N'
          | 0x87 -> fnc 'S'
          | 0x88 -> fnc 'O'
          | 0x89 -> fnc 'C'
          | 0x8A -> fnc 'P'
          | 0x8B -> fnc 'J'
          | 0x8C -> fnc 'R'
          | 0x8D -> fnc 'S'
          | 0x8E -> fnc 'V'
          | 0x8F -> fnc 'T'
          | 0x90 -> fnc 'R'
          | 0x91 -> fnc 'C'
          | 0x92 -> fnc 'W'
          | 0x93 -> fnc 'P'
          | 0x94 -> fnc 'K'
          | 0x95 -> fnc 'O'
          | 0x96 -> fnc 'F'
          | 0xA1 -> fnc 'a'
          | 0xA2 -> fnc 'b'
          | 0xA3 -> fnc 'g'
          | 0xA4 -> fnc 'd'
          | 0xA5 -> fnc 'e'
          | 0xA6 -> fnc 'z'
          | 0xA7 -> fnc 'e'
          | 0xA8 -> fnc 'e'
          | 0xA9 -> fnc 't'
          | 0xAA -> fnc 'z'
          | 0xAB -> fnc 'i'
          | 0xAC -> fnc 'l'
          | 0xAD -> fnc 'x'
          | 0xAE -> fnc 'c'
          | 0xAF -> fnc 'k'
          | 0xB0 -> fnc 'h'
          | 0xB1 -> fnc 'j'
          | 0xB2 -> fnc 'l'
          | 0xB3 -> fnc 'c'
          | 0xB4 -> fnc 'm'
          | 0xB5 -> fnc 'y'
          | 0xB6 -> fnc 'n'
          | 0xB7 -> fnc 's'
          | 0xB8 -> fnc 'o'
          | 0xB9 -> fnc 'c'
          | 0xBA -> fnc 'p'
          | 0xBB -> fnc 'j'
          | 0xBC -> fnc 'r'
          | 0xBD -> fnc 's'
          | 0xBE -> fnc 'v'
          | 0xBF -> fnc 't'
          | _ -> unsupported s i nbc
        end

      | 0xD6 ->
        begin match Char.code @@ String.unsafe_get s (i+1) with
          | 0x80 -> fnc 'r'
          | 0x81 -> fnc 'c'
          | 0x82 -> fnc 'w'
          | 0x83 -> fnc 'p'
          | 0x84 -> fnc 'k'
          | 0x85 -> fnc 'o'
          | 0x86 -> fnc 'f'
          | _ -> unsupported s i nbc
        end

      | _ -> unsupported s i nbc

    in
    i + nbc
