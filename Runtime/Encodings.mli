
open FontMetric;
open Unicode;
open Unicode.Types;
open Substitute;

value undefined : Charmap.charmap glyph_desc;
value uc_to_ot1 : Charmap.charmap glyph_desc;
value uc_to_t1  : Charmap.charmap glyph_desc;
value uc_to_ott : Charmap.charmap glyph_desc;
value uc_to_oms : Charmap.charmap glyph_desc;
value uc_to_oml : Charmap.charmap glyph_desc;
value fake      : Charmap.charmap glyph_desc;

value ot1_to_uc : array uc_string;
value t1_to_uc  : array uc_string;
value ott_to_uc : array uc_string;
value oms_to_uc : array uc_string;
value oml_to_uc : array uc_string;

value fake_encoding    : array uc_string -> Charmap.charmap glyph_desc;
value charmap_encoding : Charmap.charmap glyph_desc -> uc_char -> glyph_desc;
value array_decoding   : array uc_string -> glyph_desc -> uc_string;

value raw_encoding     : uc_char -> glyph_desc;
value raw_decoding     : glyph_desc -> uc_string;

