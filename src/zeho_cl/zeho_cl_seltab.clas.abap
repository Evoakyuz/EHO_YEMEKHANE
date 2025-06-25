CLASS zeho_cl_seltab DEFINITION
  PUBLIC

  CREATE PROTECTED.

*  global friends IF_LIB_SELTAB .

  PUBLIC SECTION.

*      TYPES:
*    BEGIN OF ts_sel,
*      sign(1)   TYPE c,
*      option(2) TYPE c,
*      low(30)   TYPE c,
*      high(30)  TYPE c,
*    END OF ts_sel .
*  TYPES:
*    tt_sel TYPE STANDARD TABLE OF ts_sel WITH DEFAULT KEY .

    TYPES:
      BEGIN OF ts_sel,
        sign(1)   TYPE c,
        option(2) TYPE c,
        low(30)   TYPE c,
        high(30)  TYPE c,
      END OF ts_sel .
    TYPES:
      tt_sel TYPE STANDARD TABLE OF ts_sel WITH DEFAULT KEY .

    CONSTANTS:
      BEGIN OF mc_all,
        sign   TYPE c VALUE 'E',
        option TYPE char2 VALUE 'NL',
        low    TYPE char30 VALUE '',
        high   TYPE char30 VALUE '',
      END OF mc_all,
      BEGIN OF mc_c_all,
        sign   TYPE c VALUE 'I',
        option TYPE char2 VALUE 'CP',
        low    TYPE char30 VALUE '*',
        high   TYPE char30 VALUE '',
      END OF mc_c_all .
    CONSTANTS:
      BEGIN OF mc_i_all,
        sign   TYPE c VALUE 'I',
        option TYPE char2 VALUE 'CP',
        low    TYPE int1 VALUE 0,
        high   TYPE int1 VALUE 0,
      END OF mc_i_all .

    TYPES:
      tv_large_integer TYPE p LENGTH 16 DECIMALS 0 .
*  types:
*    BEGIN OF ts_check_base,
*      status    TYPE if_lib_msg_writer=>tv_status,
*      selected  TYPE abap_bool,
*      processed TYPE if_check_issue=>tv_icon,
*      sw_comp   TYPE dlvunit,
*      bal_logid TYPE balloghndl,
*    END OF ts_check_base .
*  types:
*    tt_check_base TYPE STANDARD TABLE OF ts_check_base WITH DEFAULT KEY .
    TYPES:
      tt_string TYPE STANDARD TABLE OF string WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_parval,
        par TYPE string,
        val TYPE string,
      END OF ts_parval .
    TYPES:
      tt_parval TYPE STANDARD TABLE OF ts_parval WITH KEY par .
    TYPES:
      BEGIN OF ts_named_oref,
        name TYPE string,
        oref TYPE REF TO object,
      END OF ts_named_oref .
    TYPES:
      tt_named_oref TYPE STANDARD TABLE OF ts_named_oref WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_named_dref,
        name TYPE string,
        dref TYPE REF TO data,
      END OF ts_named_dref .
    TYPES:
      tt_named_dref TYPE STANDARD TABLE OF ts_named_dref WITH DEFAULT KEY .

    CONSTANTS mc_lock_defined TYPE abap_bool VALUE 'X' ##NO_TEXT.

    TYPES ts_named_seltable TYPE ts_named_dref .
    TYPES tt_named_seltabs TYPE tt_named_oref .
    TYPES tt_named_seltables TYPE tt_named_dref .

    DATA mv_escape TYPE string VALUE '@' ##NO_TEXT.
    CONSTANTS mc_sign_incl TYPE char1 VALUE 'I' ##NO_TEXT.
    CONSTANTS mc_sign_excl TYPE char1 VALUE 'E' ##NO_TEXT.

    CLASS-METHODS new
      IMPORTING
        !it_sel          TYPE table
        !iv_expand_empty TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rr_ref)    TYPE REF TO zeho_cl_seltab .
    CLASS-METHODS new_flat
      IMPORTING
        !iv_sel          TYPE string
        !iv_expand_empty TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rr_ref)    TYPE REF TO zeho_cl_seltab .
    METHODS sql_where_condition
      IMPORTING
        !iv_field      TYPE string
      RETURNING
        VALUE(rv_cond) TYPE string
      RAISING
        zeho_cl_messages .
    METHODS seltab
      RETURNING
        VALUE(rr_seltab) TYPE REF TO data .
    CLASS-METHODS resolve_named_seltabs
      IMPORTING
        !it_named_seltabs TYPE tt_named_seltabs
        !iv_operator      TYPE string DEFAULT 'AND'
      RETURNING
        VALUE(rv_sql)     TYPE string
      RAISING
        zeho_cl_messages .
    CLASS-METHODS combine_seltabs
      IMPORTING
        !it_named_seltabs TYPE tt_named_seltables
        !iv_client_field  TYPE string OPTIONAL
      RETURNING
        VALUE(rv_where)   TYPE string
      RAISING
        zeho_cl_messages .
    CLASS-METHODS ztest .
  PROTECTED SECTION.

    DATA _mr_seltab TYPE REF TO data .

    METHODS constructor
      IMPORTING
        !it_sel          TYPE table
        !iv_expand_empty TYPE abap_bool DEFAULT abap_true .
  PRIVATE SECTION.

    DATA _mr_table_descr TYPE REF TO cl_abap_tabledescr .
    DATA _mv_quote_l TYPE string .
    DATA _mv_quote_r TYPE string .
    DATA _mv_clike TYPE abap_bool .
    DATA _mv_expand_empty TYPE abap_bool .

    METHODS _sql_operator
      IMPORTING
        !iv_selop       TYPE char2
      RETURNING
        VALUE(rv_sqlop) TYPE string .
    METHODS _build_conditions
      IMPORTING
        !iv_field      TYPE string
        !iv_sign       TYPE char1
      RETURNING
        VALUE(rv_cond) TYPE string
      RAISING
        zeho_cl_messages .
    CLASS-METHODS _empty_seltab
      RETURNING
        VALUE(rr_ref) TYPE REF TO cl_abap_tabledescr .
ENDCLASS.



CLASS ZEHO_CL_SELTAB IMPLEMENTATION.


  METHOD combine_seltabs.
    DATA:
      ls_seltab TYPE ts_named_seltable,
      lr_seltab TYPE REF TO zeho_cl_seltab,
      lv_sep    TYPE string.
    FIELD-SYMBOLS
      <fs_seltab> TYPE table.

    IF iv_client_field IS NOT INITIAL.
      rv_where = |{ iv_client_field } = '{ sy-mandt }'|.
      lv_sep = ` AND `.
    ENDIF.

    LOOP AT it_named_seltabs INTO ls_seltab.
      ASSIGN ls_seltab-dref->* TO <fs_seltab>.
      IF <fs_seltab> IS NOT INITIAL.
        lr_seltab = zeho_cl_seltab=>new( <fs_seltab> ).
        rv_where = |{ rv_where }{ lv_sep }( { lr_seltab->sql_where_condition( ls_seltab-name ) } )|.
        lv_sep = ` AND `.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.
    DATA:
      lr_struc_descr TYPE REF TO cl_abap_structdescr,
      lt_fields      TYPE abap_component_tab,
      ls_low         TYPE abap_componentdescr,
      ls_high        TYPE abap_componentdescr.
    FIELD-SYMBOLS:
      <lt_seltab> TYPE table,
      <ls_all>    TYPE data.

    "regard empty selection options:
    _mv_expand_empty = iv_expand_empty.
    IF it_sel IS INITIAL AND _mv_expand_empty = abap_true.
      _mr_table_descr = _empty_seltab( ).
    ELSE.
      _mr_table_descr ?= cl_abap_datadescr=>describe_by_data( it_sel ).
    ENDIF.

    lr_struc_descr ?= _mr_table_descr->get_table_line_type( ).

    "precondition: expects selection tables only:
    lt_fields = lr_struc_descr->get_components( ).
    READ TABLE lt_fields WITH KEY name = 'SIGN' TRANSPORTING NO FIELDS.
    ASSERT sy-subrc = 0.
    READ TABLE lt_fields WITH KEY name = 'OPTION' TRANSPORTING NO FIELDS.
    ASSERT sy-subrc = 0.
    READ TABLE lt_fields WITH KEY name = 'LOW' TRANSPORTING NO FIELDS.
    ASSERT sy-subrc = 0.
    READ TABLE lt_fields WITH KEY name = 'HIGH' TRANSPORTING NO FIELDS.
    ASSERT sy-subrc = 0.

    READ TABLE lt_fields WITH KEY name = 'LOW' INTO ls_low.
    READ TABLE lt_fields WITH KEY name = 'HIGH' INTO ls_high.
    ASSERT ls_low-type->type_kind = ls_high-type->type_kind.
    CASE ls_low-type->type_kind.
      WHEN cl_abap_typedescr=>typekind_class        "not allowed types in SELTAB
        OR cl_abap_typedescr=>typekind_dref
        OR cl_abap_typedescr=>typekind_iref
        OR cl_abap_typedescr=>typekind_oref
        OR cl_abap_typedescr=>typekind_intf
        OR cl_abap_typedescr=>typekind_struct1
        OR cl_abap_typedescr=>typekind_struct2
        OR cl_abap_typedescr=>typekind_table
        OR cl_abap_typedescr=>typekind_bref.
        ASSERT ls_low-type->type_kind IS NOT INITIAL.
      WHEN cl_abap_typedescr=>typekind_char         "char-like types
        OR cl_abap_typedescr=>typekind_clike
        OR cl_abap_typedescr=>typekind_csequence
        OR cl_abap_typedescr=>typekind_string.
        _mv_quote_l = `'`.
        _mv_quote_r = `'`.
        _mv_clike = abap_true.
      WHEN cl_abap_typedescr=>typekind_any
        OR cl_abap_typedescr=>typekind_data
        OR cl_abap_typedescr=>typekind_date
        OR cl_abap_typedescr=>typekind_num
        OR cl_abap_typedescr=>typekind_time
        OR cl_abap_typedescr=>typekind_xstring.
        _mv_quote_l = `'`.
        _mv_quote_r = `'`.
      WHEN cl_abap_typedescr=>typekind_hex          "hexadecimal types
        OR cl_abap_typedescr=>typekind_xsequence.
        _mv_quote_l = `0x`.
        _mv_quote_r = ``.
      WHEN cl_abap_typedescr=>typekind_decfloat     "numeric types
        OR cl_abap_typedescr=>typekind_decfloat16
        OR cl_abap_typedescr=>typekind_decfloat34
        OR cl_abap_typedescr=>typekind_float
        OR cl_abap_typedescr=>typekind_int
        OR cl_abap_typedescr=>typekind_int1
        OR cl_abap_typedescr=>typekind_int2
        OR cl_abap_typedescr=>typekind_numeric
        OR cl_abap_typedescr=>typekind_packed
        OR cl_abap_typedescr=>typekind_simple
        OR cl_abap_typedescr=>typekind_w.
        CLEAR: _mv_quote_l, _mv_quote_r.
    ENDCASE.

    CREATE DATA _mr_seltab TYPE HANDLE _mr_table_descr.
    ASSIGN _mr_seltab->* TO <lt_seltab>.

    IF it_sel IS INITIAL AND _mv_expand_empty = abap_true.
      "empty seltab == return all values!
      APPEND mc_c_all TO <lt_seltab>.
    ELSE.
      "Fill _mr_seltab with input
      <lt_seltab> = it_sel.
    ENDIF.

  ENDMETHOD.                    "constructor


  METHOD new.
    CREATE OBJECT rr_ref
      EXPORTING
        it_sel          = it_sel
        iv_expand_empty = iv_expand_empty.
  ENDMETHOD.                    "if_lib_seltab~new


  METHOD new_flat.
    DATA:
      lt_lines TYPE STANDARD TABLE OF string,
      lv_line  TYPE string,
      ls_sel   TYPE ts_sel,
      lt_sel   TYPE tt_sel.

    SPLIT iv_sel AT ';' INTO TABLE lt_lines.
    LOOP AT lt_lines INTO lv_line.
      CONDENSE lv_line.
      IF find( val = lv_line  sub = ',' ) = -1.
        "simplify user-syntax for just a sequence of values:
        ls_sel-sign = 'I'.
        ls_sel-option = 'EQ'.
        ls_sel-low = lv_line.
        CLEAR: ls_sel-high.
      ELSE.
        CLEAR: ls_sel-high.
        SPLIT lv_line AT ',' INTO ls_sel-sign ls_sel-option ls_sel-low ls_sel-high.
      ENDIF.
      APPEND ls_sel TO lt_sel.
    ENDLOOP.
    rr_ref = new( it_sel = lt_sel iv_expand_empty = iv_expand_empty ).

  ENDMETHOD.                    "new_flat


  METHOD resolve_named_seltabs.
    DATA:
      ls_seltab TYPE ts_named_oref,
      lr_seltab TYPE REF TO zeho_cl_seltab,
      lv_where  TYPE string,
      lv_sep    TYPE string.

    LOOP AT it_named_seltabs INTO ls_seltab.
      lr_seltab ?= ls_seltab-oref.
      lv_where = lr_seltab->sql_where_condition( ls_seltab-name ).
      IF lv_where IS NOT INITIAL.
        rv_sql = |{ rv_sql }{ lv_sep }({ lv_where })|.
        lv_sep = | { iv_operator } |.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD seltab.
    rr_seltab = _mr_seltab.
  ENDMETHOD.


  METHOD sql_where_condition.
    DATA:
      lv_field   TYPE string,
      lv_include TYPE string,
      lv_exclude TYPE string.

    "Regard fieldnames containing namespaces:
    IF find( val = iv_field  sub = '/' ) >= 0.
      lv_field = |"{ iv_field }"|.
    ELSE.
      lv_field = iv_field.
    ENDIF.

    "Collect amount of including and excluding lines (OR):
    lv_include = _build_conditions( iv_field = lv_field  iv_sign = mc_sign_incl ).
    lv_exclude = _build_conditions( iv_field = lv_field  iv_sign = mc_sign_excl ).

    "Reduce the including amount by the excluding amount (AND NOT)
    IF lv_include IS NOT INITIAL AND lv_exclude IS NOT INITIAL.
      rv_cond = |( { lv_include } ) AND NOT ({ lv_exclude } )|.
    ELSEIF lv_exclude IS NOT INITIAL.
      rv_cond = |NOT ( { lv_exclude } )|.
    ELSEIF lv_include IS NOT INITIAL.
      rv_cond = lv_include.
    ELSE.
      rv_cond = ''.
    ENDIF.

  ENDMETHOD.                    "sql_where_condition


  METHOD ztest.

  ENDMETHOD.


  METHOD _build_conditions.
    DATA:
      lv_line          TYPE string,
      lv_op            TYPE string,
      lv_sep           TYPE string,
      lv_escape        TYPE abap_bool,
      lv_bracket_left  TYPE char1,
      lv_bracket_right TYPE char1,
      lv_sign_count    TYPE i.
    FIELD-SYMBOLS:
      <lt_seltab> TYPE table,
      <ls_seltab> TYPE data,
      <sign>      TYPE data,
      <option>    TYPE data,
      <low>       TYPE data,
      <high>      TYPE data.

    ASSIGN _mr_seltab->* TO <lt_seltab>.
    IF <lt_seltab> IS INITIAL AND _mv_expand_empty = abap_false.
      RETURN.
    ENDIF.

    CLEAR lv_sep.
    LOOP AT <lt_seltab> ASSIGNING <ls_seltab>.

      CLEAR: lv_escape.
      ASSIGN COMPONENT 'SIGN'   OF STRUCTURE <ls_seltab> TO <sign>.
      ASSIGN COMPONENT 'OPTION' OF STRUCTURE <ls_seltab> TO <option>.
      ASSIGN COMPONENT 'LOW'    OF STRUCTURE <ls_seltab> TO <low>.
      ASSIGN COMPONENT 'HIGH'   OF STRUCTURE <ls_seltab> TO <high>.

      "use strings instead of the original type intermediatly due to possible enlargement by DB quotation,
      "which may exceed the defined length of the type!
      "Here we can't do a simple assignment like lv_low = <low> because of negative numeric values
      "e.g. <low> = 42 would result lv_low = '42-' in string representation (leads to a SQL error)
      "Instead we use string templates resulting the correct value lv_low = '-42'.
      DATA lv_low TYPE string.
      lv_low  = |{ <low> }|.
      DATA lv_high TYPE string.
      lv_high = |{ <high> }|.

      "avoid usage of options which are not supported for the particular data type:
      IF ( <option> = 'CP' OR <option> = 'NP' ) AND _mv_clike = abap_false.
        RAISE EXCEPTION TYPE zeho_cl_messages
          EXPORTING
            textid      = zeho_cl_messages=>wrong_sel_option
*           previous    = previous
            mv_option  = <option>.
      ENDIF.

      IF <sign> = iv_sign.

        IF sy-tabix > 1. rv_cond = |{ rv_cond } |. ENDIF.

        IF _mv_clike = abap_true.
          "Escape special chars (ATTENTION: the correct sequence is important!)"
          REPLACE ALL OCCURRENCES OF |'| IN lv_low  WITH |''|.
          REPLACE ALL OCCURRENCES OF |'| IN lv_high WITH |''|.
          IF <option> = 'CP' OR <option> = 'NP'.
            "special escapes, for compare options only:
            REPLACE ALL OCCURRENCES OF |@| IN lv_low  WITH |{ mv_escape }@|. lv_escape = boolc( sy-subrc < 4 OR lv_escape = abap_true ).
            REPLACE ALL OCCURRENCES OF |_| IN lv_low  WITH |{ mv_escape }_|. lv_escape = boolc( sy-subrc < 4 OR lv_escape = abap_true ).
            REPLACE ALL OCCURRENCES OF |%| IN lv_low  WITH |{ mv_escape }%|. lv_escape = boolc( sy-subrc < 4 OR lv_escape = abap_true ).
          ENDIF.
        ENDIF.

        lv_op = _sql_operator( <option> ).
        IF lv_escape = abap_true. lv_op = |{ lv_op } ESCAPE '{ mv_escape }' |. ENDIF.

        REPLACE ALL OCCURRENCES OF '%LOW%'  IN lv_op WITH |{ lv_low }|.
        REPLACE ALL OCCURRENCES OF '%HIGH%' IN lv_op WITH |{ lv_high }|.
        IF <option> = 'CP' OR <option> = 'NP'.
          "wildcard transformation for compare options only:
          REPLACE ALL OCCURRENCES OF '*'    IN lv_op WITH '%'.
          REPLACE ALL OCCURRENCES OF '+'    IN lv_op WITH '_'.
        ENDIF.

        lv_line = |{ iv_field } { lv_op }|.
        rv_cond = |{ rv_cond }{ lv_sep }{ lv_line }|.
        lv_sep = `OR `.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD _empty_seltab.
    DATA:
      lt_all LIKE STANDARD TABLE OF mc_c_all.
    "if seltab is empty, always create a char-like seltab type:
    rr_ref ?= cl_abap_datadescr=>describe_by_data( lt_all ).
  ENDMETHOD.


  METHOD _sql_operator.
    CASE iv_selop.
      WHEN: 'EQ'. rv_sqlop = |= { _mv_quote_l }%LOW%{ _mv_quote_r }|.
      WHEN: 'CP'. rv_sqlop = |LIKE { _mv_quote_l }%LOW%{ _mv_quote_r }|.
      WHEN: 'NP'. rv_sqlop = |NOT LIKE { _mv_quote_l }%LOW%{ _mv_quote_r }|.
      WHEN: 'NE'. rv_sqlop = |!= { _mv_quote_l }%LOW%{ _mv_quote_r }|.
      WHEN: 'GE'. rv_sqlop = |>= { _mv_quote_l }%LOW%{ _mv_quote_r }|.
      WHEN: 'LE'. rv_sqlop = |<= { _mv_quote_l }%LOW%{ _mv_quote_r }|.
      WHEN: 'GT'. rv_sqlop = |> { _mv_quote_l }%LOW%{ _mv_quote_r }|.
      WHEN: 'LT'. rv_sqlop = |< { _mv_quote_l }%LOW%{ _mv_quote_r }|.
      WHEN: 'BT'. rv_sqlop = |BETWEEN { _mv_quote_l }%LOW%{ _mv_quote_r } AND { _mv_quote_l }%HIGH%{ _mv_quote_r }|.
      WHEN: 'NB'. rv_sqlop = |NOT BETWEEN { _mv_quote_l }%LOW%{ _mv_quote_r } AND { _mv_quote_l }%HIGH%{ _mv_quote_r }|.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
