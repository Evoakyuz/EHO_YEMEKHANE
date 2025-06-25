CLASS zeho_cl_messages DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .
    INTERFACES if_abap_behv_message .

    CONSTANTS mc_err_part_spec LIKE textid VALUE '100' ##NO_TEXT.

    CONSTANTS:
      gc_msgid TYPE symsgid VALUE 'ZEHO_MSG',

      BEGIN OF acct_already_exists,
        msgid TYPE symsgid VALUE 'ZEHO_MSG',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MV_BANKCODE',
        attr2 TYPE scx_attrname VALUE 'MV_BUKRS',
        attr3 TYPE scx_attrname VALUE 'MV_ACTIVITY_TYPE',
        attr4 TYPE scx_attrname VALUE 'MV_PRIORITY',
      END OF acct_already_exists,

      BEGIN OF exp_already_exists,
        msgid TYPE symsgid VALUE 'ZEHO_MSG',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MV_BANKCODE',
        attr2 TYPE scx_attrname VALUE 'MV_BUKRS',
        attr3 TYPE scx_attrname VALUE 'MV_ACTIVITY_TYPE',
        attr4 TYPE scx_attrname VALUE 'MV_EXPLANATION',
      END OF exp_already_exists,

      BEGIN OF unique_maintenance_is_wrong,
        msgid TYPE symsgid VALUE 'ZEHO_MSG',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'MV_BANKCODE',
        attr2 TYPE scx_attrname VALUE 'MV_BUKRS',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF unique_maintenance_is_wrong ,

      BEGIN OF service_not_found,
        msgid TYPE symsgid VALUE 'ZEHO_MSG',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF service_not_found ,

      BEGIN OF request_mapping_not_found,
        msgid TYPE symsgid VALUE 'ZEHO_MSG',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'MV_BANKCODE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF request_mapping_not_found ,


      BEGIN OF response_mapping_not_found,
        msgid TYPE symsgid VALUE 'ZEHO_MSG',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'MV_BANKCODE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF response_mapping_not_found,

      BEGIN OF request_send_error,
        msgid TYPE symsgid VALUE 'ZEHO_MSG',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'MV_BANKCODE',
        attr2 TYPE scx_attrname VALUE 'MV_BUKRS',
        attr3 TYPE scx_attrname VALUE 'MV_IBAN',
        attr4 TYPE scx_attrname VALUE '',
      END OF request_send_error,

      BEGIN OF unique_mapping_not_developed,
        msgid TYPE symsgid VALUE 'ZEHO_MSG',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE 'MV_BANKCODE',
        attr2 TYPE scx_attrname VALUE 'MV_BUKRS',
        attr3 TYPE scx_attrname VALUE 'MV_IBAN',
        attr4 TYPE scx_attrname VALUE '',
      END OF unique_mapping_not_developed,

      BEGIN OF wrong_sel_option,
        msgid TYPE symsgid VALUE 'ZEHO_MSG',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE 'MV_OPTION',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF wrong_sel_option,


      BEGIN OF badi_implementation_missing,
        msgid TYPE symsgid VALUE 'ZEHO_MSG',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF badi_implementation_missing,


      BEGIN OF recording_started,
        msgid TYPE symsgid VALUE 'ZEHO_MSG',
        msgno TYPE symsgno VALUE '009',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF recording_started,

      BEGIN OF document_created,
        msgid TYPE symsgid VALUE 'ZEHO_MSG',
        msgno TYPE symsgno VALUE '010',
*        msgtype TYPE symsgty VALUE 'S',
        attr1 TYPE scx_attrname VALUE 'MV_BELNR',
        attr2 TYPE scx_attrname VALUE 'MV_GJAHR',
        attr3 TYPE scx_attrname VALUE 'MV_BUKRS',
        attr4 TYPE scx_attrname VALUE '',
      END OF document_created,

      BEGIN OF fill_account,
        msgid TYPE symsgid VALUE 'ZEHO_MSG',
        msgno TYPE symsgno VALUE '011',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
*        attr4   TYPE scx_attrname VALUE '',
      END OF fill_account,

      BEGIN OF amount_cannot_be_null,
        msgid TYPE symsgid VALUE 'ZEHO_MSG',
        msgno TYPE symsgno VALUE '012',
*        msgtype TYPE symsgty VALUE 'S',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF amount_cannot_be_null,

      BEGIN OF new_item_saved_succesfully,
        msgid TYPE symsgid VALUE 'ZEHO_MSG',
        msgno TYPE symsgno VALUE '013',
*        msgtype TYPE symsgty VALUE 'S',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF new_item_saved_succesfully,

      BEGIN OF activity_posted_or_canceled,
        msgid TYPE symsgid VALUE 'ZEHO_MSG',
        msgno TYPE symsgno VALUE '014',
*        msgtype TYPE symsgty VALUE 'S',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF activity_posted_or_canceled,

      BEGIN OF subtotal_is_not_zero,
        msgid TYPE symsgid VALUE 'ZEHO_MSG',
        msgno TYPE symsgno VALUE '015',
*        msgtype TYPE symsgty VALUE 'S',
        attr1 TYPE scx_attrname VALUE 'MV_AMOUNT',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF subtotal_is_not_zero,

      BEGIN OF document_type_missing,
        msgid TYPE symsgid VALUE 'ZEHO_MSG',
        msgno TYPE symsgno VALUE '016',
*        msgtype TYPE symsgty VALUE 'S',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF document_type_missing,
            BEGIN OF item_not_found,
        msgid TYPE symsgid VALUE 'ZEHO_MSG',
        msgno TYPE symsgno VALUE '017',
*        msgtype TYPE symsgty VALUE 'S',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF item_not_found,
           BEGIN OF update_completed,
        msgid TYPE symsgid VALUE 'ZEHO_MSG',
        msgno TYPE symsgno VALUE '018',
*        msgtype TYPE symsgty VALUE 'S',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
         END OF update_completed,

          BEGIN OF multiple_currency,
        msgid TYPE symsgid VALUE 'ZEHO_MSG',
        msgno TYPE symsgno VALUE '019',
*        msgtype TYPE symsgty VALUE 'S',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
         END OF multiple_currency,

             BEGIN OF fill_currency,
        msgid TYPE symsgid VALUE 'ZEHO_MSG',
        msgno TYPE symsgno VALUE '020',
*        msgtype TYPE symsgty VALUE 'S',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
         END OF fill_currency.





    METHODS constructor
      IMPORTING
        textid           LIKE if_t100_message=>t100key OPTIONAL
        attr1            TYPE string OPTIONAL
        attr2            TYPE string OPTIONAL
        attr3            TYPE string OPTIONAL
        attr4            TYPE string OPTIONAL
        mv_bankcode      TYPE bankk OPTIONAL
        mv_bukrs         TYPE bukrs OPTIONAL
        mv_activity_type TYPE zeho_de_activity_types OPTIONAL
        mv_priority      TYPE zeho_de_priority OPTIONAL
        mv_uname         TYPE syuname OPTIONAL
        previous         LIKE previous OPTIONAL
        severity         TYPE if_abap_behv_message=>t_severity OPTIONAL
        uname            TYPE syuname OPTIONAL
        mv_explanation   TYPE zeho_de_explanation OPTIONAL
        mv_iban          TYPE iban OPTIONAL
        mv_option        TYPE chAr2 OPTIONAL
        mv_belnr         TYPE belnr_d OPTIONAL
        mv_gjahr         TYPE gjahr OPTIONAL
        mv_amount        TYPE zeho_de_amount OPTIONAL.




    DATA:
      mv_attr1         TYPE string,
      mv_attr2         TYPE string,
      mv_attr3         TYPE string,
      mv_attr4         TYPE string,
      mv_bankcode      TYPE bankk,
      mv_bukrs         TYPE bukrs,
      mv_activity_type TYPE zeho_de_activity_types,
      mv_priority      TYPE zeho_de_priority,
      mv_uname         TYPE syuname,
      mv_explanation   TYPE zeho_de_explanation,
      mv_iban          TYPE iban,
      mv_option        TYPE char2,
      mv_belnr         TYPE belnr_d,
      mv_gjahr         TYPE gjahr,
      mv_amount        TYPE zeho_de_amount.
*      mv_bukrs         TYPE bukrs.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZEHO_CL_MESSAGES IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor(  previous = previous ) .

    me->mv_attr1                 = attr1.
    me->mv_attr2                 = attr2.
    me->mv_attr3                 = attr3.
    me->mv_attr4                 = attr4.
    me->mv_bankcode              = mv_bankcode.
    me->mv_bukrs                 = mv_bukrs.
    me->mv_priority              = mv_priority.
    me->mv_activity_type         = mv_activity_type.
    me->mv_uname                 = uname.
    me->mv_explanation           = mv_explanation.
    me->mv_iban                  = mv_iban.
    me->mv_option                = mv_option.
    me->mv_belnr                 = mv_belnr.
    me->mv_gjahr                 = mv_gjahr.
    me->mv_amount                = mv_amount.
    if_abap_behv_message~m_severity = severity.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
