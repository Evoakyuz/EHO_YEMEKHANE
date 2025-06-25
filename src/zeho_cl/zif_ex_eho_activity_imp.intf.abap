INTERFACE zif_ex_eho_activity_imp
  PUBLIC .
  TYPES: mty_je_post_hierarchy TYPE STRUCTURE FOR ACTION IMPORT i_journalentrytp~post .
  TYPES: lt_entyry     TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post,
         ls_entry      TYPE LINE OF lt_entyry,
         lty_glitem    TYPE  ls_entry-%param-_glitems,
         lty_currencya TYPE LINE OF  ls_entry-%param-_glitems,
         lty_currency  TYPE  lty_currencya-_currencyamount,
         lty_ARItems   TYPE  ls_entry-%param-_aritems,
         lty_APItems   TYPE  ls_entry-%param-_apitems,
         lty_log       TYPE TABLE OF zeho_a_log.

  INTERFACES if_badi_interface .

  METHODS  firstgl_processing
    IMPORTING
      !rd_aa      TYPE zeho_i_activities
      !e_hkont    TYPE hkont
      !e_dc       TYPE zeho_de_dc
    CHANGING
      !tt_glitems TYPE   lty_glitem.
  METHODS  secondgl_processing
    IMPORTING
              !rd_aa     TYPE zeho_i_activities
              !e_hkont   TYPE hkont
              !e_dc      TYPE zeho_de_dc
    CHANGING  tt_glitems TYPE   lty_glitem.

  METHODS customer_processing
    IMPORTING
      !rd_aa      TYPE zeho_i_activities
      !e_dc       TYPE zeho_de_dc
    CHANGING
      !tt_aritems TYPE lty_ARItems.

  METHODS supplier_processing
    IMPORTING
      !rd_aa      TYPE zeho_i_activities
      !e_dc       TYPE zeho_de_dc
    CHANGING
      !tt_apitems TYPE lty_APItems.


  METHODS before_modify_JE
    IMPORTING
      !rd_aa TYPE zeho_i_activities
    CHANGING
      tt_bapidata  TYPE lt_entyry.

  METHODS after_modify_je
    IMPORTING
      !tt_log TYPE lty_log.


ENDINTERFACE.
