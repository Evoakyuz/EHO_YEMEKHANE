CLASS lhc_acc DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS get_global_features FOR GLOBAL FEATURES
      IMPORTING REQUEST requested_features FOR ACC RESULT result.

ENDCLASS.

CLASS lhc_acc IMPLEMENTATION.

  METHOD get_global_features.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_resp DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS get_global_features FOR GLOBAL FEATURES
      IMPORTING REQUEST requested_features FOR Resp RESULT result.

ENDCLASS.

CLASS lhc_resp IMPLEMENTATION.

  METHOD get_global_features.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_req DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS get_global_features FOR GLOBAL FEATURES
      IMPORTING REQUEST requested_features FOR Req RESULT result.

ENDCLASS.

CLASS lhc_req IMPLEMENTATION.

  METHOD get_global_features.
  ENDMETHOD.

ENDCLASS.



CLASS lhc_Singleton DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Singleton RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR Singleton RESULT result.

ENDCLASS.

CLASS lhc_Singleton IMPLEMENTATION.

  METHOD get_instance_features.
  ENDMETHOD.

  METHOD get_global_authorizations.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_Bank DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_global_features FOR GLOBAL FEATURES
      IMPORTING REQUEST requested_features FOR Bank RESULT result.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Bank RESULT result.
    METHODS checkvalues FOR VALIDATE ON SAVE
      IMPORTING keys FOR bank~checkvalues.
*    METHODS earlynumbering_cba_Comps FOR NUMBERING
*      IMPORTING entities FOR CREATE Bank\_Comps.

ENDCLASS.

CLASS lhc_Bank IMPLEMENTATION.

  METHOD get_global_features.
  ENDMETHOD.



*  METHOD earlynumbering_cba_Comps.
**  mapped-bank = VALUE #( ( %cid = entities[ 1 ]-%target[ 1 ]-%cid
**                           %is_draft = entities[ 1 ]-%is_draft
**                           Bankcode = entities[ 1 ]-Bankcode ) ).
*  ENDMETHOD.

  METHOD get_instance_features.
    TYPES : BEGIN  OF lty_bank ,
            bankcode TYPE bankk,
            is_draft TYPE abp_behv_flag,
            exist(1),
            END OF lty_bank.
    DATA : lt_bank TYPE TABLE OF lty_bank.
*    AUTHORITY-CHECK OBJECT 'OBJ'
*
*                    FOR USER sy-uname
*                    FIELD 'BANKCODE' .


    SELECT DISTINCT data~*
*                    COUNT(  )
    FROM @KEYS as data
    INNER JOIN zeho_a_comp as comp
                 ON data~Bankcode = comp~bankcode
             INTO TABLE @DATA(lt_keys).


       result = VALUE #( FOR key IN lt_keys  (
                        %is_DRAFT = KEY-%is_draft
                        Bankcode  = KEY-Bankcode
                        %features-%delete  = if_abap_behv=>fc-o-disabled
                        ) ).



  ENDMETHOD.

  METHOD checkvalues.
  READ ENTITIES OF ZEHO_I_SINGLETON IN LOCAL MODE
    ENTITY Bank
   FIELDS (  Bankcode bank_desc )
        WITH CORRESPONDING #( keys )
      RESULT DATA(Banks).
*    if 1 = 2.
*
*    endif.
*        READ ENTITIES OF ZEHO_I_BANK IN LOCAL MODE
*      ENTITY Bank
*        FIELDS ( Bankcode bank_desc  )
*        WITH CORRESPONDING #( keys )
*      RESULT DATA(travels).

  ENDMETHOD.

ENDCLASS.

CLASS lhc_Comps DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS get_global_features FOR GLOBAL FEATURES
      IMPORTING REQUEST requested_features FOR Comp RESULT result.


ENDCLASS.

CLASS lhc_Comps IMPLEMENTATION.

  METHOD get_global_features.
  ENDMETHOD.





ENDCLASS.

CLASS lsc_ZEHO_I_SINGLETON DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZEHO_I_SINGLETON IMPLEMENTATION.

  METHOD save_modified.



  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.


CLASS lhc_serv DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS get_global_features FOR GLOBAL FEATURES
      IMPORTING REQUEST requested_features FOR Serv RESULT result.

ENDCLASS.

CLASS lhc_serv IMPLEMENTATION.

  METHOD get_global_features.
  ENDMETHOD.

ENDCLASS.
