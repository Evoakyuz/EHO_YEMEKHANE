CLASS lhc_EXP DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR exp RESULT result.
    METHODS getdefaultsforcreate FOR READ
      IMPORTING keys FOR FUNCTION exp~getdefaultsforcreate RESULT result.
    METHODS validate_record FOR VALIDATE ON SAVE
      IMPORTING keys FOR exp~validate_record.

ENDCLASS.

CLASS lhc_EXP IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD GetDefaultsForCreate.
    DATA : lt_pur_con TYPE TABLE FOR READ RESULT  zeho_i_exp\\exp .
    APPEND INITIAL LINE TO lt_pur_con ASSIGNING FIELD-SYMBOL(<fs_acct>).
    <fs_acct>-Dc = 'A'.
    result = VALUE #( FOR <fs_rec_m> IN lt_pur_con
                         (

                            %cid = keys[ 1 ]-%cid
                            %param-Dc = <fs_rec_m>-Dc )
                         ) .

  ENDMETHOD.

  METHOD validate_record.

    READ ENTITIES OF zeho_i_exp IN LOCAL MODE
    ENTITY exp
      FIELDS (  Bankcode Bukrs ActivityType Explanation Dc )
      WITH CORRESPONDING #( keys )
    RESULT DATA(Exp).

    SELECT exp~bankcode ,
           exp~bukrs    ,
           exp~activity_type ,
           exp~explanation,
           exp~dc
     FROM zeho_a_exp AS exp
     INNER JOIN @Exp AS data
            ON data~Bankcode       = exp~bankcode
           AND data~bukrs          = exp~bukrs
           AND data~ActivityType  = exp~activity_type
           AND data~explanation    = exp~explanation
           AND data~dc             = exp~dc
   INTO TABLE @DATA(lt_exp).

    LOOP AT Exp INTO DATA(ls_exp).
      IF line_exists( lt_exp[ Bankcode       = ls_exp-Bankcode
                               Bukrs         = ls_exp-Bukrs
                               activity_type = ls_exp-ActivityType
                               explanation   = ls_exp-explanation
                               dc            = ls_exp-Dc
                                ] ).

        APPEND VALUE #( %tky = ls_exp-%tky ) TO failed-exp.

        APPEND VALUE #( %tky               = ls_exp-%tky
                        %state_area        = 'VALIDATE_RECORD'
                        %msg               = NEW zeho_cl_messages(
                                                                textid     = zeho_cl_messages=>exp_already_exists
                                                                mv_bankcode = ls_exp-Bankcode
                                                                mv_bukrs    = ls_exp-Bukrs
                                                                mv_explanation = ls_exp-explanation
                                                                mv_activity_type = ls_exp-ActivityType
                                                                severity   = if_abap_behv_message=>severity-error )
*                        %element = if_abap_behv=>mk-on
*                        %element-EndDate   = if_abap_behv=>mk-on
                        ) TO reported-exp.

      ENDIF.


    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZEHO_I_EXP DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS adjust_numbers REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZEHO_I_EXP IMPLEMENTATION.

  METHOD adjust_numbers.
      DATA(lt_exp) = mapped-exp.
    IF lt_exp IS NOT INITIAL.
      LOOP AT mapped-exp REFERENCE INTO DATA(lr_exp).
        lr_exp->Bankcode = lr_exp->%tmp-Bankcode.
        lr_exp->Bukrs = lr_exp->%tmp-Bukrs.
        lr_exp->ActivityType = lr_exp->%tmp-ActivityType.
        lr_exp->Explanation = lr_exp->%tmp-Explanation.
        lr_exp->dc = lr_exp->%tmp-Dc.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.
