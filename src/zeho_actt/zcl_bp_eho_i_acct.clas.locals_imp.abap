CLASS lhc_ACCT DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR acct RESULT result.
    METHODS GetDefaultsForCreate FOR READ
      IMPORTING keys FOR FUNCTION acct~getdefaultsforcreate RESULT result.
    METHODS validate_record FOR VALIDATE ON SAVE
      IMPORTING keys FOR acct~validate_record.
    METHODS handle_mybox FOR DETERMINE ON MODIFY
      IMPORTING keys FOR acct~handle_mybox.

*    METHODS handle_box FOR MODIFY
*     IMPORTING keys FOR acct~handle_box .
*      IMPORTING keys FOR acct~handle_box.
*    METHODS DefaultForCreate FOR READ
*      IMPORTING keys FOR FUNCTION ACCT~DefaultForCreate RESULT result.

ENDCLASS.

CLASS lhc_ACCT IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD GetDefaultsForCreate.

    DATA : lt_pur_con TYPE TABLE FOR READ RESULT  zeho_i_acct\\acct .
    APPEND INITIAL LINE TO lt_pur_con ASSIGNING FIELD-SYMBOL(<fs_acct>).
    <fs_acct>-Dc = 'A'.
    <fs_acct>-Priority = 1.
    CLEAR : <fs_acct>-Bankcode,
            <fs_acct>-Bukrs.
    result = VALUE #( FOR <fs_rec_m> IN lt_pur_con
                         (

                            %cid = keys[ 1 ]-%cid
                            %param-Priority = <fs_rec_m>-Priority
                            %param-Dc = <fs_rec_m>-Dc
                            %param-Bankcode = <fs_acct>-Bankcode
                            %param-Bukrs = <fs_acct>-Bukrs )
                         ) .

  ENDMETHOD.




  METHOD validate_record.

    READ ENTITIES OF zeho_i_acct IN LOCAL MODE
    ENTITY acct
      FIELDS (  Bankcode Bukrs ActivityType Priority )
      WITH CORRESPONDING #( keys )
    RESULT DATA(Activity).

    SELECT acct~bankcode ,
           acct~bukrs    ,
           acct~activity_type ,
           acct~priority
     FROM zeho_a_actt AS acct
     INNER JOIN @Activity AS data
            ON data~Bankcode = acct~bankcode
           AND data~bukrs    = acct~bukrs
           AND data~ActivityType = acct~activity_type
           AND data~Priority     = acct~priority
   INTO TABLE @DATA(lt_acct).

    LOOP AT Activity INTO DATA(actt).
      IF line_exists( lt_acct[ Bankcode = actt-Bankcode
                               Bukrs    = actt-Bukrs
                               activity_type = actt-ActivityType
                               Priority      = actt-Priority ] ).

        APPEND VALUE #( %tky = actt-%tky ) TO failed-acct.

        APPEND VALUE #( %tky               = actt-%tky
                        %state_area        = 'VALIDATE_RECORD'
                        %msg               =  NEW zeho_cl_messages(
                                                                textid     = zeho_cl_messages=>acct_already_exists
                                                                mv_bankcode = actt-Bankcode
                                                                mv_bukrs    = actt-Bukrs
                                                                mv_priority = actt-Priority
                                                                mv_activity_type = actt-ActivityType
                                                                severity   = if_abap_behv_message=>severity-error )
*                          %msg =  NEW zeho_cl_messages(
*                          textid   = zeho_cl_messages=>amount_cannot_be_null
*                          severity = if_abap_behv_message=>severity-error
*                          mv_amount = lv_amount
*                            )
*                        %element = if_abap_behv=>mk-on
*                        %element-EndDate   = if_abap_behv=>mk-on
                        ) TO reported-acct.

      ENDIF.


    ENDLOOP.
  ENDMETHOD.
*  METHOD handle_box.
*
*        READ ENTITIES OF zeho_i_acct IN LOCAL MODE
*       ENTITY acct
*        FIELDS ( CustomerControl VendorControl VirementControl CancelProcess )
*        WITH CORRESPONDING #( keys )
*       RESULT DATA(BOX).
*
*
*  ENDMETHOD.

  METHOD handle_mybox.
  if 1 = 2.

  endif.
          READ ENTITIES OF zeho_i_acct IN LOCAL MODE
       ENTITY acct
        FIELDS ( CustomerControl VendorControl VirementControl CancelProcess )
        WITH CORRESPONDING #( keys )
       RESULT DATA(BOX).

  ENDMETHOD.



ENDCLASS.

CLASS lsc_ZEHO_I_ACCT DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS adjust_numbers REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZEHO_I_ACCT IMPLEMENTATION.

  METHOD adjust_numbers.


    DATA(lt_acct) = mapped-acct.
    IF lt_acct IS NOT INITIAL.
      LOOP AT mapped-acct REFERENCE INTO DATA(lr_acct).
        lr_acct->Bankcode = lr_acct->%tmp-Bankcode.
        lr_acct->Bukrs = lr_acct->%tmp-Bukrs.
        lr_acct->ActivityType = lr_acct->%tmp-ActivityType.
        lr_acct->Priority = lr_acct->%tmp-Priority.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD cleanup_finalize.
    IF 1 = 1.

    ENDIF.
  ENDMETHOD.

ENDCLASS.
