CLASS lcl_buffer DEFINITION CREATE PRIVATE .

  PUBLIC SECTION.


    TYPES : BEGIN OF ts_message,
              bankcode TYPE zeho_i_activities_custom-bankcode,
              bukrs    TYPE zeho_i_activities_custom-bukrs,
              act_date TYPE zeho_i_activities_custom-act_date,
              act_time TYPE zeho_i_activities_custom-act_time,
              act_no   TYPE zeho_i_activities_custom-act_no,
              iban     TYPE zeho_i_activities_custom-iban,
              branch   TYPE zeho_i_activities_custom-branch,
              symsg    TYPE symsg,
              fields   TYPE string_table,
            END OF ts_message,

            ty_activities      TYPE STANDARD TABLE OF zeho_i_activities_custom,
            tt_activity_in     TYPE TABLE FOR READ IMPORT zeho_i_activities_custom,
            tt_activity_out    TYPE TABLE FOR READ RESULT zeho_i_activities_custom,
            tt_activity_failed TYPE TABLE FOR FAILED zeho_i_activities_custom,
            tt_message         TYPE STANDARD TABLE OF ts_message.

    CLASS-METHODS get_instance
      RETURNING VALUE(ro_instance) TYPE REF TO lcl_buffer.

    METHODS get_data
      IMPORTING it_activity        TYPE tt_activity_in OPTIONAL
      EXPORTING et_acitivity       TYPE tt_activity_out
                et_activity_failed TYPE tt_activity_failed.

*    METHOD read_activity.
*                et_message         TYPE tt_message .


  PRIVATE SECTION.
    CLASS-DATA: go_instance TYPE REF TO lcl_buffer.
    DATA : tt_activity TYPE ty_activities.
ENDCLASS.


CLASS lcl_buffer IMPLEMENTATION.
  METHOD get_instance.
    IF go_instance IS NOT BOUND.
      go_instance = NEW #( ).
    ENDIF.
    ro_instance = go_instance.
  ENDMETHOD.

  METHOD get_data.
    DATA lt_activity  TYPE TABLE FOR READ IMPORT zeho_i_activities_custom.
    CLEAR lt_activity.
    IF it_activity IS SUPPLIED.
*       lt_activity = it_activity.
      IF tt_activity IS NOT INITIAL.

        LOOP AT it_activity ASSIGNING FIELD-SYMBOL(<fS_activity>).
          IF line_exists( tt_activity[  bankcode                   = <fS_activity>-bankcode
                                        bukrs                      = <fS_activity>-bukrs
                                        iban                       = <fS_activity>-iban
                                        branch                     = <fS_activity>-branch
                                        acccount_no                = <fS_activity>-acccount_no
                                        act_date                   = <fS_activity>-act_date
                                        act_time                   = <fS_activity>-act_time
                                        act_no                     = <fS_activity>-act_no  ] ) .
          ELSE.
            APPEND <fS_activity> TO lt_activity.

          ENDIF.
        ENDLOOP.

      ELSE.
        lt_activity = it_activity.
      ENDIF.

      zeho_cl_post_buffer=>get_instance(  )->get_data(
        EXPORTING
          it_activity        = lt_activity
        IMPORTING
          et_acitivity       = et_acitivity
          et_activity_failed = et_activity_failed
      ).

      IF et_acitivity IS NOT INITIAL.
        tt_activity = CORRESPONDING #( BASE (  tt_activity ) et_acitivity ).
      ENDIF.

    ENDIF.

  ENDMETHOD.

ENDCLASS.



CLASS lhc_Activities DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PUBLIC SECTION.
    TYPES : ty_activity TYPE zeho_tt_activities.
    CLASS-DATA tt_act TYPE zeho_tt_activities.
    DATA lo_buffer TYPE REF TO lcl_buffer.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Activities RESULT result.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE Activities.

    METHODS read FOR READ
      IMPORTING keys FOR READ Activities RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK Activities.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Activities RESULT result.
    METHODS multiplesave FOR MODIFY
      IMPORTING keys FOR ACTION activities~multiplesave RESULT result.

    METHODS reverseactivity FOR MODIFY
      IMPORTING keys FOR ACTION activities~reverseactivity RESULT result.

    METHODS saveactivity FOR MODIFY
      IMPORTING keys FOR ACTION activities~saveactivity RESULT result.



ENDCLASS.

CLASS lhc_Activities IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD update.
  ENDMETHOD.

  METHOD read.


    DATA(lo_buffer) = lcl_buffer=>get_instance( ).

    lo_buffer->get_data(
      EXPORTING
        it_activity        = keys
      IMPORTING
        et_acitivity       = result
        et_activity_failed = failed-activities
*         et_message         =
    ).
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

  METHOD get_instance_features.
    READ ENTITIES OF zeho_i_activities_custom IN LOCAL MODE
   ENTITY Activities
    ALL FIELDS WITH
      CORRESPONDING #( keys )
   RESULT DATA(activities)
   FAILED failed.

    result = VALUE #( FOR activity IN activities
                   ( bankcode                   = activity-bankcode
                     bukrs                      = activity-bukrs
                     iban                       = activity-iban
                     branch                     = activity-branch
                     acccount_no                = activity-acccount_no
                     act_date                   = activity-act_date
                     act_time                   = activity-act_time
                     act_no                     = activity-act_no

                     %features-%update      = COND #( WHEN  activity-belnr <> ' '
                                                     THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled   )
                     %action-saveActivity           = COND #( WHEN  activity-belnr <> ' '
                                                     THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled   )
                     %action-multipleSave           = COND #( WHEN  activity-belnr <> ' '
                                                     THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled   )
                     %action-reverseActivity        = COND #( WHEN  activity-belnr <> ' '
                                                     THEN if_abap_behv=>fc-o-enabled ELSE if_abap_behv=>fc-o-disabled   )
                 ) ).



  ENDMETHOD.

  METHOD multipleSave.
  ENDMETHOD.

  METHOD reverseActivity.
  ENDMETHOD.

  METHOD saveActivity.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZEHO_I_ACTIVITIES_CUSTOM DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZEHO_I_ACTIVITIES_CUSTOM IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.
  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.
