FUNCTION zeho_fm_modify_customization.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(TT_CUST) TYPE  ZEHO_TT_ACT_CUSTOM OPTIONAL
*"  RAISING
*"      CX_SY_ZERODIVIDE
*"     RESUMABLE(CX_SY_ASSIGN_CAST_ERROR)
*"----------------------------------------------------------------------








  DATA lt_cust TYPE TABLE OF zeho_a_actcust.
  if tt_cust is not initial.
     lt_cust = VALUE #( for cust IN tt_cust
                       ( CORRESPONDING #( cust ) )
                       ).
    MODIFY  zeho_a_actcust FROM TABLE @lt_cust.
  endif.


*IMPORT
*tt_cust TYPE TABLE OF zeho_a_





ENDFUNCTION.
