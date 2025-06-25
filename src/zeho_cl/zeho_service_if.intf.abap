INTERFACE zeho_service_if
  PUBLIC .
  DATA : t_reqm TYPE zeho_tt_reqm.
  DATA : t_respm TYPE zeho_tt_respm.
  DATA : t_serv  TYPE zeho_tt_serv.
  DATA : t_accounts TYPE zeho_tt_acc.
  DATA : t_xmltab  TYPE ZEHO_TT_XML_TABLE.
  DATA : t_aa       TYPE zeho_tt_aa.
  DATA : t_actt    TYPE zeho_tt_actt.
  DATA : t_exp     TYPE zeho_tt_exp.
  DATA : t_cust   TYPE zeho_tt_customer.
  DATA : t_suppl  TYPe zeho_tt_supplier.
  DATA : tt_activities TYPE zeho_tt_activities.
  data XML type STRING .
  data XML_RES type STRING .
  data XML_RESX type XSTRING .
  data HTTP_CLIENT type ref to IF_WEB_HTTP_CLIENT .
  DATA response TYPE REF TO IF_WEB_HTTP_RESPONSE.
  DATA request  TYPE rEF TO if_web_http_request.

   constants C_REQVALUE type STRING value '~request_method' ##NO_TEXT.
  constants C_POST type STRING value 'POST' ##NO_TEXT.
  constants C_SERVERPROTOCOL type STRING value '~server_protocol' ##NO_TEXT.
  constants C_HTTP type STRING value 'HTTP/1.1' ##NO_TEXT.
  constants C_HOST type STRING value 'HOST' ##NO_TEXT.
  constants C_CONTENTTYPE type STRING value 'Content-Type' ##NO_TEXT.
  constants C_CONTENTLENGTH type STRING value 'Content-Length' ##NO_TEXT.
  constants C_SOAPACTION type STRING value 'SOAPAction' ##NO_TEXT.
  constants C_ENCODING type ABAP_ENCODING value '4103' ##NO_TEXT.
  CONSTANTS c_soap(4)  VALUE 'soap'.
*  constants C_BADI type EXIT_DEF value 'zsoa_eho_REG_IMP' ##NO_TEXT.

  constants C_ACCEPT type STRING value 'Accept' ##NO_TEXT.
  constants C_ACCEPTVAL11 type STRING value 'text/xml' ##NO_TEXT.
*  CONSTANTS c_badi TYPE EXIT_DEF value 'ZEHO_BADI_SERVICE'.


   methods CREATE_REQ
    importing
      !RD_ACC type ref to zeho_s_acc optional
      !BEGDATE type DATUM optional
      !ENDDATE type DATUM optional.
*      !LOG type ref to ZSOA_EHO_CL_APP_LOG optional .
  methods SEND_REQ
    importing
      !BEGDATE type DATUM
      !ENDDATE type DATUM
      !rd_acc  TYPe REF TO zeho_s_acc.
  methods GET_RES
    importing
      !RD_ACC type ref to zeho_s_acc
    RAISING
      zeho_cl_messages.
*      !LOG type ref to ZSOA_EHO_CL_APP_LOG .

ENDINTERFACE.
