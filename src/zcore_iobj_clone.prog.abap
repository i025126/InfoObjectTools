*&---------------------------------------------------------------------*
*& Report zcore_iobj_clone
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcore_iobj_clone.

DATA:
  lv_iobjnm TYPE rsiobjnm.

SELECTION-SCREEN BEGIN OF BLOCK all WITH FRAME TITLE TEXT-100.

PARAMETERS:
  pv_clone TYPE rs_bool RADIOBUTTON GROUP gr1.

SELECTION-SCREEN BEGIN OF BLOCK clone WITH FRAME TITLE TEXT-300.

SELECT-OPTIONS:
  pt_iobj FOR lv_iobjnm NO INTERVALS.

PARAMETERS:
  pv_rfc  TYPE rfcdest DEFAULT 'NONE',
  pv_vers TYPE rsobjvers DEFAULT rs_c_objvers-active,
  pv_targ TYPE zcore_models DEFAULT zcl_core_iobj_tool=>cs_models-common.

SELECTION-SCREEN END OF BLOCK clone.

PARAMETERS:
  pv_area type rsinfoarea DEFAULT zcl_core_iobj_tool=>gv_infoarea,
  pv_afix TYPE rs_bool RADIOBUTTON GROUP gr1.

PARAMETERS:
  pv_simu  type rs_bool DEFAULT rs_c_true,
  pv_noact type rs_bool DEFAULT rs_c_true.

SELECTION-SCREEN END OF BLOCK all.

START-OF-SELECTION.

  zcl_core_iobj_tool=>gv_simulation = pv_simu.
  zcl_core_iobj_tool=>static_reset_all( ).
  zcl_core_iobj_tool=>static_set_infoarea( pv_area ).

  IF pv_clone = rs_c_true.
    CALL METHOD zcl_core_iobj_tool=>static_do_insertlist(
        it_iobjnm       = pt_iobj[]
        iv_objvers      = pv_vers
        iv_rfcdest      = pv_rfc
        iv_target_model = pv_targ ).
    CALL METHOD zcl_core_iobj_tool=>static_execute_cloning( pv_noact ).
  ENDIF.
