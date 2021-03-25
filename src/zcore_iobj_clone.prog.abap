*&---------------------------------------------------------------------*
*& Report zcore_iobj_clone
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcore_iobj_clone.

DATA:
  lv_iobjnm TYPE rsiobjnm.

SELECTION-SCREEN BEGIN OF BLOCK clone WITH FRAME TITLE TEXT-300.

SELECT-OPTIONS:
  pt_iobj FOR lv_iobjnm NO INTERVALS.

PARAMETERS:
  pv_rfc  TYPE rfcdest DEFAULT 'NONE',
  pv_vers TYPE rsobjvers DEFAULT rs_c_objvers-active,
  pv_targ TYPE zcore_cluster DEFAULT 'Z',
  pv_prex type char2.

SELECTION-SCREEN END OF BLOCK clone.

PARAMETERS:
  pv_simu  TYPE rs_bool DEFAULT rs_c_true,
  pv_noact TYPE rs_bool DEFAULT rs_c_true.

START-OF-SELECTION.

  zcl_core_iobj_tool=>gv_simulation = pv_simu.
  zcl_core_iobj_tool=>static_reset_all( ).
  zcl_core_iobj_tool=>static_set_infoarea( |{ zcl_core_basis_tools=>get_c( 'PREFIX_CLUSTER' ) }{ pv_targ }| ).

  zcl_core_iobj_tool=>gv_prefix = pv_prex.

  CALL METHOD zcl_core_iobj_tool=>static_do_insertlist(
      it_iobjnm         = pt_iobj[]
      iv_objvers        = pv_vers
      iv_rfcdest        = pv_rfc
      iv_target_cluster = pv_targ ).
  CALL METHOD zcl_core_iobj_tool=>static_execute_cloning( pv_noact ).
