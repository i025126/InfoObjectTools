*&---------------------------------------------------------------------*
*& Report zcore_iobj_create_from_ex
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcore_iobj_create_from_ex.

DATA
  gv_applnm(30)              TYPE   c.

PARAMETERS:
  p_area   TYPE rsinfoarea,
  p_file   TYPE localfile,
  p_iobjnm TYPE rsiobjnm.


*---------------------------------------------------------------------
* AT SELECTION SCREEN ON VALUE REQUEST
*---------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM do_get_local_file USING p_file.


*---------------------------------------------------------------------
* START-OF-SELECTION
*---------------------------------------------------------------------

START-OF-SELECTION.

  DATA:
    lv_filename TYPE string.

  lv_filename = p_file.

  DATA:
    ls_create_infoobject TYPE bapi6108,
    lt_create_infoobject TYPE STANDARD TABLE OF bapi6108,
    lt_header            TYPE STANDARD TABLE OF char30,
    lv_line              TYPE string,
    lt_line              TYPE STANDARD TABLE OF string,
    ls_data              TYPE char1024,
    lt_data              TYPE STANDARD TABLE OF char1024.

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = lv_filename
      filetype                = 'ASC'
    CHANGING
      data_tab                = lt_data
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  DATA:
    lrs_create_infoobject TYPE REF TO cl_abap_structdescr.


  lrs_create_infoobject ?= cl_abap_structdescr=>describe_by_name( 'BAPI6108' ).

  lv_line = lt_data[ 1 ].

  SPLIT lv_line AT ';' INTO TABLE lt_header.
  LOOP AT lt_header ASSIGNING FIELD-SYMBOL(<lv_header>).
    <lv_header> = to_upper( <lv_header> ).
  ENDLOOP.

  LOOP AT lt_data INTO lv_line FROM 2.
    SPLIT lv_line AT ';' INTO TABLE lt_line.

    LOOP AT lrs_create_infoobject->get_components(  ) ASSIGNING FIELD-SYMBOL(<ls_create_component>).
      ASSIGN COMPONENT <ls_create_component>-name OF STRUCTURE ls_create_infoobject TO FIELD-SYMBOL(<lv_create_field>).

      READ TABLE lt_header ASSIGNING FIELD-SYMBOL(<lv_header_fld>)
         WITH KEY table_line = <ls_create_component>-name.
      IF sy-subrc <> 0.
        " Field not- provided
        CONTINUE.
      ENDIF.

      READ TABLE lt_line INDEX sy-tabix ASSIGNING FIELD-SYMBOL(<lv_input_field>).

      <lv_create_field> = <lv_input_field>.
    ENDLOOP.

    APPEND ls_create_infoobject TO lt_create_infoobject.

  ENDLOOP.

  LOOP AT lt_create_infoobject ASSIGNING FIELD-SYMBOL(<ls_create_infoobject>).
    IF <ls_create_infoobject>-infoobject IS INITIAL.
      MESSAGE |Record { sy-tabix }: InfoObject not filled| TYPE rs_c_info.
      DELETE lt_create_infoobject.
      CONTINUE.
    ELSEIF <ls_create_infoobject>-type IS INITIAL.
      MESSAGE |Record { sy-tabix }: InfoObject type not filled| TYPE rs_c_info.
      DELETE lt_create_infoobject.
      CONTINUE.
    ENDIF.

    IF <ls_create_infoobject>-chatp IS INITIAL.
      <ls_create_infoobject>-chatp = 'GEN'.
    ENDIF.

    IF p_iobjnm IS NOT INITIAL.
      IF <ls_create_infoobject>-infoobject <> p_iobjnm.
        DELETE lt_create_infoobject.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF <ls_create_infoobject>-chabasnm IS INITIAL.
      IF <ls_create_infoobject>-intlen IS INITIAL.
        <ls_create_infoobject>-intlen = <ls_create_infoobject>-leng.
      ENDIF.
      IF <ls_create_infoobject>-outputlen IS INITIAL.
        <ls_create_infoobject>-outputlen = <ls_create_infoobject>-intlen.
      ENDIF.
    ENDIF.

    if <ls_create_infoobject>-chabasnm is INITIAL.
      <ls_create_infoobject>-chabasnm = <ls_create_infoobject>-infoobject.
    else.
      if <ls_create_infoobject>-chabasnm <> <ls_create_infoobject>-infoobject.
        <ls_create_infoobject>-BCHREFFL = 'X'.
      endif.
    endif.

    IF <ls_create_infoobject>-infoarea IS INITIAL.
      <ls_create_infoobject>-infoarea = p_area.
    ENDIF.

  ENDLOOP.

*  CALL METHOD cl_demo_output=>display( lt_create_infoobject ).

  LOOP AT lt_create_infoobject ASSIGNING <ls_create_infoobject>.

    DATA:
      lt_infoobject_activate TYPE STANDARD TABLE OF bapi6108io,
      lt_bapiret2            TYPE STANDARD TABLE OF bapiret2,
      ls_bapiret2            TYPE bapiret2.

    CALL FUNCTION 'BAPI_IOBJ_CREATE'
      EXPORTING
        details = <ls_create_infoobject>
      IMPORTING
        return  = ls_bapiret2.
    IF ls_bapiret2-id = 'R7' AND ls_bapiret2-number = '276'.
      CALL FUNCTION 'BAPI_IOBJ_CHANGE'
        EXPORTING
          details    = <ls_create_infoobject>
          infoobject = <ls_create_infoobject>-infoobject
        IMPORTING
          return     = ls_bapiret2.
    ENDIF.

    DATA(lr_iobjnm) = cl_rsd_iobj=>factory( <ls_create_infoobject>-infoobject ).
    CALL METHOD lr_iobjnm->if_rso_tlogo_maintain~check
      EXPORTING
        i_objvers = 'M'
      IMPORTING
        e_r_msg   = DATA(lr_msg)
        e_subrc   = DATA(lv_subrc).

    IF lv_subrc = 0.
      APPEND <ls_create_infoobject>-infoobject TO lt_infoobject_activate.
      APPEND ls_bapiret2 TO lt_bapiret2.
    ELSE.
      LOOP AT lr_msg->get_all_msg(  ) INTO DATA(ls_msg).
        ls_bapiret2-id         = ls_msg-msgid.
        ls_bapiret2-type       = ls_msg-msgty.
        ls_bapiret2-number     = ls_msg-msgno.
        ls_bapiret2-message_v1 = ls_msg-msgv1.
        ls_bapiret2-message_v2 = ls_msg-msgv2.
        ls_bapiret2-message_v3 = ls_msg-msgv3.
        ls_bapiret2-message_v4 = ls_msg-msgv4.
        MESSAGE ID ls_msg-msgid TYPE ls_msg-msgty NUMBER ls_msg-msgno WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4 INTO ls_bapiret2-message.
        APPEND ls_bapiret2 TO lt_bapiret2.
      ENDLOOP.
    ENDIF.

  ENDLOOP.

  DATA:
    lt_bapiret2_activate TYPE STANDARD TABLE OF bapiret2.

  CALL FUNCTION 'BAPI_IOBJ_ACTIVATE_MULTIPLE'
    TABLES
      infoobjects = lt_infoobject_activate
      return      = lt_bapiret2_activate.

  APPEND LINES OF lt_bapiret2_activate TO lt_bapiret2.

  CALL METHOD cl_demo_output=>display( lt_bapiret2 ).

*&---------------------------------------------------------------------*
*&      FORM  HELP_LOCAL_FILE
*&---------------------------------------------------------------------*
*       LOCAL FILE PATH
*----------------------------------------------------------------------*
*      -->FILENAME
*----------------------------------------------------------------------*

FORM do_get_local_file
   USING cv_filename TYPE localfile.

  DATA: lt_file_table TYPE filetable,
        ls_file_table LIKE LINE OF lt_file_table,
        lv_rc         TYPE i,
        lv_pcdsn      TYPE cffile-filename.

  REFRESH lt_file_table.
  CLEAR ls_file_table.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    CHANGING
      file_table = lt_file_table
      rc         = lv_rc.

  READ TABLE lt_file_table INTO ls_file_table INDEX 1.
  IF sy-subrc = 0 .
    MOVE ls_file_table-filename TO cv_filename.
  ELSE.
    CLEAR cv_filename.
  ENDIF.

ENDFORM.                    " HELP_LOCAL_FILE
