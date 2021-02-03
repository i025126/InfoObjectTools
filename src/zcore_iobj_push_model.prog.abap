*&---------------------------------------------------------------------*
*& Report zcore_iobj_push_model
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcore_iobj_push_model.
* Concept of this is to move one or more InfoObjects to another
* system - It originally created for the purpose of moving the
* 'D' cluster to WDS. 'D' is a legacy ERP that was so old that the
* extraction had to be done into WDS before being move to BW/4.
* The data model was created in BW/4 and moved to WDS
DATA lv_iobjnm TYPE rsiobjnm.

SELECT-OPTIONS:
  pt_iobj FOR lv_iobjnm NO INTERVALS.

PARAMETERS:
  pv_rfc TYPE rfcdest DEFAULT 'NONE'.


CLASS lcl_log DEFINITION.

  PUBLIC SECTION.

    CLASS-DATA:
      gv_log_handle   TYPE balloghndl.

    CLASS-METHODS:
      static_add_message
        IMPORTING
          is_bapi TYPE bapiret2 OPTIONAL
          is_msg  TYPE bal_s_msg OPTIONAL,
      static_display_log.

ENDCLASS.

CLASS lcl_log IMPLEMENTATION.

  METHOD static_display_log.

    DATA:
      ls_display_profile TYPE bal_s_prof.

* get variant which creates hierarchy according to field DETLEVEL
    CALL FUNCTION 'BAL_DSP_PROFILE_DETLEVEL_GET'
      IMPORTING
        e_s_display_profile = ls_display_profile
      EXCEPTIONS
        OTHERS              = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    DATA lt_log_handle TYPE bal_t_logh.
    APPEND gv_log_handle TO lt_log_handle.
    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile  = ls_display_profile
        i_t_log_handle       = lt_log_handle
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        OTHERS               = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD static_add_message.

    IF gv_log_handle IS INITIAL.
*-Logging messages
      DATA:
        lv_timestamp TYPE tzntstmps,
        lv_timezone  TYPE timezone VALUE 'UTC',
        ls_log       TYPE bal_s_log.

      CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP lv_timestamp TIME ZONE lv_timezone.

      ls_log-extnumber = lv_timestamp.
      CONDENSE ls_log-extnumber.
      ls_log-object = 'RSD'.
      ls_log-subobject = 'COPY'.
      ls_log-aldate_del = sy-datum + 5.

      CALL FUNCTION 'BAL_LOG_CREATE'
        EXPORTING
          i_s_log                 = ls_log
        IMPORTING
          e_log_handle            = gv_log_handle
        EXCEPTIONS
          log_header_inconsistent = 1
          OTHERS                  = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        RETURN.
      ENDIF.
    ENDIF.

    DATA ls_msg TYPE bal_s_msg.

    IF is_bapi IS SUPPLIED and is_bapi is not INITIAL.
      DATA _message TYPE string.
      MESSAGE ID is_bapi-id TYPE is_bapi-type NUMBER is_bapi-number WITH
               is_bapi-message_v1 is_bapi-message_v2 is_bapi-message_v3 is_bapi-message_v4 INTO _message.
    ENDIF.

    IF is_msg IS SUPPLIED.
      ls_msg = is_msg.
    ELSE.
      MOVE-CORRESPONDING syst TO ls_msg.
    ENDIF.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = gv_log_handle
        i_s_msg      = ls_msg
      EXCEPTIONS
        OTHERS       = 4.
    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  TYPES:
    ltyt_compounds                TYPE STANDARD TABLE OF bapi6108cm WITH NON-UNIQUE DEFAULT KEY,
    ltyt_attributes               TYPE STANDARD TABLE OF bapi6108at WITH NON-UNIQUE DEFAULT KEY,
    ltyt_navigationattributes     TYPE STANDARD TABLE OF bapi6108an WITH NON-UNIQUE DEFAULT KEY,
    ltyt_atrnavinfoprovider       TYPE STANDARD TABLE OF bapi6108np WITH NON-UNIQUE DEFAULT KEY,
    ltyt_hierarchycharacteristics TYPE STANDARD TABLE OF bapi6108hc WITH NON-UNIQUE DEFAULT KEY,
    ltyt_elimination              TYPE STANDARD TABLE OF bapi6108ie WITH NON-UNIQUE DEFAULT KEY,
    BEGIN OF ltys_infoobject,
      infoobject               TYPE rsiobjnm,
      details                  TYPE bapi6108,
      details_2                TYPE bapi6108_2,
      return_read              TYPE bapiret2,
      return_write             TYPE bapiret2,
      return_get               TYPE bapiret2,
      compounds                TYPE ltyt_compounds,
      attributes               TYPE ltyt_attributes,
      navigationattributes     TYPE ltyt_navigationattributes,
      atrnavinfoprovider       TYPE ltyt_atrnavinfoprovider,
      hierarchycharacteristics TYPE ltyt_hierarchycharacteristics,
      elimination              TYPE ltyt_elimination,
    END OF ltys_infoobject,
    ltyt_infoobjects TYPE STANDARD TABLE OF ltys_infoobject WITH NON-UNIQUE DEFAULT KEY.

  DATA gt_infoobjects TYPE ltyt_infoobjects.
  DATA lt_iobjnm      TYPE STANDARD TABLE OF rsiobjnm WITH NON-UNIQUE DEFAULT KEY.

  SELECT iobjnm
      FROM rsdiobj
      INTO TABLE @lt_iobjnm
      WHERE objvers = @rs_c_objvers-active AND
            iobjnm IN @pt_iobj.

  LOOP AT lt_iobjnm INTO DATA(ls_iobj).

    APPEND INITIAL LINE TO gt_infoobjects ASSIGNING FIELD-SYMBOL(<ls_infoobject>).
    <ls_infoobject>-infoobject = ls_iobj.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = |InfoObject { <ls_infoobject>-infoobject }|.

    CALL FUNCTION 'BAPI_IOBJ_GETDETAIL'
      EXPORTING
        infoobject               = <ls_infoobject>-infoobject
        version                  = rs_c_objvers-active
      IMPORTING
        details                  = <ls_infoobject>-details
        details_2                = <ls_infoobject>-details_2
        return                   = <ls_infoobject>-return_read
      TABLES
        compounds                = <ls_infoobject>-compounds
        attributes               = <ls_infoobject>-attributes
        navigationattributes     = <ls_infoobject>-navigationattributes
        atrnavinfoprovider       = <ls_infoobject>-atrnavinfoprovider
        hierarchycharacteristics = <ls_infoobject>-hierarchycharacteristics
        elimination              = <ls_infoobject>-elimination.

    MESSAGE <ls_infoobject>-return_read-message TYPE 'S' .
  ENDLOOP.

  LOOP AT gt_infoobjects ASSIGNING <ls_infoobject>.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = |InfoObject { <ls_infoobject>-infoobject }|.

    DO 2 TIMES.
      IF sy-index = 1.
        DATA(lv_objvers) = rs_c_objvers-modified.
      ELSE.
        lv_objvers       = rs_c_objvers-active.
      ENDIF.
      CALL FUNCTION 'BAPI_IOBJ_GETDETAIL'
        DESTINATION pv_rfc
        EXPORTING
          infoobject            = <ls_infoobject>-infoobject
          version               = lv_objvers
        IMPORTING
          return                = <ls_infoobject>-return_get
        EXCEPTIONS
          system_failure        = 2 MESSAGE <ls_infoobject>-return_get-message
          communication_failure = 3.
      IF sy-subrc <> 0.
        " This is a crash
        CALL METHOD lcl_log=>static_add_message.
      ELSE.
        IF <ls_infoobject>-return_get-number = '000'.
          CALL METHOD lcl_log=>static_add_message
            EXPORTING
              is_bapi = <ls_infoobject>-return_get.
          EXIT.
        ENDIF.
      ENDIF.
    ENDDO.

    IF <ls_infoobject>-return_get-number IS INITIAL.
      " Info Object already exists
      CALL FUNCTION 'BAPI_IOBJ_CHANGE'
        DESTINATION pv_rfc
        EXPORTING
          infoobject               = <ls_infoobject>-infoobject
          details                  = <ls_infoobject>-details
          details_2                = <ls_infoobject>-details_2
        IMPORTING
          return                   = <ls_infoobject>-return_write
        TABLES
          compounds                = <ls_infoobject>-compounds
          attributes               = <ls_infoobject>-attributes
          navigationattributes     = <ls_infoobject>-navigationattributes
          atrnavinfoprovider       = <ls_infoobject>-atrnavinfoprovider
          hierarchycharacteristics = <ls_infoobject>-hierarchycharacteristics
          elimination              = <ls_infoobject>-elimination
        EXCEPTIONS
          system_failure           = 2 MESSAGE <ls_infoobject>-return_write-message
          communication_failure    = 3.
    ELSE.
      CALL FUNCTION 'BAPI_IOBJ_CREATE'
        DESTINATION pv_rfc
        EXPORTING
          details                  = <ls_infoobject>-details
          details_2                = <ls_infoobject>-details_2
        IMPORTING
          infoobject               = lv_iobjnm
          return                   = <ls_infoobject>-return_write
        TABLES
          compounds                = <ls_infoobject>-compounds
          attributes               = <ls_infoobject>-attributes
          navigationattributes     = <ls_infoobject>-navigationattributes
          atrnavinfoprovider       = <ls_infoobject>-atrnavinfoprovider
          hierarchycharacteristics = <ls_infoobject>-hierarchycharacteristics
          elimination              = <ls_infoobject>-elimination
        EXCEPTIONS
          system_failure           = 2 MESSAGE <ls_infoobject>-return_write-message
          communication_failure    = 3.
    ENDIF.

    IF sy-subrc <> 0.
      CALL METHOD lcl_log=>static_add_message.
    ELSE.
      CALL METHOD lcl_log=>static_add_message
        EXPORTING
          is_bapi = <ls_infoobject>-return_write.
    ENDIF.

  ENDLOOP.

  CALL METHOD lcl_log=>static_display_log.
