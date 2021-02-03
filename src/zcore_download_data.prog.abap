*10/20  av/soren  download PRD data
*&---------------------------------------------------------------------*
*& Report zcore_download_data
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcore_download_data.


PARAMETERS:
  pv_path TYPE string VISIBLE LENGTH 40,
  pv_rfc  TYPE rfcdest,
  pv_read TYPE rs_bool DEFAULT rs_c_false.

DATA:
  lv_rstlogo  TYPE rstlogo,
  lv_infoprov TYPE rsinfoprov.

SELECT-OPTIONS:
  pt_tlogo FOR lv_rstlogo,
  pt_obj   FOR lv_infoprov.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR pv_path.

  DATA:
    lv_filename   TYPE string,
    lv_path       TYPE string,
    lv_useraction TYPE i.

  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title    = 'Download to what folder'
    CHANGING
      selected_folder = pv_path.

CLASS lcx_general_error DEFINITION INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    DATA:
      nv_tlogo    TYPE rstlogo,
      nv_infoprov TYPE rsinfoprov.

    INTERFACES if_rs_message .

    ALIASES get_message
      FOR if_rs_message~get_message .

ENDCLASS.

CLASS lcx_general_error IMPLEMENTATION.
  METHOD if_rs_message~get_message .
  ENDMETHOD.
ENDCLASS.

CLASS lcx_unknown_filetype DEFINITION INHERITING FROM lcx_general_error.
  PUBLIC SECTION.
    DATA:
      nv_filetype TYPE string.

    METHODS:
      get_text REDEFINITION,
      constructor
        IMPORTING
          iv_filetype TYPE any.
ENDCLASS.
CLASS lcx_unknown_filetype IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor.
    nv_filetype = iv_filetype.
  ENDMETHOD.
  METHOD get_text.
    result = |Cannot provide a filename for filetype { nv_filetype }|.
  ENDMETHOD.
ENDCLASS.

CLASS lcx_empty_record DEFINITION INHERITING FROM lcx_general_error.
ENDCLASS.

CLASS lcx_max_lines_read DEFINITION INHERITING FROM lcx_general_error.
ENDCLASS.


CLASS lcx_no_data DEFINITION INHERITING FROM lcx_general_error.
  PUBLIC SECTION.

    METHODS:
      get_text REDEFINITION,
      constructor
        IMPORTING
          iv_infoprov TYPE rsinfoprov
          iv_tlogo    TYPE rstlogo.
ENDCLASS.

CLASS lcx_no_data IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor.
    nv_tlogo = iv_tlogo.
    nv_infoprov = iv_infoprov.
  ENDMETHOD.

  METHOD get_text.
    result = |{ nv_tlogo }/{ nv_infoprov }: No data for object|.
  ENDMETHOD.
ENDCLASS.

CLASS lcx_object_unknown DEFINITION INHERITING FROM lcx_general_error.
  PUBLIC SECTION.
    METHODS:
      get_text REDEFINITION,
      constructor
        IMPORTING
          iv_tlogo    TYPE rstlogo
          iv_infoprov TYPE rsinfoprov.
ENDCLASS.

CLASS lcx_no_processing DEFINITION INHERITING FROM lcx_general_error.
ENDCLASS.

CLASS lcx_object_unknown IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor.
    nv_tlogo = iv_tlogo.
    nv_infoprov = iv_infoprov.
  ENDMETHOD.

  METHOD get_text.
    result = |The object type { nv_tlogo }, for object { nv_infoprov } could not be found|.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_download DEFINITION
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      gtyt_rng_infoprov TYPE RANGE OF rsinfoprov,
      gtyt_rng_tlogo    TYPE RANGE OF rstlogo,
      gtyt_structure    TYPE STANDARD TABLE OF dfies WITH NON-UNIQUE DEFAULT KEY.

    TYPES:
      BEGIN OF gtys_infoprov,
        tlogo      TYPE rstlogoprop-tlogo,
        infoprov   TYPE rsinfoprov,
        r_infoprov TYPE REF TO lcl_download,
      END OF gtys_infoprov,
      gtyt_infoprov TYPE STANDARD TABLE OF gtys_infoprov WITH NON-UNIQUE DEFAULT KEY.

    CONSTANTS:
      gv_package_size TYPE i VALUE 100000.

    CLASS-DATA:
      gv_max_len        TYPE i VALUE 1000000000,
      nv_simu           TYPE rs_bool,
      nv_read           TYPE rs_bool,
      nr_zip            TYPE REF TO cl_abap_zip,
      nv_message        TYPE string,
      nv_seperator      TYPE char1 VALUE ';',
      gv_appl_codepage  TYPE tcp00-cpcodepage,
      gv_abap_enconding TYPE abap_encoding.

    DATA:
      nsd_infoprov TYPE REF TO data,
      ntd_infoprov TYPE REF TO data,
      nt_structure TYPE gtyt_structure,
      nt_textstruc TYPE gtyt_structure,
      nv_tlogo     TYPE rstlogo,
      nv_infoprov  TYPE rsinfoprov.

    METHODS:
      setup
        RAISING
          lcx_no_data
          cx_static_check,
      constructor
        IMPORTING
          iv_tlogo    TYPE rstlogo OPTIONAL
          iv_infoprov TYPE rsinfoprov
        RAISING
          cx_static_check.

    CLASS-METHODS:
      class_constructor,
      void
        IMPORTING
          iv_read          TYPE rs_bool
          iv_rfcdest       TYPE rfcdest OPTIONAL
          iv_toworkstation TYPE string
          it_rng_tlogo     TYPE gtyt_rng_tlogo
          it_rng_infoprov  TYPE gtyt_rng_infoprov,
      factory
        IMPORTING
          iv_tlogo           TYPE rstlogo
          iv_infoprov        TYPE rsinfoprov
        RETURNING
          VALUE(rr_infoprov) TYPE REF TO lcl_download
        RAISING
          lcx_object_unknown
          cx_sy_create_object_error
          cx_static_check.

  PROTECTED SECTION.
    METHODS:
      get_objects
        IMPORTING
          it_rng_tlogo       TYPE gtyt_rng_tlogo OPTIONAL
          it_rng_infoprov    TYPE gtyt_rng_infoprov
        RETURNING
          VALUE(rt_infoprov) TYPE gtyt_infoprov,
      get_fieldname
        IMPORTING
          iv_structurename TYPE clike
        RETURNING
          VALUE(rv_iobjnm) TYPE sobj_name,
      do_send_to_rfc
        IMPORTING
          iv_rfcdest TYPE rfcdest
        RAISING
          cx_rs_error,
      do_move_to_workstation
        IMPORTING
          iv_path TYPE string
        RAISING
          lcx_unknown_filetype,
      is_object_empty
        RETURNING
          VALUE(rv_empty) TYPE rs_bool
        RAISING
          lcx_no_data,
      get_filename
        IMPORTING
          iv_filetype        TYPE char1
          iv_counter         TYPE num6 OPTIONAL
          iv_path            TYPE string DEFAULT '/usr/sap/trans/export_prd/'
        RETURNING
          VALUE(rv_filename) TYPE string
        RAISING
          lcx_unknown_filetype,
      get_tabname
        RETURNING
          VALUE(rv_tabname) TYPE tabname
        RAISING
          lcx_no_data
          lcx_object_unknown,
      get_texttable
        RETURNING
          VALUE(rv_tabname) TYPE tabname
        RAISING
          lcx_no_processing
          lcx_no_data
          lcx_object_unknown,
      get_structure_pointer
        EXPORTING
          esd_infoprov TYPE REF TO data
          etd_infoprov TYPE REF TO data
        RAISING
          lcx_no_data
          lcx_object_unknown
          cx_rs_msg,
      get_structure_text
        RETURNING
          VALUE(rt_structure) TYPE gtyt_structure
        RAISING
          lcx_no_processing
          lcx_no_data
          lcx_object_unknown
          cx_rs_msg,
      get_structure_information
        RETURNING
          VALUE(rt_structure) TYPE gtyt_structure
        RAISING
          lcx_no_data
          lcx_object_unknown
          cx_rs_msg.
  PRIVATE SECTION.

    CLASS-METHODS:
      _do_message
        IMPORTING
          iv_message_type TYPE sy-msgty DEFAULT 'S'
          iv_message      TYPE clike OPTIONAL
            PREFERRED PARAMETER iv_message.

    METHODS:
      _read_and_download,
      _delete_old_file,
      _download_structure,
      _download_texttable,
      _do_convert_to_download_format
        IMPORTING
          ird_data       TYPE REF TO data
          it_structure   TYPE gtyt_structure
        RETURNING
          VALUE(rv_line) TYPE string
        RAISING
          lcx_empty_record
          lcx_object_unknown
          cx_rs_error.


ENDCLASS.

CLASS lcl_download IMPLEMENTATION.

  METHOD _delete_old_file.
    DATA lv_filename TYPE string.

    " if read - don't delete the tables
    CHECK nv_read = rs_c_false.

    TRY.
        DO 3 TIMES.
          CASE sy-index.
            WHEN 1.
              lv_filename = get_filename( 'S' ).
            WHEN 2.
              lv_filename = get_filename( 'T' ).
            WHEN 3.
              lv_filename = get_filename( 'D' ).
          ENDCASE.

          OPEN DATASET lv_filename FOR INPUT IN TEXT MODE ENCODING DEFAULT.
          IF sy-subrc NE 0.
            " Dataset does not exists
            CONTINUE.
          ENDIF.
          " Close and delete
          CLOSE DATASET lv_filename.
          DELETE DATASET lv_filename.

          IF sy-subrc NE 0.
            MESSAGE e899(bd) WITH 'INVALID FILE NAME' lv_filename INTO nv_message.
            CALL METHOD _do_message.
          ELSE.
            CLOSE DATASET lv_filename.
            CALL METHOD _do_message( |{ lv_filename } deleted succesfully| ).
          ENDIF.

        ENDDO.
      CATCH lcx_unknown_filetype.
    ENDTRY.

  ENDMETHOD.

  METHOD get_texttable.
    RAISE EXCEPTION TYPE lcx_no_processing.
  ENDMETHOD.

  METHOD get_tabname.
    RAISE EXCEPTION TYPE lcx_object_unknown
      EXPORTING
        iv_tlogo    = nv_tlogo
        iv_infoprov = nv_infoprov.
  ENDMETHOD.

  METHOD is_object_empty.

    rv_empty = rs_c_true.
    DO 2 TIMES.
      DATA(lv_index) = sy-index.
      TRY.
          CASE lv_index.
            WHEN 1.
              DATA(lv_tabname) = get_tabname(  ).
            WHEN 2.
              lv_tabname = get_texttable(  ).
          ENDCASE.
        CATCH lcx_no_data
              lcx_no_processing
              lcx_object_unknown.
          CONTINUE.
      ENDTRY.

      DATA lr_data TYPE REF TO data.
      FIELD-SYMBOLS <lt_data> TYPE STANDARD TABLE.

      CREATE DATA lr_data TYPE STANDARD TABLE OF (lv_tabname).
      ASSIGN lr_data->* TO <lt_data>.

      SELECT *
          FROM (lv_tabname) UP TO 2 ROWS
          INTO TABLE <lt_data>.

      IF sy-dbcnt = 2.
        rv_empty = rs_c_false.
      ELSE.
        rv_empty = rs_c_true.
      ENDIF.
    ENDDO.

  ENDMETHOD.

  METHOD constructor.
    nv_tlogo    = iv_tlogo.
    nv_infoprov = iv_infoprov.
  ENDMETHOD.

  METHOD do_send_to_rfc.

    DATA:
      lv_line     TYPE string,
      lv_longline TYPE string,
      lv_filetype TYPE char1,
      lv_msg      TYPE char255.

    DO 3 TIMES.
      CASE sy-index.
        WHEN 1.
          lv_filetype = 'S'.
        WHEN 2.
          lv_filetype = 'D'.
        WHEN 3.
          lv_filetype = 'T'.
      ENDCASE.

      DATA(lv_filename) = get_filename( lv_filetype ).

      CLEAR lv_longline.
      OPEN DATASET lv_filename FOR INPUT IN TEXT MODE ENCODING UTF-8.
      IF sy-subrc <> 0.
        " File not found, no reason to read and at an empty line
        CONTINUE.
      ENDIF.

      DATA(lv_open) = rs_c_true.
      TRY.
          DO.
            TRY.
                DATA(lv_len) = strlen( lv_longline ).
                WHILE lv_len < 1000000.
                  READ DATASET lv_filename INTO lv_line.
                  IF sy-subrc <> 0.
                    EXIT.
                  ENDIF.
                  IF lv_longline IS INITIAL.
                    lv_longline = lv_line.
                  ELSE.
                    CONCATENATE lv_longline  cl_abap_char_utilities=>cr_lf lv_line INTO lv_longline IN CHARACTER MODE.
                  ENDIF.
                  lv_len = strlen( lv_longline ).
                ENDWHILE.

                IF lv_longline IS INITIAL.
                  RAISE EXCEPTION TYPE cx_rsb_no_more_data.
                ENDIF.

                lv_len = lv_len / 1000.
                lv_len = lv_len * 1000.
                _do_message( |Moving data to { iv_rfcdest } counting { lv_len }| ).

                CALL FUNCTION 'Z_GETDATA_WRITE_FILE'
                  DESTINATION iv_rfcdest
                  EXPORTING
                    iv_open               = lv_open
                    iv_filename           = lv_filename
                    iv_concent            = lv_longline
                  EXCEPTIONS
                    system_failure        = 11 message lv_msg
                    communication_failure = 12 message lv_msg
                    cannot_open_file      = 8
                    cannot_write_to_file  = 9.
                CASE sy-subrc.
                  WHEN 11.
                    _do_message( |System error in { iv_rfcdest } message-> { lv_msg }| ).
                    RAISE EXCEPTION TYPE cx_rs_error.
                  WHEN 12.
                    _do_message( |Communication error in { iv_rfcdest } message-> { lv_msg }| ).
                    RAISE EXCEPTION TYPE cx_rs_error.
                  WHEN 8.
                    _do_message( |Could not open file { lv_filename } on destination { iv_rfcdest }| ).
                    RAISE EXCEPTION TYPE cx_rs_error.
                  WHEN 7.
                    _do_message( |Could not write to file { lv_filename } on destination { iv_rfcdest } bytes { lv_len }| ).
                    RAISE EXCEPTION TYPE cx_rs_error.
                  WHEN OTHERS.
                    _do_message( |{ lv_len } bytes written to { lv_filename } on destination { iv_rfcdest }| ).
                ENDCASE.
                lv_open = rs_c_false.
                CLEAR lv_longline.
            ENDTRY.

          ENDDO.
        CATCH cx_rsb_no_more_data.
      ENDTRY.
      CLOSE DATASET lv_filename.

    ENDDO.


  ENDMETHOD.

  METHOD do_move_to_workstation.
    DATA:
      lv_line     TYPE string,
      lv_longline TYPE string,
      lv_xlong    TYPE xstring,
      lt_line     TYPE STANDARD TABLE OF string,
      lv_filetype TYPE char1,
      lv_folder   TYPE string.

    DO 3 TIMES.
      CASE sy-index.
        WHEN 1.
          lv_filetype = 'S'.
        WHEN 2.
          lv_filetype = 'D'.
        WHEN 3.
          lv_filetype = 'T'.
      ENDCASE.

      DATA(lv_filename) = get_filename( lv_filetype ).

      CLEAR lv_longline.
      OPEN DATASET lv_filename FOR INPUT IN TEXT MODE ENCODING UTF-8.
      IF sy-subrc <> 0.
        " File not found, no reason to read and at an empty line
        CONTINUE.
      ENDIF.

          DATA(lv_zipfile_name) = get_filename(  iv_filetype = lv_filetype iv_path = ''  ).

            DO 1000000 TIMES.
              READ DATASET lv_filename INTO lv_line.
              IF sy-subrc <> 0.
                EXIT.
              ENDIF.
              IF lv_longline IS INITIAL.
                lv_longline = lv_line.
              ELSE.
                CONCATENATE lv_longline  cl_abap_char_utilities=>cr_lf lv_line INTO lv_longline IN CHARACTER MODE.
              ENDIF.
              DATA(lv_len) = strlen( lv_longline ).
              IF lv_len > gv_max_len.
                EXIT.
              ENDIF.
            ENDDO.

            lv_len = lv_len / 1000.
            lv_len = lv_len * 1000.
            _do_message( |Moving data to { lv_zipfile_name } counting { lv_len }| ).

            IF lv_len >= gv_max_len.
              _do_message( |File { lv_zipfile_name } truncated| ).
            ENDIF.
      CLOSE DATASET lv_filename.

      CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
        EXPORTING
          text          = lv_longline
        IMPORTING
          buffer        = lv_xlong
        EXCEPTIONS
          failed        = 1
          error_message = 8
          OTHERS        = 8.
      CALL METHOD nr_zip->add(
          content = lv_xlong
          name    = lv_zipfile_name ).
    ENDDO.

  ENDMETHOD.

  METHOD get_objects.
    SELECT ta~object AS tlogo , substring( ta~obj_name, 1, 30 ) AS infoprov
    FROM tadir AS ta
      INNER JOIN
        rstlogoprop AS p ON
          ta~object = p~tlogo
    WHERE
      pgmid    = 'R3TR' AND
      object   IN @it_rng_tlogo AND
      obj_name IN @it_rng_infoprov
    INTO CORRESPONDING FIELDS OF TABLE @rt_infoprov.
  ENDMETHOD.

  METHOD _download_texttable.
  ENDMETHOD.

  METHOD _do_message.

    IF iv_message IS SUPPLIED.
      IF ( iv_message_type = 'S' OR iv_message_type = 'I' OR iv_message_type = 'W' ).
        IF sy-batch = rs_c_true. "Background processing
          MESSAGE iv_message  TYPE iv_message_type.
        ELSE.
          CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
            EXPORTING
              text = iv_message.
        ENDIF.
      ELSE. " Critical message
        MESSAGE iv_message  TYPE iv_message_type.
      ENDIF.
    ELSE. "" iv_message not supplied
      IF ( sy-msgty = 'S' OR sy-msgty = 'I' OR sy-msgty = 'W' ).
        IF sy-batch = rs_c_true.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO nv_message.
          CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
            EXPORTING
              text = nv_message.
        ENDIF.
      ELSE.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF. " non critical
    ENDIF. "" iv_message not supplied

  ENDMETHOD.

  METHOD _download_structure.

    TRY.
        DATA(lv_filename) = get_filename( 'S' ).

        OPEN DATASET lv_filename FOR OUTPUT IN TEXT MODE ENCODING UTF-8 MESSAGE nv_message.
        IF sy-subrc <> 0.
          MESSAGE nv_message TYPE rs_c_error.
          RAISE EXCEPTION TYPE cx_rs_error.
        ENDIF.

        DATA lt_structure TYPE gtyt_structure.
        DATA lrd_data TYPE REF TO data.
        CALL FUNCTION 'DDIF_FIELDINFO_GET'
          EXPORTING
            tabname   = 'DFIES'
          TABLES
            dfies_tab = lt_structure.

        LOOP AT nt_structure ASSIGNING FIELD-SYMBOL(<ls_structure>).
          GET REFERENCE OF <ls_structure> INTO lrd_data.
          DATA(lv_next_line) = _do_convert_to_download_format( ird_data = lrd_data
                                                             it_structure = lt_structure ).
          TRANSFER lv_next_line TO lv_filename.
        ENDLOOP.

      CATCH lcx_unknown_filetype
            lcx_empty_record
            lcx_object_unknown
            cx_rs_error
            cx_sy_file_access_error INTO DATA(lrx_exc).
        BREAK xbt786.
    ENDTRY.
    CLOSE DATASET lv_filename.

  ENDMETHOD.

  METHOD _read_and_download.

    FIELD-SYMBOLS:
      <lt_data> TYPE STANDARD TABLE.

    DATA:
      lv_fileopen TYPE rs_bool VALUE rs_c_false,
      lrd_data    TYPE REF TO data,
      lv_cnt      TYPE i,
      c1          TYPE cursor.

    TRY.
        TRY.
            DATA(lv_filename) = get_filename( 'D' ).
            DATA(lv_tabname) = get_tabname(  ).

            _do_message( |Reading from table { lv_tabname } transfering to file { lv_filename }| ).

            ASSIGN ntd_infoprov->* TO <lt_data>.

            OPEN CURSOR c1 FOR SELECT * FROM (lv_tabname).

            DO.
              FETCH NEXT CURSOR c1 INTO CORRESPONDING FIELDS OF TABLE <lt_data> PACKAGE SIZE gv_package_size.
              IF sy-subrc <> 0.
                CLOSE CURSOR c1.
                RAISE EXCEPTION TYPE cx_rsb_no_more_data.
              ELSE.
                IF lv_fileopen = rs_c_false.
                  " Created this way to avoid the creation of empty files
                  OPEN DATASET lv_filename FOR OUTPUT IN TEXT MODE ENCODING UTF-8 MESSAGE nv_message.
                  IF sy-subrc <> 0.
                    MESSAGE nv_message TYPE rs_c_error.
                    RAISE EXCEPTION TYPE cx_rs_error.
                  ENDIF.

                  DATA lv_line TYPE string.
                  LOOP AT get_structure_information(  ) ASSIGNING FIELD-SYMBOL(<ls_data_structure>).
                    DATA(lv_fieldname) = get_fieldname( <ls_data_structure>-fieldname ).
                    IF lv_line IS INITIAL.
                      lv_line = lv_fieldname.
                    ELSE.
                      lv_line = |{ lv_line };{ lv_fieldname  }|.
                    ENDIF.
                  ENDLOOP.
                  "" Create header line
                  TRANSFER lv_line TO lv_filename.
                ENDIF.
                lv_fileopen = rs_c_true.
                _do_message( |Reading from { lv_tabname }, { sy-dbcnt } records| ).
              ENDIF.

              LOOP AT <lt_data> REFERENCE INTO lrd_data.
                TRY.
                    lv_line = _do_convert_to_download_format( ird_data     = lrd_data
                                                              it_structure = get_structure_information(  ) ).
                    TRANSFER lv_line TO lv_filename.
                    IF sy-subrc <> 0.
                      BREAK xbt786.
                    ENDIF.
                    lv_cnt = lv_cnt + 1.
                  CATCH lcx_empty_record.
                    " Empty record.. not reason to transfer
                ENDTRY.
              ENDLOOP.
            ENDDO.

          CATCH lcx_no_data
                cx_rsb_no_more_data.
            _do_message( |Finish processing download of data from { nv_infoprov }| ).
        ENDTRY.
        CLOSE DATASET lv_filename.

        IF nv_simu = rs_c_true.
          _do_message( |Found { lv_cnt } lines to move| ).
        ENDIF.

        lv_fileopen = rs_c_false.

        TRY.
            lv_filename = get_filename( 'T' ).
            lv_tabname  = get_texttable(  ).
            _do_message( |Reading from table { lv_tabname } transfering to file { lv_filename }| ).

            DATA lrd_text TYPE REF TO data.
            CREATE DATA lrd_text TYPE STANDARD TABLE OF (lv_tabname).
            ASSIGN lrd_text->* TO <lt_data>.

            OPEN CURSOR c1 FOR SELECT * FROM (lv_tabname).

            DO.
              FETCH NEXT CURSOR c1 INTO CORRESPONDING FIELDS OF TABLE <lt_data> PACKAGE SIZE gv_package_size.
              IF sy-subrc <> 0.
                CLOSE CURSOR c1.
                RAISE EXCEPTION TYPE cx_rsb_no_more_data.
              ELSE.
                _do_message( |Reading from { lv_tabname }, { sy-dbcnt } records| ).
                IF lv_fileopen = rs_c_false.
                  OPEN DATASET lv_filename FOR OUTPUT IN TEXT MODE ENCODING UTF-8 MESSAGE nv_message.
                  IF sy-subrc <> 0.
                    MESSAGE nv_message TYPE rs_c_error.
                    RAISE EXCEPTION TYPE cx_rs_error.
                  ENDIF.

                  CLEAR lv_line.
                  LOOP AT get_structure_text(  ) ASSIGNING <ls_data_structure>.
                    lv_fieldname = get_fieldname( <ls_data_structure>-fieldname ).
                    IF lv_line IS INITIAL.
                      lv_line = lv_fieldname.
                    ELSE.
                      lv_line = |{ lv_line };{  lv_fieldname }|.
                    ENDIF.
                  ENDLOOP.
                  "" Create header line
                  TRANSFER lv_line TO lv_filename.
                  lv_fileopen = rs_c_false.
                ENDIF.
              ENDIF.

              LOOP AT <lt_data> REFERENCE INTO lrd_data.
                TRY.
                    lv_line = _do_convert_to_download_format( ird_data     = lrd_data
                                                              it_structure = get_structure_text(  ) ).
                    IF nv_simu = rs_c_false.
                      TRANSFER lv_line TO lv_filename.
                    ELSE.
                      lv_cnt = lv_cnt + 1.
                    ENDIF.
                  CATCH lcx_empty_record.
                    " Empty record
                ENDTRY.
              ENDLOOP.
            ENDDO.
          CATCH lcx_no_data
                lcx_no_processing.
            " No processing of text
        ENDTRY.

        IF nv_simu = rs_c_true.
          _do_message( |Found { lv_cnt } lines to move| ).
        ENDIF.



      CATCH cx_rsb_no_more_data.
        _do_message( |Finish processing download of data from { nv_infoprov }| ).
      CATCH lcx_no_data
            lcx_unknown_filetype
            lcx_object_unknown
            cx_rs_error
            cx_sy_file_access_error INTO DATA(lrx_exc).
        BREAK xbt786.
    ENDTRY.
    CLOSE DATASET lv_filename.

  ENDMETHOD.

  METHOD void.

    DATA:
      lt_infoprov TYPE gtyt_infoprov.

    try.
    IF iv_toworkstation IS NOT INITIAL.
      CREATE OBJECT nr_zip.
    ELSE.
      nv_simu = rs_c_false.
    ENDIF.
    nv_read = iv_read.

    DATA lr_infoprov TYPE REF TO lcl_download.
    LOOP AT it_rng_tlogo INTO DATA(ls_tlogo).
      DATA(lc_class) = |LCL_{ ls_tlogo-low }|.
      CREATE OBJECT lr_infoprov TYPE (lc_class)
        EXPORTING
          iv_infoprov = ''.
      APPEND LINES OF lr_infoprov->get_objects(
                it_rng_tlogo = it_rng_tlogo
                it_rng_infoprov = it_rng_infoprov ) TO lt_infoprov.
    ENDLOOP.

    SORT lt_infoprov.
    DELETE ADJACENT DUPLICATES FROM lt_infoprov.

    _do_message( '-----------------------' ).
    _do_message( '--- Do prepressing ----' ).
    _do_message( '-----------------------' ).
    _do_message( |--- Found { lines(  lt_infoprov ) } to process| ).

    LOOP AT lt_infoprov ASSIGNING FIELD-SYMBOL(<ls_infoprov>).
      TRY.
          <ls_infoprov>-r_infoprov = factory( iv_tlogo    = <ls_infoprov>-tlogo
                                               iv_infoprov = <ls_infoprov>-infoprov ).

        CATCH lcx_object_unknown INTO DATA(lcx_lcx).
          _do_message(  lcx_lcx->get_text( ) ).
        CATCH cx_sy_create_object_error INTO DATA(lrx_obj).
          " Even if this is not a success, at least we don't get a popup
          _do_message( lrx_obj->get_text(  ) ).
        CATCH cx_static_check INTO DATA(lrx_error).
          _do_message( lrx_error->get_text(  ) ).
      ENDTRY.
    ENDLOOP.


    _do_message( '-----------------------' ).
    _do_message( '--- Do download    ----' ).
    _do_message( '-----------------------' ).

    IF iv_read = rs_c_false.
      LOOP AT lt_infoprov ASSIGNING <ls_infoprov>.
        TRY.
            IF <ls_infoprov>-r_infoprov IS NOT BOUND OR
                        <ls_infoprov>-r_infoprov->is_object_empty(  ) = rs_c_true.
              " The object was not instantiate, due to whatever reason
              _do_message( |InfoProvider { <ls_infoprov>-tlogo }/{ <ls_infoprov>-infoprov } does not conntain any data| ).
              CONTINUE.
            ENDIF.
          CATCH lcx_no_data.
            CONTINUE.
        ENDTRY.
        TRY.
            _do_message( |Processing InfoProvider { <ls_infoprov>-tlogo }/{ <ls_infoprov>-infoprov }| ).

            " Only read... so find what's on the disk
            CALL METHOD <ls_infoprov>-r_infoprov->_download_structure.
            CALL METHOD <ls_infoprov>-r_infoprov->_read_and_download.

          CATCH cx_static_check INTO lrx_error.
            MESSAGE lrx_error->get_text(  ) TYPE rs_c_info.
        ENDTRY.
      ENDLOOP.
    ENDIF.

    IF iv_toworkstation IS NOT INITIAL.
      _do_message( '-----------------------' ).
      _do_message( '--- Do ZIP ------------' ).
      _do_message( '-----------------------' ).
      LOOP AT lt_infoprov ASSIGNING <ls_infoprov>.
        CALL METHOD <ls_infoprov>-r_infoprov->do_move_to_workstation
          EXPORTING
            iv_path = iv_toworkstation.
      ENDLOOP.

      DATA:
        lv_zipfile  TYPE string,
        lv_zip_size TYPE i,
        lv_zip_data TYPE STANDARD TABLE OF raw255.

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = nr_zip->save( )
        IMPORTING
          output_length = lv_zip_size
        TABLES
          binary_tab    = lv_zip_data.

      CONCATENATE iv_toworkstation '\download.zip' INTO lv_zipfile.
      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          filename     = lv_zipfile
          bin_filesize = lv_zip_size
          filetype     = 'BIN'
        CHANGING
          data_tab     = lv_zip_data
        EXCEPTIONS
          OTHERS       = 8.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

    IF iv_rfcdest IS NOT INITIAL.
      _do_message( '-----------------------' ).
      _do_message( '--- Do send to { iv_rfcdest } ----' ).
      _do_message( '-----------------------' ).
      LOOP AT lt_infoprov ASSIGNING <ls_infoprov>.
        CALL METHOD <ls_infoprov>-r_infoprov->do_send_to_rfc
          EXPORTING
            iv_rfcdest = iv_rfcdest.
      ENDLOOP.
    ENDIF.

      catch cx_rs_error.
    endtry.

  ENDMETHOD.

  METHOD setup.
    " Needs to be called within the object to ensure that
    " correct methods are called

    TRY.
        CALL METHOD get_tabname.
        CALL METHOD get_structure_pointer.
        nt_structure = get_structure_information(  ).
      CATCH lcx_no_data.
        " No attributes, let's see if the is a text table
        " This one throws the lcx_no_data again, but this also
        " means that neither attribute nor text is in
        CALL METHOD get_texttable.
    ENDTRY.

  ENDMETHOD.

  METHOD factory.

    DATA(lc_class) = |LCL_{ iv_tlogo }|.
    CREATE OBJECT rr_infoprov TYPE (lc_class)
      EXPORTING
        iv_tlogo    = iv_tlogo
        iv_infoprov = iv_infoprov.

    " Delete any file that might exists
    CALL METHOD rr_infoprov->_delete_old_file.
    CALL METHOD rr_infoprov->setup.

  ENDMETHOD.

  METHOD get_filename.
    rv_filename = |{ iv_path }{ nv_infoprov }|.
    IF iv_counter IS SUPPLIED.
      rv_filename = |{ rv_filename }{ iv_counter }|.
    ENDIF.
    CASE iv_filetype.
      WHEN 'S'.
        rv_filename = |{ rv_filename }.ddic|.
      WHEN 'D'.
        rv_filename = |{ rv_filename }.data|.
      WHEN 'T'.
        rv_filename = |{ rv_filename }.text|.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE lcx_unknown_filetype
          EXPORTING
            iv_filetype = iv_filetype.
    ENDCASE.
  ENDMETHOD.

  METHOD get_fieldname.


    DATA:
      lv_ddname TYPE rs_char30,
      lv_name   TYPE rs_char30.

    lv_ddname = iv_structurename.

    CALL FUNCTION 'RSD_IOBJNM_GET_FROM_FIELDNM'
      EXPORTING
        i_ddname = lv_ddname
      IMPORTING
        e_name   = lv_name.

    rv_iobjnm = lv_name.

    " if the InfoObject does not exist
    " return the original name
    IF cl_rsd_iobj=>exists_on_db(
          EXPORTING
            i_objnm = rv_iobjnm
            i_objvers = rs_c_objvers-active ) = rs_c_false.
      rv_iobjnm = iv_structurename.
    ENDIF.
  ENDMETHOD.

  METHOD get_structure_pointer.

    IF nsd_infoprov IS NOT BOUND.
      DATA(lt_structure) = get_structure_information(  ).
      DATA:
        ls_component TYPE abap_componentdescr,
        lt_component TYPE abap_component_tab.

      LOOP AT lt_structure ASSIGNING FIELD-SYMBOL(<ls_structure>).
        ls_component-name = <ls_structure>-fieldname.
        ls_component-type ?= cl_rsr=>get_datadescr( i_inttp    = <ls_structure>-inttype
                                                    i_intlen   = <ls_structure>-intlen
                                                    i_decimals = <ls_structure>-decimals ).
        APPEND ls_component TO lt_component.
      ENDLOOP.

      DATA(lsh_infoprov) = cl_abap_structdescr=>create(  lt_component ).
      DATA(lth_infoprov) = cl_abap_tabledescr=>create(  lsh_infoprov ).

      CREATE DATA nsd_infoprov TYPE HANDLE lsh_infoprov.
      CREATE DATA ntd_infoprov TYPE HANDLE lth_infoprov.
    ENDIF.

    esd_infoprov = nsd_infoprov.
    etd_infoprov = ntd_infoprov.

  ENDMETHOD.

  METHOD get_structure_text.

    IF nt_textstruc IS INITIAL.
      DATA(lv_tabname) = get_texttable(  ).
      CALL FUNCTION 'DDIF_FIELDINFO_GET'
        EXPORTING
          tabname        = lv_tabname
        TABLES
          dfies_tab      = nt_textstruc
        EXCEPTIONS
          internal_error = 1
          not_found      = 2
          error_message  = 3
          OTHERS         = 4.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_rs_msg
          EXPORTING
            msgid = sy-msgid
            msgty = sy-msgty
            msgno = sy-msgno
            msgv1 = sy-msgv1
            msgv2 = sy-msgv2
            msgv3 = sy-msgv3
            msgv4 = sy-msgv4.
      ENDIF.
    ENDIF.
    rt_structure = nt_textstruc.

  ENDMETHOD.

  METHOD get_structure_information.

    IF nt_structure IS INITIAL.
      DATA(lv_tabname) = get_tabname(  ).
      CALL FUNCTION 'DDIF_FIELDINFO_GET'
        EXPORTING
          tabname        = lv_tabname
        TABLES
          dfies_tab      = nt_structure
        EXCEPTIONS
          internal_error = 1
          not_found      = 2
          error_message  = 3
          OTHERS         = 4.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_rs_msg
          EXPORTING
            msgid = sy-msgid
            msgty = sy-msgty
            msgno = sy-msgno
            msgv1 = sy-msgv1
            msgv2 = sy-msgv2
            msgv3 = sy-msgv3
            msgv4 = sy-msgv4.
      ENDIF.
    ENDIF.
    rt_structure = nt_structure.
  ENDMETHOD.

  METHOD _do_convert_to_download_format.
* Convert a line of the input table to a line seperated with ';'
* it might not be required to have this - I'm also curious about
* if this covers all data types
    DATA:
      lv_num_buffer(100) TYPE n,
      lt_line            TYPE STANDARD TABLE OF string,
      lv_line            TYPE string.

*   Convert each field of the line
    ASSIGN ird_data->* TO FIELD-SYMBOL(<ls_data>).

    LOOP AT it_structure INTO DATA(ls_tab_desc).

      CLEAR lv_line.
*     Assign field symbol to the field
      ASSIGN COMPONENT ls_tab_desc-position OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<fs>).

      IF ls_tab_desc-keyflag = rs_c_true AND ls_tab_desc-fieldname = 'OBJVERS'.
        " Do not do anything
      ELSE.
        " Even with empty records the OBJVERS is flled
        IF <fs> IS NOT INITIAL.
          DATA(lv_value_found) = rs_c_true.
        ENDIF.
      ENDIF.

*     Convert the field depending on its type and write it to the line
*     buffer
      CASE ls_tab_desc-inttype.
        WHEN 'N'.                      " Numeric Character
          TRY.
              ls_tab_desc-leng = ls_tab_desc-intlen - ls_tab_desc-leng.
              MOVE <fs>+ls_tab_desc-leng TO lv_line.
            CATCH cx_sy_range_out_of_bounds.
              MOVE <fs> TO lv_line.
          ENDTRY.
        WHEN 'C'                       " Character
          OR 'D'                       " Date
          OR 'T'.                      " Time
          MOVE <fs> TO lv_line.
        WHEN 'P'                       " Packed Number
          OR '8'                       " 8-Byte Integer
          OR 'I'                       " 4-Byte Integer
          OR 's'                       " 2-Byte Integer
          OR 'b'.                      " 1-Byte Integer
          MOVE <fs> TO lv_num_buffer(ls_tab_desc-leng).
          MOVE lv_num_buffer(ls_tab_desc-leng) TO lv_line.
        WHEN 'X'.
*         DATA:
*           l_r_convert TYPE REF TO cl_abap_conv_in_ce,

          FIELD-SYMBOLS:
            <l_x>      TYPE x.
          ASSIGN <fs> TO <l_x> CASTING.
          CALL METHOD cl_abap_conv_in_ce=>create(
              input       = <l_x>
              encoding    = gv_abap_enconding
              ignore_cerr = rs_c_true )->read( IMPORTING data = lv_line ).

        WHEN OTHERS.
          RAISE EXCEPTION TYPE cx_rs_error.
      ENDCASE.
      APPEND lv_line TO lt_line.
    ENDLOOP. " at tab_desc

    IF lv_value_found = rs_c_false.
      RAISE EXCEPTION TYPE lcx_empty_record.
    ENDIF.

    CONCATENATE LINES OF lt_line INTO rv_line SEPARATED BY nv_seperator.
  ENDMETHOD.

  METHOD class_constructor.

    CALL FUNCTION 'SCP_GET_CODEPAGE_NUMBER'
      EXPORTING
        database_also = ' '
      IMPORTING
        appl_codepage = gv_appl_codepage
      EXCEPTIONS
        OTHERS        = 2.
    gv_abap_enconding = gv_appl_codepage.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_iobj DEFINITION
  INHERITING FROM lcl_download.

  PROTECTED SECTION.
    METHODS:
      get_objects REDEFINITION,
      get_texttable REDEFINITION,
      get_tabname REDEFINITION.
ENDCLASS.

CLASS lcl_iobj IMPLEMENTATION.

  METHOD get_objects.

    SELECT 'IOBJ' AS tlogo,
           iobjnm AS infoprov
       FROM rsdiobj
       WHERE objvers = @rs_c_objvers-active AND
             iobjnm  IN @it_rng_infoprov
       INTO CORRESPONDING FIELDS OF TABLE @rt_infoprov.

  ENDMETHOD.

  METHOD get_tabname.
    TRY.
        DATA(lr_infoobject) = cl_rsd_iobj_prop_cache=>get_cha( nv_infoprov ).
        IF lr_infoobject->n_s_cha-chabasnm <> nv_infoprov.
          RAISE EXCEPTION TYPE lcx_no_data
            EXPORTING
              iv_infoprov = nv_infoprov
              iv_tlogo    = 'IOBJ'.
        ENDIF.
      CATCH cx_rsd_iobj_not_exist.
        RAISE EXCEPTION TYPE lcx_object_unknown
          EXPORTING
            iv_infoprov = nv_infoprov
            iv_tlogo    = 'IOBJ'.
    ENDTRY.
    CALL METHOD lr_infoobject->get_master_data_table_names
      IMPORTING
        e_chktab = rv_tabname
      EXCEPTIONS
        OTHERS   = 8.

    IF rv_tabname IS INITIAL.
      RAISE EXCEPTION TYPE lcx_no_data
        EXPORTING
          iv_infoprov = nv_infoprov
          iv_tlogo    = 'IOBJ'.
    ENDIF.
  ENDMETHOD.

  METHOD get_texttable.
    TRY.
        DATA(lr_infoobject) = cl_rsd_iobj_prop_cache=>get_cha( nv_infoprov ).
        IF lr_infoobject->n_s_cha-chabasnm <> nv_infoprov.
          RAISE EXCEPTION TYPE lcx_no_data
            EXPORTING
              iv_infoprov = nv_infoprov
              iv_tlogo    = 'IOBJ'.
        ENDIF.
      CATCH cx_rsd_iobj_not_exist.
        RAISE EXCEPTION TYPE lcx_object_unknown
          EXPORTING
            iv_infoprov = nv_infoprov
            iv_tlogo    = 'IOBJ'.
    ENDTRY.
    CALL METHOD lr_infoobject->get_master_data_table_names
      IMPORTING
        e_txttab = rv_tabname
      EXCEPTIONS
        OTHERS   = 8.

    IF rv_tabname IS INITIAL.
      RAISE EXCEPTION TYPE lcx_no_data
        EXPORTING
          iv_infoprov = nv_infoprov
          iv_tlogo    = 'IOBJ'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_adso DEFINITION
  INHERITING FROM lcl_download.
  PROTECTED SECTION.
    METHODS:
      get_tabname REDEFINITION.
ENDCLASS.

CLASS lcl_adso IMPLEMENTATION.
  METHOD get_tabname.
    TRY.
        DATA(lt_tabnames) = cl_rso_adso=>get_tablnm( EXPORTING
          i_adsonm  = nv_infoprov
          i_objvers = rs_c_objvers-active ).
        rv_tabname = lt_tabnames[ dsotabtype = 'VR' ]-name.
      CATCH cx_sy_itab_line_not_found
            cx_rs_not_found.
        RAISE EXCEPTION TYPE lcx_object_unknown
          EXPORTING
            iv_infoprov = nv_infoprov
            iv_tlogo    = 'ADSO'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_odso DEFINITION
  INHERITING FROM lcl_download.
  PROTECTED SECTION.
    METHODS:
      get_tabname REDEFINITION.
ENDCLASS.

CLASS lcl_odso IMPLEMENTATION.
  METHOD get_tabname.
    CALL METHOD cl_rsd_odso=>get_tablnm
      EXPORTING
        i_odsobject = nv_infoprov
      IMPORTING
        e_tablnm    = rv_tabname
      EXCEPTIONS
        OTHERS      = 8.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_object_unknown
        EXPORTING
          iv_infoprov = nv_infoprov
          iv_tlogo    = 'ODSO'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  CALL METHOD lcl_download=>void
    EXPORTING
      iv_toworkstation = pv_path
      iv_read          = pv_read
      iv_rfcdest       = pv_rfc
      it_rng_tlogo     = pt_tlogo[]
      it_rng_infoprov  = pt_obj[].
