CLASS zcl_core_iobj_tool DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: ltyt_rng_iobjnm TYPE RANGE OF rsiobjnm.
    TYPES: ltyth_iobjnm TYPE HASHED TABLE OF rsiobjnm WITH UNIQUE KEY table_line.

    TYPES:
      BEGIN OF ltys_infoobject,
        " This InfoObject is required by the InfoObject we are trying to clone
        original     TYPE rsiobjnm,
        " What target
        target_cluster TYPE zcore_cluster,
        " This will be the new name of the new object
        iobjnm       TYPE rsiobjnm,
        " this is the object of the new infoobject
        r_iobjnm     TYPE REF TO zcl_core_iobj_tool,
      END OF ltys_infoobject,
      ltyt_infoobject  TYPE STANDARD TABLE OF ltys_infoobject WITH NON-UNIQUE DEFAULT KEY,
      ltyth_infoobject TYPE HASHED TABLE OF ltys_infoobject WITH UNIQUE KEY original target_cluster.

    TYPES:
      BEGIN OF ltys_iobj_meta,
        details                  TYPE bapi6108,
        atr                      TYPE rs_bool,
        details_2                TYPE bapi6108_2,
        compounds                TYPE STANDARD TABLE OF bapi6108cm WITH NON-UNIQUE DEFAULT KEY,
        attributes               TYPE STANDARD TABLE OF bapi6108at WITH NON-UNIQUE DEFAULT KEY,
        navigationattributes     TYPE STANDARD TABLE OF bapi6108an WITH NON-UNIQUE DEFAULT KEY,
        atrnavinfoprovider       TYPE STANDARD TABLE OF bapi6108np WITH  NON-UNIQUE DEFAULT KEY,
        hierarchycharacteristics TYPE STANDARD TABLE OF bapi6108hc WITH  NON-UNIQUE DEFAULT KEY,
        elimination              TYPE STANDARD TABLE OF bapi6108ie WITH  NON-UNIQUE DEFAULT KEY,
        hanafieldsmapping        TYPE STANDARD TABLE OF bapi6108hana_map WITH  NON-UNIQUE DEFAULT KEY,
        xxlattributes            TYPE STANDARD TABLE OF bapi6108atxxl WITH  NON-UNIQUE DEFAULT KEY,
      END OF ltys_iobj_meta.

    CONSTANTS:
      BEGIN OF cs_languages,
        english TYPE langu VALUE 'E',
        french  TYPE langu VALUE 'F',
      END OF cs_languages.

    CONSTANTS:
      BEGIN OF gc_cluster,
        common TYPE zcore_cluster VALUE 'X',
      END OF gc_cluster,
      cv_replacestring TYPE char72 VALUE '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'.

    CONSTANTS:
      BEGIN OF gc_processing,
        persistent_none  TYPE char1 VALUE '1',
        persistent_start TYPE char1 VALUE '2',
        persistent_temp  TYPE char1 VALUE '3',
        persistent_done  TYPE char1 VALUE '4',
      END OF gc_processing.

    CLASS-DATA:
      gv_infoarea     TYPE rsinfoarea VALUE 'SYS_COPY',
      _message        TYPE string,
      gv_prefix       type char2,
      gv_detlevel     TYPE bal_s_msg-detlevel VALUE '1',
      gv_numlevel     TYPE i VALUE 1,
      gv_iobjnm       TYPE rsiobjnm,
      gv_simulation   TYPE rs_bool VALUE rs_c_false,
      gv_log_handle   TYPE balloghndl,
      gth_cluster_iobj  TYPE ltyth_infoobject,
      gth_workinglist TYPE ltyth_iobjnm,
      gv_rfcdest      TYPE rfcdest,
      gv_target_cluster TYPE zcore_cluster.

    CLASS-METHODS:
      static_set_infoarea
        IMPORTING
          iv_infoarea TYPE rsinfoarea,
      static_reset_all,
      static_add_fixed_infoobject
        IMPORTING it_iobjnm TYPE ltyt_rng_iobjnm,
      static_do_insertlist
        IMPORTING
          it_iobjnm       TYPE ltyt_rng_iobjnm
          iv_objvers      TYPE rsobjvers
          iv_target_cluster TYPE zcore_cluster
          iv_rfcdest      TYPE rfcdest DEFAULT 'NONE',
      static_execute_cloning
        IMPORTING
          iv_no_activation TYPE rs_bool DEFAULT rs_c_false,
      factory
        IMPORTING
                  iv_original          TYPE rsiobjnm
                  iv_objvers           TYPE rsobjvers DEFAULT rs_c_objvers-active
                  iv_rfcdest           TYPE rfcdest DEFAULT 'NONE'
                  iv_target_cluster      TYPE zcore_cluster DEFAULT gc_cluster-common
        RETURNING VALUE(rr_cluster_iobj) TYPE REF TO zcl_core_iobj_tool
        RAISING
                  cx_rs_error.

    METHODS:
      constructor
        IMPORTING
          iv_original     TYPE rsiobjnm
          iv_objvers      TYPE rsobjvers
          iv_target_cluster TYPE zcore_cluster
          iv_rfcdest      TYPE rfcdest DEFAULT 'NONE'
        RAISING
          cx_rs_not_found
          cx_rs_error.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA:
      " for presistence,
      nv_processing    TYPE char1,
      nv_original      TYPE rsiobjnm,
      nr_fromwho       TYPE REF TO zcl_core_iobj_tool,
      nv_iobjnm        TYPE rsiobjnm,
      nv_objvers       TYPE rsobjvers,
      nv_baseiobjnm    TYPE rsdchabas,
      nv_cloned        TYPE rs_bool VALUE rs_c_false,
      nv_missing       TYPE rs_bool VALUE rs_c_false,
      nv_updateiobj    TYPE rs_bool VALUE rs_c_false,
      nv_rfcdest       TYPE rfcdest,
      nv_target_cluster  TYPE zcore_cluster,
      ns_new_meta      TYPE ltys_iobj_meta,
      ns_original_meta TYPE ltys_iobj_meta,
      nt_required      TYPE ltyt_infoobject.

    CLASS-METHODS:
      static_add_message
        IMPORTING
          is_msg TYPE bal_s_msg OPTIONAL,
      static_name_already_used
        IMPORTING
                  iv_newname     TYPE rsiobjnm
        RETURNING VALUE(rv_used) TYPE rs_bool,
      static_display_log,
      static_go_level_low,
      static_go_level_up.

    METHODS:
      check_original_check_exists
        RETURNING VALUE(rv_check_exists) TYPE rs_bool
        RAISING
                  cx_rs_error,
      check_exists
        IMPORTING
                  iv_objvers             TYPE rsobjvers DEFAULT rs_c_objvers-active
                  iv_static_add_message  TYPE rs_bool DEFAULT rs_c_true
        RETURNING VALUE(rv_check_exists) TYPE rs_bool.

    METHODS:
      do_add_required_infoobject
        IMPORTING iv_required             TYPE rsiobjnm
        RETURNING VALUE(rv_newinfoobject) TYPE rsiobjnm
        RAISING   cx_rs_error.

    METHODS:
      do_persistest,
      do_persistest_processing
        RETURNING
          VALUE(rt_rng_iobjnm) type ltyt_rng_iobjnm,
      do_adapt
        RAISING cx_rs_error,
      do_adapt_baseinfoobject
        RAISING cx_rs_error,
      do_adapt_compounding
        RAISING cx_rs_error,
      do_adapt_attributes
        RAISING cx_rs_error,
      do_adapt_details
        RAISING cx_rs_error.


    METHODS:
      do_reset,
      get_existing
        IMPORTING
                  iv_iobjnm            TYPE rsiobjnm OPTIONAL
                  iv_check_workinglist TYPE rs_bool DEFAULT rs_c_false
                    PREFERRED PARAMETER iv_iobjnm
        RETURNING VALUE(rv_iobjnm)     TYPE rsiobjnm
        RAISING   cx_rs_not_found,
      get_current_metadata
        RAISING cx_rs_not_found,
      get_cluster
        IMPORTING
          iv_iobjnm type rsiobjnm OPTIONAL
        RETURNING
          VALUE(rv_cluster) type zcore_cluster,
      get_new_name
        RETURNING
          VALUE(rv_new_name) TYPE rsiobjnm,
      get_parameter_list
        IMPORTING
                  iv_funcname         TYPE funcname
        RETURNING VALUE(rt_parambind) TYPE abap_func_parmbind_tab .

ENDCLASS.

CLASS zcl_core_iobj_tool IMPLEMENTATION.


  METHOD do_persistest_processing.

    DATA  ls_return TYPE bapiret2.
    DATA  lt_returntable TYPE STANDARD TABLE OF bapiret2.

    " Who do the call coming from - Keep in mind that
    " calling object might be different from call to call
    " I've added the me to make the logic more understanable
    " as me is used when calling

    IF nv_processing = gc_processing-persistent_start.
      " We are already processing this IOBJ and need to place a temp IOBJ
      MESSAGE i010(zcore) WITH nv_iobjnm INTO _message.
      static_add_message( ).
      " So do not continue further, we need
      " the IOBJ as it is used, but we don't need all the details
      RETURN.
    ELSE.
      IF nv_processing <> gc_processing-persistent_done.
        nv_processing = gc_processing-persistent_start.
        " Start processing the cloning of InfoObject nv_iobjnm.
        MESSAGE i012(zcore) WITH nv_original nv_iobjnm INTO _message.
        static_add_message( ).

        static_go_level_low( ).
        LOOP AT nt_required ASSIGNING FIELD-SYMBOL(<ls_required>).
          append lines of <ls_required>-r_iobjnm->do_persistest_processing( ) to rt_rng_iobjnm.
        ENDLOOP.
        do_persistest(  ).
        static_go_level_up( ).
      ENDIF.
    ENDIF.

    IF nv_original <> nv_iobjnm OR nv_updateiobj = rs_c_true.
      APPEND VALUE #(  sign    = rs_c_range_sign-including
                       option  = rs_c_range_opt-equal
                       low     = nv_iobjnm ) TO rt_rng_iobjnm.
    endif.

  ENDMETHOD.

  METHOD do_persistest.

    DATA:
      lv_tool        TYPE string,
      lt_returntable TYPE STANDARD TABLE OF bapiret2,
      ls_return      TYPE bapiret2.

    IF gv_simulation = rs_c_false.
      IF check_exists(  ) = rs_c_true OR check_exists( iv_objvers = rs_c_objvers-modified ).
        lv_tool = 'Update'.
        CALL FUNCTION 'BAPI_IOBJ_CHANGE'
          EXPORTING
            infoobject               = nv_iobjnm
            details                  = ns_new_meta-details
            details_2                = ns_new_meta-details_2
          IMPORTING
            return                   = ls_return
          TABLES
            compounds                = ns_new_meta-compounds
            attributes               = ns_new_meta-attributes
            navigationattributes     = ns_new_meta-navigationattributes
            atrnavinfoprovider       = ns_new_meta-atrnavinfoprovider
            hierarchycharacteristics = ns_new_meta-hierarchycharacteristics
            elimination              = ns_new_meta-elimination
            returntable              = lt_returntable
            hanafieldsmapping        = ns_new_meta-hanafieldsmapping
            xxlattributes            = ns_new_meta-xxlattributes.
      ELSE.
        lv_tool = 'Create'.
        CALL FUNCTION 'BAPI_IOBJ_CREATE'
          EXPORTING
            details                  = ns_new_meta-details
            details_2                = ns_new_meta-details_2
          IMPORTING
            infoobject               = nv_iobjnm
            return                   = ls_return
          TABLES
            compounds                = ns_new_meta-compounds
            attributes               = ns_new_meta-attributes
            navigationattributes     = ns_new_meta-navigationattributes
            atrnavinfoprovider       = ns_new_meta-atrnavinfoprovider
            hierarchycharacteristics = ns_new_meta-hierarchycharacteristics
            elimination              = ns_new_meta-elimination
            returntable              = lt_returntable
            hanafieldsmapping        = ns_new_meta-hanafieldsmapping
            xxlattributes            = ns_new_meta-xxlattributes.

        DATA: ls_trans TYPE zcore_trans.

        ls_trans-original   = nv_original.
        ls_trans-doccluster = nv_target_cluster.
        ls_trans-iobjnm     = nv_iobjnm.
        INSERT zcore_trans FROM ls_trans.
        IF sy-subrc <> 0.
          BREAK-POINT.
        ENDIF.
        COMMIT WORK.
      ENDIF.
    ELSE.
      IF check_exists(  ) = rs_c_true.
        lv_tool = 'Simulation: Change'.
      ELSE.
        lv_tool = 'Simulation: Create'.
      ENDIF.
    ENDIF.
    MESSAGE i013(zcore) WITH nv_iobjnm nv_original nv_rfcdest lv_tool INTO _message.
    static_add_message(  ).
    IF gv_simulation = rs_c_false.

      CALL METHOD static_add_message( VALUE #( msgty = ls_return-type
                                        msgid = ls_return-id
                                        msgno = ls_return-number
                                        msgv1 = ls_return-message_v1
                                        msgv2 = ls_return-message_v2
                                        msgv3 = ls_return-message_v3
                                        msgv4 = ls_return-message_v4 ) ).
    ENDIF.
    " Hopefully done
    nv_processing = gc_processing-persistent_done.

  ENDMETHOD.

  METHOD do_adapt.
    static_go_level_low( ).
    CALL METHOD:
      do_adapt_details,
      do_adapt_compounding,
      do_adapt_attributes.
    static_go_level_up( ).
  ENDMETHOD.

  METHOD do_adapt_baseinfoobject.

    " No reason to do anything if the base object is not filled
    " this is not the case for Key figures
    CHECK ns_original_meta-details-chabasnm IS NOT INITIAL.

    IF ns_original_meta-details-chabasnm = nv_original.
      ns_new_meta-details-chabasnm = nv_iobjnm.
    ELSE.
      " If the object is a reference to another InfoObject, make sure
      " this object is also cloned as a prerequiste for the orginal clone
      ns_new_meta-details-chabasnm = do_add_required_infoobject( ns_original_meta-details-chabasnm ).
      MESSAGE i009(zcore) WITH ns_original_meta-details-chabasnm ns_new_meta-details-chabasnm 'Base InfoObject' INTO _message.
      CALL METHOD static_add_message.
    ENDIF.

  ENDMETHOD.

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

  METHOD factory.

    DATA ls_return TYPE bapiret2.

    IF gv_iobjnm IS INITIAL.
      gv_iobjnm = iv_original.
    ENDIF.

    READ TABLE gth_cluster_iobj ASSIGNING FIELD-SYMBOL(<ls_cluster_iobj>)
       WITH TABLE KEY original     = iv_original
                      target_cluster = iv_target_cluster.
    IF sy-subrc <> 0.
      MESSAGE i002(zcore) WITH iv_original iv_target_cluster INTO _message.
      static_add_message(  ).
      "" at this time we only have the meta information for the original and now we need to create the new
      TRY.
          static_go_level_low( ).

          INSERT VALUE #(  original = iv_original
                           target_cluster = iv_target_cluster )
                     INTO TABLE gth_cluster_iobj
                     ASSIGNING <ls_cluster_iobj>.

          TRY.
              CREATE OBJECT <ls_cluster_iobj>-r_iobjnm
                EXPORTING
                  iv_original     = iv_original
                  iv_target_cluster = iv_target_cluster
                  iv_objvers      = iv_objvers
                  iv_rfcdest      = iv_rfcdest.
            CATCH cx_rs_not_found INTO DATA(lrx_not_found).
              RAISE EXCEPTION TYPE cx_rs_error
                EXPORTING
                  previous = lrx_not_found.
          ENDTRY.


          " Let's try and see if we already have the target object in the curernt system
          " and model. The get_existing will throw te CX_RS_NOT_FOUND if a clone
          " is not found, not clone found means that we need a new clone
          CALL METHOD <ls_cluster_iobj>-r_iobjnm->get_existing( ).
          <ls_cluster_iobj>-iobjnm       = <ls_cluster_iobj>-r_iobjnm->nv_iobjnm.
          <ls_cluster_iobj>-r_iobjnm->nv_processing = rs_c_true.
          " And load the corresponding metadata
          CALL METHOD <ls_cluster_iobj>-r_iobjnm->get_current_metadata( ).
          CALL METHOD <ls_cluster_iobj>-r_iobjnm->do_adapt_details.
          CALL METHOD <ls_cluster_iobj>-r_iobjnm->do_adapt_attributes.

        CATCH cx_rs_not_found.
          " and it turns our that he InfoObject clone target is not really created yet
          " se we will create a "model" of it, first finding an appropriate name, this
          " will also set the nv_iobjnm as per the target_cluster - later we might be able
          " to extend the concept to create more than one model
          CALL METHOD <ls_cluster_iobj>-r_iobjnm->get_new_name.
          <ls_cluster_iobj>-iobjnm       = <ls_cluster_iobj>-r_iobjnm->nv_iobjnm.
          " Adapt the values in the details
          CALL METHOD <ls_cluster_iobj>-r_iobjnm->do_adapt_details.
          CALL METHOD <ls_cluster_iobj>-r_iobjnm->do_adapt_compounding.
          CALL METHOD <ls_cluster_iobj>-r_iobjnm->do_adapt_attributes.
          IF <ls_cluster_iobj>-r_iobjnm->ns_original_meta-elimination IS NOT INITIAL OR
             <ls_cluster_iobj>-r_iobjnm->ns_original_meta-hanafieldsmapping IS NOT INITIAL OR
             <ls_cluster_iobj>-r_iobjnm->ns_original_meta-xxlattributes IS NOT INITIAL.
            BREAK-POINT.
          ENDIF.
      ENDTRY.
      static_go_level_up( ).
    ENDIF.

    rr_cluster_iobj = <ls_cluster_iobj>-r_iobjnm.

  ENDMETHOD.

  METHOD get_current_metadata.

    DATA ls_return TYPE bapiret2.

    IF nv_cloned = rs_c_true.
      " If the object is already cloned, we will find the current
      " metadata from this systems
      DATA(lv_rfcdest) = 'NONE'.
    ELSE.
      lv_rfcdest = nv_rfcdest.
    ENDIF.

    MESSAGE i003(zcore) WITH nv_iobjnm lv_rfcdest INTO _message.
    static_add_message(  ).

    DO 2 TIMES.
      CASE sy-index.
        WHEN 1.
          DATA(lv_objvers) = rs_c_objvers-active.
        WHEN 2.
          lv_objvers = rs_c_objvers-modified.
      ENDCASE.


      CALL FUNCTION 'BAPI_IOBJ_GETDETAIL'
        DESTINATION lv_rfcdest
        EXPORTING
          version                  = lv_objvers
          infoobject               = nv_iobjnm
        IMPORTING
          details                  = ns_new_meta-details
          return                   = ls_return
          is_atr                   = ns_new_meta-atr
          details_2                = ns_new_meta-details_2
        TABLES
          compounds                = ns_new_meta-compounds
          attributes               = ns_new_meta-attributes
          navigationattributes     = ns_new_meta-navigationattributes
          atrnavinfoprovider       = ns_new_meta-atrnavinfoprovider
          hierarchycharacteristics = ns_new_meta-hierarchycharacteristics
          elimination              = ns_new_meta-elimination
          hanafieldsmapping        = ns_new_meta-hanafieldsmapping
          xxlattributes            = ns_new_meta-xxlattributes.

      IF ls_return-number IS INITIAL.
        " Don't try and find the modified version
        EXIT.
      ENDIF.
    ENDDO.
    IF ls_return-number IS NOT INITIAL.
      nv_cloned = rs_c_false.
      nv_missing = rs_c_true.
      RAISE EXCEPTION TYPE cx_rs_not_found.
*      CALL METHOD static_add_message( VALUE #( msgty = ls_return-type
*                                               msgid = ls_return-id
*                                               msgno = ls_return-number
*                                               msgv1 = ls_return-message_v1
*                                               msgv2 = ls_return-message_v2
*                                               msgv3 = ls_return-message_v3
*                                               msgv4 = ls_return-message_v4 ) ).
    ENDIF.

  ENDMETHOD.

  METHOD static_go_level_low.
    gv_numlevel = gv_numlevel + 1.
    IF gv_numlevel > 9.
      gv_detlevel = '9'.
    ELSE.
      gv_detlevel = gv_numlevel.
    ENDIF.
  ENDMETHOD.

  METHOD static_name_already_used.
    " input iv_newname
    " output rv_used = 'X' if found

    rv_used = rs_c_false.
    LOOP AT gth_cluster_iobj ASSIGNING FIELD-SYMBOL(<ls_iobj>).
      IF <ls_iobj>-iobjnm = iv_newname.
        rv_used = rs_c_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD static_go_level_up.

    gv_numlevel = gv_numlevel - 1.
    IF gv_numlevel > 9.
      gv_detlevel = '9'.
    ELSE.
      gv_detlevel = gv_numlevel.
    ENDIF.
  ENDMETHOD.

  METHOD do_add_required_infoobject.

    MESSAGE i004(zcore) WITH iv_required nv_target_cluster INTO _message.
    static_add_message( ).

    static_go_level_low( ).

    DATA ls_required TYPE ltys_infoobject.

    READ TABLE nt_required INTO ls_required
      WITH KEY original = iv_required.
    IF sy-subrc <> 0.
      ls_required-original     = iv_required.
      ls_required-target_cluster = nv_target_cluster.

      ls_required-r_iobjnm = factory(
                  iv_original         = iv_required
                  iv_objvers          = nv_objvers
                  iv_rfcdest          = nv_rfcdest
                  iv_target_cluster     = nv_target_cluster ).
      " We don't need a read as on object can only popup once per
      " InfoObject. It's not a hashed table as we need the processing
      " to be done once and in order (compounding first)
      APPEND ls_required TO nt_required.
      rv_newinfoobject = ls_required-r_iobjnm->nv_iobjnm.
    ELSE.
      rv_newinfoobject = ls_required-r_iobjnm->nv_iobjnm.
    ENDIF.

    static_go_level_up( ).

  ENDMETHOD.

  METHOD do_adapt_details.

    MESSAGE i005(zcore) WITH 'DETAILS' nv_iobjnm INTO _message.
    static_add_message( ).

    IF me->nv_cloned = rs_c_false.
      " It might be that some adaption is needed for a clone IOBJ
      " but up front... no
      CLEAR ns_new_meta-details.
      ns_new_meta-details = ns_original_meta-details.

      ns_new_meta-details-infoobject = nv_iobjnm.
      ns_new_meta-details-version    = rs_c_objvers-active.
      ns_new_meta-details-objstat    = rs_c_objstat-active.
      CALL FUNCTION 'RSD_FIELDNM_GET_FROM_IOBJNM'
        EXPORTING
          i_name            = nv_iobjnm
        IMPORTING
          e_ddname          = ns_new_meta-details-fieldnm
        EXCEPTIONS
          name_error        = 1
          prefix_invalid    = 2
          namespace_invalid = 3
          OTHERS            = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      IF ns_new_meta-details-gisattrinm IS NOT INITIAL.
        ns_new_meta-details-gisattrinm = do_add_required_infoobject( ns_original_meta-details-gisattrinm ).
      ENDIF.

      IF ns_new_meta-details-uninm IS NOT INITIAL.
        ns_new_meta-details-uninm = do_add_required_infoobject( ns_original_meta-details-uninm ).
      ENDIF.

      " Make sure we have the Basis InfoObject
      CALL METHOD do_adapt_baseinfoobject.
      CLEAR:
        ns_new_meta-details-chktab,
        ns_new_meta-details-chntab,
        ns_new_meta-details-txttab,
        ns_new_meta-details-hietab,
        ns_new_meta-details-hintab,
        ns_new_meta-details-checkods.

      ns_new_meta-details-infoarea = gv_infoarea.
      ns_new_meta-details-applnm   = ''.
      ns_new_meta-details_2-infoobject = nv_iobjnm.
      ns_new_meta-details_2-version    = rs_c_objvers-active.
    ELSE.
      " If you have logged on in Frensh... we will take the FR Text from the
      " source and use it. The only thing that will be changed. If these
      " have been maintained... do not overwrite
      IF ns_new_meta-details-textshort IS INITIAL.
        ns_new_meta-details-textshort = ns_original_meta-details-textshort.
      ENDIF.
      IF ns_new_meta-details-textlong IS INITIAL.
        ns_new_meta-details-textlong  = ns_original_meta-details-textlong.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD do_adapt_compounding.

    MESSAGE i005(zcore) WITH 'COMPOUNDING' nv_iobjnm INTO _message.
    static_add_message( ).
    " In case a compounding infoObject is in the original
    " we must copy it.

    IF me->nv_cloned = rs_c_false.
      " For a clone... we don't need to update the compounding
      LOOP AT ns_original_meta-compounds INTO DATA(ls_compounds).
        DATA(lv_fromcmp) = ls_compounds-iobjcmp.
        ls_compounds-infoobject = nv_iobjnm.
        ls_compounds-objvers    = rs_c_objvers-active.
        ls_compounds-iobjcmp    = do_add_required_infoobject( ls_compounds-iobjcmp ).
        READ TABLE ns_new_meta-compounds TRANSPORTING NO FIELDS
          WITH KEY iobjcmp = ls_compounds-iobjcmp.
        IF sy-subrc <> 0.
          APPEND ls_compounds TO ns_new_meta-compounds.
        ENDIF.
        MESSAGE i009(zcore) WITH lv_fromcmp ls_compounds-iobjcmp 'Compounding' INTO _message.
        CALL METHOD static_add_message.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD do_adapt_attributes.

    DATA lv_iobjnm TYPE rsiobjnm.

    MESSAGE i005(zcore) WITH 'ATTRIBUTES' nv_iobjnm INTO _message.
    static_add_message( ).

    LOOP AT ns_original_meta-attributes INTO DATA(ls_attributes).
      TRY.
          " So we will find out if this attributes is in the system
          " and get the clone name
          lv_iobjnm = ls_attributes-attrinm.
          ls_attributes-attrinm = get_existing(  iv_iobjnm = ls_attributes-attrinm
                                                 iv_check_workinglist = rs_c_true ).
          IF ls_attributes-attrinm IS INITIAL.
            " The attribute is in the buffer and being prepared for cloning
            " hence we just add it as required object
            ls_attributes-attrinm = do_add_required_infoobject( lv_iobjnm ).
            MESSAGE i009(zcore) WITH lv_iobjnm ls_attributes-attrinm 'Attribute Present in workinglist' INTO _message.
            CALL METHOD static_add_message.
          ENDIF.
          READ TABLE ns_new_meta-attributes TRANSPORTING NO FIELDS
            WITH KEY attrinm = ls_attributes-attrinm.
          IF sy-subrc <> 0.
            ls_attributes-chabasnm = nv_iobjnm.
            ls_attributes-objvers  = rs_c_objvers-active.
            APPEND ls_attributes TO ns_new_meta-attributes.
            nv_updateiobj = rs_c_true.
            MESSAGE i017(zcore) WITH nv_iobjnm ls_attributes-attrinm INTO _message.
            static_add_message( ).
          ENDIF.
        CATCH cx_rs_not_found.
          " Basically the rule is: If the Clone is not in the attribute does
          " not get cloned
      ENDTRY.
    ENDLOOP.

    MESSAGE i005(zcore) WITH 'NAV.ATTRIBUTES' nv_iobjnm INTO _message.
    static_add_message( ).

    LOOP AT ns_original_meta-navigationattributes INTO DATA(ls_navigationattributes).
      TRY.
          lv_iobjnm = ls_navigationattributes-attrinm.
          ls_navigationattributes-attrinm = get_existing( iv_iobjnm = ls_navigationattributes-attrinm
                                                                      iv_check_workinglist = rs_c_true ).
          IF ls_navigationattributes-attrinm IS INITIAL.
            " The attribute is in the buffer and being prepared for cloning
            " hence we just add it as required object
            ls_navigationattributes-attrinm = do_add_required_infoobject( lv_iobjnm ).
            MESSAGE i009(zcore) WITH lv_iobjnm ls_navigationattributes-attrinm 'Nav.Attribute Present in workinglist' INTO _message.
            CALL METHOD static_add_message.
          ENDIF.
          READ TABLE ns_new_meta-navigationattributes TRANSPORTING NO FIELDS
            WITH KEY attrinm = ls_navigationattributes-attrinm.
          IF sy-subrc <> 0.
            ls_navigationattributes-chanm = nv_iobjnm.
            ls_navigationattributes-chanm = rs_c_objvers-active.
            ls_navigationattributes-atrnavnm = |{ ls_navigationattributes-chanm }__{ ls_navigationattributes-attrinm }|.
            nv_updateiobj = rs_c_true.
            APPEND ls_navigationattributes TO ns_new_meta-navigationattributes.
            MESSAGE i018(zcore) WITH nv_iobjnm ls_navigationattributes-chanm ls_navigationattributes-attrinm INTO _message.
            static_add_message( ).
          ENDIF.
        CATCH cx_rs_not_found.
          " Basically the rule is: If the Clone is not in the attribute does
          " not get cloned
      ENDTRY.
    ENDLOOP.

    " The content of atrnavinfoprovider contains the value that can be used as navigational attributes
    " when the InfoObject is used as InfoProvider
    MESSAGE i005(zcore) WITH 'Nav Attr for InfoObject as InfoProvider' nv_iobjnm INTO _message.
    static_add_message(  ).

    DATA:
      lv_needed_infoobject TYPE rsiobjnm,
      lv_needed_attriniobj TYPE rsiobjnm.

    LOOP AT ns_original_meta-atrnavinfoprovider INTO DATA(ls_atrnavinfoprovider).
      " Make sure the InfoObject as attribute is also listed as
      " This is a reference to an Attribute of the InfoObject itself AND an attribute of the
      " mentioned InfoObject
      CALL FUNCTION 'RSD_ATTRINM_GET_FROM_ATRNAVNM'
        EXPORTING
          i_atrnavnm = ls_atrnavinfoprovider-atrnavnm
        IMPORTING
          e_chanm    = lv_needed_infoobject
          e_attrinm  = lv_needed_attriniobj.
      lv_needed_infoobject  = get_existing( iv_iobjnm = lv_needed_infoobject
                                            iv_check_workinglist = rs_c_true ).
      IF lv_needed_infoobject IS INITIAL.
        CONTINUE.
      ENDIF.
      lv_needed_attriniobj  = get_existing( iv_iobjnm = lv_needed_attriniobj
                                           iv_check_workinglist = rs_c_true ).
      IF lv_needed_attriniobj IS INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE ns_new_meta-atrnavinfoprovider TRANSPORTING NO FIELDS
         WITH KEY atrnavnm = ls_atrnavinfoprovider-atrnavnm.
      IF sy-subrc <> 0.
        ls_atrnavinfoprovider-infoobject = nv_iobjnm.
        ls_atrnavinfoprovider-atrnavnm   = |{ lv_needed_infoobject }__{ lv_needed_attriniobj }|.
        APPEND ls_atrnavinfoprovider TO ns_new_meta-atrnavinfoprovider.
      ENDIF.

    ENDLOOP.

    MESSAGE i005(zcore) WITH 'HIER CHARACTERISTICS' nv_iobjnm INTO _message.
    static_add_message( ).

    " Add all InfoObjects that can be used in the hierarchy
    LOOP AT ns_original_meta-hierarchycharacteristics INTO DATA(ls_hierchar).
      DATA(lv_iobjhier) = ls_hierchar-iobjnm.
      ls_hierchar-iobjnm = do_add_required_infoobject( ls_hierchar-iobjnm ).
      ls_hierchar-chabasnm = nv_iobjnm.
      READ TABLE ns_new_meta-hierarchycharacteristics TRANSPORTING NO FIELDS
        WITH KEY iobjnm = ls_hierchar-iobjnm.
      IF sy-subrc <> 0.
        APPEND ls_hierchar TO ns_new_meta-hierarchycharacteristics.
      ENDIF.
      MESSAGE i009(zcore) WITH lv_iobjhier ls_hierchar-iobjnm 'Hierarchy Characteristics' INTO _message.
      CALL METHOD static_add_message.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_existing.

    DATA ls_return TYPE bapiret2.
    DATA(lv_iobjnm) = nv_original.
    IF iv_iobjnm IS SUPPLIED.
      lv_iobjnm = iv_iobjnm.
    ENDIF.

    CLEAR rv_iobjnm.
    DO 6 TIMES.
      CASE sy-index.
        WHEN 1.
          " Never copy the fixed InfoObjects, but also just check if these have been activated
          SELECT SINGLE iobjnm INTO rv_iobjnm
              FROM rsdiobjfix
              WHERE iobjnm = lv_iobjnm.
          IF sy-subrc <> 0.
            CONTINUE.
          ELSE.
            "" Make sure that the fixed infoObject have been activated
            CALL FUNCTION 'BAPI_IOBJ_GETDETAIL'
              EXPORTING
                infoobject = rv_iobjnm
                version    = rs_c_objvers-active
              IMPORTING
                return     = ls_return.
            IF ls_return-number = '000'.
              nv_cloned = rs_c_true.
              MESSAGE i016(zcore) WITH lv_iobjnm rv_iobjnm 'Fixed InfoObject - copy' INTO _message.
              CALL METHOD static_add_message.
              EXIT.
            ELSE.
              MESSAGE e015(zcore) WITH lv_iobjnm INTO _message.
              CALL METHOD static_add_message.
              RAISE EXCEPTION TYPE cx_rs_not_found
                EXPORTING
                  key    = _message
                  object = 'Original Object'.
            ENDIF.
          ENDIF.
        WHEN 2.
          IF nv_missing = rs_c_false.
            " Find out if the name is already in
            SELECT SINGLE iobjnm INTO rv_iobjnm
                FROM zcore_trans
                WHERE original    = lv_iobjnm AND
                       doccluster = gv_target_cluster.
            IF sy-subrc = 0.
              nv_cloned = rs_c_true.
              MESSAGE i016(zcore) WITH lv_iobjnm rv_iobjnm 'Existing clone Object - Update' INTO _message.
              CALL METHOD static_add_message.
              EXIT.
            ENDIF.
          ENDIF.
        when 3.
          " Techincally you should be able to clone any object
          " 0 is business content
          " X is cross
          " Z is playground
          if nv_rfcdest = 'NONE' and get_cluster( lv_iobjnm ) <> gv_target_cluster and get_cluster( lv_iobjnm ) between 'A' and 'Y'.
            IF iv_check_workinglist = rs_c_true.
              READ TABLE gth_workinglist TRANSPORTING NO FIELDS
                 WITH TABLE KEY table_line = lv_iobjnm.
              IF sy-subrc = 0.
                MESSAGE i016(zcore) WITH lv_iobjnm '?' 'No processing yet - Clone?' INTO _message.
                EXIT.
              ENDIF.
            else.
              nv_cloned = rs_c_false.
              raise exception type cx_rs_not_found.
            endif.
          endif.
        WHEN 4.
          " So let us see if the original (name) does check_exists in
          " a active version in the this installation
          select single iobjnm
              from zcore_fixed
              into @data(lv_fixed_iobjnm)
              where iobjnm = @lv_iobjnm.
          if sy-subrc <> 0.
            CALL FUNCTION 'BAPI_IOBJ_GETDETAIL'
              EXPORTING
                infoobject = lv_iobjnm
                version    = rs_c_objvers-active
              IMPORTING
                return     = ls_return.
            IF ls_return-number = '000'.
              rv_iobjnm = lv_iobjnm.
              nv_cloned = rs_c_true.
              MESSAGE i016(zcore) WITH lv_iobjnm rv_iobjnm 'Reuase name - copy' INTO _message.
              CALL METHOD static_add_message.
              EXIT.
            ENDIF.
          else.
            " The infoobject is listed in the table to not be already cloned,
            " This is a way of preventing a activated InfoObject to be part
            " of another project. f.x. if an InfoObject is activated for ONE purpose
            " this Iobj will always be part of the clone process. This to prevent
            "
          endif.
        WHEN 5.
          IF iv_check_workinglist = rs_c_true.
            READ TABLE gth_workinglist TRANSPORTING NO FIELDS
               WITH TABLE KEY table_line = lv_iobjnm.
            IF sy-subrc = 0.
              MESSAGE i016(zcore) WITH lv_iobjnm '?' 'No processing yet - Clone?' INTO _message.
              EXIT.
            ENDIF.
          ENDIF.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE cx_rs_not_found.
      ENDCASE.
    ENDDO.

    IF rv_iobjnm IS NOT INITIAL AND iv_check_workinglist IS NOT SUPPLIED.
      nv_iobjnm = rv_iobjnm.
    ENDIF.

  ENDMETHOD.


  method get_cluster.
  " Techincally the correct way would be to select
  " the InfoArea of the InfoObject and return that cluster
    if iv_iobjnm is supplied.
      data(lv_iobjnm) = iv_iobjnm.
    else.
      lv_iobjnm = nv_iobjnm.
    endif.

    try.
        "Not sure that I would like to have the ZI_CORE_ContentView as a preqequisute for this
        "little module
        select single zi~doccluster
          from ('ZI_CORE_ContentView as zi inner join rsdiobj as rs on zi~InfoArea = rs~mtinfoarea')
          where rs~objvers = @rs_c_objvers-active and
                rs~iobjnm  = @lv_iobjnm
          into @rv_cluster.
      CATCH cx_sy_sql_error.
        " The ZI_CORE_ContentView have not been implemented
    endtry.

    if sy-subrc <> 0.
      rv_cluster = lv_iobjnm(1).
    endif.

    if rv_cluster not between 'A' and 'Z'.
      rv_cluster = gc_cluster-common.
    endif.

  ENDMETHOD.

  METHOD get_new_name.
** Method that will find a new name when an InfoObject is taken from a remote source
** or business content, that being either local or remote. It is possible to take
** BCT InfoObject from 'D' version is both local and remote
** - '0COMP_CODE becomes XCOMP_COD
** - '0COMP_CODF becomes XCOMP_CO1
** - '0DATE becomes XDATE1
** The latter is wrong as we will not make a copy of 0DATE, be it's just an example
    TRY.
        CALL METHOD get_existing.
        RETURN.
      CATCH cx_rs_not_found.
        "" Basically we need to fill the blanks
    ENDTRY.

    CLEAR nv_iobjnm.

    DATA lv_iobjnm TYPE rsiobjnm.

    " Regardless
    IF nv_missing = rs_c_true.

    ENDIF.

    nv_iobjnm = nv_original.
    IF nv_missing = rs_c_true.
      SELECT SINGLE iobjnm
        FROM zcore_trans
        INTO @nv_iobjnm
        WHERE original   = @nv_original AND
              doccluster = @gv_target_cluster.
    ELSEIF nv_original+0(1) = '0'.
      SELECT SINGLE smartname
          INTO @nv_iobjnm
          FROM zcore_smart
          WHERE remoteiobjnm = @nv_original.
      if sy-subrc = 0.
        nv_iobjnm = |{ nv_target_cluster }{ nv_iobjnm }|.
      else.
      " No smart translation of BCS InfoObject
      " Use standard transaltion
      " Standard InfoObject is always translated to 'X' first
        nv_iobjnm+0(1) = nv_target_cluster.
      endif.

      if gv_prefix is NOT INITIAL.
        data(lv_prelen) = strlen( gv_prefix ).
        if nv_iobjnm+1(lv_prelen) <> gv_prefix.
          nv_iobjnm = |{ nv_iobjnm(1) }{ gv_prefix }{ nv_iobjnm+1 }|.
        endif.
      endif.

      DATA(lv_iobjlen) = strlen( nv_iobjnm ).

      DO.
        DATA(lv_index) = sy-index - 1.
        IF lv_iobjlen GT 9.
          lv_iobjnm = nv_iobjnm.
          lv_prelen = lv_prelen + 1.
          if lv_prelen < 2.
            lv_prelen = 2.
          endif.

          do.
            if lv_iobjnm ca '_'.
              " First option is to replace the underscores
              replace FIRST OCCURRENCE OF '_' IN lv_iobjnm with '' in CHARACTER MODE.
            elseif lv_iobjnm+lv_prelen ca 'A'.
              REPLACE FIRST OCCURRENCE OF 'A' in lv_iobjnm+lv_prelen with '' in CHARACTER MODE.
            elseif lv_iobjnm+lv_prelen ca 'E'.
              REPLACE FIRST OCCURRENCE OF 'E' in lv_iobjnm+lv_prelen with '' in CHARACTER MODE.
            elseif lv_iobjnm+lv_prelen ca 'I'.
              REPLACE FIRST OCCURRENCE OF 'I' in lv_iobjnm+lv_prelen with '' in CHARACTER MODE.
            elseif lv_iobjnm+lv_prelen ca 'O'.
              REPLACE FIRST OCCURRENCE OF 'O' in lv_iobjnm+lv_prelen with '' in CHARACTER MODE.
            elseif lv_iobjnm+lv_prelen ca 'U'.
              REPLACE FIRST OCCURRENCE OF 'U' in lv_iobjnm+lv_prelen with '' in CHARACTER MODE.
            elseif lv_iobjnm+lv_prelen ca 'Y'.
              REPLACE FIRST OCCURRENCE OF 'Y' in lv_iobjnm+lv_prelen with '' in CHARACTER MODE.
            endif.

            if strlen( lv_iobjnm ) LE 9 or lv_iobjnm = nv_iobjnm.
              " So have we removed enough underscores and characters so that the length is below
              " 9... or do we not have more tools in the back
              nv_iobjnm = lv_iobjnm.
              exit.
            endif.
            nv_iobjnm = lv_iobjnm.
          enddo.
        endif.

        lv_iobjlen = strlen( nv_iobjnm ).
        IF lv_iobjlen GE 8.
          " If the Name is 8 or 9 characters long we will use the last
          " character on find a unique name... or crash in the process
          " when the lv_index is above the length of cv_replacestring
          " Make sure that the length is maximum 9 characters
          nv_iobjnm      = nv_iobjnm(9).
          IF lv_index >  0.
            nv_iobjnm+8(1) = cv_replacestring+lv_index(1).
          ENDIF.
        ELSE.
          IF lv_index >  0.
            nv_iobjnm+lv_iobjlen(1) = cv_replacestring+lv_index(1).
          ENDIF.
        ENDIF.
        IF check_exists(  ) = rs_c_false AND static_name_already_used(  nv_iobjnm ) = rs_c_false.
          " The name have not been used
          " check if it is valid
          CALL FUNCTION 'RSD_NAME_CHECK'
            EXPORTING
              i_objnm             = nv_iobjnm
              i_tlogo             = rs_c_tlogo-infoobject
            EXCEPTIONS
              name_invalid        = 1
              bwappl_not_selected = 2
              OTHERS              = 3.
          CASE sy-subrc.
            WHEN 0.
              EXIT.
            WHEN 1.
              " lets try the next
            WHEN OTHERS.
              " Something when wrong
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDCASE.
        ENDIF.
        ASSERT lv_index <= strlen(  cv_replacestring ).
      ENDDO.
    ELSEIF
       " Again - We should have a check that the cluster is a valid cluster
       " But this allow clone between clusters
       nv_original+0(1) between 'A' and 'Z' AND nv_rfcdest = 'NONE'.
      " From the new commen datamodel in 'X'
      " the the new model from initialization
      nv_iobjnm+0(1) = nv_target_cluster.
    ELSEIF
       nv_rfcdest <> 'NONE'.
      " Remote none Business content InfoObject
      SELECT SINGLE smartname
          INTO @nv_iobjnm
          FROM zcore_smart
          WHERE remoteiobjnm = @nv_original.
      IF sy-subrc = 0.
        SELECT SINGLE iobjnm
          INTO @lv_iobjnm
          FROM zcore_fixed
          WHERE iobjnm = @nv_iobjnm.
        IF sy-subrc = 0.
          " Going from fishy stuff to standard
          " no concatenating with the model
        ELSE.
          " found a preferred translation of the InfoObject
          " kind of moving from ZZSOME to MEANING
          CONCATENATE nv_target_cluster nv_iobjnm INTO nv_iobjnm.
        ENDIF.
      ELSE.
        CONCATENATE nv_target_cluster nv_iobjnm INTO nv_iobjnm.
        nv_iobjnm = nv_iobjnm(9).
        lv_iobjlen = strlen( nv_iobjnm ).
        DO.
          lv_index = sy-index.
          IF lv_iobjlen GE 8.
            " If the Name is 8 or 9 characters long we will use the last
            " character on find a unique name... or crash in the process
            " when the lv_index is above the length of cv_replacestring
            " Make sure that the length is maximum 9 characters
            nv_iobjnm       = nv_iobjnm(9).
            nv_iobjnm+8(1) = cv_replacestring+lv_index(1).
          ELSE.
            nv_iobjnm+lv_iobjlen(1) = cv_replacestring+lv_index(1).
          ENDIF.
          IF check_exists( ) = rs_c_false.
            CALL FUNCTION 'RSD_NAME_CHECK'
              EXPORTING
                i_objnm             = nv_iobjnm
                i_tlogo             = rs_c_tlogo-infoobject
              EXCEPTIONS
                name_invalid        = 1
                bwappl_not_selected = 2
                OTHERS              = 3.
            CASE sy-subrc.
              WHEN 0.
                EXIT.
              WHEN 1.
                " lets try the next
              WHEN OTHERS.
                " Something when wrong
                MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDCASE.
          ENDIF.
          ASSERT lv_index <= strlen(  cv_replacestring ).
        ENDDO.
      ENDIF.
    ELSE.
      " We must adapt the program to deal with this
      ASSERT 1 = 0.
    ENDIF.

    rv_new_name = nv_iobjnm.

    MESSAGE i006(zcore) WITH rv_new_name nv_original INTO _message.
    static_add_message( ).

  ENDMETHOD.

  METHOD constructor.
    nv_original     = iv_original.
    nv_target_cluster = iv_target_cluster.
    nv_objvers      = iv_objvers.
    nv_rfcdest = iv_rfcdest.

    DATA ls_return TYPE bapiret2.
    DATA lv_mess   TYPE char72.

    IF check_original_check_exists( ) = rs_c_true.
      MESSAGE i003(zcore) WITH nv_original nv_rfcdest INTO _message.
      static_add_message(  ).
      "" Could be that we run into a case of missing objects from the remote system
      "" if f.x. the HANAFIELDSMAPPING, XXLA... is not supported
      CALL FUNCTION 'BAPI_IOBJ_GETDETAIL'
        DESTINATION nv_rfcdest
        EXPORTING
          version                  = nv_objvers
          infoobject               = nv_original
        IMPORTING
          details                  = ns_original_meta-details
          return                   = ls_return
          is_atr                   = ns_original_meta-atr
          details_2                = ns_original_meta-details_2
        TABLES
          compounds                = ns_original_meta-compounds
          attributes               = ns_original_meta-attributes
          navigationattributes     = ns_original_meta-navigationattributes
          atrnavinfoprovider       = ns_original_meta-atrnavinfoprovider
          hierarchycharacteristics = ns_original_meta-hierarchycharacteristics
          elimination              = ns_original_meta-elimination
          hanafieldsmapping        = ns_original_meta-hanafieldsmapping
          xxlattributes            = ns_original_meta-xxlattributes
        EXCEPTIONS
          communication_failure    = 4 MESSAGE lv_mess
          system_failure           = 8 MESSAGE lv_mess.
      IF sy-subrc <> 0.
        MESSAGE lv_mess TYPE rs_c_error.
        RAISE EXCEPTION TYPE cx_rs_error.
      ENDIF.
      IF ls_return-number IS NOT INITIAL.
        CALL METHOD static_add_message( VALUE #( msgty = ls_return-type
                                          msgid = ls_return-id
                                          msgno = ls_return-number
                                          msgv1 = ls_return-message_v1
                                          msgv2 = ls_return-message_v2
                                          msgv3 = ls_return-message_v3
                                          msgv4 = ls_return-message_v4 ) ).
      ENDIF.
    ELSE.
      MESSAGE i008(zcore) WITH nv_original nv_objvers nv_rfcdest INTO _message.
      CALL METHOD static_add_message.
      RAISE EXCEPTION TYPE cx_rs_not_found
        EXPORTING
          key    = _message
          object = 'Original Object'.
    ENDIF.
    nv_processing = gc_processing-persistent_none.

  ENDMETHOD.

  METHOD get_parameter_list.

    DATA:
      lt_exception_list     TYPE STANDARD TABLE OF rsexc,
      lt_export_parameter   TYPE STANDARD TABLE OF rsexp,
      lt_import_parameter   TYPE STANDARD TABLE OF rsimp,
      lt_changing_parameter TYPE STANDARD TABLE OF rscha,
      lt_tables_parameter   TYPE STANDARD TABLE OF rstbl.

    CALL FUNCTION 'FUNCTION_IMPORT_INTERFACE'
      DESTINATION nv_rfcdest
      EXPORTING
        funcname              = iv_funcname
      TABLES
        exception_list        = lt_exception_list
        export_parameter      = lt_export_parameter
        import_parameter      = lt_import_parameter
        changing_parameter    = lt_changing_parameter
        tables_parameter      = lt_tables_parameter
      EXCEPTIONS
        error_message         = 1
        function_not_found    = 2
        invalid_name          = 3
        communication_failure = 10
        system_failure        = 11
        OTHERS                = 99.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    DATA ls_parambind TYPE abap_func_parmbind .

    ls_parambind-kind = abap_func_exporting .
    LOOP AT lt_export_parameter ASSIGNING FIELD-SYMBOL(<ls_export>).
      ls_parambind-name = <ls_export>-parameter.
      INSERT ls_parambind INTO TABLE rt_parambind.
    ENDLOOP.

    ls_parambind-kind = abap_func_importing .
    LOOP AT lt_import_parameter ASSIGNING FIELD-SYMBOL(<ls_import>).
      ls_parambind-name = <ls_import>-parameter.
      INSERT ls_parambind INTO TABLE rt_parambind.
    ENDLOOP.

    ls_parambind-kind = abap_func_changing.
    LOOP AT lt_changing_parameter ASSIGNING FIELD-SYMBOL(<ls_changing>).
      ls_parambind-name = <ls_changing>-parameter.
      INSERT ls_parambind INTO TABLE rt_parambind.
    ENDLOOP.

    ls_parambind-kind = abap_func_tables.
    LOOP AT lt_tables_parameter ASSIGNING FIELD-SYMBOL(<ls_tables>).
      ls_parambind-name = <ls_tables>-parameter.
      INSERT ls_parambind INTO TABLE rt_parambind.
    ENDLOOP.

  ENDMETHOD.

  METHOD check_original_check_exists.

    DATA ls_return TYPE bapiret2.
    DATA lv_mess TYPE char72.

    CALL FUNCTION 'BAPI_IOBJ_GETDETAIL'
      DESTINATION nv_rfcdest
      EXPORTING
        infoobject            = nv_original
        version               = nv_objvers
      IMPORTING
        return                = ls_return
      EXCEPTIONS
        communication_failure = 4 MESSAGE lv_mess
        system_failure        = 8 MESSAGE lv_mess.
    IF sy-subrc <> 0.
      MESSAGE lv_mess TYPE rs_c_error.
      RAISE EXCEPTION TYPE cx_rs_error.
    ENDIF.

    IF ls_return-number = '000'.
      rv_check_exists = rs_c_true.
    ELSE.
      CALL METHOD static_add_message( VALUE #( msgty = ls_return-type
                                        msgid = ls_return-id
                                        msgno = ls_return-number
                                        msgv1 = ls_return-message_v1
                                        msgv2 = ls_return-message_v2
                                        msgv3 = ls_return-message_v3
                                        msgv4 = ls_return-message_v4 ) ).
      rv_check_exists = rs_c_false.
    ENDIF.
  ENDMETHOD.

  METHOD check_exists.

    DATA ls_return TYPE bapiret2.

    CALL FUNCTION 'BAPI_IOBJ_GETDETAIL'
      EXPORTING
        infoobject = nv_iobjnm
        version    = iv_objvers
      IMPORTING
        return     = ls_return.

    IF ls_return-number = '000'.
      rv_check_exists = rs_c_true.
    ELSE.
      IF iv_static_add_message = rs_c_true.
        CALL METHOD static_add_message( VALUE #( msgty = rs_c_info
                                          msgid = ls_return-id
                                          msgno = ls_return-number
                                          msgv1 = ls_return-message_v1
                                          msgv2 = ls_return-message_v2
                                          msgv3 = ls_return-message_v3
                                          msgv4 = ls_return-message_v4 ) ).
      ENDIF.
      rv_check_exists = rs_c_false.
    ENDIF.
  ENDMETHOD.

  METHOD static_reset_all.
    LOOP AT gth_cluster_iobj ASSIGNING FIELD-SYMBOL(<ls_iobj>).
      CALL METHOD <ls_iobj>-r_iobjnm->do_reset( ).
    ENDLOOP.

    CLEAR gv_log_handle.

  ENDMETHOD.

  METHOD do_reset.
    nv_processing = gc_processing-persistent_none.
  ENDMETHOD.

  METHOD static_execute_cloning.

    DATA:
      lt_rng_iobj  TYPE RANGE OF rsiobjnm,
      lt_rng_trans TYPE RANGE OF rstran.

    LOOP AT gth_workinglist ASSIGNING FIELD-SYMBOL(<lv_iobjnm>).
      DATA(lr_iobjnm) = gth_cluster_iobj[ original = <lv_iobjnm> target_cluster = gv_target_cluster ]-r_iobjnm.
      " Make sure the parent is itself for the intitial call
      append lines of lr_iobjnm->do_persistest_processing( ) to lt_rng_iobj.
    ENDLOOP.

    sort lt_rng_iobj by low.
    delete ADJACENT DUPLICATES FROM lt_rng_iobj COMPARING low.

    IF iv_no_activation = rs_c_false.
      SUBMIT rsdg_iobj_activate WITH
         so_iobj IN lt_rng_iobj AND RETURN.
    ENDIF.
    CALL METHOD static_display_log.

  ENDMETHOD.

  METHOD static_add_fixed_infoobject.
    DATA lt_iobjnm TYPE STANDARD TABLE OF zcore_fixed.
    DATA lt_notact TYPE STANDARD TABLE OF zcore_fixed.

    SELECT * FROM rsdiobj
      WHERE
        iobjnm IN @it_iobjnm AND
        objvers = @rs_c_objvers-active
      INTO CORRESPONDING FIELDS OF TABLE @lt_iobjnm.
    SELECT * FROM rsdiobj
      WHERE
        iobjnm IN @it_iobjnm AND
        objvers = @rs_c_objvers-delivery
      INTO CORRESPONDING FIELDS OF TABLE @lt_notact.
    LOOP AT lt_notact INTO DATA(ls_notact).
      READ TABLE lt_iobjnm TRANSPORTING NO FIELDS
        WITH KEY iobjnm = ls_notact-iobjnm.
      IF sy-subrc = 0.
        DELETE lt_notact.
      ELSE.
        MESSAGE i015(zcore) WITH ls_notact-iobjnm.
      ENDIF.
    ENDLOOP.

    INSERT zcore_fixed FROM TABLE lt_iobjnm.
    IF sy-subrc = 0.
      MESSAGE i007(zcore) WITH sy-dbcnt 'ZCORE_FIXED'.
    ENDIF.
    COMMIT WORK.

  ENDMETHOD.

  METHOD static_do_insertlist.

    gv_rfcdest = iv_rfcdest.
    gv_target_cluster = iv_target_cluster.

    " Insert the entries in the list and start the cloning
    LOOP AT it_iobjnm ASSIGNING FIELD-SYMBOL(<ls_iobjnm>).
      INSERT <ls_iobjnm>-low INTO TABLE gth_workinglist.
    ENDLOOP.

    TRY.
        LOOP AT gth_workinglist ASSIGNING FIELD-SYMBOL(<lv_iobjnm>).
          CALL METHOD zcl_core_iobj_tool=>factory(
              iv_objvers      = iv_objvers
              iv_rfcdest      = iv_rfcdest
              iv_target_cluster = iv_target_cluster
              iv_original     = <lv_iobjnm> ).
        ENDLOOP.
      CATCH cx_rs_error.
        CALL METHOD static_display_log.
        REFRESH: gth_cluster_iobj, gth_workinglist.
    ENDTRY.
  ENDMETHOD.

  METHOD static_set_infoarea.
    gv_infoarea = iv_infoarea.
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

    IF is_msg IS SUPPLIED.
      ls_msg = is_msg.
    ELSE.
      MOVE-CORRESPONDING syst TO ls_msg.
    ENDIF.

    ls_msg-detlevel = gv_detlevel.
*    ls-msg-probclass.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = gv_log_handle
        i_s_msg      = ls_msg
      EXCEPTIONS
        OTHERS       = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

ENDCLASS.


