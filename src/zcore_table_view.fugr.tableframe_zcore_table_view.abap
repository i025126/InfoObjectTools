*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZCORE_TABLE_VIEW
*   generation date: 06.01.2020 at 14:46:10
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZCORE_TABLE_VIEW   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
