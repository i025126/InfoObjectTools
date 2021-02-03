*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 27.01.2020 at 10:36:32
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZCORE_FIXED.....................................*
DATA:  BEGIN OF STATUS_ZCORE_FIXED                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCORE_FIXED                   .
CONTROLS: TCTRL_ZCORE_FIXED
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZCORE_SMART.....................................*
DATA:  BEGIN OF STATUS_ZCORE_SMART                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCORE_SMART                   .
CONTROLS: TCTRL_ZCORE_SMART
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZCORE_TRANS.....................................*
DATA:  BEGIN OF STATUS_ZCORE_TRANS                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCORE_TRANS                   .
CONTROLS: TCTRL_ZCORE_TRANS
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZCORE_FIXED                   .
TABLES: *ZCORE_SMART                   .
TABLES: *ZCORE_TRANS                   .
TABLES: ZCORE_FIXED                    .
TABLES: ZCORE_SMART                    .
TABLES: ZCORE_TRANS                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
