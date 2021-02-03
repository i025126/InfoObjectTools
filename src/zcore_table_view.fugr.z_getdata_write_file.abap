FUNCTION Z_GETDATA_WRITE_FILE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_OPEN) TYPE  RS_BOOL
*"     VALUE(IV_FILENAME) TYPE  STRING
*"     VALUE(IV_CONTENT) TYPE  STRING OPTIONAL
*"  EXPORTING
*"     VALUE(EV_BYTEWRITTEN) TYPE  I
*"  TABLES
*"      IT_DATA STRUCTURE  CHAR8000 OPTIONAL
*"  EXCEPTIONS
*"      CANNOT_OPEN_FILE
*"      CANNOT_WRITE_TO_FILE
*"----------------------------------------------------------------------
  if iv_open = rs_c_true.
    OPEN DATASET iv_filename FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
  else.
    OPEN DATASET iv_filename FOR APPENDING IN TEXT MODE ENCODING UTF-8.
  endif.
  if sy-subrc <> 0.
    raise cannot_open_file.
  endif.

  if iv_content is SUPPLIED.
    TRANSFER iv_content to iv_filename.
  endif.

  if it_data is SUPPLIED.
    loop at it_data ASSIGNING FIELD-SYMBOL(<ls_data>).
      TRANSFER <ls_data> to iv_filename.
    ENDLOOP.
  endif.

  if sy-subrc <> 0.
    raise cannot_write_to_file.
  endif.

  close DATASET iv_filename.

ENDFUNCTION.
