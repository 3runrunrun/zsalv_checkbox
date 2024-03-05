*&---------------------------------------------------------------------*
*& Report zsalv_checkbox
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsalv_checkbox.

INCLUDE <icon>.

CLASS lcl_event_handler DEFINITION DEFERRED.

TYPES: BEGIN OF ty_report,
         no         TYPE c LENGTH 4,
         item       TYPE c LENGTH 6,
         matnr      TYPE matnr,
         view       TYPE char10,
         status     TYPE icon_d,
         sel        TYPE char1,
         t_celltype TYPE salv_t_int4_column,
       END OF ty_report.

TYPES: BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
       END OF ty_mara.

TYPES: BEGIN OF ty_fcat,
         name TYPE lvc_fname,
         text TYPE scrtext_l,
       END OF ty_fcat.

DATA: gi_report TYPE TABLE OF ty_report,
      gi_mara   TYPE TABLE OF ty_mara,
      gi_fcat   TYPE TABLE OF ty_fcat.

DATA: gw_report TYPE ty_report.
DATA: go_alv     TYPE REF TO cl_salv_table,
      go_handler TYPE REF TO lcl_event_handler.

CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    METHODS on_click FOR EVENT link_click OF cl_salv_events_table
      IMPORTING row column.

ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_click.

    READ TABLE gi_report ASSIGNING FIELD-SYMBOL(<fs_clicked>) INDEX row.
    LOOP AT gi_report ASSIGNING FIELD-SYMBOL(<fs_report>)
    WHERE no = <fs_clicked>-no.
      <fs_report>-sel = SWITCH #( <fs_report>-sel
          WHEN '' THEN 'X'
          ELSE '' ).
    ENDLOOP.

    go_alv->refresh( ).

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  PERFORM f_get_data.
  PERFORM f_build_data.
  PERFORM f_display_data.

FORM f_get_data.

  SELECT matnr
  FROM mara
  WHERE mtart = 'ZFGD'
  ORDER BY matnr DESCENDING
  INTO CORRESPONDING FIELDS OF TABLE @gi_mara
  UP TO 3 ROWS.

  APPEND VALUE #( name = 'NO'     text = 'ID' ) TO gi_fcat.
  APPEND VALUE #( name = 'ITEM'   text = 'Item' ) TO gi_fcat.
  APPEND VALUE #( name = 'MATNR'  text = 'Material' ) TO gi_fcat.
  APPEND VALUE #( name = 'VIEW'   text = 'View' ) TO gi_fcat.
  APPEND VALUE #( name = 'STATUS' text = 'Status' ) TO gi_fcat.
  APPEND VALUE #( name = 'SEL'    text = 'Checkbox' ) TO gi_fcat.

ENDFORM.

FORM f_build_data.

  DO 3 TIMES.
    DATA(id_idx) = sy-index. "--ID

    LOOP AT gi_mara ASSIGNING FIELD-SYMBOL(<fs_mara>).
      DATA(seq_idx) = sy-tabix. "-- item number

      "-- fill the T_CELLTYPE column for each row,
      "-- based on ITEM NUMBER
      DATA(ctype) = SWITCH salv_s_int4_column( seq_idx
        WHEN 1 THEN VALUE #( columnname = 'SEL' value = if_salv_c_cell_type=>checkbox_hotspot )
        ELSE VALUE #( columnname = 'SEL' value = if_salv_c_cell_type=>checkbox ) ).

      DATA(view) = SWITCH char10( seq_idx
        WHEN 1 THEN 'BASIC'
        WHEN 2 THEN 'PLANT'
        WHEN 3 THEN 'PURCHASING' ).

      gw_report = VALUE #(
        no = id_idx
        item = seq_idx
        matnr = <fs_mara>-matnr
        view = view
        status = icon_incomplete ).

      APPEND ctype TO gw_report-t_celltype.
      APPEND gw_report TO gi_report.

    ENDLOOP.

  ENDDO.

ENDFORM.

FORM f_display_data.

  DATA lo_collist TYPE REF TO cl_salv_column_list.

  cl_salv_table=>factory(
    IMPORTING
      r_salv_table = go_alv
    CHANGING
      t_table      = gi_report
  ).

  DATA(lo_cols) = go_alv->get_columns( ).
  lo_cols->set_optimize(
    value = abap_true
  ).

*-- fieldcat
  LOOP AT gi_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    DATA(lo_col) = lo_cols->get_column( columnname = <fs_fcat>-name ).

    lo_col->set_long_text( value = <fs_fcat>-text ).
  ENDLOOP.

*-- Set CHECKBOX_HOTSPOT first
  lo_collist ?= lo_cols->get_column( columnname = 'SEL' ).
  lo_collist->set_cell_type(
    value = if_salv_c_cell_type=>checkbox_hotspot
  ).

*-- Then, setting up cell type, based on value in T_CELLTYPE
  lo_cols->set_cell_type_column( value = 'T_CELLTYPE' ).

*-- events
  DATA(lo_events) = go_alv->get_event( ).

  CREATE OBJECT go_handler.
  SET HANDLER go_handler->on_click FOR lo_events.

  go_alv->display( ).

ENDFORM.
