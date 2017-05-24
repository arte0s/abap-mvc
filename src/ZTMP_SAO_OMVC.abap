*&---------------------------------------------------------------------*
*& Report  ZTMP_SAO_OMVC
*&
*&---------------------------------------------------------------------*
report  ztmp_sao_omvc no standard page heading.
*&---------------------------------------------------------------------*
*& Data types and interface
*&---------------------------------------------------------------------*
type-pools icon.

types t_index type sy-index.

types: begin of ts_data,
         key type t_index,
         text type text20,
       end of ts_data.

types tt_data type standard table of ts_data with default key.
*&=====================================================================*
*& Class Model
*&---------------------------------------------------------------------*
class cl_model definition final.
  public section.
    events data_changed.

    methods modified
      importing
        is_data type ts_data
      raising
        cx_sy_duplicate_key.

    methods remove
      raising
        cx_sy_duplicate_key.

    methods get_all
      returning
        value(rt_data) type tt_data.

    methods get_index
      returning
        value(r_index) type index.

    methods set_index
      importing
        i_index type t_index
      raising
        cx_sy_duplicate_key.

  private section.
    data mt_data type tt_data.
    data m_index type t_index.
endclass.                    "c2 DEFINITION
*&---------------------------------------------------------------------*
*       CLASS cl_model IMPLEMENTATION
*----------------------------------------------------------------------*
class cl_model implementation.

  method modified.
    field-symbols <ls_data> like line of mt_data.

    read table mt_data assigning <ls_data> index m_index.
    if sy-subrc = 0.
      <ls_data>-text = is_data-text.
    else.
      data l_key type ts_data-key.

      data lt_data like mt_data.
      lt_data = mt_data.
      sort lt_data by key descending.

      data ls_data like line of lt_data.
      read table lt_data into ls_data index 1.
      if sy-subrc = 0.
        l_key = ls_data-key + 1.
      else.
        l_key = 1.
      endif.

      append initial line to mt_data assigning <ls_data>.
      <ls_data>-key = l_key.
      <ls_data>-text = is_data-text.
    endif.

    raise event data_changed.
  endmethod.                    "cl_observer

  method remove.
    read table mt_data transporting no fields index m_index.
    if sy-subrc = 0.
      delete mt_data index m_index.
      clear m_index.
      raise event data_changed.
    else.
      raise exception type cx_sy_duplicate_key.
    endif.
  endmethod.                    "cl_observer

  method set_index.
    data l_length type i.
    l_length = lines( mt_data ).

    if i_index >= 0 and i_index <= l_length.
      m_index = i_index.
      raise event data_changed.
    else.
      raise exception type cx_sy_duplicate_key.
    endif.
  endmethod.                    "cl_observer

  method get_all.
    rt_data = mt_data.
  endmethod.                    "cl_observer

  method get_index.
    r_index = m_index.
  endmethod.                    "cl_observer
endclass.                    "c2 IMPLEMENTATION
*&=====================================================================*
*       CLASS cl_frame
*----------------------------------------------------------------------*
class cl_frame_stat definition final.
  public section.

    class-methods start
      importing
        i_beg_col type i default 1
        i_beg_row type i optional
        i_width type i.

    class-methods middle.
    class-methods line.
    class-methods end.

  private section.
    class-data m_beg_row type i.
    class-data m_beg_col type i.
    class-data m_end_col type i.
    class-data m_length type i.
endclass.                    "cl_frame
*----------------------------------------------------------------------*
*       CLASS cl_frame IMPLEMENTATION
*----------------------------------------------------------------------*
class cl_frame_stat implementation.
  method start.
    m_beg_col = i_beg_col.
    m_beg_row = i_beg_row.

    m_end_col = i_beg_col + i_width.
    m_length = m_end_col - m_beg_col + 1.

*Позиционируем и выводим текст + боковые границы
    data y type i.
    y = m_beg_row + 1.
    skip to line y.
    position m_beg_col.
    write '|'.
  endmethod.                    "start

  method middle.
    write at m_end_col '|'.

    data l_hight type i.
    l_hight = m_beg_col - m_end_col.
    skip to line l_hight.

    write at m_beg_col '|'.
  endmethod.                    "middle

  method line.
    write at m_end_col '|'.

    data l_hight type i.
    l_hight = m_beg_col - m_end_col.
    skip to line l_hight.

*Рисуем промежуточную границу
    data y type i.
    uline at m_beg_col(m_length).

    y = sy-linno + 1.
    skip to line y.

    write at m_beg_col '|'.
  endmethod.                    "middle

  method end.
    data l_lin_end type i.
    l_lin_end = sy-linno - m_beg_row + 1.

    write at m_end_col '|'.

*Рисуем верхнюю границу
    skip to line m_beg_row.
    uline at m_beg_col(m_length).

*Рисуем нижнюю границу
    data y type i.
    y = m_beg_row + l_lin_end.
    skip to line y.
    uline at m_beg_col(m_length).

*Переводим каретку к новой строке
    skip to line y.
  endmethod.                    "end
endclass.                    "cl_frame IMPLEMENTATION
*&=====================================================================*
*& Class View
*&---------------------------------------------------------------------*
class cl_view_form definition final.
  public section.
    events clear_form.

    events save_item
      exporting
        value(is_data) type ts_data.

    events remove_item.

    methods constructor
      importing
        ir_model type ref to cl_model
        i_beg_row type i default 1
        i_beg_col type i default 1.

    methods draw for event data_changed of cl_model.

    methods line_sel_event
      importing
        value(i_row) type t_index
        value(i_col) type t_index.

  private section.
    methods read_data
      returning
        value(rs_data) type ts_data.

    data m_beg_row type i.  "Относительная начальная строка контрола
    data m_beg_col type i.  "Начальный столбец контрола

    data m_root_row type i. "Абсолютная начальная строка контрола

    data mr_model type ref to cl_model.

endclass.                    "c2 DEFINITION
*----------------------------------------------------------------------*
*       CLASS cl_view IMPLEMENTATION
*----------------------------------------------------------------------*
class cl_view_form implementation.
  method constructor.
    mr_model = ir_model.
    m_beg_col = i_beg_col.
    m_beg_row = i_beg_row.

    draw( ).
    set handler draw for ir_model.
  endmethod.                    "cl_view

  method draw.
    m_root_row = sy-linno + m_beg_row. "Отступ от верхнего элемента

    data l_index type t_index.
    l_index = mr_model->get_index( ).

    data lt_data type tt_data.
    lt_data = mr_model->get_all( ).

    data ls_data like line of lt_data.
    if l_index is not initial.
      read table lt_data into ls_data index l_index.
    endif.

*Рисуем форму
    set blank lines on.
    cl_frame_stat=>start(
      i_beg_col = m_beg_col
      i_beg_row = m_root_row
      i_width = 35 ).

    write: (10) 'Key',
           (20) ls_data-key color col_heading.

    cl_frame_stat=>middle( ).
    write: (10) 'Text',
           (20) ls_data-text input.

    data l_shift type i.
    l_shift = m_beg_col + 29.

*Рисуем кнопки
    cl_frame_stat=>middle( ).
    cl_frame_stat=>middle( ).
    format hotspot on.
    write:      icon_create as icon,
     at l_shift icon_delete as icon,
                icon_system_save as icon.
    format hotspot off.
    cl_frame_stat=>end( ).
    set blank lines off.
  endmethod.                    "if_observer_item~execute

  method line_sel_event.
    sy-lsind = 0.

    data l_ind type t_index.
    l_ind = m_root_row + 4.

    data l_col type i.
    l_col = i_col - m_beg_col + 1.

    if i_row = l_ind.
      case l_col.
        when 4.
          raise event clear_form.

        when 31.
          raise event remove_item.

        when 34.
          data ls_data type ts_data.
          ls_data = read_data( ).
          if ls_data is not initial.
            raise event save_item exporting is_data = ls_data.
          endif.

      endcase.
    endif.
  endmethod.                    "line_selection_event

  method read_data.
    data l_line type string.
    data l_val type string.

    data l_index type t_index.
    l_index = m_root_row + 2.

    data l_shift type i.
    l_shift = m_beg_col + 12.

    read line l_index line value into l_line.
    if sy-subrc = 0
    and l_line is not initial.
      l_val = l_line+l_shift(20).
      condense l_val.
      rs_data-text = l_val.
    endif.
  endmethod.                    "read_data
endclass.                    "c2 IMPLEMENTATION
*&=====================================================================*
*& Class View
*&---------------------------------------------------------------------*
class cl_view_table definition final.
  public section.

    events line_selection
      exporting
        value(i_index) type t_index.

    methods constructor
      importing
        ir_model type ref to cl_model
        i_beg_row type i default 1 "Если не заполнено с текущей строки
        i_beg_col type i default 1.

    methods draw for event data_changed of cl_model.

    methods line_sel_event
      importing
        value(i_row) type t_index.

    data m_beg_row type i.  "Относительная начальная строка контрола
    data m_beg_col type i.  "Начальный столбец контрола

    data m_root_row type i. "Абсолютная начальная строка контрола

    data mr_model type ref to cl_model.

endclass.                    "c2 DEFINITION
*----------------------------------------------------------------------*
*       CLASS cl_view IMPLEMENTATION
*----------------------------------------------------------------------*
class cl_view_table implementation.
  method constructor.
    mr_model = ir_model.
    m_beg_col = i_beg_col.
    m_beg_row = i_beg_row.

    draw( ).
    set handler draw for ir_model.
  endmethod.                    "cl_view

  method draw.
    m_root_row = sy-linno + m_beg_row. "Отступ от верхнего элемента

    data l_index type t_index.
    l_index = mr_model->get_index( ).

    data lt_data type tt_data.
    lt_data = mr_model->get_all( ).

    cl_frame_stat=>start(
      i_beg_col = m_beg_col
      i_beg_row = m_root_row
      i_width = 30 ).

    format color col_heading.
    write: (5)  'Key'  centered,
      '|', (20) 'Text' centered.
    format color off.
    cl_frame_stat=>line( ).

    if lt_data is not initial.
      data ls_data like line of lt_data.
      loop at lt_data into ls_data.
        if sy-tabix > 1.
          cl_frame_stat=>middle( ).
        endif.

        if sy-tabix = l_index.
          format color col_total.
        endif.

        format hotspot on.
        write: (5) ls_data-key, '|', (20) ls_data-text.
        format hotspot off.

        if sy-tabix = l_index.
          format color off.
        endif.
      endloop.
    else.
      data l_shift type i.
      l_shift = m_beg_col + 8.
      write at l_shift '|'.
    endif.
    cl_frame_stat=>end( ).
  endmethod.                    "if_observer_item~execute

  method line_sel_event.
    sy-lsind = 0.

    data lt_data type tt_data.
    lt_data = mr_model->get_all( ).

    data l_min type t_index.
    l_min = m_root_row + 2.

    data l_max type i.
    l_max = lines( lt_data ) + l_min + 1.

    data l_index type t_index.

    if i_row > l_min
    and i_row <= l_max.

      l_index = i_row - l_min.
      raise event line_selection exporting i_index = l_index.
    endif.
  endmethod.                    "line_selection_event
endclass.                    "c2 IMPLEMENTATION
*&=====================================================================*
*& Class Controller
*&---------------------------------------------------------------------*
class cl_contr definition final.

  public section.
    methods constructor
      importing
        ir_model type ref to cl_model
        ir_view_form type ref to cl_view_form
        ir_view_table type ref to cl_view_table.

    methods clear_form
      for event clear_form of cl_view_form.

    methods save_item
      for event save_item of cl_view_form
      importing is_data.

    methods remove_item
      for event remove_item of cl_view_form.

    methods line_sel
      for event line_selection of cl_view_table
      importing i_index.

  private section.
    data mr_model type ref to cl_model.

    constants mc_err_mess type sy-msgv1 value 'No line selected'.
endclass.                    "c2 DEFINITION
*----------------------------------------------------------------------*
*       CLASS cl_view IMPLEMENTATION
*----------------------------------------------------------------------*
class cl_contr implementation.
  method constructor.
    set handler clear_form for ir_view_form.
    set handler save_item for ir_view_form.
    set handler remove_item for ir_view_form.
    set handler line_sel for ir_view_table.

    mr_model = ir_model.
  endmethod.                    "cl_view

  method line_sel.
    try.
        mr_model->set_index( i_index ).
      catch cx_sy_duplicate_key.
        message e001(00) with mc_err_mess.
    endtry.
  endmethod.                    "line_selection

  method clear_form.
    mr_model->set_index( 0 ).
  endmethod.                    "add_item

  method save_item.
    mr_model->modified( is_data ).
  endmethod.                    "add_item

  method remove_item.
    try.
        mr_model->remove( ).
      catch cx_sy_duplicate_key.
        message e001(00) with mc_err_mess.
    endtry.
  endmethod.                    "remove_item
endclass.                    "c2 IMPLEMENTATION
*&=====================================================================*
start-of-selection.
  data gr_model type ref to cl_model.
  create object gr_model.

  data gr_view_form type ref to cl_view_form.
  create object gr_view_form
    exporting
      ir_model  = gr_model
      i_beg_row = 5
      i_beg_col = 10.

  data gr_view_table type ref to cl_view_table.
  create object gr_view_table
    exporting
      ir_model  = gr_model
      i_beg_row = 1
      i_beg_col = 20.

  data gr_contr type ref to cl_contr.
  create object gr_contr
    exporting
      ir_model      = gr_model
      ir_view_form  = gr_view_form
      ir_view_table = gr_view_table.

at line-selection.
  gr_view_form->line_sel_event( i_row = sy-lilli i_col = sy-cucol ).
  gr_view_table->line_sel_event( sy-lilli ).
