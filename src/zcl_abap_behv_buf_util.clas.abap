class zcl_abap_behv_buf_util definition
  public
  final
  create public .

  public section.
    class-methods:
      check_instance_equal
        importing
                  instance_1    type any
                  instance_2    type any
                  entity_fields type ddfields
        returning value(equal)  type abap_bool,

      check_instance_exists
        importing
                  instance_to_check type any
                  entity_instances  type any table
                  entity_fields     type ddfields
        returning value(exists)     type abap_bool,

      get_entity_fields
        importing
                  entity_name          type abp_entity_name
        returning value(entity_fields) type ddfields,

      deep_clone
        importing
          data_to_be_cloned        type any
        exporting
          reference_to_cloned_data type ref to data,

      format_res_as_per_cntrl_flds
        importing
          control_fields  type any
          entity_fields   type ddfields
        changing
          entity_instance type any,

      get_entity_alias
        importing
          entity    type cl_abap_behv_load=>t_entity
        returning
          value(alias)  type string.

  protected section.
  private section.
endclass.



class zcl_abap_behv_buf_util implementation.


  method check_instance_equal.
    data entity_field type dfies.
    field-symbols <instance_1_field> type any.
    field-symbols <instance_2_field> type any.

    loop at entity_fields into entity_field where keyflag = abap_true.
      assign component entity_field-fieldname of structure instance_1 to <instance_1_field>.
      assign component entity_field-fieldname of structure instance_2 to <instance_2_field>.
      if <instance_1_field> <> <instance_2_field>.
        " entity does not match
        equal = abap_false.
        return.
      endif.
    endloop.

    equal = abap_true.
    return.
  endmethod.


  method check_instance_exists.
    field-symbols <entity_instance> type any.

    loop at entity_instances assigning <entity_instance>.
      data(equal) = check_instance_equal(
                          instance_1    = instance_to_check
                          instance_2    = <entity_instance>
                          entity_fields = entity_fields ).

      if equal = abap_true.
        exists = abap_true.
        return.
      endif.
    endloop.

    exists = abap_false.
  endmethod.


  method get_entity_fields.
    data struct_descr type ref to cl_abap_structdescr.

    struct_descr ?= cl_abap_typedescr=>describe_by_name( entity_name ).
    entity_fields = struct_descr->get_ddic_field_list( ).
  endmethod.


  method deep_clone.
    field-symbols <fs_clone> type any.
    create data reference_to_cloned_data like data_to_be_cloned.
    assign reference_to_cloned_data->* to <fs_clone>.
    <fs_clone> = data_to_be_cloned.
  endmethod.


  method format_res_as_per_cntrl_flds.
    data entity_field type dfies.

    field-symbols <fs_control_field_value> type any.
    field-symbols <fs_data_to_be_removed> type any.

    loop at entity_fields into entity_field where fieldname <> '.NODE1' and keyflag = abap_false.
      assign component entity_field-fieldname of structure control_fields to <fs_control_field_value>.
      if <fs_control_field_value> = if_abap_behv=>mk-off.
        " clear data
        assign component entity_field-fieldname of structure entity_instance to <fs_data_to_be_removed>.
        clear <fs_data_to_be_removed>.
      endif.
    endloop.
  endmethod.

  method get_entity_alias.
    if entity-alias is initial.
      alias = to_upper( entity-name ).
    else.
      alias = to_upper( entity-alias ).
    endif.
  endmethod.

endclass.
