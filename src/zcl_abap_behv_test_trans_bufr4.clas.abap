class zcl_abap_behv_test_trans_bufr4 definition public final create private.

  public section.
    interfaces zif_abap_behv_test_trans_bufr.

    class-methods:
      "! <p class="shorttext synchronized" lang="en">Get the transactional buffer instance</p>
      get_instance returning value(double_transactional_buffer) type ref to zif_abap_behv_test_trans_bufr.
  protected section.
  private section.

    types: begin of exception_structure,
             operation type if_abap_behv=>t_char01,
             reported  type ref to data,
             failed    type ref to data,
           end of exception_structure.

    types: begin of ty_double_transactional_buffer,
             entity_root      type abp_root_entity_name,
             entity_name      type abp_entity_name,
             entity_alias     type abp_entity_name,
             entity_instances type ref to data,
             reported         type ref to data,
           end of ty_double_transactional_buffer.

    class-data:
     "! <p class="shorttext synchronized" lang="en">Transactional buffer double singleton instance</p>
     double_transactional_buffer type ref to zif_abap_behv_test_trans_bufr.

    data:
     "! <p class="shorttext synchronized" lang="en">Transactional buffer double</p>
      dbl_transactional_buffer_table type standard table of ty_double_transactional_buffer with key entity_name.    " TODO : should we have a sorted table on entity name for binary search, or provide hashing to improve the search
    constants: c_message_class type symsgid value 'ZBO_MOCKING_MESSAGE'.
    methods:
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

      fill_read_reported_and_failed
        importing
          i_entity_fields       type ddfields
          alias_name            type abp_entity_name
          message_number        type symsgno
          retrieval             type abp_behv_retrievals
        changing
          c_failed              type data
          c_reported            type data
          cs_retrieval_instance type any,

      fill_modify_reported_n_failed
        importing
          i_entity_fields    type ddfields
          alias_name         type abp_entity_name
          message_number     type symsgno
          change             type abp_behv_changes
        changing
          c_failed           type data
          c_reported         type data
          cs_change_instance type any,

      fill_modify_mapped
        importing
          i_entity_fields    type ddfields
          alias_name         type abp_entity_name
          message_number     type symsgno
          change             type abp_behv_changes
        changing
          c_mapped           type data
          cs_change_instance type any,
      find_reported_if_configured
        importing
          entities          type cl_abap_behv_load=>tt_entity
          instance_to_check type any
        changing
          buffer_4_entity   type ty_double_transactional_buffer
          c_reported        type data.



endclass.



class zcl_abap_behv_test_trans_bufr4 implementation.


  method get_instance.
    if zcl_abap_behv_test_trans_bufr4=>double_transactional_buffer is initial.
      zcl_abap_behv_test_trans_bufr4=>double_transactional_buffer = new zcl_abap_behv_test_trans_bufr4( ).
    endif.
    double_transactional_buffer = zcl_abap_behv_test_trans_bufr4=>double_transactional_buffer.
  endmethod.


  method zif_abap_behv_test_trans_bufr~read.
    data entity_fields type ddfields.
    data buffer_entry_for_entity type ty_double_transactional_buffer.
    data entity_buffer_instance_copy type ref to data.

    field-symbols <fs_retrieval> type abp_behv_retrievals.
    field-symbols <fs_retrieval_instances> type standard table.
    field-symbols <fs_retrieval_instance> type any.
    field-symbols <fs_control_fields> type any.

    field-symbols <entity_buffer_instances> type standard table.
    field-symbols <entity_buffer_instance> type any.
    field-symbols <entity_buffer_instance_copy> type any.
    field-symbols <fs_result> type standard table.

    cl_abap_behv_load=>get_load(
                     exporting
                       entity   = root_name
                       all      = abap_true
                     importing
                       head         = data(head)
                       entities     = data(entities)
                       features     = data(features)
                       actions      = data(actions)
                       relations    = data(relations)
                       associations = data(associations) ).

    loop at retrievals assigning <fs_retrieval>.
      clear buffer_entry_for_entity.

      " retrieve the buffer entry for the entity
      if line_exists( dbl_transactional_buffer_table[ entity_name = <fs_retrieval>-entity_name ] ).
        buffer_entry_for_entity = dbl_transactional_buffer_table[ entity_name = <fs_retrieval>-entity_name ].
        loop at entities into data(entity) where name = <fs_retrieval>-entity_name.
          buffer_entry_for_entity-entity_alias = entity-alias.
          exit.
        endloop.

      endif.

      " retrieve fields of the entity
      entity_fields = get_entity_fields( <fs_retrieval>-entity_name ).
      " get the reference to the entity instances requested
      assign <fs_retrieval>-instances->* to <fs_retrieval_instances>.
      " get the reference to the output result
      assign <fs_retrieval>-results->* to <fs_result>.

      " get the reference to the entity instances in buffer
      assign buffer_entry_for_entity-entity_instances->* to <entity_buffer_instances>.

      " TODO: Fill failed structure when the entry does not exist in the transactional buffer
      if buffer_entry_for_entity is initial.
        "TODO: Throw exception that entity doesn't exist.
        data(entity_details) = <fs_retrieval>-entity_name.
        loop at entities into entity where name = <fs_retrieval>-entity_name.
          buffer_entry_for_entity-entity_alias = entity-alias.
          exit.
        endloop.
        data(alias) = buffer_entry_for_entity-entity_alias.
        alias = to_upper( alias ).
        loop at <fs_retrieval_instances> assigning <fs_retrieval_instance>.
          fill_read_reported_and_failed(
           exporting
            i_entity_fields = entity_fields
            alias_name = alias
            message_number = 001
            retrieval = <fs_retrieval>
          changing
            c_failed = failed
            c_reported = reported
            cs_retrieval_instance = <fs_retrieval_instance> ).
        endloop.
        continue.
      endif.

      case <fs_retrieval>-op.
        when if_abap_behv=>op-r-read.

          " TODO: Probably we can get rid of key fields calculation from the structure. Because the retrieval instance entry already
          " has all the key fields. So probably loop through the components and filter out the key fields.

*---- read the buffer entries corresponding to the entries in the retrieval table

          loop at <fs_retrieval_instances> assigning <fs_retrieval_instance>.

            " check whether the retrieval entry is present in the buffer
            loop at <entity_buffer_instances> assigning <entity_buffer_instance>.

              data(equal) = check_instance_equal(
                                exporting
                                  instance_1    = <entity_buffer_instance>
                                  instance_2    = <fs_retrieval_instance>
                                  entity_fields = entity_fields
                            ).

              if equal = abap_true.
                " create a copy of buffer instance entry
                deep_clone( exporting data_to_be_cloned = <entity_buffer_instance> importing reference_to_cloned_data = entity_buffer_instance_copy ).
                assign entity_buffer_instance_copy->* to <entity_buffer_instance_copy>.

                " format data as per the control structures provided
                assign component '%control' of structure <fs_retrieval_instance> to <fs_control_fields>.
                format_res_as_per_cntrl_flds(
                  exporting
                    control_fields  = <fs_control_fields>
                    entity_fields   = entity_fields
                  changing
                    entity_instance = <entity_buffer_instance_copy>
                ).

                append <entity_buffer_instance_copy> to <fs_result>.
                exit.
              endif.

            endloop.

            "Instance doesn't exist in buffer
            if equal eq abap_false.

              entity_details = <fs_retrieval>-entity_name.
              alias = buffer_entry_for_entity-entity_alias.
              alias = to_upper( alias ).

              fill_read_reported_and_failed(
               exporting
                i_entity_fields = entity_fields
                alias_name = alias
                message_number = 001
                retrieval = <fs_retrieval>
              changing
                c_failed = failed
                c_reported = reported
                cs_retrieval_instance = <fs_retrieval_instance> ).
            endif.
            assign buffer_entry_for_entity to field-symbol(<buffer_4_entity>).
            find_reported_if_configured(  exporting
                    entities = entities
                    instance_to_check = <fs_retrieval_instance>
                   changing
                     buffer_4_entity = <buffer_4_entity>
                     c_reported = reported
                  ).

          endloop.





        when if_abap_behv=>op-r-read_ba.

          data(association) = associations[ name =  <fs_retrieval>-sub_name ].
          data(associated_entity) = association-target_entity.
          data buffer_entry_for_assoc_entity type ty_double_transactional_buffer.
          " retrieve fields of the associated entity
          data(assoc_entity_fields) = get_entity_fields( associated_entity ).

          " TODO: Probably we can get rid of key fields calculation from the structure. Because the retrieval instance entry already
          " has all the key fields. So probably loop through the components and filter out the key fields.

*---- read the buffer entries corresponding to the entries in the retrieval table

          loop at <fs_retrieval_instances> assigning <fs_retrieval_instance>.

            " check whether the retrieval entry is present in the buffer
            loop at <entity_buffer_instances> assigning <entity_buffer_instance>.

              data(parent_equal) = check_instance_equal(
                                exporting
                                  instance_1    = <entity_buffer_instance>
                                  instance_2    = <fs_retrieval_instance>
                                  entity_fields = entity_fields
                            ).

              if parent_equal = abap_true.

                " retrieve the buffer entry for the associated entity
                if line_exists( dbl_transactional_buffer_table[ entity_name = associated_entity ] ).
                  buffer_entry_for_assoc_entity = dbl_transactional_buffer_table[ entity_name = associated_entity ].
                  loop at entities into entity where name = associated_entity.
                    buffer_entry_for_assoc_entity-entity_alias = entity-alias.
                    exit.
                  endloop.
                endif.


                field-symbols <assoc_entity_buff_instances> type standard table.
                assign buffer_entry_for_assoc_entity-entity_instances->* to <assoc_entity_buff_instances>.

                " check whether the retrieval entry is present in the buffer
                if <assoc_entity_buff_instances> is assigned.
                  loop at <assoc_entity_buff_instances> assigning field-symbol(<assoc_entity_buffer_instance>).

                    equal = check_instance_equal(
                                exporting
                                  instance_1    = <assoc_entity_buffer_instance>
                                  instance_2    = <fs_retrieval_instance>
                                  entity_fields =  entity_fields    " here we are passing the entity fields of parent entity and not associated entity because
                                                                    "  the read happens only based on the key value of parent and not associated entity
                            ).

                    if equal = abap_true.

                      " create a copy of buffer instance entry
                      deep_clone( exporting data_to_be_cloned = <assoc_entity_buffer_instance> importing reference_to_cloned_data = entity_buffer_instance_copy ).
                      assign entity_buffer_instance_copy->* to <entity_buffer_instance_copy>.

                      " format data as per the control structures provided
                      assign component '%control' of structure <fs_retrieval_instance> to <fs_control_fields>.
                      format_res_as_per_cntrl_flds(
                        exporting
                          control_fields  = <fs_control_fields>
                          entity_fields   = assoc_entity_fields
                        changing
                          entity_instance = <entity_buffer_instance_copy>
                      ).

                      append <entity_buffer_instance_copy> to <fs_result>.
                      exit.
                    endif.

                  endloop.

                endif.

                continue.

              endif.

            endloop.

            " If the parent entity itself does not exist
            if parent_equal eq abap_false.
              " fill failed and reported

              alias = buffer_entry_for_entity-entity_alias.
              alias = to_upper( alias ).

              fill_read_reported_and_failed(
               exporting
                i_entity_fields = entity_fields
                alias_name = alias
                message_number = 001
                retrieval = <fs_retrieval>
              changing
                c_failed = failed
                c_reported = reported
                cs_retrieval_instance = <fs_retrieval_instance> ).
            endif.

          endloop.

      endcase.

    endloop.
  endmethod.


  method zif_abap_behv_test_trans_bufr~modify.

    " steps

    " loop through each entity in the changes

    " check whether a buffer entry is present in the transactional buffer or not for the entity

    " if buffer entry is not present, first create a new buffer entry for the entity

    " If operation is CREATE

    "   loop through instances of entity to be created from the change

    "   for each instance to create, check whether the buffer entry for the entity already has an entry with same keys

    "   if entry is already present fill the failed and reported structure

    "   if instance is not present add the instance to the buffer entry and fill the mapped structure

    " if operation is UPDATE

    "   check whether the buffer contains an entry with the same primary keys, if present update the instance and fill mapped

    "   if not available, fill failed and reported

    " if operation is DELETE

    "   check whether the buffer contains an entry with the same primary keys, if present delete the instance

    "   if not available, fill failed and reported

    data buffer_4_entity type ty_double_transactional_buffer.
    data struct_descr_4_entity type ref to cl_abap_structdescr.
    data entity_fields type ddfields.
    data entity_buffer_instance type ref to data.

    field-symbols <fs_change> type abp_behv_changes.
    field-symbols <instances_4_entity_in_buffer> type standard table.
    field-symbols <entity_buffer_instance> type any.
    field-symbols <input_entity_instances> type standard table.
    field-symbols <input_entity_instance> type any.
    cl_abap_behv_load=>get_load(
                         exporting
                           entity   = root_name
                           all      = abap_true
                         importing
                           head         = data(head)
                           entities     = data(entities)
                           features     = data(features)
                           actions      = data(actions)
                           relations    = data(relations)
                           associations = data(associations) ).

    loop at changes assigning <fs_change>.
      clear buffer_4_entity.

      if line_exists( dbl_transactional_buffer_table[ entity_name = <fs_change>-entity_name ] ).
        buffer_4_entity = dbl_transactional_buffer_table[ entity_name = <fs_change>-entity_name ].
      endif.

      struct_descr_4_entity ?= cl_abap_typedescr=>describe_by_name( <fs_change>-entity_name ).
      entity_fields = struct_descr_4_entity->get_ddic_field_list( ).

      if buffer_4_entity is initial.
        " no buffer entry present for the entity
        buffer_4_entity-entity_name = <fs_change>-entity_name.
        buffer_4_entity-entity_instances = cl_abap_behvdescr=>create_data(
                                                        p_name     = <fs_change>-entity_name
                                                        p_op       = if_abap_behv=>op-r-read "TODO : Should it be modify/read?
                                                        p_kind     = if_abap_behv=>typekind-result
                                                   ).
        append buffer_4_entity to dbl_transactional_buffer_table.
      endif.

      assign buffer_4_entity-entity_instances->* to <instances_4_entity_in_buffer>.
      assign <fs_change>-instances->* to <input_entity_instances>.

      case <fs_change>-op.
        when if_abap_behv=>op-m-create.

          loop at <input_entity_instances> assigning <input_entity_instance>.

            "check whether the entity instance is already present in the buffer
            data(instance_exists_in_buffer) = check_instance_exists(
                              exporting
                                instance_to_check = <input_entity_instance>
                                entity_instances  = <instances_4_entity_in_buffer>
                                entity_fields     = entity_fields
                           ).



            data(entity_details) = entities[ name = <fs_change>-entity_name ].
            data(alias)  = entity_details-alias.
            alias = to_upper( alias ).
            if alias is not initial.
              buffer_4_entity-entity_alias = alias.
            else.
              buffer_4_entity-entity_alias = <fs_change>-entity_name.
            endif. "Won't be filled in all the use cases.

            if instance_exists_in_buffer = abap_true.
              " fill failed and reported
              fill_modify_reported_n_failed(
               exporting
                i_entity_fields = entity_fields
                alias_name = alias
                message_number = 001
                change = <fs_change>
              changing
                c_failed = failed
                c_reported = reported
                cs_change_instance = <input_entity_instance> ).

              "Fill configured reported if any for the instance.
              data: found_reported_inst type ref to data.
              field-symbols: <buffer_4_entity> type any.
              assign buffer_4_entity to <buffer_4_entity>.
              find_reported_if_configured(  exporting
                                            entities = entities
                                            instance_to_check = <input_entity_instance>
                                           changing
                                             buffer_4_entity = <buffer_4_entity>
                                             c_reported = reported
                                          ).

            else.

              " Create instance
              create data entity_buffer_instance like line of <instances_4_entity_in_buffer>.
              assign entity_buffer_instance->* to <entity_buffer_instance>.
              <entity_buffer_instance> = corresponding #( <input_entity_instance> ).
              append <entity_buffer_instance> to <instances_4_entity_in_buffer>.

              " Fill mapped table
              fill_modify_mapped(
                exporting
                 alias_name = alias
                 i_entity_fields = entity_fields
                 message_number = '001'
                 change = <fs_change>
                changing
                  c_mapped = mapped
                  cs_change_instance = <input_entity_instance> ).

            endif.
          endloop.

        when if_abap_behv=>op-m-update.
          loop at <input_entity_instances> assigning <input_entity_instance>.

            "check whether the entity instance is already present in the buffer
            data(instance_exists) = check_instance_exists(
                              exporting
                                instance_to_check = <input_entity_instance>
                                entity_instances  = <instances_4_entity_in_buffer>
                                entity_fields     = entity_fields
                           ).

            data(entity_detail) = entities[ name = <fs_change>-entity_name ].
            data(alias_name)  = entity_detail-alias.
            alias_name = to_upper( alias_name ).

            if alias_name is not initial.
              buffer_4_entity-entity_alias = alias_name.
            else.
              buffer_4_entity-entity_alias = <fs_change>-entity_name.
            endif. "Won't be filled in all the use cases.


            if instance_exists = abap_false.
              " fill failed and reported
              fill_modify_reported_n_failed(
               exporting
                i_entity_fields = entity_fields
                alias_name = alias_name
                message_number = 001
                change = <fs_change>
              changing
                c_failed = failed
                c_reported = reported
                cs_change_instance = <input_entity_instance> ).

            else.

              " update the instance
              create data entity_buffer_instance like line of <instances_4_entity_in_buffer>.
              assign entity_buffer_instance->* to <entity_buffer_instance>.
              <entity_buffer_instance> = corresponding #( <input_entity_instance> ).
              modify table <instances_4_entity_in_buffer> from <entity_buffer_instance> .

              " fill mapped
              fill_modify_mapped(
                exporting
                 alias_name = alias_name
                 i_entity_fields = entity_fields
                 message_number = '001'
                 change = <fs_change>
                changing
                  c_mapped = mapped
                  cs_change_instance = <input_entity_instance> ).

            endif.
          endloop.

        when if_abap_behv=>op-m-delete.
          loop at <input_entity_instances> assigning <input_entity_instance>.

            "check whether the entity instance is already present in the buffer
            instance_exists_in_buffer = check_instance_exists(
                                  exporting
                                    instance_to_check = <input_entity_instance>
                                    entity_instances  = <instances_4_entity_in_buffer>
                                    entity_fields     = entity_fields
                               ).

            entity_details = entities[ name = <fs_change>-entity_name ].
            alias  = entity_details-alias.
            alias = to_upper( alias ).

            if alias is not initial.
              buffer_4_entity-entity_alias = alias.
            else.
              buffer_4_entity-entity_alias = <fs_change>-entity_name.
            endif. "Won't be filled in all the use cases.

            if instance_exists_in_buffer = abap_false.
              " fill failed and reported
              fill_modify_reported_n_failed(
               exporting
                i_entity_fields = entity_fields
                alias_name = alias
                message_number = 001
                change = <fs_change>
              changing
                c_failed = failed
                c_reported = reported
                cs_change_instance = <input_entity_instance> ).

            else.
              "TODO: Cascading delete
              "delete if there are associated entities - i.e. cascading delete.
              data buffer_4_assoc_entity type ty_double_transactional_buffer.
              data(association) = associations[ source_entity =  <fs_change>-entity_name ].
              data(associated_entity) = association-target_entity.
              buffer_4_assoc_entity = dbl_transactional_buffer_table[ entity_name = associated_entity  ].

              if buffer_4_assoc_entity is not initial.

                loop at relations[ source_entity = associated_entity ]-foreignkeys into data(key).

                  "Get instance based on the key and value.
                  "Remove the instance.

                endloop.

              endif.


              "delete the instance
              create data entity_buffer_instance like line of <instances_4_entity_in_buffer>.
              assign entity_buffer_instance->* to <entity_buffer_instance>.
              <entity_buffer_instance> = corresponding #( <input_entity_instance> ).
              delete table <instances_4_entity_in_buffer> from <entity_buffer_instance> .

            endif.
          endloop.

        when if_abap_behv=>op-m-create_ba.


          data struct_descr_assoc_entity type ref to cl_abap_structdescr.
          data entity_fields_of_assoc_entity type ddfields.

          association = associations[ source_entity =  <fs_change>-entity_name ].
          associated_entity = association-target_entity.

          struct_descr_assoc_entity  ?= cl_abap_typedescr=>describe_by_name( associated_entity ).
          entity_fields_of_assoc_entity = struct_descr_assoc_entity->get_ddic_field_list( ).

          " Check if buffer entry exists for the associated entity
          if line_exists( dbl_transactional_buffer_table[ entity_name = associated_entity ] ).
            buffer_4_assoc_entity = dbl_transactional_buffer_table[ entity_name = associated_entity  ].
          endif.

          if  buffer_4_assoc_entity  is initial.
            " no buffer entry present for the associated entity
            buffer_4_assoc_entity-entity_name = associated_entity.
            buffer_4_assoc_entity-entity_instances = cl_abap_behvdescr=>create_data(
                                                            p_name     = associated_entity
                                                            p_op       = if_abap_behv=>op-r-read
                                                            p_kind     = if_abap_behv=>typekind-result

                                                       ).
            append buffer_4_assoc_entity to dbl_transactional_buffer_table.
          endif.

          field-symbols <assoc_entity_buff_instances> type standard table.
          field-symbols <fs_assoc_entity_instances> type standard table.
          assign buffer_4_assoc_entity-entity_instances->* to <assoc_entity_buff_instances>.


          loop at <input_entity_instances> assigning <input_entity_instance>.

            " Check if the parent entity instance exists.
            data(parent_exists)    = check_instance_exists(
                                 exporting
                                   instance_to_check = <input_entity_instance>
                                   entity_instances  = <instances_4_entity_in_buffer>
                                   entity_fields     = entity_fields
                              ).

            assign component cl_abap_behv=>co_techfield_name-target of structure <input_entity_instance> to <fs_assoc_entity_instances>.

            loop at <fs_assoc_entity_instances> assigning field-symbol(<fs_assoc_entity_inst>).


              loop at relations[ source_entity = associated_entity ]-foreignkeys into key.

                assign component key-local_name of structure <input_entity_instance> to field-symbol(<fs_val_1>).
                assign component key-local_name of structure <fs_assoc_entity_inst> to field-symbol(<fs_val_2>).
                <fs_val_2> = <fs_val_1>.
              endloop.

              "check whether the associated entity instance is already present in the buffer
              instance_exists_in_buffer = check_instance_exists(
                                exporting
                                  instance_to_check = <fs_assoc_entity_inst>
                                  entity_instances  = <assoc_entity_buff_instances>
                                  entity_fields     = entity_fields_of_assoc_entity
                             ).

              entity_details = entities[ name = associated_entity ].
              alias = entity_details-alias.
              alias = to_upper( alias ).
              if alias is not initial.
                buffer_4_assoc_entity-entity_alias = alias.
              else.
                buffer_4_assoc_entity-entity_alias = associated_entity.
              endif. "Won't be filled in all the use cases.

              if instance_exists_in_buffer = abap_true or parent_exists = abap_false.
                " fill failed and reported
                fill_modify_reported_n_failed(
                 exporting
                  i_entity_fields = entity_fields_of_assoc_entity
                  alias_name = alias
                  message_number = 001
                  change = <fs_change>
                changing
                  c_failed = failed
                  c_reported = reported
                  cs_change_instance = <fs_assoc_entity_inst> ).

              else.
                create data entity_buffer_instance like line of <assoc_entity_buff_instances>.
                assign entity_buffer_instance->* to <entity_buffer_instance>.
                <entity_buffer_instance> = corresponding #( <fs_assoc_entity_inst> ).
                append <entity_buffer_instance> to <assoc_entity_buff_instances>.

                " Fill mapped table
                fill_modify_mapped(
                  exporting
                   alias_name = alias
                   i_entity_fields = entity_fields_of_assoc_entity
                   message_number = '001'
                   change = <fs_change>
                  changing
                    c_mapped = mapped
                    cs_change_instance = <entity_buffer_instance> ).

              endif.


            endloop.

          endloop.

      endcase.


    endloop.

  endmethod.


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


  method fill_read_reported_and_failed.

    field-symbols <fs_entry_failed_table> type standard table.
    field-symbols <fs_entry_reported_table> type standard table.

    assign component alias_name of structure c_failed to <fs_entry_failed_table>.
    data entry_failed type ref to data.
    create data entry_failed like line of <fs_entry_failed_table>.
    assign entry_failed->* to field-symbol(<fs_entry_failed>).
    "Assign key fields
    loop at i_entity_fields into data(entity_field) where keyflag = abap_true.
      assign component entity_field-fieldname of structure cs_retrieval_instance to field-symbol(<fs_val_1>).
      assign component entity_field-fieldname of structure <fs_entry_failed> to field-symbol(<fs_val_2>).
      <fs_val_2> = <fs_val_1>.
    endloop.
    "Assign fail reason
    assign component  cl_abap_behv=>co_techfield_name-fail of structure <fs_entry_failed> to field-symbol(<fail>).
    assign component 'CAUSE' of structure <fail> to field-symbol(<fail_reason>).
    <fail_reason> = if_abap_behv=>cause-not_found.
    "Assign Association if read by association operation.
    if retrieval-op eq if_abap_behv=>op-r-read_ba.
      assign component  cl_abap_behv=>co_techfield_name-assoc of structure <fs_entry_failed> to field-symbol(<assoc>).
      assign component  retrieval-sub_name of structure <assoc> to field-symbol(<assoc_subname>).
      <assoc_subname> = '01'.
    endif.

    append <fs_entry_failed> to <fs_entry_failed_table>.

    "Fill reported
    assign component alias_name of structure c_reported to <fs_entry_reported_table>.
    data entry_reported type ref to data.
    create data entry_reported like line of <fs_entry_reported_table>.
    assign entry_reported->* to field-symbol(<fs_entry_reported>).
    "Assign key fields
    loop at i_entity_fields into entity_field where keyflag = abap_true.
      assign component entity_field-fieldname of structure cs_retrieval_instance to <fs_val_1>.
      assign component entity_field-fieldname of structure <fs_entry_reported> to <fs_val_2>.
      <fs_val_2> = <fs_val_1>.
    endloop.

    "Assign exception message instance
    data(lo_message) = new lcl_exception_handler( )->new_message( id = c_message_class
                                   number   = message_number
                                   severity = if_abap_behv_message=>severity-error ).

    field-symbols:<fs_message_obj> type ref to if_abap_behv_message.
    field-symbols:<fs_message_obj2> type ref to if_abap_behv_message.
    assign component cl_abap_behv=>co_techfield_name-msg of structure <fs_entry_reported> to <fs_message_obj2>.
    assign  lo_message to <fs_message_obj>.
    <fs_message_obj2> = <fs_message_obj>.
    append <fs_entry_reported> to <fs_entry_reported_table>.

  endmethod.


  method fill_modify_reported_n_failed.

    field-symbols <fs_entry_failed_table> type standard table.
    field-symbols <fs_entry_reported_table> type standard table.

    assign component alias_name of structure c_failed to <fs_entry_failed_table>.
    data entry_failed type ref to data.
    create data entry_failed like line of <fs_entry_failed_table>.
    assign entry_failed->* to field-symbol(<fs_entry_failed>).
    "Assign key fields
    loop at i_entity_fields into data(entity_field) where keyflag = abap_true.
      assign component entity_field-fieldname of structure cs_change_instance to field-symbol(<fs_val_1>).
      assign component entity_field-fieldname of structure <fs_entry_failed> to field-symbol(<fs_val_2>).
      <fs_val_2> = <fs_val_1>.
    endloop.
    "Assign fail reason
    assign component  cl_abap_behv=>co_techfield_name-fail of structure <fs_entry_failed> to field-symbol(<fail>).
    assign component 'CAUSE' of structure <fail> to field-symbol(<fail_reason>).

    " Make operation specific additions to failed
    case change-op.
      when if_abap_behv=>op-m-create.
        <fail_reason> = if_abap_behv=>cause-unspecific.
        assign component  cl_abap_behv=>co_techfield_name-create of structure <fs_entry_failed> to field-symbol(<create>).
        <create> = '01'.

      when if_abap_behv=>op-m-create_ba.
        <fail_reason> = if_abap_behv=>cause-unspecific.
        assign component  cl_abap_behv=>co_techfield_name-create of structure <fs_entry_failed> to <create>.
        <create> = '01'.

      when if_abap_behv=>op-m-update.
        <fail_reason> = if_abap_behv=>cause-not_found.
        assign component  cl_abap_behv=>co_techfield_name-update of structure <fs_entry_failed> to field-symbol(<update>).
        <update> = '01'.

      when if_abap_behv=>op-m-delete.
        <fail_reason> = if_abap_behv=>cause-not_found.
        assign component  cl_abap_behv=>co_techfield_name-delete of structure <fs_entry_failed> to field-symbol(<delete>).
        <delete> = '01'.

    endcase.

    append <fs_entry_failed> to <fs_entry_failed_table>.

    "Fill reported
    assign component alias_name of structure c_reported to <fs_entry_reported_table>.
    data entry_reported type ref to data.
    create data entry_reported like line of <fs_entry_reported_table>.
    assign entry_reported->* to field-symbol(<fs_entry_reported>).
    "Assign key fields
    loop at i_entity_fields into entity_field where keyflag = abap_true.
      assign component entity_field-fieldname of structure cs_change_instance to <fs_val_1>.
      assign component entity_field-fieldname of structure <fs_entry_reported> to <fs_val_2>.
      <fs_val_2> = <fs_val_1>.
    endloop.

    "Assign exception message instance
    data(lo_message) = new lcl_exception_handler( )->new_message( id = c_message_class
                                   number   = message_number
                                   severity = if_abap_behv_message=>severity-error ).

    field-symbols:<fs_message_obj> type ref to if_abap_behv_message.
    field-symbols:<fs_message_obj2> type ref to if_abap_behv_message.
    assign component cl_abap_behv=>co_techfield_name-msg of structure <fs_entry_reported> to <fs_message_obj2>.
    assign  lo_message to <fs_message_obj>.
    <fs_message_obj2> = <fs_message_obj>.
    append <fs_entry_reported> to <fs_entry_reported_table>.

  endmethod.


  method fill_modify_mapped.

    field-symbols <fs_entry_mapped_table> type standard table.

    assign component alias_name of structure c_mapped to <fs_entry_mapped_table>.
    data entry_mapped type ref to data.
    create data entry_mapped like line of <fs_entry_mapped_table>.
    assign entry_mapped->* to field-symbol(<fs_entry_mapped>).

    "Assign key fields
    loop at i_entity_fields into data(entity_field) where keyflag = abap_true.
      assign component entity_field-fieldname of structure cs_change_instance to field-symbol(<fs_val_1>).
      assign component entity_field-fieldname of structure <fs_entry_mapped> to field-symbol(<fs_val_2>).
      <fs_val_2> = <fs_val_1>.
    endloop.

    append <fs_entry_mapped> to <fs_entry_mapped_table>.

  endmethod.

  method zif_abap_behv_test_trans_bufr~config_error_response_4_modify.

    "Check if the entity exists in double transactional buffer
    "If it exists, create a reported and failed structure for that entity.
    "data: buffer_4_entity  type ty_double_transactional_buffer.
    field-symbols: <buffer_4_entity>         type ty_double_transactional_buffer,
                   <entity_buffer_instances> type standard table,
                   <entity_buffer_instance>  type any,
                   <entity_reported>         type standard table.
    cl_abap_behv_load=>get_load(
                  exporting
                    entity   = root_name
                    all      = abap_true
                  importing
                    head         = data(head)
                    entities     = data(entities)
                    features     = data(features)
                    actions      = data(actions)
                    relations    = data(relations)
                    associations = data(associations) ).

    "Fill reported.
    if reported is not initial.

      loop at entities into data(entity).
        if entity-alias is initial.
          assign component to_upper( entity-name ) of structure reported to <entity_reported>.
          if <entity_reported> is assigned.
            break-point.
          endif.
        else.
          assign component to_upper( entity-alias ) of structure reported to <entity_reported>.
          if <entity_reported> is assigned and <entity_reported> is not initial.
            "Check if double transactional buffer contains the entity and entity instance.
            loop at <entity_reported> assigning field-symbol(<entity_reported_instance>).
              if line_exists( dbl_transactional_buffer_table[ entity_name = entity-name ] ).
                " <buffer_4_entity> = dbl_transactional_buffer_table[ entity_name = entity-name  ].
                assign dbl_transactional_buffer_table[ entity_name = entity-name  ] to <buffer_4_entity>.
                assign <buffer_4_entity>-entity_instances->* to <entity_buffer_instances>.
                data(entity_fields) = get_entity_fields( entity-name ).
                loop at <entity_buffer_instances> assigning <entity_buffer_instance>.
                  data(equal) = check_instance_equal(
                                       exporting
                                         instance_1    = <entity_buffer_instance>
                                         instance_2    = <entity_reported_instance>
                                         entity_fields = entity_fields
                                   ).

                  if equal = abap_true.
                    if <buffer_4_entity>-reported is initial.
                      create data <buffer_4_entity>-reported like reported.
                    endif.
                    field-symbols: <to_be_filled_buff_reported> type any,
                                   <to_be_filled_buff_entity>   type standard table.
                    assign <buffer_4_entity>-reported->* to <to_be_filled_buff_reported>.
                    assign component to_upper( entity-alias ) of structure <to_be_filled_buff_reported> to <to_be_filled_buff_entity>.
                    append <entity_reported_instance> to <to_be_filled_buff_entity>.
                  endif.
                endloop.
              else.
                "TODO: Throw exception with a reason that  "no buffer exists for the entity"
              endif.
            endloop.
          else.
            "TODO: Throw exception with a reason that  "reported can't be empty"
          endif.
        endif.
      endloop.
    else.
      "TODO: Throw exception with a reason that  "reported can't be empty"
    endif.

  endmethod.


  method find_reported_if_configured.

    field-symbols: <configured_entity_reported> type standard table.

    field-symbols: <existing_buff_reported> type any.
    assign buffer_4_entity-reported->* to <existing_buff_reported>.
    if buffer_4_entity-reported is not initial.
      loop at entities into data(entity).
        data(entity_fields) = get_entity_fields( entity-name ).
        if entity-alias is initial.
          assign component to_upper( entity-name ) of structure buffer_4_entity-reported to <configured_entity_reported>.
          if <configured_entity_reported> is assigned.
            break-point.
          endif.
        else.

          assign component to_upper( entity-alias ) of structure <existing_buff_reported> to <configured_entity_reported>.
          if <configured_entity_reported> is assigned and <configured_entity_reported> is not initial.
            "Check if double transactional buffer contains the entity and entity instance.
            loop at <configured_entity_reported> assigning field-symbol(<existing_reported_ent_inst>).
              data(equal) = check_instance_equal(
                     exporting
                       instance_1    = instance_to_check
                       instance_2    = <existing_reported_ent_inst>
                       entity_fields = entity_fields
                 ).

              if equal = abap_true.
                field-symbols <fs_entry_reported_table> type standard table.
                assign component to_upper( entity-alias ) of structure c_reported to <fs_entry_reported_table>.
                append <existing_reported_ent_inst> to <fs_entry_reported_table>.
              endif.

            endloop.
          endif.

        endif.
      endloop.
    endif.

  endmethod.


  method zif_abap_behv_test_trans_bufr~config_error_response_4_read.

    "Check if the entity exists in double transactional buffer
    "If it exists, create a reported and failed structure for that entity.
    "data: buffer_4_entity  type ty_double_transactional_buffer.
    field-symbols: <buffer_4_entity>         type ty_double_transactional_buffer,
                   <entity_buffer_instances> type standard table,
                   <entity_buffer_instance>  type any,
                   <entity_reported>         type standard table.
    cl_abap_behv_load=>get_load(
                  exporting
                    entity   = root_name
                    all      = abap_true
                  importing
                    head         = data(head)
                    entities     = data(entities)
                    features     = data(features)
                    actions      = data(actions)
                    relations    = data(relations)
                    associations = data(associations) ).

    "Fill reported.
    if reported is not initial.

      loop at entities into data(entity).
        if entity-alias is initial.
          assign component to_upper( entity-name ) of structure reported to <entity_reported>.
          if <entity_reported> is assigned.
            break-point.
          endif.
        else.
          assign component to_upper( entity-alias ) of structure reported to <entity_reported>.
          if <entity_reported> is assigned and <entity_reported> is not initial.
            "Check if double transactional buffer contains the entity and entity instance.
            loop at <entity_reported> assigning field-symbol(<entity_reported_instance>).
              if line_exists( dbl_transactional_buffer_table[ entity_name = entity-name ] ).
                " <buffer_4_entity> = dbl_transactional_buffer_table[ entity_name = entity-name  ].
                assign dbl_transactional_buffer_table[ entity_name = entity-name  ] to <buffer_4_entity>.
                assign <buffer_4_entity>-entity_instances->* to <entity_buffer_instances>.
                data(entity_fields) = get_entity_fields( entity-name ).
                loop at <entity_buffer_instances> assigning <entity_buffer_instance>.
                  data(equal) = check_instance_equal(
                                       exporting
                                         instance_1    = <entity_buffer_instance>
                                         instance_2    = <entity_reported_instance>
                                         entity_fields = entity_fields
                                   ).

                  if equal = abap_true.
                    if <buffer_4_entity>-reported is initial.
                      create data <buffer_4_entity>-reported like reported.
                    endif.
                    field-symbols: <to_be_filled_buff_reported> type any,
                                   <to_be_filled_buff_entity>   type standard table.
                    assign <buffer_4_entity>-reported->* to <to_be_filled_buff_reported>.
                    assign component to_upper( entity-alias ) of structure <to_be_filled_buff_reported> to <to_be_filled_buff_entity>.
                    append <entity_reported_instance> to <to_be_filled_buff_entity>.
                  endif.
                endloop.
              else.
                "TODO: Throw exception with a reason that  "no buffer exists for the entity"
              endif.
            endloop.
          else.
            "TODO: Throw exception with a reason that  "reported can't be empty"
          endif.
        endif.
      endloop.
    else.
      "TODO: Throw exception with a reason that  "reported can't be empty"
    endif.


  endmethod.

endclass.
