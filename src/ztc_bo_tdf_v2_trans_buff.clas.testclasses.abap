*"* use this source file for your ABAP unit test classes
class ltcl_modify_eml definition final for testing
  duration short
  risk level harmless.

  private section.

    methods create_valid_instance for testing raising cx_static_check.
    methods create_invalid_entity_instance for testing raising cx_static_check.
    methods create_inst_incomplete_key for testing raising cx_static_check.

    methods create_valid_inst_by_assoc for testing raising cx_static_check.
    methods create_invalid_inst_by_assoc_1 for testing raising cx_static_check.
    methods create_invalid_inst_by_assoc_2 for testing raising cx_static_check.

    methods update_invalid_instance for testing raising cx_static_check.
    methods update_valid_instance for testing raising cx_static_check.
    methods update_inst_imcomplete_key for testing raising cx_static_check.

    methods delete_inst_incomplete_key for testing raising cx_static_check.
    methods delete_valid_instance for testing raising cx_static_check.
    methods delete_valid_instance_2 for testing raising cx_static_check.
    methods delete_invalid_instance for testing raising cx_static_check.
    methods cascading_delete_ok for testing raising cx_static_check.

    methods complex_modify for testing raising cx_static_check.
    methods modify_with_multiple_ops for testing raising cx_static_check.
    methods mock_multiple_instances for testing raising cx_static_check.

    class-methods class_setup.
    class-data double type ref to zcl_abap_behv_generic_test_dbl.
endclass.


class ltcl_modify_eml implementation.

  method class_setup.
    double = new zcl_abap_behv_generic_test_dbl( '/dmo/i_travel_tum' ).
    cl_abap_behv_test_environment=>set_test_double( double ).

  endmethod.

  method create_invalid_entity_instance.

    "Record
    modify entities of /dmo/i_travel_tum
    entity travel
      create from value #( ( travelid = '987'
                            begindate = '20180101' EndDate = '20180101'
                            totalprice = 100
                            %control-begindate = if_abap_behv=>mk-on
                            %control-totalprice = if_abap_behv=>mk-on )
                         )
    reported data(reported)
    failed data(failed)
    mapped data(mapped).

    "Try t create the same instance
    modify entities of /dmo/i_travel_tum
    entity travel
      create from value #( ( travelid = '987'
                             begindate = '20180101' EndDate = '20180101'
                            totalprice = 100
                            %control-begindate = if_abap_behv=>mk-on
                            %control-totalprice = if_abap_behv=>mk-on )
                         )
    reported data(reported1)
    failed data(failed1)
    mapped data(mapped1).

    cl_abap_unit_assert=>assert_initial( mapped1 ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ).

  endmethod.

  method mock_multiple_instances.
    double = new zcl_abap_behv_generic_test_dbl( '/dmo/i_travel_tmn' ).
    cl_abap_behv_test_environment=>set_test_double( double ).

    data:   cut  type ref to zcl_abap_behv_test_demo .
    cut = new zcl_abap_behv_test_demo( ).

    modify entities of /dmo/i_travel_tmn
     entity travel
       create from value #( ( travel_id = '987'  ) )
     reported data(reported2)
     failed   data(failed2)
     mapped   data(mapped2).

    "Inserting duplicate test data
    modify entities of /dmo/i_travel_tum
      entity travel
        create from value #(
                             ( travelid = '987' begindate = '20180101' EndDate = '20180101' totalprice = 100  %control-begindate = if_abap_behv=>mk-on %control-totalprice = if_abap_behv=>mk-on )
                             ( travelid = '987' currencycode = 'EUR'  %control-currencycode = if_abap_behv=>mk-on )
                           )
      reported data(reported3)
      failed data(failed3)
      mapped data(mapped3).


    "Read
    read entity /dmo/i_travel_tum
    from value #(
               ( travelid = '987' %control-begindate = if_abap_behv=>mk-on %control-totalprice = if_abap_behv=>mk-on )
             )
    result   data(result)
    reported data(reported)
    failed   data(failed).

    read entity /dmo/i_travel_tmn
     from value #(
                   ( travel_id = '987' %control-begin_date = if_abap_behv=>mk-on )
                 )
     result   data(result4)
     reported data(reported4)
     failed   data(failed4).

    cl_abap_unit_assert=>assert_not_initial( result ).
    cl_abap_unit_assert=>assert_not_initial( result4 ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ).

  endmethod.

  method complex_modify.
  endmethod.

  method create_inst_incomplete_key.
    "if key is incomplete 00000 is the value that is assigned by default to a key.
*     modify entities of /dmo/i_travel_tum
*      entity travel
*        create from value #(
*                              ( begindate = '20180101' EndDate = '20180101'
*                              totalprice = 100
*                              %control-begindate = if_abap_behv=>mk-on
*                              %control-totalprice = if_abap_behv=>mk-on )
*                           )
*      reported data(reported3)
*      failed data(failed3)
*      mapped data(mapped3).
*
*   cl_abap_unit_assert=>assert_initial( mapped3 ).  " assertion fails because if key is incomplete 00000 is the value that is
*                                                    " assigned by default to a key.


  endmethod.

  method create_valid_instance.

    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    cl_abap_unit_assert=>assert_not_initial( mapped ).
    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ).


  endmethod.

  method delete_inst_incomplete_key.

  endmethod.

  method delete_invalid_instance.
    modify entities of /dmo/i_travel_tum
       entity travel
         delete from value #( ( %key-TravelID = '987' )
                            )
       reported data(reported)
       failed data(failed)
       mapped data(mapped).

    cl_abap_unit_assert=>assert_initial( mapped ).
    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_not_initial( reported ).

  endmethod.

  method delete_valid_instance.

    " Record test data
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    " cuts
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' )
                          )
     reported data(reported2)
     failed data(failed2)
     mapped data(mapped2).

    cl_abap_unit_assert=>assert_initial( mapped2 ).
    cl_abap_unit_assert=>assert_initial( failed2 ).
    cl_abap_unit_assert=>assert_initial( reported2 ).
  endmethod.

  method delete_valid_instance_2.

   "This method tries to check the use-case :
   "if the root-instance is deleted then associated entities are also deleted."

    " Record test data for root entity
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    " Record test data for associated entity
     ""Create booking by association
    modify entities of /dmo/i_travel_tum
     entity travel
       create by \_Booking
         from value #( ( %key-TravelID = '987'
                         %target = value #( (
                                              BookingID = '333'
                                              BookingDate = '20200902'
                                              FlightPrice = '100'
                                              %control-BookingDate = if_abap_behv=>mk-on
                                              %control-FlightPrice = if_abap_behv=>mk-on
                                               ) )
                         ) )
             mapped data(ls_mapped)
             failed data(ls_failed)
             reported data(ls_reported).

    " cuts
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' )
                          )
     reported data(reported2)
     failed data(failed2)
     mapped data(mapped2).

   "Check that associated entity should is also deleted - i.e. cascading delete
   "Read the booking entity
   read entity /dmo/i_travel_tum
    by \_Booking
     from value #(
                ( travelid = '987'  %control-FlightPrice = if_abap_behv=>mk-on %control-BookingDate = if_abap_behv=>mk-on
                  %control-BookingID = if_abap_behv=>mk-on )
              )
     result   data(result3)
     reported data(reported3)
     failed   data(failed3).


    cl_abap_unit_assert=>assert_initial( mapped2 ).
    cl_abap_unit_assert=>assert_initial( failed2 ).
    cl_abap_unit_assert=>assert_initial( reported2 ).
  endmethod.

  method cascading_delete_ok.
    "Insert test travel entity
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                            ( travelid = '988'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 200
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
       reported data(reported)
       failed data(failed)
       mapped data(mapped).

   "Insert test booking instance
    modify entities of /dmo/i_travel_tum
     entity travel
       create by \_Booking
         from value #( ( %key-TravelID = '988'
                         %target = value #( (
                                              BookingDate = '20200902'
                                              FlightPrice = '100'

                                              %control-BookingDate = if_abap_behv=>mk-on
                                              %control-FlightPrice = if_abap_behv=>mk-on
                                               ) )
                         ) )
             mapped data(ls_mapped)
             failed data(ls_failed)
             reported data(ls_reported).

   "Insert test booking supplement instance
    modify entities of /dmo/i_travel_tum
     entity booking
       create by \_BookSuppl
         from value #( ( %key-TravelID = '988'
                         %key-BookingID = '000'
                         %target = value #( (
                                              Price = '100'
                                              %control-Price = if_abap_behv=>mk-on
                                               ) )
                         ) )
             mapped ls_mapped
             failed ls_failed
             reported ls_reported.


    " Confirm if test data was inserted sucessfully
    READ ENTITIES OF /dmo/i_travel_tum
      ENTITY TRAVEL
        all fields with value #( ( %key-TravelID = '987' ) ( %key-TravelID = '988' ) )
        result data(lt_result_travel)
        by \_Booking
          all fields with value #( ( %key-TravelID ='987' ) ( %key-TravelID ='988' ) )
          result data(lt_result_booking)
      entity booking
        by \_BookSuppl
        all fields with value #( ( %key-travelid = '987'  %key-bookingid = '000' )
                                  ( %key-travelid = '988'  %key-bookingid = '000' ) )
          result data(lt_result_bookSupp)
          failed failed
          reported data(lt_reported).

    " Delete the travel instance "Cascading delete
    MODIFY ENTITIES OF /dmo/i_travel_tum
      ENTITY Travel
        DELETE FROM VALUE #( ( %key-travelid = '988'
                           ) )
        FAILED DATA(failed_delete)
        REPORTED DATA(reported_delete)
        MAPPED DATA(mapped_delete).


    READ ENTITIES OF /dmo/i_travel_tum
      ENTITY TRAVEL
        all fields with value #( ( %key-TravelID = '987' ) ( %key-TravelID = '988' ) )
        result lt_result_travel
        by \_Booking
          all fields with value #( ( %key-TravelID ='987' ) ( %key-TravelID ='988' ) )
          result lt_result_booking
      entity booking
        by \_BookSuppl
        all fields with value #( ( %key-travelid = '987'  %key-bookingid = '000' )
                                 ( %key-travelid = '988'  %key-bookingid = '000' ) )
          result lt_result_bookSupp
          failed failed
          reported lt_reported.
  ENDMETHOD.


  method modify_with_multiple_ops.
    " Create and update in the same modify statement.
    " Create will pass but update will fail as we try to update an instance that does not exist.

    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
       update from value #( ( %key-TravelID = '988'
                             EndDate = '20180102'
                             totalprice = 200
                             %control-enddate = if_abap_behv=>mk-on
                             %control-totalprice = if_abap_behv=>mk-on )
                          )

     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    cl_abap_unit_assert=>assert_not_initial( reported ).
    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_not_initial( mapped ).

    cl_abap_unit_assert=>assert_equals( exp = '987' act = mapped-travel[ 1 ]-TravelID ).
    cl_abap_unit_assert=>assert_equals( exp = '988' act = reported-travel[ 1 ]-TravelID ).
    cl_abap_unit_assert=>assert_equals( exp = '988' act = failed-travel[ 1 ]-TravelID ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ).



  endmethod.

  method update_inst_imcomplete_key.

  endmethod.

  method update_invalid_instance.

    modify entities of /dmo/i_travel_tum
     entity travel
       update from value #( ( %key-TravelID = '987'
                             begindate = '20180101' EndDate = '20180101'
                             totalprice = 100
                             %control-begindate = if_abap_behv=>mk-on
                             %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported3)
     failed data(failed3)
     mapped data(mapped3).

    cl_abap_unit_assert=>assert_initial( mapped3 ).
    cl_abap_unit_assert=>assert_not_initial( failed3 ).
    cl_abap_unit_assert=>assert_not_initial( reported3 ).

  endmethod.

  method update_valid_instance.

    " Record test data
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    " cut
    modify entities of /dmo/i_travel_tum
     entity travel
       update from value #( ( %key-TravelID = '987'
                             EndDate = '20180102'
                             totalprice = 200
                             %control-enddate = if_abap_behv=>mk-on
                             %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported2)
     failed data(failed2)
     mapped data(mapped2).

    cl_abap_unit_assert=>assert_not_initial( mapped2 ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ).

  endmethod.

  method create_valid_inst_by_assoc.

    " Record step to insert a travel instance for which booking is to be created.
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).


    ""Create booking by association
    modify entities of /dmo/i_travel_tum
     entity travel
       create by \_Booking
         from value #( ( %key-TravelID = '987'
                         %target = value #( (
                                              BookingDate = '20200902'
                                              FlightPrice = '100'
                                              %control-BookingDate = if_abap_behv=>mk-on
                                              %control-FlightPrice = if_abap_behv=>mk-on
                                               ) )
                         ) )
             mapped data(ls_mapped)
             failed data(ls_failed)
             reported data(ls_reported).

    cl_abap_unit_assert=>assert_initial( ls_failed ).
    cl_abap_unit_assert=>assert_equals( msg = 'create booking failed' exp = '0000' act = ls_mapped-booking[ 1 ]-BookingID ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ).

    modify entities of /dmo/i_travel_tum
     entity booking
       delete from value #( ( %key-TravelID = '987' %key-BookingID = '000' ) ).


  endmethod.


  method create_invalid_inst_by_assoc_1.

    " Record step to insert a travel instance for which booking is to be created.
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).


    ""Create booking by association- Record
    modify entities of /dmo/i_travel_tum
     entity travel
       create by \_Booking
         from value #( ( %key-TravelID = '987'
                         %target = value #( (
                                              BookingDate = '20200902'
                                              FlightPrice = '100'
                                              %control-BookingDate = if_abap_behv=>mk-on
                                              %control-FlightPrice = if_abap_behv=>mk-on
                                               ) )
                         ) )
             mapped data(ls_mapped)
             failed data(ls_failed)
             reported data(ls_reported).

    cl_abap_unit_assert=>assert_initial( ls_failed ).
    cl_abap_unit_assert=>assert_equals( msg = 'create booking failed' exp = '0000' act = ls_mapped-booking[ 1 ]-BookingID ).


    ""Create booking by association- Create the same booking again
    modify entities of /dmo/i_travel_tum
     entity travel
       create by \_Booking
         from value #( ( %key-TravelID = '987'
                         %target = value #( (
                                              BookingDate = '20200902'
                                              FlightPrice = '100'
                                              %control-BookingDate = if_abap_behv=>mk-on
                                              %control-FlightPrice = if_abap_behv=>mk-on
                                               ) )
                         ) )
             mapped ls_mapped
             failed ls_failed
             reported ls_reported.


    cl_abap_unit_assert=>assert_initial( ls_mapped ).
    cl_abap_unit_assert=>assert_equals( msg = 'create booking should have failed' exp = '0000' act = ls_failed-booking[ 1 ]-BookingID ).
    cl_abap_unit_assert=>assert_equals( msg = 'create booking should have failed' exp = '01' act = ls_failed-booking[ 1 ]-%create ).
    cl_abap_unit_assert=>assert_equals( msg = 'create booking should have failed' exp = '0000' act = ls_reported-booking[ 1 ]-BookingID ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ).

    modify entities of /dmo/i_travel_tum
     entity booking
       delete from value #( ( %key-TravelID = '987' %key-BookingID = '000' ) ).

  endmethod.

  method create_invalid_inst_by_assoc_2.

    ""Create booking by association
    "" Travel instance with TravelID 987 does not exist.
    modify entities of /dmo/i_travel_tum
     entity travel
       create by \_Booking
         from value #( ( %key-TravelID = '987'
                         %target = value #( (
                                              BookingDate = '20200902'
                                              FlightPrice = '100'
                                              %control-BookingDate = if_abap_behv=>mk-on
                                              %control-FlightPrice = if_abap_behv=>mk-on
                                               ) )
                         ) )
             mapped data(ls_mapped)
             failed data(ls_failed)
             reported data(ls_reported).


    cl_abap_unit_assert=>assert_initial( ls_mapped ).
    cl_abap_unit_assert=>assert_equals( msg = 'create booking should have failed' exp = '0000' act = ls_failed-booking[ 1 ]-BookingID ).
    cl_abap_unit_assert=>assert_equals( msg = 'create booking should have failed' exp = '01' act = ls_failed-booking[ 1 ]-%create ).
    cl_abap_unit_assert=>assert_equals( msg = 'create booking should have failed' exp = '0000' act = ls_reported-booking[ 1 ]-BookingID ).

  endmethod.


endclass.


class ltcl_read_eml definition final for testing
  duration short
  risk level harmless.

  private section.

    methods read_valid_entity_instance for testing raising cx_static_check.
    methods read_valid_entity_instances for testing raising cx_static_check.
    methods read_invalid_entity_instance for testing raising cx_static_check.
    methods read_not_existing_bo_node for testing raising cx_static_check.
    methods read_entity_with_multiple_keys for testing raising cx_static_check.
    methods read_entity_with_ctrl_struct for testing raising cx_static_check.
    methods read_mixed_entity_instances for testing raising cx_static_check.

    methods read_valid_instance_by_assoc for testing raising cx_static_check.
    methods read_invalid_inst_by_assoc_1 for testing raising cx_static_check.
    methods read_invalid_inst_by_assoc_2 for testing raising cx_static_check.
    methods read_invalid_inst_by_assoc_3 for testing raising cx_static_check.
    methods report_an_instance_for_modify for testing raising cx_static_check.
    methods report_an_instance_for_read for testing raising cx_static_check.

    class-methods class_setup.
    methods teardown.
    class-data double type ref to zcl_abap_behv_generic_test_dbl.
endclass.


class ltcl_read_eml implementation.

  method class_setup.
    double = new zcl_abap_behv_generic_test_dbl( '/dmo/i_travel_tum' ).
    cl_abap_behv_test_environment=>set_test_double( double ).
  endmethod.

  method read_entity_with_ctrl_struct.
    " TODO: Check if required.
  endmethod.

  method read_entity_with_multiple_keys.
    " A booking with TravelID and BookingID as keys

    " Record test data
    " create travel instance
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                            ( travelid = '000'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    " create booking instance
    modify entities of /dmo/i_travel_tum
     entity booking
       create from value #( ( travelid = '987'
                              BookingID = '001'
                                             BookingDate = '20200902'
                                             FlightPrice = '100'
                                             %control-BookingDate = if_abap_behv=>mk-on
                                             %control-FlightPrice = if_abap_behv=>mk-on )
                            ( travelid = '000'
                              BookingID = '001'
                                             BookingDate = '20200902'
                                             FlightPrice = '100'
                                             %control-BookingDate = if_abap_behv=>mk-on
                                             %control-FlightPrice = if_abap_behv=>mk-on )
                          )
     reported reported
     failed failed
     mapped mapped.

    " CUT
    " Read booking

    read entities of /dmo/i_travel_tum
    entity booking
     from value #(
                ( travelid = '987' BookingID = '001' %control-FlightPrice = if_abap_behv=>mk-on )
              )
     result   data(result)
     reported reported
     failed   failed.

    cl_abap_unit_assert=>assert_not_initial( result ).
    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).

    cl_abap_unit_assert=>assert_equals( msg = 'read failed' exp = '100' act = result[ 1 ]-FlightPrice ).


    read entities of /dmo/i_travel_tum
     entity booking
      from value #(
                 ( travelid = '988' BookingID = '001' %control-FlightPrice = if_abap_behv=>mk-on )
               )
      result   result
      reported reported
      failed   failed.

    cl_abap_unit_assert=>assert_equals( msg = 'read passed' exp = '988' act = failed-booking[ 1 ]-TravelID ).
    cl_abap_unit_assert=>assert_equals( msg = 'read passed' exp = '001' act = failed-booking[ 1 ]-BookingID ).

    read entities of /dmo/i_travel_tum
     entity booking
      from value #(
                 ( travelid = '987' BookingID = '002' %control-FlightPrice = if_abap_behv=>mk-on )
               )
      result   result
      reported reported
      failed   failed.

    cl_abap_unit_assert=>assert_equals( msg = 'read passed' exp = '987' act = failed-booking[ 1 ]-TravelID ).
    cl_abap_unit_assert=>assert_equals( msg = 'read passed' exp = '002' act = failed-booking[ 1 ]-BookingID ).

    read entities of /dmo/i_travel_tum
     entity booking
      from value #(
                 (  BookingID = '001' %control-FlightPrice = if_abap_behv=>mk-on )
               )
      result   result
      reported reported
      failed   failed.

    cl_abap_unit_assert=>assert_equals( msg = 'read failed' exp = '100' act = result[ 1 ]-FlightPrice ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ( %key-TravelID = '000' ) ).

    modify entities of /dmo/i_travel_tum
     entity booking
       delete from value #( ( %key-TravelID = '987' %key-BookingID = '001' ) ( %key-TravelID = '000' %key-BookingID = '001' ) ).


  endmethod.

  method read_invalid_entity_instance.
    " read an entity that does not exist

    " Record test data
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    " Read travel entity which does not exist
    read entity /dmo/i_travel_tum
     from value #(
                ( travelid = '988' %control-begindate = if_abap_behv=>mk-on %control-totalprice = if_abap_behv=>mk-on )
              )
     result   data(result)
     reported reported
     failed   failed.

    cl_abap_unit_assert=>assert_initial( result ).
    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_not_initial( reported ).

    cl_abap_unit_assert=>assert_equals( msg = 'read passed' exp = '988' act = failed-travel[ 1 ]-TravelID ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ).


  endmethod.


  method read_mixed_entity_instances.
    " read an entity that does not exist and read an entity that exists

    " Record test data
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    " Read travel entity which does not exist
    read entity /dmo/i_travel_tum
     from value #(
                ( travelid = '987' %control-begindate = if_abap_behv=>mk-on %control-totalprice = if_abap_behv=>mk-on )
                ( travelid = '988' %control-begindate = if_abap_behv=>mk-on %control-totalprice = if_abap_behv=>mk-on )
              )
     result   data(result)
     reported reported
     failed   failed.

    cl_abap_unit_assert=>assert_equals( msg = 'read failed for travel id 987' exp = '987' act = result[ 1 ]-TravelID ).
    cl_abap_unit_assert=>assert_equals( msg = 'read passed for travel id 988' exp = '988' act = failed-travel[ 1 ]-TravelID ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ).

  endmethod.



  method read_not_existing_bo_node.
    " Results in a compile time error to read a non existing bo node.
  endmethod.

  method read_valid_entity_instance.

    " Record test data
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              memo = 'travel 1'
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on
                              %control-Memo = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    " CUT
    read entity /dmo/i_travel_tum
     from value #(
                ( travelid = '987' %control-Memo = if_abap_behv=>mk-on )
              )
     result   data(result)
     reported data(reported2)
     failed   data(failed2).

    cl_abap_unit_assert=>assert_not_initial( result ).
    cl_abap_unit_assert=>assert_initial( reported2 ).
    cl_abap_unit_assert=>assert_initial( failed2 ).

    cl_abap_unit_assert=>assert_equals( msg = 'read failed' exp = '987' act = result[ 1 ]-TravelID ).
    cl_abap_unit_assert=>assert_equals( msg = 'read failed' exp = 'travel 1' act = result[ 1 ]-Memo ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' )
                          ).

  endmethod.

  method read_valid_entity_instances.

    " Record test data
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              Memo = 'travel 1'
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on
                              %control-Memo = if_abap_behv=>mk-on )
                            ( travelid = '988'
                              begindate = '20190101' EndDate = '20190101'
                              totalprice = 300
                              Memo = 'travel 2'
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on
                              %control-Memo = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    " CUT
    read entity /dmo/i_travel_tum
     from value #(
                ( travelid = '987' %control-begindate = if_abap_behv=>mk-on %control-totalprice = if_abap_behv=>mk-on )
                ( travelid = '988' %control-memo = if_abap_behv=>mk-on %control-totalprice = if_abap_behv=>mk-on )
              )
     result   data(result)
     reported data(reported2)
     failed   data(failed2).

    cl_abap_unit_assert=>assert_not_initial( result ).
    cl_abap_unit_assert=>assert_initial( reported2 ).
    cl_abap_unit_assert=>assert_initial( failed2 ).

    read table result with key TravelID = '988' into data(travel).
    cl_abap_unit_assert=>assert_equals( msg = 'read failed' exp = 0 act = sy-subrc ).

    cl_abap_unit_assert=>assert_equals( msg = 'read failed' exp = 'travel 2' act = travel-Memo ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ( %key-TravelID = '988' ) ).

  endmethod.


  method read_valid_instance_by_assoc.

    " Record step to insert a travel instance .
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).


    ""Record: Create booking by association
    modify entities of /dmo/i_travel_tum
     entity travel
       create by \_Booking
         from value #( ( %key-TravelID = '987'
                         %target = value #( ( "Since no booking Id is passed booking id 0000 will be assigned to the booking
                                              BookingDate = '20200902'
                                              FlightPrice = '100'
                                              %control-BookingDate = if_abap_behv=>mk-on
                                              %control-FlightPrice = if_abap_behv=>mk-on
                                               ) )
                         ) )
             mapped data(ls_mapped)
             failed data(ls_failed)
             reported data(ls_reported).

    cl_abap_unit_assert=>assert_initial( ls_failed ).
    cl_abap_unit_assert=>assert_equals( msg = 'create booking failed' exp = '0000' act = ls_mapped-booking[ 1 ]-BookingID ).

    " CUT
    read entity /dmo/i_travel_tum
    by \_Booking
     from value #(
                ( travelid = '987'  %control-FlightPrice = if_abap_behv=>mk-on %control-BookingDate = if_abap_behv=>mk-on )
              )
     result   data(result)
     reported data(reported2)
     failed   data(failed2).

    cl_abap_unit_assert=>assert_not_initial( result ).
    cl_abap_unit_assert=>assert_initial( reported2 ).
    cl_abap_unit_assert=>assert_initial( failed2 ).

    read table result with key bookingid = '000' into data(booking).
    cl_abap_unit_assert=>assert_equals( msg = 'read failed' exp = 0 act = sy-subrc ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ( %key-TravelID = '000' ) ).

    modify entities of /dmo/i_travel_tum
     entity booking
       delete from value #( ( %key-TravelID = '987' %key-BookingID = '001' ) ( %key-TravelID = '000' %key-BookingID = '001' ) ).


  endmethod.

  method read_invalid_inst_by_assoc_1.
    " No booking is present for the queried travel

    " Record step to insert a travel instance .
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).


    ""NO booking
*    modify entities of /dmo/i_travel_tum
*     entity travel
*       create by \_Booking
*         from value #( ( %key-TravelID = '987'
*                         %target = value #( (
*                                              BookingDate = '20200902'
*                                              FlightPrice = '100'
*                                              %control-BookingDate = if_abap_behv=>mk-on
*                                              %control-FlightPrice = if_abap_behv=>mk-on
*                                               ) )
*                         ) )
*             MAPPED DATA(ls_mapped)
*             FAILED DATA(ls_failed)
*             REPORTED DATA(ls_reported).
*
*    cl_abap_unit_assert=>assert_initial( ls_failed ).
*    cl_abap_unit_assert=>assert_equals( msg = 'create booking failed' exp = '0000' act = ls_mapped-booking[ 1 ]-BookingID ).

    " CUT
    read entity /dmo/i_travel_tum
    by \_Booking
     from value #(
                ( travelid = '987'  %control-FlightPrice = if_abap_behv=>mk-on %control-BookingDate = if_abap_behv=>mk-on )
              )
     result   data(result)
     reported data(reported2)
     failed   data(failed2).

    cl_abap_unit_assert=>assert_initial( result ).
    cl_abap_unit_assert=>assert_initial( reported2 ).
    cl_abap_unit_assert=>assert_initial( failed2 ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ).

  endmethod.

  method read_invalid_inst_by_assoc_2.
    "  queried travel for booking does not exist

    " Record step to insert a travel instance .
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).


    " CUT
    read entity /dmo/i_travel_tum
    by \_Booking
     from value #(
                ( travelid = '988'  %control-FlightPrice = if_abap_behv=>mk-on %control-BookingDate = if_abap_behv=>mk-on )
              )
     result   data(result)
     reported data(reported2)
     failed   data(failed2).

    cl_abap_unit_assert=>assert_initial( result ).
    cl_abap_unit_assert=>assert_not_initial( failed2 ).

    cl_abap_unit_assert=>assert_equals( msg = 'read should failed' exp = '988' act = failed2-travel[ 1 ]-TravelID ).
    cl_abap_unit_assert=>assert_equals( msg = 'read should failed' exp = '01' act = failed2-travel[ 1 ]-%assoc-_Booking ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ).

  endmethod.

  method read_invalid_inst_by_assoc_3.
    " No travel entity instance exists in buffer
    " Querying a travel for booking will fail

    " Record step to insert a travel instance .
*     modify entities of /dmo/i_travel_tum
*      entity travel
*        create from value #( ( travelid = '987'
*                               begindate = '20180101' EndDate = '20180101'
*                               totalprice = 100
*                               %control-begindate = if_abap_behv=>mk-on
*                               %control-totalprice = if_abap_behv=>mk-on )
*                           )
*      reported data(reported)
*      failed data(failed)
*      mapped data(mapped).


    " CUT
    read entity /dmo/i_travel_tum
    by \_Booking
     from value #(
                ( travelid = '988'  %control-FlightPrice = if_abap_behv=>mk-on %control-BookingDate = if_abap_behv=>mk-on )
              )
     result   data(result)
     reported data(reported2)
     failed   data(failed2).

    cl_abap_unit_assert=>assert_initial( result ).
    cl_abap_unit_assert=>assert_not_initial( failed2 ).

    cl_abap_unit_assert=>assert_equals( msg = 'read should failed' exp = '988' act = failed2-travel[ 1 ]-TravelID ).
    cl_abap_unit_assert=>assert_equals( msg = 'read should failed' exp = '01' act = failed2-travel[ 1 ]-%assoc-_Booking ).

  endmethod.

  method report_an_instance_for_modify.

    "Record
    modify entities of /dmo/i_travel_tum
    entity travel
      create from value #( ( travelid = '987'
                            begindate = '20180101' EndDate = '20180101'
                            totalprice = 100
                            %control-begindate = if_abap_behv=>mk-on
                            %control-totalprice = if_abap_behv=>mk-on )
                         )
    reported data(reported)
    failed data(failed)
    mapped data(mapped).




    "Configure the exception
    data: lo_bo_exception type ref to z_bo_mock_exception.
          create object lo_bo_exception.
    reported-travel = value #( ( TravelID = '987' %msg = lo_bo_exception ) ).
    double->config_error_response_4_modify( reported = reported  failed = failed ).
    "double->config_success_response( mapped = mapped ).
    "Replay - code under test
    modify entities of /dmo/i_travel_tum
    entity travel
      create from value #( ( travelid = '987'
                             begindate = '20180101' EndDate = '20180101'
                            totalprice = 100
                            %control-begindate = if_abap_behv=>mk-on
                            %control-totalprice = if_abap_behv=>mk-on )
                         )
    reported data(reported1)
    failed data(failed1)
    mapped data(mapped1).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines(  reported1-travel ) ).

  endmethod.

  method report_an_instance_for_read.

  "Record
    modify entities of /dmo/i_travel_tum
    entity travel
      create from value #( ( travelid = '987'
                            begindate = '20180101' EndDate = '20180101'
                            totalprice = 100
                            %control-begindate = if_abap_behv=>mk-on
                            %control-totalprice = if_abap_behv=>mk-on )
                         )
    reported data(reported)
    failed data(failed)
    mapped data(mapped).

    data: lo_bo_exception type ref to z_bo_mock_exception.
          create object lo_bo_exception.
    reported-travel = value #( ( TravelID = '987' %msg = lo_bo_exception ) ).
    double->config_error_response_4_read( reported = reported failed = failed ).

  read entity /dmo/i_travel_tum
    from value #(
               ( travelid = '987' %control-begindate = if_abap_behv=>mk-on %control-totalprice = if_abap_behv=>mk-on )
             )
    result   data(result)
    reported data(reported2)
    failed   data(failed2).

   cl_abap_unit_assert=>assert_equals( exp =  reported act = reported2 ).


  endmethod.

  method teardown.
    rollback entities.
  endmethod.

endclass.
