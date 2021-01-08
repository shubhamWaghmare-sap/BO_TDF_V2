class ltc_bo_tdf_demo definition final for testing duration short risk level harmless.

  private section.
    class-data:
        cut type ref to zcl_abap_behv_test_demo.

    class-methods:
        class_setup.

    methods:
        validate_discounted_price for testing.

endclass.


class ltc_bo_tdf_demo implementation.

  method class_setup.
    data double type ref to zcl_abap_behv_generic_test_dbl.

    double = new zcl_abap_behv_generic_test_dbl( '/dmo/i_travel_tum' ).
    cl_abap_behv_test_environment=>set_test_double( double ).
    cut = new zcl_abap_behv_test_demo( ).
  endmethod.

  method validate_discounted_price.

    "Inserting test data
    modify entities of /dmo/i_travel_tum
      entity travel
        create from value #( ( travelid = '66' currencycode = 'USD' %control-currencycode = if_abap_behv=>mk-on ) )
      reported data(reported)
      failed   data(failed)
      mapped   data(mapped).

    "Inserting duplicate test data
    modify entities of /dmo/i_travel_tum
      entity travel
        create from value #(
                             ( travelid = '987' begindate = '20180101' EndDate = '20180101' totalprice = 100  %control-begindate = if_abap_behv=>mk-on %control-totalprice = if_abap_behv=>mk-on )
                             ( travelid = '987' currencycode = 'EUR'  %control-currencycode = if_abap_behv=>mk-on )
                           )
      reported reported
      failed failed
      mapped mapped.

    data(discounted_price) = cut->calculate_discount( '987' ).

    " check returned values
    cl_abap_unit_assert=>assert_equals( exp = 80 act = discounted_price ).

  endmethod.
endclass.
