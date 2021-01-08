class zcl_abap_behv_test_demo definition public final create public .

  public section.
    types: tt_reported type response for reported /dmo/i_travel_tum.

    methods: calculate_discount
      importing
        travel_id               type i
      returning
        value(discounted_price) type i.

  protected section.
  private section.
endclass.

class zcl_abap_behv_test_demo implementation.

  method calculate_discount.

    read entity /dmo/i_travel_tum
     from value #(
                   ( travelid = travel_id %control-begindate = if_abap_behv=>mk-on %control-totalprice = if_abap_behv=>mk-on )
                 )
     result   data(result)
     reported data(reported)
     failed   data(failed).
    data(travel_result) = result[ 1 ].
    discounted_price = travel_result-totalprice - ( ( travel_result-totalprice * 20 ) / 100 ).

  endmethod.

endclass.
