#include "decimal.h"

#define BOOST_TEST_MODULE Decimal
#include <boost/test/included/unit_test.hpp>

#include <iostream>

BOOST_AUTO_TEST_CASE(DecimalConstruction)
{
    BOOST_CHECK_EQUAL(blink::Decimal(314, -2), 3.14);
    BOOST_CHECK_EQUAL(blink::Decimal(100, 0), 100);
    BOOST_CHECK_EQUAL(blink::Decimal(1000, 2), 100000);
    BOOST_CHECK_EQUAL(blink::Decimal(3.14), 3.14);
    BOOST_CHECK_EQUAL(blink::Decimal(3.04), 3.04);
}

