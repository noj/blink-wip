// vim: set ts=4 sw=4
#pragma once

#include "types.h"

#include <iosfwd>

namespace blink
{
    class Decimal {
    public:
        Decimal(double d);
        Decimal(i64 mantissa, i8 exponent);

        inline i64 mantissa() const { return m_mantissa; }
        inline i8 exponent() const  { return m_exponent; }

        operator double() const;

    private:
        void normalize();
        void from_double(double d);

        friend std::ostream& operator<<(std::ostream&, const Decimal&);

        i64 m_mantissa;
        i8  m_exponent;
    };
}

