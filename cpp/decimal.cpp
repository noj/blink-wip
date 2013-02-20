// vim: set ts=4 sw=4
#include "decimal.h"
#include "types.h"

#include <cmath>
#include <iostream>

namespace blink
{
    Decimal::Decimal(double d)
        : m_mantissa(0)
        , m_exponent(0)
    {
        from_double(d);
    }

    Decimal::Decimal(i64 mantissa, i8 exponent)
        : m_mantissa(mantissa)
        , m_exponent(exponent)
    {
        normalize();
    }

    Decimal::operator double() const
    {
        return m_mantissa * std::pow(10.0, m_exponent);
    }

    // Private implementation:
    //
    void Decimal::normalize()
    {
        int decs = 0;

        while(m_mantissa % 10 == 0)
        {
            ++decs;
            m_mantissa /= 10;
        }

        m_exponent += decs;
    }

    void Decimal::from_double(double d)
    {
        // Truncate decimal part:
        m_mantissa = static_cast<i64>(d);

        double rest = d - static_cast<double>(m_mantissa);

        // Accumulated decimals:
        i32 decimals = 0;
        i64 acc = 0;

        while(true) {
            // Calculate next decimal:
            i32 digit = static_cast<i32>(rest * 10);
            // TODO: Rest comparison here, something more scientifically sound perhaps?
            if(digit == 0 && rest <= 1e-6)
                // No more decimals, we're done
                break;

            ++decimals;

            acc *= 10;
            acc += digit;

            rest *= 10;
            rest -= static_cast<double>(digit);
        }

        m_mantissa = m_mantissa * static_cast<i64>(std::pow(10.0, decimals));
        m_mantissa += acc;

        m_exponent = -decimals;

        // TODO: Shouldn't be needed:
        normalize();
    }

    std::ostream& operator<<(std::ostream& os, const Decimal& dec)
    {
        return os << static_cast<double>(dec);
    }
}

