#pragma once

#include <stdexcept>

namespace blink
{
    template <typename T>
    struct Optional {

        Optional()
            : m_set(false)
            , m_value()
        {}

        Optional(T val)
            : m_set(true)
            , m_value(val)
        {}

        operator bool() const { return m_set; }

        T value() const {
            if(!m_set)
                throw std::runtime_error("Optional not set");

            return m_value;
        }

        Optional<T>& operator=(const T& val) {
            m_set = true;
            m_value = val;

            return *this;
        }

    private:

        bool m_set;
        T    m_value;
    };
}
