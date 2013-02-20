#pragma once

#include "types.h"
#include "decimal.h"
#include "opt.h"

#include <unordered_map>
#include <map>

//
// Using this sample schema:
//
// namespace Draw
//
// Shape ->
//   decimal Area?
//
// Circle : Shape ->
//   u32 Radius
//
// Tag encoded:
//   @Draw:Circle|Radius=2|Area=28.3
//

template <blink::u64 Id, 
          typename T>
struct Field {

    enum { FieldId = Id };

    T           value;
   // std::string name;
};

typedef std::unordered_map<std::string,
            std::map<blink::i64, std::string>> MetaData;

namespace Draw {

    struct Circle {

        static const char * type_name();
        static void register_meta_data(MetaData&);

        Field<1, blink::u32> Radius;

        // Shape fields:
        Field<2, blink::Optional<blink::Decimal>> Area;
    };

    void register_meta_data();

}

