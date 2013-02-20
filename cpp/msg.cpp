#include "msg.h"

#include <unordered_map>
#include <string>
#include <vector>

namespace Draw {

    static MetaData s_meta;

    const char * Circle::type_name() { return "Draw:Circle"; }

    void Circle::register_meta_data(MetaData& meta)
    {
        auto fields = meta[type_name()];

        fields[Circle::Radius::FieldId] = "Radius";
        fields[Circle::Area::FieldId] = "Area";
    }

    void register_meta_data()
    {
        Circle::register_meta_data(s_meta);
    }
}

