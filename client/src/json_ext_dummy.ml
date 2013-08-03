module type Json_ext = sig
  type a
end

module Default(D : Json_ext) : Json_ext with type a = D.a = struct
  include D
end

module Json_ext_int = Default(struct
    type a = int
  end)


module Json_ext_int32 = Default(struct
    type a = int32
  end)

module Json_ext_int64 = Default(struct
    type a = int64
  end)

module Json_ext_bool = Default(struct
    type a = bool
  end)

module Json_ext_float = Default(struct
    type a = float
  end)

module Json_ext_string = Default(struct
    type a = string
  end)

module Json_ext_list (A : Json_ext) = Default(struct
    type a = A.a list
  end)


module Json_ext_array (A : Json_ext) = Default(struct
    type a = A.a array
  end)


module Json_ext_option (A : Json_ext) = Default(struct
    type a = A.a option
  end)
