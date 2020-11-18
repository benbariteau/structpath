extern crate serde;
extern crate thiserror;

use std::collections::HashMap;
use thiserror::Error;
use std::num::{ParseFloatError, ParseIntError};
use serde::de::Visitor;
use std::fmt::Display;

pub enum SegmentType {
    F32,
    F64,
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    String,
}

pub struct SegmentValueSchema {
    pub name: String,
    pub segment_type: SegmentType,
}

#[derive(PartialEq, Debug, Clone)]
pub enum SegmentValue {
    F32(f32),
    F64(f64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    String(String),
}


pub enum SegmentSchema {
    Literal(String),
    Value(SegmentValueSchema),
}

pub struct Schema {
    pub segments: Vec<SegmentSchema>,
}

#[derive(Error, Debug)]
pub enum StructPathError {
    #[error("Incorrect path segment (expected {expected:?}, got {got:?})")]
    IncorrectSegment{
        got: String,
        expected: String,
    },
    #[error(transparent)]
    ParseFloatError(#[from] ParseFloatError),
    #[error(transparent)]
    ParseIntError(#[from] ParseIntError),
    #[error("Error from serde: {0}")]
    SerdeInternalError(String),
    #[error("Error is impossible, but reqired structurrally")]
    Impossible,
    #[error("Expected {0}, but got {1:?}")]
    ExpectedType(String, SegmentValue),
    #[error("Not supported: {0}")]
    NotSupported(String),
}

impl serde::de::Error for StructPathError {
    fn custom<T>(msg: T) -> Self where T: Display {
        StructPathError::SerdeInternalError(msg.to_string())
    }
}

fn parse_path_generic(path: String, schema: Schema) -> Result<HashMap<String, SegmentValue>, StructPathError> {
    let mut path_values = HashMap::new();
    for (segment, segment_schema) in path.split("/").skip(1).zip(schema.segments.iter()) {
        match segment_schema {
            SegmentSchema::Literal(literal) => {
                if segment != literal {
                    return Err(StructPathError::IncorrectSegment{got: segment.to_owned(), expected: literal.clone()});
                }
            }
            SegmentSchema::Value(segment_value_schema) => {
                match segment_value_schema.segment_type {
                    SegmentType::F32 => {
                        path_values.insert(segment_value_schema.name.clone(), SegmentValue::F32(segment.parse()?));
                    },
                    SegmentType::F64 => {
                        path_values.insert(segment_value_schema.name.clone(), SegmentValue::F64(segment.parse()?));
                    },
                    SegmentType::I8 => {
                        path_values.insert(segment_value_schema.name.clone(), SegmentValue::I8(segment.parse()?));
                    },
                    SegmentType::I16 => {
                        path_values.insert(segment_value_schema.name.clone(), SegmentValue::I16(segment.parse()?));
                    },
                    SegmentType::I32 => {
                        path_values.insert(segment_value_schema.name.clone(), SegmentValue::I32(segment.parse()?));
                    },
                    SegmentType::I64 => {
                        path_values.insert(segment_value_schema.name.clone(), SegmentValue::I64(segment.parse()?));
                    },
                    SegmentType::I128 => {
                        path_values.insert(segment_value_schema.name.clone(), SegmentValue::I128(segment.parse()?));
                    },
                    SegmentType::U8 => {
                        path_values.insert(segment_value_schema.name.clone(), SegmentValue::U8(segment.parse()?));
                    },
                    SegmentType::U16 => {
                        path_values.insert(segment_value_schema.name.clone(), SegmentValue::U16(segment.parse()?));
                    },
                    SegmentType::U32 => {
                        path_values.insert(segment_value_schema.name.clone(), SegmentValue::U32(segment.parse()?));
                    },
                    SegmentType::U64 => {
                        path_values.insert(segment_value_schema.name.clone(), SegmentValue::U64(segment.parse()?));
                    },
                    SegmentType::U128 => {
                        path_values.insert(segment_value_schema.name.clone(), SegmentValue::U128(segment.parse()?));
                    },
                    SegmentType::String => {
                        path_values.insert(segment_value_schema.name.clone(), SegmentValue::String(segment.to_owned()));
                    },
                }
            },
        }
    }
    Ok(path_values)
}

struct Deserializer {
    generic_parsed_path: HashMap<String, SegmentValue>,
}

impl <'de, 'a> serde::de::Deserializer<'de> for &'a Deserializer {
    type Error = StructPathError;

    fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("deserialize_any".to_owned()))
    }

    fn deserialize_bool<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("bool".to_owned()))
    }

    fn deserialize_i8<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("i8".to_owned()))
    }

    fn deserialize_i16<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("i16".to_owned()))
    }

    fn deserialize_i32<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("i32".to_owned()))
    }

    fn deserialize_i64<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("i64".to_owned()))
    }

    fn deserialize_i128<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("i128".to_owned()))
    }

    fn deserialize_u8<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("u8".to_owned()))
    }

    fn deserialize_u16<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("u16".to_owned()))
    }

    fn deserialize_u32<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("u32".to_owned()))
    }

    fn deserialize_u64<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("u64".to_owned()))
    }

    fn deserialize_u128<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("u128".to_owned()))
    }

    fn deserialize_f32<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("f32".to_owned()))
    }

    fn deserialize_f64<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("f64".to_owned()))
    }

    fn deserialize_char<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("char".to_owned()))
    }

    fn deserialize_str<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("&str".to_owned()))
    }

    fn deserialize_string<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("String".to_owned()))
    }

    fn deserialize_bytes<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("bytes".to_owned()))
    }

    fn deserialize_byte_buf<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("bytes_buf".to_owned()))
    }

    fn deserialize_option<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("Option".to_owned()))
    }

    fn deserialize_unit<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("()".to_owned()))
    }

    fn deserialize_unit_struct<V>(self, _name: &'static str, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("unit struct".to_owned()))
    }

    fn deserialize_newtype_struct<V>(self, _name: &'static str, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("newtype struct".to_owned()))
    }

    fn deserialize_seq<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("sequence".to_owned()))
    }

    fn deserialize_tuple<V>(self, _len: usize, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("tuple".to_owned()))
    }

    fn deserialize_tuple_struct<V>(self, _name: &'static str, _len: usize, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("tuple struct".to_owned()))
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        visitor.visit_map(MapAccess{
            deserializer: &self,
            keys: self.generic_parsed_path.keys().cloned().collect(),
            last_key: "".to_owned(),
        })
    }

    fn deserialize_struct<V>(self, _name: &'static str, _fields: &'static [&'static str], visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        self.deserialize_map(visitor)
    }

    fn deserialize_enum<V>(self, _name: &'static str, _variants: &'static [&'static str], _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("enum".to_owned()))
    }

    fn deserialize_identifier<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("identifier".to_owned()))
    }

    fn deserialize_ignored_any<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("deserialize_ignored_any".to_owned()))
    }
}

struct MapAccess<'a> {
    deserializer: &'a Deserializer,
    keys: Vec<String>,
    last_key: String,
}

impl<'de, 'a> serde::de::MapAccess<'de> for MapAccess<'a> {
    type Error = StructPathError;
    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error> where K: serde::de::DeserializeSeed<'de> {
        if self.keys.len() == 0 {
            return Ok(None)
        }
        {
            let next_key = &self.keys[0];
            self.last_key = next_key.clone();
        }
        self.keys = self.keys.iter().skip(1).cloned().collect();
        seed.deserialize(IdentDeserializer{ident: self.last_key.clone()}).map(Some)
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>  where V: serde::de::DeserializeSeed<'de> {
        let value = self.deserializer.generic_parsed_path.get(&self.last_key).ok_or(StructPathError::Impossible)?;
        seed.deserialize(ValueDeserializer{value: value.clone()})
    }
}

struct IdentDeserializer{
    ident: String,
}

impl <'de> serde::de::Deserializer<'de> for IdentDeserializer {
    type Error = StructPathError;

    fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("deserialize_any".to_owned()))
    }

    fn deserialize_bool<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("bool".to_owned()))
    }

    fn deserialize_i8<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("i8".to_owned()))
    }

    fn deserialize_i16<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("i16".to_owned()))
    }

    fn deserialize_i32<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("i32".to_owned()))
    }

    fn deserialize_i64<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("i64".to_owned()))
    }

    fn deserialize_u8<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("u8".to_owned()))
    }

    fn deserialize_u16<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("u16".to_owned()))
    }

    fn deserialize_u32<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("u32".to_owned()))
    }

    fn deserialize_u64<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("u64".to_owned()))
    }

    fn deserialize_f32<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("f32".to_owned()))
    }

    fn deserialize_f64<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("f64".to_owned()))
    }

    fn deserialize_char<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("char".to_owned()))
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        visitor.visit_string(self.ident.clone())
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        self.deserialize_str(visitor)
    }

    fn deserialize_bytes<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("bytes".to_owned()))
    }

    fn deserialize_byte_buf<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("byte_buf".to_owned()))
    }

    fn deserialize_option<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("Option".to_owned()))
    }

    fn deserialize_unit<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("()".to_owned()))
    }

    fn deserialize_unit_struct<V>(self, _name: &'static str, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("unit struct".to_owned()))
    }

    fn deserialize_newtype_struct<V>(self, _name: &'static str, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("newtype struct".to_owned()))
    }

    fn deserialize_seq<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("sequence".to_owned()))
    }

    fn deserialize_tuple<V>(self, _len: usize, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("len".to_owned()))
    }

    fn deserialize_tuple_struct<V>(self, _name: &'static str, _len: usize, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("tuple struct".to_owned()))
    }

    fn deserialize_map<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("map".to_owned()))
    }

    fn deserialize_struct<V>(self, _name: &'static str, _fields: &'static [&'static str], _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("struct".to_owned()))
    }

    fn deserialize_enum<V>(self, _name: &'static str, _variants: &'static [&'static str], _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("enum".to_owned()))
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        self.deserialize_str(visitor)
    }

    fn deserialize_ignored_any<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("deserialize_ignored_any".to_owned()))
    }
}

struct ValueDeserializer {
    value: SegmentValue,
}

impl <'de, 'a> serde::de::Deserializer<'de> for ValueDeserializer {
    type Error = StructPathError;

    fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("deserialize_any".to_owned()))
    }

    fn deserialize_bool<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("bool".to_owned()))
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        match self.value {
            SegmentValue::I8(value) => visitor.visit_i8(value),
            _ => Err(StructPathError::ExpectedType("i8".to_owned(), self.value)),
        }
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        match self.value {
            SegmentValue::I16(value) => visitor.visit_i16(value),
            _ => Err(StructPathError::ExpectedType("i16".to_owned(), self.value)),
        }
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        match self.value {
            SegmentValue::I32(value) => visitor.visit_i32(value),
            _ => Err(StructPathError::ExpectedType("i32".to_owned(), self.value)),
        }
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        match self.value {
            SegmentValue::I64(value) => visitor.visit_i64(value),
            _ => Err(StructPathError::ExpectedType("i64".to_owned(), self.value)),
        }
    }

    fn deserialize_i128<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        match self.value {
            SegmentValue::I128(value) => visitor.visit_i128(value),
            _ => Err(StructPathError::ExpectedType("i128".to_owned(), self.value)),
        }
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        match self.value {
            SegmentValue::U8(value) => visitor.visit_u8(value),
            _ => Err(StructPathError::ExpectedType("u8".to_owned(), self.value)),
        }
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        match self.value {
            SegmentValue::U16(value) => visitor.visit_u16(value),
            _ => Err(StructPathError::ExpectedType("u16".to_owned(), self.value)),
        }
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        match self.value {
            SegmentValue::U32(value) => visitor.visit_u32(value),
            _ => Err(StructPathError::ExpectedType("u32".to_owned(), self.value)),
        }
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        match self.value {
            SegmentValue::U64(value) => visitor.visit_u64(value),
            _ => Err(StructPathError::ExpectedType("u64".to_owned(), self.value)),
        }
    }

    fn deserialize_u128<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        match self.value {
            SegmentValue::U128(value) => visitor.visit_u128(value),
            _ => Err(StructPathError::ExpectedType("u128".to_owned(), self.value)),
        }
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        match self.value {
            SegmentValue::F32(value) => visitor.visit_f32(value),
            _ => Err(StructPathError::ExpectedType("f32".to_owned(), self.value)),
        }
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        match self.value {
            SegmentValue::F64(value) => visitor.visit_f64(value),
            _ => Err(StructPathError::ExpectedType("f64".to_owned(), self.value)),
        }
    }

    fn deserialize_char<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("char".to_owned()))
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        self.deserialize_string(visitor)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        match self.value {
            SegmentValue::String(value) => visitor.visit_string(value.clone()),
            _ => Err(StructPathError::ExpectedType("String".to_owned(), self.value)),
        }
    }

    fn deserialize_bytes<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("bytes".to_owned()))
    }

    fn deserialize_byte_buf<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("byte_buf".to_owned()))
    }

    fn deserialize_option<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("Option".to_owned()))
    }

    fn deserialize_unit<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("()".to_owned()))
    }

    fn deserialize_unit_struct<V>(self, _name: &'static str, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("unit struct".to_owned()))
    }

    fn deserialize_newtype_struct<V>(self, _name: &'static str, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("newtype struct".to_owned()))
    }

    fn deserialize_seq<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("sequence".to_owned()))
    }

    fn deserialize_tuple<V>(self, _len: usize, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("tuple".to_owned()))
    }

    fn deserialize_tuple_struct<V>(self, _name: &'static str, _len: usize, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("tuple struct".to_owned()))
    }

    fn deserialize_map<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("map".to_owned()))
    }

    fn deserialize_struct<V>(self, _name: &'static str, _fields: &'static [&'static str], _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("struct".to_owned()))
    }

    fn deserialize_enum<V>(self, _name: &'static str, _variants: &'static [&'static str], _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("enum".to_owned()))
    }

    fn deserialize_identifier<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("identifier".to_owned()))
    }

    fn deserialize_ignored_any<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        Err(StructPathError::NotSupported("deserialize_ignored_any".to_owned()))
    }
}

pub fn parse_path<'a, T>(path: String, schema: Schema) -> Result<T, StructPathError> where T: serde::Deserialize<'a> {
    let generic_parsed_path_value = parse_path_generic(path, schema)?;
    let deserializer = Deserializer{generic_parsed_path: generic_parsed_path_value};
    T::deserialize(&deserializer)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde::Deserialize;
    #[test]
    fn test_parse_path_generic() {
        assert_eq!(
            parse_path_generic(
                "/foo/1/bar/thing".to_owned(),
                Schema{
                    segments: vec![
                        SegmentSchema::Literal("foo".to_owned()),
                        SegmentSchema::Value(SegmentValueSchema{
                            name: "foo".to_owned(),
                            segment_type: SegmentType::U64,
                        }),
                        SegmentSchema::Literal("bar".to_owned()),
                        SegmentSchema::Value(SegmentValueSchema{
                            name: "bar".to_owned(),
                            segment_type: SegmentType::String,
                        }),
                    ],
                }
            ).unwrap(),
            {
                let mut map = HashMap::new();
                map.insert("foo".to_owned(), SegmentValue::U64(1));
                map.insert("bar".to_owned(), SegmentValue::String("thing".to_owned()));
                map
            },
            );
    }

    #[test]
    fn test_parse_path_generic_float() {
        assert_eq!(
            parse_path_generic(
                "/foo/1.2".to_owned(),
                Schema{
                    segments: vec![
                        SegmentSchema::Literal("foo".to_owned()),
                        SegmentSchema::Value(SegmentValueSchema{
                            name: "foo".to_owned(),
                            segment_type: SegmentType::F64,
                        }),
                    ],
                },
                ).unwrap(),
            {
                let mut map = HashMap::new();
                map.insert("foo".to_owned(), SegmentValue::F64(1.2));
                map
            },
            );
    }

    #[test]
    fn test_parse_path_generic_signed_integer() {
        assert_eq!(
            parse_path_generic(
                "/foo/-1".to_owned(),
                Schema{
                    segments: vec![
                        SegmentSchema::Literal("foo".to_owned()),
                        SegmentSchema::Value(SegmentValueSchema{
                            name: "foo".to_owned(),
                            segment_type: SegmentType::I128,
                        }),
                    ],
                },
                ).unwrap(),
            {
                let mut map = HashMap::new();
                map.insert("foo".to_owned(), SegmentValue::I128(-1));
                map
            },
            );
    }

    #[test]
    fn test_parse_path_basic() {
        #[derive(Debug, Deserialize, PartialEq)]
        struct Value{
            foo: u64,
            bar: String,
        }

        let value: Value = parse_path(
            "/foo/1/bar/thing".to_owned(),
            Schema{
                segments: vec![
                    SegmentSchema::Literal("foo".to_owned()),
                    SegmentSchema::Value(SegmentValueSchema{
                        name: "foo".to_owned(),
                        segment_type: SegmentType::U64,
                    }),
                    SegmentSchema::Literal("bar".to_owned()),
                    SegmentSchema::Value(SegmentValueSchema{
                        name: "bar".to_owned(),
                        segment_type: SegmentType::String,
                    }),
                ],
            }
        ).unwrap();
        assert_eq!(value, Value{foo: 1, bar: "thing".to_owned()});
    }

    #[test]
    fn test_parse_path_i64() {
        #[derive(Debug, Deserialize, PartialEq)]
        struct Value{
            foo: i128
        }

        let value: Value = parse_path(
            "/foo/-1".to_owned(),
            Schema{
                segments: vec![
                    SegmentSchema::Literal("foo".to_owned()),
                    SegmentSchema::Value(SegmentValueSchema{
                        name: "foo".to_owned(),
                        segment_type: SegmentType::I128,
                    }),
                ],
            },
            ).unwrap();
        assert_eq!(value, Value{foo: -1});
    }

    #[test]
    fn test_parse_path_f64() {
        #[derive(Debug, Deserialize, PartialEq)]
        struct Value{
            foo: f64
        }

        let value: Value = parse_path(
            "/foo/1.2".to_owned(),
            Schema{
                segments: vec![
                    SegmentSchema::Literal("foo".to_owned()),
                    SegmentSchema::Value(SegmentValueSchema{
                        name: "foo".to_owned(),
                        segment_type: SegmentType::F64,
                    }),
                ],
            },
            ).unwrap();
        assert_eq!(value, Value{foo: 1.2});
    }
}
