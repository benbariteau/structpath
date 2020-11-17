#[macro_use] extern crate maplit;
extern crate serde;
extern crate thiserror;

use std::collections::HashMap;
use thiserror::Error;
use std::num::{ParseFloatError, ParseIntError};
use serde::de::Visitor;
use std::fmt::Display;

pub enum SegmentType {
    Float,
    Integer,
    String,
    UnsignedInteger,
}

pub struct SegmentValueSchema {
    pub name: String,
    pub segment_type: SegmentType,
}

#[derive(PartialEq, Debug)]
pub enum SegmentValue {
    F64(f64),
    I128(i128),
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
                    SegmentType::Float => {
                        path_values.insert(segment_value_schema.name.clone(), SegmentValue::F64(segment.parse()?));
                    },
                    SegmentType::Integer => {
                        path_values.insert(segment_value_schema.name.clone(), SegmentValue::I128(segment.parse()?));
                    },
                    SegmentType::UnsignedInteger => {
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

impl <'de, 'a> serde::de::Deserializer<'de> for &'a mut Deserializer {
    type Error = StructPathError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_unit_struct<V>(self, name: &'static str, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_newtype_struct<V>(self, name: &'static str, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_tuple<V>(self, len: usize, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_tuple_struct<V>(self, name: &'static str, len: usize, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_struct<V>(self, name: &'static str, fields: &'static [&'static str], visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_enum<V>(self, name: &'static str, variants: &'static [&'static str], visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
        unimplemented!();
    }
}

pub fn parse_path<'a, T>(path: String, schema: Schema) -> Result<T, StructPathError> where T: serde::Deserialize<'a> {
    let generic_parsed_path_value = parse_path_generic(path, schema)?;
    unimplemented!();
}

#[cfg(test)]
mod tests {
    use super::*;
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
                            segment_type: SegmentType::UnsignedInteger,
                        }),
                        SegmentSchema::Literal("bar".to_owned()),
                        SegmentSchema::Value(SegmentValueSchema{
                            name: "bar".to_owned(),
                            segment_type: SegmentType::String,
                        }),
                    ],
                }
                ).unwrap(),
                hashmap!{
                    "foo".to_owned() => SegmentValue::U128(1),
                    "bar".to_owned() => SegmentValue::String("thing".to_owned()),
                }
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
                            segment_type: SegmentType::Float,
                        }),
                    ],
                },
                ).unwrap(),
            hashmap!{
                "foo".to_owned() => SegmentValue::F64(1.2),
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
                            segment_type: SegmentType::Integer,
                        }),
                    ],
                },
                ).unwrap(),
            hashmap!{
                "foo".to_owned() => SegmentValue::I128(-1),
            },
            );
    }
}
