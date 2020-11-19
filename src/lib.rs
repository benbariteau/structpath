extern crate serde;
extern crate thiserror;

use std::collections::HashMap;
use thiserror::Error;
use std::num::{ParseFloatError, ParseIntError};
use serde::de::Visitor;
use std::fmt::Display;

#[derive(PartialEq, Debug)]
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

#[derive(PartialEq, Debug)]
pub struct SegmentValueSchema {
    name: String,
    segment_type: SegmentType,
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


#[derive(PartialEq, Debug)]
pub enum SegmentSchema {
    Literal(String),
    Value(SegmentValueSchema),
}

#[derive(PartialEq, Debug)]
pub struct Schema {
    segments: Vec<SegmentSchema>,
}

#[derive(Error, Debug)]
pub enum PathParseError {
    #[error("Path schema syntax error in {segment:?}: {message}")]
    SyntaxError{
        segment: String,
        message: String,
    },
    #[error("Unrecognized type: {0}")]
    UnrecognizedType(String),
}

impl Schema {
    pub fn new() -> Self {
        Self{segments: vec![]}
    }

    pub fn path<S: Into<String>>(path: S) -> Result<Self, PathParseError> {
        let mut schema = Schema{segments: vec![]};
        for segment in path.into().split("/").skip(1) {
            if &segment[0..1] == "<" {
                let no_brackets: String = segment.chars().skip(1).take_while(|c| c != &'>').collect();
                let chunks: Vec<&str> = no_brackets.split(":").collect();
                if chunks.len() > 2 {
                    return Err(PathParseError::SyntaxError{
                        segment: segment.to_owned(),
                        message: "Expected at most one ':' in path segment".to_owned(),
                    });
                } else if chunks.len() == 2 {
                    let name = chunks[0];
                    let segment_type = match chunks[1] {
                        "f32" => SegmentType::F32,
                        "f64" => SegmentType::F64,
                        "u8" => SegmentType::U8,
                        "u16" => SegmentType::U16,
                        "u32" => SegmentType::U32,
                        "u64" => SegmentType::U64,
                        "u128" => SegmentType::U128,
                        "i8" => SegmentType::I8,
                        "i16" => SegmentType::I16,
                        "i32" => SegmentType::I32,
                        "i64" => SegmentType::I64,
                        "i128" => SegmentType::I128,
                        "String" => SegmentType::String,
                        _ => {
                            return Err(PathParseError::UnrecognizedType(chunks[1].to_owned()))
                        },
                    };
                    schema.segments.push(SegmentSchema::Value(SegmentValueSchema{
                        name: name.to_owned(),
                        segment_type: segment_type,
                    }))
                } else { // chunks.len() == 1
                    schema.segments.push(SegmentSchema::Value(SegmentValueSchema{
                        name: chunks[0].to_owned(),
                        segment_type: SegmentType::String,
                    }));
                }
            } else {
                schema.segments.push(SegmentSchema::Literal(segment.to_owned()));
            }
        }
        Ok(schema)
    }

    pub fn literal<S: Into<String>>(mut self, segment_literal: S) -> Self {
        self.segments.push(SegmentSchema::Literal(segment_literal.into()));
        self
    }

    pub fn value<S: Into<String>>(mut self, name: S, segment_type: SegmentType) -> Self {
        self.segments.push(SegmentSchema::Value(SegmentValueSchema{name: name.into(), segment_type: segment_type}));
        self
    }

    pub fn parse<'a, S, T>(&self, path: S) -> Result<T, StructPathError> where S: Into<String>, T: serde::Deserialize<'a> {
        parse_path(path, self)
    }

    pub fn generate<T>(&self, parameters: &T) -> Result<String, StructPathError> where T: serde::Serialize {
        generate_path(parameters, self)
    }
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
    #[error("Expected field {0:?} missing from input")]
    MissingField(String),
}

impl serde::de::Error for StructPathError {
    fn custom<T>(msg: T) -> Self where T: Display {
        StructPathError::SerdeInternalError(msg.to_string())
    }
}

impl serde::ser::Error for StructPathError {
    fn custom<T>(msg: T) -> Self where T: Display {
        StructPathError::SerdeInternalError(msg.to_string())
    }
}

fn parse_path_generic(path: String, schema: &Schema) -> Result<HashMap<String, SegmentValue>, StructPathError> {
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

pub fn parse_path<'a, S, T>(path: S, schema: &Schema) -> Result<T, StructPathError> where S: Into<String>, T: serde::Deserialize<'a> {
    let generic_parsed_path_value = parse_path_generic(path.into(), schema)?;
    let deserializer = Deserializer{generic_parsed_path: generic_parsed_path_value};
    T::deserialize(&deserializer)
}

struct Serializer{
    last_key: String,
    serialized_values: HashMap<String, String>,
}

impl<'a> serde::ser::Serializer for &'a mut Serializer {
    type Ok = ();
    type Error = StructPathError;

    type SerializeSeq = Self;
    type SerializeTuple = Self;
    type SerializeTupleStruct = Self;
    type SerializeTupleVariant = Self;
    type SerializeMap = Self;
    type SerializeStruct = Self;
    type SerializeStructVariant = Self;

    fn serialize_bool(self, _v: bool) -> Result<(), StructPathError> {
        Err(StructPathError::NotSupported("bool".to_owned()))
    }

    fn serialize_i8(self, v: i8) -> Result<(), StructPathError> {
        self.serialized_values.insert(self.last_key.clone(), v.to_string());
        Ok(())
    }

    fn serialize_i16(self, v: i16) -> Result<(), StructPathError> {
        self.serialized_values.insert(self.last_key.clone(), v.to_string());
        Ok(())
    }

    fn serialize_i32(self, v: i32) -> Result<(), StructPathError> {
        self.serialized_values.insert(self.last_key.clone(), v.to_string());
        Ok(())
    }

    fn serialize_i64(self, v: i64) -> Result<(), StructPathError> {
        self.serialized_values.insert(self.last_key.clone(), v.to_string());
        Ok(())
    }

    fn serialize_i128(self, v: i128) -> Result<(), StructPathError> {
        self.serialized_values.insert(self.last_key.clone(), v.to_string());
        Ok(())
    }

    fn serialize_u8(self, v: u8) -> Result<(), StructPathError> {
        self.serialized_values.insert(self.last_key.clone(), v.to_string());
        Ok(())
    }

    fn serialize_u16(self, v: u16) -> Result<(), StructPathError> {
        self.serialized_values.insert(self.last_key.clone(), v.to_string());
        Ok(())
    }

    fn serialize_u32(self, v: u32) -> Result<(), StructPathError> {
        self.serialized_values.insert(self.last_key.clone(), v.to_string());
        Ok(())
    }

    fn serialize_u64(self, v: u64) -> Result<(), StructPathError> {
        self.serialized_values.insert(self.last_key.clone(), v.to_string());
        Ok(())
    }

    fn serialize_u128(self, v: u128) -> Result<(), StructPathError> {
        self.serialized_values.insert(self.last_key.clone(), v.to_string());
        Ok(())
    }

    fn serialize_f32(self, v: f32) -> Result<(), StructPathError> {
        self.serialized_values.insert(self.last_key.clone(), v.to_string());
        Ok(())
    }

    fn serialize_f64(self, v: f64) -> Result<(), StructPathError> {
        self.serialized_values.insert(self.last_key.clone(), v.to_string());
        Ok(())
    }

    fn serialize_char(self, _v: char) -> Result<(), StructPathError> {
        Err(StructPathError::NotSupported("char".to_owned()))
    }

    fn serialize_str(self, v: &str) -> Result<(), StructPathError> {
        self.serialized_values.insert(self.last_key.clone(), v.to_owned());
        Ok(())
    }

    fn serialize_bytes(self, _v: &[u8]) -> Result<(), StructPathError> {
        Err(StructPathError::NotSupported("bytes".to_owned()))
    }

    fn serialize_none(self) -> Result<(), StructPathError> {
        Err(StructPathError::NotSupported("None".to_owned()))
    }

    fn serialize_some<T>(self, _value: &T) -> Result<(), StructPathError> where T: ?Sized + serde::Serialize {
        Err(StructPathError::NotSupported("Some".to_owned()))
    }

    fn serialize_unit(self) -> Result<(), StructPathError> {
        Err(StructPathError::NotSupported("unit".to_owned()))
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<(), StructPathError> {
        Err(StructPathError::NotSupported("unit struct".to_owned()))
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
    ) -> Result<(), StructPathError> {
        Err(StructPathError::NotSupported("unit variant".to_owned()))
    }

    fn serialize_newtype_struct<T>(
        self,
        _name: &'static str,
        _value: &T,
        ) -> Result<(), StructPathError> where T: ?Sized + serde::Serialize, {
        Err(StructPathError::NotSupported("newtype struct".to_owned()))
    }

    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _value: &T,
        ) -> Result<(), StructPathError> where T: ?Sized + serde::Serialize, {
        Err(StructPathError::NotSupported("newtype variant".to_owned()))
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, StructPathError> {
        Err(StructPathError::NotSupported("sequence".to_owned()))
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple, StructPathError> {
        Err(StructPathError::NotSupported("tuple".to_owned()))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        _len: usize,
        ) -> Result<Self::SerializeTupleStruct, StructPathError> {
        Err(StructPathError::NotSupported("tuple struct".to_owned()))
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
        ) -> Result<Self::SerializeTupleVariant, StructPathError> {
        Err(StructPathError::NotSupported("tuple variant".to_owned()))
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap, StructPathError> {
        // TODO should probaby support this
        Err(StructPathError::NotSupported("map".to_owned()))
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        _len: usize,
        ) -> Result<Self::SerializeStruct, StructPathError> {
        Ok(self)
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
        ) -> Result<Self::SerializeStructVariant, StructPathError> {
        Err(StructPathError::NotSupported("struct variant".to_owned()))
    }

}

impl<'a> serde::ser::SerializeSeq for &'a mut Serializer {
    type Ok = ();
    type Error = StructPathError;

    fn serialize_element<T>(&mut self, _value: &T) -> Result<(), StructPathError> where T: ?Sized + serde::Serialize {
        Err(StructPathError::NotSupported("sequence".to_owned()))
    }

    fn end(self) -> Result<(), StructPathError> {
        Err(StructPathError::NotSupported("sequence".to_owned()))
    }
}

impl<'a> serde::ser::SerializeTuple for &'a mut Serializer {
    type Ok = ();
    type Error = StructPathError;

    fn serialize_element<T>(&mut self, _value: &T) -> Result<(), StructPathError> where T: ?Sized + serde::Serialize {
        Err(StructPathError::NotSupported("tuple".to_owned()))
    }

    fn end(self) -> Result<(), StructPathError> {
        Err(StructPathError::NotSupported("tuple".to_owned()))
    }
}

impl<'a> serde::ser::SerializeTupleStruct for &'a mut Serializer {
    type Ok = ();
    type Error = StructPathError;

    fn serialize_field<T>(&mut self, _value: &T) -> Result<(), StructPathError> where T: ?Sized + serde::Serialize {
        Err(StructPathError::NotSupported("tuple struct".to_owned()))
    }

    fn end(self) -> Result<(), StructPathError> {
        Err(StructPathError::NotSupported("tuple struct".to_owned()))
    }
}

impl<'a> serde::ser::SerializeTupleVariant for &'a mut Serializer {
    type Ok = ();
    type Error = StructPathError;

    fn serialize_field<T>(&mut self, _value: &T) -> Result<(), StructPathError> where T: ?Sized + serde::Serialize {
        Err(StructPathError::NotSupported("tuple variant".to_owned()))
    }

    fn end(self) -> Result<(), StructPathError> {
        Err(StructPathError::NotSupported("tuple variant".to_owned()))
    }
}

impl<'a> serde::ser::SerializeMap for &'a mut Serializer {
    type Ok = ();
    type Error = StructPathError;

    fn serialize_key<T>(&mut self, _key: &T) -> Result<(), StructPathError> where T: ?Sized + serde::Serialize, {
        Err(StructPathError::NotSupported("map".to_owned()))
    }

    fn serialize_value<T>(&mut self, _value: &T) -> Result<(), StructPathError> where T: ?Sized + serde::Serialize {
        Err(StructPathError::NotSupported("map".to_owned()))
    }

    fn end(self) -> Result<(), StructPathError> {
        Err(StructPathError::NotSupported("map".to_owned()))
    }
}

impl<'a> serde::ser::SerializeStruct for &'a mut Serializer {
    type Ok = ();
    type Error = StructPathError;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), StructPathError> where T: ?Sized + serde::Serialize {
        self.last_key = key.to_owned();
        value.serialize(&mut **self)?;
        Ok(())
    }

    fn end(self) -> Result<(), StructPathError> {
        Ok(())
    }
}

impl<'a> serde::ser::SerializeStructVariant for &'a mut Serializer {
    type Ok = ();
    type Error = StructPathError;

    fn serialize_field<T>(&mut self, _key: &'static str, _value: &T) -> Result<(), StructPathError> where T: ?Sized + serde::Serialize {
        Err(StructPathError::NotSupported("struct variant".to_owned()))
    }

    fn end(self) -> Result<(), StructPathError> {
        Err(StructPathError::NotSupported("struct variant".to_owned()))
    }
}

fn generate_path<T>(parameters: &T, schema: &Schema) -> Result<String, StructPathError> where T: serde::Serialize {
    let mut serializer = Serializer{
        last_key: "".to_owned(),
        serialized_values: HashMap::new(),
    };
    parameters.serialize(&mut serializer)?;
    let mut generated_path = String::new();
    for segment_schema in &schema.segments {
        match segment_schema {
            SegmentSchema::Literal(literal) => generated_path = format!("{}/{}", generated_path, literal),
            SegmentSchema::Value(segment_value_schema) => match serializer.serialized_values.get(&segment_value_schema.name) {
                Some(value) => generated_path = format!("{}/{}", generated_path, value),
                None => return Err(StructPathError::MissingField(segment_value_schema.name.clone())),
            }
        }
    }
    Ok(generated_path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde::{Deserialize, Serialize};
    #[test]
    fn test_parse_path_generic() {
        assert_eq!(
            parse_path_generic(
                "/foo/1/bar/thing".to_owned(),
                &Schema{
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
                &Schema{
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
                &Schema{
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
    fn test_schema_building() {
        let schema = Schema::new()
            .literal("foo")
            .value("foo", SegmentType::U64)
            .literal("bar")
            .value("bar", SegmentType::String);
        assert_eq!(
            schema,
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
                },
            );
    }

    #[test]
    fn test_schema_path() {
        assert_eq!(
            Schema::path("/foo/<foo_id:u128>/bar/<bar_thing:String>").unwrap(),
            Schema{
                segments: vec![
                    SegmentSchema::Literal("foo".to_owned()),
                    SegmentSchema::Value(SegmentValueSchema{
                        name: "foo_id".to_owned(),
                        segment_type: SegmentType::U128,
                    }),
                    SegmentSchema::Literal("bar".to_owned()),
                    SegmentSchema::Value(SegmentValueSchema{
                        name: "bar_thing".to_owned(),
                        segment_type: SegmentType::String,
                    }),
                ],
            }
            );
    }

    #[test]
    fn test_schema_path_string_default() {
        assert_eq!(
            Schema::path("/foo/<bar>").unwrap(),
            Schema{
                segments: vec![
                    SegmentSchema::Literal("foo".to_owned()),
                    SegmentSchema::Value(SegmentValueSchema{
                        name: "bar".to_owned(),
                        segment_type: SegmentType::String,
                    }),
                ],
            }
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
            &Schema{
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
            &Schema{
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
            &Schema{
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

    #[test]
    fn test_parse_path_idiomatic() {

        #[derive(Deserialize, PartialEq, Debug)]
        struct Parameters{
            foo: u64,
            bar: String,
        }

        let path_schema = Schema::path("/foo/<foo:u64>/bar/<bar>").unwrap();
        let parameters: Parameters = path_schema.parse("/foo/1/bar/thing").unwrap();
        assert_eq!(parameters, Parameters{foo: 1, bar: "thing".to_owned()});

    }

    #[test]
    fn test_generate_path() {
        #[derive(Serialize, PartialEq, Debug)]
        struct Parameters{
            foo: u64,
            bar: String,
        }

        let schema = Schema::path("/foo/<foo:u64>/bar/<bar>").unwrap();
        assert_eq!(schema.generate(&Parameters{foo: 1, bar: "thing".to_owned()}).unwrap(), "/foo/1/bar/thing");
    }
}
