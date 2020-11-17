#[macro_use] extern crate maplit;
extern crate serde;
extern crate thiserror;

use std::collections::HashMap;
use thiserror::Error;
use std::str::FromStr;
use std::num::{ParseFloatError, ParseIntError};

enum SegmentType {
    Float,
    Integer,
    String,
    UnsignedInteger,
}

struct SegmentValueSchema {
    name: String,
    segment_type: SegmentType,
}

#[derive(PartialEq, Debug)]
enum SegmentValue {
    F64(f64),
    I128(i128),
    U128(u128),
    String(String),
}


enum SegmentSchema {
    Literal(String),
    Value(SegmentValueSchema),
}

struct Schema {
    segments: Vec<SegmentSchema>,
}

#[derive(Error, Debug)]
enum StructPathError {
    #[error("Incorrect path segment (expected {expected:?}, got {got:?})")]
    IncorrectSegment{
        got: String,
        expected: String,
    },
    #[error(transparent)]
    ParseFloatError(#[from] ParseFloatError),
    #[error(transparent)]
    ParseIntError(#[from] ParseIntError),
}

fn parse_path(path: String, schema: Schema) -> Result<HashMap<String, SegmentValue>, StructPathError> {
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

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn parse_path_works() {
        assert_eq!(
            parse_path(
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
}
