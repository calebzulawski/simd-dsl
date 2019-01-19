#[derive(PartialEq, Clone, Debug)]
pub enum Primitive {
    Unsigned8,
    Unsigned16,
    Unsigned32,
    Unsigned64,
    Signed8,
    Signed16,
    Signed32,
    Signed64,
    Float32,
    Float64,
}
