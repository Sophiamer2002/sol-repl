use core::panic;
use std::{collections::HashMap, rc::Rc};

use super::prelude::*;

#[derive(Clone, Debug)]
pub struct Storage {
    pub table: Vec<StorageSlot> // no garbage collection
}


#[derive(Clone, Debug)]
pub enum StorageSlot {
    D(Primitive),
    Array {
        ty: Rc<Type>,
        values: Vec<usize>,
    },
    Mapping {
        ty: Rc<Type>,
        map: HashMap<Primitive, usize>,
    },
    Struct {
        ty: Rc<Type>,
        fields: Vec<usize>,
    },
    Bytes {
        value: Vec<u8>,
    },
}

#[derive(Clone, Debug)]
pub struct Memory {
    pub table: Vec<MemorySlot>
}

#[derive(Clone, Debug)]
pub enum MemorySlot {
    D(Primitive),
    Array {
        value_type: Rc<Type>,
        sz: u32,
        values: Vec<usize>,
    },
    Struct {
        ty: Rc<Type>,
        fields: Vec<usize>,
    },
    Bytes {
        value: Vec<u8>,
    },
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Location {
    Memory,
    StorageOwn, // storage slot owned by the variable
    Storage,
    Calldata,
}

impl Storage {
    pub fn new() -> Self {
        Storage {
            table: Vec::new(),
        }
    }

    pub fn get(&self, slot: usize) -> Option<&StorageSlot> {
        self.table.get(slot)
    }

    pub fn get_data(&mut self, slot: usize) -> Option<&mut Primitive> {
        if let Some(StorageSlot::D(data)) = self.table.get_mut(slot) {
            Some(data)
        } else {
            None
        }
    }

    pub fn get_compound_type(&mut self, slot: usize) -> Option<Rc<Type>> {
        if let Some(slot) = self.table.get(slot) {
            match slot {
                StorageSlot::Array { ty, .. } => Some(ty.clone()),
                StorageSlot::Mapping { ty, .. } => Some(ty.clone()),
                StorageSlot::Struct { ty, .. } => Some(ty.clone()),
                _ => None,
            }
        } else { None }
    }

    // check if the content in the slot can be applied to a variable of type ty.
    pub fn match_type(&self, slot: usize, ty: &Rc<Type>) -> bool {
        if let Some(slot) = self.table.get(slot) {
            match (slot, &**ty) {
                (StorageSlot::D(data), Type::B(ty)) => data.match_type_auto(ty),
                (StorageSlot::Array { ty: slot_ty, .. }, Type::C(CompoundType::Array { .. })) => {
                    ty == slot_ty
                },
                (StorageSlot::Mapping { ty: slot_ty, .. }, Type::C(CompoundType::Mapping { .. })) => {
                    ty == slot_ty
                },
                (StorageSlot::Struct { ty: slot_ty, .. }, Type::C(CompoundType::Struct { .. })) => {
                    ty == slot_ty
                },
                _ => false,
            }
        } else { false }
    }

    pub fn assign_basic_data(&mut self, data: Primitive) -> usize {
        let slot = self.table.len();
        self.table.push(StorageSlot::D(data));
        slot
    }

    pub fn assign_mapping(&mut self, ty: &Rc<Type>) -> usize {
        let slot = self.table.len();
        self.table.push(StorageSlot::Mapping {
            ty: ty.clone(),
            map: HashMap::new(),
        });
        slot
    }

    pub fn assign_array(&mut self, ty: &Rc<Type>) -> usize {
        if let Type::C(CompoundType::Array { len, value }) = &**ty {
            if let Some(len) = len {
                let slots = (0..*len).map(|_| {
                    self.assign_type_default(value)
                }).collect::<Vec<_>>();

                let slot = self.table.len();
                self.table.push(StorageSlot::Array {
                    ty: ty.clone(),
                    values: slots,
                });
                return slot
            }
        }
        panic!("assign_array: not an array type")
    }

    pub fn assign_struct(&mut self, ty: &Rc<Type>) -> usize {
        if let Type::C(CompoundType::Struct { fields, .. }) = &**ty {
            let slots = fields.iter().map(|(_, ty)| {
                self.assign_type_default(ty)
            }).collect::<Vec<_>>();
            let slot = self.table.len();
            self.table.push(StorageSlot::Struct {
                ty: ty.clone(),
                fields: slots,
            });
            return slot
        }
        panic!("assign_struct: not a struct type")
    }

    pub fn assign_type_default(&mut self, ty: &Rc<Type>) -> usize {
        match &**ty {
            Type::B(ty) => {
                let slot = self.table.len();
                self.table.push(StorageSlot::D(Primitive::default(ty)));
                slot
            },
            Type::C(CompoundType::Array { .. }) => {
                self.assign_array(ty)
            },
            Type::C(CompoundType::Mapping { .. }) => {
                self.assign_mapping(ty)
            },
            Type::C(CompoundType::Struct { .. }) => {
                self.assign_struct(ty)
            },
            Type::C(CompoundType::DynamicBytes) => {
                unimplemented!()
            },
            Type::C(CompoundType::String) => {
                unimplemented!()
            },
            Type::Literal => {
                panic!("Literal type cannot be default")
            },
        }
    }
}

impl Memory {
    pub fn new() -> Self {
        Memory {
            table: Vec::new(),
        }
    }

    pub fn get(&self, slot: usize) -> Option<&MemorySlot> {
        self.table.get(slot)
    }

    pub fn get_data(&mut self, slot: usize) -> Option<&mut Primitive> {
        if let Some(MemorySlot::D(data)) = self.table.get_mut(slot) {
            Some(data)
        } else {
            None
        }
    }
}
