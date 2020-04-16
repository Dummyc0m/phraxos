use im::hashmap::HashMap;
use crate::types::ast::Ident;

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct BindingPower(pub u8, pub u8);

#[derive(Debug, Clone)]
pub struct PresCtx {
    pres_map: HashMap<Ident, BindingPower>
}

impl PresCtx {
    pub fn new() -> Self {
        PresCtx {
            pres_map: HashMap::new()
        }
    }

    pub fn get(&self, k: &Ident) -> Option<&BindingPower> {
        self.pres_map.get(k)
    }

    pub fn insert(&mut self, k: Ident, v: BindingPower) {
        self.pres_map.insert(k, v);
    }
}
