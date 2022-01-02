use std::collections::BTreeMap;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, Ord, PartialOrd)]
pub struct Symbol<'a> {
    name: &'a str,
    unique_id: Option<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, Ord, PartialOrd)]
pub struct UniqueSymbol<'a>(Symbol<'a>);

impl<'a> UniqueSymbol<'a> {
    pub fn new(symbol: Symbol<'a>) -> Option<Self> {
        if symbol.unique_id.is_some() {
            Some(UniqueSymbol(symbol))
        } else {
            None
        }
    }
}

impl<'a> From<UniqueSymbol<'a>> for Symbol<'a> {
    fn from(unique_symbol: UniqueSymbol<'a>) -> Self {
        unique_symbol.0
    }
}

impl<'a> Symbol<'a> {
    pub fn new(name: &'a str, unique_id: usize) -> Self {
        Symbol {
            name,
            unique_id: Some(unique_id),
        }
    }
    pub fn non_unique(name: &'a str) -> Self {
        Symbol {
            name,
            unique_id: None,
        }
    }
    pub fn name(&self) -> &'a str {
        self.name
    }
    pub fn unique_id(&self) -> Option<usize> {
        self.unique_id
    }
    pub fn is_unique(&self) -> bool {
        self.unique_id.is_some()
    }
}

pub trait ToSymbol<'a> {
    fn to_symbol(&self) -> Symbol<'a>;
}

impl<'a> ToSymbol<'a> for &'a str {
    fn to_symbol(&self) -> Symbol<'a> {
        Symbol::non_unique(self)
    }
}

pub struct SymGen {
    curr_id: usize,
}

impl SymGen {
    pub fn new() -> Self {
        SymGen { curr_id: 0 }
    }

    /// Generate a unique symbol for a given name.
    pub fn gensym<'a>(&mut self, name: &'a str) -> Symbol<'a> {
        let id = self.curr_id;
        self.curr_id += 1;
        Symbol::new(name, id)
    }
}

/// A symbol table that be forked in constant time without affecting the
/// original.
pub struct Symtab<'a, T> {
    pub store: BTreeMap<Symbol<'a>, T>,
    pub parent: Option<&'a Symtab<'a, T>>,
}

impl<'a, T> Symtab<'a, T> {
    pub fn new() -> Self {
        Symtab {
            store: BTreeMap::new(),
            parent: None,
        }
    }

    pub fn fork(&'a self) -> Self {
        Symtab {
            store: BTreeMap::new(),
            parent: Some(self),
        }
    }

    /// Get the value associated with the given key. If the key is not present
    /// in the current scope, the parent scope is searched.
    pub fn get(&'a self, key: Symbol<'a>) -> Option<&'a T> {
        self.store
            .get(&key)
            .or_else(|| self.parent.and_then(|p| p.get(key)))
    }

    /// Inserts a key-value pair into the map.
    ///
    /// If the map did not have this key present, None is returned.
    ///
    /// If the map did have this key present, the value is updated, and the old
    /// value is returned. The key is not updated, though; this matters for
    /// types that can be `==` without being identical.
    pub fn insert(&mut self, key: Symbol<'a>, value: T) -> Option<T> {
        self.store.insert(key, value)
    }

    /// Remove a key from current scope, returning the value at the key if the
    /// key exists in the current scope.
    pub fn remove(&mut self, key: Symbol<'a>) -> Option<T> {
        self.store.remove(&key)
    }
}
