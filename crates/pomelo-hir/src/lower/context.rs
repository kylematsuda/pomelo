//! Lowering context.

use std::collections::{HashMap, HashSet};
use std::hash::Hash;

use crate::identifiers::{LongTyCon, LongVId, NameInterner, NameInternerImpl, SwitchInterner};
use crate::File;

/// Context needed while lowering.
///
/// This is used to do name resolution on the topdecs during lowering.
///
/// Maybe need to also have a `BodyLoweringCtxt` as well, just to keep track of weird nested infix stuff?
/// That seems like an annoying corner case...
#[derive(Debug, Default, Clone)]
pub struct LoweringCtxt {
    resolver: Resolver,
    file: File,
}

impl LoweringCtxt {
    pub fn resolver(&self) -> &Resolver {
        &self.resolver
    }

    pub fn resolver_mut(&mut self) -> &mut Resolver {
        &mut self.resolver
    }

    pub fn file(&self) -> &File {
        &self.file
    }

    pub fn file_mut(&mut self) -> &mut File {
        &mut self.file
    }
}

/// Holds the results of early name resolution
#[derive(Debug, Default, Clone)]
pub struct Resolver {
    // Hold names that have top-level scope
    interner: NameInternerImpl,
    values: NamespaceRes<LongVId>,
    tys: NamespaceRes<LongTyCon>,
}

/// This is just a wrapper to avoid looking at the interned string indices relative to the wrong
/// interner.
///
/// Maybe there's a more elegant way to do this... perhaps a trick using lifetimes?
pub struct InternedHere<T>(T);

impl Resolver {
    /// Update the interned string refs in `id` (initially relative to `body_interner`)
    /// to point to the correct indices relative to `self.interner`.
    pub fn rebase_id_on_self<T: SwitchInterner, I: NameInterner>(
        &mut self,
        id: T,
        body_interner: &I,
    ) -> Option<InternedHere<T>> {
        id.switch_interner(body_interner, &mut self.interner)
            .map(InternedHere)
    }

    pub fn add_value_def(&mut self, vid: InternedHere<LongVId>, at_index: usize) -> Option<usize> {
        self.values.add_def(vid.0, at_index)
    }

    pub fn add_ty_def(&mut self, ty: InternedHere<LongTyCon>, at_index: usize) -> Option<usize> {
        self.tys.add_def(ty.0, at_index)
    }

    pub fn find_value_def(
        &mut self,
        vid: &InternedHere<LongVId>,
        current_index: usize,
    ) -> Option<usize> {
        self.values.find_def(&vid.0, current_index)
    }

    pub fn find_ty_def(
        &mut self,
        ty: &InternedHere<LongTyCon>,
        current_index: usize,
    ) -> Option<usize> {
        self.tys.find_def(&ty.0, current_index)
    }

    pub fn add_value_ref(
        &mut self,
        vid: InternedHere<LongVId>,
        current_index: usize,
    ) -> Option<usize> {
        self.values.add_ref(vid.0, current_index)
    }

    pub fn add_ty_ref(
        &mut self,
        ty: InternedHere<LongTyCon>,
        current_index: usize,
    ) -> Option<usize> {
        self.tys.add_ref(ty.0, current_index)
    }
}

/// FIXME: Need to distinguish value vs type namespace!!!
#[derive(Debug, Clone)]
pub struct NamespaceRes<T> {
    // Maps from a name to the index of the topdec where it's defined.
    // The value type is a `Vec` because names may be shadowed.
    defs: HashMap<T, Vec<usize>>,
    // Maps from a name and its def location to the topdecs where it is referenced.
    refs: HashMap<(T, usize), Vec<usize>>,
    // Set of shadowed names inside of the body.
    // This is to avoid having `refs` erroneously point at a shadowed name inside of a body.
    shadowed: HashSet<T>,
}

impl<T> Default for NamespaceRes<T> {
    fn default() -> Self {
        Self {
            defs: HashMap::new(),
            refs: HashMap::new(),
            shadowed: HashSet::new(),
        }
    }
}

impl<T: SwitchInterner + Eq + Hash> NamespaceRes<T> {
    pub fn add_def(&mut self, id: T, at_index: usize) -> Option<usize> {
        self.defs
            .entry(id)
            .and_modify(|indices| indices.push(at_index))
            .or_insert_with(|| vec![at_index]);
        Some(at_index)
    }

    pub fn find_def(&mut self, longid: &T, current_index: usize) -> Option<usize> {
        // TODO: figure out if this shadowed check is helpful / needed / correct
        if self.shadowed.get(longid).is_some() {
            return None;
        }

        let indices = self.defs.get(longid)?;
        // !!! This assumes that `indices` is in order !!! It would be nice if we didn't have to.
        indices.iter().rev().find(|&&n| n > current_index).copied()
    }

    pub fn add_ref(&mut self, longid: T, current_index: usize) -> Option<usize> {
        let def_pos = self.find_def(&longid, current_index)?;

        self.refs
            .entry((longid, def_pos))
            .and_modify(|indices| indices.push(current_index))
            .or_insert_with(|| vec![current_index]);
        Some(current_index)
    }
}
