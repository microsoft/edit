use std::borrow::Borrow;
use std::ops::Deref;

pub enum MaybeOwned<'a, B, O>
where
    B: ?Sized,
    O: Borrow<B>,
{
    Borrowed(&'a B),
    Owned(O),
}

impl<'a, B, O> Deref for MaybeOwned<'a, B, O>
where
    B: ?Sized,
    O: Borrow<B>,
{
    type Target = B;

    fn deref(&self) -> &Self::Target {
        match self {
            MaybeOwned::Borrowed(b) => b,
            MaybeOwned::Owned(o) => o.borrow(),
        }
    }
}
