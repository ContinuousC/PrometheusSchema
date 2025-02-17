/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use rayon::iter::ParallelIterator;

/// `Limited` is the return type for the limited monad, that provides
/// computations with early failure if the result surpasses a
/// preconfigured limit. The first element of the tuple represents the
/// number of permissions (limit) left. The second element is the
/// actual return value. If the return value is `None`, the
/// computation failed to produce a result within the limit.
///
/// Functions should have a signature:
///   fn computation(max: usize, ...) -> Limited<T>;
///
/// Operations:
/// - Consume permission from the limit:
///     let left = max.checked_sub(1)?;
/// - Return `r`:
///     Some((left, r))
/// - Sequential computations:
///     let (left, a) = opA(max, ...)?;
///     let (left, b) = opB(left, ...)?;
/// - Alternative computations (see `LimitedAlt` trait):
///     (0..10).iter().limited_alt(...);
pub(super) type Limited<T> = Option<(usize, T)>;

/// `LimitedAlt` extension trait.
pub(super) trait LimitedAlt: ParallelIterator + Sized {
    fn limited_alt<F, T: Send>(self, max: usize, f: F) -> Limited<T>
    where
        F: Fn(usize, Self::Item) -> Limited<T> + Send + Sync,
    {
        self.try_fold::<Limited<T>, _, _, _>(
            || None,
            |r, elem| match r
                .as_ref()
                .map_or(Some(max), |(left, _)| (max - *left).checked_sub(1))
            {
                Some(local_max) => Ok(f(local_max, elem)
                    .map(|(left, r)| (left + (max - local_max), r))
                    .or(r)),
                None => Err(r),
            },
        )
        .map(|r| r.unwrap_or_else(|e| e))
        .reduce(
            || None,
            |a, b| match (a, b) {
                (Some((aleft, _)), b @ Some((bleft, _))) if bleft > aleft => b,
                (None, b @ Some(_)) => b,
                (a, _) => a,
            },
        )
    }
}

impl<T: ParallelIterator + Sized> LimitedAlt for T {}
