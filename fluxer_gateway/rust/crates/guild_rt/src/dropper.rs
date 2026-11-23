// SPDX-License-Identifier: AGPL-3.0-or-later

use std::panic::{AssertUnwindSafe, catch_unwind};
use std::sync::OnceLock;
use std::sync::mpsc::{Receiver, SyncSender, TrySendError, sync_channel};

use crate::metrics::{self, ResourceKind};

const QUEUE_BOUND: usize = 4_096;

struct Parcel {
    value: Box<dyn Send>,
    kind: ResourceKind,
}

static DROPPER: OnceLock<Option<SyncSender<Parcel>>> = OnceLock::new();

fn run(rx: Receiver<Parcel>) {
    while let Ok(parcel) = rx.recv() {
        let Parcel { value, kind } = parcel;
        let _ = catch_unwind(AssertUnwindSafe(move || drop(value)));
        metrics::dequeued(kind);
    }
}

fn spawn() -> Option<SyncSender<Parcel>> {
    let (tx, rx) = sync_channel::<Parcel>(QUEUE_BOUND);
    std::thread::Builder::new()
        .name("gateway_nif_dropper".to_owned())
        .spawn(move || run(rx))
        .ok()
        .map(|_| tx)
}

pub fn start() -> bool {
    DROPPER.get_or_init(spawn).is_some()
}

pub fn drop_later<T: Send + 'static>(value: T, kind: ResourceKind) {
    let Some(tx) = DROPPER.get_or_init(spawn) else {
        metrics::dropped_inline();
        drop(value);
        return;
    };
    metrics::queued(kind);
    let parcel = Parcel {
        value: Box::new(value),
        kind,
    };
    match tx.try_send(parcel) {
        Ok(()) => {}
        Err(TrySendError::Full(parcel)) | Err(TrySendError::Disconnected(parcel)) => {
            metrics::dequeued(kind);
            metrics::dropped_inline();
            drop(parcel);
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::mpsc::{SyncSender, sync_channel};
    use std::time::{Duration, Instant};

    use super::{drop_later, start};
    use crate::metrics::{ResourceKind, metrics};

    struct Unwinds;

    impl Drop for Unwinds {
        fn drop(&mut self) {
            std::panic::resume_unwind(Box::new("drop unwinds"));
        }
    }

    struct Witness(SyncSender<Option<String>>);

    impl Drop for Witness {
        fn drop(&mut self) {
            let name = std::thread::current().name().map(str::to_owned);
            let _ = self.0.send(name);
        }
    }

    fn dropping(kind: ResourceKind) -> u64 {
        let name = format!("{}_dropping", kind.name());
        metrics()
            .into_iter()
            .find(|(found, _)| *found == name)
            .map_or(0, |(_, value)| value)
    }

    #[test]
    fn a_drop_that_unwinds_leaves_the_dropper_running() {
        assert!(start());
        let kind = ResourceKind::ZstdContext;
        drop_later(Unwinds, kind);
        let deadline = Instant::now() + Duration::from_secs(5);
        while dropping(kind) != 0 && Instant::now() < deadline {
            std::thread::sleep(Duration::from_millis(1));
        }
        assert_eq!(dropping(kind), 0);
        let (tx, rx) = sync_channel(1);
        drop_later(Witness(tx), kind);
        let dropped_on = rx.recv_timeout(Duration::from_secs(5));
        assert_eq!(dropped_on, Ok(Some("gateway_nif_dropper".to_owned())));
    }
}
