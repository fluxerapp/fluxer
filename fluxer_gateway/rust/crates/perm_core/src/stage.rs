// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::flags::Permissions;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub struct Stage {
    allow: Permissions,
    deny: Permissions,
}

impl Stage {
    pub const IDENTITY: Stage = Stage {
        allow: Permissions::empty(),
        deny: Permissions::empty(),
    };

    pub const fn new(allow: Permissions, deny: Permissions) -> Stage {
        Stage { allow, deny }
    }

    pub const fn allow(self) -> Permissions {
        self.allow
    }

    pub const fn deny(self) -> Permissions {
        self.deny
    }

    pub fn apply(self, permissions: Permissions) -> Permissions {
        (permissions & !self.deny) | self.allow
    }

    pub fn then(self, next: Stage) -> Stage {
        Stage {
            deny: self.deny | next.deny,
            allow: (self.allow & !next.deny) | next.allow,
        }
    }

    pub fn fold<I>(stages: I) -> Stage
    where
        I: IntoIterator<Item = Stage>,
    {
        stages
            .into_iter()
            .fold(Stage::IDENTITY, |acc, stage| acc.then(stage))
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub struct Grant {
    allow: Permissions,
    deny: Permissions,
}

impl Grant {
    pub const NONE: Grant = Grant {
        allow: Permissions::empty(),
        deny: Permissions::empty(),
    };

    pub const fn new(allow: Permissions, deny: Permissions) -> Grant {
        Grant { allow, deny }
    }

    pub const fn allow(self) -> Permissions {
        self.allow
    }

    pub const fn deny(self) -> Permissions {
        self.deny
    }

    pub fn merge(self, other: Grant) -> Grant {
        Grant {
            allow: self.allow | other.allow,
            deny: self.deny | other.deny,
        }
    }

    pub const fn as_stage(self) -> Stage {
        Stage {
            allow: self.allow,
            deny: self.deny,
        }
    }
}
