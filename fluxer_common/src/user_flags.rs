pub const USER_FLAG_STAFF: i64 = 1 << 0;
pub const USER_FLAG_PARTNER: i64 = 1 << 2;
const USER_FLAG_BUG_HUNTER: i64 = 1 << 3;
const USER_FLAG_FRIENDLY_BOT: i64 = 1 << 4;
const USER_FLAG_FRIENDLY_BOT_MANUAL_APPROVAL: i64 = 1 << 5;
const USER_FLAG_SPAMMER: i64 = 1 << 6;
pub const USER_FLAG_PROFILE_HIDDEN: i64 = 1 << 7;
const USER_FLAG_DELETED: i64 = 1 << 34;
const USER_FLAG_DISABLED: i64 = 1 << 38;
const LEGACY_USER_FLAG_STAFF_HIDDEN: i64 = 1 << 57;
const PUBLIC_USER_FLAGS: i64 = USER_FLAG_STAFF
    | USER_FLAG_PARTNER
    | USER_FLAG_BUG_HUNTER
    | USER_FLAG_FRIENDLY_BOT
    | USER_FLAG_FRIENDLY_BOT_MANUAL_APPROVAL
    | USER_FLAG_SPAMMER;
const HIDEABLE_FLAG_BADGES: [(&str, i64); 3] = [
    ("staff", USER_FLAG_STAFF),
    ("partner", USER_FLAG_PARTNER),
    ("bug_hunter", USER_FLAG_BUG_HUNTER),
];
const NON_ENFORCEMENT_DELETION_REASONS: [i32; 3] = [1, 2, 19];

pub fn visible_user_flags(flags: i64, hidden_profile_badges: &[String]) -> i32 {
    let mut visible = PUBLIC_USER_FLAGS;
    for (key, flag) in HIDEABLE_FLAG_BADGES {
        if hidden_profile_badges.iter().any(|hidden| hidden == key) {
            visible &= !flag;
        }
    }
    if flags & LEGACY_USER_FLAG_STAFF_HIDDEN != 0 {
        visible &= !USER_FLAG_STAFF;
    }
    (flags & visible) as i32
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct AccountStanding {
    pub flags: i64,
    pub temp_banned_until_ms: Option<i64>,
    pub pending_deletion_at_ms: Option<i64>,
    pub deletion_reason_code: Option<i32>,
}

impl AccountStanding {
    pub fn deleted_for_display(&self) -> bool {
        self.flags & USER_FLAG_DELETED != 0 && self.pending_deletion_at_ms.is_none()
    }

    pub fn profile_hidden(&self, now_ms: i64) -> bool {
        let banned = self.flags & USER_FLAG_DISABLED != 0
            && self
                .temp_banned_until_ms
                .is_some_and(|until| until > now_ms);
        let deleting = self.pending_deletion_at_ms.is_some()
            && self
                .deletion_reason_code
                .is_some_and(|code| !NON_ENFORCEMENT_DELETION_REASONS.contains(&code));
        !self.deleted_for_display()
            && (self.flags & (USER_FLAG_PROFILE_HIDDEN | USER_FLAG_SPAMMER) != 0
                || banned
                || deleting)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_hidden_bit_is_never_public() {
        assert_eq!(
            visible_user_flags(
                USER_FLAG_PROFILE_HIDDEN | USER_FLAG_DISABLED | USER_FLAG_PARTNER,
                &[]
            ),
            USER_FLAG_PARTNER as i32
        );
    }
}
