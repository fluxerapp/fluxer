pub const USER_FLAG_STAFF: i64 = 1 << 0;
pub const USER_FLAG_PARTNER: i64 = 1 << 2;
const USER_FLAG_BUG_HUNTER: i64 = 1 << 3;
const USER_FLAG_FRIENDLY_BOT: i64 = 1 << 4;
const USER_FLAG_FRIENDLY_BOT_MANUAL_APPROVAL: i64 = 1 << 5;
const USER_FLAG_SPAMMER: i64 = 1 << 6;
pub const USER_FLAG_PROFILE_HIDDEN: i64 = 1 << 7;
const USER_FLAG_DELETED: i64 = 1 << 34;
const USER_FLAG_DISABLED: i64 = 1 << 38;
pub const USER_FLAG_STAFF_HIDDEN: i64 = 1 << 57;
const PUBLIC_USER_FLAGS: i64 = USER_FLAG_STAFF
    | USER_FLAG_PARTNER
    | USER_FLAG_BUG_HUNTER
    | USER_FLAG_FRIENDLY_BOT
    | USER_FLAG_FRIENDLY_BOT_MANUAL_APPROVAL
    | USER_FLAG_SPAMMER;
const PUBLIC_USER_FLAGS_WITHOUT_STAFF: i64 = PUBLIC_USER_FLAGS & !USER_FLAG_STAFF;
const NON_ENFORCEMENT_DELETION_REASONS: [i32; 3] = [1, 2, 19];

pub fn visible_user_flags(flags: i64) -> i32 {
    let visible_flags = if (flags & USER_FLAG_STAFF_HIDDEN) != 0 {
        PUBLIC_USER_FLAGS_WITHOUT_STAFF
    } else {
        PUBLIC_USER_FLAGS
    };
    (flags & visible_flags) as i32
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

    const NOW: i64 = 1_790_000_000_000;

    fn standing(flags: i64) -> AccountStanding {
        AccountStanding {
            flags,
            ..AccountStanding::default()
        }
    }

    #[test]
    fn profile_hidden_follows_enforcement_state() {
        assert!(!standing(0).profile_hidden(NOW));
        assert!(standing(USER_FLAG_PROFILE_HIDDEN).profile_hidden(NOW));
        assert!(standing(USER_FLAG_SPAMMER).profile_hidden(NOW));
        assert!(!standing(USER_FLAG_DISABLED).profile_hidden(NOW));
        let banned = AccountStanding {
            temp_banned_until_ms: Some(NOW + 1),
            ..standing(USER_FLAG_DISABLED)
        };
        assert!(banned.profile_hidden(NOW));
        assert!(!banned.profile_hidden(NOW + 1));
        let deleting = |code| AccountStanding {
            pending_deletion_at_ms: Some(NOW),
            deletion_reason_code: Some(code),
            ..standing(USER_FLAG_DELETED)
        };
        assert!(deleting(3).profile_hidden(NOW));
        assert!(deleting(20).profile_hidden(NOW));
        for code in NON_ENFORCEMENT_DELETION_REASONS {
            assert!(!deleting(code).profile_hidden(NOW), "{code}");
        }
        assert!(!standing(USER_FLAG_DELETED | USER_FLAG_SPAMMER).profile_hidden(NOW));
    }

    #[test]
    fn the_hidden_bit_is_never_public() {
        assert_eq!(
            visible_user_flags(USER_FLAG_PROFILE_HIDDEN | USER_FLAG_DISABLED | USER_FLAG_PARTNER),
            USER_FLAG_PARTNER as i32
        );
    }
}
