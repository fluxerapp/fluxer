pub const USER_FLAG_STAFF: i64 = 1 << 0;
pub const USER_FLAG_PARTNER: i64 = 1 << 2;
const USER_FLAG_BUG_HUNTER: i64 = 1 << 3;
const USER_FLAG_FRIENDLY_BOT: i64 = 1 << 4;
const USER_FLAG_FRIENDLY_BOT_MANUAL_APPROVAL: i64 = 1 << 5;
const USER_FLAG_SPAMMER: i64 = 1 << 6;
pub const USER_FLAG_STAFF_HIDDEN: i64 = 1 << 57;
const PUBLIC_USER_FLAGS: i64 = USER_FLAG_STAFF
    | USER_FLAG_PARTNER
    | USER_FLAG_BUG_HUNTER
    | USER_FLAG_FRIENDLY_BOT
    | USER_FLAG_FRIENDLY_BOT_MANUAL_APPROVAL
    | USER_FLAG_SPAMMER;
const PUBLIC_USER_FLAGS_WITHOUT_STAFF: i64 = PUBLIC_USER_FLAGS & !USER_FLAG_STAFF;

pub fn visible_user_flags(flags: i64) -> i32 {
    let visible_flags = if (flags & USER_FLAG_STAFF_HIDDEN) != 0 {
        PUBLIC_USER_FLAGS_WITHOUT_STAFF
    } else {
        PUBLIC_USER_FLAGS
    };
    (flags & visible_flags) as i32
}
