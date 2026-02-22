/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

/** @jsxRuntime automatic */
/** @jsxImportSource hono/jsx */

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {DeletionReasons as CoreDeletionReasons} from '@fluxer/constants/src/Core';
import {GuildFeatures, GuildOperations} from '@fluxer/constants/src/GuildConstants';
import {SuspiciousActivityFlags, UserFlags} from '@fluxer/constants/src/UserConstants';

export interface PatchableUserFlag {
	name: string;
	value: bigint;
}

export const FLAG_STAFF: PatchableUserFlag = {name: 'STAFF', value: UserFlags.STAFF};
export const FLAG_STAFF_HIDDEN: PatchableUserFlag = {name: 'STAFF_HIDDEN', value: UserFlags.STAFF_HIDDEN};
export const FLAG_CTP_MEMBER: PatchableUserFlag = {name: 'CTP_MEMBER', value: UserFlags.CTP_MEMBER};
export const FLAG_PARTNER: PatchableUserFlag = {name: 'PARTNER', value: UserFlags.PARTNER};
export const FLAG_BUG_HUNTER: PatchableUserFlag = {name: 'BUG_HUNTER', value: UserFlags.BUG_HUNTER};
export const FLAG_HIGH_GLOBAL_RATE_LIMIT: PatchableUserFlag = {
	name: 'HIGH_GLOBAL_RATE_LIMIT',
	value: UserFlags.HIGH_GLOBAL_RATE_LIMIT,
};
export const FLAG_PREMIUM_PURCHASE_DISABLED: PatchableUserFlag = {
	name: 'PREMIUM_PURCHASE_DISABLED',
	value: UserFlags.PREMIUM_PURCHASE_DISABLED,
};
export const FLAG_PREMIUM_ENABLED_OVERRIDE: PatchableUserFlag = {
	name: 'PREMIUM_ENABLED_OVERRIDE',
	value: UserFlags.PREMIUM_ENABLED_OVERRIDE,
};
export const FLAG_RATE_LIMIT_BYPASS: PatchableUserFlag = {
	name: 'RATE_LIMIT_BYPASS',
	value: UserFlags.RATE_LIMIT_BYPASS,
};
export const FLAG_REPORT_BANNED: PatchableUserFlag = {name: 'REPORT_BANNED', value: UserFlags.REPORT_BANNED};
export const FLAG_VERIFIED_NOT_UNDERAGE: PatchableUserFlag = {
	name: 'VERIFIED_NOT_UNDERAGE',
	value: UserFlags.VERIFIED_NOT_UNDERAGE,
};
export const FLAG_USED_MOBILE_CLIENT: PatchableUserFlag = {
	name: 'USED_MOBILE_CLIENT',
	value: UserFlags.USED_MOBILE_CLIENT,
};
export const FLAG_APP_STORE_REVIEWER: PatchableUserFlag = {
	name: 'APP_STORE_REVIEWER',
	value: UserFlags.APP_STORE_REVIEWER,
};
export const FLAG_DM_HISTORY_BACKFILLED: PatchableUserFlag = {
	name: 'DM_HISTORY_BACKFILLED',
	value: UserFlags.DM_HISTORY_BACKFILLED,
};

export const SELF_HOSTED_PATCHABLE_FLAGS: Array<PatchableUserFlag> = [
	FLAG_STAFF,
	FLAG_STAFF_HIDDEN,
	FLAG_HIGH_GLOBAL_RATE_LIMIT,
	FLAG_RATE_LIMIT_BYPASS,
	FLAG_REPORT_BANNED,
	FLAG_VERIFIED_NOT_UNDERAGE,
];

export const PATCHABLE_FLAGS: Array<PatchableUserFlag> = [
	FLAG_STAFF,
	FLAG_STAFF_HIDDEN,
	FLAG_CTP_MEMBER,
	FLAG_PARTNER,
	FLAG_BUG_HUNTER,
	FLAG_HIGH_GLOBAL_RATE_LIMIT,
	FLAG_PREMIUM_PURCHASE_DISABLED,
	FLAG_PREMIUM_ENABLED_OVERRIDE,
	FLAG_RATE_LIMIT_BYPASS,
	FLAG_REPORT_BANNED,
	FLAG_VERIFIED_NOT_UNDERAGE,
	FLAG_USED_MOBILE_CLIENT,
	FLAG_APP_STORE_REVIEWER,
	FLAG_DM_HISTORY_BACKFILLED,
];

export interface Flag {
	name: string;
	value: number;
}

export const SUSPICIOUS_FLAG_REQUIRE_VERIFIED_EMAIL: Flag = {
	name: 'REQUIRE_VERIFIED_EMAIL',
	value: SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL,
};
export const SUSPICIOUS_FLAG_REQUIRE_REVERIFIED_EMAIL: Flag = {
	name: 'REQUIRE_REVERIFIED_EMAIL',
	value: SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL,
};
export const SUSPICIOUS_FLAG_REQUIRE_VERIFIED_PHONE: Flag = {
	name: 'REQUIRE_VERIFIED_PHONE',
	value: SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE,
};
export const SUSPICIOUS_FLAG_REQUIRE_REVERIFIED_PHONE: Flag = {
	name: 'REQUIRE_REVERIFIED_PHONE',
	value: SuspiciousActivityFlags.REQUIRE_REVERIFIED_PHONE,
};
export const SUSPICIOUS_FLAG_REQUIRE_VERIFIED_EMAIL_OR_VERIFIED_PHONE: Flag = {
	name: 'REQUIRE_VERIFIED_EMAIL_OR_VERIFIED_PHONE',
	value: SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL_OR_VERIFIED_PHONE,
};
export const SUSPICIOUS_FLAG_REQUIRE_REVERIFIED_EMAIL_OR_VERIFIED_PHONE: Flag = {
	name: 'REQUIRE_REVERIFIED_EMAIL_OR_VERIFIED_PHONE',
	value: SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL_OR_VERIFIED_PHONE,
};
export const SUSPICIOUS_FLAG_REQUIRE_VERIFIED_EMAIL_OR_REVERIFIED_PHONE: Flag = {
	name: 'REQUIRE_VERIFIED_EMAIL_OR_REVERIFIED_PHONE',
	value: SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL_OR_REVERIFIED_PHONE,
};
export const SUSPICIOUS_FLAG_REQUIRE_REVERIFIED_EMAIL_OR_REVERIFIED_PHONE: Flag = {
	name: 'REQUIRE_REVERIFIED_EMAIL_OR_REVERIFIED_PHONE',
	value: SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL_OR_REVERIFIED_PHONE,
};

export const SUSPICIOUS_ACTIVITY_FLAGS: Array<Flag> = [
	SUSPICIOUS_FLAG_REQUIRE_VERIFIED_EMAIL,
	SUSPICIOUS_FLAG_REQUIRE_REVERIFIED_EMAIL,
	SUSPICIOUS_FLAG_REQUIRE_VERIFIED_PHONE,
	SUSPICIOUS_FLAG_REQUIRE_REVERIFIED_PHONE,
	SUSPICIOUS_FLAG_REQUIRE_VERIFIED_EMAIL_OR_VERIFIED_PHONE,
	SUSPICIOUS_FLAG_REQUIRE_REVERIFIED_EMAIL_OR_VERIFIED_PHONE,
	SUSPICIOUS_FLAG_REQUIRE_VERIFIED_EMAIL_OR_REVERIFIED_PHONE,
	SUSPICIOUS_FLAG_REQUIRE_REVERIFIED_EMAIL_OR_REVERIFIED_PHONE,
];

export const DELETION_REASONS: Array<{id: number; label: string}> = [
	{id: CoreDeletionReasons.USER_REQUESTED, label: 'User requested'},
	{id: CoreDeletionReasons.OTHER, label: 'Other'},
	{id: CoreDeletionReasons.SPAM, label: 'Spam'},
	{id: CoreDeletionReasons.CHEATING_OR_EXPLOITATION, label: 'Cheating or exploitation'},
	{id: CoreDeletionReasons.COORDINATED_RAIDING, label: 'Coordinated raiding or manipulation'},
	{id: CoreDeletionReasons.AUTOMATION_OR_SELFBOT, label: 'Automation or self-bot usage'},
	{id: CoreDeletionReasons.NONCONSENSUAL_SEXUAL_CONTENT, label: 'Nonconsensual sexual content'},
	{id: CoreDeletionReasons.SCAM_OR_SOCIAL_ENGINEERING, label: 'Scam or social engineering'},
	{id: CoreDeletionReasons.CHILD_SEXUAL_CONTENT, label: 'Child sexual content'},
	{id: CoreDeletionReasons.PRIVACY_VIOLATION_OR_DOXXING, label: 'Privacy violation or doxxing'},
	{id: CoreDeletionReasons.HARASSMENT_OR_BULLYING, label: 'Harassment or bullying'},
	{id: CoreDeletionReasons.PAYMENT_FRAUD, label: 'Payment fraud'},
	{id: CoreDeletionReasons.CHILD_SAFETY_VIOLATION, label: 'Child safety violation'},
	{id: CoreDeletionReasons.BILLING_DISPUTE_OR_ABUSE, label: 'Billing dispute or abuse'},
	{id: CoreDeletionReasons.UNSOLICITED_EXPLICIT_CONTENT, label: 'Unsolicited explicit content'},
	{id: CoreDeletionReasons.GRAPHIC_VIOLENCE, label: 'Graphic violence'},
	{id: CoreDeletionReasons.BAN_EVASION, label: 'Ban evasion'},
	{id: CoreDeletionReasons.TOKEN_OR_CREDENTIAL_SCAM, label: 'Token or credential scam'},
	{id: CoreDeletionReasons.INACTIVITY, label: 'Inactivity'},
	{id: CoreDeletionReasons.HATE_SPEECH_OR_EXTREMIST_CONTENT, label: 'Hate speech or extremist content'},
	{id: CoreDeletionReasons.MALICIOUS_LINKS_OR_MALWARE, label: 'Malicious links or malware distribution'},
	{id: CoreDeletionReasons.IMPERSONATION_OR_FAKE_IDENTITY, label: 'Impersonation or fake identity'},
];

export const TEMP_BAN_DURATIONS: Array<{hours: number; label: string}> = [
	{hours: 1, label: '1 hour'},
	{hours: 12, label: '12 hours'},
	{hours: 24, label: '1 day'},
	{hours: 72, label: '3 days'},
	{hours: 120, label: '5 days'},
	{hours: 168, label: '1 week'},
	{hours: 336, label: '2 weeks'},
	{hours: 720, label: '30 days'},
	{hours: 0, label: 'Permanent'},
];

export const ALL_ACLS = Object.values(AdminACLs);

export const GUILD_FEATURES = Object.values(GuildFeatures) as ReadonlyArray<string>;

const HOSTED_ONLY_GUILD_FEATURES: ReadonlyArray<string> = [
	GuildFeatures.VISIONARY,
	GuildFeatures.VIP_VOICE,
	GuildFeatures.OPERATOR,
];

export const SELF_HOSTED_GUILD_FEATURES: ReadonlyArray<string> = GUILD_FEATURES.filter(
	(f) => !HOSTED_ONLY_GUILD_FEATURES.includes(f),
);

export const DISABLED_OP_PUSH_NOTIFICATIONS: Flag = {
	name: 'PUSH_NOTIFICATIONS',
	value: GuildOperations.PUSH_NOTIFICATIONS,
};
export const DISABLED_OP_EVERYONE_MENTIONS: Flag = {
	name: 'EVERYONE_MENTIONS',
	value: GuildOperations.EVERYONE_MENTIONS,
};
export const DISABLED_OP_TYPING_EVENTS: Flag = {name: 'TYPING_EVENTS', value: GuildOperations.TYPING_EVENTS};
export const DISABLED_OP_INSTANT_INVITES: Flag = {name: 'INSTANT_INVITES', value: GuildOperations.INSTANT_INVITES};
export const DISABLED_OP_SEND_MESSAGE: Flag = {name: 'SEND_MESSAGE', value: GuildOperations.SEND_MESSAGE};
export const DISABLED_OP_REACTIONS: Flag = {name: 'REACTIONS', value: GuildOperations.REACTIONS};
export const DISABLED_OP_MEMBER_LIST_UPDATES: Flag = {
	name: 'MEMBER_LIST_UPDATES',
	value: GuildOperations.MEMBER_LIST_UPDATES,
};

export const DISABLED_OPERATIONS: Array<Flag> = [
	DISABLED_OP_PUSH_NOTIFICATIONS,
	DISABLED_OP_EVERYONE_MENTIONS,
	DISABLED_OP_TYPING_EVENTS,
	DISABLED_OP_INSTANT_INVITES,
	DISABLED_OP_SEND_MESSAGE,
	DISABLED_OP_REACTIONS,
	DISABLED_OP_MEMBER_LIST_UPDATES,
];
