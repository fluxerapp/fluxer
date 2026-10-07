// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ReportFlowTargetType} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';

interface ReportReasonDef {
	label: string;
	highestPriority: boolean;
	message: string;
	user: string;
	guild: string;
}

export const REPORT_REASONS = {
	spam: {label: 'Spam', highestPriority: false, message: 'spam', user: 'spam_account', guild: 'spam'},
	fake_account: {
		label: 'Fake or deceptive account',
		highestPriority: false,
		message: 'spam',
		user: 'spam_account',
		guild: 'spam',
	},
	platform_abuse: {
		label: 'Scraping, automation or other platform abuse',
		highestPriority: false,
		message: 'spam',
		user: 'spam_account',
		guild: 'spam',
	},
	account_trading: {
		label: 'Buying or selling accounts, logins or card details',
		highestPriority: false,
		message: 'illegal_activity',
		user: 'spam_account',
		guild: 'illegal_activity',
	},
	metric_manipulation: {
		label: 'Faked member or reaction counts',
		highestPriority: false,
		message: 'spam',
		user: 'spam_account',
		guild: 'spam',
	},
	harassment: {
		label: 'Harassment or bullying',
		highestPriority: false,
		message: 'harassment',
		user: 'harassment',
		guild: 'harassment',
	},
	unwanted_contact: {
		label: 'Stalking or unwanted contact',
		highestPriority: false,
		message: 'harassment',
		user: 'harassment',
		guild: 'harassment',
	},
	raid: {
		label: 'Raid or coordinated attack',
		highestPriority: false,
		message: 'harassment',
		user: 'harassment',
		guild: 'raid_coordination',
	},
	false_claims_about_person: {
		label: 'False claims used to target a person',
		highestPriority: false,
		message: 'harassment',
		user: 'harassment',
		guild: 'harassment',
	},
	hate_slurs: {
		label: 'Slurs, hateful symbols or stereotypes',
		highestPriority: false,
		message: 'hate_speech',
		user: 'hate_speech',
		guild: 'hate_speech',
	},
	hate_dehumanizing: {
		label: 'Dehumanizing a group',
		highestPriority: false,
		message: 'hate_speech',
		user: 'hate_speech',
		guild: 'hate_speech',
	},
	hate_incitement: {
		label: 'Calls for violence or exclusion against a group',
		highestPriority: true,
		message: 'hate_speech',
		user: 'hate_speech',
		guild: 'hate_speech',
	},
	atrocity_denial: {
		label: 'Genocide or atrocity denial or celebration',
		highestPriority: false,
		message: 'hate_speech',
		user: 'hate_speech',
		guild: 'hate_speech',
	},
	lgbtq_targeting: {
		label: 'Misgendering, deadnaming, outing or identity denial',
		highestPriority: false,
		message: 'hate_speech',
		user: 'hate_speech',
		guild: 'hate_speech',
	},
	conversion_therapy: {
		label: 'Promoting conversion therapy',
		highestPriority: false,
		message: 'hate_speech',
		user: 'hate_speech',
		guild: 'hate_speech',
	},
	hate_other: {
		label: 'Other attacks on people for who they are',
		highestPriority: false,
		message: 'hate_speech',
		user: 'hate_speech',
		guild: 'hate_speech',
	},
	violent_threat: {
		label: 'Threat of violence',
		highestPriority: true,
		message: 'violent_content',
		user: 'harassment',
		guild: 'harassment',
	},
	violence_incitement: {
		label: 'Encouraging violence or instructions to harm',
		highestPriority: true,
		message: 'violent_content',
		user: 'inappropriate_profile',
		guild: 'violent_content',
	},
	violence_glorification: {
		label: 'Glorifying real-world violence',
		highestPriority: false,
		message: 'violent_content',
		user: 'inappropriate_profile',
		guild: 'violent_content',
	},
	graphic_violence: {
		label: 'Real violence shown to shock',
		highestPriority: false,
		message: 'violent_content',
		user: 'inappropriate_profile',
		guild: 'violent_content',
	},
	terrorism_extremism: {
		label: 'Terrorism or violent extremism',
		highestPriority: false,
		message: 'violent_content',
		user: 'inappropriate_profile',
		guild: 'extremist_community',
	},
	harmful_false_claims: {
		label: 'False claims likely to cause serious harm',
		highestPriority: false,
		message: 'other',
		user: 'other',
		guild: 'other',
	},
	deceptive_synthetic_media: {
		label: 'Fake or AI media presented as real',
		highestPriority: false,
		message: 'other',
		user: 'other',
		guild: 'other',
	},
	sexual_unwanted: {
		label: 'Sexual images or messages nobody asked for',
		highestPriority: false,
		message: 'harassment',
		user: 'harassment',
		guild: 'harassment',
	},
	adult_content_unmarked: {
		label: 'Adult content outside an 18+ space',
		highestPriority: false,
		message: 'nsfw_violation',
		user: 'inappropriate_profile',
		guild: 'nsfw_violation',
	},
	intimate_image_abuse: {
		label: 'Intimate images shared without consent',
		highestPriority: true,
		message: 'doxxing',
		user: 'inappropriate_profile',
		guild: 'harassment',
	},
	sexual_exploitation: {
		label: 'Sexual exploitation, coercion or trafficking',
		highestPriority: true,
		message: 'illegal_activity',
		user: 'inappropriate_profile',
		guild: 'illegal_activity',
	},
	minor_fictional_sexualization: {
		label: 'Sexualized fictional child',
		highestPriority: true,
		message: 'child_safety',
		user: 'child_safety',
		guild: 'child_safety',
	},
	minor_sexualization: {
		label: 'Sexual comments about minors',
		highestPriority: true,
		message: 'child_safety',
		user: 'child_safety',
		guild: 'child_safety',
	},
	minor_sexual_contact: {
		label: 'Flirting with or sexually messaging a minor',
		highestPriority: true,
		message: 'child_safety',
		user: 'child_safety',
		guild: 'child_safety',
	},
	minor_sexual_content: {
		label: 'A minor sharing sexual content themselves',
		highestPriority: true,
		message: 'child_safety',
		user: 'child_safety',
		guild: 'child_safety',
	},
	csam: {
		label: 'Child sexual abuse material',
		highestPriority: true,
		message: 'child_safety',
		user: 'child_safety',
		guild: 'child_safety',
	},
	minor_grooming: {
		label: 'Grooming a minor',
		highestPriority: true,
		message: 'child_safety',
		user: 'child_safety',
		guild: 'child_safety',
	},
	minor_contact: {
		label: 'Predatory contact with a minor',
		highestPriority: true,
		message: 'child_safety',
		user: 'child_safety',
		guild: 'child_safety',
	},
	minor_in_adult_space: {
		label: 'A minor in an 18+ space',
		highestPriority: false,
		message: 'child_safety',
		user: 'child_safety',
		guild: 'child_safety',
	},
	minor_dating_community: {
		label: 'Community for dating between minors',
		highestPriority: true,
		message: 'child_safety',
		user: 'child_safety',
		guild: 'child_safety',
	},
	doxxing: {
		label: 'Private information shared without consent',
		highestPriority: false,
		message: 'doxxing',
		user: 'inappropriate_profile',
		guild: 'harassment',
	},
	underage: {
		label: 'Under the minimum age',
		highestPriority: false,
		message: 'underage_user',
		user: 'underage_user',
		guild: 'underage_user',
	},
	wellbeing_concern: {
		label: "Worried about someone's safety",
		highestPriority: true,
		message: 'self_harm',
		user: 'other',
		guild: 'self_harm',
	},
	self_harm_promotion: {
		label: 'Encouraging self-harm or sharing methods',
		highestPriority: false,
		message: 'self_harm',
		user: 'inappropriate_profile',
		guild: 'self_harm',
	},
	suicide_promotion: {
		label: 'Encouraging suicide',
		highestPriority: false,
		message: 'self_harm',
		user: 'inappropriate_profile',
		guild: 'self_harm',
	},
	eating_disorder_promotion: {
		label: 'Encouraging disordered eating',
		highestPriority: false,
		message: 'self_harm',
		user: 'inappropriate_profile',
		guild: 'self_harm',
	},
	self_harm_graphic: {
		label: 'Graphic self-harm images',
		highestPriority: false,
		message: 'self_harm',
		user: 'inappropriate_profile',
		guild: 'self_harm',
	},
	self_harm_pressure: {
		label: 'Pressuring someone to harm themselves',
		highestPriority: false,
		message: 'self_harm',
		user: 'inappropriate_profile',
		guild: 'self_harm',
	},
	impersonation_staff: {
		label: 'Pretending to be staff or support',
		highestPriority: false,
		message: 'impersonation',
		user: 'impersonation',
		guild: 'impersonation',
	},
	impersonation_person: {
		label: 'Impersonating a person',
		highestPriority: false,
		message: 'impersonation',
		user: 'impersonation',
		guild: 'impersonation',
	},
	impersonation_public_figure: {
		label: 'Impersonating a well-known person',
		highestPriority: false,
		message: 'impersonation',
		user: 'impersonation',
		guild: 'impersonation',
	},
	impersonation_organization: {
		label: 'Impersonating a company, brand or group',
		highestPriority: false,
		message: 'impersonation',
		user: 'impersonation',
		guild: 'impersonation',
	},
	fraud: {
		label: 'Conning people out of money or accounts',
		highestPriority: false,
		message: 'illegal_activity',
		user: 'spam_account',
		guild: 'illegal_activity',
	},
	illegal_goods: {
		label: 'Drug, weapon or other illegal sales',
		highestPriority: false,
		message: 'illegal_activity',
		user: 'inappropriate_profile',
		guild: 'illegal_activity',
	},
	illegal_activity: {
		label: 'Other illegal activity',
		highestPriority: false,
		message: 'illegal_activity',
		user: 'inappropriate_profile',
		guild: 'illegal_activity',
	},
	malware_phishing: {
		label: 'Phishing, malware or hacking attempts',
		highestPriority: false,
		message: 'malicious_links',
		user: 'spam_account',
		guild: 'malware_distribution',
	},
	ban_evasion: {
		label: 'Getting around a ban',
		highestPriority: false,
		message: 'other',
		user: 'other',
		guild: 'other',
	},
	permits_violations: {
		label: 'Community allows serious rule-breaking',
		highestPriority: false,
		message: 'other',
		user: 'other',
		guild: 'other',
	},
	copyright: {
		label: 'Copyright or trademark complaint',
		highestPriority: false,
		message: 'other',
		user: 'other',
		guild: 'other',
	},
	other: {label: 'Something not listed', highestPriority: false, message: 'other', user: 'other', guild: 'other'},
} as const satisfies Record<string, ReportReasonDef>;

export type ReportReasonKey = keyof typeof REPORT_REASONS;

export interface ReportReason {
	key: ReportReasonKey;
	label: string;
	highestPriority: boolean;
	legacyCategories: Record<ReportFlowTargetType, string>;
}

export function isReportReasonKey(value: string): value is ReportReasonKey {
	return Object.hasOwn(REPORT_REASONS, value);
}

export function getLegacyCategory(reason: ReportReasonKey, target: ReportFlowTargetType): string {
	return REPORT_REASONS[reason][target];
}

function toReportReason(key: ReportReasonKey): ReportReason {
	const def: ReportReasonDef = REPORT_REASONS[key];
	return {
		key,
		label: def.label,
		highestPriority: def.highestPriority,
		legacyCategories: {message: def.message, user: def.user, guild: def.guild},
	};
}

export function findReportReason(key: string): ReportReason | null {
	return isReportReasonKey(key) ? toReportReason(key) : null;
}

export function listReportReasons(): Array<ReportReason> {
	return (Object.keys(REPORT_REASONS) as Array<ReportReasonKey>).map(toReportReason);
}
