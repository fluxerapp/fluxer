// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	ReportGuildRequest,
	ReportMessageRequest,
	ReportUserRequest,
} from '@fluxer/schema/src/domains/report/ReportSchemas';

export type IARReportContextType = 'message' | 'user' | 'guild';
export type IARRuleReasonId =
	| 'harassment'
	| 'hate'
	| 'violence'
	| 'terrorism_extremism'
	| 'mature_content'
	| 'child_safety'
	| 'harmful_misinformation'
	| 'illegal_activity'
	| 'spam_scams'
	| 'malware'
	| 'privacy'
	| 'impersonation'
	| 'inappropriate_profile'
	| 'raid_coordination'
	| 'self_harm'
	| 'other';
export type IARRuleCategoryId = 'targeted_harm' | 'safety_minors' | 'privacy_identity' | 'deception' | 'illegal_other';

export const RULE_REASONS_BY_CATEGORY: Record<IARRuleCategoryId, ReadonlyArray<IARRuleReasonId>> = {
	targeted_harm: ['harassment', 'hate', 'violence', 'terrorism_extremism', 'raid_coordination', 'self_harm'],
	safety_minors: ['child_safety', 'mature_content'],
	privacy_identity: ['privacy', 'impersonation', 'inappropriate_profile'],
	deception: ['spam_scams', 'malware', 'harmful_misinformation'],
	illegal_other: ['illegal_activity', 'other'],
};

export function getCategoryForReason(reason: IARRuleReasonId): IARRuleCategoryId {
	for (const [category, reasons] of Object.entries(RULE_REASONS_BY_CATEGORY) as Array<
		[IARRuleCategoryId, ReadonlyArray<IARRuleReasonId>]
	>) {
		if (reasons.includes(reason)) return category;
	}
	return 'illegal_other';
}

type CategoryMap = {
	message: ReportMessageRequest['category'];
	user: ReportUserRequest['category'];
	guild: ReportGuildRequest['category'];
};

export const REPORT_CATEGORY_BY_REASON: Record<IARRuleReasonId, CategoryMap> = {
	harassment: {
		message: 'harassment',
		user: 'harassment',
		guild: 'harassment',
	},
	hate: {
		message: 'hate_speech',
		user: 'hate_speech',
		guild: 'hate_speech',
	},
	violence: {
		message: 'violent_content',
		user: 'harassment',
		guild: 'other',
	},
	terrorism_extremism: {
		message: 'violent_content',
		user: 'other',
		guild: 'extremist_community',
	},
	mature_content: {
		message: 'nsfw_violation',
		user: 'harassment',
		guild: 'other',
	},
	child_safety: {
		message: 'child_safety',
		user: 'underage_user',
		guild: 'child_safety',
	},
	harmful_misinformation: {
		message: 'other',
		user: 'other',
		guild: 'other',
	},
	illegal_activity: {
		message: 'illegal_activity',
		user: 'other',
		guild: 'illegal_activity',
	},
	spam_scams: {
		message: 'spam',
		user: 'spam_account',
		guild: 'spam',
	},
	malware: {
		message: 'malicious_links',
		user: 'spam_account',
		guild: 'malware_distribution',
	},
	privacy: {
		message: 'doxxing',
		user: 'harassment',
		guild: 'harassment',
	},
	impersonation: {
		message: 'impersonation',
		user: 'impersonation',
		guild: 'other',
	},
	inappropriate_profile: {
		message: 'other',
		user: 'inappropriate_profile',
		guild: 'other',
	},
	raid_coordination: {
		message: 'harassment',
		user: 'other',
		guild: 'raid_coordination',
	},
	self_harm: {
		message: 'self_harm',
		user: 'other',
		guild: 'other',
	},
	other: {
		message: 'other',
		user: 'other',
		guild: 'other',
	},
};

export function getReportCategoryForReason(
	contextType: IARReportContextType,
	reason: IARRuleReasonId,
): CategoryMap[IARReportContextType] {
	return REPORT_CATEGORY_BY_REASON[reason][contextType];
}
